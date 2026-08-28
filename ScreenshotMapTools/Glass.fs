module Glass

open System
open System.Windows
open System.Windows.Media
open System.Threading
open System.Windows.Controls

open Winterop

let makeArrow(targetX, targetY, sourceX, sourceY, brush) =
    let tx,ty = targetX, targetY
    let sx,sy = sourceX, sourceY
    // line from source to target
    let line = new Shapes.Line(X1=sx, Y1=sy, X2=tx, Y2=ty, Stroke=brush, StrokeThickness=3.)
    line.StrokeDashArray <- new DoubleCollection(seq[5.;4.])
    let sq(x) = x*x
    let pct = 1. - 15./sqrt(sq(tx-sx)+sq(ty-sy))   // arrowhead base ideally 15 pixels down the line
    let pct = max pct 0.93                         // but at most 93% towards the target, for small lines
    let ax,ay = (tx-sx)*pct+sx, (ty-sy)*pct+sy
    // differential between target and arrowhead base
    let dx,dy = tx-ax, ty-ay
    // points orthogonal to the line from the base
    let p1x,p1y = ax+dy/2., ay-dx/2.
    let p2x,p2y = ax-dy/2., ay+dx/2.
    // triangle to make arrowhead
    let triangle = new Shapes.Polygon(Fill=brush)
    triangle.Points <- new PointCollection([Point(tx,ty); Point(p1x,p1y); Point(p2x,p2y)])
    line, triangle

let debugOutput = true
let debugWindowZOrder() =
    if debugOutput then
        let mutable hwndCur = Win32.GetTopWindow(IntPtr(0))
        let mutable count = 0
        printfn "Current top of window stack:"
        while hwndCur <> IntPtr(0) && count < 5 do
            if Win32.IsWindowVisible(hwndCur) then
                let title = WinteropUtils.GetWindowTitle(hwndCur)
                if not(System.String.IsNullOrEmpty(title)) then
                    printfn "    %s" (WinteropUtils.GetWindowTitle hwndCur)
                    count <- count + 1
            hwndCur <- Win32.GetWindow(hwndCur, GW_HWNDNEXT)

/// Configures a WPF window to hover seamlessly over a target external HWND
let setupOverlayWindow(overlayWindow: Window, targetHWnd: nativeint, controlsWindow:Window, isFirstClickFocusSwitch:bool ref) =
    let helper = System.Windows.Interop.WindowInteropHelper(overlayWindow)
    let overlayHWnd = helper.Handle
    let controlsHwnd = System.Windows.Interop.WindowInteropHelper(controlsWindow).Handle
    // Establish native Window Ownership
    // This forces Windows to keep the overlay above the target in Z-order automatically
    Win32.SetWindowLongPtrA(overlayHWnd, GWLP_HWNDPARENT, targetHWnd) |> ignore
    // Inject Click-Through (TRANSPARENT) and Focus Prevention (NOACTIVATE) styles
    let currentExStyle = Win32.GetWindowLong(overlayHWnd, GWL_EXSTYLE)
    let newExStyle = currentExStyle ||| WS_EX_TRANSPARENT ||| WS_EX_NOACTIVATE
    Win32.SetWindowLongPtrA(overlayHWnd, GWL_EXSTYLE, nativeint newExStyle) |> ignore
    // Intercept the native Win32 Message Pump
    let source = System.Windows.Interop.HwndSource.FromHwnd(overlayHWnd)
    source.AddHook(System.Windows.Interop.HwndSourceHook(fun hwnd msg wParam lParam handled ->
        if msg = WM_MOUSEACTIVATE then
            let currentForeground = Win32.GetForegroundWindow()
            // If unrelated third-party window currently has focus
            if currentForeground <> overlayHWnd && currentForeground <> targetHWnd && currentForeground <> controlsHwnd then
                isFirstClickFocusSwitch.Value <- true
                // Explicitly bring your target window cluster forward
                Win32.SetForegroundWindow(targetHWnd) |> ignore
                handled <- true
                MA_NOACTIVATEANDEAT
            else
                isFirstClickFocusSwitch.Value <- false
                // Tell the OS to process the click locally, but do not trigger 
                // an OS activation cycle (restores smooth clicking to 3rd party windows)
                handled <- true
                MA_NOACTIVATE
        else
            0n
        ))
    // Refresh Z-order to apply changes immediately without shifting position
    Win32.SetWindowPos(overlayHWnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE ||| SWP_NOSIZE ||| SWP_NOACTIVATE) |> ignore

/// Toggles whether the overlay window blocks mouse clicks or lets them pass through.
/// Set 'isClickThrough' to true to make it click-through, or false to intercept clicks.
let setOverlayClickThrough(overlayHwnd: nativeint, isClickThrough: bool) =
    if overlayHwnd <> 0n then
        let currentExStyle = Win32.GetWindowLong(overlayHwnd, GWL_EXSTYLE)
        let newExStyle = 
            if isClickThrough then
                currentExStyle ||| WS_EX_TRANSPARENT
            else
                currentExStyle &&& ~~~WS_EX_TRANSPARENT
        Win32.SetWindowLongPtrA(overlayHwnd, GWL_EXSTYLE, nativeint newExStyle) |> ignore
        // 4. Force Windows to redraw the frame and update hit-testing behavior immediately
        // SWP_FRAMECHANGED (0x0020u) is critical here to tell the OS the window frame/styles changed.
        Win32.SetWindowPos(
            overlayHwnd, HWND_TOP, 0, 0, 0, 0, 
            SWP_NOMOVE ||| SWP_NOSIZE ||| SWP_NOACTIVATE ||| SWP_FRAMECHANGED
        ) |> ignore


type ControlsWindow(parentGlass : Window, renameF, eraseF, sizeParentF, updateClickThruModeF, updatePenShapeF, updateModeF, updateDrawArrowHeadsF, updatePenColorF) as this =
    inherit Window()
    let mutable clickThru = false
    let mutable hwndGlassTarget = IntPtr(0)
    let isFirstClickFocusSwitch = ref false
    let label = new Label(Content="switch to click-thru")
    let toggleClickThruButton = new Button(Content=label, Margin=Thickness(2.))
    do
        this.Title <- "GlassControl"
        this.Loaded.Add(fun _ ->
            //printfn "loading controls"
            this.Content <- new TextBox(Text="focus the window you\nwant to draw on top of", Margin=Thickness(8.), BorderThickness=Thickness(0.))
            async {
                //printfn "waiting for user to focus another app"
                let! _ = Async.AwaitEvent this.Deactivated
                do! Async.Sleep(100)  // ensure time for OS to activate other app
                sizeParentF()
                updateClickThruModeF(clickThru)
                hwndGlassTarget <- Win32.GetForegroundWindow()
                let targetWindowName = WinteropUtils.GetWindowTitle(hwndGlassTarget)
                renameF(targetWindowName)
                let r = WinteropUtils.GetActiveWindowClientRect()
                this.Top <- float(r.bottom + 4)
                this.Left <- float(r.left)
                setupOverlayWindow(parentGlass,hwndGlassTarget,this,isFirstClickFocusSwitch)
                Win32.SetWindowLongPtrA(System.Windows.Interop.WindowInteropHelper(this).Handle, GWLP_HWNDPARENT, hwndGlassTarget) |> ignore
                let hwndParentGlass = System.Windows.Interop.WindowInteropHelper(parentGlass).Handle
                toggleClickThruButton.Click.Add(fun _ ->
                    clickThru <- not clickThru
                    setOverlayClickThrough(hwndParentGlass, clickThru)
                    label.Content <- if clickThru then "switch to drawing" else "switch to click-thru"
                    updateClickThruModeF(clickThru)
                    )
                setOverlayClickThrough(hwndParentGlass, clickThru)
                let eraseButton = new Button(Content=new Label(Content="erase all"), Margin=Thickness(2.))
                eraseButton.Click.Add(fun _ -> eraseF())
                (*
                // shape
                let shapePanel =
                    let rbPanel = new StackPanel(Orientation=Orientation.Vertical, Margin=Thickness(2.))
                    let penEllipseRB = new RadioButton(Content=new Label(Content="ellipse"), GroupName="penShape", VerticalContentAlignment=VerticalAlignment.Center)
                    penEllipseRB.Click.Add(fun _ -> updatePenShapeF(0))
                    let penRectangleRB = new RadioButton(Content=new Label(Content="rectange"), GroupName="penShape", VerticalContentAlignment=VerticalAlignment.Center)
                    penRectangleRB.Click.Add(fun _ -> updatePenShapeF(1))
                    rbPanel.Children.Add(penEllipseRB) |> ignore
                    rbPanel.Children.Add(penRectangleRB) |> ignore
                    // default
                    penEllipseRB.IsChecked <- true
                    updatePenShapeF(0)
                    rbPanel
                // mode
                let modePanel =
                    let rbPanel = new StackPanel(Orientation=Orientation.Vertical, Margin=Thickness(2.))
                    let spotlightRB = new RadioButton(Content=new Label(Content="spotlight"), GroupName="mode", VerticalContentAlignment=VerticalAlignment.Center)
                    spotlightRB.Click.Add(fun _ -> updateModeF(0))
                    let penRB = new RadioButton(Content=new Label(Content="pen"), GroupName="mode", VerticalContentAlignment=VerticalAlignment.Center)
                    penRB.Click.Add(fun _ -> updateModeF(1))
                    rbPanel.Children.Add(spotlightRB) |> ignore
                    rbPanel.Children.Add(penRB) |> ignore
                    // default
                    penRB.IsChecked <- true
                    updateModeF(1)
                    rbPanel
                // draw arrowheads?
                let daPanel =
                    let rbPanel = new StackPanel(Orientation=Orientation.Vertical, Margin=Thickness(2.))
                    let yesRB = new RadioButton(Content=new Label(Content="arrows"), GroupName="da", VerticalContentAlignment=VerticalAlignment.Center)
                    yesRB.Click.Add(fun _ -> updateDrawArrowHeadsF(true))
                    let noRB = new RadioButton(Content=new Label(Content="no arrows"), GroupName="da", VerticalContentAlignment=VerticalAlignment.Center)
                    noRB.Click.Add(fun _ -> updateDrawArrowHeadsF(false))
                    rbPanel.Children.Add(yesRB) |> ignore
                    rbPanel.Children.Add(noRB) |> ignore
                    // default
                    noRB.IsChecked <- true
                    updateDrawArrowHeadsF(false)
                    rbPanel
                *)
                updatePenShapeF(0)
                updateModeF(1)
                updateDrawArrowHeadsF(false)
                let arrowCB = new CheckBox(Content="arrowheads", IsChecked=false, Margin=Thickness(2.), VerticalAlignment=VerticalAlignment.Center)
                arrowCB.Checked.Add(fun _ -> updateDrawArrowHeadsF(true))
                arrowCB.Unchecked.Add(fun _ -> updateDrawArrowHeadsF(false))
                let colorGrid =
                    let colors = [| [| Brushes.Red; Brushes.Green; Brushes.Blue; Brushes.White |] 
                                    [| Brushes.Cyan; Brushes.Magenta; Brushes.Yellow; Brushes.Black |] |]
                    let g = Utils.makeGrid(4,2,24,24)
                    g.Background <- new SolidColorBrush(Color=Color.FromRgb(0x60uy,0x60uy,0x60uy))
                    let allBorders = ResizeArray()
                    for i = 0 to 3 do
                        for j = 0 to 1 do
                            let curColor = colors.[j].[i]
                            let b = new Border(BorderThickness=Thickness(2.), BorderBrush=(if i=0 && j=0 then Brushes.Lime else Brushes.Transparent),
                                                    Child=new DockPanel(Background=curColor), Width=20., Height=20.)
                            allBorders.Add(b)
                            b.MouseDown.Add(fun _ -> 
                                for x in allBorders do x.BorderBrush <- Brushes.Transparent
                                b.BorderBrush <- Brushes.Lime
                                updatePenColorF(curColor)
                                )
                            Utils.gridAdd(g, b, i, j)
                    g
                // overall ui                
                let sp = new StackPanel(Orientation=Orientation.Horizontal, Margin=Thickness(4.))
                sp.Children.Add(toggleClickThruButton) |> ignore
                sp.Children.Add(eraseButton) |> ignore
                sp.Children.Add(arrowCB) |> ignore
                sp.Children.Add(colorGrid) |> ignore
                this.Content <- sp
                do! Async.Sleep(1)  // pump ui
                parentGlass.Topmost <- true
                parentGlass.Activate() |> ignore
                parentGlass.Topmost <- false
                } |> Async.StartImmediate
        )
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Closed.Add(fun _ ->
            async { 
                let ctxt = SynchronizationContext.Current
                do! Async.Sleep(20)
                do! Async.SwitchToContext(ctxt)
                parentGlass.Close() 
                } |> Async.StartImmediate
            )
    member this.TargetHwnd = hwndGlassTarget
    member this.IsFirstClickFocusSwitch = isFirstClickFocusSwitch.Value

// TODO if the target window moves or changes size, things kinda fall apart
type DrawingGlassWindow() as this =
    inherit Window()
    let mutable isCurrentlyClickThru = false
    let mutable myControlsWindow = null
    let mutable thisHwnd, controlsHwnd = IntPtr(0), IntPtr(0)
    do
        do  // essential window styles for all the magic to work
            this.WindowStyle <- WindowStyle.None
            this.AllowsTransparency <- true
            this.Background <- Brushes.Transparent
            this.Topmost <- false

        this.SizeToContent <- SizeToContent.Manual
        this.WindowStartupLocation <- WindowStartupLocation.Manual
        //this.ForceCursor <- true
        this.Cursor <- System.Windows.Input.Cursors.None

        let myCursor = 
            let r = new Shapes.Polygon(Stroke=Brushes.Black, StrokeThickness=1., Fill=Brushes.White, IsHitTestVisible=false)
            r.Points <- new PointCollection([Point(0.,0.); Point(0.,14.); Point(3.,10.); Point(6.,15.); Point(7.,14.); Point(5.,9.); Point(9.,9.)])
            r

        let total = new Canvas()
        let catchAll = new Canvas(Background=Brushes.Black, Opacity=0.01)  // to get all mouse clicks
        let spotlightCanvas = new Canvas(Background=Brushes.Black, IsHitTestVisible=false, Opacity=0.99)   // never completely obscure the game, lest that somehow prevent windows from drawing some game pixels or something
        let penCanvas = new Canvas(Background=Brushes.Transparent, IsHitTestVisible=false)
        total.Children.Add(catchAll) |> ignore
        total.Children.Add(spotlightCanvas) |> ignore
        total.Children.Add(penCanvas) |> ignore
        let borderRect = new System.Windows.Shapes.Rectangle(Stroke=Brushes.Transparent)
        total.Children.Add(borderRect) |> ignore
        let watermark = new TextBox(Text="LorgonGlass", FontSize=8., Foreground=Brushes.DarkOrange, Background=Brushes.Black, 
                                        IsHitTestVisible=false, IsReadOnly=true, Opacity=0., BorderThickness=Thickness(1.), BorderBrush=Brushes.Transparent)
        total.Children.Add(watermark) |> ignore
        total.Children.Add(myCursor) |> ignore
        Canvas.SetRight(watermark, 0.)
        Canvas.SetTop(watermark, 0.)
        
        let mutable drawingMode = -1   // 0=spotlight, 1=pen
        let mutable penShape = -1      // 0=ellipse, 1=rectangle
        let mutable drawArrowheads = true
        let redoStack = new System.Collections.Generic.Stack<UIElement>()
        let pen = new Pen(Brush=Brushes.Red, Thickness=10.)
        let updateMode(m) = drawingMode <- m
        let updatePenShape(p) = 
            penShape <- p
            pen.StartLineCap <- if penShape=0 then PenLineCap.Round else PenLineCap.Square
            pen.EndLineCap <- if penShape=0 then PenLineCap.Round else PenLineCap.Square
            pen.LineJoin <- if penShape=0 then PenLineJoin.Round else PenLineJoin.Miter
        let sizeMe() =
            let r = WinteropUtils.GetActiveWindowClientRect()
            this.Left <- float(r.left)
            this.Top <- float(r.top)
            this.Width <- float(r.right - r.left)
            this.Height <- float(r.bottom - r.top)
            spotlightCanvas.Width <- this.Width
            spotlightCanvas.Height <- this.Height
            catchAll.Width <- this.Width
            catchAll.Height <- this.Height
            penCanvas.Width <- this.Width
            penCanvas.Height <- this.Height
            borderRect.Width <- this.Width
            borderRect.Height <- this.Height
            this.Content <- total
        let eraseSpotlightOpacityMask() =
            let b = new SolidColorBrush(Color.FromArgb(0x01uy,0xFFuy,0xFFuy,0xFFuy))
            let all = new RectangleGeometry(Rect(0., 0., 1., 1.))
            let spotlight = new DrawingGroup()
            spotlight.Children.Add(new GeometryDrawing(b,null,all))
            let db = new DrawingBrush(Drawing=spotlight)
            spotlightCanvas.OpacityMask <- db
        let updateClickThruMode(clickThru) =
            //printfn "clickthru: %A" clickThru
            isCurrentlyClickThru <- clickThru
            if clickThru then
                borderRect.Stroke <- Brushes.Transparent
                watermark.Opacity <- 0.
            else
                borderRect.Stroke <- Brushes.DarkOrange
                watermark.Opacity <- 1.
        let spotlight = // x,y,w,h     0 to 1 range
            let darkenBrush = new SolidColorBrush(Color.FromArgb(0xAAuy,0xFFuy,0xFFuy,0xFFuy))
            let all = new RectangleGeometry(Rect(0., 0., 1., 1.))
            let spotlight = new DrawingGroup()
            spotlight.Children.Add(new GeometryDrawing(darkenBrush,null,all))
            let db = new DrawingBrush()
            db.Drawing <- spotlight
            let f(x,y,w,h) =
                spotlight.ClipGeometry <- new CombinedGeometry(GeometryCombineMode.Exclude,all,
                    if penShape=0 then upcast new EllipseGeometry(Rect(x,y,w,h)) : Geometry else upcast new RectangleGeometry(Rect(x,y,w,h)))
                spotlightCanvas.OpacityMask <- db
            f
        eraseSpotlightOpacityMask()
        let rename(s) = 
            let name = if s = null then "LorgonGlass" else "LorgonGlass " + s
            this.Title <- name
        rename(null)
        let erase() =
            if drawingMode=0 then
                eraseSpotlightOpacityMask()
            elif drawingMode=1 then
                penCanvas.Children.Clear()
                redoStack.Clear()
        this.Loaded.Add(fun _ ->
            let cw = new ControlsWindow(this, rename, erase, sizeMe, updateClickThruMode, updatePenShape, updateMode, (fun b -> drawArrowheads <- b), (fun c -> pen.Brush <- c))
            cw.Show()
            myControlsWindow <- cw
            thisHwnd <- System.Windows.Interop.WindowInteropHelper(this).Handle
            controlsHwnd <- System.Windows.Interop.WindowInteropHelper(cw).Handle
            )
        let mutable startPoint = None
        let mutable geoGroup = new GeometryGroup()
        let mutable tempRect = null
        catchAll.MouseLeftButtonDown.Add(fun ea -> 
            ea.Handled <- true
            if not(myControlsWindow.IsFirstClickFocusSwitch) then
                startPoint <- Some(ea.GetPosition(catchAll))
                if drawingMode=1 then
                    geoGroup <- new GeometryGroup()
                    tempRect <- new System.Windows.Shapes.Rectangle(Fill=new DrawingBrush(Drawing=new GeometryDrawing(Geometry=geoGroup, Pen=pen.Clone())))
                    penCanvas.Children.Add(tempRect) |> ignore
                    redoStack.Clear()
                )
        let adjustRect() =
            tempRect.Width <- geoGroup.Bounds.Width + pen.Thickness
            tempRect.Height <- geoGroup.Bounds.Height + pen.Thickness
            Canvas.SetLeft(tempRect, geoGroup.Bounds.Left - pen.Thickness/2.0)
            Canvas.SetTop(tempRect, geoGroup.Bounds.Top - pen.Thickness/2.0)
        let extendPenDraw(e:Point) =
            match startPoint with
            | None -> ()
            | Some(p) ->
                geoGroup.Children.Add(new LineGeometry(StartPoint=p, EndPoint=e))
                startPoint <- Some(e)
                adjustRect()
        CompositionTarget.Rendering.Add(fun _ ->
            let e = Input.Mouse.GetPosition(catchAll)
            if e.X>=0. && e.X<=catchAll.Width && e.Y>=0. && e.Y<=catchAll.Height then
                myCursor.Opacity <- 1.0
            else
                myCursor.Opacity <- 0.0
            )
        let finishStroke(pos:Point) =
            if drawingMode=0 then
                match startPoint with
                | None -> ()
                | Some(startPoint) ->
                    let endPoint = pos
                    let sx,ex = if startPoint.X > endPoint.X then endPoint.X,startPoint.X else startPoint.X,endPoint.X
                    let sy,ey = if startPoint.Y > endPoint.Y then endPoint.Y,startPoint.Y else startPoint.Y,endPoint.Y
                    let x = sx / spotlightCanvas.Width
                    let y = sy / spotlightCanvas.Height
                    let w = (ex - sx) / spotlightCanvas.Width
                    let h = (ey - sy) / spotlightCanvas.Height
                    spotlight(x,y,w,h)
            elif drawingMode=1 then
                match startPoint with
                | None -> ()
                | Some(_) ->
                    let e = pos
                    extendPenDraw(e)
                    if geoGroup.Children.Count > 0 && tempRect<> null then
                        if drawArrowheads then
                            let A = pen.Thickness * 2.0
                            let mutable i = geoGroup.Children.Count-1
                            let mutable lg = geoGroup.Children.Item(i) :?> LineGeometry
                            let e = lg.EndPoint
                            let sq(x) = x*x
                            // find a long enough bit of 'stroke' suffix to decide which direction the end of the stroke is pointing
                            while i>0 && sqrt(sq(lg.StartPoint.X-e.X)+sq(lg.StartPoint.Y-e.Y))<A do
                                i <- i - 1
                                lg <- geoGroup.Children.Item(i) :?> LineGeometry
                            // if we found one, then draw arrowhead, else (e.g. in case they just clicked to draw a 'dot' with no direction) don't
                            if not(sqrt(sq(lg.StartPoint.X-e.X)+sq(lg.StartPoint.Y-e.Y))<A) then
                                // compute arrowhead points...
                                let tx,ty = e.X, e.Y          // target
                                let sx,sy = lg.StartPoint.X, lg.StartPoint.Y      // source
                                let pct = 1. - A/sqrt(sq(tx-sx)+sq(ty-sy))   // arrowhead base A pixels down the line
                                let ax,ay = (tx-sx)*pct+sx, (ty-sy)*pct+sy
                                // ...differential between target and arrowhead base
                                let dx,dy = tx-ax, ty-ay
                                // ...points orthogonal to the line from the base
                                let SF = 1.5   // spread factor
                                let p1x,p1y = ax+dy/SF, ay-dx/SF
                                let p2x,p2y = ax-dy/SF, ay+dx/SF
                                // make arrowhead
                                geoGroup.Children.Add(new LineGeometry(StartPoint=Point(p1x,p1y), EndPoint=e))
                                geoGroup.Children.Add(new LineGeometry(StartPoint=Point(p2x,p2y), EndPoint=e))
                        adjustRect()
            startPoint <- None
            tempRect <- null
        catchAll.MouseMove.Add(fun ea ->
            ea.Handled <- true
            let e = ea.GetPosition(catchAll)
            Canvas.SetLeft(myCursor, e.X)
            Canvas.SetTop(myCursor, e.Y)
            if ea.LeftButton = Input.MouseButtonState.Pressed then
                if drawingMode=1 then
                    extendPenDraw(e)
            else
                // sometimes clicking into window/activation it sees the mousedown but misses the mouseup and draw without button held
                match startPoint with
                | None -> ()
                | Some(_) ->
                    finishStroke(e)
            )
        catchAll.MouseLeftButtonUp.Add(fun ea -> 
            ea.Handled <- true
            finishStroke(ea.GetPosition(catchAll))
            )
        this.PreviewKeyDown.Add(fun ea ->
            if ea.Key = System.Windows.Input.Key.Z && System.Windows.Input.Keyboard.Modifiers.HasFlag(System.Windows.Input.ModifierKeys.Control) then
                ea.Handled <- true
                if penCanvas.Children.Count > 0 then
                    let x = penCanvas.Children.[penCanvas.Children.Count - 1]
                    penCanvas.Children.RemoveAt(penCanvas.Children.Count - 1)
                    redoStack.Push(x)
            if ea.Key = System.Windows.Input.Key.Y && System.Windows.Input.Keyboard.Modifiers.HasFlag(System.Windows.Input.ModifierKeys.Control) then
                ea.Handled <- true
                if redoStack.Count <> 0 then
                    penCanvas.Children.Add(redoStack.Pop()) |> ignore
            )
        

