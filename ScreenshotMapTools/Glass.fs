module Glass

open System
open System.Windows
open System.Windows.Media
open System.Threading
open System.Runtime.InteropServices
open System.Text
open System.Windows.Threading
open System.Windows.Controls

module Winterop =
    [<DllImport("user32.dll")>]
    extern IntPtr GetForegroundWindow()
    [<DllImport("user32.dll")>]
    extern int GetWindowText(IntPtr hWnd, StringBuilder text, int count)

    [<DllImport("USER32.DLL")>]
    extern bool SetForegroundWindow(IntPtr hwnd)
    [<DllImport("USER32.DLL")>]
    extern bool ShowWindow(IntPtr hWnd, int nCmdShow)
    [<DllImport("USER32.DLL")>]
    extern IntPtr SetWindowLongPtrA(IntPtr hWnd, int nIndex, IntPtr dwNewLong)
    [<DllImport("USER32.DLL")>]
    extern IntPtr GetWindowLongPtrA(IntPtr hWnd, int nIndex)
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type POINT =
        val mutable x:int
        val mutable y:int
    [<DllImport("USER32.DLL", SetLastError = true)>]
    extern bool ClientToScreen(IntPtr hWnd, [<In;Out>] POINT& lpPoint)

    [<DllImport("user32.dll", SetLastError = true)>]
    extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int x, int y, int cx, int cy, uint32 uFlags)
    [<DllImport("user32.dll", SetLastError = true)>]
    extern IntPtr GetWindow(IntPtr hWnd, uint32 uCmd)
    [<DllImport("user32.dll", SetLastError = true)>]
    extern IntPtr GetTopWindow(IntPtr hWnd)
    [<DllImport("user32.dll", SetLastError = true)>]
    extern bool IsWindowVisible(IntPtr hWnd)

// Win32 Constants
let GW_HWNDNEXT = 2u
let GW_HWNDPREV = 3u
let HWND_NOTOPMOST = nativeint -2
let HWND_TOP = nativeint 0
let SWP_NOMOVE     = 0x0002u
let SWP_NOSIZE     = 0x0001u
let SWP_NOACTIVATE = 0x0010u

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

(*
// see also https://www.codeproject.com/Articles/12877/Transparent-Click-Through-Forms
let MakeNonActivateable(w:Window) =
    let helper = new System.Windows.Interop.WindowInteropHelper(w)
    let hwnd = helper.Handle
    let SW_HIDE, SW_SHOW = 0, 5
    let GWL_EXSTYLE = -20
    let WS_EX_NOACTIVATE, WS_EX_APPWINDOW = 0x08000000L, 0x00040000L
    Winterop.ShowWindow(hwnd, SW_HIDE) |> ignore
    Winterop.SetWindowLongPtrA(hwnd, GWL_EXSTYLE, IntPtr((int64(Winterop.GetWindowLongPtrA(hwnd, GWL_EXSTYLE))) ||| WS_EX_NOACTIVATE ||| WS_EX_APPWINDOW)) |> ignore
    Winterop.ShowWindow(hwnd, SW_SHOW) |> ignore
*)
let GetWindowTitle(hwnd) =
    let N = 256
    let buff = new StringBuilder(N)
    if (Winterop.GetWindowText(hwnd, buff, N) > 0) then
        buff.ToString()
    else
        null
let GetActiveWindowTitle() =
    let handle = Winterop.GetForegroundWindow()
    GetWindowTitle(handle)

let GetActiveWindowClientRect() =
    let handle = Winterop.GetForegroundWindow()
    let mutable r : Elephantasy.Screenshot.RECT = Unchecked.defaultof<_>
    if Elephantasy.Screenshot.GetClientRect(handle, &r) = false then failwith "bad window"
    let mutable p : Winterop.POINT = Unchecked.defaultof<_>
    if Winterop.ClientToScreen(handle, &p) = false then failwith "bad window"
    r.left <- r.left + p.x
    r.right <- r.right + p.x
    r.top <- r.top + p.y
    r.bottom <- r.bottom + p.y
    r

let debugOutput = true
let debugWindowZOrder() =
    if debugOutput then
        let mutable hwndCur = Winterop.GetTopWindow(IntPtr(0))
        let mutable count = 0
        printfn "Current top of window stack:"
        while hwndCur <> IntPtr(0) && count < 5 do
            if Winterop.IsWindowVisible(hwndCur) then
                let title = GetWindowTitle(hwndCur)
                if not(System.String.IsNullOrEmpty(title)) then
                    printfn "    %s" (GetWindowTitle hwndCur)
                    count <- count + 1
            hwndCur <- Winterop.GetWindow(hwndCur, GW_HWNDNEXT)

type ControlsWindow(parentGlass : Window, eraseF, sizeParentF, updateClickThruModeF, updatePenShapeF, updateModeF, updateDrawArrowHeadsF, updatePenColorF) as this =
    inherit Window()
    let hwndParentGlass = System.Windows.Interop.WindowInteropHelper(parentGlass).Handle
    let mutable clickThru = false
    let mutable hwndGlassTarget = IntPtr(0)
    let mutable ignoreChanges = false
    let moveGlassAtopTarget(debugStr) =
        if hwndGlassTarget <> IntPtr(0) then
            async {
                do! Async.Sleep(1)  // pump ui
                // move the glass to just above the glass target
                let nextAboveTarget = Winterop.GetWindow(hwndGlassTarget, GW_HWNDPREV)
                debugWindowZOrder()
                if debugOutput then printfn "moving glass(%s) above target %s" debugStr (GetWindowTitle hwndGlassTarget)
                ignoreChanges <- true
                Winterop.SetWindowPos(hwndParentGlass, nextAboveTarget, 0, 0, 0, 0, SWP_NOMOVE ||| SWP_NOSIZE) |> ignore
                do! Async.Sleep(1)  // pump ui
                ignoreChanges <- false
                debugWindowZOrder()
            } |> Async.StartImmediate
        else
            printfn "mgat-no target yet"
    let activateTargetAndMoveGlass(debugStr) =
        if hwndGlassTarget <> IntPtr(0) then
            async {
                // activate the target
                do! Async.Sleep(1)  // pump ui
                if debugOutput then printfn "activating target (%s)" debugStr
                debugWindowZOrder()
                ignoreChanges <- true
                Winterop.SetWindowPos(hwndGlassTarget, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE ||| SWP_NOSIZE) |> ignore
                do! Async.Sleep(1)  // pump ui
                moveGlassAtopTarget(sprintf "atamg(%s)" debugStr)
            } |> Async.StartImmediate
        else
            printfn "atamg-no target yet"
    let focusChanged(_prevHwnd, newHwnd) =
        if not ignoreChanges then
            if hwndGlassTarget <> IntPtr(0) then
                if newHwnd = hwndGlassTarget then
                    if debugOutput then printfn "glass target"
                    moveGlassAtopTarget("focus changed to glass target")
                elif newHwnd = hwndParentGlass then
                    if debugOutput then printfn "glass itself"
                    activateTargetAndMoveGlass("focus changed to glass")
            else
                printfn "fc-no target yet"
    let watcherId = WindowFocusTrackingUtils.addWindowFocusWatcher(focusChanged)
    let label = new Label(Content="switch to click-thru")
    let toggleClickThruButton = new Button(Content=label, Margin=Thickness(2.))
    do
        this.Owner <- parentGlass
        this.Title <- "GlassControl"
        this.Loaded.Add(fun _ ->
            //printfn "loading controls"
            this.Content <- new TextBox(Text="focus the window you\nwant to draw on top of", Margin=Thickness(8.), BorderThickness=Thickness(0.))
            async {
                //printfn "waiting for user to focus another app"
                let! _ = Async.AwaitEvent this.Deactivated
                do! Async.Sleep(100)  // ensure time for OS to activate other app
                //printfn "saw focus change"
                this.Activated.Add(fun _ -> 
                    // if they click on the controls window, bring the target to top as well
                    // but first process whatever they clicked on
                    async {
                        do! Async.Sleep(100) // kludgy - pump ui to process the click e.g. on a button, before reordering windows
                        activateTargetAndMoveGlass("control window was activated")
                    } |> Async.StartImmediate
                    )  
                sizeParentF()
                updateClickThruModeF(clickThru)
                hwndGlassTarget <- Winterop.GetForegroundWindow()
                let r = GetActiveWindowClientRect()
                this.Top <- float(r.bottom + 4)
                this.Left <- float(r.left)
                let phelper = new System.Windows.Interop.WindowInteropHelper(parentGlass)
                let phwnd = phelper.Handle
                let SW_HIDE, SW_SHOW = 0, 5
                let GWL_EXSTYLE = -20
                let WS_EX_LAYERED = 0x00080000L
                let WS_EX_TRANSPARENT = 0x00000020L
                toggleClickThruButton.Click.Add(fun _ ->
                    if not clickThru then
                        Winterop.ShowWindow(phwnd, SW_HIDE) |> ignore
                        Winterop.SetWindowLongPtrA(phwnd, GWL_EXSTYLE, IntPtr((int64(Winterop.GetWindowLongPtrA(phwnd, GWL_EXSTYLE))) ||| WS_EX_LAYERED ||| WS_EX_TRANSPARENT)) |> ignore
                        Winterop.ShowWindow(phwnd, SW_SHOW) |> ignore
                    else
                        Winterop.ShowWindow(phwnd, SW_HIDE) |> ignore
                        Winterop.SetWindowLongPtrA(phwnd, GWL_EXSTYLE, IntPtr((int64(Winterop.GetWindowLongPtrA(phwnd, GWL_EXSTYLE))) &&& (~~~(WS_EX_LAYERED ||| WS_EX_TRANSPARENT)))) |> ignore
                        Winterop.ShowWindow(phwnd, SW_SHOW) |> ignore
                    clickThru <- not clickThru
                    label.Content <- if clickThru then "switch to drawing" else "switch to click-thru"
                    updateClickThruModeF(clickThru)
                    )
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
            WindowFocusTrackingUtils.removeWindowFocusWatcher(watcherId)
            async { 
                let ctxt = SynchronizationContext.Current
                do! Async.Sleep(20)
                do! Async.SwitchToContext(ctxt)
                parentGlass.Close() 
                } |> Async.StartImmediate
            )

type DrawingGlassWindow() as this =
    inherit Window()
    do
        this.Title <- "LorgonGlass"
        this.SizeToContent <- SizeToContent.Manual
        this.WindowStartupLocation <- WindowStartupLocation.Manual
        this.Background <- Brushes.Transparent
        this.AllowsTransparency <- true
        this.WindowStyle <- WindowStyle.None
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
            let r = GetActiveWindowClientRect()
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
        let erase() =
            if drawingMode=0 then
                eraseSpotlightOpacityMask()
            elif drawingMode=1 then
                penCanvas.Children.Clear()
                redoStack.Clear()
        this.Loaded.Add(fun _ ->
            let cw = new ControlsWindow(this, erase, sizeMe, updateClickThruMode, updatePenShape, updateMode, (fun b -> drawArrowheads <- b), (fun c -> pen.Brush <- c))
            cw.Show()
            )
        let mutable startPoint = None
        let mutable geoGroup = new GeometryGroup()
        let mutable tempRect = null
        catchAll.MouseLeftButtonDown.Add(fun ea -> 
            //printfn "mouse LB down, active: %A" this.IsActive
            ea.Handled <- true
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
(*
        // TODO remove this?
        catchAll.MouseRightButtonDown.Add(fun ea -> 
            ea.Handled <- true
            async { 
                let ctxt = SynchronizationContext.Current
                do! Async.Sleep(20)
                do! Async.SwitchToContext(ctxt)
                this.Close() 
                } |> Async.StartImmediate
            )
*)
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
        this.MouseDown.Add(fun ea ->
            // ensure all clicks are handled and not passed down to the app under the glass
            ea.Handled <- true
            )
        

