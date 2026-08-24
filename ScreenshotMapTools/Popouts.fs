module Popouts

open System.Windows
open System.Windows.Controls
open System.Windows.Media

let MakeWindowChromelessAndHandleClicksForMoveAndClose(w:Window) =
    let customChrome = new System.Windows.Shell.WindowChrome(CaptionHeight=0, ResizeBorderThickness = Thickness(8), GlassFrameThickness = new Thickness(0), CornerRadius = new CornerRadius(0))
    System.Windows.Shell.WindowChrome.SetWindowChrome(w, customChrome)
    w.MouseDown.Add(fun ea ->
        if ea.ChangedButton = System.Windows.Input.MouseButton.Left then
            ea.Handled <- true
            w.DragMove()
        )
    w.MouseUp.Add(fun ea ->
        if ea.ChangedButton = System.Windows.Input.MouseButton.Right then
            ea.Handled <- true
            w.Close()
        )

//////////////////////////////////////////////////////////////////////////

type ControlsCheatsheetPopoutWindow(owner) as this =
    inherit Window()
    let g = new Grid()
    let b = new Border(BorderThickness=Thickness(6.), Child=g, Background=Brushes.Gray, BorderBrush=Brushes.Gray)
    do
        this.Owner <- owner
        this.Title <- "Controls cheatsheet"
        this.Loaded.Add(fun _ ->
            ()
            )
        this.Closed.Add(fun _ ->
            ()
            )
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        this.Content <- b
        this.Width <- 220.
        this.Height <- 24. * 10. + 12.
        g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(50.)))
        g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength.Auto))
        for i = 0 to 9 do
            g.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.)))
        let mkTxt(txt) = new TextBlock(IsHitTestVisible=false, FontSize=16., FontWeight=FontWeights.Bold, Text=txt, Foreground=Brushes.Black, Background=Brushes.Transparent)
        Utils.gridAdd(g, mkTxt("2468"), 0, 0)
        Utils.gridAdd(g, mkTxt("move cursor"), 1, 0)
        Utils.gridAdd(g, mkTxt("0"), 0, 1)
        Utils.gridAdd(g, mkTxt("take screenshot"), 1, 1)
        Utils.gridAdd(g, mkTxt("- +"), 0, 2)
        Utils.gridAdd(g, mkTxt("cut/paste"), 1, 2)
        Utils.gridAdd(g, mkTxt("7 9"), 0, 3)
        Utils.gridAdd(g, mkTxt("zoom out/in"), 1, 3)
        Utils.gridAdd(g, mkTxt("*"), 0, 4)
        Utils.gridAdd(g, mkTxt("cycle zone"), 1, 4)
        Utils.gridAdd(g, mkTxt("/"), 0, 5)
        Utils.gridAdd(g, mkTxt("edit notes"), 1, 5)
        Utils.gridAdd(g, mkTxt("1"), 0, 6)
        Utils.gridAdd(g, mkTxt("pan/zoom window"), 1, 6)
        Utils.gridAdd(g, mkTxt("ctrl1"), 0, 7)
        Utils.gridAdd(g, mkTxt("2x map pan/zoom"), 1, 7)
        Utils.gridAdd(g, mkTxt("."), 0, 8)
        Utils.gridAdd(g, mkTxt("toggle TODO tag"), 1, 8)
        Utils.gridAdd(g, mkTxt("3"), 0, 9)
        Utils.gridAdd(g, mkTxt("edit TODO tag"), 1, 9)


//////////////////////////////////////////////////////////////////////////

module LocalWinterop =
    open System.Runtime.InteropServices
    type IntPtr = System.IntPtr
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type RECT =
        val mutable Left: int
        val mutable Top: int
        val mutable Right: int
        val mutable Bottom: int
    let WM_SIZING = 0x0214
    let WMSZ_LEFT = 1
    let WMSZ_RIGHT = 2
    let WMSZ_TOP = 3
    let WMSZ_BOTTOM = 6
    let LockWindowAspectRatioButAllowResizing(this:Window, minWidth, minHeight, aspectRatio, expectChrome) =
        // Hook the window lifecycle on initialization
        this.SourceInitialized.Add(fun _ ->
            let chromeW, chromeH =
                if expectChrome then
                    SystemParameters.WindowResizeBorderThickness.Left + SystemParameters.WindowResizeBorderThickness.Right, // plus SystemParameters.FixedFrameHorizontalBorderHeight depending on your window style
                        SystemParameters.WindowCaptionHeight + SystemParameters.WindowResizeBorderThickness.Top + SystemParameters.WindowResizeBorderThickness.Bottom
                else
                    0., 0.
            let helper = System.Windows.Interop.WindowInteropHelper(this)
            let source = System.Windows.Interop.HwndSource.FromHwnd(helper.Handle)
            if source <> null then
                source.AddHook(System.Windows.Interop.HwndSourceHook(fun (hwnd: IntPtr) (msg: int) (wParam: IntPtr) (lParam: IntPtr) (handled: byref<bool>) ->
                        // Intercept sizing messages and modify the bounding rectangle
                        if msg = WM_SIZING then
                            let mutable rect = System.Runtime.InteropServices.Marshal.PtrToStructure<RECT>(lParam)
                            // Calculate current dragged dimensions, first subtracting window chrome
                            let mutable width = rect.Right - rect.Left - int chromeW
                            let mutable height = rect.Bottom - rect.Top - int chromeH
                            let side = wParam.ToInt32()
                            // 1. Apply Minimum Bounds Check
                            if float width < minWidth then
                                width <- int minWidth
                                if side = WMSZ_LEFT then rect.Left <- rect.Right - width
                                else rect.Right <- rect.Left + width
                            if float height < minHeight then
                                height <- int minHeight
                                if side = WMSZ_TOP then rect.Top <- rect.Bottom - height
                                else rect.Bottom <- rect.Top + height
                            // 2. Adjust for Aspect Ratio based on drag direction
                            if side = WMSZ_LEFT || side = WMSZ_RIGHT then
                                // Dragging horizontally: force height to match width
                                let newHeight = int (float width / aspectRatio)
                                rect.Bottom <- rect.Top + newHeight
                            elif side = WMSZ_TOP || side = WMSZ_BOTTOM then
                                // Dragging vertically: force width to match height
                                let newWidth = int (float height * aspectRatio)
                                rect.Right <- rect.Left + newWidth
                            else
                                // Dragging a corner: prioritize width changes
                                let newHeight = int (float width / aspectRatio)
                                rect.Bottom <- rect.Top + newHeight
                            // add back window chrome
                            rect.Right <- rect.Right + int chromeW
                            rect.Bottom <- rect.Bottom + int chromeH
                            // Marshal changes back to Windows
                            System.Runtime.InteropServices.Marshal.StructureToPtr(rect, lParam, false)
                            handled <- true
                        IntPtr.Zero
                )))
        

type VisualPopoutWindow(owner, title, viz:Visual, aspect) as this =
    inherit Window()
    let g = new Grid()
    do
        this.Owner <- owner
        this.Title <- title
        this.Height <- 300.
        this.Width <- this.Height * aspect
        this.Content <- g
        LocalWinterop.LockWindowAspectRatioButAllowResizing(this, 100., 100., aspect, true)
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        this.Loaded.Add(fun _ ->
            g.Background <- new VisualBrush(viz)
            )
        this.Closed.Add(fun _ ->
            ()
            )


//////////////////////////////////////////////////////////////////////////

type ZoomableLiveMinimapWindow(owner, aspect, getProjection:InMemoryStore.ZoneMemory->InMemoryStore.ImgArrayCache, x, y, zm:InMemoryStore.ZoneMemory, updateEv:IEvent<int*int*InMemoryStore.ZoneMemory>) as this =
    inherit Window()
    let mutable curZoomStep = 3
    let b = new Border(Background=Brushes.DarkMagenta)
    let mutable curX, curY, curZm = x, y, zm
    let redraw() =
        let gr = FeatureWindow.GridRange(InMemoryStore.MAX,InMemoryStore.MAX,0,0)
        let bmpDict = new System.Collections.Generic.Dictionary<_,_>()
        for i = curX-curZoomStep to curX+curZoomStep do
            for j = curY-curZoomStep to curY+curZoomStep do
                let bmp = getProjection(curZm).GetCopyOfBmp(i,j)            // TODO if outside wrap range, cycle to grab image, e.g. treat k as ((k-min)%width)+min
                bmpDict[(i,j)] <- bmp
                if bmp <> null || (i=curX && j=curY) then
                    gr.Extend(i,j)
        if not(gr.MaxX >= gr.MinX) then
            b.Child <- null
        else              // there was at least one screenshot
            // get an NxN area 'centered' on the cursor
            while gr.Width > curZoomStep do
                if curX-gr.MinX > gr.MaxX-curX then
                    gr.MinX <- gr.MinX + 1
                else
                    gr.MaxX <- gr.MaxX - 1
            while gr.Height > curZoomStep do
                if curY-gr.MinY > gr.MaxY-curY then
                    gr.MinY <- gr.MinY + 1
                else
                    gr.MaxY <- gr.MaxY - 1
            // make a grid of images
            let w,h = this.Width/float curZoomStep, this.Height/float curZoomStep
            let g = Utils.makeGridF(curZoomStep, curZoomStep, w, h)
            g.Width <- this.Width
            g.Height <- this.Height
            for i = 0 to curZoomStep-1 do
                for j = 0 to curZoomStep-1 do
                    let bmp = bmpDict.[i+gr.MinX, j+gr.MinY]
                    if bmp <> null then
                        let img = Utils.BMPtoImage(bmp)
                        img.Width <- w
                        img.Height <- h
                        img.Stretch <- Stretch.Fill
                        Utils.gridAdd(g, img, i, j)
                    if i+gr.MinX = curX && j+gr.MinY = curY then
                        let rect = new Shapes.Rectangle(Width=w, Height=h, Stroke=Brushes.Yellow, StrokeThickness=3.)
                        Utils.gridAdd(g, rect, i, j)
            b.Child <- g                                // TODO also add thick gridline if wrapedge
    do
        this.Owner <- owner
        this.Title <- "Zoomable Live Minimap"
        this.Height <- 300.
        this.Width <- this.Height * aspect
        this.Content <- b
        LocalWinterop.LockWindowAspectRatioButAllowResizing(this, 100., 100., aspect, false)
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        updateEv.Add(fun (x,y,zm) -> 
            curX <- x
            curY <- y
            curZm <- zm
            redraw()
            )
        b.MouseWheel.Add(fun ea ->
            if ea.Delta > 0 then 
                curZoomStep <- curZoomStep + 1
            else
                curZoomStep <- curZoomStep - 1
            curZoomStep <- max curZoomStep 1        // 1 is smallest can go
            redraw()
            )
        this.SizeChanged.Add(fun _ -> redraw())
        this.Loaded.Add(fun _ ->
            ()
            )
        this.Closed.Add(fun _ ->
            ()
            )

//////////////////////////////////////////////////////////////////////////

[<RequireQualifiedAccess>]
type EditNotesListenerMessage = 
    | StartEditing
    | Edit of string*int*int*int   // tb.Text, tb.CaretIndex, tb.SelectionStart, tb.SelectionLength
    | FinishEditing

[<AllowNullLiteral>]
type LiveNotesWindow(owner, x, y, zm, updateEv:IEvent<int*int*InMemoryStore.ZoneMemory>) as this =
    inherit Window()
    static let mutable theLiveNotesWindow : LiveNotesWindow = null
    let mutable curX, curY, (curZm : InMemoryStore.ZoneMemory) = x, y, zm
    let mutable fontSize = 20
    let tb = new TextBlock(FontSize=float(fontSize), Foreground=Brushes.White, Background=Brushes.Transparent,
                                FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, IsHitTestVisible=false, 
                                HorizontalAlignment=HorizontalAlignment.Stretch, TextWrapping=TextWrapping.Wrap, 
                                Margin=Thickness(3.))
    let sv = new ScrollViewer(VerticalScrollBarVisibility=ScrollBarVisibility.Hidden, Content=tb, IsHitTestVisible=false)
    let UpdateStaticNote() =
        let note = curZm.MapTiles.[curX,curY].Note
        tb.Text <- 
            if System.String.IsNullOrEmpty(note) then 
                tb.Foreground <- Brushes.Gray
                "<no note>" 
            else 
                tb.Foreground <- Brushes.White
                note
    let blinkyBrush = 
        let colorAnimation = new System.Windows.Media.Animation.ColorAnimation()
        colorAnimation.From <- System.Nullable<_>(System.Windows.Media.Colors.Yellow)
        colorAnimation.To <- System.Nullable<_>(System.Windows.Media.Colors.DarkMagenta)
        colorAnimation.Duration <- new Duration(System.TimeSpan.FromSeconds(0.5))
        colorAnimation.AutoReverse <- true
        colorAnimation.RepeatBehavior <- System.Windows.Media.Animation.RepeatBehavior.Forever
        let brush = new SolidColorBrush(Colors.Black)
        brush.BeginAnimation(SolidColorBrush.ColorProperty, colorAnimation)
        brush
    do
        this.Owner <- owner
        this.Title <- "Note at cursor"
        this.UseLayoutRounding <- true
        this.Width <- 300.
        this.Height <- 80.
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        this.Loaded.Add(fun _ ->
            theLiveNotesWindow <- this
            )
        this.Closed.Add(fun _ ->
            theLiveNotesWindow <- null
            )
        let b = new Border(BorderThickness=Thickness(3.), Background=Brushes.DarkMagenta, BorderBrush=Brushes.DarkMagenta)
        b.MouseWheel.Add(fun ea ->
            if ea.Delta > 0 then
                fontSize <- fontSize + 2
            else
                fontSize <- fontSize - 2
            // clamp values
            fontSize <- max 8 fontSize
            fontSize <- min 72 fontSize
            tb.FontSize <- float fontSize
        )
        this.Content <- b
        tb.Width <- System.Double.NaN
        tb.Height <- System.Double.NaN
        b.Child <- sv
        updateEv.Add(fun (x,y,zm) ->
            curX <- x
            curY <- y
            curZm <- zm
            UpdateStaticNote()
            )
    member this.StartEdit() = 
        tb.Foreground <- Brushes.Lime
    member this.NoteEdit(fullText:string,_caretIndex,selectionStart,selectionLength) = 
        let start = selectionStart
        tb.Inlines.Clear()
        let textBefore = fullText.Substring(0, start)
        let textSelected = fullText.Substring(start, selectionLength)
        let textAfter = fullText.Substring(start + selectionLength)
        if not (System.String.IsNullOrEmpty(textBefore)) then
            tb.Inlines.Add(System.Windows.Documents.Run(textBefore))
        if true then
            let caretElement = new Border()
            caretElement.Width <- 2.0
            caretElement.Height <- float fontSize
            caretElement.Background <- blinkyBrush
            caretElement.Margin <- new Thickness(-1, 2, -1, -2)
            let caretContainer = new System.Windows.Documents.InlineUIContainer(caretElement);
            tb.Inlines.Add(caretContainer)
            Application.Current.Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Background, new System.Action(fun () -> caretContainer.BringIntoView())) |> ignore
        if not (System.String.IsNullOrEmpty(textSelected)) then
            let selectionRun = System.Windows.Documents.Run(textSelected)
            selectionRun.Background <- Brushes.Gray
            selectionRun.Foreground <- Brushes.Lime
            tb.Inlines.Add(selectionRun)
        if not (System.String.IsNullOrEmpty(textAfter)) then
            tb.Inlines.Add(System.Windows.Documents.Run(textAfter))
    member this.FinishEdit() = 
        UpdateStaticNote()
        sv.ScrollToTop()
    static member TheNotesWindow with get() = theLiveNotesWindow and set(x) = theLiveNotesWindow <- x

let theEditNotesListenerEvent = new Event<EditNotesListenerMessage>()
do
    theEditNotesListenerEvent.Publish.Add(fun msg ->
        if LiveNotesWindow.TheNotesWindow <> null then
            match msg with 
            | EditNotesListenerMessage.StartEditing -> LiveNotesWindow.TheNotesWindow.StartEdit()
            | EditNotesListenerMessage.Edit(t,ci,ss,sl) -> LiveNotesWindow.TheNotesWindow.NoteEdit(t,ci,ss,sl)
            | EditNotesListenerMessage.FinishEditing -> LiveNotesWindow.TheNotesWindow.FinishEdit()
        )
