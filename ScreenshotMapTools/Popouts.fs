module Popouts

open System.Windows
open System.Windows.Controls
open System.Windows.Media

//////////////////////////////////////////////////////////////////////////

module LocalWinterop =
    // Gemini
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
        
//////////////////////////////////////////////////////////////////////////

// To make popouts not all come to front when the app comes to front, they need to each live in their own UI dispatcher thread.
[<AllowNullLiteral>]
type IndependentWindow() =
    inherit Window()
    let mainUIDispatcher = Application.Current.Dispatcher
    let thisWindowDispatcher = System.Windows.Threading.Dispatcher.CurrentDispatcher
    do
        if obj.ReferenceEquals(mainUIDispatcher,thisWindowDispatcher) then
            failwith "created an IndependentWindow on the main UI thread"
    member this.EnsureOnMainUIThread() =
        if not(mainUIDispatcher.CheckAccess()) then
            failwith "EnsureOnMainUIThread failed"
    member this.EnsureOnThisWindowsOwnThread() =
        if not(thisWindowDispatcher.CheckAccess()) then
            failwith "EnsureOnMainUIThread failed"
    member this.ThreadSafeClose() =     // convenience
        thisWindowDispatcher.InvokeAsync(fun() -> this.Close()) |> ignore

// WPF Windows on their own threads will not be brought-to-front by Windows when the main window gets focus.  
let CreateAndShowWindowOnItsOwnUIDispatcherThreadCore(windowCreator:unit->IndependentWindow, uponLoadedF) =
    let thread = new System.Threading.Thread(fun() ->
        let win = windowCreator()
        win.Closed.Add(fun _ -> System.Windows.Threading.Dispatcher.CurrentDispatcher.InvokeShutdown())
        win.Loaded.Add(fun _ -> uponLoadedF())
        win.Show()
        System.Windows.Threading.Dispatcher.Run()
        )
    thread.SetApartmentState(System.Threading.ApartmentState.STA)
    thread.IsBackground <- true
    thread.Start()
let CreateAndShowWindowOnItsOwnUIDispatcherThread(windowCreator:unit->IndependentWindow) =
    CreateAndShowWindowOnItsOwnUIDispatcherThreadCore(windowCreator, fun() -> ())

let MainUIInvoke<'T>(f:unit->'T) = Application.Current.Dispatcher.Invoke(f)

(*
Since my model code (e.g. backing store) is not threadsafe, unless I want to rewrite all that, I will need to ensure that all the popout code
that interacts with the model marshals that work to the 'main' UI thread.

To avoid deadlocks, I should ensure I only Dispatcher.Invoke() in one direction (e.g. from popouts to main app, and never other way around).
That is, the main UI thread should only fire-and-forget notifications to the popout windows.

Since main app will fire events or call popout methods from main ui thread, I should put explicit guards on all popout entrypoints to ensure
I'm managing threads correctly.

Note that the popout constructor will run on its own thread, which means any model code needs to be Invoke()d on the main thread.

Make sure have right encapsulation boundaries architected to make it so that stuff above won't be error-prone!
*)

//////////////////////////////////////////////////////////////////////////

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

let MakeWindowSmartByRememberingPositionAndSize(w:Window, json:AppSettings.PopoutDetailJson) =    // call this in the constructor, after settting Width/Height(/Left/Top) to a default
    AppSettings.WindowPosition.SetInitialWindowPosition(w, json.XYWH)
    let save() =
        let xywh = ((int w.Left), (int w.Top), (int w.Width), (int w.Height))
        MainUIInvoke(fun() ->
            json.XYWH <- xywh
            AppSettings.theAppSettingsJson.Save()
            )
    w.SizeChanged.Add(fun _ -> save())
    w.LocationChanged.Add(fun _ -> save())

//////////////////////////////////////////////////////////////////////////

type ControlsCheatsheetPopoutWindow() as this =
    inherit IndependentWindow()
    static let mutable singleton = null
    let mkTxt(txt) = new TextBlock(IsHitTestVisible=false, FontSize=18., FontWeight=FontWeights.Bold, Text=txt, Foreground=Brushes.Black, Background=Brushes.Transparent)
    let g = new Grid()
    let sp = new StackPanel(Orientation=Orientation.Vertical)
    let b = new Border(BorderThickness=Thickness(6.), Child=sp, Background=Brushes.Gray, BorderBrush=Brushes.Gray, Margin=Thickness(0.,-4.,0.,0.))
    do
        sp.Children.Add(mkTxt("Be sure NumLock is on!")) |> ignore
        sp.Children.Add(new DockPanel(Height=2.0, Background=Brushes.Black)) |> ignore
        sp.Children.Add(g) |> ignore
        singleton <- this
        this.Width <- 220.
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        MakeWindowSmartByRememberingPositionAndSize(this, MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.ControlsCheatSheetPopout))
        this.Title <- ControlsCheatsheetPopoutWindow.Name
        this.Loaded.Add(fun _ ->
            ()
            )
        this.Closed.Add(fun _ ->
            singleton <- null
            )
        this.Content <- b
        this.ResizeMode <- ResizeMode.NoResize
        g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(50.)))
        g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength.Auto))
        let data = [|
                "2468",     "move cursor"
                "0",        "take screenshot"
                "-  +",     "cut  paste"
                "7 9",      "zoom out/in"
                "*",        "cycle zone"
                "/",        "edit note@cursor"
                "ctrl /",   "edit global note"
                "1",        "pan/zoom window"
                "ctrl 1",   "2x map pan/zoom"
                "3",        "toggle text macro"
                "ctrl 3",   "edit text macro"
            |]
        let COUNT = data.Length
        for i = 0 to COUNT-1 do
            g.RowDefinitions.Add(new RowDefinition(Height=GridLength(24.)))
            let a,b = data.[i]
            let at = mkTxt(a)
            at.TextAlignment <- TextAlignment.Right
            at.Padding <- Thickness(0.,0.,8.,0.)
            if at.Text="." || at.Text="*" || at.Text="-  +" then
                at.FontSize <- at.FontSize + 4.0
                at.Margin <- Thickness(0., -2., 0., 0.)
            Utils.gridAdd(g, at, 0, i)
            Utils.gridAdd(g, mkTxt(b), 1, i)
        this.Height <- 24. * float (COUNT+1) + 12.
    static member Singleton = singleton
    static member Name = "Cheatsheet"

(*
type VisualPopoutWindow(owner, title, viz:Visual, aspect) as this =
    inherit Window()
    static let mutable singleton = null
    let g = new Grid()
    do
        singleton <- this
        this.Height <- 300.
        this.Width <- this.Height * aspect
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        MakeWindowSmartByRememberingPositionAndSize(this, AppSettings.theAppSettingsJson.MapPanePopout)
        LocalWinterop.LockWindowAspectRatioButAllowResizing(this, 100., 100., aspect, false)
        this.Owner <- owner
        this.Title <- title
        this.Content <- g
        this.Loaded.Add(fun _ ->
            let vb = new VisualBrush(viz)
            RenderOptions.SetBitmapScalingMode(vb, BitmapScalingMode.HighQuality)
            g.Background <- vb
            )
        this.Closed.Add(fun _ ->
            singleton <- null
            )
    static member Singleton = singleton
*)

// same behavior as non-independent, but can run on own UI thread, which means can't use VisualBrush
type IndependentVisualPopoutWindow(updateEv:IEvent<System.Windows.Media.Imaging.BitmapSource>, w, h, thisName, popoutJsonF) as this =
    inherit IndependentWindow()
    let g = new Grid()
    let aspect = w/h
    do
        this.Height <- 300.
        this.Width <- this.Height * aspect
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        MakeWindowSmartByRememberingPositionAndSize(this, MainUIInvoke(popoutJsonF))
        LocalWinterop.LockWindowAspectRatioButAllowResizing(this, 100., 100., aspect, false)
        this.Title <- thisName
        this.Content <- g
        this.Loaded.Add(fun _ ->
                updateEv.Add(fun bms ->
                    this.Dispatcher.InvokeAsync(fun() ->
                        let ib = new ImageBrush(bms)
                        RenderOptions.SetBitmapScalingMode(ib, BitmapScalingMode.HighQuality)
                        g.Background <- ib
                        ) |> ignore
                )
            )

type AppGridPanePopoutWindow(updateEv:IEvent<System.Windows.Media.Imaging.BitmapSource>, w, h) as this =
    inherit IndependentVisualPopoutWindow(updateEv, w, h, AppGridPanePopoutWindow.Name, fun() -> AppSettings.theAppSettingsJson.AppGridPanePopout)
    static let mutable singleton = null
    do
        singleton <- this
        this.Closed.Add(fun _ ->
            singleton <- null
            )
    static member Singleton = singleton
    static member Name = "AppGridPane"

type AppPreviewPanePopoutWindow(updateEv:IEvent<System.Windows.Media.Imaging.BitmapSource>, w, h) as this =
    inherit IndependentVisualPopoutWindow(updateEv, w, h, AppGridPanePopoutWindow.Name, fun() -> AppSettings.theAppSettingsJson.AppPreviewPanePopout)
    static let mutable singleton = null
    do
        singleton <- this
        this.Background <- Brushes.DarkSlateBlue    // takes a moment to populate, don't flashbang with white
        this.Closed.Add(fun _ ->
            singleton <- null
            )
    static member Singleton = singleton
    static member Name = "AppPreviewPane"

//////////////////////////////////////////////////////////////////////////

type ZoomableLiveMinimapWindow(aspect, x, y, updateEv:IEvent<int*int>) as this =
    inherit IndependentWindow()
    static let mutable singleton = null
    let mutable curZoomStep, curZm = MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.LiveMinimapZoomLevel, InMemoryStore.ZoneMemory.Get(BackingStoreData.theGame.CurZone))
    let b = new Border(Background=Brushes.DarkMagenta)
    let mutable curX, curY = x, y
    let redraw() =
        this.EnsureOnThisWindowsOwnThread()
        let gr, bmpDict = MainUIInvoke(fun() ->
            let gr = FeatureWindow.GridRange(InMemoryStore.MAX,InMemoryStore.MAX,0,0)
            let bmpDict = new System.Collections.Generic.Dictionary<_,_>()
            for i = curX-curZoomStep to curX+curZoomStep do
                for j = curY-curZoomStep to curY+curZoomStep do
                    let bmp = 
                        if i>=0 && i<=99 && j>=0 && j<=99 then
                            curZm.MapImgArray.GetCopyOfBmp(i,j)            // TODO if outside wrap range, cycle to grab image, e.g. treat k as ((k-min)%width)+min
                        else null
                    bmpDict[(i,j)] <- bmp
                    if bmp <> null || (i=curX && j=curY) then
                        gr.Extend(i,j)
            gr, bmpDict
            )
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
                        RenderOptions.SetBitmapScalingMode(img, BitmapScalingMode.HighQuality)
                        Utils.gridAdd(g, img, i, j)
                    if i+gr.MinX = curX && j+gr.MinY = curY then
                        let rect = new Shapes.Rectangle(Width=w, Height=h, Stroke=Brushes.Yellow, StrokeThickness=3.)
                        Utils.gridAdd(g, rect, i, j)
            b.Child <- g                                // TODO also add thick gridline if wrapedge
    do
        singleton <- this
        this.Height <- 300.
        this.Width <- this.Height * aspect
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        MakeWindowSmartByRememberingPositionAndSize(this, MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.LiveMinimapPopout))
        // if they just performed a Trim, or switched games, the aspect may have changed since the last time the XYWH of this popout was saved; adjust the H
        let EPSILON = 0.01
        if abs(aspect - (this.Width / this.Height)) > EPSILON then
            this.Height <- (this.Width / aspect)
        LocalWinterop.LockWindowAspectRatioButAllowResizing(this, 100., 100., aspect, false)
        this.Title <- ZoomableLiveMinimapWindow.Name
        this.Content <- b
        updateEv.Add(fun (x,y) -> 
            this.EnsureOnMainUIThread()
            curX <- x
            curY <- y
            curZm <- InMemoryStore.ZoneMemory.Get(BackingStoreData.theGame.CurZone)
            this.Dispatcher.InvokeAsync(redraw) |> ignore
            )
        b.MouseWheel.Add(fun ea ->
            if ea.Delta > 0 then 
                curZoomStep <- curZoomStep + 1
            else
                curZoomStep <- curZoomStep - 1
            curZoomStep <- max curZoomStep 1        // 1 is smallest can go
            redraw()
            let czs = curZoomStep
            MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.LiveMinimapZoomLevel <- czs; AppSettings.theAppSettingsJson.Save())
            )
        this.SizeChanged.Add(fun _ -> redraw())
        this.Loaded.Add(fun _ ->
            ()
            )
        this.Closed.Add(fun _ ->
            singleton <- null
            )
    static member Singleton = singleton
    static member Name = "Minimap"

//////////////////////////////////////////////////////////////////////////

[<RequireQualifiedAccess>]
type EditNotesListenerMessage = 
    | StartEditing
    | Edit of string*int*int*int   // tb.Text, tb.CaretIndex, tb.SelectionStart, tb.SelectionLength
    | FinishEditing

let makeBlinkyBrush() =
    let colorAnimation = new System.Windows.Media.Animation.ColorAnimation()
    colorAnimation.From <- System.Nullable<_>(System.Windows.Media.Colors.Yellow)
    colorAnimation.To <- System.Nullable<_>(System.Windows.Media.Colors.DarkMagenta)
    colorAnimation.Duration <- new Duration(System.TimeSpan.FromSeconds(0.5))
    colorAnimation.AutoReverse <- true
    colorAnimation.RepeatBehavior <- System.Windows.Media.Animation.RepeatBehavior.Forever
    let brush = new SolidColorBrush(Colors.Black)
    brush.BeginAnimation(SolidColorBrush.ColorProperty, colorAnimation)
    brush

type NoteHelper(fontSizeGetter, fontSizeSetterSaver, dispatcher:System.Windows.Threading.Dispatcher) =
    let mutable fontSize = fontSizeGetter()
    let blinkyBrush = makeBlinkyBrush()
    let tb = new TextBlock(FontSize=float fontSize, Foreground=Brushes.White, Background=Brushes.Transparent,
                                FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, IsHitTestVisible=false, 
                                HorizontalAlignment=HorizontalAlignment.Stretch, TextWrapping=TextWrapping.Wrap, 
                                Margin=Thickness(3.), Width=System.Double.NaN, Height=System.Double.NaN)
    let sv = new ScrollViewer(VerticalScrollBarVisibility=ScrollBarVisibility.Hidden, Content=tb, IsHitTestVisible=false)
    let b = 
        let b = new Border(BorderThickness=Thickness(3.), Background=Brushes.DarkMagenta, BorderBrush=Brushes.DarkMagenta, Child=sv)
        b.MouseWheel.Add(fun ea -> 
            if ea.Delta > 0 then
                fontSize <- fontSize + 2
            else
                fontSize <- fontSize - 2
            // clamp values
            fontSize <- max 8 fontSize
            fontSize <- min 72 fontSize
            tb.FontSize <- float fontSize
            fontSizeSetterSaver(fontSize)
            )
        b
    member this.TextBlock = tb
    member this.ScrollViewer = sv
    member this.Border = b
    member this.NoteEdit(fullText:string,selectionStart,selectionLength) = 
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
            dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Background, new System.Action(fun () -> caretContainer.BringIntoView())) |> ignore
        if not (System.String.IsNullOrEmpty(textSelected)) then
            let selectionRun = System.Windows.Documents.Run(textSelected)
            selectionRun.Background <- Brushes.Gray
            selectionRun.Foreground <- Brushes.Lime
            tb.Inlines.Add(selectionRun)
        if not (System.String.IsNullOrEmpty(textAfter)) then
            tb.Inlines.Add(System.Windows.Documents.Run(textAfter))

[<AllowNullLiteral>]
type GlobalNoteWindow() as this =
    inherit IndependentWindow()
    static let mutable singleton : GlobalNoteWindow = null
    let helper = new NoteHelper((fun() -> MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.GlobalNoteFontSize)), 
                                (fun(x) -> MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.GlobalNoteFontSize <- x; AppSettings.theAppSettingsJson.Save())),  
                                System.Windows.Threading.Dispatcher.CurrentDispatcher)
    let UpdateNote() =
        this.EnsureOnThisWindowsOwnThread()
        let note = MainUIInvoke(fun() -> BackingStoreData.theGame.GlobalNote)
        helper.TextBlock.Text <- if System.String.IsNullOrEmpty(note) then "" else note
    do
        singleton <- this
        this.Width <- 300.
        this.Height <- 80.
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        MakeWindowSmartByRememberingPositionAndSize(this, MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.GlobalNotePopout))
        this.Title <- GlobalNoteWindow.Name
        this.UseLayoutRounding <- true
        this.Loaded.Add(fun _ ->
            UpdateNote()
            )
        this.Closed.Add(fun _ ->
            singleton <- null
            )
        this.Content <- helper.Border
    member this.StartEdit() = 
        this.EnsureOnMainUIThread()
        this.Dispatcher.InvokeAsync(fun() -> helper.TextBlock.Foreground <- Brushes.Lime) |> ignore
        BackingStoreData.theGame.GlobalNote
    member this.NoteEdit(fullText:string,_caretIndex,selectionStart,selectionLength) = 
        this.EnsureOnMainUIThread()
        this.Dispatcher.InvokeAsync(fun() -> helper.NoteEdit(fullText,selectionStart,selectionLength)) |> ignore
    member this.Save(result) = 
        this.EnsureOnMainUIThread()
        BackingStoreData.theGame.GlobalNote <- result
        BackingStoreData.theGame.Save()
    member this.FinishEdit() = 
        this.EnsureOnMainUIThread()
        this.Dispatcher.InvokeAsync(fun() -> 
            helper.TextBlock.Foreground <- Brushes.White
            UpdateNote()
            helper.ScrollViewer.ScrollToTop()
            ) |> ignore
    static member Singleton = singleton
    static member Name = "Global Note"

[<AllowNullLiteral>]
type LiveNotesWindow(originalX, originalY, updateEv:IEvent<int*int>) as this =
    inherit IndependentWindow()
    static let mutable singleton : LiveNotesWindow = null
    let mutable curX, curY, curZm = originalX, originalY, InMemoryStore.ZoneMemory.Get(BackingStoreData.theGame.CurZone)
    let helper = new NoteHelper((fun() -> MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.LiveNotesFontSize)), 
                                (fun(x) -> MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.LiveNotesFontSize <- x; AppSettings.theAppSettingsJson.Save())),  
                                System.Windows.Threading.Dispatcher.CurrentDispatcher)
    let UpdateStaticNote() =
        this.EnsureOnThisWindowsOwnThread()
        let note = MainUIInvoke(fun() -> curZm.MapTiles.[curX,curY].Note)
        helper.TextBlock.Text <- 
            if System.String.IsNullOrEmpty(note) then 
                //helper.TextBlock.Foreground <- Brushes.Gray
                "" //"<no note>" // for dwarf
            else 
                helper.TextBlock.Foreground <- Brushes.White
                note
    do
        singleton <- this
        this.Width <- 300.
        this.Height <- 80.
        MakeWindowChromelessAndHandleClicksForMoveAndClose(this)
        MakeWindowSmartByRememberingPositionAndSize(this, MainUIInvoke(fun() -> AppSettings.theAppSettingsJson.LiveNotesPopout))
        this.Title <- LiveNotesWindow.Name
        this.UseLayoutRounding <- true
        this.Loaded.Add(fun _ ->
            UpdateStaticNote()
            )
        this.Closed.Add(fun _ ->
            singleton <- null
            )
        this.Content <- helper.Border
        updateEv.Add(fun (x,y) ->
            this.EnsureOnMainUIThread()
            curX <- x
            curY <- y
            curZm <- InMemoryStore.ZoneMemory.Get(BackingStoreData.theGame.CurZone)
            this.Dispatcher.InvokeAsync(UpdateStaticNote) |> ignore
            )
    member this.StartEdit() = 
        this.EnsureOnMainUIThread()
        this.Dispatcher.InvokeAsync(fun() -> helper.TextBlock.Foreground <- Brushes.Lime) |> ignore
    member this.NoteEdit(fullText:string,_caretIndex,selectionStart,selectionLength) = 
        this.EnsureOnMainUIThread()
        this.Dispatcher.InvokeAsync(fun() -> helper.NoteEdit(fullText,selectionStart,selectionLength)) |> ignore
    member this.FinishEdit() = 
        this.EnsureOnMainUIThread()
        this.Dispatcher.InvokeAsync(fun() -> 
            UpdateStaticNote()
            helper.ScrollViewer.ScrollToTop()
            ) |> ignore
    static member Singleton = singleton
    static member Name = "CursorNote"

let theEditNotesListenerEvent = new Event<EditNotesListenerMessage>()
do
    theEditNotesListenerEvent.Publish.Add(fun msg ->
        if LiveNotesWindow.Singleton <> null then
            match msg with 
            | EditNotesListenerMessage.StartEditing -> LiveNotesWindow.Singleton.StartEdit()
            | EditNotesListenerMessage.Edit(t,ci,ss,sl) -> LiveNotesWindow.Singleton.NoteEdit(t,ci,ss,sl)
            | EditNotesListenerMessage.FinishEditing -> LiveNotesWindow.Singleton.FinishEdit()
        )
