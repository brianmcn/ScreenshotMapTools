module Elephantasy

module Screenshot =
    open System.Collections.Generic
    open System.Text

    open System
    open System.Runtime.InteropServices
    type EnumWindowsProc = delegate of IntPtr * IntPtr -> bool
    type HWND = IntPtr
    [<DllImport("USER32.DLL")>]
    extern bool EnumWindows(EnumWindowsProc enumFunc, IntPtr lParam)
    [<DllImport("USER32.DLL")>]
    extern int GetWindowText(HWND hWnd, StringBuilder lpString, int nMaxCount)
    [<DllImport("USER32.DLL")>]
    extern int GetWindowTextLength(HWND hWnd)
    [<DllImport("USER32.DLL")>]
    extern bool IsWindowVisible(HWND hWnd)
    [<DllImport("USER32.DLL")>]
    extern HWND GetShellWindow()
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type RECT =
        val left:int
        val top:int
        val right:int
        val bottom:int
    [<DllImport("user32.dll", SetLastError = true)>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool GetWindowRect(IntPtr hWnd, [<Out>] RECT& lpRect)

    let GetOpenWindows() =
        let shellWindow = GetShellWindow()
        let windows = new Dictionary<HWND, string*RECT>()
        let perWindow(hWnd : HWND, lParam : HWND) : bool =
            if hWnd = shellWindow then true
            elif not(IsWindowVisible(hWnd)) then true
            else
                let length = GetWindowTextLength(hWnd)
                if length = 0 then true
                else
                    let builder = new StringBuilder(length)
                    GetWindowText(hWnd, builder, length + 1) |> ignore
                    let mutable r : RECT = Unchecked.defaultof<_>
                    if GetWindowRect(hWnd, &r) = false then failwith "bad window"
                    windows.[hWnd] <- (builder.ToString(), r)
                    true
        EnumWindows(new EnumWindowsProc(fun h l -> perWindow(h,l)), IntPtr.Zero) |> ignore
        windows

    let findElephantasyWindowLeftTop() =
        let mutable r = None
        for KeyValue(hwnd,(title,rect)) in GetOpenWindows() do
            if title = "Elephantasy" then
                r <- Some(rect.left, rect.top)
        r

    let getScreenBitmap(w,h,left,top) =
        let bmpScreenshot = new System.Drawing.Bitmap(w, h, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let gfxScreenshot = System.Drawing.Graphics.FromImage(bmpScreenshot)
        gfxScreenshot.CopyFromScreen(left, top, 0, 0, System.Drawing.Size(w,h), System.Drawing.CopyPixelOperation.SourceCopy)
        bmpScreenshot

    let getScreenInfo() =
        // with Elephantasy drawn at 4x size
        let w,h = 640,640   // how big the area to screenshot
        let left,top = findElephantasyWindowLeftTop().Value
        let left, top = left+8, top+32-1  // upper left of drawn area
        let left,top = left+80, top+80
        let gameArea = getScreenBitmap(w,h,left,top)
        let screenName = getScreenBitmap(400, 80, left, top-80)
        gameArea, screenName    

    let testMain() =
        let bmp1,bmp2 = getScreenInfo()
        bmp1.Save("test1.png", System.Drawing.Imaging.ImageFormat.Png)
        bmp2.Save("test2.png", System.Drawing.Imaging.ImageFormat.Png)

module Winterop =
    open System
    open System.Runtime.InteropServices
    // hotkeys
    [<DllImport("User32.dll")>]
    extern bool RegisterHotKey(IntPtr hWnd,int id,uint32 fsModifiers,uint32 vk)
    [<DllImport("User32.dll")>]
    extern bool UnregisterHotKey(IntPtr hWnd,int id)
    let HOTKEY_ID = 9000
    // windows
    [<DllImport("kernel32.dll")>]
    extern IntPtr GetConsoleWindow()
    [<DllImport("user32.dll")>]
    extern bool ShowWindow(IntPtr hWnd, int nCmdShow)
    let SW_HIDE = 0
    let SW_SHOW = 5
    let SW_MINIMIZE = 6

open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
type GridCanvas(rows,cols,rowH,colW,borderThickness) =
    // canvas
    let w = cols*colW + (cols+1)*borderThickness
    let h = rows*rowH + (rows+1)*borderThickness
    let c = new Canvas(Width=float w, Height=float h)
    // elements
    let a = Array2D.zeroCreate cols rows
    // cursor highlight
    let fill = Brushes.Lime // TODO
    let top = new Rectangle(Width=float(colW+2*borderThickness), Height=float(borderThickness), Fill=fill, Opacity=0.)
    let bottom = new Rectangle(Width=float(colW+2*borderThickness), Height=float(borderThickness), Fill=fill, Opacity=0.)
    let left = new Rectangle(Width=float(borderThickness), Height=float(rowH+2*borderThickness), Fill=fill, Opacity=0.)
    let right = new Rectangle(Width=float(borderThickness), Height=float(rowH+2*borderThickness), Fill=fill, Opacity=0.)
    do 
        c.Children.Add(top) |> ignore
        c.Children.Add(bottom) |> ignore
        c.Children.Add(left) |> ignore
        c.Children.Add(right) |> ignore
    member this.Canvas = c
    member this.Put(x,y,e:System.Windows.UIElement) =
        if a.[x,y] <> null then
            c.Children.Remove(a.[x,y])
        a.[x,y] <- e
        Utils.canvasAdd(c, e, float(borderThickness+x*(colW+borderThickness)), float(borderThickness+y*(rowH+borderThickness)))
    member this.GetCenterCoord(x,y) =
        float(borderThickness+x*(colW+borderThickness)+colW/2), float(borderThickness+y*(rowH+borderThickness)+rowH/2)
    member this.Highlight(x,y) =
        Canvas.SetLeft(top, float(x*(colW+borderThickness)))
        Canvas.SetTop(top, float(y*(rowH+borderThickness)))
        Canvas.SetLeft(bottom, float(x*(colW+borderThickness)))
        Canvas.SetTop(bottom, float((y+1)*(rowH+borderThickness)))
        Canvas.SetLeft(left, float(x*(colW+borderThickness)))
        Canvas.SetTop(left, float(y*(rowH+borderThickness)))
        Canvas.SetLeft(right, float((x+1)*(colW+borderThickness)))
        Canvas.SetTop(right, float(y*(rowH+borderThickness)))
        for e in [top;bottom;left;right] do
            e.Opacity <- 1.0
    member this.Unhighlight() =
        for e in [top;bottom;left;right] do
            e.Opacity <- 0.0
        
open System
open System.Windows
type MyWindow() as this = 
    inherit Window()
    // for hotkeys
    let mutable source = null
    let VK_F5 = 0x74
    let VK_F10 = 0x79
    let VK_NUMPAD0 = 0x60
    let VK_NUMPAD1 = 0x61
    let VK_NUMPAD2 = 0x62
    let VK_NUMPAD3 = 0x63
    let VK_NUMPAD4 = 0x64
    let VK_NUMPAD5 = 0x65
    let VK_NUMPAD6 = 0x66
    let VK_NUMPAD7 = 0x67
    let VK_NUMPAD8 = 0x68
    let VK_NUMPAD9 = 0x69
    let MOD_NONE = 0u
    let KEYS = [| VK_NUMPAD0; VK_NUMPAD1; VK_NUMPAD2; VK_NUMPAD3; VK_NUMPAD4; VK_NUMPAD5; VK_NUMPAD6; VK_NUMPAD7; VK_NUMPAD8; VK_NUMPAD9 |]
    // for app
    let mutable curX,curY = 9,9     // skurry at 10,10, but array is 0-based
    let SIZE = 32
    let ROWS,COLS = 20,18
    let gc = new GridCanvas(ROWS,COLS,SIZE,SIZE,2)
    let ssc = new GridCanvas(ROWS,COLS,SIZE,SIZE,2)
    let screenshots = Array2D.zeroCreate COLS ROWS
    let screenNames = Array2D.zeroCreate COLS ROWS
    let bottomLeftPreview = new Canvas(Width=float(SIZE*16), Height=float(SIZE*3))
    let updateBottomLeftPreview() =
        bottomLeftPreview.Children.Clear()
        let tb = new TextBox(IsReadOnly=true, FontSize=12., Text=sprintf "%d,%d" (curX+1) (curY+1), BorderThickness=Thickness(0.), 
                                Foreground=Brushes.Black, Background=Brushes.LightGray,
                                Width=float SIZE, Height=float SIZE, HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
        bottomLeftPreview.Children.Add(tb) |> ignore
        if screenNames.[curX,curY] <> null then
            let i = Utils.BMPtoImage screenNames.[curX,curY]
            i.Height <- float (2*SIZE)
            i.Width <- float (10*SIZE)
            i.Stretch <- Stretch.Fill
            Utils.canvasAdd(bottomLeftPreview, i, 0., float(SIZE))
            let i = Utils.BMPtoImage screenshots.[curX,curY]
            i.Height <- float (3*SIZE)
            i.Width <- float (3*SIZE)
            Utils.canvasAdd(bottomLeftPreview, i, float(SIZE*10), 0.)
    let mutable isZoomed = false
    let c = new Canvas(Width=gc.Canvas.Width, Height=gc.Canvas.Height)  // on which draw coord grid, then screenshots painted atop
    let recenterZoom() =
        if isZoomed then
            let SCALE = 4
            let cx,cy = gc.GetCenterCoord(curX,curY)
            c.RenderTransform <- ScaleTransform(float SCALE, float SCALE, cx, cy)
    let update() = 
        updateBottomLeftPreview()
        recenterZoom()
    do
        this.Title <- "Elephantasy Screenshot Mapper"
        this.Left <- 950.
        this.Top <- 10.
        //this.Topmost <- true
        this.SizeToContent <- SizeToContent.Manual
        this.Width <- 622. + 16.
        this.Height <- 800. + 16. + 20.
        // top layout
        let topc = new Canvas(Width=gc.Canvas.Width, Height=gc.Canvas.Height)
        topc.ClipToBounds <- true
        for x = 0 to 17 do
            for y = 0 to 19 do
                let tb = new TextBox(IsReadOnly=true, FontSize=10., Text=sprintf "%d,%d" (x+1) (y+1), BorderThickness=Thickness(0.), 
                                        Foreground=Brushes.Black, Background=Brushes.LightGray,
                                        Width=float SIZE, Height=float SIZE, HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
                gc.Put(x,y,tb)
        gc.Highlight(curX,curY)
        c.Children.Add(gc.Canvas) |> ignore
        // TODO load screenshots from disk
        c.Children.Add(ssc.Canvas) |> ignore
        topc.Children.Add(c) |> ignore
        let b = new Border(Child=topc, BorderBrush=Brushes.Blue, BorderThickness=Thickness(2.0), Background=Brushes.Black)
        let top = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(b, Dock.Right)
        top.Children.Add(b) |> ignore
        top.Children.Add(new DockPanel()) |> ignore // eat rest of space
        // rest layout
        let all = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(top, Dock.Top)
        all.Children.Add(top) |> ignore
        let bottom = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(bottomLeftPreview, Dock.Left)
        bottom.Children.Add(bottomLeftPreview) |> ignore
        bottom.Children.Add(new DockPanel()) |> ignore // eat rest of space
        all.Children.Add(bottom) |> ignore // eat rest of space
        this.Content <- all
        this.Loaded.Add(fun _ ->
            printfn "top: %A %A" top.ActualWidth top.ActualHeight
            let handle = Winterop.GetConsoleWindow()
            Winterop.ShowWindow(handle, Winterop.SW_MINIMIZE) |> ignore
            )
    override this.OnSourceInitialized(e) =
        base.OnSourceInitialized(e)
        let helper = new System.Windows.Interop.WindowInteropHelper(this)
        source <- System.Windows.Interop.HwndSource.FromHwnd(helper.Handle)
        source.AddHook(System.Windows.Interop.HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        this.RegisterHotKey()
    override this.OnClosed(e) =
        if source <> null then
            source.RemoveHook(System.Windows.Interop.HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        source <- null
        this.UnregisterHotKey()
        base.OnClosed(e)
    member this.RegisterHotKey() =
        let helper = new System.Windows.Interop.WindowInteropHelper(this);
        for k in KEYS do
            if(not(Winterop.RegisterHotKey(helper.Handle, Winterop.HOTKEY_ID, MOD_NONE, uint32 k))) then
                failwithf "could not register hotkey %A" k
    member this.UnregisterHotKey() =
        let helper = new System.Windows.Interop.WindowInteropHelper(this)
        Winterop.UnregisterHotKey(helper.Handle, Winterop.HOTKEY_ID) |> ignore
    member this.HwndHook(_hwnd:IntPtr, msg:int, wParam:IntPtr, lParam:IntPtr, handled:byref<bool>) : IntPtr =
        let WM_HOTKEY = 0x0312
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Winterop.HOTKEY_ID then
                //let ctrl_bits = lParam.ToInt32() &&& 0xF  // see WM_HOTKEY docs
                let key = lParam.ToInt32() >>> 16
                if false then
                    for k in KEYS do
                        if key = k then
                            printfn "key %A was pressed" k
                if key = VK_NUMPAD4 then
                    if curX > 0 then
                        curX <- curX - 1
                        gc.Highlight(curX,curY)
                        update()
                if key = VK_NUMPAD6 then
                    if curX < COLS-1 then
                        curX <- curX + 1
                        gc.Highlight(curX,curY)
                        update()
                if key = VK_NUMPAD8 then
                    if curY > 0 then
                        curY <- curY - 1
                        gc.Highlight(curX,curY)
                        update()
                if key = VK_NUMPAD2 then
                    if curY < ROWS-1 then
                        curY <- curY + 1
                        gc.Highlight(curX,curY)
                        update()
                if key = VK_NUMPAD5 then
                    let ss,name = Screenshot.getScreenInfo()
                    screenshots.[curX,curY] <- ss
                    screenNames.[curX,curY] <- name
                    let i = Utils.BMPtoImage screenshots.[curX,curY]
                    i.Height <- float SIZE
                    i.Width <- float SIZE
                    ssc.Put(curX, curY, i)
                    update()
                if key = VK_NUMPAD7 then
                    isZoomed <- not isZoomed
                    if isZoomed then
                        recenterZoom()
                    else
                        c.RenderTransform <- null
        IntPtr.Zero

(*
at 4x draw, game is 10x10 blocks of 80x80 pixels, e.g. 800x800
in a 16x9 OBS layout, that means 9x9 is taken by the game, leaving 7x9 for the tool
so tool window is (800*7/9 wide (~622) by 800 tall)

the map is 18 across by 20 tall
if we did 30x30 for the grid this is 540x600, which leaves ~82 to the side and 200 on the bottom
if there was 60x60 zoom on bottom, could fit 9x3 preview...
hm, but eyeballing, previews only look good at at least 160x160, hm... a 5x5 would take the whole screen, and 3x3 would take 480x480

in-game minimap uses 30x30 (6x6 grid of 5x5 pixels) abstract map, could maybe use that?
aside: skurry is at 10,10 where upper-left is 1,1

I think we need to go to the "so what it it's modal" view...

What if we just show (most of) a 5x5 zoom with coords while you are taking screenshots, but then also have a map view which is either screenshot of in-game abstract map
or inscrutable screenshots?

OK:

inscrutable 32x32 would take 576x640, leaves room for interior grid, coordinates, keyboard help, etc
same for in-game map
then the zoom could be ... the 80x80 pixel blocks at 16x16 at 5x5, so 1/5 scale is totally crisp 128x128, so a 5x5 grid would be 640x640, which doesn't quite fit width-wise, but
leaves the same bottom 160 for coords, keyboard help, etc
and I should save the screenshots on disk at the reduced size 128x128

play game with keyboard left-hand
global hotkey the numpad to run the map/screenshot thingy with right hand

*)