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
        // with Elephantasy drawn at +4 size, everything in 5x5 pixel groups
        let w,h = 640,640   // how big the area to screenshot
        let left,top = findElephantasyWindowLeftTop().Value
        let left, top = left+8, top+32-1  // upper left of drawn area
        let left,top = left+80, top+80
        let gameArea = getScreenBitmap(w,h,left,top)
        let screenName = getScreenBitmap(400, 80, left, top-80)
        Utils.PerfectDownscale(gameArea, 5), Utils.PerfectDownscale(screenName, 5)

    let allBlack(w:int,h:int) = 
        let r = new System.Drawing.Bitmap(w,h)
        for x = 0 to w-1 do
            for y = 0 to h-1 do
                r.SetPixel(x,y,Drawing.Color.Black)
        r

    let testMain() =
        let bmp1,bmp2 = getScreenInfo()
        bmp1.Save("test1.png", System.Drawing.Imaging.ImageFormat.Png)
        bmp2.Save("test2.png", System.Drawing.Imaging.ImageFormat.Png)

module Winterop =
    open System
    open System.Runtime.InteropServices
    // hotkeys
    let VK_RETURN = 0x0D
    //let VK_F5 = 0x74
    //let VK_F10 = 0x79
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
    let VK_MULTIPLY = 0x6A
    let VK_ADD = 0x6B
    //let VK_SEPARATOR = 0x6C
    let VK_SUBTRACT = 0x6D
    let VK_DECIMAL = 0x6E
    let VK_DIVIDE = 0x6F
    let MOD_NONE = 0u
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
    member this.Canvas = c
    member this.Put(x,y,e:System.Windows.UIElement) =
        if a.[x,y] <> null then
            c.Children.Remove(a.[x,y])
        a.[x,y] <- e
        Utils.canvasAdd(c, e, float(borderThickness+x*(colW+borderThickness)), float(borderThickness+y*(rowH+borderThickness)))
type GridCanvasCursor(rows,cols,rowH,colW,borderThickness) =
    // canvas
    let w = cols*colW + (cols+1)*borderThickness
    let h = rows*rowH + (rows+1)*borderThickness
    let c = new Canvas(Width=float w, Height=float h)
    // cursor highlight
    let fill = new SolidColorBrush()
    let all = new Rectangle(Width=float(colW+2*borderThickness), Height=float(rowH+2*borderThickness), Stroke=fill, StrokeThickness=float borderThickness, Opacity=0.)
    do 
        c.Children.Add(all) |> ignore
        let ca = System.Windows.Media.Animation.ColorAnimation()
        ca.From <- Colors.LightGray
        ca.To <- Colors.Transparent
        ca.AutoReverse <- true
        ca.EasingFunction <- System.Windows.Media.Animation.PowerEase(Power=3.0, EasingMode=System.Windows.Media.Animation.EasingMode.EaseInOut)
        ca.Duration <- System.Windows.Duration(System.TimeSpan.FromSeconds(0.5))
        ca.RepeatBehavior <- System.Windows.Media.Animation.RepeatBehavior.Forever
        fill.BeginAnimation(SolidColorBrush.ColorProperty, ca)
    member this.Canvas = c
    member this.Highlight(x,y) =
        Canvas.SetLeft(all, float(x*(colW+borderThickness)))
        Canvas.SetTop(all, float(y*(rowH+borderThickness)))
        all.Opacity <- 1.0
    member this.Unhighlight() =
        all.Opacity <- 0.0

let CIRCLES_JSON_FILENAME = "CIRCLES.json"
[<AllowNullLiteral>]
type CirclesJson() =
    member val Circles : int[][] = null with get,set
type GridCanvasCircles(rows,cols,rowH,colW,borderThickness,colors:_[],circleThickness) as this =
    // canvas
    let w = cols*colW + (cols+1)*borderThickness
    let h = rows*rowH + (rows+1)*borderThickness
    let root = new Canvas(Width=float w, Height=float h)
    let fcc = new Canvas(Width=float w, Height=float h)  // fixed circle canvas
    let acc = new Canvas(Width=float w, Height=float h)  // animated circle canvas
    let mutable allHidden = false
    // elements
    let a = Array.init colors.Length (fun _ -> Array2D.zeroCreate cols rows)
    let MIDDLE_OPACITY = 0.5
    do
        root.Children.Add(fcc) |> ignore
        root.Children.Add(acc) |> ignore
        try
            let json = System.IO.File.ReadAllText(CIRCLES_JSON_FILENAME)
            let data = System.Text.Json.JsonSerializer.Deserialize<CirclesJson>(json)
            for y = 0 to rows-1 do
                for x = 0 to cols-1 do
                    let s = data.Circles.[y].[x].ToString()
                    for i = 0 to colors.Length-1 do
                        if s.Chars(i)='2' then
                            this.Cycle(x,y,i)
                        elif s.Chars(i)='3' then
                            this.Cycle(x,y,i)
                            this.Cycle(x,y,i)
        with e ->
            printfn "failed to open %s: %s" CIRCLES_JSON_FILENAME (e.ToString())
    member this.Canvas = root
    member this.Cycle(x, y, colorIndex) =
        if a.[colorIndex].[x,y] = null then
            let n = colorIndex-3
            let e = new System.Windows.Shapes.Ellipse(Stroke=colors.[colorIndex], StrokeThickness=float circleThickness, Width=float(colW-n*2*circleThickness), Height=float(rowH-n*2*circleThickness))
            Utils.canvasAdd(fcc, e, float(borderThickness+x*(colW+borderThickness)+n*circleThickness), float(borderThickness+y*(rowH+borderThickness)+n*circleThickness))
            a.[colorIndex].[x,y] <- e
        elif a.[colorIndex].[x,y].Opacity = 1.0 then
            a.[colorIndex].[x,y].Opacity <- MIDDLE_OPACITY
        else
            fcc.Children.Remove(a.[colorIndex].[x,y])
            a.[colorIndex].[x,y] <- null
    member this.HideAll() =
        fcc.Opacity <- 0.0
        allHidden <- true
    member this.ShowSome() =
        fcc.Opacity <- 1.0
        allHidden <- false
        for e in fcc.Children do
            if e.Opacity <> 1.0 then
                e.Opacity <- 0.0
    member this.ShowAll() =
        fcc.Opacity <- 1.0
        allHidden <- false
        for e in fcc.Children do
            if e.Opacity <> 1.0 then
                e.Opacity <- MIDDLE_OPACITY
    member this.StartAnimateColors(which) =
        fcc.Opacity <- 0.0
        acc.Children.Clear()
        for x in fcc.Children do
            let e = x :?> System.Windows.Shapes.Ellipse
            for ci in which do
                if obj.ReferenceEquals(colors.[ci], e.Stroke) then
                    let z = new System.Windows.Shapes.Ellipse(Stroke=e.Stroke, StrokeThickness=e.StrokeThickness, Width=e.Width, Height=e.Height, Opacity=if e.Opacity=0.0 then MIDDLE_OPACITY else e.Opacity)
                    Utils.canvasAdd(acc, z, Canvas.GetLeft(e), Canvas.GetTop(e))
                    let a = System.Windows.Media.Animation.DoubleAnimation()
                    a.From <- e.StrokeThickness
                    a.To <- e.StrokeThickness*3.0
                    a.AutoReverse <- true
                    a.EasingFunction <- System.Windows.Media.Animation.PowerEase(Power=1.0, EasingMode=System.Windows.Media.Animation.EasingMode.EaseInOut)
                    a.Duration <- System.Windows.Duration(System.TimeSpan.FromSeconds(0.5))
                    a.RepeatBehavior <- System.Windows.Media.Animation.RepeatBehavior.Forever
                    z.BeginAnimation(System.Windows.Shapes.Ellipse.StrokeThicknessProperty, a)
    member this.EndAnimateColors() =
        fcc.Opacity <- if allHidden then 0.0 else 1.0
        acc.Children.Clear()
    member this.AsJsonLines() =
        (*
        { Circles: [
              [ 1111, 1112, 1211 ],
              [ 1111, 1112, 1211 ],
              [ 1111, 1112, 1211 ]
          ]
        }
        *)
        let lines = [|
            yield "{ \"Circles\": ["
            for y = 0 to rows-1 do
                let sb = new System.Text.StringBuilder("    [ ")
                for x = 0 to cols-1 do
                    for i = 0 to colors.Length-1 do
                        if a.[i].[x,y]=null then
                            sb.Append('1') |> ignore
                        elif a.[i].[x,y].Opacity=1.0 then
                            sb.Append('2') |> ignore
                        else
                            sb.Append('3') |> ignore
                    if x<>cols-1 then
                        sb.Append(", ") |> ignore
                sb.Append(" ]") |> ignore
                if y<>rows-1 then
                    sb.Append(',') |> ignore
                yield sb.ToString()
            yield "  ]"
            yield "}"
            |]
        lines
        
open Winterop
open System
open System.Windows
type MyWindow() as this = 
    inherit Window()
    // for hotkeys
    let mutable source = null
    let KEYS = [| VK_NUMPAD0; VK_NUMPAD1; VK_NUMPAD2; VK_NUMPAD3; VK_NUMPAD4; VK_NUMPAD5; VK_NUMPAD6; VK_NUMPAD7; VK_NUMPAD8; VK_NUMPAD9;
                    VK_MULTIPLY; VK_ADD; VK_SUBTRACT; VK_DECIMAL; VK_DIVIDE; VK_RETURN |]
    // for app
    let mutable curX,curY = 9,9     // skurry at 10,10, but array is 0-based
    let SIZE = 32
    let BORDER = 2
    let ROWS,COLS = 20,18
    let gc = new GridCanvas(ROWS,COLS,SIZE,SIZE,BORDER)       // coordinates
    let ssc = new GridCanvas(ROWS,COLS,SIZE,SIZE,BORDER)      // screenshots
    let circlesc = new GridCanvasCircles(ROWS,COLS,SIZE,SIZE,BORDER,[|Brushes.Red;Brushes.Yellow;Brushes.Lime;Brushes.Cyan|],BORDER)  // circles
    let mutable circleState = 0 // 0 = show all, 1 = hide middle opacity, 2 = hide all
    let circleStateTBBackground = new SolidColorBrush(Colors.LightGray)
    let circleStateTB = new TextBox(IsReadOnly=true, FontSize=12., Text="", BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=circleStateTBBackground,
                                    Width=float (4*SIZE), Height=float(SIZE*3/4), HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
    let hoverTargetB = new TextBox(IsReadOnly=true, FontSize=12., Text="B", BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=Brushes.LightGray,
                                    Width=float (SIZE*3/4), Height=float(SIZE*3/4), HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
    let hoverTargetR = new TextBox(IsReadOnly=true, FontSize=12., Text="R", BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=Brushes.LightGray,
                                    Width=float (SIZE*3/4), Height=float(SIZE*3/4), HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
    let hoverTargetG = new TextBox(IsReadOnly=true, FontSize=12., Text="G", BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=Brushes.LightGray,
                                    Width=float (SIZE*3/4), Height=float(SIZE*3/4), HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
    let hoverTargetY = new TextBox(IsReadOnly=true, FontSize=12., Text="Y", BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=Brushes.LightGray,
                                    Width=float (SIZE*3/4), Height=float(SIZE*3/4), HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
    let updateCircleStateLegend() = 
        if circleState = 0 then circleStateTB.Text <- "Circles: Show All"
        elif circleState = 1 then circleStateTB.Text <- "Circles: Hide Some"
        elif circleState = 2 then circleStateTB.Text <- "Circles: Hide All"
        let ca = System.Windows.Media.Animation.ColorAnimation()
        ca.From <- Colors.Red
        ca.To <- Colors.LightGray
        ca.EasingFunction <- System.Windows.Media.Animation.PowerEase(Power=3.0, EasingMode=System.Windows.Media.Animation.EasingMode.EaseIn)
        ca.Duration <- System.Windows.Duration(System.TimeSpan.FromSeconds(0.5))
        circleStateTBBackground.BeginAnimation(SolidColorBrush.ColorProperty, ca)
    let gccursor = new GridCanvasCursor(ROWS,COLS,SIZE,SIZE,BORDER)       // cursor
    let screenshots = Array2D.zeroCreate COLS ROWS
    let screenNames = Array2D.zeroCreate COLS ROWS
    let bottomLeftPreview = new Canvas(Width=float(SIZE*16), Height=float(SIZE*3))
    let updateBottomLeftPreview() =
        bottomLeftPreview.Children.Clear()
        let BUFF = 8
        // coords
        let tb = new TextBox(IsReadOnly=true, FontSize=12., Text=sprintf "%d,%d" (curX+1) (curY+1), BorderThickness=Thickness(0.), 
                                Foreground=Brushes.Black, Background=Brushes.LightGray,
                                Width=float SIZE, Height=float(SIZE*3/4), HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
        Utils.canvasAdd(bottomLeftPreview, tb, float(BUFF), 0.) |> ignore
        // circle state
        Utils.canvasAdd(bottomLeftPreview, circleStateTB, float(BUFF+SIZE*3/2), 0.) |> ignore
        Utils.canvasAdd(bottomLeftPreview, hoverTargetB, float(BUFF+SIZE*4+SIZE*3/2+SIZE/4), 0.) |> ignore
        Utils.canvasAdd(bottomLeftPreview, hoverTargetR, float(BUFF+SIZE*5+SIZE*3/2+SIZE/4), 0.) |> ignore
        Utils.canvasAdd(bottomLeftPreview, hoverTargetG, float(BUFF+SIZE*6+SIZE*3/2+SIZE/4), 0.) |> ignore
        Utils.canvasAdd(bottomLeftPreview, hoverTargetY, float(BUFF+SIZE*7+SIZE*3/2+SIZE/4), 0.) |> ignore
        // key legend
        let tb = new TextBox(IsReadOnly=true, FontSize=12., Text="", BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=Brushes.LightGray,
                                Width=float(5*SIZE), Height=float(SIZE*3), HorizontalContentAlignment=HorizontalAlignment.Left, VerticalContentAlignment=VerticalAlignment.Center)
        tb.Text <- "enter\tcombine screenshot\n0\treplace screenshot\n2468\tmove cursor\n7\ttoggle zoom\n9\tcycle circle display\n+-*/\ttoggle circle"
        Utils.canvasAdd(bottomLeftPreview, tb, float(SIZE*13+4*BUFF), 0.) |> ignore
        if screenNames.[curX,curY] <> null then
            // screen name
            let i = Utils.BMPtoImage screenNames.[curX,curY]
            i.Height <- float (2*SIZE)
            i.Width <- float (10*SIZE)
            RenderOptions.SetBitmapScalingMode(i, BitmapScalingMode.NearestNeighbor)
            Utils.canvasAdd(bottomLeftPreview, i, float(BUFF), float(SIZE))
            // screenshot
            let i = Utils.BMPtoImage screenshots.[curX,curY]
            i.Height <- float (3*SIZE)
            i.Width <- float (3*SIZE)
            RenderOptions.SetBitmapScalingMode(i, BitmapScalingMode.NearestNeighbor)
            Utils.canvasAdd(bottomLeftPreview, i, float(SIZE*10+2*BUFF), 0.)
    let mutable isZoomed = false
    let c = new Canvas(Width=gc.Canvas.Width, Height=gc.Canvas.Height)  // on which draw coord grid, then screenshots painted atop
    let recenterZoom() =
        if isZoomed then
            let SCALE = 4
            let SIZE = SIZE + BORDER
            let cx,cy = float(curX*SIZE+SIZE/2)/float(COLS)*float(COLS+2)-float(SIZE), 
                        float(curY*SIZE+SIZE/2)/float(ROWS)*float(ROWS+2)-float(SIZE)
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
        // load screenshots from disk
        for i = 0 to COLS-1 do
            for j = 0 to ROWS-1 do
                let ssf = sprintf "SS-%02d-%02d.png" i j
                if System.IO.File.Exists(ssf) then
                    let bytes = System.IO.File.ReadAllBytes(ssf)
                    let ss = new System.Drawing.Bitmap(new System.IO.MemoryStream(bytes))
                    screenshots.[i,j] <- ss
                    let img = Utils.BMPtoImage ss
                    img.Height <- float SIZE
                    img.Width <- float SIZE
                    RenderOptions.SetBitmapScalingMode(img, BitmapScalingMode.NearestNeighbor)
                    ssc.Put(i, j, img)
                let snf = sprintf "SN-%02d-%02d.png" i j
                if System.IO.File.Exists(snf) then
                    let bytes = System.IO.File.ReadAllBytes(snf)
                    let name = new System.Drawing.Bitmap(new System.IO.MemoryStream(bytes))
                    screenNames.[i,j] <- name
        // init other elements
        updateCircleStateLegend()
        // top layout
        let topc = new Canvas(Width=gc.Canvas.Width, Height=gc.Canvas.Height)
        topc.ClipToBounds <- true
        for x = 0 to 17 do
            for y = 0 to 19 do
                let tb = new TextBox(IsReadOnly=true, FontSize=10., Text=sprintf "%d,%d" (x+1) (y+1), BorderThickness=Thickness(0.), 
                                        Foreground=Brushes.Black, Background=Brushes.Gray,
                                        Width=float SIZE, Height=float SIZE, HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
                gc.Put(x,y,tb)
        c.Children.Add(gc.Canvas) |> ignore
        c.Children.Add(ssc.Canvas) |> ignore
        c.Children.Add(circlesc.Canvas) |> ignore
        c.Children.Add(gccursor.Canvas) |> ignore
        gccursor.Highlight(curX,curY)
        topc.Children.Add(c) |> ignore
        let b = new Border(Child=topc, BorderBrush=Brushes.Blue, BorderThickness=Thickness(0.0), Background=Brushes.DarkSlateBlue)
        let top = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(b, Dock.Top)
        top.Children.Add(b) |> ignore
        top.Children.Add(new DockPanel()) |> ignore // eat rest of space
        // rest layout
        let all = new DockPanel(LastChildFill=true, Background=Brushes.DarkSlateBlue)
        DockPanel.SetDock(top, Dock.Top)
        all.Children.Add(top) |> ignore
        let bottom = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(bottomLeftPreview, Dock.Left)
        bottom.Children.Add(bottomLeftPreview) |> ignore
        bottom.Children.Add(new DockPanel()) |> ignore // eat rest of space
        all.Children.Add(bottom) |> ignore // eat rest of space
        all.UseLayoutRounding <- true
        this.Content <- all
        this.Loaded.Add(fun _ ->
            printfn "top: %A %A" top.ActualWidth top.ActualHeight
            let handle = Winterop.GetConsoleWindow()
            Winterop.ShowWindow(handle, Winterop.SW_MINIMIZE) |> ignore
            update()
            // hover reactions
            for ht,n,b in [hoverTargetB,3,Brushes.LightCyan; hoverTargetR,0,Brushes.Pink; hoverTargetG,2,Brushes.LightGreen; hoverTargetY,1,Brushes.LightGoldenrodYellow] do
                ht.MouseEnter.Add(fun _ -> circlesc.StartAnimateColors([n]); ht.Background <- b)
                ht.MouseLeave.Add(fun _ -> circlesc.EndAnimateColors(); ht.Background <- Brushes.LightGray)
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
                        gccursor.Highlight(curX,curY)
                        update()
                if key = VK_NUMPAD6 then
                    if curX < COLS-1 then
                        curX <- curX + 1
                        gccursor.Highlight(curX,curY)
                        update()
                if key = VK_NUMPAD8 then
                    if curY > 0 then
                        curY <- curY - 1
                        gccursor.Highlight(curX,curY)
                        update()
                if key = VK_NUMPAD2 then
                    if curY < ROWS-1 then
                        curY <- curY + 1
                        gccursor.Highlight(curX,curY)
                        update()
                let updateSS(ss,name) =
                    screenshots.[curX,curY] <- ss
                    screenNames.[curX,curY] <- name
                    ss.Save(sprintf "SS-%02d-%02d.png" curX curY, System.Drawing.Imaging.ImageFormat.Png)
                    name.Save(sprintf "SN-%02d-%02d.png" curX curY, System.Drawing.Imaging.ImageFormat.Png)
                    let i = Utils.BMPtoImage screenshots.[curX,curY]
                    i.Height <- float SIZE
                    i.Width <- float SIZE
                    RenderOptions.SetBitmapScalingMode(i, BitmapScalingMode.NearestNeighbor)
                    ssc.Put(curX, curY, i)
                    update()
                if key = VK_NUMPAD0 then
                    let ss,name = 
                        try
                            Screenshot.getScreenInfo()
                        with _ ->
                            printfn "screenshot failed, using all black"
                            Screenshot.allBlack(640/5, 640/5), Screenshot.allBlack(400/5, 80/5)
                    updateSS(ss,name)
                if key = VK_RETURN then
                    let ss,name = Screenshot.getScreenInfo()
                    if screenNames.[curX,curY] = null then
                        updateSS(ss,name)
                    else
                        // verify same name screen before merge
                        let oldName = screenNames.[curX,curY]
                        let mutable ok = true
                        for x = 0 to name.Width-1 do
                            for y = 0 to name.Height-1 do
                                if name.GetPixel(x,y).ToArgb() <> oldName.GetPixel(x,y).ToArgb() then
                                    ok <- false
                        if not ok then
                            System.Console.Beep()
                        else
                            // combine two screenshots
                            let oldSS = screenshots.[curX,curY]
                            for x = 0 to ss.Width-1 do
                                for y = 0 to ss.Height-1 do
                                    if ss.GetPixel(x,y).ToArgb() = System.Drawing.Color.Black.ToArgb() then
                                        ss.SetPixel(x,y,oldSS.GetPixel(x,y))
                            updateSS(ss,name)
                if key = VK_NUMPAD7 then
                    isZoomed <- not isZoomed
                    if isZoomed then
                        recenterZoom()
                    else
                        c.RenderTransform <- null
                let saveJson() = System.IO.File.WriteAllLines(CIRCLES_JSON_FILENAME, circlesc.AsJsonLines())
                let showAllCircles() = 
                    circleState <- 0
                    circlesc.ShowAll()
                for k,n in [VK_MULTIPLY,0; VK_ADD,1; VK_SUBTRACT,2; VK_DIVIDE,3] do
                    if key = k then
                        if circleState=0 then
                            circlesc.Cycle(curX, curY, n)
                            saveJson()
                        else
                            showAllCircles()
                            updateCircleStateLegend()
                if key = VK_NUMPAD9 then
                    if circleState=0 then
                        circleState <- 1
                        circlesc.ShowSome()
                    elif circleState=1 then
                        circleState <- 2
                        circlesc.HideAll()
                    else
                        showAllCircles()
                    updateCircleStateLegend()
        IntPtr.Zero

(*
TODO

text notes area?

show percentage of whole that is mapped

undo (accidentally take screenshot wrong place, accidentally overwrite old screenshot)
 - maybe have two SS for each room, and a button to toggle between them? (or a history of all SS and indexes, or... what is decent UI/function likely to be sufficient?)

based on errors I have made, perhaps
 - delete one screen
 - cut and paste one screen
 - GRAB to move a segment, like z-tracker
 - idea of taking a screenshot every second, just keeping a back catalog, and indexing into catalog for screenshot uses, if have memory and disk to deal with that

fidgety controls:
 - I sometimes replace when I forgot to move cursor
 - I sometimes replace when I meant to combine
 - Maybe replace should be a different button from take-screenshot (on empty screen) to make it explicit and fewer errors

thinking about e.g. using on ESA
 - no size limit, just two zoom sizes, always centered
 - zero-based or one-based coords
 - abstracting the screenshot protocol (wouldn't be room names, would need to save more size/detail)
 - rather than just color circles, maybe like color asterisks that also carry text, and hovering shows the text or whatnot... and maybe text can refer to other locations
   so that when there's a warp from here to (12,23) you could click the text or something?  the circles got too noisy and also didn't carry enough info, need an arbitrary 
   'category set' and also arbitrary extra data ideally, hm


*)