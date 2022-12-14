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

