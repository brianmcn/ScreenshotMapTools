module Utils

//////////////////////////////////////////////////////

// note https://stackoverflow.com/questions/20444735/issue-with-setforegroundwindow-in-net

module Win32 =
    open System
    open System.Runtime.InteropServices
    [<DllImport("USER32.DLL")>]
    extern bool SetForegroundWindow(IntPtr hwnd)

//////////////////////////////////////////////////////


open System.Windows
open System.Windows.Controls

let canvasAdd(c:Canvas,e,x,y) =
    Canvas.SetLeft(e,x)
    Canvas.SetTop(e,y)
    c.Children.Add(e) |> ignore
let centerWithGrid(x) =
    let g = new Grid()
    g.Children.Add(x) |> ignore
    g
let gridAdd(g:Grid, x, c, r) =
    g.Children.Add(x) |> ignore
    Grid.SetColumn(x, c)
    Grid.SetRow(x, r)
let makeGrid(nc, nr, cw, rh) =
    let grid = new Grid()
    for i = 0 to nc-1 do
        let w = if cw = -1 then GridLength.Auto else GridLength(float cw)
        grid.ColumnDefinitions.Add(new ColumnDefinition(Width=w))
    for i = 0 to nr-1 do
        let h = if rh = -1 then GridLength.Auto else GridLength(float rh)
        grid.RowDefinitions.Add(new RowDefinition(Height=h))
    grid
let deparent(e:FrameworkElement) =
    match e.Parent with
    | null -> ()
    | :? Panel as p -> p.Children.Remove(e)
    | _ -> ()
let mutable aModalDialogIsOpen = false
let DoModalDialogCore(parentWindow, element, title, close:IEvent<unit>, onLoad) =
    let w = new Window()
    w.Title <- title
    w.Content <- element
    w.SizeToContent <- SizeToContent.WidthAndHeight
    w.Owner <- parentWindow
    w.WindowStartupLocation <- WindowStartupLocation.CenterOwner
    close.Add(fun _ -> w.Close())
    aModalDialogIsOpen <- true
    w.Closed.Add(fun _ -> aModalDialogIsOpen <- false)
    w.Loaded.Add(fun _ -> onLoad())
    w.ShowDialog() |> ignore
let DoModalDialog(parentWindow, element, title, close:IEvent<unit>) = DoModalDialogCore(parentWindow, element, title, close, (fun() -> ()))
let BMPtoImage(bmp:System.Drawing.Bitmap) =
    let ms = new System.IO.MemoryStream()
    bmp.Save(ms, System.Drawing.Imaging.ImageFormat.Png)  // must be png (not bmp) to save transparency info
    let bmimage = new System.Windows.Media.Imaging.BitmapImage()
    bmimage.BeginInit()
    ms.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
    bmimage.StreamSource <- ms
    bmimage.EndInit()
    let i = new Image()
    i.Source <- bmimage
    i.Height <- float bmp.Height 
    i.Width <- float bmp.Width 
    i
let BMPtoBitmapImage(bmp:System.Drawing.Bitmap) =  // can be done on background thread
    let ms = new System.IO.MemoryStream()
    bmp.Save(ms, System.Drawing.Imaging.ImageFormat.Png)  // must be png (not bmp) to save transparency info
    let bmimage = new System.Windows.Media.Imaging.BitmapImage()
    bmimage.BeginInit()
    ms.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
    bmimage.CacheOption <- Media.Imaging.BitmapCacheOption.OnLoad
    bmimage.StreamSource <- ms
    bmimage.EndInit()
    bmimage.Freeze()  // makes threadsafe
    bmimage
let ImageProjection(img:System.Windows.Controls.Image,(x,y,w,h)) =
    let src = img.Source :?> System.Windows.Media.Imaging.BitmapSource
    let crop = new System.Windows.Media.Imaging.CroppedBitmap(src, System.Windows.Int32Rect(x,y,w,h))
    let r = new System.Windows.Controls.Image(Source=crop)
    r
open System.Drawing
open System.Drawing.Imaging
open System.Drawing.Drawing2D
(*
let ResizeImage(image:Image, width, height) =
    let destRect = new Rectangle(0, 0, width, height)
    let destImage = new Bitmap(width, height)
    destImage.SetResolution(image.HorizontalResolution, image.VerticalResolution)
    use graphics = Graphics.FromImage(destImage)
    graphics.CompositingMode <- CompositingMode.SourceCopy
    graphics.CompositingQuality <- CompositingQuality.HighQuality
    graphics.InterpolationMode <- InterpolationMode.NearestNeighbor // .HighQualityBicubic
    graphics.SmoothingMode <- SmoothingMode.None // .HighQuality
    graphics.PixelOffsetMode <- PixelOffsetMode.None // .HighQuality
    use wrapMode = new ImageAttributes()
    wrapMode.SetWrapMode(WrapMode.TileFlipXY)
    graphics.DrawImage(image, destRect, 0, 0, image.Width,image.Height, GraphicsUnit.Pixel, wrapMode)
    destImage
*)
let PerfectDownscale(bmp:Bitmap, scale) =
    let r = new Bitmap(bmp.Width/scale, bmp.Height/scale)
    for x = 0 to r.Width-1 do
        for y = 0 to r.Height-1 do
            r.SetPixel(x,y, bmp.GetPixel(x*scale, y*scale))
    r

#nowarn "9"
let GetColorFromLockedFormat32BppArgb(x,y,bmd:BitmapData) =
    let PixelSize = 4
    let rowOffset = y * bmd.Stride
    let colOffset = x * PixelSize
    let ptr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt bmd.Scan0
    let b = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 0)
    let g = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 1)
    let r = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 2)
    let a = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 3)
    Color.FromArgb(int a, int r, int g, int b)
let SetColorFromLockedFormat32BppArgb(x,y,bmd:BitmapData,c:Color) =
    let PixelSize = 4
    let rowOffset = y * bmd.Stride
    let colOffset = x * PixelSize
    let ptr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt bmd.Scan0
    NativeInterop.NativePtr.set ptr (rowOffset + colOffset + 0) c.B
    NativeInterop.NativePtr.set ptr (rowOffset + colOffset + 1) c.G
    NativeInterop.NativePtr.set ptr (rowOffset + colOffset + 2) c.R
    NativeInterop.NativePtr.set ptr (rowOffset + colOffset + 3) c.A
let SetAndGetColorFromLockedFormat32BppArgb(rx,ry,rbmd:BitmapData,x,y,bmd:BitmapData) =   // r is result
    let PixelSize = 4
    let rowOffset = y * bmd.Stride
    let colOffset = x * PixelSize
    let ptr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt bmd.Scan0
    let b = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 0)
    let g = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 1)
    let r = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 2)
    let a = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 3)
    let rrowOffset = ry * rbmd.Stride
    let rcolOffset = rx * PixelSize
    let rptr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt rbmd.Scan0
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 0) b
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 1) g
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 2) r
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 3) a

//////////////////////////////////////////////////////////////////////

// https://stackoverflow.com/questions/1335426/is-there-a-built-in-c-net-system-api-for-hsv-to-rgb
let ColorToHSV(color:Color) =
    let max = System.Math.Max(color.R, System.Math.Max(color.G, color.B))
    let min = System.Math.Min(color.R, System.Math.Min(color.G, color.B))
    //let hue = (float(color.GetHue()) + 180.) % 360.
    let hue = float(color.GetHue())
    let saturation = if (max = 0uy) then 0. else 1. - (1. * float min / float max)
    let value = float max / 255.
    hue, saturation, value

let ColorFromHSV(hue, saturation, value) =
    let hi = System.Convert.ToInt32(System.Math.Floor(hue / 60.)) % 6
    let f = hue / 60. - System.Math.Floor(hue / 60.)

    let value = value * 255.
    let v = System.Convert.ToInt32(value);
    let p = System.Convert.ToInt32(value * (1. - saturation))
    let q = System.Convert.ToInt32(value * (1. - f * saturation))
    let t = System.Convert.ToInt32(value * (1. - (1. - f) * saturation))

    if (hi = 0) then
        Color.FromArgb(255, v, t, p)
    elif (hi = 1) then
        Color.FromArgb(255, q, v, p)
    elif (hi = 2) then
        Color.FromArgb(255, p, v, t)
    elif (hi = 3) then
        Color.FromArgb(255, p, q, v)
    elif (hi = 4) then
        Color.FromArgb(255, t, p, v)
    else
        Color.FromArgb(255, v, p, q)

//////////////////////////////////////////////////////////////////////

let cropToRect(bmp:System.Drawing.Bitmap, r:System.Drawing.Rectangle) =
    if bmp=null then null else
    let nb = new Bitmap(r.Width, r.Height)
    use g = Graphics.FromImage(nb)
    g.DrawImage(bmp, -r.X, -r.Y)
    nb

let ConvertBmpToBGRA(bmp:System.Drawing.Bitmap) =
    let data = bmp.LockBits(System.Drawing.Rectangle(0, 0, bmp.Width, bmp.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
    // copy bgra32 data into byte array
    let numBytes = data.Stride * bmp.Height
    let byteArray : byte[] = Array.zeroCreate numBytes
    System.Runtime.InteropServices.Marshal.Copy(data.Scan0, byteArray, 0, numBytes)
    let w,h,stride = bmp.Width, bmp.Height, data.Stride
    bmp.UnlockBits(data)
    byteArray

let CopyBGRARegion(destBytes:byte[], destStride, destX, destY, sourceBytes:byte[], sourceStride, sourceX, sourceY, sourceW, sourceH) =
    // asssume BGRA 4 bytes per pixel
    for dh = 0 to sourceH-1 do
        // copy each row as a strip
        let sourceIndex = (sourceY+dh) * sourceStride + sourceX*4
        let destIndex = (destY+dh) * destStride + destX*4
        for i = 0 to (sourceW*4)-1 do
            destBytes.[destIndex+i] <- sourceBytes.[sourceIndex+i]

let CopyBGRARegionOnlyPartsWithAlpha(destBytes:byte[], destStride, destX, destY, sourceBytes:byte[], sourceStride, sourceX, sourceY, sourceW, sourceH) =
    // asssume BGRA 4 bytes per pixel
    for dh = 0 to sourceH-1 do
        // copy each row as a strip
        let sourceIndex = (sourceY+dh) * sourceStride + sourceX*4
        let destIndex = (destY+dh) * destStride + destX*4
        for i = 0 to sourceW-1 do
            let j = sourceIndex+i*4
            let b,g,r,a = sourceBytes.[j], sourceBytes.[j+1], sourceBytes.[j+2], sourceBytes.[j+3]
            if a <> 0uy then
                destBytes.[destIndex+i*4] <- b
                destBytes.[destIndex+i*4+1] <- g
                destBytes.[destIndex+i*4+2] <- r
                destBytes.[destIndex+i*4+3] <- a
            
////////////////////////////////////////////////////////////

type Win32() =
    [<System.Runtime.InteropServices.DllImport("User32.dll")>]
    static extern bool SetCursorPos(int X, int Y)
    static member SetCursor(theWindow:Window,x,y) = 
        let pos = theWindow.PointToScreen(System.Windows.Point(x,y))
        SetCursorPos(int pos.X, int pos.Y) |> ignore
// ideas from
// https://kent-boogaart.com/blog/dispatcher-frames
// from https://stackoverflow.com/questions/4502037/where-is-the-application-doevents-in-wpf
// see also
// https://stackoverflow.com/questions/21248643/how-can-i-convert-win32-mouse-messages-to-wpf-mouse-events 
let mutable sawMouseEvent = false
let mutable theTimer = null
let mutable theFrame = null : System.Windows.Threading.DispatcherFrame
let mutable theCounter = 0
let mutable theWindow = null
let setup(w:Window) =
    if theTimer=null then
        theTimer <- new System.Windows.Threading.DispatcherTimer(System.Windows.Threading.DispatcherPriority.Render) // this only runs for a very short time when warping mouse; use high priority to avoid noticeable latency
        theTimer.Stop()
        theTimer.Interval <- System.TimeSpan.FromSeconds(0.03)
        theTimer.Tick.Add(fun _ -> 
            if theFrame <> null then   // avoid a race
                if not sawMouseEvent && theCounter>4 then
                    //printfn "uh oh too slow"     // or, the mouse didn't move at all
                    sawMouseEvent <- true
                if sawMouseEvent then
                    theFrame.Continue <- false
                    theTimer.Stop()
                else
                    theCounter <- theCounter + 1
            )
        w.MouseMove.Add(fun _ -> sawMouseEvent <- true)
        theWindow <- w
let SilentlyWarpMouseCursorTo(pos:System.Windows.Point) =
    if theFrame = null then  // Note: cannot process a second of these calls while one is active; I think that's fine
        theFrame <- new System.Windows.Threading.DispatcherFrame()
        sawMouseEvent <- false
        theCounter <- 0
        theTimer.Start()
        Win32.SetCursor(theWindow, pos.X, pos.Y)    // this queues a mouse move input, and we want to block until that event is processed, to avoid spurious MouseLeave()s and other reasons
        System.Windows.Threading.Dispatcher.PushFrame(theFrame)   // creates a blocking message pump that only stops when Continue set to false
        theFrame <- null
