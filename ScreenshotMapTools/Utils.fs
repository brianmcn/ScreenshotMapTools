module Utils

//////////////////////////////////////////////////////

// note https://stackoverflow.com/questions/20444735/issue-with-setforegroundwindow-in-net

module Win32 =
    open System
    open System.Runtime.InteropServices
    [<DllImport("USER32.DLL")>]
    extern bool SetForegroundWindow(IntPtr hwnd)
    [<DllImport("USER32.DLL")>]
    extern bool ShowWindow(IntPtr hWnd, int nCmdShow)
    [<DllImport("USER32.DLL")>]
    extern IntPtr SetWindowLongPtrA(IntPtr hWnd, int nIndex, IntPtr dwNewLong)
    [<DllImport("USER32.DLL")>]
    extern IntPtr GetWindowLongPtrA(IntPtr hWnd, int nIndex)

//////////////////////////////////////////////////////

type IEventingReader<'T> =
    abstract Value : 'T with get
    abstract Changed : IEvent<unit>
type Eventing<'T when 'T:equality>(orig:'T) =
    let mutable state = orig
    let e = new Event<unit>()
    member this.Value with get() = state and set(x) = let orig=state in state <- x; if orig<>state then e.Trigger()
    member this.Changed = e.Publish
    interface IEventingReader<'T> with
        member this.Value with get() = this.Value
        member this.Changed = this.Changed
type EventingBool = Eventing<bool>
type EventingInt = Eventing<int>
type SyntheticEventingBool(recompute, changesToWatch:seq<IEvent<unit>>) =
    let b = new EventingBool(recompute())
    do
        for e in changesToWatch do
            e.Add(fun _ -> b.Value <- recompute())
    member this.Value = b.Value
    member this.Changed = b.Changed
    interface IEventingReader<bool> with
        member this.Value with get() = this.Value
        member this.Changed = this.Changed

type UISettlingEvent(ms, evs:IEvent<unit>[]) =
    let e = new Event<unit>()
    let mutable dt = new System.Windows.Threading.DispatcherTimer(System.Windows.Threading.DispatcherPriority.Loaded, Interval=System.TimeSpan.FromMilliseconds(float ms))
    let mutable moreEvents = false
    let trigger() =
        moreEvents <- true
        if not dt.IsEnabled then
            dt.Start()
    do
        dt.Tick.Add(fun _ -> 
            if not moreEvents then
                dt.Stop()
                e.Trigger()
            else
                moreEvents <- false
            )
        for ev in evs do
            ev.Add(fun _ -> trigger())
    member this.ChangedAndSettled = e.Publish

/////////////////////////////////////////////////////

open System.Windows
open System.Windows.Controls

module Extensions =
    type DockPanel with
        member this.AddLeft(x) =
            DockPanel.SetDock(x, Dock.Left)
            this.Children.Add(x) |> ignore
            this
        member this.AddRight(x) =
            DockPanel.SetDock(x, Dock.Right)
            this.Children.Add(x) |> ignore
            this
        member this.AddTop(x) =
            DockPanel.SetDock(x, Dock.Top)
            this.Children.Add(x) |> ignore
            this
        member this.AddBottom(x) =
            DockPanel.SetDock(x, Dock.Bottom)
            this.Children.Add(x) |> ignore
            this
        member this.Add(x) =
            this.Children.Add(x) |> ignore
            this

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
///////////////////
// from https://stackoverflow.com/a/32841840/5599927
type SharedBitmapSource(bmp:System.Drawing.Bitmap) as this =
    inherit System.Windows.Media.Imaging.BitmapSource()

    let mutable disposed  = false
    do  
        if bmp.PixelFormat <> System.Drawing.Imaging.PixelFormat.Format32bppArgb then
            failwith "bad pixel fmt"
        this.Freeze()

    member this.Bmp = bmp
    override this.DpiX = float bmp.HorizontalResolution
    override this.DpiY = float bmp.VerticalResolution
    override this.PixelHeight = bmp.Height
    override this.PixelWidth = bmp.Width
    override this.Format = System.Windows.Media.PixelFormats.Bgra32  // NOTE only works with these
    override this.Palette = null

    override this.Finalize() = this.Dispose(false)

    override this.CopyPixels(sourceRect:Int32Rect, pixels : System.Array, stride:int, offset:int) =
        let sourceData = bmp.LockBits(new System.Drawing.Rectangle(sourceRect.X, sourceRect.Y, sourceRect.Width, sourceRect.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, bmp.PixelFormat)
        let length = sourceData.Stride * sourceData.Height
        match box pixels with
        | :? (byte[]) as bytes -> System.Runtime.InteropServices.Marshal.Copy(sourceData.Scan0, bytes, 0, length)
        | _ -> ()
        bmp.UnlockBits(sourceData)

    override this.CreateInstanceCore() = System.Activator.CreateInstance(this.GetType()) :?> Freezable

    member this.Dispose(disposing) =
        if not disposed then
            if disposing then
                // Free other state (managed objects).
                ()
            // Free your own state (unmanaged objects).
            // Set large fields to null.
            disposed <- true
    interface System.IDisposable with
        member this.Dispose() = 
            this.Dispose(true)
            System.GC.SuppressFinalize(this)

///////////////////
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
let SetAndGetAndTransformColorFromLockedFormat32BppArgb(rx,ry,rbmd:BitmapData,x,y,bmd:BitmapData,tf) =   // r is result
    let PixelSize = 4
    let rowOffset = y * bmd.Stride
    let colOffset = x * PixelSize
    let ptr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt bmd.Scan0
    let b = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 0)
    let g = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 1)
    let r = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 2)
    let a = NativeInterop.NativePtr.get ptr (rowOffset + colOffset + 3)
    let a,r,g,b = tf(a,r,g,b)
    let rrowOffset = ry * rbmd.Stride
    let rcolOffset = rx * PixelSize
    let rptr : nativeptr<byte> = NativeInterop.NativePtr.ofNativeInt rbmd.Scan0
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 0) b
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 1) g
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 2) r
    NativeInterop.NativePtr.set rptr (rrowOffset + rcolOffset + 3) a
let SetAndGetColorFromLockedFormat32BppArgb(rx,ry,rbmd:BitmapData,x,y,bmd:BitmapData) =   // r is result
    SetAndGetAndTransformColorFromLockedFormat32BppArgb(rx,ry,rbmd,x,y,bmd,id)

let LoadBitmapWithoutLockingFile(filename:string) =
    use bmp = new Bitmap(filename)   // keeps file locked until required to make a deep copy into memory
    let r = new Bitmap(bmp)  // deep clone
    r

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

// from https://stackoverflow.com/questions/309149/generate-distinctly-different-rgb-colors-in-graphs
let DistinctColors = [|
    System.Windows.Media.Color.FromRgb(0x00uy, 0x00uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xFFuy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x00uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x00uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x01uy, 0xFFuy, 0xFEuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xA6uy, 0xFEuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xDBuy, 0x66uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x64uy, 0x01uy)
    System.Windows.Media.Color.FromRgb(0x01uy, 0x00uy, 0x67uy)
    System.Windows.Media.Color.FromRgb(0x95uy, 0x00uy, 0x3Auy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x7Duy, 0xB5uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x00uy, 0xF6uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xEEuy, 0xE8uy)
    System.Windows.Media.Color.FromRgb(0x77uy, 0x4Duy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x90uy, 0xFBuy, 0x92uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x76uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0xD5uy, 0xFFuy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x93uy, 0x7Euy)
    System.Windows.Media.Color.FromRgb(0x6Auy, 0x82uy, 0x6Cuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x02uy, 0x9Duy)
    System.Windows.Media.Color.FromRgb(0xFEuy, 0x89uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x7Auy, 0x47uy, 0x82uy)
    System.Windows.Media.Color.FromRgb(0x7Euy, 0x2Duy, 0xD2uy)
    System.Windows.Media.Color.FromRgb(0x85uy, 0xA9uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x00uy, 0x56uy)
    System.Windows.Media.Color.FromRgb(0xA4uy, 0x24uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xAEuy, 0x7Euy)
    System.Windows.Media.Color.FromRgb(0x68uy, 0x3Duy, 0x3Buy)
    System.Windows.Media.Color.FromRgb(0xBDuy, 0xC6uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0x26uy, 0x34uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0xBDuy, 0xD3uy, 0x93uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xB9uy, 0x17uy)
    System.Windows.Media.Color.FromRgb(0x9Euy, 0x00uy, 0x8Euy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x15uy, 0x44uy)
    System.Windows.Media.Color.FromRgb(0xC2uy, 0x8Cuy, 0x9Fuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x74uy, 0xA3uy)
    System.Windows.Media.Color.FromRgb(0x01uy, 0xD0uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x47uy, 0x54uy)
    System.Windows.Media.Color.FromRgb(0xE5uy, 0x6Fuy, 0xFEuy)
    System.Windows.Media.Color.FromRgb(0x78uy, 0x82uy, 0x31uy)
    System.Windows.Media.Color.FromRgb(0x0Euy, 0x4Cuy, 0xA1uy)
    System.Windows.Media.Color.FromRgb(0x91uy, 0xD0uy, 0xCBuy)
    System.Windows.Media.Color.FromRgb(0xBEuy, 0x99uy, 0x70uy)
    System.Windows.Media.Color.FromRgb(0x96uy, 0x8Auy, 0xE8uy)
    System.Windows.Media.Color.FromRgb(0xBBuy, 0x88uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x43uy, 0x00uy, 0x2Cuy)
    System.Windows.Media.Color.FromRgb(0xDEuy, 0xFFuy, 0x74uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xFFuy, 0xC6uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xE5uy, 0x02uy)
    System.Windows.Media.Color.FromRgb(0x62uy, 0x0Euy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x8Fuy, 0x9Cuy)
    System.Windows.Media.Color.FromRgb(0x98uy, 0xFFuy, 0x52uy)
    System.Windows.Media.Color.FromRgb(0x75uy, 0x44uy, 0xB1uy)
    System.Windows.Media.Color.FromRgb(0xB5uy, 0x00uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xFFuy, 0x78uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x6Euy, 0x41uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x5Fuy, 0x39uy)
    System.Windows.Media.Color.FromRgb(0x6Buy, 0x68uy, 0x82uy)
    System.Windows.Media.Color.FromRgb(0x5Fuy, 0xADuy, 0x4Euy)
    System.Windows.Media.Color.FromRgb(0xA7uy, 0x57uy, 0x40uy)
    System.Windows.Media.Color.FromRgb(0xA5uy, 0xFFuy, 0xD2uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xB1uy, 0x67uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x9Buy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0xE8uy, 0x5Euy, 0xBEuy)
    |]
