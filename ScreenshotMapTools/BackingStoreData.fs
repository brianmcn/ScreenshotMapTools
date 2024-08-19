module BackingStoreData

module Win32 =
    [<System.Runtime.InteropServices.DllImport("User32.dll")>]
    extern bool PrintWindow(System.IntPtr hwnd, nativeint hdcBlt, uint32 nFlags)

let GetWindowScreenshot(hwnd:System.IntPtr, w, h) =
    let bmp = new System.Drawing.Bitmap(w, h, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
    let gfxBmp = System.Drawing.Graphics.FromImage(bmp)
    let hdcBitmap = gfxBmp.GetHdc()
    let PW_CLIENTONLY = 1u
    let PW_RENDERFULLCONTENT = 2u
    let succeeded = Win32.PrintWindow(hwnd, hdcBitmap, PW_RENDERFULLCONTENT ||| PW_CLIENTONLY)   // theoretically might run faster if topmost, but in practice, I didn't see difference, and this is not the bottleneck
    gfxBmp.ReleaseHdc(hdcBitmap)
    if not succeeded then
        gfxBmp.FillRectangle(new System.Drawing.SolidBrush(System.Drawing.Color.Black), new System.Drawing.Rectangle(System.Drawing.Point.Empty, bmp.Size))
    (*
    let hRgn = Win32.CreateRectRgn(0, 0, 0, 0)
    Win32.GetWindowRgn(hwnd, hRgn) |> ignore
    let region = Region.FromHrgn(hRgn)
    if not(region.IsEmpty(gfxBmp)) then
        gfxBmp.ExcludeClip(region)
        gfxBmp.Clear(Color.Transparent)
    Win32.DeleteObject(hRgn) |> ignore
    *)
    gfxBmp.Dispose()
    bmp

//////////////////////////////////////////////////////////////////////////

open GameSpecific

//let rootFolder = System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, GAME)
let rootFolder = System.IO.Path.Combine(GAME)
let GetRootFolder() = rootFolder

let WriteAllText(filename, text) =
    let dir = System.IO.Path.GetDirectoryName(filename)
    System.IO.Directory.CreateDirectory(dir) |> ignore   // ensure directory exists
    System.IO.File.WriteAllText(filename, text)

[<AllowNullLiteral>]
type Game() =   // e.g. Zelda
    member val ZoneNames : string[] = null with get,set           // e.g. Overworld,Dungeon1 
    member val MetadataNames : string[] = null with get,set       // e.g. TakeAny,BurnBush
    member val CurX : int = 50 with get,set
    member val CurY : int = 50 with get,set
    member val CurZone : int = 0 with get,set

// load root game data
let theGame = Game()
let LoadRootGameData() =
    let gameFile = System.IO.Path.Combine(GetRootFolder(), "game.json")
    if not(System.IO.File.Exists(gameFile)) then
        theGame.ZoneNames <- [| "zone00" |]
        let json = System.Text.Json.JsonSerializer.Serialize<Game>(theGame)
        WriteAllText(gameFile, json)
    else
        let json = System.IO.File.ReadAllText(gameFile)
        let data = System.Text.Json.JsonSerializer.Deserialize<Game>(json)
        theGame.ZoneNames <- data.ZoneNames
        theGame.MetadataNames <- theGame.MetadataNames
        theGame.CurX <- data.CurX
        theGame.CurY <- data.CurY
        theGame.CurZone <- data.CurZone

// screenshots folder of yyyy-MM-dd-HH-mm-ss
let DATE_TIME_FORMAT = "yyyy-MM-dd-HH-mm-ss"
let SCREENSHOTS_FOLDER = "screenshots"
let ScreenshotFilenameFromTimestampId(id) =
    System.IO.Path.Combine(GetRootFolder(), SCREENSHOTS_FOLDER, id+".png")
let SaveScreenshot(bmp : System.Drawing.Bitmap) =
    let now = System.DateTime.Now
    let id = now.ToString(DATE_TIME_FORMAT)
    let file = ScreenshotFilenameFromTimestampId(id)
    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(file)) |> ignore
    bmp.Save(file, System.Drawing.Imaging.ImageFormat.Png)
    let img = Utils.BMPtoImage bmp
    img.Stretch <- System.Windows.Media.Stretch.Fill
    //System.Windows.Media.RenderOptions.SetBitmapScalingMode(img, System.Windows.Media.BitmapScalingMode.NearestNeighbor)
    id, img
// each zone has a folder:
// with files MapTileXnnYmm where nn/mm range 00-99
let GetZoneFolder(zone) = System.IO.Path.Combine(GetRootFolder(), sprintf "zone%02d" zone)
let rec GetZoneName(zoneNum) = 
    if zoneNum < theGame.ZoneNames.Length then
        theGame.ZoneNames.[zoneNum]
    else
        let r = [|
            for i = 0 to zoneNum do
                if i < theGame.ZoneNames.Length then
                    yield theGame.ZoneNames.[i]
                else
                    yield sprintf "zone%02d" i
            |]
        theGame.ZoneNames <- r
        GetZoneName(zoneNum)

let TakeNewScreenshot() =
    let bmp =
        let mutable r = None
        for KeyValue(hwnd,(title,rect)) in Elephantasy.Screenshot.GetOpenWindows() do
            if title.StartsWith(WINDOW_TITLE) then
                r <- Some hwnd
        match r with
        | Some(hwnd) -> GetWindowScreenshot(hwnd, GAMESCREENW, GAMESCREENH)
        | None -> failwith "window not found"
    let nativeBmp = 
        if NATIVE_FACTOR <> 1 then
            let r = new System.Drawing.Bitmap(GAMENATIVEW, GAMENATIVEH)
            for i = 0 to GAMENATIVEW-1 do
                for j = 0 to GAMENATIVEH-1 do
                    r.SetPixel(i, j, bmp.GetPixel(i*NATIVE_FACTOR, j*NATIVE_FACTOR))
            r
        else
            bmp
    let id,img = SaveScreenshot(nativeBmp)
    img, nativeBmp, id

//////////////////////////////////////////////////////////////////////////

let AAppend(a:_[],x) =
    if a=null then
        [|x|]
    else
        Array.init (a.Length+1) (fun i -> if i<a.Length then a.[i] else x)

let screenshotKindUniverse = ResizeArray()
let MAIN_KIND = "main"
do
    screenshotKindUniverse.Add(MAIN_KIND)     // the default kind

[<AllowNullLiteral>]
type ScreenshotWithKinds() =
    member val Id : string = null with get,set         // e.g. "2024-05-12-09-45-43"
    member val Kinds : string[] = null with get,set    // e.g. [| "main"; "shop" |]
    member this.IsMainKind() =  this.Kinds |> Array.contains MAIN_KIND

[<AllowNullLiteral>]
type MapTile() =   // e.g. 50,50
    member val Screenshots : string[] = null with get,set         // e.g. [ 2024-05-12-09-45-43, 2024-05-12-09-46-16 ]  // used for backward compat, before kinds; all assumed "main"
    member val ScreenshotsWithKinds : ScreenshotWithKinds[] = [||] with get,set         // new version of data
    member val Note : string = null with get,set                  // e.g. "I spawned here at start"
    member this.Canonicalize() =
        if this.Screenshots = null && this.ScreenshotsWithKinds <> null then
            for swk in this.ScreenshotsWithKinds do
                for k in swk.Kinds do
                    if not(screenshotKindUniverse.Contains(k)) then
                        screenshotKindUniverse.Add(k)
        else
            // preserve new data
            let swks = ResizeArray()
            if this.ScreenshotsWithKinds <> null then
                for swk in this.ScreenshotsWithKinds do
                    swks.Add(swk)
                    for k in swk.Kinds do
                        if not(screenshotKindUniverse.Contains(k)) then
                            screenshotKindUniverse.Add(k)
            // translate old disk data format into new disk and runtime format
            if this.Screenshots <> null then
                for ssid in this.Screenshots do
                    let swk = new ScreenshotWithKinds()
                    swk.Id <- ssid
                    swk.Kinds <- [| MAIN_KIND |]
                    swks.Add(swk)
            // new format uses only ScreenshotsWithKinds
            this.Screenshots <- null
            this.ScreenshotsWithKinds <- swks |> Seq.toArray
    member this.Assert() =
        if this.Screenshots <> null || this.ScreenshotsWithKinds = null then
            failwith "noncanonical MapTile"
    member this.IsEmpty = not(this.ThereAreScreenshots()) && (this.Note = null || this.Note="")
    member this.ThereAreScreenshots() = 
        this.Assert()
        this.ScreenshotsWithKinds.Length > 0
    member this.CutScreenshot(ssid) = 
        this.Assert()
        this.ScreenshotsWithKinds <- this.ScreenshotsWithKinds |> Array.filter (fun s -> s.Id <> ssid)
    member this.AddScreenshot(ssid) =
        this.Assert()
        let swk = new ScreenshotWithKinds()
        swk.Id <- ssid
        swk.Kinds <- [| MAIN_KIND |]
        this.ScreenshotsWithKinds <- AAppend(this.ScreenshotsWithKinds, swk)
    member this.NumScreenshots() = 
        this.Assert()
        this.ScreenshotsWithKinds.Length
let MapTileFilename(i,j,zone) = System.IO.Path.Combine(GetZoneFolder(zone), sprintf "tile%02d-%02d.json" i j)

