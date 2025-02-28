module InMemoryStore

open BackingStoreData

type SSID = string

let MAX = 100

let FULL = 0
let MAP  = 1
let META = 2

// caches are per-zone
type ImgArrayCache(proj,zone) =
    let prefix = 
        match proj with
        | x when x=FULL -> "full-cache"
        | x when x=MAP  -> "map-cache"
        | x when x=META -> "meta-cache"
        | _ -> failwith "bad projection type"
    let imgArray : System.Windows.Controls.Image[,] = Array2D.zeroCreate MAX MAX          // representative single image per screen, displayed on the grid map
    let rawCaches = Array2D.init MAX MAX (fun _ _ -> new System.Collections.Generic.Dictionary<(int*int),byte[]>())   // BGRA data of screen[x,y] when resized to (w,h)
    let ownedBmps = Array2D.zeroCreate MAX MAX
    let downsampledBmps = Array2D.zeroCreate MAX MAX
    let cachedFilenames = Array2D.zeroCreate MAX MAX
    let doesFileExist = Array2D.zeroCreate MAX MAX         // 0=unknown, 111=yes, 222=no
    let GetCacheFilename(x,y) = 
        if cachedFilenames.[x,y] = null then
            cachedFilenames.[x,y] <- System.IO.Path.Combine(GetZoneFolder(zone), prefix, sprintf "%02d-%02d.png" x y)
        cachedFilenames.[x,y]
    let CacheToDisk(x,y,bmp : System.Drawing.Bitmap) =
        let file = GetCacheFilename(x,y)
        System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(file)) |> ignore
        if bmp = null then
            System.IO.File.Delete(file)
            doesFileExist.[x,y] <- 222
        else
            bmp.Save(file, System.Drawing.Imaging.ImageFormat.Png)
            doesFileExist.[x,y] <- 111
    member this.TryReadFromDisk(x,y) =
        async {
            let file = GetCacheFilename(x,y)
            if System.IO.File.Exists(file) then
                doesFileExist.[x,y] <- 111
                (*
                //let bmp = new System.Drawing.Bitmap(file)
                //let bi = bmp |> Utils.BMPtoBitmapImage
                use fs = System.IO.File.Open(file, System.IO.FileMode.Open)
                let bytes = Array.zeroCreate (int fs.Length)
                let! _ = fs.ReadAsync(bytes, 0, int fs.Length) |> Async.AwaitTask
                let ms = new System.IO.MemoryStream(bytes)
                let bmp = new System.Drawing.Bitmap(ms)
                ms.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
                let decoder = new System.Windows.Media.Imaging.PngBitmapDecoder(ms, System.Windows.Media.Imaging.BitmapCreateOptions.PreservePixelFormat, System.Windows.Media.Imaging.BitmapCacheOption.OnLoad)
                let bi = decoder.Frames.[0]
                *)
                let bmp = new System.Drawing.Bitmap(file)
                let bi = new Utils.SharedBitmapSource(bmp)
                ownedBmps.[x,y] <- bmp
                downsampledBmps.[x,y] <- new System.Drawing.Bitmap(bmp, System.Drawing.Size(max 1 (bmp.Width/2),max 1 (bmp.Height/2)))
                return Some(fun() -> 
                                let img = new System.Windows.Controls.Image(Source=bi, Width=float bmp.Width, Height=float bmp.Height)
                                imgArray.[x,y] <- img
                            )
            else
                return None
        }
    member this.Item with get(x,y) = imgArray.[x,y]
    member this.Set(x,y,bmp) = 
        CacheToDisk(x,y,bmp)
        imgArray.[x,y] <- if bmp=null then null else Utils.BMPtoImage bmp
        rawCaches.[x,y].Clear()
        ownedBmps.[x,y] <- if bmp=null then null else new System.Drawing.Bitmap(bmp, System.Drawing.Size(bmp.Width,bmp.Height))
        downsampledBmps.[x,y] <- if bmp=null then null else new System.Drawing.Bitmap(bmp, System.Drawing.Size(max 1 (bmp.Width/2),max 1 (bmp.Height/2)))
    member this.HasBmp(x,y) =
        match doesFileExist.[x,y] with
        | 0 ->
            let file = GetCacheFilename(x,y)
            let r = System.IO.File.Exists(file)
            //printfn "has(%d,%d)=%A" x y r
            if r then
                doesFileExist.[x,y] <- 111
            else
                doesFileExist.[x,y] <- 222
            r
        | 111 -> true
        | 222 -> false
        | _ -> failwith "impossible doesFileExist value"
    member this.GetCopyOfBmp(x,y) =
        if ownedBmps.[x,y] <> null then
            let bmp = ownedBmps.[x,y]
            new System.Drawing.Bitmap(bmp, System.Drawing.Size(bmp.Width,bmp.Height))
        else
            match doesFileExist.[x,y] with 
            | 222 -> null
            | _ ->
                let file = GetCacheFilename(x,y)
                if System.IO.File.Exists(file) then
                    doesFileExist.[x,y] <- 111
                    let bmp = Utils.LoadBitmapWithoutLockingFile(file)
                    ownedBmps.[x,y] <- new System.Drawing.Bitmap(bmp, System.Drawing.Size(bmp.Width,bmp.Height))
                    downsampledBmps.[x,y] <- new System.Drawing.Bitmap(bmp, System.Drawing.Size(bmp.Width/2,bmp.Height/2))
                    bmp
                else
                    doesFileExist.[x,y] <- 222
                    null
    member this.GetRaw(x, y, width, height) =
        match rawCaches.[x,y].TryGetValue((width,height)) with
        | false, _ ->
            let bmp = new System.Drawing.Bitmap(downsampledBmps.[x,y], System.Drawing.Size(width,height))
            let byteArray = Utils.ConvertBmpToBGRA(bmp)
            bmp.Dispose()
            rawCaches.[x,y].Add((width,height), byteArray)
            byteArray
        | _, r -> r

// these are global to all zones
let metadataStore = GenericMetadata.MetadataStore()
let bmpDict = new System.Collections.Generic.Dictionary<SSID,System.Drawing.Bitmap>()    // contains all screenshots, regardless of Kind
// per-zone stuff 
type ZoneMemory(zone:int) =
    static let dict = new System.Collections.Generic.Dictionary<int,ZoneMemory>()
    let mapTiles = Array2D.init MAX MAX (fun _ _ -> MapTile())             // backing store data
    let fullImgArray = ImgArrayCache(0,zone)
    let mapImgArray = ImgArrayCache(1,zone)
    let metaImgArray = ImgArrayCache(2,zone)
    member this.MapTiles = mapTiles
    member this.FullImgArray = fullImgArray
    member this.MapImgArray = mapImgArray
    member this.MetaImgArray = metaImgArray
    member this.Zone = zone
    static member Get(z) =
        if dict.ContainsKey(z) then
            dict.[z]
        else
            let zm = new ZoneMemory(z)
            dict.[z] <- zm
            zm

let RecomputeBitmap(i,j,zm:ZoneMemory) =
    let data = zm.MapTiles.[i,j]
    if data.ThereAreScreenshots() then
        let bmps = ResizeArray()
        for swk in data.ScreenshotsWithKinds do
            if swk.IsMainKind() then
                let bmp = bmpDict.[swk.Id]
                bmps.Add(bmp)
        MultipleScreenshotForOneScreen.GetRepresentative(bmps)
    else
        null

let RecomputeImageCore(i,j,bmp,zm:ZoneMemory) =
    zm.FullImgArray.Set(i,j,bmp)
    zm.MapImgArray.Set(i,j,Utils.cropToRect(bmp,GameSpecific.MapAreaRectangle))
    zm.MetaImgArray.Set(i,j,Utils.cropToRect(bmp,GameSpecific.MetaAreaRectangle))
let RecomputeImage(i,j,zm:ZoneMemory) =
    let bmp = RecomputeBitmap(i,j,zm)
    RecomputeImageCore(i,j,bmp,zm)

let LoadZoneMapTiles(zm:ZoneMemory) =
    // load map tile data and screenshots from disk
    let asyncs = ResizeArray()
    let codas = ResizeArray()
    let bgWork = ResizeArray()
    let mutable bmpCount = 0
    let mutable mapTileCount = 0
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            let file = MapTileFilename(i,j,zm.Zone)
            if System.IO.File.Exists(file) then
                let json = System.IO.File.ReadAllText(file)
                let data = System.Text.Json.JsonSerializer.Deserialize<MapTile>(json)
                data.Canonicalize()
                zm.MapTiles.[i,j] <- data
                mapTileCount <- mapTileCount + 1
                metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,i,j), "", data.Note)
                if data.ThereAreScreenshots() then
                    for swk in data.ScreenshotsWithKinds do
                        if not(bmpDict.ContainsKey(swk.Id)) then
                            let ssFile = ScreenshotFilenameFromTimestampId(swk.Id)
                            if System.IO.File.Exists(ssFile) then
                                let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                                bmpDict.Add(swk.Id, bmp)
                                bmpCount <- bmpCount + 1
                            else
                                printfn "warning, file '%s' reference by (zone%d,%d,%d) was not found" ssFile theGame.CurZone i j
                    asyncs.Add(zm.FullImgArray.TryReadFromDisk(i,j))
                    asyncs.Add(zm.MapImgArray.TryReadFromDisk(i,j))
                    asyncs.Add(zm.MetaImgArray.TryReadFromDisk(i,j))
                    codas.Add(fun() ->
                        if zm.FullImgArray.[i,j] = null then   // assume 3 disks stay in sync
                            bgWork.Add((i,j))
                        )
            else
                let mt = MapTile()
                mt.Canonicalize()
                zm.MapTiles.[i,j] <- mt 
    let nexts = Async.Parallel asyncs |> Async.RunSynchronously
    for fo in nexts do
        match fo with
        | Some f -> f()   // converts BitmapImages to Images
        | _ -> ()
    for f in codas do
        f()     // populate bgwork
    // recompute bitmaps that were not cached
    let cde = new System.Threading.CountdownEvent(bgWork.Count)
    let fgWork = new System.Collections.Concurrent.ConcurrentBag<_>()
    let ctxt = System.Threading.SynchronizationContext.Current
    for (i,j) in bgWork do
        async {
            let bmp = RecomputeBitmap(i,j,zm)
            if bmp <> null then
                fgWork.Add(fun () -> RecomputeImageCore(i,j,bmp,zm))
            cde.Signal() |> ignore
        } |> Async.Start
    cde.Wait()
    for f in fgWork do
        f()
    bmpCount, mapTileCount

