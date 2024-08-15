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
    let GetCacheFilename(x,y) = System.IO.Path.Combine(GetZoneFolder(zone), prefix, sprintf "%02d-%02d.png" x y)
    let CacheToDisk(x,y,bmp : System.Drawing.Bitmap) =
        let file = GetCacheFilename(x,y)
        System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(file)) |> ignore
        if bmp = null then
            System.IO.File.Delete(file)
        else
            bmp.Save(file, System.Drawing.Imaging.ImageFormat.Png)
    member this.TryReadFromDisk(x,y) =
        (*
        let file = GetCacheFilename(x,y)
        if System.IO.File.Exists(file) then
            imgArray.[x,y] <- new System.Drawing.Bitmap(file) |> Utils.BMPtoImage
        *)
        async {
            let file = GetCacheFilename(x,y)
            if System.IO.File.Exists(file) then
                let bmp = new System.Drawing.Bitmap(file)
                let bi = bmp |> Utils.BMPtoBitmapImage
                return Some(fun() -> 
                                let img = new System.Windows.Controls.Image(Source=bi, Width=float bmp.Width, Height=float bmp.Height)
                                imgArray.[x,y] <- img
                            )
            else
                return None
        }
    member this.Item with get(x,y) = imgArray.[x,y]
    member this.Set(x,y,bmp,writeToDisk) = 
        if writeToDisk then
            CacheToDisk(x,y,bmp)
        imgArray.[x,y] <- if bmp=null then null else Utils.BMPtoImage bmp
        rawCaches.[x,y].Clear()
    member this.GetCopyOfBmp(x,y) =
        let file = GetCacheFilename(x,y)
        if System.IO.File.Exists(file) then
            new System.Drawing.Bitmap(file)
        else
            null
    member this.GetRaw(x, y, width, height) =
        match rawCaches.[x,y].TryGetValue((width,height)) with
        | false, _ ->
            let bmp = this.GetCopyOfBmp(x,y)
            let bmp = new System.Drawing.Bitmap(bmp, System.Drawing.Size(width,height))
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
    zm.FullImgArray.Set(i,j,bmp,true)
    zm.MapImgArray.Set(i,j,Utils.cropToRect(bmp,GameSpecific.MapAreaRectangle),true)
    zm.MetaImgArray.Set(i,j,Utils.cropToRect(bmp,GameSpecific.MetaAreaRectangle),true)
let RecomputeImage(i,j,zm:ZoneMemory) =
    let bmp = RecomputeBitmap(i,j,zm)
    RecomputeImageCore(i,j,bmp,zm)

let LoadZoneMapTiles(zm:ZoneMemory) =
    // load map tile data and screenshots from disk
    let asyncs = ResizeArray()
    let codas = ResizeArray()
    let bgWork = ResizeArray()
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            zm.FullImgArray.Set(i,j,null,false)    // we might have changed zones, null out old value
            zm.MapImgArray.Set(i,j,null,false)    // we might have changed zones, null out old value
            zm.MetaImgArray.Set(i,j,null,false)    // we might have changed zones, null out old value
            let file = MapTileFilename(i,j,zm.Zone)
            if System.IO.File.Exists(file) then
                let json = System.IO.File.ReadAllText(file)
                let data = System.Text.Json.JsonSerializer.Deserialize<MapTile>(json)
                data.Canonicalize()
                zm.MapTiles.[i,j] <- data
                metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,i,j), "", data.Note)
                if data.ThereAreScreenshots() then
                    for swk in data.ScreenshotsWithKinds do
                        if not(bmpDict.ContainsKey(swk.Id)) then
                            let ssFile = ScreenshotFilenameFromTimestampId(swk.Id)
                            let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                            bmpDict.Add(swk.Id, bmp)
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

