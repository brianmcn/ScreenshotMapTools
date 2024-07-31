module InMemoryStore

open BackingStoreData

type SSID = string

let MAX = 100

let FULL = 0
let MAP  = 1
let META = 2

// caches are per-zone
type ImgArrayCache(proj) =
    let prefix = 
        match proj with
        | x when x=FULL -> "full"
        | x when x=MAP  -> "map"
        | x when x=META -> "meta"
        | _ -> failwith "bad projection type"
    let imgArray : System.Windows.Controls.Image[,] = Array2D.zeroCreate MAX MAX          // representative single image per screen, displayed on the grid map
    let rawCaches = Array2D.init MAX MAX (fun _ _ -> new System.Collections.Generic.Dictionary<(int*int),byte[]>())   // BGRA data of screen[x,y] when resized to (w,h)
    let GetCacheFilename(x,y) = System.IO.Path.Combine(GetZoneFolder(), prefix, "-cache", sprintf "%02d-%02d.png" x y)
    let CacheToDisk(x,y,bmp : System.Drawing.Bitmap) =
        let file = GetCacheFilename(x,y)
        System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(file)) |> ignore
        if bmp = null then
            System.IO.File.Delete(file)
        else
            bmp.Save(file, System.Drawing.Imaging.ImageFormat.Png)
    member this.TryReadFromDisk(x,y) =
        let file = GetCacheFilename(x,y)
        if System.IO.File.Exists(file) then
            imgArray.[x,y] <- new System.Drawing.Bitmap(file) |> Utils.BMPtoImage
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

// this is all for the current loaded zone
let mapTiles = Array2D.create MAX MAX (MapTile())             // backing store data
let metadataStore = GenericMetadata.MetadataStore()
let bmpDict = new System.Collections.Generic.Dictionary<SSID,System.Drawing.Bitmap>()    // contains all screenshots, regardless of Kind
let fullImgArray = ImgArrayCache(0)
let mapImgArray = ImgArrayCache(1)
let metaImgArray = ImgArrayCache(2)

let RecomputeBitmap(i,j) =
    let data = mapTiles.[i,j]
    if data.ThereAreScreenshots() then
        let bmps = ResizeArray()
        for swk in data.ScreenshotsWithKinds do
            if swk.IsMainKind() then
                let bmp = bmpDict.[swk.Id]
                bmps.Add(bmp)
        MultipleScreenshotForOneScreen.GetRepresentative(bmps)
    else
        null

let RecomputeImageCore(i,j,bmp) =
    fullImgArray.Set(i,j,bmp,true)
    mapImgArray.Set(i,j,Utils.cropToRect(bmp,GameSpecific.MapAreaRectangle),true)
    metaImgArray.Set(i,j,Utils.cropToRect(bmp,GameSpecific.MetaAreaRectangle),true)
let RecomputeImage(i,j) =
    let bmp = RecomputeBitmap(i,j)
    RecomputeImageCore(i,j,bmp)

let LoadZoneMapTiles(alsoLoadImages) =
    // load map tile data and screenshots from disk
    let bgWork = ResizeArray()
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            fullImgArray.Set(i,j,null,false)    // we might have changed zones, null out old value
            mapImgArray.Set(i,j,null,false)    // we might have changed zones, null out old value
            metaImgArray.Set(i,j,null,false)    // we might have changed zones, null out old value
            let file = MapTileFilename(i,j)
            if System.IO.File.Exists(file) then
                let json = System.IO.File.ReadAllText(file)
                let data = System.Text.Json.JsonSerializer.Deserialize<MapTile>(json)
                data.Canonicalize()
                mapTiles.[i,j] <- data
                metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,i,j), "", data.Note)
                if alsoLoadImages && data.ThereAreScreenshots() then
                    for swk in data.ScreenshotsWithKinds do
                        if not(bmpDict.ContainsKey(swk.Id)) then
                            let ssFile = ScreenshotFilenameFromTimestampId(swk.Id)
                            let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                            bmpDict.Add(swk.Id, bmp)
                    fullImgArray.TryReadFromDisk(i,j)
                    mapImgArray.TryReadFromDisk(i,j)
                    metaImgArray.TryReadFromDisk(i,j)
                    if fullImgArray.[i,j] = null then   // assume 3 disks stay in sync
                        bgWork.Add((i,j))
            else
                let mt = MapTile()
                mt.Canonicalize()
                mapTiles.[i,j] <- mt 
    let cde = new System.Threading.CountdownEvent(bgWork.Count)
    let fgWork = new System.Collections.Concurrent.ConcurrentBag<_>()
    let ctxt = System.Threading.SynchronizationContext.Current
    for (i,j) in bgWork do
        async {
            let bmp = RecomputeBitmap(i,j)
            if bmp <> null then
                fgWork.Add(fun () -> RecomputeImageCore(i,j,bmp))
            cde.Signal() |> ignore
        } |> Async.Start
    cde.Wait()
    for f in fgWork do
        f()

