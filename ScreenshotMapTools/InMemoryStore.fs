﻿module InMemoryStore

open BackingStoreData

type SSID = string

let MAX = 100

// caches are per-zone
type ImgArrayCache() =
    let imgArray : System.Windows.Controls.Image[,] = Array2D.zeroCreate MAX MAX          // representative single image per screen, displayed on the grid map
    let GetCacheFilename(x,y) = System.IO.Path.Combine(GetZoneFolder(), "cache", sprintf "%02d-%02d.png" x y)
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

// this is all for the current loaded zone
let mapTiles = Array2D.create MAX MAX (MapTile())             // backing store data
let metadataStore = GenericMetadata.MetadataStore()
let bmpDict = new System.Collections.Generic.Dictionary<SSID,System.Drawing.Bitmap>()
let imgArray = ImgArrayCache()

let RecomputeBitmap(i,j) =
    let data = mapTiles.[i,j]
    if data.Screenshots <> null && data.Screenshots.Length > 0 then
        let bmps = ResizeArray()
        for ts in data.Screenshots do
            let bmp = bmpDict.[ts]
            bmps.Add(bmp)
        MultipleScreenshotForOneScreen.GetRepresentative(bmps)
    else
        null

let RecomputeImage(i,j) =
    let bmp = RecomputeBitmap(i,j)
    imgArray.Set(i,j,bmp,true)

let LoadZoneMapTiles(alsoLoadImages) =
    // load map tile data and screenshots from disk
    let bgWork = ResizeArray()
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            imgArray.Set(i,j,null,false)    // we might have changed zones, null out old value
            let file = MapTileFilename(i,j)
            if System.IO.File.Exists(file) then
                let json = System.IO.File.ReadAllText(file)
                let data = System.Text.Json.JsonSerializer.Deserialize<MapTile>(json)
                mapTiles.[i,j] <- data
                metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,i,j), "", data.Note)
                if alsoLoadImages && data.Screenshots <> null && data.Screenshots.Length > 0 then
                    for ts in data.Screenshots do
                        let ssFile = ScreenshotFilenameFromTimestampId(ts)
                        let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                        bmpDict.Add(ts, bmp)
                    imgArray.TryReadFromDisk(i,j)
                    if imgArray.[i,j] = null then
                        bgWork.Add((i,j))
            else
                mapTiles.[i,j] <- MapTile()
    let cde = new System.Threading.CountdownEvent(bgWork.Count)
    let fgWork = new System.Collections.Concurrent.ConcurrentBag<_>()
    let ctxt = System.Threading.SynchronizationContext.Current
    for (i,j) in bgWork do
        async {
            let bmp = RecomputeBitmap(i,j)
            if bmp <> null then
                fgWork.Add(fun () -> imgArray.Set(i,j,bmp,true))
            cde.Signal() |> ignore
        } |> Async.Start
    cde.Wait()
    for f in fgWork do
        f()

