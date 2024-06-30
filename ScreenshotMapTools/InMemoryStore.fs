module InMemoryStore

open BackingStoreData

type SSID = string

// this is all for the current loaded zone
let MAX = 100
let mapTiles = Array2D.create MAX MAX (MapTile())             // backing store data
let metadataStore = GenericMetadata.MetadataStore()
let bmpDict = new System.Collections.Generic.Dictionary<SSID,System.Drawing.Bitmap>()
let imgArray : System.Windows.Controls.Image[,] = Array2D.zeroCreate MAX MAX          // representative single image per screen, displayed on the grid map

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
    if bmp = null then 
        null
    else
        Utils.BMPtoImage(bmp)

let LoadZoneMapTiles(alsoLoadImages) =
    // load map tile data and screenshots from disk
    let bgWork = ResizeArray()
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            let file = MapTileFilename(i,j)
            if System.IO.File.Exists(file) then
                let json = System.IO.File.ReadAllText(file)
                let data = System.Text.Json.JsonSerializer.Deserialize<MapTile>(json)
                mapTiles.[i,j] <- data
                metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,i,j), "", data.Note)
                imgArray.[i,j] <- null
                if alsoLoadImages && data.Screenshots <> null && data.Screenshots.Length > 0 then
                    for ts in data.Screenshots do
                        let ssFile = ScreenshotFilenameFromTimestampId(ts)
                        let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                        bmpDict.Add(ts, bmp)
                    bgWork.Add((i,j))
            else
                mapTiles.[i,j] <- MapTile()
                imgArray.[i,j] <- null
    let cde = new System.Threading.CountdownEvent(bgWork.Count)
    let fgWork = new System.Collections.Concurrent.ConcurrentBag<_>()
    let ctxt = System.Threading.SynchronizationContext.Current
    for (i,j) in bgWork do
        async {
            let bmp = RecomputeBitmap(i,j)
            if bmp <> null then
                fgWork.Add(fun () -> imgArray.[i,j] <- Utils.BMPtoImage(bmp))
            cde.Signal() |> ignore
        } |> Async.Start
    cde.Wait()
    for f in fgWork do
        f()

