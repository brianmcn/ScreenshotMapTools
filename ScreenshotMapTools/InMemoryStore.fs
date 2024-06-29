module InMemoryStore

open BackingStoreData

type SSID = string

// this is all for the current loaded zone
let MAX = 100
let mapTiles = Array2D.create MAX MAX (MapTile())             // backing store data
let metadataStore = GenericMetadata.MetadataStore()
let bmpDict = new System.Collections.Generic.Dictionary<SSID,System.Drawing.Bitmap>()
let imgArray : System.Windows.Controls.Image[,] = Array2D.zeroCreate MAX MAX          // representative single image per screen, displayed on the grid map

let RecomputeImage(i,j) =
    let data = mapTiles.[i,j]
    if data.Screenshots <> null && data.Screenshots.Length > 0 then
        let bmps = ResizeArray()
        for ts in data.Screenshots do
            let bmp = bmpDict.[ts]
            bmps.Add(bmp)
        Utils.BMPtoImage(MultipleScreenshotForOneScreen.GetRepresentative(bmps))
    else
        null

let LoadZoneMapTiles(alsoLoadImages) =
    // load map tile data and screenshots from disk
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
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
                    imgArray.[i,j] <- RecomputeImage(i,j)
                else
                    imgArray.[i,j] <- null
            else
                mapTiles.[i,j] <- MapTile()
                imgArray.[i,j] <- null
