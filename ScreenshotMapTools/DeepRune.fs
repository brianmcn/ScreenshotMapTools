module DeepRune

type Zone =
    | AR // Ancient Ruins
    | FC // Forsaken Crypt
    | SC // Sunken Castle
    | CO // Cave of Origin
    member this.AsIndex = 
        match this with
        | AR -> 0
        | FC -> 1
        | SC -> 2
        | CO -> 3

type Side =
    | FRONT
    | BACK

let zones = [| AR; FC; SC; CO |]
let xs = [| 1; 2; 3; 4; 5 |]
let ys = [| 1; 2; 3 |]
let sides = [| FRONT; BACK |]

type ScreenshotID(z:Zone,x:int,y:int,s:Side) =
    member this.AsTuple = (z,x,y,s)
    member this.AsName = sprintf "%A.%d.%d.%A" z x y s
    member this.AsFileName = sprintf "%A.%d.%d.%A.png" z x y s

module Screenshot =
    let getScreenBitmap() =
        let w,h = 728,728   // how big the area to screenshot
        let bmpScreenshot = new System.Drawing.Bitmap(w, h, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let gfxScreenshot = System.Drawing.Graphics.FromImage(bmpScreenshot)
        let left,top = 255,182   // where it is on screen in the YouTube player
        gfxScreenshot.CopyFromScreen(left, top, 0, 0, System.Drawing.Size(w,h), System.Drawing.CopyPixelOperation.SourceCopy)
        bmpScreenshot

open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Utils

type MyWindow() as this =
    inherit Window()
    do
        this.Title <- "Deep Rune Screenshot Mapper"
        this.Left <- 1400.
        this.Top <- 100.
        this.Topmost <- true
        this.SizeToContent <- SizeToContent.WidthAndHeight

        let zoneCB = new ComboBox(ItemsSource=zones, SelectedIndex=0)
        let xCB = new ComboBox(ItemsSource=xs, SelectedIndex=0)
        let yCB = new ComboBox(ItemsSource=ys, SelectedIndex=0)
        let sideCB = new ComboBox(ItemsSource=sides, SelectedIndex=0)
        
        let makeSSID() = new ScreenshotID(zones.[zoneCB.SelectedIndex], xs.[xCB.SelectedIndex], ys.[yCB.SelectedIndex], sides.[sideCB.SelectedIndex])
        let imgStore = new System.Collections.Generic.Dictionary<_,_>()
        
        let takeScreenshotButton = new Button(Content=new Label(Content="Take Screenshot"))

        let combosSP = new StackPanel(Orientation=Orientation.Horizontal)
        combosSP.Children.Add(zoneCB) |> ignore
        combosSP.Children.Add(xCB) |> ignore
        combosSP.Children.Add(yCB) |> ignore
        combosSP.Children.Add(sideCB) |> ignore
        combosSP.Children.Add(takeScreenshotButton) |> ignore

        let cursor = new Canvas(Background=Brushes.Yellow, Opacity=0.5)

        let previewGrid = makeGrid(5,3,100,100)
        let updatePreview() =
            previewGrid.Children.Clear()
            let zone,x,y,side = makeSSID().AsTuple
            for i = 0 to 4 do
                for j = 0 to 2 do
                    let ssid = new ScreenshotID(zone,i+1,j+1,side)
                    match imgStore.TryGetValue(ssid.AsName) with
                    | true, img -> gridAdd(previewGrid, img, i, j)
                    | _ -> ()
            gridAdd(previewGrid, cursor, x-1, y-1)
        let updateCursor() =
            let _zone,x,y,_side = makeSSID().AsTuple
            Grid.SetColumn(cursor, x-1)
            Grid.SetRow(cursor, y-1)
        zoneCB.SelectionChanged.Add(fun _ -> updatePreview())                    
        sideCB.SelectionChanged.Add(fun _ -> updatePreview())        
        xCB.SelectionChanged.Add(fun _ -> updateCursor())
        yCB.SelectionChanged.Add(fun _ -> updateCursor())
        let addBmp(ssid:ScreenshotID, bmp) =
            let img = BMPtoImage bmp
            img.Width <- 100.
            img.Height <- 100.
            img.Stretch <- Media.Stretch.Uniform
            imgStore.[ssid.AsName] <- img
        takeScreenshotButton.Click.Add(fun _ -> 
            let bmp = Screenshot.getScreenBitmap()
            let ssid = makeSSID()
            addBmp(ssid,bmp)
            bmp.Save(ssid.AsFileName, System.Drawing.Imaging.ImageFormat.Png)
            updatePreview()
            )

        let appSP = new StackPanel(Orientation=Orientation.Vertical)
        appSP.Children.Add(combosSP) |> ignore
        appSP.Children.Add(previewGrid) |> ignore

        this.Content <- appSP

        // startup code
        for z in zones do
            for x in xs do
                for y in ys do
                    for s in sides do
                        let ssid = new ScreenshotID(z,x,y,s)
                        if System.IO.File.Exists(ssid.AsFileName) then
                            let bmp = new System.Drawing.Bitmap(ssid.AsFileName)
                            addBmp(ssid, bmp)
        updatePreview()

let drMain() =
    if true then
        let app = new Application()
        app.Run(new MyWindow())
    else
    (*
        let frontBmp = new System.Drawing.Bitmap(728*5, 728*3*4)
        for z in zones do
            for x in xs do
                for y in ys do
                    let s = Side.FRONT
                    let ssid = new ScreenshotID(z,x,y,s)
                    if System.IO.File.Exists(ssid.AsFileName) then
                        let bmp = new System.Drawing.Bitmap(ssid.AsFileName)
                        let px = (x-1)*728
                        let py = z.AsIndex*728*3 + (y-1)*728
                        for i = 0 to bmp.Width-1 do
                            for j = 0 to bmp.Height-1 do
                                frontBmp.SetPixel(px + i, py + j, bmp.GetPixel(i,j))
        frontBmp.Save("00_FRONT.png", System.Drawing.Imaging.ImageFormat.Png)
    *)
        for z in zones do
            let frontBmp = new System.Drawing.Bitmap(728*5, 728*3)
            for x in xs do
                for y in ys do
                    let s = Side.FRONT
                    let ssid = new ScreenshotID(z,x,y,s)
                    if System.IO.File.Exists(ssid.AsFileName) then
                        let bmp = new System.Drawing.Bitmap(ssid.AsFileName)
                        let px = (x-1)*728
                        let py = (y-1)*728
                        for i = 0 to bmp.Width-1 do
                            for j = 0 to bmp.Height-1 do
                                frontBmp.SetPixel(px + i, py + j, bmp.GetPixel(i,j))
            frontBmp.Save(sprintf "00_%A_FRONT.jpg" z, System.Drawing.Imaging.ImageFormat.Jpeg)
        0



