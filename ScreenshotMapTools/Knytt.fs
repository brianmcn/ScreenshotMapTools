module Knytt

module Screenshot =
    let getScreenBitmap() =
        let w,h = 960-0,788-248   // how big the area to screenshot
        let bmpScreenshot = new System.Drawing.Bitmap(w, h, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let gfxScreenshot = System.Drawing.Graphics.FromImage(bmpScreenshot)
        let left,top = 0,248   // where it is on screen in the Windows+Left docked media player
        gfxScreenshot.CopyFromScreen(left, top, 0, 0, System.Drawing.Size(w,h), System.Drawing.CopyPixelOperation.SourceCopy)
        bmpScreenshot

open Utils
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media

let MAXX = 48
let MAXY = 30
let BG = new SolidColorBrush(Color.FromRgb(0uy, 60uy, 60uy))
type MyWindow() as this =
    inherit Window()
    do
        this.Title <- "Knytt Underground Screenshot Mapper"
        this.Left <- 950.
        this.Top <- 10.
        this.Topmost <- true
        this.SizeToContent <- SizeToContent.WidthAndHeight

        let mutable curX = 0
        let mutable curY = 0
        
        let makeSSID(i,j) = sprintf "Knytt.%02d.%02d.png" i j
        let imgStore = new System.Collections.Generic.Dictionary<_,_>()
        let zoomImgStore = new System.Collections.Generic.Dictionary<_,_>()
        let bmpStore = new System.Collections.Generic.Dictionary<_,_>()
        
        let coordSP = new StackPanel(Orientation=Orientation.Horizontal)
        let xTB = new TextBox(Text="00", IsReadOnly=true)
        let yTB = new TextBox(Text="00", IsReadOnly=true)
        let explorationPercentTB = new TextBox(Text="", IsReadOnly=true)
        coordSP.Children.Add(xTB) |> ignore
        coordSP.Children.Add(yTB) |> ignore
        coordSP.Children.Add(explorationPercentTB) |> ignore

        let topBar = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(coordSP, Dock.Left)
        topBar.Children.Add(coordSP) |> ignore
        let buttonExplainerSP = new StackPanel(Orientation=Orientation.Horizontal)
        let saveAsGiantPngButton = new Button(Content="SAVE")
        buttonExplainerSP.Children.Add(saveAsGiantPngButton) |> ignore
        buttonExplainerSP.Children.Add(new TextBox(Text="Left: Take screenshot & paste", IsReadOnly=true)) |> ignore
        buttonExplainerSP.Children.Add(new TextBox(Text="Middle: Delete", IsReadOnly=true)) |> ignore
        buttonExplainerSP.Children.Add(new TextBox(Text="Right: Copy then paste", IsReadOnly=true)) |> ignore
        DockPanel.SetDock(buttonExplainerSP, Dock.Right)
        topBar.Children.Add(buttonExplainerSP) |> ignore
        let copyDeleteStatusTB = new TextBox(Text="", IsReadOnly=true, Foreground=Brushes.Red, Background=Brushes.Black, HorizontalAlignment=HorizontalAlignment.Center)
        topBar.Children.Add(centerWithGrid copyDeleteStatusTB) |> ignore

        saveAsGiantPngButton.Click.Add(fun _ ->
            let BIGX,BIGY = 480,270
            let whole = new Canvas(Width=float(BIGX*MAXX), Height=float(BIGY*MAXY))
            let giantGrid = makeGrid(MAXX,MAXY,BIGX,BIGY)
            whole.Children.Add(giantGrid) |> ignore
            let minimap = new System.Drawing.Bitmap("""C:\Users\Admin1\Desktop\KnyttMinimap.png""")
            let mini = BMPtoImage minimap
            mini.Width <- whole.Width
            mini.Height <- whole.Height
            mini.Stretch <- Media.Stretch.Fill
            mini.Opacity <- 0.4
            whole.Children.Add(mini) |> ignore
            giantGrid.Background <- Brushes.DarkSlateGray
            giantGrid.ShowGridLines <- true
            printf "saving..."
            for x = 0 to MAXX-1 do
                for y = 0 to MAXY-1 do
                    let ssid = makeSSID(x,y)
                    match bmpStore.TryGetValue(ssid) with
                    | true, bmp -> 
                        let img = BMPtoImage bmp
                        img.Width <- float BIGX
                        img.Height <- float BIGY
                        img.Stretch <- Media.Stretch.Uniform
                        gridAdd(giantGrid, img, x, y)
                    | _ -> ()
                printf "."
            printf "... and ... "
            // render giantGrid control as a file to disk
            let size = Size(float(MAXX*BIGX), float(MAXY*BIGY))
            whole.Measure(size)
            whole.Arrange(Rect(size))
            let rtb = new System.Windows.Media.Imaging.RenderTargetBitmap(int size.Width, int size.Height, 96., 96., PixelFormats.Pbgra32)
            rtb.Render(whole)
            let encoder = new System.Windows.Media.Imaging.PngBitmapEncoder()
            encoder.Frames.Add(System.Windows.Media.Imaging.BitmapFrame.Create(rtb))
            use stream = System.IO.File.Create("GiantWithMini.png")
            encoder.Save(stream)
            printf "almost ... "
            mini.Opacity <- 0.0
            rtb.Render(whole)
            let encoder = new System.Windows.Media.Imaging.PngBitmapEncoder()
            encoder.Frames.Add(System.Windows.Media.Imaging.BitmapFrame.Create(rtb))
            use stream = System.IO.File.Create("GiantSansMini.png")
            encoder.Save(stream)
            printfn "done!"
            )

        let updateExplorationPercent() =
            let a,b = imgStore.Count, (MAXX*MAXY)
            let pct = 100. * float a / float b
            explorationPercentTB.Text <- sprintf " %3.1f%% (%d/%d) explored" pct a b
        let cursor = new Canvas(Background=Brushes.Yellow, Opacity=0.5)

        let SZX, SZY = 20, 20
        let cursorRectangle = new Shapes.Rectangle(Width=float(SZX*5), Height=float(SZY*3), Stroke=Brushes.Yellow, StrokeThickness=2.)
        canvasAdd(cursor, cursorRectangle, -float(SZX*2), -float(SZY))

        let zoomPreviewGrid = makeGrid(5,3,160,90)
        zoomPreviewGrid.Background <- Brushes.Black
        zoomPreviewGrid.ShowGridLines <- true
        let updateZoomPreview() =
            zoomPreviewGrid.Children.Clear()
            for i = 0 to 4 do
                for j = 0 to 2 do
                    let x,y = curX-2+i,curY-1+j
                    let ssid = makeSSID(x,y)
                    match zoomImgStore.TryGetValue(ssid) with
                    | true, img -> gridAdd(zoomPreviewGrid, img, i, j)
                    | _ -> 
                        let outside = x<0 || y<0 || x>=MAXX || y>=MAXY 
                        gridAdd(zoomPreviewGrid, new Canvas(Background=if outside then Brushes.DarkSlateBlue else Brushes.DarkSlateGray), i, j)
        let previewGrid = makeGrid(MAXX,MAXY,SZX,SZY)
        previewGrid.Background <- Brushes.Black
        let previewGridBlanks = Array2D.init MAXX MAXY (fun _ _ -> new Border(BorderThickness=Thickness(1.0, 1.0, 0., 0.), BorderBrush=Brushes.DarkSlateGray, Background=BG))
        let updatePreview() =
            previewGrid.Children.Clear()
            for i = 0 to MAXX-1 do
                for j = 0 to MAXY-1 do
                    let ssid = makeSSID(i,j)
                    match imgStore.TryGetValue(ssid) with
                    | true, img -> gridAdd(previewGrid, img, i, j)
                    | _ -> gridAdd(previewGrid, previewGridBlanks.[i,j], i, j)
            gridAdd(previewGrid, cursor, curX, curY)
        let updateCursor() =
            Grid.SetColumn(cursor, curX)
            Grid.SetRow(cursor, curY)
            xTB.Text <- sprintf "%02d" curX
            yTB.Text <- sprintf "%02d" curY
        let addBmp(ssid, bmp) =
            bmpStore.[ssid] <- bmp
            let img = BMPtoImage bmp
            img.Width <- float SZX
            img.Height <- float SZY
            img.Stretch <- Media.Stretch.Fill
            imgStore.[ssid] <- img
            let img = BMPtoImage bmp
            img.Width <- 160.
            img.Height <- 90.
            img.Stretch <- Media.Stretch.Uniform
            zoomImgStore.[ssid] <- img
            updateExplorationPercent()
        let addBI(ssid, bmp, bi:System.Windows.Media.Imaging.BitmapImage) =
            bmpStore.[ssid] <- bmp
            let img = new Image(Source=bi)
            img.Width <- float SZX
            img.Height <- float SZY
            img.Stretch <- Media.Stretch.Fill
            imgStore.[ssid] <- img
            let img = new Image(Source=bi)
            img.Width <- 160.
            img.Height <- 90.
            img.Stretch <- Media.Stretch.Uniform
            zoomImgStore.[ssid] <- img
            updateExplorationPercent()
        let remove(ssid) = 
            bmpStore.Remove(ssid) |> ignore
            imgStore.Remove(ssid) |> ignore
            zoomImgStore.Remove(ssid) |> ignore
            System.IO.File.Delete(ssid)
            updateExplorationPercent()
            updatePreview()
            updateZoomPreview()
        previewGrid.MouseMove.Add(fun ea ->
            let pos = ea.GetPosition(previewGrid)
            curX <- int pos.X / SZX
            curY <- int pos.Y / SZY
            updateCursor()
            updateZoomPreview()
            )
        let mutable bmpCopy = null
        let mutable ssidToDelete = ""
        previewGrid.MouseDown.Add(fun ea -> 
            let pos = ea.GetPosition(previewGrid)
            curX <- int pos.X / SZX
            curY <- int pos.Y / SZY
            let ssid = makeSSID(curX,curY)
            if ea.ChangedButton = Input.MouseButton.Left then
                bmpCopy <- null; copyDeleteStatusTB.Text <- ""; ssidToDelete <- ssid
                let bmp = Screenshot.getScreenBitmap()
                addBmp(ssid,bmp)
                bmp.Save(ssid, System.Drawing.Imaging.ImageFormat.Png)
                updatePreview()
                updateZoomPreview()
            elif ea.ChangedButton = Input.MouseButton.Right then
                if bmpCopy = null then
                    match bmpStore.TryGetValue(ssid) with
                    | true, img -> bmpCopy <- img; copyDeleteStatusTB.Text <- "COPY"; ssidToDelete <- ssid
                    | _ -> ()
                else
                    let bmp = bmpCopy
                    bmpCopy <- null; copyDeleteStatusTB.Text <- ""
                    addBmp(ssid,bmp)
                    bmp.Save(ssid, System.Drawing.Imaging.ImageFormat.Png)
                    updatePreview()
                    updateZoomPreview()
            elif ea.ChangedButton = Input.MouseButton.Middle then
                if ssidToDelete = ssid then
                    remove(ssid)
                    bmpCopy <- null; copyDeleteStatusTB.Text <- ""; ssidToDelete <- ""
                else
                    bmpCopy <- null; copyDeleteStatusTB.Text <- "CONFIRM DELETE"; ssidToDelete <- ssid
            )

        let appSP = new StackPanel(Orientation=Orientation.Vertical)
        appSP.Children.Add(topBar) |> ignore
        appSP.Children.Add(previewGrid) |> ignore
        appSP.Children.Add(new DockPanel(Background=Brushes.Gray, Height=2.)) |> ignore
        
        let bottom = new Grid(Background=Brushes.Black)
        bottom.Children.Add(new Border(BorderBrush=Brushes.Yellow, BorderThickness=Thickness(2.), Child=zoomPreviewGrid, Width=5.*160.+4.)) |> ignore
        appSP.Children.Add(bottom) |> ignore

        this.Content <- appSP

        this.Loaded.Add(fun _ ->
            // startup code
            let sw = System.Diagnostics.Stopwatch.StartNew()
            printfn "loading bitmaps from disk..."
            let ctxt = System.Threading.SynchronizationContext.Current
            let loadTasks = 
                [|
                    for i = 0 to MAXX-1 do
                        for j = 0 to MAXY-1 do
                            let ssid = makeSSID(i,j)
                            if System.IO.File.Exists(ssid) then
                                yield async {
                                    let bytes = System.IO.File.ReadAllBytes(ssid)
                                    let bmp = new System.Drawing.Bitmap(new System.IO.MemoryStream(bytes))
                                    let bi = BMPtoBitmapImage bmp
                                    printf "."
                                    return ssid,bmp,bi
                                    }
                |]
            async {
                let! bis = Async.Parallel loadTasks
                printfn ""
                printf "converting to images..."
                do! Async.SwitchToContext ctxt
                for ssid,bmp,bi in bis do
                    addBI(ssid, bmp, bi)
                    printf "."
                printfn ""
                printfn "done"
                printfn "took %d ms" sw.ElapsedMilliseconds
                updatePreview()
                updateExplorationPercent()
                } |> Async.StartImmediate
            )
