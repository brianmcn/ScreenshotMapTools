module Generic

let APP_WIDTH = 1920 - 1280 - 12        // my monitor, a game, a little buffer
let APP_HEIGHT = 980. + 16. + 20.
let BOTTOM_HEIGHT = 480.
let KEYS_LIST_BOX_WIDTH = MapIcons.KEYS_LIST_BOX_WIDTH
let VIEWX = APP_WIDTH

//////////////////////////////////////////////////////////

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Elephantasy.Winterop
open GameSpecific
open BackingStoreData
open InMemoryStore

let mutable clipboardSSID = ""
let mutable updateClipboardView = fun() -> ()        // TODO how remove this ugly tangle

let SerializeMapTile(x,y,zm:ZoneMemory) = 
    let json = System.Text.Json.JsonSerializer.Serialize<MapTile>(zm.MapTiles.[x,y])
    WriteAllText(MapTileFilename(x,y,zm.Zone), json)

let broadcastHotKeyEv = new Event<int*IntPtr*IntPtr>()

let DoScreenshotDisplayWindow(x,y,parent:Window,zm:ZoneMemory) = 
    let closeEv = new Event<unit>()
    // layout
    let sp = new StackPanel(Orientation=Orientation.Vertical)
    let all = new ScrollViewer(VerticalScrollBarVisibility=ScrollBarVisibility.Visible, Content=sp)
    let TH = 6.
    all.Width <- float APP_WIDTH - 8. - TH*2. - 40.  // 40. for scrollbar
    all.Height <- float APP_HEIGHT - 8. - TH*2.
    all.UseLayoutRounding <- true
    // state
    let mutable whichSSIDisHighlighted = None
    let allBorders = ResizeArray()
    for swk in zm.MapTiles.[x,y].ScreenshotsWithKinds do
        let ssid = swk.Id
        let bmp = bmpDict.[ssid]
        let img = Utils.BMPtoImage(bmp)
        let largeImage = Utils.ImageProjection(img,(0,0,GAMENATIVEW,GAMENATIVEH))
        let border = new Border(BorderThickness=Thickness(TH), Child=largeImage)
        let kindPanel = new StackPanel(Orientation=Orientation.Vertical, VerticalAlignment=VerticalAlignment.Center, Margin=Thickness(0.,0.,4.,0.))
        for k in BackingStoreData.screenshotKindUniverse do
            let cb = new CheckBox(Content=k, IsChecked=(swk.Kinds |> Array.contains k))
            kindPanel.Children.Add(cb) |> ignore
            cb.Checked.Add(fun _ -> 
                swk.Kinds <- BackingStoreData.AAppend(swk.Kinds, k)
                RecomputeImage(x,y,zm)
                SerializeMapTile(x,y,zm)
                )
            cb.Unchecked.Add(fun _ -> 
                swk.Kinds <- swk.Kinds |> Array.filter (fun x -> x<> k)
                RecomputeImage(x,y,zm)
                SerializeMapTile(x,y,zm)
                )
        let dp = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(kindPanel, Dock.Right)
        dp.Children.Add(kindPanel) |> ignore
        dp.Children.Add(border) |> ignore
        sp.Children.Add(dp) |> ignore
        allBorders.Add(border)
        border.MouseDown.Add(fun ea ->
            if ea.RightButton = System.Windows.Input.MouseButtonState.Pressed then
                let img = Utils.BMPtoImage(bmp)
                img.Width <- 1280.
                img.Height <- 720.
                img.Stretch <- Stretch.Uniform
                FeatureWindow.EnsureFeature(parent.Owner, img)
            else
                //printfn "highlighting %s" ssid
                whichSSIDisHighlighted <- Some(ssid)
                for b in allBorders do
                    b.BorderBrush <- Brushes.Transparent
                border.BorderBrush <- Brushes.Orange
            )
    broadcastHotKeyEv.Publish.Add(fun (msg, wParam, lParam) ->              // TODO this leaks, but hopefully isn't used frequently
        let WM_HOTKEY = 0x0312
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Elephantasy.Winterop.HOTKEY_ID then
                let key = lParam.ToInt32() >>> 16
                if key = VK_SUBTRACT then
                    match whichSSIDisHighlighted with
                    | None -> ()
                    | Some(ssid) ->
                        //printfn "cutting %s" ssid
                        clipboardSSID <- ssid
                        updateClipboardView()
                        zm.MapTiles.[x,y].CutScreenshot(ssid)
                        // update current tile view
                        RecomputeImage(x,y,zm)
                        //zoom(...) will be called when the window closes
                        // update disk
                        SerializeMapTile(x,y,zm)
                        //printfn "closing..."
                        closeEv.Trigger()
                        //printfn "closed"
        )
    Utils.DoModalDialog(parent, all, sprintf "All Screenshots for (%d,%d)" x y, closeEv.Publish)

type MyWindow() as this = 
    inherit Window()
    let KEYS = [| VK_NUMPAD0; VK_NUMPAD1; VK_NUMPAD2; VK_NUMPAD3; VK_NUMPAD4; VK_NUMPAD5; VK_NUMPAD6; VK_NUMPAD7; VK_NUMPAD8; VK_NUMPAD9;
                    VK_MULTIPLY; VK_ADD; VK_SUBTRACT; VK_DECIMAL; VK_DIVIDE (*; VK_RETURN *) |]
    let MAPX,MAPY = VIEWX,420
    let backBuffer, backBufferStride = Array.zeroCreate (3*MAPX*3*MAPY*4), 3*MAPX*4   // 3x so I can write 'out of bounds' and clip it later
    let writeableBitmapImage = new Image(Width=float(3*MAPX), Height=float(3*MAPY))
    let mapCanvas = new Canvas(Width=float(MAPX), Height=float(MAPY), ClipToBounds=true, Background=Brushes.Transparent)  // transparent background to see mouse events even where nothing drawn
    let mapMarkersImage = new Image(Width=float(3*MAPX), Height=float(3*MAPY), IsHitTestVisible=false)
    let mapMarkersHoverImage = new Image(Width=float(3*MAPX), Height=float(3*MAPY), IsHitTestVisible=false)
    let wholeMapCanvas =
        let r = new Canvas(Width=float(MAPX), Height=float(MAPY), ClipToBounds=true, Background=Brushes.Gray)
        Utils.canvasAdd(r, writeableBitmapImage, float(-MAPX), float(-MAPY))
        r.Children.Add(mapCanvas) |> ignore
        Utils.canvasAdd(r, mapMarkersImage, float(-MAPX), float(-MAPY))
        Utils.canvasAdd(r, mapMarkersHoverImage, float(-MAPX), float(-MAPY))
        r
    let RT = 4.
    let mouseCursor = new Shapes.Rectangle(StrokeThickness=RT/2.)
    let bottomFloat = new Canvas(Width=float APP_WIDTH, Height=BOTTOM_HEIGHT)    // a place to draw over the bottom potion of the app
    let mutable mapCanvasMouseMoveFunc = fun _ -> ()
    let mutable mapCanvasMouseDownFunc = fun (_:Input.MouseEventArgs,_x,_y) -> ()
    let mutable curZoom = 10
    let mutable hwndSource = null
    // current zone combobox
    let addNewZoneButton = new Button(Content="Add new zone", Margin=Thickness(4.))
    let zoneOptions = System.Collections.ObjectModel.ObservableCollection<string>()
    let mutable selectionChangeIsDisabled = false
    let zoneComboBox = new ComboBox(ItemsSource=zoneOptions, IsReadOnly=true, IsEditable=false, SelectedIndex=0, Width=200., Margin=Thickness(4.))
    let renameZoneButton = new Button(Content="Rename zone", Margin=Thickness(4.))
    let printCurrentZoneButton = new Button(Content="Print this zone", Margin=Thickness(4.))
    // summary of current selection
    let summaryTB = new TextBox(IsReadOnly=true, FontSize=20., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue, // SolidColorBrush(Color.FromRgb(0x84uy,0xB5uy,0xFDuy)), 
                                    FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, TextWrapping=TextWrapping.Wrap, SelectionBrush=Brushes.Orange,
                                    HorizontalAlignment=HorizontalAlignment.Stretch,
                                    Height=200., VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(4.))
    let floatSummaryTB = new TextBox(IsReadOnly=true, FontSize=18., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=SolidColorBrush(Color.FromRgb(0x84uy,0xB5uy,0xFDuy)), 
                                    FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, TextWrapping=TextWrapping.Wrap, SelectionBrush=Brushes.Orange,
                                    HorizontalAlignment=HorizontalAlignment.Stretch,
                                    Height=200., VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(4.))
    // clipboard display
    let clipTB = new TextBox(IsReadOnly=true, FontSize=12., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.White, Margin=Thickness(2.))
    let clipView = new Border(Width=float(VIEWX/5), Height=float(VIEWX/6), BorderThickness=Thickness(2.), BorderBrush=Brushes.Orange, Margin=Thickness(2.))
    let clipDP = 
        let r = new DockPanel(LastChildFill=true)
        r.Children.Add(clipTB) |> ignore
        DockPanel.SetDock(clipTB, Dock.Top)
        r.Children.Add(clipView) |> ignore
        r
    // meta and full summary of current tile
    let metadataKeys = new System.Collections.ObjectModel.ObservableCollection<string>()
    let refreshMetadataKeys() =   // TODO can clean this up
        let newKeys = metadataStore.AllKeys() |> Array.sort
        let oldKeys = if metadataKeys.Count=0 then [||] else metadataKeys |> Seq.toArray
        if oldKeys <> newKeys then
            MapIcons.redrawPanelEv.Trigger()
            metadataKeys.Clear()
            for s in newKeys do
                metadataKeys.Add(s)
    let metaAndScreenshotPanel = new DockPanel(Margin=Thickness(4.), LastChildFill=true)
    let mutable doZoom = fun _ -> ()
    let mfsRefresh() =
        let zm = ZoneMemory.Get(theGame.CurZone)
        metaAndScreenshotPanel.Children.Clear()
        let child : UIElement = 
            if zm.FullImgArray.[theGame.CurX,theGame.CurY] <> null then
                let top = Utils.ImageProjection(zm.FullImgArray.[theGame.CurX,theGame.CurY],MetaArea)
                metaAndScreenshotPanel.Children.Add(top) |> ignore
                DockPanel.SetDock(top, Dock.Top)
                let largeImage = Utils.ImageProjection(zm.FullImgArray.[theGame.CurX,theGame.CurY],(0,0,GAMENATIVEW,GAMENATIVEH))
                upcast largeImage
            else
                let w = float(APP_WIDTH - KEYS_LIST_BOX_WIDTH - 2*4)
                let h = w / 16. * 9.
                upcast new DockPanel(Background=Brushes.DarkSlateBlue, Width=w, Height=h)
        metaAndScreenshotPanel.Children.Add(child) |> ignore
        child.MouseDown.Add(fun _ ->
            let cmt = zm.MapTiles.[theGame.CurX, theGame.CurY]
            if cmt.NumScreenshots() > 0 then
                DoScreenshotDisplayWindow(theGame.CurX, theGame.CurY, this, zm)
                doZoom(theGame.CurX, theGame.CurY, curZoom)
            )
    // zoom/refresh
    let mutable curProjection = 1  // 0=full, 1=map, 2=meta
    //let tbLight, tbDark = Brushes.LightGray, Brushes.DarkGray
    let tbLight, tbDark = new SolidColorBrush(Color.FromRgb(0xE8uy,0xD3uy,0xD3uy)), new SolidColorBrush(Color.FromRgb(0xC0uy,0xA9uy,0xA9uy))
    let zoomTextboxes = Array2D.init MAX MAX (fun i j ->
        new TextBox(IsReadOnly=true, IsHitTestVisible=false, FontSize=12., Text=sprintf"%02d,%02d"i j, BorderThickness=Thickness(1.), Foreground=Brushes.Black,
                    HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
        )
    let mutable mapIconHoverRedraw = fun _ -> ()
    let allZeroes : byte[] = Array.zeroCreate (GameSpecific.GAMESCREENW * GameSpecific.GAMESCREENH * 4)
    let rec zoom(ci, cj, level) = // level = 1->1x1, 2->3x3, 3->5x5, etc    
        //printfn "called zoom(%d,%d,%d)" ci cj level
        let zm = ZoneMemory.Get(theGame.CurZone)
        let aspect,kludge,ia,pw,ph = 
            match curProjection with
            | 0 -> GAMEASPECT, 0, zm.FullImgArray, GAMENATIVEW, GAMENATIVEH
            | 1 -> let _,_,w,h = MapArea in float w / float h, 0, zm.MapImgArray, w, h
            | 2 -> let _,_,w,h = MetaArea in float w / float h, 9, zm.MetaImgArray, w, h
            | _ -> failwith "bad curProjection"
        let VIEWY = System.Math.Floor((float(VIEWX)/aspect) + 0.83) |> int
        let DX,DY = float(MAPX - VIEWX)/2., float(MAPY - VIEWY)/2.
        let scale = float(2*(level-1)+1)
        mapCanvas.Children.Clear()
        mapMarkersImage.Source <- null
        mapMarkersHoverImage.Source <- null
        let W,H = float(VIEWX)/scale,float(VIEWY)/scale
        let drawnLocations = ResizeArray()
        for i = ci-level to ci+level do
            for j = cj-level-kludge to cj+level+kludge do
                if i>=0 && i<MAX && j>=0 && j<MAX then
                    drawnLocations.Add(i,j)
                    let xoff,yoff = DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H
                    if zm.FullImgArray.[i,j] <> null then
                        let W,H = int(W),(max 1 (int H))
                        let bytes = ia.GetRaw(i,j,W,H)
                        let stride = W*4
                        Utils.CopyBGRARegion(backBuffer, backBufferStride, MAPX+int(xoff), MAPY+int(yoff), bytes, stride, 0, 0, W, H)
                    else
                        do
                            let W,H = int(W),int(H)
                            let stride = W*4
                            Utils.CopyBGRARegion(backBuffer, backBufferStride, MAPX+int(xoff), MAPY+int(yoff), allZeroes, stride, 0, 0, W, H)
                        let tb = zoomTextboxes.[i,j]
                        Utils.deparent(tb)
                        tb.Background <- (if zm.MapTiles.[i,j].IsEmpty then (if (i+j)%2 = 0 then tbLight else tbDark) else Brushes.CornflowerBlue)
                        tb.Width <- W
                        tb.Height<- H
                        Utils.canvasAdd(mapCanvas, tb, DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
                else
                    Utils.canvasAdd(mapCanvas, new DockPanel(Background=Brushes.LightGray, Width=W, Height=H), DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
        let bitmapSource = System.Windows.Media.Imaging.BitmapSource.Create(3*MAPX, 3*MAPY, 96., 96., PixelFormats.Bgra32, null, backBuffer, backBufferStride)
        writeableBitmapImage.Source <- bitmapSource
        let cursor = new Shapes.Rectangle(Stroke=Brushes.Yellow, StrokeThickness=RT, Width=W + RT*2., Height=H + RT*2.)
        Utils.canvasAdd(mapCanvas, cursor, DX-W+float(level)*W-RT, DY-H+float(level)*H-RT)
        let cmt = zm.MapTiles.[ci,cj]
        summaryTB.Text <- sprintf "(%02d,%02d)        %d screenshots\n%s" ci cj (cmt.NumScreenshots()) cmt.Note
        mfsRefresh()
        do
            mouseCursor.Width <- W + RT
            mouseCursor.Height <- H + RT
            mapCanvas.Children.Add(mouseCursor) |> ignore
            do
                // map icons
                MapIcons.redrawMapIconsEv.Publish.Add(fun _ ->
                    let backBuffer, backBufferStride = Array.zeroCreate (3*MAPX*3*MAPY*4), 3*MAPX*4   // 3x so I can write 'out of bounds' and clip it later
                    let draw(i,j,key) =
                        let xoff,yoff = DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H
                        let W,H = int(W),int(H)
                        let bytes = MapIcons.mapMarkerCaches.[key].Get(W,H)
                        let stride = W*4
                        Utils.CopyBGRARegionOnlyPartsWithAlpha(backBuffer, backBufferStride, MAPX+int(xoff), MAPY+int(yoff), bytes, stride, 0, 0, W, H)
                    if not(MapIcons.allIconsDisabledCheckbox.IsChecked.Value) then
                        do
                            // TODO this probably doesn't refresh with text updates to tiles, would need to un-click&re-click the icon
                            if not(System.String.IsNullOrWhiteSpace(MapIcons.userRegex)) && MapIcons.keyDrawFuncs.[MapIcons.REGEX_DUMMY].IsSome then
                                let re = new System.Text.RegularExpressions.Regex(MapIcons.userRegex)
                                for i,j in drawnLocations do
                                    let note = zm.MapTiles.[i,j].Note
                                    if note <> null && re.IsMatch(note) then
                                        draw(i,j,MapIcons.REGEX_DUMMY)
                        let keys = InMemoryStore.metadataStore.AllKeys() |> Array.sort
                        for k in keys do
                            let locs = metadataStore.LocationsForKey(k)
                            for i,j in drawnLocations do
                                let loc = GenericMetadata.Location(theGame.CurZone,i,j)
                                if locs.Contains(loc) then
                                    match MapIcons.keyDrawFuncs.[k] with
                                    | Some _ -> draw(i,j,k)
                                    | _ -> ()
                    let bitmapSource = System.Windows.Media.Imaging.BitmapSource.Create(3*MAPX, 3*MAPY, 96., 96., PixelFormats.Bgra32, null, backBuffer, backBufferStride)
                    mapMarkersImage.Source <- bitmapSource
                    )
                MapIcons.redrawMapIconHoverOnly.Publish.Add(fun _ ->
                    let backBuffer, backBufferStride = Array.zeroCreate (3*MAPX*3*MAPY*4), 3*MAPX*4   // 3x so I can write 'out of bounds' and clip it later
                    if MapIcons.currentlyHoveredHashtagKey<>null then   // even when disabled is checked, hovering should highlight
                        // TODO consider hover for userRegex
                        for i = ci-level to ci+level do
                            for j = cj-level-kludge to cj+level+kludge do
                                if i>=0 && i<MAX && j>=0 && j<MAX then
                                    let loc = GenericMetadata.Location(theGame.CurZone,i,j)
                                    if metadataStore.LocationsForKey(MapIcons.currentlyHoveredHashtagKey).Contains(loc) then
                                        let xoff,yoff = DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H
                                        let W,H = int(W),int(H)
                                        let bytes = MapIcons.mapMarkerCaches.[MapIcons.HOVER_DUMMY].Get(W,H)
                                        let stride = W*4
                                        Utils.CopyBGRARegion(backBuffer, backBufferStride, MAPX+int(xoff), MAPY+int(yoff), bytes, stride, 0, 0, W, H)
                    let bitmapSource = System.Windows.Media.Imaging.BitmapSource.Create(3*MAPX, 3*MAPY, 96., 96., PixelFormats.Bgra32, null, backBuffer, backBufferStride)
                    mapMarkersHoverImage.Source <- bitmapSource
                    )
            mapCanvasMouseMoveFunc <- (fun (x,y) ->
                // compute which index we are over
                let i = (ci - level) + int((x - DX + W)/W)
                let j = (cj - level) + int((y - DY + H)/H)
                //printfn "mousemove (%6.2f,%6.2f,%2d,%2d)" x y i j
                // draw mouse cursor
                Canvas.SetLeft(mouseCursor, DX-W+float(i-ci+level)*W-RT/2.)
                Canvas.SetTop(mouseCursor, DY-H+float(j-cj+level)*H-RT/2.)
                mouseCursor.Stroke <- Brushes.Cyan
                // draw quick hover data
                bottomFloat.Children.Clear()
                if i>=0 && i<MAX && j>=0 && j<MAX && zm.FullImgArray.[i,j] <> null then
                    let largeImage = Utils.ImageProjection(ia.[i,j],(0,0,pw,ph))
                    let cmt = zm.MapTiles.[i,j]
                    floatSummaryTB.Text <- sprintf "(%02d,%02d)        %d screenshots\n%s" i j (cmt.NumScreenshots()) cmt.Note
                    let dp = new DockPanel(Width=float APP_WIDTH - float KEYS_LIST_BOX_WIDTH - 30., Height=BOTTOM_HEIGHT - 10., Background=Brushes.Cyan, LastChildFill=true)
                    DockPanel.SetDock(largeImage, Dock.Bottom)
                    dp.Children.Add(largeImage) |> ignore
                    Utils.deparent(floatSummaryTB)
                    dp.Children.Add(floatSummaryTB) |> ignore
                    Utils.canvasAdd(bottomFloat, new Border(Child=dp, BorderBrush=Brushes.Cyan, BorderThickness=Thickness(4.)), 0., 0.)
                )
            mapCanvasMouseDownFunc <- (fun (me,x,y) ->
                // compute which index we are over
                let i = (ci - level) + int((x - DX + W)/W)
                let j = (cj - level) + int((y - DY + H)/H)
                if i>=0 && i<MAX && j>=0 && j<MAX then
                    theGame.CurX <- i
                    theGame.CurY <- j
                    UpdateGameFile()
                    zoom(theGame.CurX,theGame.CurY, curZoom)
                    if me.RightButton = Input.MouseButtonState.Pressed then
                        Utils.DoModalDialog(this, zm.FullImgArray.GetCopyOfBmp(i,j) |> Utils.BMPtoImage, sprintf "Fullsize(%2d,%2d)" i j, (new Event<unit>()).Publish)
                )
        MapIcons.redrawMapIconsEv.Trigger()
        MapIcons.redrawMapIconHoverOnly.Trigger()
    and UpdateGameFile() =
        let gameFile = System.IO.Path.Combine(GetRootFolder(), "game.json")
        let json = System.Text.Json.JsonSerializer.Serialize<Game>(theGame)
        WriteAllText(gameFile, json)
    let UpdateCurrentNote(origNote, newNote, zm:ZoneMemory) =
        zm.MapTiles.[theGame.CurX,theGame.CurY].Note <- newNote
        SerializeMapTile(theGame.CurX,theGame.CurY,zm)
        metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,theGame.CurX,theGame.CurY), origNote, newNote)
        refreshMetadataKeys()
        zoom(theGame.CurX,theGame.CurY, curZoom)   // redraw note preview in summary area
    do
        doZoom <- zoom
        mapCanvas.MouseMove.Add(fun me -> let p = me.GetPosition(mapCanvas) in mapCanvasMouseMoveFunc(p.X, p.Y))
        mapCanvas.MouseLeave.Add(fun _ -> mouseCursor.Stroke <- Brushes.Transparent; bottomFloat.Children.Clear())
        mapCanvas.MouseDown.Add(fun me -> let p = me.GetPosition(mapCanvas) in mapCanvasMouseDownFunc(me, p.X, p.Y))
        // init zones and ensure directories
        LoadRootGameData()
        do
            let zoneFolder = GetZoneFolder(0)
            System.IO.Directory.CreateDirectory(zoneFolder) |> ignore
        System.IO.Directory.CreateDirectory(System.IO.Path.Combine(GetRootFolder(),SCREENSHOTS_FOLDER)) |> ignore
        let savedZone = theGame.CurZone
        for i = 0 to theGame.ZoneNames.Length-1 do
            // populate zone names for combobox
            zoneOptions.Add(theGame.ZoneNames.[i])
            // populate key metdata from all notes
            theGame.CurZone <- i
            let zm = ZoneMemory.Get(i)
            LoadZoneMapTiles(zm)
        theGame.CurZone <- savedZone
        // populate images for initial map
        let mutable zm = ZoneMemory.Get(theGame.CurZone)
        zoneComboBox.ItemsSource <- zoneOptions
        zoneComboBox.SelectedIndex <- savedZone
        // zone changes
        zoneComboBox.SelectionChanged.Add(fun _ ->
            if not selectionChangeIsDisabled then
                theGame.CurZone <- zoneComboBox.SelectedIndex
                UpdateGameFile()
                zm <- ZoneMemory.Get(theGame.CurZone)
                zoom(theGame.CurX, theGame.CurY, curZoom)
            )
        addNewZoneButton.Click.Add(fun _ ->
            let n = theGame.ZoneNames.Length
            theGame.ZoneNames <- AAppend(theGame.ZoneNames, GetZoneName(n))
            zoneOptions.Add(theGame.ZoneNames.[n])
            UpdateGameFile()
            selectionChangeIsDisabled <- true
            zoneComboBox.SelectedIndex <- n
            selectionChangeIsDisabled <- false
            theGame.CurZone <- zoneComboBox.SelectedIndex
            zm <- ZoneMemory.Get(theGame.CurZone)
            zoom(theGame.CurX, theGame.CurY, curZoom)
            )
        renameZoneButton.Click.Add(fun _ ->
            let orig = GetZoneName(theGame.CurZone)
            let tb = new TextBox(IsReadOnly=false, FontSize=12., Text=(if orig=null then "" else orig), BorderThickness=Thickness(1.), 
                                    Foreground=Brushes.Black, Background=Brushes.White,
                                    Width=float(VIEWX/2), Height=20., TextWrapping=TextWrapping.NoWrap, AcceptsReturn=false, 
                                    Margin=Thickness(5.))
            let closeEv = new Event<unit>()
            let mutable save = false
            let cb = new Button(Content=" Cancel ", Margin=Thickness(4.))
            let sb = new Button(Content=" Save ", Margin=Thickness(4.))
            cb.Click.Add(fun _ -> closeEv.Trigger())
            sb.Click.Add(fun _ -> save <- true; closeEv.Trigger())
            let dp = new DockPanel(LastChildFill=true)
            dp.Children.Add(cb) |> ignore
            dp.Children.Add(sb) |> ignore
            dp.Children.Add(new DockPanel()) |> ignore
            DockPanel.SetDock(cb, Dock.Left)
            DockPanel.SetDock(sb, Dock.Right)
            let sp = new StackPanel(Orientation=Orientation.Vertical)
            sp.Children.Add(tb) |> ignore
            sp.Children.Add(dp) |> ignore
            tb.Loaded.Add(fun _ ->
                tb.Select(tb.Text.Length, 0)   // position the cursor at the end
                System.Windows.Input.Keyboard.Focus(tb) |> ignore
                )
            Utils.DoModalDialog(this, sp, "Edit zone name", closeEv.Publish)
            if save then
                theGame.ZoneNames.[theGame.CurZone] <- tb.Text
                UpdateGameFile()
                selectionChangeIsDisabled <- true
                zoneOptions.[theGame.CurZone] <- theGame.ZoneNames.[theGame.CurZone]
                zoneComboBox.SelectedIndex <- theGame.CurZone 
                selectionChangeIsDisabled <- false
            )
        printCurrentZoneButton.Click.Add(fun _ ->
            // TODO
#if OLD
            // load full screenshots from disk
            let bmps = Array2D.zeroCreate 100 100
            let mutable minx,miny,maxx,maxy = 100,100,0,0
            for i = 0 to MAX-1 do
                for j = 0 to MAX-1 do
                    let file = MapTileFilename(i,j)
                    if System.IO.File.Exists(file) then
                        let json = System.IO.File.ReadAllText(file)
                        let data = System.Text.Json.JsonSerializer.Deserialize<MapTile>(json)
                        if data.Screenshots <> null && data.Screenshots.Length > 0 then
                            // TODO representatives?
                            let ts = data.Screenshots.[data.Screenshots.Length-1]
                            let ssFile = ScreenshotFilenameFromTimestampId(ts)
                            let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                            bmps.[i,j] <- bmp
                            minx <- min minx i
                            miny <- min miny j
                            maxx <- max maxx i
                            maxy <- max maxy j
#else
            let bmps = Array2D.zeroCreate 100 100
            let mutable minx,miny,maxx,maxy = 100,100,0,0
            for i = 0 to MAX-1 do
                for j = 0 to MAX-1 do
                    let bmp = zm.FullImgArray.GetCopyOfBmp(i,j)
                    if bmp <> null then
                        bmps.[i,j] <- bmp
                        minx <- min minx i
                        miny <- min miny j
                        maxx <- max maxx i
                        maxy <- max maxy j
#endif
            if maxx >= minx then // there was at least one screenshot
                //for ma,fn in [MetaArea,"printed_meta.png";    MapArea,"printed_map.png"] do
                for ma,fn in [MapArea,"printed_map.png"] do
                    let mx,my,mw,mh = ma
                    let r = new System.Drawing.Bitmap(mw*(maxx-minx+1), mh*(maxy-miny+1))
                    let rData = r.LockBits(System.Drawing.Rectangle(0,0,r.Width,r.Height), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                    //System.Diagnostics.Debugger.Break()
                    for i = minx to maxx do
                        printfn "[%d..%d] - %d" minx maxx i
                        for j = miny to maxy do
                            let bmp = bmps.[i,j]
                            if bmp <> null then
                                let data = bmp.LockBits(System.Drawing.Rectangle(0,0,bmp.Width,bmp.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                                for x = 0 to mw-1 do
                                    for y = 0 to mh-1 do
                                        //let color = bmp.GetPixel(mx+x,my+y)
                                        //let color = Utils.GetColorFromLockedFormat32BppArgb(mx+x,my+y,data)
                                        //r.SetPixel(mw*(i-minx) + x, mh*(j-miny) + y, color)
                                        //Utils.SetColorFromLockedFormat32BppArgb(mw*(i-minx) + x, mh*(j-miny) + y,rData,color)
                                        if x=mw-1 || y=mh-1 then
                                            Utils.SetColorFromLockedFormat32BppArgb(mw*(i-minx) + x, mh*(j-miny) + y,rData, System.Drawing.Color.Gray)
                                        else
                                            Utils.SetAndGetColorFromLockedFormat32BppArgb(mw*(i-minx) + x, mh*(j-miny) + y, rData, mx+x, my+y, data)
                                bmp.UnlockBits(data)
                    r.UnlockBits(rData)
                    r.Save(fn, System.Drawing.Imaging.ImageFormat.Png)
            else
                printfn "no screenshots to print"
            )
        updateClipboardView <- (fun () ->
            if not(System.String.IsNullOrEmpty(clipboardSSID)) then
                let img = bmpDict.[clipboardSSID] |> Utils.BMPtoImage
                //printfn "CV: %f, %f" clipView.Width clipView.ActualWidth
                clipView.Child <- img
                //img.Stretch <- Stretch.Uniform
                //img.StretchDirection <- StretchDirection.Both
                // code below seems stupid but it works, as opposed to code above
                img.Width <- clipView.ActualWidth
                img.Height <- clipView.ActualHeight
                clipTB.Text <- clipboardSSID
            )
        // window
        this.Title <- "Generic Screenshot Mapper"
        this.Left <- 1290.
        this.Top <- 4.
        //this.Topmost <- true
        this.SizeToContent <- SizeToContent.WidthAndHeight
        //this.SizeToContent <- SizeToContent.Manual
        //this.Width <- float APP_WIDTH
        //this.Height <- float APP_HEIGHT
        // layout
        let all = new StackPanel(Orientation=Orientation.Vertical)
        let mapPortion = new StackPanel(Orientation=Orientation.Vertical, Width=float APP_WIDTH)
        let topBar =
            let sp = new StackPanel(Orientation=Orientation.Horizontal)
            sp.Children.Add(addNewZoneButton) |> ignore
            sp.Children.Add(zoneComboBox) |> ignore
            sp.Children.Add(renameZoneButton) |> ignore
            sp.Children.Add(printCurrentZoneButton) |> ignore
            let toggleLayoutButton = new Button(Content="Toggle layout", Margin=Thickness(4.))
            toggleLayoutButton.Click.Add(fun _ ->
                if all.Orientation = Orientation.Vertical then
                    all.Orientation <- Orientation.Horizontal
                    this.Left <- this.Left - float APP_WIDTH
                else
                    all.Orientation <- Orientation.Vertical
                    this.Left <- this.Left + float APP_WIDTH
                )
            sp.Children.Add(toggleLayoutButton) |> ignore
            let featureButton = new Button(Content="Feature", Margin=Thickness(4.))
            featureButton.Click.Add(fun _ -> FeatureWindow.MakeFeatureMap(this.Owner, zm))
            sp.Children.Add(featureButton) |> ignore
            sp
        mapPortion.Children.Add(topBar) |> ignore
        mapPortion.Children.Add(wholeMapCanvas) |> ignore
        all.Children.Add(mapPortion) |> ignore
        let bottom =
            let dp = new DockPanel(LastChildFill=true, Width=float APP_WIDTH, Height=BOTTOM_HEIGHT)
            refreshMetadataKeys()
            let rightColumn =
                let rc = new DockPanel(LastChildFill=true)
                rc.Children.Add(clipDP) |> ignore
                DockPanel.SetDock(clipDP, Dock.Bottom)
                let mutable iconKeys = MapIcons.MakeIconUI(this)
                rc.Children.Add(iconKeys) |> ignore
                MapIcons.redrawPanelEv.Publish.Add(fun _ ->
                    rc.Children.Remove(iconKeys)
                    iconKeys <- MapIcons.MakeIconUI(this)
                    rc.Children.Add(iconKeys) |> ignore
                    )
                rc
            let leftColumn = 
                let lc = new DockPanel(LastChildFill=true, Background=Brushes.Yellow)
                DockPanel.SetDock(metaAndScreenshotPanel, Dock.Bottom)
                lc.Children.Add(metaAndScreenshotPanel) |> ignore
                lc.Children.Add(summaryTB) |> ignore
                lc
            dp.Children.Add(rightColumn) |> ignore
            DockPanel.SetDock(rightColumn, Dock.Right)
            dp.Children.Add(leftColumn) |> ignore
            let r = new Canvas(Width=float APP_WIDTH, Height=BOTTOM_HEIGHT)
            r.Children.Add(dp) |> ignore
            r.Children.Add(bottomFloat) |> ignore
            r
        all.Children.Add(bottom) |> ignore
        all.UseLayoutRounding <- true
        this.Content <- all
        this.Loaded.Add(fun _ ->
            let handle = Elephantasy.Winterop.GetConsoleWindow()
            Elephantasy.Winterop.ShowWindow(handle, Elephantasy.Winterop.SW_MINIMIZE) |> ignore
            zoom(theGame.CurX,theGame.CurY,curZoom)
            )
    override this.OnSourceInitialized(e) =
        base.OnSourceInitialized(e)
        let helper = new System.Windows.Interop.WindowInteropHelper(this)
        hwndSource <- System.Windows.Interop.HwndSource.FromHwnd(helper.Handle)
        hwndSource.AddHook(System.Windows.Interop.HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        this.RegisterHotKey()
    override this.OnClosed(e) =
        if hwndSource <> null then
            hwndSource.RemoveHook(System.Windows.Interop.HwndSourceHook(fun a b c d e -> this.HwndHook(a,b,c,d,&e)))
        hwndSource <- null
        this.UnregisterHotKey()
        base.OnClosed(e)
    member this.RegisterHotKey() =
        let helper = new System.Windows.Interop.WindowInteropHelper(this);
        for k in KEYS do
            if(not(Elephantasy.Winterop.RegisterHotKey(helper.Handle, Elephantasy.Winterop.HOTKEY_ID, MOD_NONE, uint32 k))) then
                failwithf "could not register hotkey %A" k
    member this.UnregisterHotKey() =
        let helper = new System.Windows.Interop.WindowInteropHelper(this)
        Elephantasy.Winterop.UnregisterHotKey(helper.Handle, Elephantasy.Winterop.HOTKEY_ID) |> ignore
    member this.HwndHook(_hwnd:IntPtr, msg:int, wParam:IntPtr, lParam:IntPtr, handled:byref<bool>) : IntPtr =
        if Utils.aModalDialogIsOpen then (broadcastHotKeyEv.Trigger(msg,wParam,lParam); IntPtr.Zero) else
        let WM_HOTKEY = 0x0312
        let zm = ZoneMemory.Get(theGame.CurZone)
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Elephantasy.Winterop.HOTKEY_ID then
                //let ctrl_bits = lParam.ToInt32() &&& 0xF  // see WM_HOTKEY docs
                let key = lParam.ToInt32() >>> 16
                if false then
                    for k in KEYS do
                        if key = k then
                            printfn "key %A was pressed" k
                if key = VK_SUBTRACT && zm.MapTiles.[theGame.CurX,theGame.CurY].ThereAreScreenshots() then   // assumes we want to remove last in the list; if user wants specific one, they click image and select among them
                    let id = (zm.MapTiles.[theGame.CurX,theGame.CurY].ScreenshotsWithKinds |> Array.last).Id
                    zm.MapTiles.[theGame.CurX,theGame.CurY].CutScreenshot(id)
                    clipboardSSID <- id
                    updateClipboardView()
                    // update current tile view
                    RecomputeImage(theGame.CurX,theGame.CurY,zm)
                    zoom(theGame.CurX,theGame.CurY, curZoom)
                    // update disk
                    SerializeMapTile(theGame.CurX,theGame.CurY,zm)
                if key = VK_ADD && not(System.String.IsNullOrEmpty(clipboardSSID)) then
                    zm.MapTiles.[theGame.CurX,theGame.CurY].AddScreenshot(clipboardSSID)
                    SerializeMapTile(theGame.CurX,theGame.CurY,zm)
                    RecomputeImage(theGame.CurX,theGame.CurY,zm)
                    zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_MULTIPLY then
                    curProjection <- curProjection + 1
                    if curProjection >= 3 then
                        curProjection <- 0
                    zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_NUMPAD4 then
                    if theGame.CurX > 0 then
                        theGame.CurX <- theGame.CurX - 1
                        UpdateGameFile()
                        zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_NUMPAD6 then
                    if theGame.CurX < 99 then
                        theGame.CurX <- theGame.CurX + 1
                        UpdateGameFile()
                        zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_NUMPAD8 then
                    if theGame.CurY > 0 then
                        theGame.CurY <- theGame.CurY - 1
                        UpdateGameFile()
                        zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_NUMPAD2 then
                    if theGame.CurY < 99 then
                        theGame.CurY <- theGame.CurY + 1
                        UpdateGameFile()
                        zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_NUMPAD0 then
                    let img,bmp,id = TakeNewScreenshot()
                    bmpDict.Add(id, bmp)
                    zm.MapTiles.[theGame.CurX,theGame.CurY].AddScreenshot(id)
                    SerializeMapTile(theGame.CurX,theGame.CurY,zm)
                    RecomputeImage(theGame.CurX,theGame.CurY,zm)
                    zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_NUMPAD7 then
                    if curZoom > 1 then
                        curZoom <- curZoom - 1
                        zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_NUMPAD9 then
                    if curZoom < MAX/2 then
                        curZoom <- curZoom + 1
                        zoom(theGame.CurX,theGame.CurY, curZoom)
                if key = VK_DIVIDE then
                    Utils.Win32.SetForegroundWindow(_hwnd) |> ignore
                    let orig = zm.MapTiles.[theGame.CurX,theGame.CurY].Note
                    let tb = new TextBox(IsReadOnly=false, FontSize=12., Text=(if orig=null then "" else orig), BorderThickness=Thickness(1.), 
                                            Foreground=Brushes.Black, Background=Brushes.White,
                                            Width=float(VIEWX/2), Height=float(VIEWX/2), TextWrapping=TextWrapping.Wrap, AcceptsReturn=true, 
                                            VerticalScrollBarVisibility=ScrollBarVisibility.Visible, Margin=Thickness(5.))
                    let closeEv = new Event<unit>()
                    let mutable save = false
                    let cb = new Button(Content=" Cancel ", Margin=Thickness(4.))
                    let sb = new Button(Content=" Save ", Margin=Thickness(4.))
                    cb.Click.Add(fun _ -> closeEv.Trigger())
                    sb.Click.Add(fun _ -> save <- true; closeEv.Trigger())
                    let dp = new DockPanel(LastChildFill=true)
                    dp.Children.Add(cb) |> ignore
                    dp.Children.Add(sb) |> ignore
                    dp.Children.Add(new DockPanel()) |> ignore
                    DockPanel.SetDock(cb, Dock.Left)
                    DockPanel.SetDock(sb, Dock.Right)
                    let sp = new StackPanel(Orientation=Orientation.Vertical)
                    sp.Children.Add(tb) |> ignore
                    sp.Children.Add(dp) |> ignore
                    tb.Loaded.Add(fun _ ->
                        tb.Select(tb.Text.Length, 0)   // position the cursor at the end
                        System.Windows.Input.Keyboard.Focus(tb) |> ignore
                        )
                    Utils.DoModalDialog(this, sp, "Edit note", closeEv.Publish)
                    if save then
                        UpdateCurrentNote(orig, tb.Text, zm)
                if key = VK_DECIMAL then
                    let orig = zm.MapTiles.[theGame.CurX,theGame.CurY].Note
                    let orig = if orig = null then "" else orig
                    //let special = "#TODO"
                    //let special = "#UV"
                    let special = "#NOW"
                    if orig.EndsWith(special) then
                        UpdateCurrentNote(orig, orig.Substring(0,orig.Length-special.Length), zm)
                    else
                        UpdateCurrentNote(orig, orig+"\n"+special, zm)
        IntPtr.Zero
