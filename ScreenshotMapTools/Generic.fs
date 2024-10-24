module Generic

let APP_WIDTH = 1920 - 1280 - 12        // my monitor, a game, a little buffer
let APP_HEIGHT = 980. + 16. + 20.
let BOTTOM_HEIGHT = 480.
let KEYS_LIST_BOX_WIDTH = MapIcons.KEYS_LIST_BOX_WIDTH

//////////////////////////////////////////////////////////

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Elephantasy.Winterop
open GameSpecific
open BackingStoreData
open InMemoryStore
open Utils.Extensions

let mutable clipboardSSID = ""
let mutable updateClipboardView = fun() -> ()        // TODO how remove this ugly tangle

let SerializeMapTile(x,y,zm:ZoneMemory) = 
    let json = System.Text.Json.JsonSerializer.Serialize<MapTile>(zm.MapTiles.[x,y])
    WriteAllText(MapTileFilename(x,y,zm.Zone), json)

let broadcastHotKeyEv = new Event<int*IntPtr*IntPtr>()
let mutable theCurrentLocalBHKE = fun _ -> ()
do
    broadcastHotKeyEv.Publish.Add(fun x -> theCurrentLocalBHKE x)

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
        let largeImage = Utils.ImageProjection(img,(0,0,GAMESCREENW,GAMESCREENH))
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
        let dp = (new DockPanel(LastChildFill=true)).AddRight(kindPanel).Add(border)
        sp.Children.Add(dp) |> ignore
        allBorders.Add(border)
        border.MouseDown.Add(fun ea ->
            ea.Handled <- true
            if ea.RightButton = System.Windows.Input.MouseButtonState.Pressed then
                let img = Utils.BMPtoImage(bmp)
                img.Width <- 1280.
                img.Height <- 720.
                img.Stretch <- Stretch.Uniform
                FeatureWindow.EnsureFeature(parent.Owner, img)
            else
                whichSSIDisHighlighted <- Some(ssid)
                for b in allBorders do
                    b.BorderBrush <- Brushes.Transparent
                border.BorderBrush <- Brushes.Orange
            )
    theCurrentLocalBHKE <- (fun (msg, wParam, lParam) ->
        let WM_HOTKEY = 0x0312
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Elephantasy.Winterop.HOTKEY_ID then
                let key = lParam.ToInt32() >>> 16
                if key = VK_SUBTRACT then
                    match whichSSIDisHighlighted with
                    | None -> ()
                    | Some(ssid) ->
                        clipboardSSID <- ssid
                        updateClipboardView()
                        zm.MapTiles.[x,y].CutScreenshot(ssid)
                        // update current tile view
                        RecomputeImage(x,y,zm)
                        //zoom(...) will be called when the window closes
                        // update disk
                        SerializeMapTile(x,y,zm)
                        closeEv.Trigger()
        )
    Utils.DoModalDialog(parent, all, sprintf "All Screenshots for (%d,%d)" x y, closeEv.Publish)
    theCurrentLocalBHKE <- fun _ -> ()

type MyWindow() as this = 
    inherit Window()
    let mutable currentlyRunningAHotkeyCommand = false
    let KEYS = [| VK_NUMPAD0; VK_NUMPAD1; VK_NUMPAD2; VK_NUMPAD3; VK_NUMPAD4; VK_NUMPAD5; VK_NUMPAD6; VK_NUMPAD7; VK_NUMPAD8; VK_NUMPAD9;
                    VK_MULTIPLY; VK_ADD; VK_SUBTRACT; VK_DECIMAL; VK_DIVIDE (*; VK_RETURN *) |]
    let ARROWKEYS = [| VK_NUMPAD2; VK_NUMPAD4; VK_NUMPAD6; VK_NUMPAD8 |]
    let MAPX,MAPY = APP_WIDTH,420
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
    let mouseCursor = new Shapes.Rectangle(StrokeThickness=RT/2., Stroke=Brushes.Yellow)
    let mutable mapCanvasMouseMoveFunc = fun _ -> ()
    let mutable mapCanvasMouseLeaveFunc = fun _ -> ()
    let mutable mapCanvasMouseDownFunc = fun (_:Input.MouseEventArgs,_x,_y) -> ()
    let mutable warpMouseTo = fun _ -> ()
    let mutable redrawMapIconsFunc = fun _ -> ()
    let mutable redrawMapIconsHoverOnlyFunc = fun _ -> ()
    let kbdX, kbdY = Utils.EventingInt(0), Utils.EventingInt(0)    // last keyboarded cursor location
    let curZoneChanged = new Event<unit>()
    let pictureChanged = new Utils.EventingBool(false)
    let uise = new Utils.UISettlingEvent(100, [| kbdX.Changed; kbdY.Changed; curZoneChanged.Publish; (pictureChanged.Changed |> Event.filter (fun () -> pictureChanged.Value)) |])
    let mutable hwndSource = null
    let setCursor() =          // make the current cursor (moused or keyboard) the keyboard return location
        kbdX.Value <- theGame.CurX
        kbdY.Value <- theGame.CurY
    let warp() = warpMouseTo(theGame.CurX, theGame.CurY)
    // current zone combobox
    let addNewZoneButton = new Button(Content="Add zone", Margin=Thickness(4.))
    let zoneOptions = System.Collections.ObjectModel.ObservableCollection<string>()
    let makeZoneName(z) = sprintf "%02d: %s" z (theGame.ZoneNames.[z])
    let mutable selectionChangeIsDisabled = false
    let zoneComboBox = new ComboBox(ItemsSource=zoneOptions, IsReadOnly=true, IsEditable=false, SelectedIndex=0, Width=200., Margin=Thickness(4.))
    let renameZoneButton = new Button(Content="Rename zone", Margin=Thickness(4.))
    let printCurrentZoneButton = new Button(Content="Print zone", Margin=Thickness(4.))
    // summary of current selection
    let summaryTB = MinimapWindow.MakeRichTextBox(4.)
    let mutable NavigateTo = (fun (loc:GenericMetadata.Location) -> ())
    let navigationFunc (o:obj) (ea:System.Windows.Navigation.RequestNavigateEventArgs) =
        let s = ea.Uri.AbsolutePath
        let zone = s.Substring(1,2) |> int
        let x = s.Substring(4,2) |> int
        let y = s.Substring(7,2) |> int
        NavigateTo(GenericMetadata.Location(zone,x,y))
    // clipboard display
    let clipTB = new TextBox(IsReadOnly=true, FontSize=12., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.White, Margin=Thickness(2.))
    let clipView = new Border(Width=float(MAPX/5), Height=float(MAPX/6), BorderThickness=Thickness(2.), BorderBrush=Brushes.Orange, Margin=Thickness(2.))
    let clipDP = (new DockPanel(LastChildFill=true)).AddTop(clipTB).Add(clipView)
    // meta and full summary of current tile
    let metadataKeys = new System.Collections.ObjectModel.ObservableCollection<string>()
    let refreshMetadataKeys() =   // TODO can clean this up
        let newKeys = metadataStore.AllKeys() |> Array.sort
        MapIcons.redrawPanelEv.Trigger()
        metadataKeys.Clear()
        for s in newKeys do
            metadataKeys.Add(s)
    let metaAndScreenshotPanel = new DockPanel(Margin=Thickness(4.,0.,4.,4.),LastChildFill=true, Background=Brushes.DarkSlateBlue)
    let mutable doZoom = fun () -> ()
    let mfsRefresh() =
        let zm = ZoneMemory.Get(theGame.CurZone)
        metaAndScreenshotPanel.Children.Clear()
        if zm.FullImgArray.[theGame.CurX,theGame.CurY] <> null then
            match MetaArea with
            | _,_,_,1 -> ()  // meta height of 1 means there is none, skip it
            | _ ->
                let top = Utils.ImageProjection(zm.FullImgArray.[theGame.CurX,theGame.CurY],MetaArea)
                metaAndScreenshotPanel.AddTop(top) |> ignore
            let main = Utils.ImageProjection(zm.FullImgArray.[theGame.CurX,theGame.CurY],(0,0,GAMESCREENW,GAMESCREENH))
            metaAndScreenshotPanel.Children.Add(main) |> ignore
        metaAndScreenshotPanel.MouseDown.Add(fun ea ->
            ea.Handled <- true
            let cmt = zm.MapTiles.[theGame.CurX, theGame.CurY]
            if cmt.NumScreenshots() > 0 then
                DoScreenshotDisplayWindow(theGame.CurX, theGame.CurY, this, zm)
                doZoom()
            )
        let cmt = zm.MapTiles.[theGame.CurX,theGame.CurY]
        MinimapWindow.UpdateRichTextBox(summaryTB, theGame.CurX, theGame.CurY, theGame.CurZone, cmt)
    //let tbLight, tbDark = Brushes.LightGray, Brushes.DarkGray
    let tbLight, tbDark = new SolidColorBrush(Color.FromRgb(0xE8uy,0xD3uy,0xD3uy)), new SolidColorBrush(Color.FromRgb(0xC0uy,0xA9uy,0xA9uy))
    let zoomTextboxes = Array2D.init MAX MAX (fun i j ->
        new TextBlock(IsHitTestVisible=false, FontSize=12., Text=sprintf"%02d,%02d"i j, Foreground=Brushes.Black)  // TextBlock is much lighter weight (perf), but lacks alignment centering
        )
    let mutable mapIconHoverRedraw = fun _ -> ()
    let allZeroes : byte[] = Array.zeroCreate (GameSpecific.GAMESCREENW * GameSpecific.GAMESCREENH * 4)
    let mutable priorCenterX, priorCenterY, priorZone, priorLevel = -999,-999,-999,-999
    let rec zoom() = 
        let level = theGame.CurZoom // level = 1->1x1, 2->3x3, 3->5x5, etc    
        let zm = ZoneMemory.Get(theGame.CurZone)
        let aspect,kludge,ia,pw,ph = 
            match theGame.CurProjection with
            | 0 -> GAMEASPECT, 0, zm.FullImgArray, GAMESCREENW, GAMESCREENH
            | 1 -> let _,_,w,h = MapArea in float w / float h, 0, zm.MapImgArray, w, h
            | 2 -> let _,_,w,h = MetaArea in float w / float h, 9, zm.MetaImgArray, w, h
            | _ -> failwith "bad curProjection"
        // ensure cursor is fully on-screen
        while theGame.CurX <= theGame.CenterX - level do
            theGame.CenterX <- theGame.CenterX - 1
        while theGame.CurX >= theGame.CenterX + level do
            theGame.CenterX <- theGame.CenterX + 1
        while theGame.CurY <= theGame.CenterY - level - kludge do
            theGame.CenterY <- theGame.CenterY - 1
        while theGame.CurY >= theGame.CenterY + level + kludge do
            theGame.CenterY <- theGame.CenterY + 1
        // see if we need to redraw anything
        if theGame.CenterX <> priorCenterX || theGame.CenterY <> priorCenterY || theGame.CurZone <> priorZone || level <> priorLevel || pictureChanged.Value then   
            priorCenterX <- theGame.CenterX
            priorCenterY <- theGame.CenterY
            priorZone <- theGame.CurZone
            priorLevel <- level
            pictureChanged.Value <- false
            let VIEWX,VIEWY = 
                let mapAspect = float MAPX / float MAPY
                if aspect > mapAspect then
                    MAPX, System.Math.Floor((float(MAPX)/aspect) + 0.83) |> int
                else
                    System.Math.Floor((float(MAPY)*aspect) + 0.83) |> int, MAPY
            let DX,DY = float(MAPX - VIEWX)/2., float(MAPY - VIEWY)/2.
            let scale = float(2*(level-1)+1)
            mapCanvas.Children.Clear()
            mapMarkersImage.Source <- null
            mapMarkersHoverImage.Source <- null
            for i = 0 to backBuffer.Length-1 do
                backBuffer.[i] <- 0uy
            let W,H = float(VIEWX)/scale,float(VIEWY)/scale
            let drawnLocations = ResizeArray()
            let ci, cj = theGame.CenterX, theGame.CenterY
            for i = ci-level to ci+level do
                for j = cj-level-kludge to cj+level+kludge do
                    if i>=0 && i<MAX && j>=0 && j<MAX then
                        drawnLocations.Add(i,j)
                        let xoff,yoff = DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H
                        let IW,IH = int(W),(max 1 (int H))
                        let stride = IW*4
                        if zm.FullImgArray.[i,j] <> null then
                            let bytes = ia.GetRaw(i,j,IW,IH)
                            Utils.CopyBGRARegion(backBuffer, backBufferStride, MAPX+int(xoff), MAPY+int(yoff), bytes, stride, 0, 0, IW, IH)
                        else
                            Utils.CopyBGRARegion(backBuffer, backBufferStride, MAPX+int(xoff), MAPY+int(yoff), allZeroes, stride, 0, 0, IW, IH)
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
            mfsRefresh()
            do
                mouseCursor.Width <- W + RT
                mouseCursor.Height <- H + RT
                Canvas.SetLeft(mouseCursor, DX-W+float(theGame.CurX-ci+level)*W-RT/2.)
                Canvas.SetTop(mouseCursor, DY-H+float(theGame.CurY-cj+level)*H-RT/2.)
                mapCanvas.Children.Add(mouseCursor) |> ignore
                do
                    // map icons
                    redrawMapIconsFunc <- (fun _ ->
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
                    redrawMapIconsHoverOnlyFunc <- (fun _ ->
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
                mapCanvasMouseLeaveFunc <- (fun _ ->
                    theGame.CurX <- kbdX.Value
                    theGame.CurY <- kbdY.Value
                    // draw mouse cursor
                    Canvas.SetLeft(mouseCursor, DX-W+float(theGame.CurX-ci+level)*W-RT/2.)
                    Canvas.SetTop(mouseCursor, DY-H+float(theGame.CurY-cj+level)*H-RT/2.)
                    // update bottom panel
                    mfsRefresh()
                    )
                mapCanvasMouseMoveFunc <- (fun (x,y) ->
                    // compute which index we are over
                    let i = (ci - level) + int((x - DX + W)/W)
                    let j = (cj - level) + int((y - DY + H)/H)
                    if i>=0 && i<MAX && j>=0 && j<MAX then
                        theGame.CurX <- i
                        theGame.CurY <- j
                        // draw mouse cursor
                        Canvas.SetLeft(mouseCursor, DX-W+float(theGame.CurX-ci+level)*W-RT/2.)
                        Canvas.SetTop(mouseCursor, DY-H+float(theGame.CurY-cj+level)*H-RT/2.)
                        // update bottom panel
                        mfsRefresh()
                    else    // e.g. they are mousing on the canvas where -1,50 would be, center is like 0,50, left half of screen is blank and mouse into blank, behave like a Leave()
                        mapCanvasMouseLeaveFunc()
                    )
                mapCanvasMouseDownFunc <- (fun (me,x,y) ->
                    // compute which index we are over
                    let i = (ci - level) + int((x - DX + W)/W)
                    let j = (cj - level) + int((y - DY + H)/H)
                    if i>=0 && i<MAX && j>=0 && j<MAX then
                        theGame.CurX <- i
                        theGame.CurY <- j
                        setCursor()
                        UpdateGameFile()
                        zoom()
                        if me.RightButton = Input.MouseButtonState.Pressed then
                            Utils.DoModalDialog(this, zm.FullImgArray.GetCopyOfBmp(i,j) |> Utils.BMPtoImage, sprintf "Fullsize(%2d,%2d)" i j, (new Event<unit>()).Publish)
                    )
                warpMouseTo <- (fun (i,j) ->
                    let pos = mapCanvas.TranslatePoint(Point(DX+float(i-ci+level)*W-W/2.,DY+float(j-cj+level)*H-H/2.),this)  // center of i,j   // TODO might be offscreen
                    Utils.SilentlyWarpMouseCursorTo(pos)
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
        mfsRefresh()   // redraw note preview in summary area
        MapIcons.redrawMapIconsEv.Trigger()
        MapIcons.redrawMapIconHoverOnly.Trigger()
    do
        doZoom <- zoom
        mapCanvas.MouseMove.Add(fun me -> let p = me.GetPosition(mapCanvas) in mapCanvasMouseMoveFunc(p.X, p.Y))
        mapCanvas.MouseLeave.Add(fun _ -> mapCanvasMouseLeaveFunc())
        mapCanvas.MouseDown.Add(fun me -> let p = me.GetPosition(mapCanvas) in (me.Handled <- true; mapCanvasMouseDownFunc(me, p.X, p.Y)))
        MapIcons.redrawMapIconsEv.Publish.Add(fun () -> redrawMapIconsFunc())
        MapIcons.redrawMapIconHoverOnly.Publish.Add(fun () -> redrawMapIconsHoverOnlyFunc())
        summaryTB.AddHandler(System.Windows.Documents.Hyperlink.RequestNavigateEvent,new System.Windows.Navigation.RequestNavigateEventHandler(navigationFunc))
        // init zones and ensure directories
        LoadRootGameData()
        do
            let zoneFolder = GetZoneFolder(0)
            System.IO.Directory.CreateDirectory(zoneFolder) |> ignore
        System.IO.Directory.CreateDirectory(System.IO.Path.Combine(GetRootFolder(),SCREENSHOTS_FOLDER)) |> ignore
        let savedZone = theGame.CurZone
        let mutable totalBmpCount, totalMapTileCount = 0, 0
        for i = 0 to theGame.ZoneNames.Length-1 do
            let name = makeZoneName i
            printf "Loading screenshots for zone%-35s..." name
            // populate zone names for combobox
            zoneOptions.Add(name)
            // populate key metdata from all notes
            theGame.CurZone <- i
            let zm = ZoneMemory.Get(i)
            let bmpCount,maptileCount = LoadZoneMapTiles(zm)
            printfn " %3d screenshots loaded for %3d map tiles!" bmpCount maptileCount
            totalBmpCount <- totalBmpCount + bmpCount
            totalMapTileCount <- totalMapTileCount + maptileCount
        printfn "...done! %d total screenshots, %d total map tiles" totalBmpCount totalMapTileCount
        theGame.CurZone <- savedZone
        setCursor()
        // populate images for initial map
        let mutable zm = ZoneMemory.Get(theGame.CurZone)
        zoneComboBox.ItemsSource <- zoneOptions
        zoneComboBox.SelectedIndex <- savedZone
        // zone changes
        NavigateTo <- (fun loc -> 
            if loc.Zone >= 0 && loc.Zone < theGame.ZoneNames.Length && loc.X >= 0 && loc.X < MAX && loc.Y >= 0 && loc.Y < MAX then
                selectionChangeIsDisabled <- true
                zoneComboBox.SelectedIndex <- loc.Zone
                selectionChangeIsDisabled <- false
                theGame.CurZone <- zoneComboBox.SelectedIndex
                curZoneChanged.Trigger()
                zm <- ZoneMemory.Get(theGame.CurZone)
                refreshMetadataKeys()   // to update counts 
                theGame.CurX <- loc.X
                theGame.CurY <- loc.Y
                kbdX.Value <- loc.X
                kbdY.Value <- loc.Y
                UpdateGameFile()
                zoom()
                warp()
            )
        zoneComboBox.Focusable <- false                // prevent accidents
        zoneComboBox.SelectionChanged.Add(fun _ ->
            if not selectionChangeIsDisabled then
                theGame.CurZone <- zoneComboBox.SelectedIndex
                curZoneChanged.Trigger()
                UpdateGameFile()
                zm <- ZoneMemory.Get(theGame.CurZone)
                refreshMetadataKeys()   // to update counts 
                zoom()
            )
        addNewZoneButton.Focusable <- false            // prevent accidents
        addNewZoneButton.Click.Add(fun _ ->
            let n = theGame.ZoneNames.Length
            theGame.ZoneNames <- AAppend(theGame.ZoneNames, GetZoneName(n))
            zoneOptions.Add(makeZoneName n)
            UpdateGameFile()
            selectionChangeIsDisabled <- true
            zoneComboBox.SelectedIndex <- n
            selectionChangeIsDisabled <- false
            theGame.CurZone <- zoneComboBox.SelectedIndex
            curZoneChanged.Trigger()
            zm <- ZoneMemory.Get(theGame.CurZone)
            zoom()
            )
        renameZoneButton.Click.Add(fun _ ->
            let orig = GetZoneName(theGame.CurZone)
            let tb = new TextBox(IsReadOnly=false, FontSize=12., Text=(if orig=null then "" else orig), BorderThickness=Thickness(1.), 
                                    Foreground=Brushes.Black, Background=Brushes.White,
                                    Width=float(MAPX/2), Height=20., TextWrapping=TextWrapping.NoWrap, AcceptsReturn=false, 
                                    Margin=Thickness(5.))
            let closeEv = new Event<unit>()
            let mutable save = false
            let cb = new Button(Content=" Cancel ", Margin=Thickness(4.))
            let sb = new Button(Content=" Save ", Margin=Thickness(4.))
            cb.Click.Add(fun _ -> closeEv.Trigger())
            sb.Click.Add(fun _ -> save <- true; closeEv.Trigger())
            let dp = (new DockPanel(LastChildFill=true)).AddLeft(cb).AddRight(sb).Add(new DockPanel())
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
                zoneOptions.[theGame.CurZone] <- makeZoneName(theGame.CurZone)
                zoneComboBox.SelectedIndex <- theGame.CurZone 
                selectionChangeIsDisabled <- false
            )
        printCurrentZoneButton.Click.Add(fun _ ->
            let bmps = Array2D.create MAX MAX (null, -1)
            let gr = FeatureWindow.GridRange(MAX,MAX,0,0)
            for i = 0 to MAX-1 do
                for j = 0 to MAX-1 do
                    let bmp = zm.FullImgArray.GetCopyOfBmp(i,j)
                    if bmp <> null then
                        bmps.[i,j] <- bmp, -1
                        gr.Extend(i,j)
            let PrintRegion(bmpcis:(System.Drawing.Bitmap*int)[,], minx, miny, maxx, maxy, filename:string) =
                if maxx >= minx then // there was at least one screenshot
                    //for ma,fn in [MetaArea,"printed_meta.png";    MapArea,"printed_map.png"] do
                    for ma,fn in [MapArea,filename] do
                        let mx,my,mw,mh = ma
                        let r = new System.Drawing.Bitmap(mw*(maxx-minx+1), mh*(maxy-miny+1))
                        let rData = r.LockBits(System.Drawing.Rectangle(0,0,r.Width,r.Height), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                        for i = minx to maxx do
                            printfn "[%d..%d] - %d" minx maxx i
                            for j = miny to maxy do
                                let bmp,ci = bmpcis.[i,j]
                                if bmp <> null then
                                    let tf = 
                                        if ci = -1 then        // ci is color index for kinds (e.g. MasterKey interiors)
                                            id
                                        else
                                            (fun (a,r,g,b) ->
                                                let c = Utils.DistinctColors.[ci % Utils.DistinctColors.Length]
                                                let P = 0.8
                                                let f(x,y) = float x * P + float y * (1.0-P) |> byte
                                                f(a, c.A), f(r, c.R), f(g, c.G), f(b, c.B)
                                            )
                                    let data = bmp.LockBits(System.Drawing.Rectangle(0,0,bmp.Width,bmp.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                                    for x = 0 to mw-1 do
                                        for y = 0 to mh-1 do
                                            if x=mw-1 || y=mh-1 then
                                                Utils.SetColorFromLockedFormat32BppArgb(mw*(i-minx) + x, mh*(j-miny) + y,rData, System.Drawing.Color.Gray)
                                            else
                                                Utils.SetAndGetAndTransformColorFromLockedFormat32BppArgb(mw*(i-minx) + x, mh*(j-miny) + y, rData, mx+x, my+y, data, tf)
                                    bmp.UnlockBits(data)
                        r.UnlockBits(rData)
                        r.Save(fn, System.Drawing.Imaging.ImageFormat.Png)
            PrintRegion(bmps, gr.MinX, gr.MinY, gr.MaxX, gr.MaxY, "printed_map.png")
            // other kinds
            for k in screenshotKindUniverse do
                if k <> MAIN_KIND then
                    let workQ = new System.Collections.Generic.Queue<_>()
                    let mutable colorIndex = 0
                    for i = 0 to MAX-1 do
                        for j = 0 to MAX-1 do
                            let mt = zm.MapTiles.[i,j]
                            let mutable r = []
                            for ss in mt.ScreenshotsWithKinds do
                                if ss.Kinds |> Array.contains k then
                                    r <- bmpDict.[ss.Id] :: r
                            match r with
                            | [] -> ()
                            | _ -> 
                                workQ.Enqueue((i,j), r, colorIndex)
                                colorIndex <- colorIndex + 1
                    let bmps = Array2D.create MAX MAX (null,-1)
                    let gr = FeatureWindow.GridRange(MAX,MAX,0,0)
                    while workQ.Count <> 0 do
                        let (i,j),list,ci = workQ.Dequeue()
                        let f(x,y) = if x>=0 && y>=0 && x<MAX && y<MAX && fst(bmps.[x,y])=null then Some(x,y) else None
                        let which = [(i,j); (i,j+1); (i,j-1); (i+1,j); (i-1,j)] |> Seq.tryPick f 
                        match which,list with
                        | None,_ -> ()
                        | _, [] -> ()
                        | Some(i,j),hd::tl ->
                            bmps.[i,j] <- hd,ci
                            workQ.Enqueue((i,j),tl,ci)
                            gr.Extend(i,j)
                    PrintRegion(bmps, gr.MinX, gr.MinY, gr.MaxX, gr.MaxY, sprintf "printed_map_%s.png" k)
            )
        updateClipboardView <- (fun () ->
            if not(System.String.IsNullOrEmpty(clipboardSSID)) then
                let img = bmpDict.[clipboardSSID] |> Utils.BMPtoImage
                clipView.Child <- img
                img.Width <- clipView.ActualWidth
                img.Height <- clipView.ActualHeight
                clipTB.Text <- clipboardSSID
            )
        // window
        this.Title <- "Generic Screenshot Mapper"
        this.Left <- 1290.
        this.Top <- 4.
        //this.Topmost <- true
        this.UseLayoutRounding <- true
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
            featureButton.Click.Add(fun _ -> 
                let W = 220
                let diag = Utils.makeGrid(2,3,W,20)
                Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="Width"), 0, 0)
                let wInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="1", CaretIndex=1)
                Utils.gridAdd(diag, wInput, 1, 0)
                Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="Height"), 0, 1)
                let hInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="1", CaretIndex=1)
                Utils.gridAdd(diag, hInput, 1, 1)
                Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="Comma-separated zones"), 0, 2)
                let zs = theGame.CurZone.ToString()
                let zInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text=zs, CaretIndex=zs.Length)
                Utils.gridAdd(diag, zInput, 1, 2)
                let closeEv = new Event<unit>()
                wInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then hInput.Focus() |> ignore)
                hInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then zInput.Focus() |> ignore)
                zInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then closeEv.Trigger())
                Utils.DoModalDialogCore(this, diag, "Select zones to feature", closeEv.Publish, (fun () -> wInput.Focus() |> ignore))
                try
                    let w = wInput.Text |> int
                    let h = hInput.Text |> int
                    let zs = zInput.Text.Split([|','|], System.StringSplitOptions.None) |> Array.map (fun s -> if s="" then None else Some(ZoneMemory.Get(int s)))
                    if zs.Length <> w*h then failwith "wrong number of comma-separated entries"
                    let a = Array2D.init w h (fun x y -> zs.[y*h+x])
                    FeatureWindow.MakeFeatureMap(this.Owner,a)
                with e ->
                    System.Console.Beep()
                    printfn "FEATURE error: %s" (e.ToString())
                )
            sp.Children.Add(featureButton) |> ignore
            let dualFeatureButton = new Button(Content="Dual", Margin=Thickness(4.))
            dualFeatureButton.Click.Add(fun _ -> 
                let W = 220
                let diag = Utils.makeGrid(2,3,W,20)
                Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="Left Zone"), 0, 0)
                let lInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="1", CaretIndex=1)
                Utils.gridAdd(diag, lInput, 1, 0)
                Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="Right Zone"), 0, 1)
                let rInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="2", CaretIndex=1)
                Utils.gridAdd(diag, rInput, 1, 1)
                Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="minx,miny,maxx,maxy"), 0, 2)
                let boundsInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="", CaretIndex=0)
                Utils.gridAdd(diag, boundsInput, 1, 2)
                let closeEv = new Event<unit>()
                lInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then rInput.Focus() |> ignore)
                rInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then 
                                                boundsInput.Focus() |> ignore
                                                try
                                                    let gr1 = FeatureWindow.ComputeRange(ZoneMemory.Get(int lInput.Text))
                                                    let gr2 = FeatureWindow.ComputeRange(ZoneMemory.Get(int rInput.Text))
                                                    boundsInput.Text <- sprintf "%d,%d,%d,%d" (min gr1.MinX gr2.MinX) (min gr1.MinY gr2.MinY) (max gr1.MaxX gr2.MaxX) (max gr1.MaxY gr2.MaxY)
                                                    boundsInput.CaretIndex <- boundsInput.Text.Length
                                                with _ -> ()
                                                )
                boundsInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then closeEv.Trigger())
                Utils.DoModalDialogCore(this, diag, "Select zones to feature", closeEv.Publish, (fun () -> lInput.Focus() |> ignore))
                try
                    let l = lInput.Text |> int
                    let r = rInput.Text |> int
                    let [|a;b;c;d|] = boundsInput.Text.Split([|','|], System.StringSplitOptions.None) |> Array.map (fun s -> int s)
                    FeatureWindow.MakeDualFeatureMap(this, ZoneMemory.Get(l), ZoneMemory.Get(r), FeatureWindow.GridRange(a,b,c,d))
                with e ->
                    System.Console.Beep()
                    printfn "FEATURE error: %s" (e.ToString())

                )
            sp.Children.Add(dualFeatureButton) |> ignore
            sp
        mapPortion.Children.Add(topBar) |> ignore
        mapPortion.Children.Add(wholeMapCanvas) |> ignore
        all.Children.Add(mapPortion) |> ignore
        let bottom =
            refreshMetadataKeys()
            let rightColumn =
                let rc = (new DockPanel(LastChildFill=true)).AddBottom(clipDP)
                let mutable iconKeys = MapIcons.MakeIconUI(this)
                rc.Children.Add(iconKeys) |> ignore
                MapIcons.redrawPanelEv.Publish.Add(fun _ ->
                    rc.Children.Remove(iconKeys)
                    iconKeys <- MapIcons.MakeIconUI(this)
                    rc.Children.Add(iconKeys) |> ignore
                    )
                rc
            let leftColumn = (new DockPanel(LastChildFill=true, Background=Brushes.Yellow)).AddTop(summaryTB).Add(metaAndScreenshotPanel)
            let dp = (new DockPanel(LastChildFill=true, Width=float APP_WIDTH, Height=BOTTOM_HEIGHT)).AddRight(rightColumn).Add(leftColumn)
            let r = new Canvas(Width=float APP_WIDTH, Height=BOTTOM_HEIGHT)
            r.Children.Add(dp) |> ignore
            r
        all.Children.Add(bottom) |> ignore
        all.UseLayoutRounding <- true
        this.Content <- all
        this.Loaded.Add(fun _ ->
            let handle = Elephantasy.Winterop.GetConsoleWindow()
            Elephantasy.Winterop.ShowWindow(handle, Elephantasy.Winterop.SW_MINIMIZE) |> ignore
            Utils.setup(this)
            zoom()
            mapCanvasMouseLeaveFunc()  // to move the drawn cursor to correct spot on-screen
            this.Activate() |> ignore
            if false then   // this was useful for sidescape, which had empty screen area
                // minimap
                let uev = new Event<_>()
                uise.ChangedAndSettled.Add(fun _ ->
                    uev.Trigger(kbdX.Value, kbdY.Value, zm)
                    )
                let mini = new MinimapWindow.MinimapWindow(this.Owner, 2, uev.Publish)
                mini.Show()
                // abstract minimap
                let amini = new MinimapWindow.AbstractFixedMinimapWindow(this.Owner, uev.Publish)
                amini.Show()
                // notes
                let notes = new MinimapWindow.NotesWindow(this.Owner, uev.Publish)
                notes.Show()
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
        for k in ARROWKEYS do
            if(not(Elephantasy.Winterop.RegisterHotKey(helper.Handle, Elephantasy.Winterop.HOTKEY_ID+1, MOD_CONTROL, uint32 k))) then
                failwithf "could not register hotkey %A" k
    member this.UnregisterHotKey() =
        let helper = new System.Windows.Interop.WindowInteropHelper(this)
        Elephantasy.Winterop.UnregisterHotKey(helper.Handle, Elephantasy.Winterop.HOTKEY_ID) |> ignore
        Elephantasy.Winterop.UnregisterHotKey(helper.Handle, Elephantasy.Winterop.HOTKEY_ID+1) |> ignore
    member this.HwndHook(_hwnd:IntPtr, msg:int, wParam:IntPtr, lParam:IntPtr, handled:byref<bool>) : IntPtr =
        if Utils.aModalDialogIsOpen then (broadcastHotKeyEv.Trigger(msg,wParam,lParam); IntPtr.Zero) else
        let WM_HOTKEY = 0x0312
        let zm = ZoneMemory.Get(theGame.CurZone)
        if msg = WM_HOTKEY && (wParam.ToInt32() = Elephantasy.Winterop.HOTKEY_ID || wParam.ToInt32() = Elephantasy.Winterop.HOTKEY_ID+1) then
            if currentlyRunningAHotkeyCommand then
                () 
                //System.Console.Beep()
            else
                currentlyRunningAHotkeyCommand <- true
                let ctrl_bits = lParam.ToInt32() &&& 0xF
                let key = lParam.ToInt32() >>> 16
                if false then
                    for k in KEYS do
                        if key = k then
                            printfn "key %A was pressed, ctrl_bits are %d" k ctrl_bits
                if key = VK_SUBTRACT && zm.MapTiles.[theGame.CurX,theGame.CurY].ThereAreScreenshots() then   // assumes we want to remove last in the list; if user wants specific one, they click image and select among them
                    setCursor()
                    let id = (zm.MapTiles.[theGame.CurX,theGame.CurY].ScreenshotsWithKinds |> Array.last).Id
                    zm.MapTiles.[theGame.CurX,theGame.CurY].CutScreenshot(id)
                    clipboardSSID <- id
                    updateClipboardView()
                    // update current tile view
                    pictureChanged.Value <- true
                    RecomputeImage(theGame.CurX,theGame.CurY,zm)
                    zoom()
                    // update disk
                    SerializeMapTile(theGame.CurX,theGame.CurY,zm)
                if key = VK_ADD && not(System.String.IsNullOrEmpty(clipboardSSID)) then
                    setCursor()
                    zm.MapTiles.[theGame.CurX,theGame.CurY].AddScreenshot(clipboardSSID)
                    pictureChanged.Value <- true
                    SerializeMapTile(theGame.CurX,theGame.CurY,zm)
                    RecomputeImage(theGame.CurX,theGame.CurY,zm)
                    zoom()
                if key = VK_MULTIPLY then
                    theGame.CurProjection <- theGame.CurProjection + 1
                    if theGame.CurProjection >= 3 then
                        theGame.CurProjection <- 0
                    UpdateGameFile()
                    pictureChanged.Value <- true
                    zoom()
                if key = VK_NUMPAD4 then
                    if ctrl_bits = int MOD_CONTROL then
                        if theGame.CenterX > 0 then
                            theGame.CenterX <- theGame.CenterX - 1
                            UpdateGameFile()
                            zoom()
                    else
                        if theGame.CurX > 0 then
                            theGame.CurX <- theGame.CurX - 1
                            UpdateGameFile()
                            zoom()
                    setCursor()
                    warp()
                if key = VK_NUMPAD6 then
                    if ctrl_bits = int MOD_CONTROL then
                        if theGame.CenterX < 99 then
                            theGame.CenterX <- theGame.CenterX + 1
                            UpdateGameFile()
                            zoom()
                    else
                        if theGame.CurX < 99 then
                            theGame.CurX <- theGame.CurX + 1
                            UpdateGameFile()
                            zoom()
                    setCursor()
                    warp()
                if key = VK_NUMPAD8 then
                    if ctrl_bits = int MOD_CONTROL then
                        if theGame.CenterY > 0 then
                            theGame.CenterY <- theGame.CenterY - 1
                            UpdateGameFile()
                            zoom()
                    else
                        if theGame.CurY > 0 then
                            theGame.CurY <- theGame.CurY - 1
                            UpdateGameFile()
                            zoom()
                    setCursor()
                    warp()
                if key = VK_NUMPAD2 then
                    if ctrl_bits = int MOD_CONTROL then
                        if theGame.CenterY < 99 then
                            theGame.CenterY <- theGame.CenterY + 1
                            UpdateGameFile()
                            zoom()
                    else
                        if theGame.CurY < 99 then
                            theGame.CurY <- theGame.CurY + 1
                            UpdateGameFile()
                            zoom()
                    setCursor()
                    warp()
                if key = VK_NUMPAD0 then
                    setCursor()
                    let img,bmp,id = TakeNewScreenshot()
                    bmpDict.Add(id, bmp)
                    zm.MapTiles.[theGame.CurX,theGame.CurY].AddScreenshot(id)
                    pictureChanged.Value <- true
                    SerializeMapTile(theGame.CurX,theGame.CurY,zm)
                    RecomputeImage(theGame.CurX,theGame.CurY,zm)
                    zoom()
                if key = VK_NUMPAD7 then
                    setCursor()
                    if theGame.CurZoom > 1 then
                        theGame.CurZoom <- theGame.CurZoom - 1
                        UpdateGameFile()
                        zoom()
                    warp()
                if key = VK_NUMPAD9 then
                    setCursor()
                    if theGame.CurZoom < MAX/2 then
                        theGame.CurZoom <- theGame.CurZoom + 1
                        UpdateGameFile()
                        zoom()
                    warp()
                if key = VK_NUMPAD5 then
                    if theGame.CurX=kbdX.Value && theGame.CurY=kbdY.Value then
                        // center the current map
                        let gr = FeatureWindow.ComputeRange(zm)
                        let x = gr.MinX + (gr.MaxX-gr.MinX)/2 
                        let y = gr.MinY + (gr.MaxY-gr.MinY)/2 
                        theGame.CurX <- x
                        theGame.CurY <- y
                        theGame.CenterX <- x
                        theGame.CenterY <- y
                        zoom()
                        warp()
                    else
                        // warp mouse back to last keyboard location
                        theGame.CurX <- kbdX.Value
                        theGame.CurY <- kbdY.Value
                        warp()   
                    // temp kludge, shift things up one y
                    if false then
                        if theGame.CurY > 0 && zm.MapTiles.[theGame.CurX,theGame.CurY-1].IsEmpty then
                            zm.MapTiles.[theGame.CurX,theGame.CurY-1] <- zm.MapTiles.[theGame.CurX,theGame.CurY]
                            let e = MapTile()
                            e.Canonicalize()
                            zm.MapTiles.[theGame.CurX,theGame.CurY] <- e
                            SerializeMapTile(theGame.CurX,theGame.CurY-1,zm)
                            SerializeMapTile(theGame.CurX,theGame.CurY,zm)
                            RecomputeImage(theGame.CurX,theGame.CurY-1,zm)
                            RecomputeImage(theGame.CurX,theGame.CurY,zm)
                            //metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,theGame.CurX,theGame.CurY), origNote, newNote)
                            //refreshMetadataKeys()
                            zoom()   // redraw note preview in summary area
                        else
                            System.Console.Beep()
                if key = VK_DIVIDE then
                    setCursor()
                    Utils.Win32.SetForegroundWindow(_hwnd) |> ignore
                    let orig = zm.MapTiles.[theGame.CurX,theGame.CurY].Note
                    let tb = new TextBox(IsReadOnly=false, FontSize=12., Text=(if orig=null then "" else orig), BorderThickness=Thickness(1.), 
                                            Foreground=Brushes.Black, Background=Brushes.White,
                                            Width=float(MAPX/2), Height=float(MAPX/2), TextWrapping=TextWrapping.Wrap, AcceptsReturn=true, 
                                            VerticalScrollBarVisibility=ScrollBarVisibility.Visible, Margin=Thickness(5.))
                    let closeEv = new Event<unit>()
                    let mutable save = false
                    let cb = new Button(Content=" Cancel ", Margin=Thickness(4.))
                    let sb = new Button(Content=" Save ", Margin=Thickness(4.))
                    cb.Click.Add(fun _ -> closeEv.Trigger())
                    sb.Click.Add(fun _ -> save <- true; closeEv.Trigger())
                    let dp = (new DockPanel(LastChildFill=true)).AddLeft(cb).AddRight(sb).Add(new DockPanel())
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
                        pictureChanged.Value <- true // TODO decide if want separate updates for notes window changing, or how want to do this
                if key = VK_DECIMAL then
                    setCursor()
                    let orig = zm.MapTiles.[theGame.CurX,theGame.CurY].Note
                    let orig = if orig = null then "" else orig
                    //let special = "#TODO"
                    //let special = "#UV"
                    let special = "#NOW"
                    if orig.EndsWith(special) then
                        UpdateCurrentNote(orig, orig.Substring(0,orig.Length-special.Length), zm)
                    else
                        UpdateCurrentNote(orig, orig+"\n"+special, zm)
                currentlyRunningAHotkeyCommand <- false
        IntPtr.Zero
