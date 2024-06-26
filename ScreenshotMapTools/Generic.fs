﻿module Generic

let AAppend(a:_[],x) =
    if a=null then
        [|x|]
    else
        Array.init (a.Length+1) (fun i -> if i<a.Length then a.[i] else x)
let ACut(a:_[]) =
    if a=null || a.Length=0 then
        failwith "array cut"
    elif a.Length=1 then
        [||]
    else
        Array.init (a.Length-1) (fun i -> a.[i])

let APP_WIDTH = 1920 - 1280 - 12        // my monitor, a game, a little buffer
let APP_HEIGHT = 980. + 16. + 20.
let BOTTOM_HEIGHT = 480.
let KEYS_LIST_BOX_WIDTH = 150
let VIEWX = APP_WIDTH

//////////////////////////////////////////////////////////

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
open Elephantasy.Winterop
open GameSpecific
open BackingStoreData
open InMemoryStore

let mutable clipboardSSID = ""
let mutable updateClipboardView = fun() -> ()        // TODO how remove this ugly tangle

let SerializeMapTile(x,y) = 
    let json = System.Text.Json.JsonSerializer.Serialize<MapTile>(mapTiles.[x,y])
    WriteAllText(MapTileFilename(x,y), json)

let broadcastHotKeyEv = new Event<int*IntPtr*IntPtr>()

let DoScreenshotDisplayWindow(x,y,parent:Window) = 
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
    for id in mapTiles.[x,y].Screenshots do
        let ssid = id   // local immutable will get captured   // TODO needed?
        let bmp = bmpDict.[ssid]
        let img = Utils.BMPtoImage(bmp)
        let largeImage = Utils.ImageProjection(img,(0,0,GAMENATIVEW,GAMENATIVEH))
        let border = new Border(BorderThickness=Thickness(TH), Child=largeImage)
        sp.Children.Add(border) |> ignore
        allBorders.Add(border)
        border.MouseDown.Add(fun _ ->
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
                        mapTiles.[x,y].Screenshots <- mapTiles.[x,y].Screenshots |> Array.filter (fun s -> s <> ssid)
                        // update current tile view
                        RecomputeImage(x,y)
                        //zoom(...) will be called when the window closes
                        // update disk
                        SerializeMapTile(x,y)
                        //printfn "closing..."
                        closeEv.Trigger()
                        //printfn "closed"
        )
    Utils.DoModalDialog(parent, all, sprintf "All Screenshots for (%d,%d)" x y, closeEv.Publish)

type MyWindow() as this = 
    inherit Window()
    let KEYS = [| VK_NUMPAD0; VK_NUMPAD1; VK_NUMPAD2; VK_NUMPAD3; VK_NUMPAD4; VK_NUMPAD5; VK_NUMPAD6; VK_NUMPAD7; VK_NUMPAD8; VK_NUMPAD9;
                    VK_MULTIPLY; VK_ADD; VK_SUBTRACT; VK_DECIMAL; VK_DIVIDE (*; VK_RETURN *) |]
    let mutable curKey = null
    let MAPX,MAPY = VIEWX,420
    let mapCanvas = new Canvas(Width=float(MAPX), Height=float(MAPY), ClipToBounds=true)
    let RT = 4.
    let mouseCursor = new Shapes.Rectangle(StrokeThickness=RT/2.)
    let bottomFloat = new Canvas(Width=float APP_WIDTH, Height=BOTTOM_HEIGHT)    // a place to draw over the bottom potion of the app
    let mutable mapCanvasMouseMoveFunc = fun _ -> ()
    let mutable mapCanvasMouseDownFunc = fun _ -> ()
    let mutable curZoom = 7
    let mutable hwndSource = null
    // current zone combobox
    let addNewZoneButton = new Button(Content="Add new zone", Margin=Thickness(4.))
    let zoneOptions = System.Collections.ObjectModel.ObservableCollection<string>()
    let zoneComboBox = new ComboBox(ItemsSource=zoneOptions, IsReadOnly=true, IsEditable=false, SelectedIndex=0, Width=200., Margin=Thickness(4.))
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
    let refreshMetadataKeys() =
        //printfn "called refreshMetadataKeys()"
        let newKeys = metadataStore.AllKeys() |> Array.sort
        let oldKeys = if metadataKeys.Count=0 then [||] else metadataKeys |> Seq.skip 1 |> Seq.toArray   // skip 1 to ignore the 'no highlight'
        if oldKeys <> newKeys then
            metadataKeys.Clear()
            metadataKeys.Add("(no highlight)")
            for s in newKeys do
                metadataKeys.Add(s)                   // TODO consider displaying count
    let metaAndScreenshotPanel = new DockPanel(Margin=Thickness(4.), LastChildFill=true)
    let mutable doZoom = fun _ -> ()
    let mfsRefresh() =
        metaAndScreenshotPanel.Children.Clear()
        if imgArray.[theGame.CurX,theGame.CurY] <> null then
            let top = Utils.ImageProjection(imgArray.[theGame.CurX,theGame.CurY],MetaArea)
            metaAndScreenshotPanel.Children.Add(top) |> ignore
            DockPanel.SetDock(top, Dock.Top)
            let largeImage = Utils.ImageProjection(imgArray.[theGame.CurX,theGame.CurY],(0,0,GAMENATIVEW,GAMENATIVEH))
            metaAndScreenshotPanel.Children.Add(largeImage) |> ignore
            largeImage.MouseDown.Add(fun _ ->
                let cmt = mapTiles.[theGame.CurX, theGame.CurY]
                if cmt.Screenshots.Length > 1 then
                    DoScreenshotDisplayWindow(theGame.CurX, theGame.CurY, this)
                    doZoom(theGame.CurX, theGame.CurY, curZoom)
                )
        else
            let w = float(APP_WIDTH - KEYS_LIST_BOX_WIDTH - 2*4)
            let h = w / 16. * 9.
            metaAndScreenshotPanel.Children.Add(new DockPanel(Background=Brushes.DarkSlateBlue, Width=w, Height=h)) |> ignore
    // zoom/refresh
    let mutable curProjection = 1  // 0=full, 1=map, 2=meta
    let project(img) =
        match curProjection with
        | 0 -> img
        | 1 -> Utils.ImageProjection(img,MapArea)
        | 2 -> Utils.ImageProjection(img,MetaArea)
        | _ -> failwith "bad curProjection"
    let zoomTextboxes = Array2D.init MAX MAX (fun i j ->
        new TextBox(IsReadOnly=true, IsHitTestVisible=true, FontSize=12., Text=sprintf"%02d,%02d"i j, BorderThickness=Thickness(1.), Foreground=Brushes.Black, 
                    HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
        )
    let rec zoom(ci, cj, level) = // level = 1->1x1, 2->3x3, 3->5x5, etc    
        //printfn "called zoom(%d,%d,%d)" ci cj level
        let aspect,kludge = 
            match curProjection with
            | 0 -> GAMEASPECT, 0
            | 1 -> let _,_,w,h = MapArea in float w / float h, 0
            | 2 -> let _,_,w,h = MetaArea in float w / float h, 9
            | _ -> failwith "bad curProjection"
        let VIEWY = System.Math.Floor((float(VIEWX)/aspect) + 0.83) |> int
        let DX,DY = float(MAPX - VIEWX)/2., float(MAPY - VIEWY)/2.
        let scale = float(2*(level-1)+1)
        mapCanvas.Children.Clear()
        let W,H = float(VIEWX)/scale,float(VIEWY)/scale
        let toHighlight = if curKey <> null then metadataStore.LocationsForKey(curKey) else System.Collections.Generic.HashSet()
        for i = ci-level to ci+level do
            for j = cj-level-kludge to cj+level+kludge do
                if i>=0 && i<MAX && j>=0 && j<MAX then
                    if imgArray.[i,j] <> null then
                        let img = project(imgArray.[i,j])
                        img.Width <- W
                        img.Height <- H
                        img.Stretch <- System.Windows.Media.Stretch.Fill
                        Utils.canvasAdd(mapCanvas, img, DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
                    else
                        let tb = zoomTextboxes.[i,j]
                        Utils.deparent(tb)
                        tb.Background <- (if mapTiles.[i,j].IsEmpty then (if (i+j)%2 = 0 then Brushes.LightGray else Brushes.DarkGray) else Brushes.CornflowerBlue)
                        tb.Width <- W
                        tb.Height<- H
                        Utils.canvasAdd(mapCanvas, tb, DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
                    if toHighlight.Contains(GenericMetadata.Location(theGame.CurZone,i,j)) then
                        Utils.canvasAdd(mapCanvas, new Shapes.Ellipse(Stroke=Brushes.Lime, Width=W, Height=H, StrokeThickness=3.), DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
                else
                    Utils.canvasAdd(mapCanvas, new DockPanel(Background=Brushes.LightGray, Width=W, Height=H), DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
        let cursor = new Shapes.Rectangle(Stroke=Brushes.Yellow, StrokeThickness=RT, Width=W + RT*2., Height=H + RT*2.)
        Utils.canvasAdd(mapCanvas, cursor, DX-W+float(level)*W-RT, DY-H+float(level)*H-RT)
        let cmt = mapTiles.[ci,cj]
        summaryTB.Text <- sprintf "(%02d,%02d)        %d screenshots\n%s" ci cj (if cmt.Screenshots=null then 0 else cmt.Screenshots.Length) cmt.Note
        mfsRefresh()
        do
            mouseCursor.Width <- W + RT
            mouseCursor.Height <- H + RT
            mapCanvas.Children.Add(mouseCursor) |> ignore
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
                if i>=0 && i<MAX && j>=0 && j<MAX && imgArray.[i,j] <> null then
                    let largeImage = Utils.ImageProjection(imgArray.[i,j],(0,0,GAMENATIVEW,GAMENATIVEH))
                    let cmt = mapTiles.[i,j]
                    floatSummaryTB.Text <- sprintf "(%02d,%02d)        %d screenshots\n%s" i j (if cmt.Screenshots=null then 0 else cmt.Screenshots.Length) cmt.Note
                    let dp = new DockPanel(Width=float APP_WIDTH - float KEYS_LIST_BOX_WIDTH - 30., Height=BOTTOM_HEIGHT - 10., Background=Brushes.Cyan, LastChildFill=true)
                    DockPanel.SetDock(largeImage, Dock.Bottom)
                    dp.Children.Add(largeImage) |> ignore
                    Utils.deparent(floatSummaryTB)
                    dp.Children.Add(floatSummaryTB) |> ignore
                    Utils.canvasAdd(bottomFloat, new Border(Child=dp, BorderBrush=Brushes.Cyan, BorderThickness=Thickness(4.)), 0., 0.)
                )
            mapCanvasMouseDownFunc <- (fun (x,y) ->
                // compute which index we are over
                let i = (ci - level) + int((x - DX + W)/W)
                let j = (cj - level) + int((y - DY + H)/H)
                if i>=0 && i<MAX && j>=0 && j<MAX then
                    theGame.CurX <- i
                    theGame.CurY <- j
                    UpdateGameFile()
                    zoom(theGame.CurX,theGame.CurY, curZoom)
                )
    and UpdateGameFile() =
        let gameFile = System.IO.Path.Combine(GetRootFolder(), "game.json")
        let json = System.Text.Json.JsonSerializer.Serialize<Game>(theGame)
        WriteAllText(gameFile, json)
    let UpdateCurrentNote(origNote, newNote) =
        mapTiles.[theGame.CurX,theGame.CurY].Note <- newNote
        SerializeMapTile(theGame.CurX,theGame.CurY)
        metadataStore.ChangeNote(GenericMetadata.Location(theGame.CurZone,theGame.CurX,theGame.CurY), origNote, newNote)
        refreshMetadataKeys()
        zoom(theGame.CurX,theGame.CurY, curZoom)   // redraw note preview in summary area
    do
        doZoom <- zoom
        mapCanvas.MouseMove.Add(fun me -> let p = me.GetPosition(mapCanvas) in mapCanvasMouseMoveFunc(p.X, p.Y))
        mapCanvas.MouseLeave.Add(fun _ -> mouseCursor.Stroke <- Brushes.Transparent; bottomFloat.Children.Clear())
        mapCanvas.MouseDown.Add(fun me -> let p = me.GetPosition(mapCanvas) in mapCanvasMouseDownFunc(p.X, p.Y))
        // init zones and ensure directories
        LoadRootGameData()
        let zoneFolder = GetZoneFolder()
        System.IO.Directory.CreateDirectory(zoneFolder) |> ignore
        System.IO.Directory.CreateDirectory(System.IO.Path.Combine(GetRootFolder(),SCREENSHOTS_FOLDER)) |> ignore
        let savedZone = theGame.CurZone
        for i = 0 to theGame.ZoneNames.Length-1 do
            // populate zone names for combobox
            zoneOptions.Add(theGame.ZoneNames.[i])
            // populate key metdata from all notes
            theGame.CurZone <- i
            LoadZoneMapTiles(false)
        theGame.CurZone <- savedZone
        // populate images for initial map
        LoadZoneMapTiles(true)
        zoneComboBox.ItemsSource <- zoneOptions
        zoneComboBox.SelectedIndex <- savedZone
        // zone changes
        zoneComboBox.SelectionChanged.Add(fun _ ->
            theGame.CurZone <- zoneComboBox.SelectedIndex
            UpdateGameFile()
            LoadZoneMapTiles(true)
            zoom(theGame.CurX, theGame.CurY, curZoom)
            )
        addNewZoneButton.Click.Add(fun _ ->
            let n = theGame.ZoneNames.Length
            theGame.ZoneNames <- AAppend(theGame.ZoneNames, GetZoneName(n))
            zoneOptions.Add(theGame.ZoneNames.[n])
            UpdateGameFile()
            zoneComboBox.SelectedIndex <- n
            LoadZoneMapTiles(true)
            zoom(theGame.CurX, theGame.CurY, curZoom)
            )
        printCurrentZoneButton.Click.Add(fun _ ->
            // TODO
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
            if maxx >= minx then // there was at least one screenshot
                for ma,fn in [MetaArea,"printed_meta.png";    MapArea,"printed_map.png"] do
                    let mx,my,mw,mh = ma
                    let r = new System.Drawing.Bitmap(mw*(maxx-minx+1), mh*(maxy-miny+1))
                    for i = minx to maxx do
                        for j = miny to maxy do
                            let bmp = bmps.[i,j]
                            if bmp <> null then
                                for x = 0 to mw-1 do
                                    for y = 0 to mh-1 do
                                        r.SetPixel(mw*(i-minx) + x, mh*(j-miny) + y, bmp.GetPixel(mx+x,my+y))
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
            sp
        mapPortion.Children.Add(topBar) |> ignore
        mapPortion.Children.Add(mapCanvas) |> ignore
        all.Children.Add(mapPortion) |> ignore
        let bottom =
            let dp = new DockPanel(LastChildFill=true, Width=float APP_WIDTH, Height=BOTTOM_HEIGHT)
            refreshMetadataKeys()
            let keysListBox = new ListBox(ItemsSource=metadataKeys, MinWidth=float KEYS_LIST_BOX_WIDTH, Margin=Thickness(4.), MaxHeight=350.)
            keysListBox.SelectionChanged.Add(fun _ -> 
                curKey <- if keysListBox.SelectedIndex <= 0 || metadataKeys.Count=0 then null else metadataKeys.Item(keysListBox.SelectedIndex)
                //printfn "curKey is '%s'" curKey
                zoom(theGame.CurX, theGame.CurY, curZoom)
                )
            let rightColumn =
                let rc = new DockPanel(LastChildFill=true)
                rc.Children.Add(clipDP) |> ignore
                DockPanel.SetDock(clipDP, Dock.Bottom)
                rc.Children.Add(keysListBox) |> ignore
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
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Elephantasy.Winterop.HOTKEY_ID then
                //let ctrl_bits = lParam.ToInt32() &&& 0xF  // see WM_HOTKEY docs
                let key = lParam.ToInt32() >>> 16
                if false then
                    for k in KEYS do
                        if key = k then
                            printfn "key %A was pressed" k
                if key = VK_SUBTRACT && imgArray.[theGame.CurX,theGame.CurY]<>null then   // assumes we want to remove last in the list; if user wants specific one, they click image and select among them
                    let id = mapTiles.[theGame.CurX,theGame.CurY].Screenshots |> Array.last
                    mapTiles.[theGame.CurX,theGame.CurY].Screenshots <- ACut(mapTiles.[theGame.CurX,theGame.CurY].Screenshots)
                    clipboardSSID <- id
                    updateClipboardView()
                    // update current tile view
                    RecomputeImage(theGame.CurX,theGame.CurY)
                    zoom(theGame.CurX,theGame.CurY, curZoom)
                    // update disk
                    SerializeMapTile(theGame.CurX,theGame.CurY)
                if key = VK_ADD && not(System.String.IsNullOrEmpty(clipboardSSID)) then
                    mapTiles.[theGame.CurX,theGame.CurY].Screenshots <- AAppend(mapTiles.[theGame.CurX,theGame.CurY].Screenshots, clipboardSSID)
                    SerializeMapTile(theGame.CurX,theGame.CurY)
                    RecomputeImage(theGame.CurX,theGame.CurY)
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
                    mapTiles.[theGame.CurX,theGame.CurY].Screenshots <- AAppend(mapTiles.[theGame.CurX,theGame.CurY].Screenshots, id)
                    SerializeMapTile(theGame.CurX,theGame.CurY)
                    RecomputeImage(theGame.CurX,theGame.CurY)
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
                    let orig = mapTiles.[theGame.CurX,theGame.CurY].Note
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
                        UpdateCurrentNote(orig, tb.Text)
                if key = VK_DECIMAL then
                    let orig = mapTiles.[theGame.CurX,theGame.CurY].Note
                    let orig = if orig = null then "" else orig
                    if orig.EndsWith("#TODO") then
                        UpdateCurrentNote(orig, orig.Substring(0,orig.Length-5))
                    else
                        UpdateCurrentNote(orig, orig+"\n#TODO")
        IntPtr.Zero
