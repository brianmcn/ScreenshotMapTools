module Generic

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

module Win32 =
    [<System.Runtime.InteropServices.DllImport("User32.dll")>]
    extern bool PrintWindow(System.IntPtr hwnd, nativeint hdcBlt, uint32 nFlags)

let GetWindowScreenshot(hwnd:System.IntPtr, w, h) =
    let bmp = new System.Drawing.Bitmap(w, h, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
    let gfxBmp = System.Drawing.Graphics.FromImage(bmp)
    let hdcBitmap = gfxBmp.GetHdc()
    let PW_CLIENTONLY = 1u
    let PW_RENDERFULLCONTENT = 2u
    let succeeded = Win32.PrintWindow(hwnd, hdcBitmap, PW_RENDERFULLCONTENT ||| PW_CLIENTONLY)   // theoretically might run faster if topmost, but in practice, I didn't see difference, and this is not the bottleneck
    gfxBmp.ReleaseHdc(hdcBitmap)
    if not succeeded then
        gfxBmp.FillRectangle(new System.Drawing.SolidBrush(System.Drawing.Color.Black), new System.Drawing.Rectangle(System.Drawing.Point.Empty, bmp.Size))
    (*
    let hRgn = Win32.CreateRectRgn(0, 0, 0, 0)
    Win32.GetWindowRgn(hwnd, hRgn) |> ignore
    let region = Region.FromHrgn(hRgn)
    if not(region.IsEmpty(gfxBmp)) then
        gfxBmp.ExcludeClip(region)
        gfxBmp.Clear(Color.Transparent)
    Win32.DeleteObject(hRgn) |> ignore
    *)
    gfxBmp.Dispose()
    bmp


// root folder for a game:
//let GAME = "Void Stranger"   // recompile for different uses, for now
//let WINDOW_TITLE = "ScreenshotMapTools (Running) - Microsoft Visual Studio"  //"Void Stranger"
let GAME = "Leafs Odyssey"   // recompile for different uses, for now
let WINDOW_TITLE = "Leaf's Odyssey"
let GAMESCREENW, GAMESCREENH = 960, 540
let NATIVE_FACTOR = 2
let GAMENATIVEW, GAMENATIVEH = GAMESCREENW/NATIVE_FACTOR, GAMESCREENH/NATIVE_FACTOR
let GAMEASPECT = float(GAMENATIVEW) / float(GAMENATIVEH)
let VIEWX = 630   // multiple of 9, 7, 5 so that we can so various zooms well onscreen
let MapArea  =  96, 14, 384, 256         // x,y,w,h
let MetaArea = 200,  0, 170,  14



let GetRootFolder() = System.IO.Path.Combine(GAME)

[<AllowNullLiteral>]
type Game() =   // e.g. Zelda
    member val ZoneNames : string[] = null with get,set           // e.g. Overworld,Dungeon1 
    member val MetadataNames : string[] = null with get,set       // e.g. TakeAny,BurnBush

// load root game data
let theGame = Game()
let LoadRootGameData() =
    let gameFile = System.IO.Path.Combine(GetRootFolder(), "game.json")
    if not(System.IO.File.Exists(gameFile)) then
        theGame.ZoneNames <- [| "zone00" |]
        let json = System.Text.Json.JsonSerializer.Serialize<Game>(theGame)
        System.IO.File.WriteAllText(gameFile, json)
    else
        let json = System.IO.File.ReadAllText(gameFile)
        let data = System.Text.Json.JsonSerializer.Deserialize<Game>(json)
        theGame.ZoneNames <- data.ZoneNames
        theGame.MetadataNames <- theGame.MetadataNames

// screenshots folder of yyyy-MM-dd-HH-mm-ss
let DATE_TIME_FORMAT = "yyyy-MM-dd-HH-mm-ss"
let SCREENSHOTS_FOLDER = "screenshots"
let ScreenshotFilenameFromTimestampId(id) =
    System.IO.Path.Combine(GetRootFolder(), SCREENSHOTS_FOLDER, id+".png")
let SaveScreenshot(bmp : System.Drawing.Bitmap) =
    let now = System.DateTime.Now
    let id = now.ToString(DATE_TIME_FORMAT)
    let file = ScreenshotFilenameFromTimestampId(id)
    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName(file)) |> ignore
    bmp.Save(file, System.Drawing.Imaging.ImageFormat.Png)
    let img = Utils.BMPtoImage bmp
    img.Stretch <- System.Windows.Media.Stretch.Fill
    //System.Windows.Media.RenderOptions.SetBitmapScalingMode(img, System.Windows.Media.BitmapScalingMode.NearestNeighbor)
    id, img
// each zone has a folder:
// with files MapTileXnnYmm where nn/mm range 00-99
let mutable curZone = 0
let GetZoneFolder() = System.IO.Path.Combine(GetRootFolder(), sprintf "zone%02d" curZone)
let GetZoneName(zoneNum) = sprintf "zone%02d" zoneNum  // TODO load names, support renaming

[<AllowNullLiteral>]
type MapTile() =   // e.g. 50,50
    member val Screenshots : string[] = null with get,set         // e.g. [ 2024-05-12-09-45-43, 2024-05-12-09-46-16 ]
    member val Note : string = null with get,set                  // e.g. "I spawned here at start"
    member this.IsEmpty = (this.Screenshots = null || this.Screenshots.Length=0) && (this.Note = null || this.Note="")
let MapTileFilename(i,j) = System.IO.Path.Combine(GetZoneFolder(), sprintf "tile%02d-%02d.json" i j)

let mutable curX,curY = 50,50
let TakeNewScreenshot() =
    let bmp =
        let mutable r = None
        for KeyValue(hwnd,(title,rect)) in Elephantasy.Screenshot.GetOpenWindows() do
            if title = WINDOW_TITLE then
                r <- Some hwnd
        match r with
        | Some(hwnd) -> GetWindowScreenshot(hwnd, GAMESCREENW, GAMESCREENH)
        | None -> failwith "window not found"
    let nativeBmp = new System.Drawing.Bitmap(GAMENATIVEW, GAMENATIVEH)
    for i = 0 to GAMENATIVEW-1 do
        for j = 0 to GAMENATIVEH-1 do
            nativeBmp.SetPixel(i, j, bmp.GetPixel(i*NATIVE_FACTOR, j*NATIVE_FACTOR))
    let id,img = SaveScreenshot(nativeBmp)
    // TODO append to MapTile
    img, id

//////////////////////////////////////////////////////////

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
open Elephantasy.Winterop
type MyWindow() as this = 
    inherit Window()
    // TODO fix key set
    let KEYS = [| VK_NUMPAD0; VK_NUMPAD1; VK_NUMPAD2; VK_NUMPAD3; VK_NUMPAD4; VK_NUMPAD5; VK_NUMPAD6; VK_NUMPAD7; VK_NUMPAD8; VK_NUMPAD9;
                    VK_MULTIPLY; VK_ADD; VK_SUBTRACT; VK_DECIMAL; VK_DIVIDE (*; VK_RETURN *) |]
    let MAX = 100
    let imgArray : Image[,] = Array2D.zeroCreate 100 100
    let mapTiles = Array2D.create 100 100 (MapTile())
    let metadataStore = GenericMetadata.MetadataStore()
    let mutable curKey = null
    let MAPX,MAPY = 720,420
    let mapCanvas = new Canvas(Width=float(MAPX), Height=float(MAPY), ClipToBounds=true)
    let mutable curZoom = 2
    let mutable hwndSource = null
    let LoadZoneMapTiles(alsoLoadImages) =
        // load map tile data and screenshots from disk
        for i = 0 to MAX-1 do
            for j = 0 to MAX-1 do
                let file = MapTileFilename(i,j)
                if System.IO.File.Exists(file) then
                    let json = System.IO.File.ReadAllText(file)
                    let data = System.Text.Json.JsonSerializer.Deserialize<MapTile>(json)
                    mapTiles.[i,j] <- data
                    metadataStore.ChangeNote(GenericMetadata.Location(curZone,i,j), "", data.Note)
                    if alsoLoadImages && data.Screenshots <> null && data.Screenshots.Length > 0 then
                        let ts = data.Screenshots.[data.Screenshots.Length-1]
                        let ssFile = ScreenshotFilenameFromTimestampId(ts)
                        let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                        imgArray.[i,j] <- Utils.BMPtoImage bmp
                    else
                        imgArray.[i,j] <- null
                else
                    mapTiles.[i,j] <- MapTile()
                    imgArray.[i,j] <- null
    // current zone combobox
    let addNewZoneButton = new Button(Content="Add new zone", Margin=Thickness(4.))
    let zoneOptions = System.Collections.ObjectModel.ObservableCollection<string>()
    let zoneComboBox = new ComboBox(ItemsSource=zoneOptions, IsReadOnly=true, IsEditable=false, SelectedIndex=0, Width=200., Margin=Thickness(4.))
    // summary of current selection
    let summaryTB = new TextBox(IsReadOnly=true, FontSize=12., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.White,
                                    Height=120., VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(4.))
    // clipboard display
    let clipTB = new TextBox(IsReadOnly=true, FontSize=12., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.White, Margin=Thickness(4.))
    let clipView = new Border(Width=float(VIEWX/6), Height=float(VIEWX/6), BorderThickness=Thickness(4.), BorderBrush=Brushes.Orange, Margin=Thickness(4.))
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
        metadataKeys.Clear()
        metadataKeys.Add("(no highlight)")
        for s in metadataStore.AllKeys() |> Array.sort do
            metadataKeys.Add(s)
    let mfssp = new StackPanel(Orientation=Orientation.Vertical, Width=(let _,_,w,_ = MetaArea in float w * 1.5), Margin=Thickness(4.))
    let mfsRefresh() =
        mfssp.Children.Clear()
        if imgArray.[curX,curY] <> null then
            mfssp.Children.Add(Utils.ImageProjection(imgArray.[curX,curY],MetaArea)) |> ignore
            mfssp.Children.Add(Utils.ImageProjection(imgArray.[curX,curY],(0,0,GAMENATIVEW,GAMENATIVEH))) |> ignore
    // zoom/refresh
    let mutable curProjection = 0  // 0=full, 1=map, 2=meta
    let project(img) =
        match curProjection with
        | 0 -> img
        | 1 -> Utils.ImageProjection(img,MapArea)
        | 2 -> Utils.ImageProjection(img,MetaArea)
        | _ -> failwith "bad curProjection"
    let zoom(ci, cj, level) = // level = 1->1x1, 2->3x3, 3->5x5, etc
        //printfn "called zoom(%d,%d,%d)" ci cj level
        let aspect = 
            match curProjection with
            | 0 -> GAMEASPECT
            | 1 -> let _,_,w,h = MapArea in float w / float h
            | 2 -> let _,_,w,h = MetaArea in float w / float h
            | _ -> failwith "bad curProjection"
        let VIEWY = System.Math.Floor((float(VIEWX)/aspect) + 0.83) |> int
        let DX,DY = float(MAPX - VIEWX)/2., float(MAPY - VIEWY)/2.
        let scale = float(2*(level-1)+1)
        mapCanvas.Children.Clear()
        let W,H = float(VIEWX)/scale,float(VIEWY)/scale
        let toHighlight = if curKey <> null then metadataStore.LocationsForKey(curKey) else System.Collections.Generic.HashSet()
        for i = ci-level to ci+level do
            for j = cj-level to cj+level do
                if i>=0 && i<MAX && j>=0 && j<MAX then
                    if imgArray.[i,j] <> null then
                        let img = project(imgArray.[i,j])
                        img.Width <- W
                        img.Height <- H
                        img.Stretch <- System.Windows.Media.Stretch.Fill
                        Utils.canvasAdd(mapCanvas, img, DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
                    else
                        let tb = new TextBox(IsReadOnly=true, FontSize=12., Text=sprintf"%02d,%02d"i j, BorderThickness=Thickness(1.), Foreground=Brushes.Black, 
                                                Background=(if mapTiles.[i,j].IsEmpty then (if (i+j)%2 = 0 then Brushes.LightGray else Brushes.DarkGray) else Brushes.CornflowerBlue),
                                                Width=W, Height=H, HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
                        Utils.canvasAdd(mapCanvas, tb, DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
                else
                    Utils.canvasAdd(mapCanvas, new DockPanel(Background=Brushes.LightGray, Width=W, Height=H), DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
                if toHighlight.Contains(GenericMetadata.Location(curZone,i,j)) then
                    Utils.canvasAdd(mapCanvas, new Shapes.Ellipse(Stroke=Brushes.Lime, Width=W, Height=H, StrokeThickness=3.), DX-W+float(i-ci+level)*W, DY-H+float(j-cj+level)*H)
        let RT = 4.
        let cursor = new Shapes.Rectangle(Stroke=Brushes.Yellow, StrokeThickness=RT, Width=W + RT*2., Height=H + RT*2.)
        Utils.canvasAdd(mapCanvas, cursor, DX-W+float(level)*W-RT, DY-H+float(level)*H-RT)
        let cmt = mapTiles.[ci,cj]
        summaryTB.Text <- sprintf "(%02d,%02d)        %d screenshots\n%s" ci cj (if cmt.Screenshots=null then 0 else cmt.Screenshots.Length) cmt.Note
        mfsRefresh()
    let mutable clipboard = ""
    do
        // init zones and ensure directories
        LoadRootGameData()
        let zoneFolder = GetZoneFolder()
        System.IO.Directory.CreateDirectory(zoneFolder) |> ignore
        System.IO.Directory.CreateDirectory(System.IO.Path.Combine(GetRootFolder(),SCREENSHOTS_FOLDER)) |> ignore
        for i = 0 to theGame.ZoneNames.Length-1 do
            // populate zone names for combobox
            zoneOptions.Add(theGame.ZoneNames.[i])
            // populate key metdata from all notes
            curZone <- i
            LoadZoneMapTiles(false)
        // populate images for initial map
        curZone <- 0
        LoadZoneMapTiles(true)
        zoneComboBox.ItemsSource <- zoneOptions
        zoneComboBox.SelectedIndex <- 0
        // zone changes
        zoneComboBox.SelectionChanged.Add(fun _ ->
            curZone <- zoneComboBox.SelectedIndex
            LoadZoneMapTiles(true)
            zoom(curX, curY, curZoom)
            )
        addNewZoneButton.Click.Add(fun _ ->
            let n = theGame.ZoneNames.Length
            theGame.ZoneNames <- AAppend(theGame.ZoneNames, GetZoneName(n))
            zoneOptions.Add(theGame.ZoneNames.[n])
            let gameFile = System.IO.Path.Combine(GetRootFolder(), "game.json")
            let json = System.Text.Json.JsonSerializer.Serialize<Game>(theGame)
            System.IO.File.WriteAllText(gameFile, json)
            zoneComboBox.SelectedIndex <- n
            LoadZoneMapTiles(true)
            zoom(curX, curY, curZoom)
            )
        // window
        this.Title <- "Generic Screenshot Mapper"
        this.Left <- 950.
        this.Top <- 10.
        //this.Topmost <- true
        this.SizeToContent <- SizeToContent.Manual
        this.Width <- 720. + 16.
        this.Height <- 800. + 16. + 20.
        // layout
        let all = new StackPanel(Orientation=Orientation.Vertical)
        let top =
            let sp = new StackPanel(Orientation=Orientation.Horizontal)
            sp.Children.Add(addNewZoneButton) |> ignore
            sp.Children.Add(zoneComboBox) |> ignore
            sp
        all.Children.Add(top) |> ignore
        all.Children.Add(mapCanvas) |> ignore
        let bottom =
            let r = new DockPanel(LastChildFill=true)
            refreshMetadataKeys()
            let keysListBox = new ListBox(ItemsSource=metadataKeys, MinWidth=150., Margin=Thickness(4.))
            r.Children.Add(keysListBox) |> ignore
            DockPanel.SetDock(keysListBox, Dock.Right)
            keysListBox.SelectionChanged.Add(fun _ -> 
                curKey <- if keysListBox.SelectedIndex <= 0 || metadataKeys.Count=0 then null else metadataKeys.Item(keysListBox.SelectedIndex)
                printfn "curKey is '%s'" curKey
                zoom(curX, curY, curZoom)
                )

            let left = new DockPanel(LastChildFill=true)
            let bot = new DockPanel(LastChildFill=true)
            left.Children.Add(bot) |> ignore
            DockPanel.SetDock(bot, Dock.Bottom)

            bot.Children.Add(mfssp) |> ignore
            DockPanel.SetDock(mfssp, Dock.Left)
            bot.Children.Add(clipDP) |> ignore
            DockPanel.SetDock(clipDP, Dock.Right)
            bot.Children.Add(new DockPanel()) |> ignore

            left.Children.Add(summaryTB) |> ignore

            r.Children.Add(left) |> ignore
            r
        all.Children.Add(bottom) |> ignore
        all.UseLayoutRounding <- true
        this.Content <- all
        this.Loaded.Add(fun _ ->
            let handle = Elephantasy.Winterop.GetConsoleWindow()
            Elephantasy.Winterop.ShowWindow(handle, Elephantasy.Winterop.SW_MINIMIZE) |> ignore
            zoom(50,50,2)
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
        if Utils.aModalDialogIsOpen then IntPtr.Zero else
        let WM_HOTKEY = 0x0312
        if msg = WM_HOTKEY then
            if wParam.ToInt32() = Elephantasy.Winterop.HOTKEY_ID then
                //let ctrl_bits = lParam.ToInt32() &&& 0xF  // see WM_HOTKEY docs
                let key = lParam.ToInt32() >>> 16
                if false then
                    for k in KEYS do
                        if key = k then
                            printfn "key %A was pressed" k
                if key = VK_SUBTRACT && imgArray.[curX,curY]<>null then
                    // remove the data and img
                    let img,id = imgArray.[curX,curY], mapTiles.[curX,curY].Screenshots |> Array.last
                    mapTiles.[curX,curY].Screenshots <- ACut(mapTiles.[curX,curY].Screenshots)
                    clipboard <- id
                    // update current tile view
                    if mapTiles.[curX,curY].Screenshots.Length > 0 then
                        let newId = mapTiles.[curX,curY].Screenshots |> Array.last
                        let ssFile = ScreenshotFilenameFromTimestampId(newId)
                        let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                        imgArray.[curX,curY] <- Utils.BMPtoImage bmp
                    else
                        imgArray.[curX,curY] <- null
                    zoom(curX, curY, curZoom)
                    // update disk
                    let json = System.Text.Json.JsonSerializer.Serialize<MapTile>(mapTiles.[curX,curY])
                    System.IO.File.WriteAllText(MapTileFilename(curX,curY), json)
                    // update the clipboard view
                    Utils.deparent(img)
                    img.Stretch <- Stretch.Uniform
                    clipView.Child <- img
                    clipTB.Text <- id
                if key = VK_ADD && not(System.String.IsNullOrEmpty(clipboard)) then
                    let ssFile = ScreenshotFilenameFromTimestampId(clipboard)
                    let bmp = System.Drawing.Bitmap.FromFile(ssFile) :?> System.Drawing.Bitmap
                    let img = Utils.BMPtoImage bmp
                    imgArray.[curX,curY] <- img
                    mapTiles.[curX,curY].Screenshots <- AAppend(mapTiles.[curX,curY].Screenshots, clipboard)
                    let json = System.Text.Json.JsonSerializer.Serialize<MapTile>(mapTiles.[curX,curY])
                    System.IO.File.WriteAllText(MapTileFilename(curX,curY), json)
                    zoom(curX, curY, curZoom)
                if key = VK_MULTIPLY then
                    curProjection <- curProjection + 1
                    if curProjection >= 3 then
                        curProjection <- 0
                    zoom(curX, curY, curZoom)
                if key = VK_NUMPAD4 then
                    if curX > 0 then
                        curX <- curX - 1
                        zoom(curX, curY, curZoom)
                if key = VK_NUMPAD6 then
                    if curX < 99 then
                        curX <- curX + 1
                        zoom(curX, curY, curZoom)
                if key = VK_NUMPAD8 then
                    if curY > 0 then
                        curY <- curY - 1
                        zoom(curX, curY, curZoom)
                if key = VK_NUMPAD2 then
                    if curY < 99 then
                        curY <- curY + 1
                        zoom(curX, curY, curZoom)
                if key = VK_NUMPAD0 then
                    let img,id = TakeNewScreenshot()
                    imgArray.[curX,curY] <- img
                    mapTiles.[curX,curY].Screenshots <- AAppend(mapTiles.[curX,curY].Screenshots, id)
                    let json = System.Text.Json.JsonSerializer.Serialize<MapTile>(mapTiles.[curX,curY])
                    System.IO.File.WriteAllText(MapTileFilename(curX,curY), json)
                    zoom(curX, curY, curZoom)
                if key = VK_DIVIDE then
                    let orig = mapTiles.[curX,curY].Note
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
                        System.Windows.Input.Keyboard.Focus(tb) |> ignore
                        )
                    Utils.DoModalDialog(this, sp, "Edit note", closeEv.Publish)
                    if save then
                        mapTiles.[curX,curY].Note <- tb.Text
                        let json = System.Text.Json.JsonSerializer.Serialize<MapTile>(mapTiles.[curX,curY])
                        System.IO.File.WriteAllText(MapTileFilename(curX,curY), json)
                        metadataStore.ChangeNote(GenericMetadata.Location(curZone,curX,curY), orig, tb.Text)
                        refreshMetadataKeys()
                        zoom(curX, curY, curZoom)   // redraw note preview in summary area
                if key = VK_NUMPAD7 then
                    if curZoom > 1 then
                        curZoom <- curZoom - 1
                        zoom(curX, curY, curZoom)
                if key = VK_NUMPAD9 then
                    if curZoom < MAX/2 then
                        curZoom <- curZoom + 1
                        zoom(curX, curY, curZoom)
        IntPtr.Zero
