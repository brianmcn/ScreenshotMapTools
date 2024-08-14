module FeatureWindow

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media

let FEATUREW,FEATUREH = 1280.,720.
[<AllowNullLiteral>]
type FeatureWindow(owner) as this =
    inherit Window()
    static let mutable theFeatureWindow : FeatureWindow = null
    let b = new Border(Width=FEATUREW, Height=FEATUREH, BorderThickness=Thickness(0.), Background=new SolidColorBrush(Color.FromRgb(0x30uy,0x40uy,0x60uy)))
    do
        this.Owner <- owner
        this.Title <- "FEATURE"
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Content <- b
        this.Loaded.Add(fun _ ->
            theFeatureWindow <- this
            )
        this.Closed.Add(fun _ ->
            theFeatureWindow <- null
            )
    member this.SetContent(c) = b.Child <- c
    static member TheFeatureWindow with get() = theFeatureWindow and set(x) = theFeatureWindow <- x

let EnsureFeature(rootOwner, content) =
    if FeatureWindow.TheFeatureWindow=null then
        FeatureWindow.TheFeatureWindow <- new FeatureWindow(rootOwner)
        FeatureWindow.TheFeatureWindow.Show()
    FeatureWindow.TheFeatureWindow.SetContent(content)


open InMemoryStore
let MakeFeatureMap(owner,zm:ZoneMemory) =
    // framing layout
    let gameMapAspect = 
        let _mx,_my,mw,mh = GameSpecific.MapArea
        float mw / float mh
    let PICH = 300.                     // height of pictures on the bottom
    let PICW = PICH * gameMapAspect       // width of left strip to preserve
    let FH = FEATUREH - PICH
    let FW = FEATUREW - PICW
    let MARGIN = 6.
    let highlightedPicture = new Canvas()
    let c = new Canvas(Width=FEATUREW, Height=FEATUREH)
    Utils.canvasAdd(c, highlightedPicture, 0., 0.)
    let HTBS = 100.   // how much skinnier to make the text box than the pic
    let highlightedTB = new TextBox(IsReadOnly=true, FontSize=20., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue,
                                    FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, TextWrapping=TextWrapping.Wrap, SelectionBrush=Brushes.Orange,
                                    Width=PICW-HTBS-MARGIN, Height=FEATUREH-PICH-MARGIN, VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(0.,0.,4.,0.))
    Utils.canvasAdd(c, highlightedTB, 0., FEATUREH-highlightedTB.Height)
    let bottom = new StackPanel(Height=PICH, Orientation=Orientation.Horizontal)
    Utils.canvasAdd(c, bottom, PICW-HTBS, c.Height - PICH)
    let DEL = 3.
    let highlightRect = 
        let animBrush = new SolidColorBrush()
        let ca = System.Windows.Media.Animation.ColorAnimation(From=Colors.Orange, To=Colors.Yellow, Duration=new Duration(System.TimeSpan.FromSeconds(0.5)), 
                                                                AutoReverse=true, RepeatBehavior=System.Windows.Media.Animation.RepeatBehavior.Forever)
        animBrush.BeginAnimation(SolidColorBrush.ColorProperty, ca)
        new Shapes.Rectangle(Stroke=animBrush, StrokeThickness=2.*DEL, IsHitTestVisible=false, Opacity=0.)
    // zone
    let MAX = 100
    let mapBmps = Array2D.zeroCreate MAX MAX 
    let nonMainBmps = Array2D.zeroCreate MAX MAX
    let linkages = Array2D.init MAX MAX (fun _ _ -> ResizeArray())
    let mutable minx,miny,maxx,maxy = MAX,MAX,0,0
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            let mt = zm.MapTiles.[i,j]
            // nonMains
            let matches = mt.ScreenshotsWithKinds |> Array.filter (fun swk -> swk.Kinds |> Array.contains("main") |> not)
            if matches.Length > 0 then
                let bmps = matches |> Array.map (fun swk -> bmpDict.[swk.Id])
                let bmps = bmps |> Array.map (fun bmp -> Utils.cropToRect(bmp, GameSpecific.MapAreaRectangle))
                nonMainBmps.[i,j] <- bmps
            // map
            if zm.MapImgArray.[i,j] <> null then
                mapBmps.[i,j] <- zm.MapImgArray.GetCopyOfBmp(i,j)
            // linkages
            for loc in GenericMetadata.FindAllLinkages(mt.Note, zm.Zone, i, j) do
                let lm = ZoneMemory.Get(loc.Zone)
                let bmp = lm.MapImgArray.GetCopyOfBmp(loc.X,loc.Y)
                if bmp <> null then  // only populate linkages when we have a screenshot on the other end to preview
                    linkages.[i,j].Add( (loc,bmp) )
            // any
            if matches.Length > 0 || zm.FullImgArray.[i,j] <> null then
                minx <- min minx i
                miny <- min miny j
                maxx <- max maxx i
                maxy <- max maxy j
    if maxx >= minx then // there was at least one screenshot
        let fitWidth = (FW-MARGIN) / float (maxx-minx+1)
        let fitHeight = (FH-MARGIN) / float (maxy-miny+1)
        let w,h =
            if fitWidth / gameMapAspect > fitHeight then
                int(fitHeight * gameMapAspect), int(fitHeight)
            else
                int(fitWidth), int(fitWidth / gameMapAspect)
        let r = new System.Drawing.Bitmap(w*(maxx-minx+1), h*(maxy-miny+1))
        let rData = r.LockBits(System.Drawing.Rectangle(0,0,r.Width,r.Height), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let backBuffer, backBufferStride = Array.zeroCreate (r.Width*r.Height*4), r.Width*4   // for drawing overlay icons
        for i = minx to maxx do
            for j = miny to maxy do
                let bmp = mapBmps.[i,j]
                if bmp <> null then
                    let bmp = new System.Drawing.Bitmap(bmp, System.Drawing.Size(w,h))
                    let data = bmp.LockBits(System.Drawing.Rectangle(0,0,bmp.Width,bmp.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                    for x = 0 to w-1 do
                        for y = 0 to h-1 do
                            if x=w-1 || y=h-1 then
                                Utils.SetColorFromLockedFormat32BppArgb(w*(i-minx) + x, h*(j-miny) + y,rData, System.Drawing.Color.Gray)
                            else
                                Utils.SetAndGetColorFromLockedFormat32BppArgb(w*(i-minx) + x, h*(j-miny) + y, rData, x, y, data)
                    bmp.UnlockBits(data)
                elif not(zm.MapTiles.[i,j].IsEmpty) then
                    for x = 0 to w-1 do
                        for y = 0 to h-1 do
                            if x=w-1 || y=h-1 then
                                Utils.SetColorFromLockedFormat32BppArgb(w*(i-minx) + x, h*(j-miny) + y,rData, System.Drawing.Color.Gray)
                            else
                                Utils.SetColorFromLockedFormat32BppArgb(w*(i-minx) + x, h*(j-miny) + y,rData, System.Drawing.Color.CornflowerBlue)
                else
                    for x = 0 to w-1 do
                        for y = 0 to h-1 do
                            if x=w-1 || y=h-1 then
                                Utils.SetColorFromLockedFormat32BppArgb(w*(i-minx) + x, h*(j-miny) + y,rData, System.Drawing.Color.Gray)
                            else
                                Utils.SetColorFromLockedFormat32BppArgb(w*(i-minx) + x, h*(j-miny) + y,rData, System.Drawing.Color.LightGray)
                // icons
                let draw(i,j,key) =
                    let xoff,yoff = w*(i-minx), h*(j-miny)
                    let bytes = MapIcons.mapMarkerCaches.[key].Get(w,h)
                    let stride = w*4
                    Utils.CopyBGRARegionOnlyPartsWithAlpha(backBuffer, backBufferStride, xoff, yoff, bytes, stride, 0, 0, w, h)
                if nonMainBmps.[i,j] <> null then
                    draw(i,j,MapIcons.HOVER_DUMMY)
                if not(MapIcons.allIconsDisabledCheckbox.IsChecked.Value) then
                    do
                        if not(System.String.IsNullOrWhiteSpace(MapIcons.userRegex)) && MapIcons.keyDrawFuncs.[MapIcons.REGEX_DUMMY].IsSome then
                            let re = new System.Text.RegularExpressions.Regex(MapIcons.userRegex)
                            let note = zm.MapTiles.[i,j].Note
                            if note <> null && re.IsMatch(note) then
                                draw(i,j,MapIcons.REGEX_DUMMY)
                    let keys = InMemoryStore.metadataStore.AllKeys() |> Array.sort
                    for k in keys do
                        let locs = metadataStore.LocationsForKey(k)
                        let loc = GenericMetadata.Location(BackingStoreData.theGame.CurZone,i,j)
                        if locs.Contains(loc) then
                            match MapIcons.keyDrawFuncs.[k] with
                            | Some _ -> draw(i,j,k)
                            | _ -> ()
        r.UnlockBits(rData)
        let img = Utils.BMPtoImage(r)
        let mapBGcolor = new DockPanel(Width=FW, Height=FH, Background=Brushes.DarkOliveGreen)
        Utils.canvasAdd(c, mapBGcolor, FEATUREW-FW, 0.)
        let imgx, imgy = PICW + (FW-float r.Width)/2., (FH-float r.Height)/2.          // for centering image
        Utils.canvasAdd(c, img, imgx, imgy)
        let bitmapSource = System.Windows.Media.Imaging.BitmapSource.Create(r.Width, r.Height, 96., 96., PixelFormats.Bgra32, null, backBuffer, backBufferStride)
        let mapMarkersImage = new Image(Source=bitmapSource, IsHitTestVisible=false)
        Utils.canvasAdd(c, mapMarkersImage, imgx, imgy)
        let WW = PICH / float GameSpecific.MapAreaRectangle.Height * float GameSpecific.MapAreaRectangle.Width
        img.MouseMove.Add(fun ea ->
            let pos = ea.GetPosition(img)
            let di, dj = int(pos.X / float w), int(pos.Y / float h)
            // highlight rect
            highlightRect.Opacity <- 1.0
            Canvas.SetLeft(highlightRect, imgx+float(di*w)-DEL)
            Canvas.SetTop(highlightRect, imgy+float(dj*h)-DEL)
            highlightRect.Width <- float w + 2.*DEL
            highlightRect.Height <- float h + 2.*DEL
            // framing updates...
            let i,j = di+minx, dj+miny
            // ... note
            highlightedTB.Text <- zm.MapTiles.[i,j].Note
            highlightedTB.Opacity <- 1.0
            // ... big picture
            let fit(bmp) =
                let i = bmp |> Utils.BMPtoImage
                i.Height <- PICH
                i.Width <- WW
                i.Stretch <- Stretch.Uniform
                i.Margin <- Thickness(0.,0.,MARGIN,0.)
                i
            highlightedPicture.Children.Clear()
            if mapBmps.[i,j] <> null then
                highlightedPicture.Children.Add(mapBmps.[i,j] |> fit) |> ignore
            // ... bottom strip images
            bottom.Children.Clear()
            for loc,bmp in linkages.[i,j] do
                let TH = 24.
                let HH = PICH - TH
                let WW = HH / float GameSpecific.MapAreaRectangle.Height * float GameSpecific.MapAreaRectangle.Width
                let dp = new DockPanel(LastChildFill=true, Height=HH, Width=WW)
                let locDesc = new TextBox(IsReadOnly=true, FontSize=20., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue,
                                                FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, TextWrapping=TextWrapping.NoWrap, SelectionBrush=Brushes.Orange,
                                                Height=TH, VerticalScrollBarVisibility=ScrollBarVisibility.Disabled)
                locDesc.Text <- 
                    if loc.X = i && loc.Y = j then
                        sprintf "(%s)" BackingStoreData.theGame.ZoneNames.[loc.Zone]
                    else
                        sprintf "(%s,%d,%d)" BackingStoreData.theGame.ZoneNames.[loc.Zone] loc.X loc.Y
                DockPanel.SetDock(locDesc, Dock.Bottom)
                dp.Children.Add(locDesc) |> ignore
                let img = bmp |> Utils.BMPtoImage
                img.Height <- HH
                img.Width <- WW
                img.Stretch <- Stretch.Uniform
                img.Margin <- Thickness(0.,0.,4.,0.)
                dp.Children.Add(img) |> ignore
                bottom.Children.Add(dp) |> ignore
            if nonMainBmps.[i,j] <> null then
                for bmp in nonMainBmps.[i,j] do
                    bottom.Children.Add(bmp |> fit) |> ignore
            )
        img.MouseLeave.Add(fun _ ->
            highlightRect.Opacity <- 0.0
            highlightedTB.Opacity <- 0.0
            highlightedPicture.Children.Clear()
            bottom.Children.Clear()
            )
    // highlightRect must be drawn on top, so add it last
    c.Children.Add(highlightRect) |> ignore
    EnsureFeature(owner, c)
