module FeatureWindow

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media

[<AllowNullLiteral>]
type FeatureWindow(owner) as this =
    inherit Window()
    static let mutable theFeatureWindow : FeatureWindow = null
    let b = new Border(Width=1280., Height=720., BorderThickness=Thickness(0.), Background=Brushes.Olive)
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
        let fitWidth = 1280. / float (maxx-minx+1)
        //let fitHeight = 720. / float (maxy-miny+1)
        let fitHeight = 480. / float (maxy-miny+1)
        let w,h =
            let _mx,_my,mw,mh = GameSpecific.MapArea
            if fitWidth / float mw * float mh > fitHeight then
                int(fitHeight / float mh * float mw), int(fitHeight)
            else
                int(fitWidth), int(fitWidth / float mw * float mh)
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
        let c = new Canvas(Width=1280., Height=720.)
        let imgx, imgy = float((1280-r.Width)/2), float((480-r.Height)/2)
        Utils.canvasAdd(c, img, imgx, imgy)
        let bitmapSource = System.Windows.Media.Imaging.BitmapSource.Create(r.Width, r.Height, 96., 96., PixelFormats.Bgra32, null, backBuffer, backBufferStride)
        let mapMarkersImage = new Image(Source=bitmapSource, IsHitTestVisible=false)
        Utils.canvasAdd(c, mapMarkersImage, imgx, imgy)
        let ca = System.Windows.Media.Animation.ColorAnimation(From=Colors.Orange, To=Colors.Yellow, Duration=new Duration(System.TimeSpan.FromSeconds(0.5)), 
                                                                AutoReverse=true, RepeatBehavior=System.Windows.Media.Animation.RepeatBehavior.Forever)
        let animBrush = new SolidColorBrush()
        animBrush.BeginAnimation(SolidColorBrush.ColorProperty, ca)
        let DEL = 3.
        let highlightRect = new Shapes.Rectangle(Width=float w + 2.*DEL, Height=float h + 2.*DEL, Stroke=animBrush, StrokeThickness=2.*DEL, IsHitTestVisible=false, Opacity=0.)
        c.Children.Add(highlightRect) |> ignore
        let HH = 240.
        let WW = HH / float GameSpecific.MapAreaRectangle.Height * float GameSpecific.MapAreaRectangle.Width
        let bottom = new StackPanel(Height=HH, Orientation=Orientation.Horizontal)
        Utils.canvasAdd(c, bottom, 0., c.Height - HH)
        let bottomTB = new TextBox(IsReadOnly=true, FontSize=20., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue,
                                        FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, TextWrapping=TextWrapping.Wrap, SelectionBrush=Brushes.Orange,
                                        Width=320., Height=HH, VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(0.,0.,4.,0.))
        img.MouseMove.Add(fun ea ->
            let pos = ea.GetPosition(img)
            let di, dj = int(pos.X / float w), int(pos.Y / float h)
            highlightRect.Opacity <- 1.0
            Canvas.SetLeft(highlightRect, imgx+float(di*w)-DEL)
            Canvas.SetTop(highlightRect, imgy+float(dj*h)-DEL)
            let i,j = di+minx, dj+miny
            bottom.Children.Clear()
            bottom.Children.Add(bottomTB) |> ignore
            bottomTB.Text <- zm.MapTiles.[i,j].Note
            let fit(bmp) =
                let i = bmp |> Utils.BMPtoImage
                i.Height <- HH
                i.Width <- WW
                i.Stretch <- Stretch.Uniform
                i.Margin <- Thickness(0.,0.,4.,0.)
                i
            if mapBmps.[i,j] <> null then
                bottom.Children.Add(mapBmps.[i,j] |> fit) |> ignore
            for loc,bmp in linkages.[i,j] do
                let TH = 24.
                let HH = HH - TH
                let WW = HH / float GameSpecific.MapAreaRectangle.Height * float GameSpecific.MapAreaRectangle.Width
                let dp = new DockPanel(LastChildFill=true, Height=HH, Width=WW)
                let locDesc = new TextBox(IsReadOnly=true, FontSize=20., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue,
                                                FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, TextWrapping=TextWrapping.Wrap, SelectionBrush=Brushes.Orange,
                                                Height=TH, VerticalScrollBarVisibility=ScrollBarVisibility.Disabled)
                locDesc.Text <- sprintf "(%s,%d,%d)" BackingStoreData.theGame.ZoneNames.[loc.Zone] loc.X loc.Y
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
            bottom.Children.Clear()
            )
        EnsureFeature(owner, c)
