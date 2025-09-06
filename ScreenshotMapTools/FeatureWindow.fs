module FeatureWindow

open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Utils.Extensions

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
        FeatureWindow.TheFeatureWindow.SetContent(content)
        FeatureWindow.TheFeatureWindow.ShowDialog() |> ignore

type GridRange(iminx,iminy,imaxx,imaxy) =
    let mutable minx,miny,maxx,maxy = iminx,iminy,imaxx,imaxy
    member this.MinX = minx
    member this.MinY = miny
    member this.MaxX = maxx
    member this.MaxY = maxy
    member this.Extend(i,j) =
        minx <- min minx i
        miny <- min miny j
        maxx <- max maxx i
        maxy <- max maxy j

let MAX = 100
let ComputeRange(zm:InMemoryStore.ZoneMemory) =
    let r = GridRange(MAX,MAX,0,0)
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            let mt = zm.MapTiles.[i,j]
            (*
            let matches = mt.ScreenshotsWithKinds |> Array.filter (fun swk -> swk.Kinds |> Array.contains("main") |> not)
            // any
            if matches.Length > 0 || zm.FullImgArray.[i,j] <> null then
            *)
            if not mt.IsEmpty then
                r.Extend(i,j)
    r

let DEL = 3.
let animBrush = 
    let r = new SolidColorBrush()
    let ca = System.Windows.Media.Animation.ColorAnimation(From=Colors.Orange, To=Colors.Yellow, Duration=new Duration(System.TimeSpan.FromSeconds(0.5)), 
                                                            AutoReverse=true, RepeatBehavior=System.Windows.Media.Animation.RepeatBehavior.Forever)
    r.BeginAnimation(SolidColorBrush.ColorProperty, ca)
    r
let makeHighlightRect() = 
    new Shapes.Rectangle(Stroke=animBrush, StrokeThickness=2.*DEL, IsHitTestVisible=false, Opacity=0.)


open InMemoryStore
let DrawCore(zm:ZoneMemory, gr:GridRange, eachWidth, eachHeight, gameMapAspect, margin, onHover:Event<_>, onLeave:Event<_>, iconForNonMains) =
    let mapBmps = Array2D.zeroCreate MAX MAX 
    let nonMainBmps = Array2D.zeroCreate MAX MAX
    let linkages = Array2D.init MAX MAX (fun _ _ -> ResizeArray())
    for i = gr.MinX to gr.MaxX do
        for j = gr.MinY to gr.MaxY do
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
            for loc,_ in GenericMetadata.FindAllLinkages(mt.Note, zm.Zone, i, j) do
                let lm = ZoneMemory.Get(loc.Zone)
                let bmp = lm.MapImgArray.GetCopyOfBmp(loc.X,loc.Y)
                if bmp <> null then  // only populate linkages when we have a screenshot on the other end to preview
                    linkages.[i,j].Add( (loc,bmp) )
    let LABELH = 18
    let w,h =
        let fitWidth = (float eachWidth-margin) / float (gr.MaxX-gr.MinX+1)
        let fitHeight = (float eachHeight-margin-float LABELH) / float (gr.MaxY-gr.MinY+1)
        if fitWidth / gameMapAspect > fitHeight then
            int(fitHeight * gameMapAspect), int(fitHeight)
        else
            int(fitWidth), int(fitWidth / gameMapAspect)
    let r = new System.Drawing.Bitmap(w*(gr.MaxX-gr.MinX+1), h*(gr.MaxY-gr.MinY+1))
    let rData = r.LockBits(System.Drawing.Rectangle(0,0,r.Width,r.Height), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
    let backBuffer, backBufferStride = Array.zeroCreate (r.Width*r.Height*4), r.Width*4   // for drawing overlay icons
    for i = gr.MinX to gr.MaxX do
        for j = gr.MinY to gr.MaxY do
            let bmp = mapBmps.[i,j]
            if bmp <> null then
                let bmp = new System.Drawing.Bitmap(bmp, System.Drawing.Size(w,h))
                let data = bmp.LockBits(System.Drawing.Rectangle(0,0,bmp.Width,bmp.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                for x = 0 to w-1 do
                    for y = 0 to h-1 do
                        if x=w-1 || y=h-1 then
                            Utils.SetColorFromLockedFormat32BppArgb(w*(i-gr.MinX) + x, h*(j-gr.MinY) + y,rData, System.Drawing.Color.Gray)
                        else
                            Utils.SetAndGetColorFromLockedFormat32BppArgb(w*(i-gr.MinX) + x, h*(j-gr.MinY) + y, rData, x, y, data)
                bmp.UnlockBits(data)
            elif not(zm.MapTiles.[i,j].IsEmpty) then
                for x = 0 to w-1 do
                    for y = 0 to h-1 do
                        if x=w-1 || y=h-1 then
                            Utils.SetColorFromLockedFormat32BppArgb(w*(i-gr.MinX) + x, h*(j-gr.MinY) + y,rData, System.Drawing.Color.Gray)
                        else
                            Utils.SetColorFromLockedFormat32BppArgb(w*(i-gr.MinX) + x, h*(j-gr.MinY) + y,rData, System.Drawing.Color.CornflowerBlue)
            else
                for x = 0 to w-1 do
                    for y = 0 to h-1 do
                        if x=w-1 || y=h-1 then
                            Utils.SetColorFromLockedFormat32BppArgb(w*(i-gr.MinX) + x, h*(j-gr.MinY) + y,rData, System.Drawing.Color.Gray)
                        else
                            Utils.SetColorFromLockedFormat32BppArgb(w*(i-gr.MinX) + x, h*(j-gr.MinY) + y,rData, System.Drawing.Color.LightGray)
            // icons
            let draw(i,j,key) =
                let xoff,yoff = w*(i-gr.MinX), h*(j-gr.MinY)
                let bytes = MapIcons.mapMarkerCaches.[key].Get(w,h)
                let stride = w*4
                Utils.CopyBGRARegionOnlyPartsWithAlpha(backBuffer, backBufferStride, xoff, yoff, bytes, stride, 0, 0, w, h)
            if iconForNonMains && nonMainBmps.[i,j] <> null then
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
                    let loc = GenericMetadata.Location(zm.Zone,i,j)
                    if locs.Contains(loc) then
                        match MapIcons.keyDrawFuncs.[k] with
                        | Some _ -> draw(i,j,k)
                        | _ -> ()
    r.UnlockBits(rData)
    let c = new Canvas()
    let img = Utils.BMPtoImage(r)
    let imgx, imgy = float(eachWidth-r.Width)/2., float(eachHeight-LABELH-r.Height)/2.          // for centering image in the tile above the label
    Utils.canvasAdd(c, img, imgx, imgy)
    let bitmapSource = System.Windows.Media.Imaging.BitmapSource.Create(r.Width, r.Height, 96., 96., PixelFormats.Bgra32, null, backBuffer, backBufferStride)
    let mapMarkersImage = new Image(Source=bitmapSource, IsHitTestVisible=false)
    Utils.canvasAdd(c, mapMarkersImage, imgx, imgy)
    let label = new TextBox(IsReadOnly=true, FontSize=12., Text=BackingStoreData.theGame.ZoneNames.[zm.Zone], BorderThickness=Thickness(1.), 
                            Foreground=Brushes.Black, Background=Brushes.White, TextAlignment=TextAlignment.Center,
                            Width=float eachWidth-4.*margin, Height=float(LABELH))
    Utils.canvasAdd(c, label, 2.*margin, float eachHeight-float LABELH)
    let highlightRect = makeHighlightRect()
    // highlightRect must be drawn on top, so add it last
    c.Children.Add(highlightRect) |> ignore
    // eventing
    let highlight(di,dj) = // diff from upper left, that is 0,0 is upper left tile
        // highlight rect
        highlightRect.Opacity <- 1.0
        Canvas.SetLeft(highlightRect, imgx+float(di*w)-DEL)
        Canvas.SetTop(highlightRect, imgy+float(dj*h)-DEL)
        highlightRect.Width <- float w + 2.*DEL
        highlightRect.Height <- float h + 2.*DEL
    let unhighlight() =
        highlightRect.Opacity <- 0.0
    img.MouseMove.Add(fun ea ->
        let pos = ea.GetPosition(img)
        let di, dj = int(pos.X / float w), int(pos.Y / float h)
        highlight(di,dj)
        let i,j = di+gr.MinX, dj+gr.MinY
        onHover.Trigger(i,j)
        )
    img.MouseLeave.Add(fun _ ->
        unhighlight()
        onLeave.Trigger()
        )
    c, mapBmps, nonMainBmps, linkages, highlight, unhighlight

let MakeFeatureMap(owner,zma:ZoneMemory option[,]) =
    // framing layout
    let gameMapAspect = 
        let _mx,_my,mw,mh = GameSpecific.TheChosenGame.MapArea
        float mw / float mh
    let PICH = 300.                     // height of pictures on the bottom
    let PICW = PICH * gameMapAspect       // width of left strip to preserve
    let TOTALH = FEATUREH - PICH
    let TOTALW = FEATUREW - PICW
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
    // zones
    let eachWidth  = TOTALW / float(zma.GetLength(0)) |> int
    let eachHeight = TOTALH / float(zma.GetLength(1)) |> int
    for zi = 0 to zma.GetLength(0)-1 do
        for zj = 0 to zma.GetLength(1)-1 do
            // bg tile color
            let mapBGcolor = new DockPanel(Width=float eachWidth, Height=float eachHeight, Background=if (zi+zj)%2=0 then Brushes.DarkOliveGreen else Brushes.OliveDrab)
            let ulx, uly = PICW+float(zi*eachWidth), float(zj*eachHeight)   // where this zone tile begins
            Utils.canvasAdd(c, mapBGcolor, ulx, uly)
            // zone
            let zmo = zma.[zi,zj]
            match zmo with
            | None -> ()
            | Some zm ->
            let gr = ComputeRange(zm)
            if gr.MaxX >= gr.MinX then // there was at least one screenshot
                let WW = PICH / float GameSpecific.MapAreaRectangle.Height * float GameSpecific.MapAreaRectangle.Width
                let onHover, onLeave = new Event<_>(), new Event<_>()
                let imgCanvas, mapBmps, nonMainBmps, linkages, _, _ = DrawCore(zm, gr, eachWidth, eachHeight, gameMapAspect, MARGIN, onHover, onLeave, true)
                Utils.canvasAdd(c, imgCanvas, ulx, uly)
                onHover.Publish.Add(fun (i,j) ->
                    // framing updates...
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
                        let WW = HH * gameMapAspect
                        let locDesc = new TextBox(IsReadOnly=true, FontSize=20., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue,
                                                        FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, TextWrapping=TextWrapping.NoWrap, SelectionBrush=Brushes.Orange,
                                                        Height=TH, VerticalScrollBarVisibility=ScrollBarVisibility.Disabled)
                        locDesc.Text <- 
                            if loc.X = i && loc.Y = j then
                                sprintf "(%s)" BackingStoreData.theGame.ZoneNames.[loc.Zone]
                            else
                                sprintf "(%s,%d,%d)" BackingStoreData.theGame.ZoneNames.[loc.Zone] loc.X loc.Y
                        let img = bmp |> Utils.BMPtoImage
                        img.Height <- HH
                        img.Width <- WW
                        img.Stretch <- Stretch.Uniform
                        img.Margin <- Thickness(0.,0.,4.,0.)
                        let dp = (new DockPanel(LastChildFill=true, Height=HH, Width=WW)).AddBottom(locDesc).Add(img)
                        bottom.Children.Add(dp) |> ignore
                    if nonMainBmps.[i,j] <> null then
                        for bmp in nonMainBmps.[i,j] do
                            bottom.Children.Add(bmp |> fit) |> ignore
                    )
                onLeave.Publish.Add(fun () ->
                    highlightedTB.Opacity <- 0.0
                    highlightedPicture.Children.Clear()
                    bottom.Children.Clear()
                    )
    EnsureFeature(owner, c)

let MakeDualFeatureMap(owner, zm1:ZoneMemory, zm2:ZoneMemory, gr) =
    // framing layout
    let gameMapAspect = 
        let _mx,_my,mw,mh = GameSpecific.TheChosenGame.MapArea
        float mw / float mh
    let PICH = 350.                     // height of pictures on the bottom
    let PICW = PICH * gameMapAspect       // width of bottom pics
    let MARGIN = 6.
    let highlightedPicture1, highlightedPicture2 = new Canvas(), new Canvas()
    let c = new Canvas(Width=FEATUREW, Height=FEATUREH)
    Utils.canvasAdd(c, highlightedPicture1, 0., FEATUREH-PICH)
    Utils.canvasAdd(c, highlightedPicture2, FEATUREW-PICW, FEATUREH-PICH)
    let eachWidth  = 640
    let eachHeight = 360
    let a = 
        [|
            for zm in [zm1; zm2] do
                let first = obj.ReferenceEquals(zm,zm1) 
                let mapBGcolor = new DockPanel(Width=float eachWidth, Height=float eachHeight, Background=if first then Brushes.DarkOliveGreen else Brushes.OliveDrab)
                let ulx, uly = (if first then 0. else 640.), 0.  // where this zone tile begins
                Utils.canvasAdd(c, mapBGcolor, ulx, uly)
                let onHover, onLeave = new Event<_>(), new Event<_>()
                let imgCanvas, mapBmps, _nonMainBmps, _linkages, hi, unhi = DrawCore(zm, gr, eachWidth, eachHeight, gameMapAspect, MARGIN, onHover, onLeave, false)
                Utils.canvasAdd(c, imgCanvas, ulx, uly)
                yield onHover, onLeave, mapBmps, hi, unhi
        |]
    let [| oh1,ol1,mb1,hi1,unhi1; oh2,ol2,mb2,hi2,unhi2 |] = a
    let onHover(i,j) =
        let fit(bmp) =
            let i = bmp |> Utils.BMPtoImage
            i.Height <- PICH
            i.Width <- PICH * gameMapAspect
            i.Stretch <- Stretch.Uniform
            i.Margin <- Thickness(0.,0.,MARGIN,0.)
            i
        highlightedPicture1.Children.Clear()
        highlightedPicture2.Children.Clear()
        if mb1.[i,j] <> null then
            highlightedPicture1.Children.Add(mb1.[i,j] |> fit) |> ignore
        if mb2.[i,j] <> null then
            highlightedPicture2.Children.Add(mb2.[i,j] |> fit) |> ignore
        hi1(i-gr.MinX,j-gr.MinY)
        hi2(i-gr.MinX,j-gr.MinY)
    let onLeave() =
        highlightedPicture1.Children.Clear()
        highlightedPicture2.Children.Clear()
        unhi1()
        unhi2()
    oh1.Publish.Add(onHover)
    ol1.Publish.Add(onLeave)
    oh2.Publish.Add(onHover)
    ol2.Publish.Add(onLeave)
    EnsureFeature(owner, c)
