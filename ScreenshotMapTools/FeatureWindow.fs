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
    let mutable rightClickBehavior = fun() -> ()
    do
        this.Owner <- owner
        this.Title <- "FEATURE"
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Content <- b
        this.Loaded.Add(fun _ ->
            theFeatureWindow <- this
            b.MouseDown.Add(fun ea ->
                ea.Handled <- true
                if ea.RightButton = System.Windows.Input.MouseButtonState.Pressed then
                    rightClickBehavior()
                )
            )
        this.Closed.Add(fun _ ->
            theFeatureWindow <- null
            )
    member this.SetContent(c) = b.Child <- c
    member this.SetRightClickBehavior(f) = rightClickBehavior <- f
    static member TheFeatureWindow with get() = theFeatureWindow and set(x) = theFeatureWindow <- x

let EnsureFeature(rootOwner, content, screenshotFilename) =
    if FeatureWindow.TheFeatureWindow=null then
        FeatureWindow.TheFeatureWindow <- new FeatureWindow(rootOwner)
    FeatureWindow.TheFeatureWindow.SetContent(content)
    if screenshotFilename <> null then
        FeatureWindow.TheFeatureWindow.SetRightClickBehavior(fun() ->
            let fullpath = 
                if System.IO.Path.IsPathRooted(screenshotFilename) then
                    screenshotFilename
                else
                    let rootDir = System.IO.Path.GetDirectoryName(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName)
                    System.IO.Path.Combine(rootDir, screenshotFilename)
            printfn "clipboard %s" fullpath
            // copy file to clipboard, so that it can be pasted into a windows folder
            let obj = new DataObject()
            let coll = new System.Collections.Specialized.StringCollection()
            coll.AddRange([|fullpath|])
            obj.SetFileDropList(coll)
            let strm = new System.IO.MemoryStream()
            strm.WriteByte((byte)DragDropEffects.Copy)
            obj.SetData("Preferred Dropeffect", strm)
            Clipboard.SetDataObject(obj)
            )
    //FeatureWindow.TheFeatureWindow.ShowDialog() |> ignore
    FeatureWindow.TheFeatureWindow.Show() |> ignore
    FeatureWindow.TheFeatureWindow.Activate() |> ignore

type GridRange(iminx,iminy,imaxx,imaxy) =
    let mutable minx,miny,maxx,maxy = iminx,iminy,imaxx,imaxy
    member this.MinX with get() = minx and set(z) = minx <- z
    member this.MinY with get() = miny and set(z) = miny <- z
    member this.MaxX with get() = maxx and set(z) = maxx <- z
    member this.MaxY with get() = maxy and set(z) = maxy <- z
    member this.Width = this.MaxX-this.MinX+1
    member this.Height = this.MaxY-this.MinY+1
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
    EnsureFeature(owner, c, null)

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
    EnsureFeature(owner, c, null)

///////////////////////////////////////////////////////////

let DrawMapIconsToBitmapSource(gr : GridRange, totalW, totalH) =
    let zm = ZoneMemory.Get(BackingStoreData.theGame.CurZone)
    let backBuffer, backBufferStride = Array.zeroCreate (totalW*totalH*4), totalW*4
    let W,H = float totalW / float gr.Width, float totalH / float gr.Height
    let draw(i,j,key) =
        let xoff,yoff = int(float i * W), int(float j * H)
        let W,H = int(W),int(H)
        let bytes = MapIcons.mapMarkerCaches.[key].Get(W,H)
        let stride = W*4
        Utils.CopyBGRARegionOnlyPartsWithAlpha(backBuffer, backBufferStride, xoff, yoff, bytes, stride, 0, 0, W, H)
    if not(MapIcons.allIconsDisabledCheckbox.IsChecked.Value) then
        do
            // TODO this probably doesn't refresh with text updates to tiles, would need to un-click&re-click the icon
            if not(System.String.IsNullOrWhiteSpace(MapIcons.userRegex)) && MapIcons.keyDrawFuncs.[MapIcons.REGEX_DUMMY].IsSome then
                let re = new System.Text.RegularExpressions.Regex(MapIcons.userRegex)
                for i = gr.MinX to gr.MaxX do
                    for j = gr.MinY to gr.MaxY do
                        if i>=0 && i<MAX && j>=0 && j<MAX then
                            let note = zm.MapTiles.[i,j].Note
                            if note <> null && re.IsMatch(note) then
                                draw(i-gr.MinX,j-gr.MinY,MapIcons.REGEX_DUMMY)
        let keys = InMemoryStore.metadataStore.AllKeys() |> Array.sort
        for k in keys do
            let locs = metadataStore.LocationsForKey(k)
            for i = gr.MinX to gr.MaxX do
                for j = gr.MinY to gr.MaxY do
                    if i>=0 && i<MAX && j>=0 && j<MAX then
                        let loc = GenericMetadata.Location(BackingStoreData.theGame.CurZone,i,j)
                        if locs.Contains(loc) then
                            match MapIcons.keyDrawFuncs.[k] with
                            | Some _ -> draw(i-gr.MinX,j-gr.MinY,k)
                            | _ -> ()
    let bitmapSource = System.Windows.Media.Imaging.BitmapSource.Create(totalW, totalH, 96., 96., PixelFormats.Bgra32, null, backBuffer, backBufferStride)
    bitmapSource

///////////////////////////////////////////////////////////

open GameSpecific
open BackingStoreData

let AssembleBmpGrid(bmpcis:(System.Drawing.Bitmap*int)[,], gameProjection) =
    let mx,my,mw,mh = gameProjection
    let bmpw,bmph = mw*bmpcis.GetLength(0), mh*bmpcis.GetLength(1)
    let mutable scale = 1   // how much to scale down each image
    let MAX = 20000         // Bitmaps fail if > 2GB, which is little larger than 23k x 23k x 4
    while bmpw/scale > MAX || bmph/scale > MAX do
        scale <- scale * 2
    let r = new System.Drawing.Bitmap(bmpw/scale, bmph/scale)
    let rData = r.LockBits(System.Drawing.Rectangle(0,0,r.Width,r.Height), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
    for i = 0 to bmpcis.GetLength(0)-1 do
        for j = 0 to bmpcis.GetLength(1)-1 do
            let bmp,ci = bmpcis.[i,j]
            if bmp <> null then
                let bmp = bmp.Clone(System.Drawing.Rectangle(mx,my,mw,mh), System.Drawing.Imaging.PixelFormat.Format32bppArgb)  // project out the map area
                let bmp, mw, mh =
                    if scale=1 then
                        bmp, mw, mh
                    else
                        Utils.CloneScaledDownSize(bmp, scale), mw/scale, mh/scale
                if ci = -1 then        // ci is color index for kinds (e.g. MasterKey interiors)
                    // no color change, just Blit the data copy efficiently...
                    Utils.Blit(bmp, rData, mw*i, mh*j)
                    // ... and then add the grid lines 
                    for x = 0 to mw-1 do
                        Utils.SetColorFromLockedFormat32BppArgb(mw*i + x, mh*j + (mh-1), rData, System.Drawing.Color.Gray)
                    for y = 0 to mh-1 do
                        Utils.SetColorFromLockedFormat32BppArgb(mw*i + (mw-1), mh*j + y, rData, System.Drawing.Color.Gray)
                else
                    // go pixel by pixel to do the color change
                    let tf = 
                        (fun (a,r,g,b) ->
                            let c = Utils.DistinctColors.[ci % Utils.DistinctColors.Length]
                            let P = 0.8
                            let f(x,y) = float x * P + float y * (1.0-P) |> byte
                            f(a, c.A), f(r, c.R), f(g, c.G), f(b, c.B)
                        )
                    let data = bmp.LockBits(System.Drawing.Rectangle(0,0,bmp.Width,bmp.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                    for x = 0 to mw-1 do
                        for y = 0 to mh-1 do
                            if x=mw-1 || y=mh-1 then        // grid lines
                                Utils.SetColorFromLockedFormat32BppArgb(mw*i + x, mh*j + y,rData, System.Drawing.Color.Gray)
                            else
                                Utils.SetAndGetAndTransformColorFromLockedFormat32BppArgb(mw*i + x, mh*j + y, rData, mx+x, my+y, data, tf)
                    bmp.UnlockBits(data)
    r.UnlockBits(rData)
    r

let DoFullMapPanZoomFeatureWindow(app:Window, ctrl) =
    let zm = ZoneMemory.Get(theGame.CurZone)
    let bmps = Array2D.create MAX MAX (null, -1)
    let gr = GridRange(MAX,MAX,0,0)
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            let bmp = zm.FullImgArray.GetCopyOfBmp(i,j)
            if bmp <> null then
                bmps.[i,j] <- bmp, -1
                gr.Extend(i,j)
    if gr.MaxX >= gr.MinX then // there was at least one screenshot
        let bmp = AssembleBmpGrid(bmps.[gr.MinX .. gr.MaxX, gr.MinY .. gr.MaxY], TheChosenGame.MapArea)
        let bmp =
            if ctrl then
                let bmp = new System.Drawing.Bitmap(bmp, System.Drawing.Size(bmp.Width/2, bmp.Height/2))
                Utils.TileReplicateBitmapEfficiently(bmp, 2)
            else
                bmp
        let img = Utils.BMPtoImage(bmp)
        let scale = new ScaleTransform()
        let trans = new TranslateTransform()
        let tg = new TransformGroup()
        tg.Children <- new TransformCollection([|scale :> Transform; trans :> Transform|])
        img.RenderTransform <- tg
        // fit to screen to start
        let usedW, usedH = 
            if float bmp.Width / float bmp.Height > float FEATUREW / float FEATUREH then
                img.Width <- float FEATUREW
                img.Height <- System.Double.NaN
                img.Width, float bmp.Height * img.Width / float bmp.Width
            else
                img.Width <- System.Double.NaN
                img.Height <- float FEATUREH
                float bmp.Width * img.Height / float bmp.Height, img.Height
        let b = new Border(ClipToBounds=true, Background=Brushes.DarkMagenta, Width=float FEATUREW, Height=float FEATUREH, Child=img)
        // mouse controls to zoom and pan
        let mutable startPanPoint, originPanOffset = Point(),Point()
        b.MouseWheel.Add(fun ea ->
            let mousePos = ea.GetPosition(img)
            let FACTOR = 1.15
            let zoomFactor = if ea.Delta > 0 then FACTOR else 1. / FACTOR
            let newScaleX = scale.ScaleX * zoomFactor
            if false then // (newScaleX < 0.05 || newScaleX > 20.) then
                () // do nothing (clamp to these limits)
            else
                // Calculate new translation adjustments to anchor zoom on the mouse cursor
                let newScaleY = scale.ScaleY * zoomFactor
                trans.X <- trans.X - (mousePos.X * (zoomFactor - 1.) * scale.ScaleX)
                trans.Y <- trans.Y - (mousePos.Y * (zoomFactor - 1.) * scale.ScaleY)
                scale.ScaleX <- newScaleX
                scale.ScaleY <- newScaleY
            )
        b.MouseLeftButtonDown.Add(fun ea ->
            // Track start coordinates of the cursor and current transform offsets
            startPanPoint <- ea.GetPosition(b)
            originPanOffset <- new Point(trans.X, trans.Y)
            // Capture the mouse to continue tracking movement even outside the window
            b.CaptureMouse() |> ignore
            )
        b.MouseMove.Add(fun ea ->
            // Only pan if the mouse capture is locked to our element
            if b.IsMouseCaptured then
                let delta = ea.GetPosition(b) - startPanPoint
                trans.X <- originPanOffset.X + delta.X
                trans.Y <- originPanOffset.Y + delta.Y
            )
        b.MouseLeftButtonUp.Add(fun _ea ->
            b.ReleaseMouseCapture()
            )
        let c = new Canvas(Width=b.Width, Height=b.Height)
        Utils.canvasAdd(c, b, 0, 0)
        if not ctrl then
            let mapMarkersImage = new Image(Width=b.Width, Height=b.Height, IsHitTestVisible=false)
            mapMarkersImage.RenderTransform <- tg
            mapMarkersImage.Source <- DrawMapIconsToBitmapSource(gr, int usedW, int usedH)
            Utils.canvasAdd(c, mapMarkersImage, 0, 0)
        EnsureFeature(app.Owner, c, null)

let DoFeatureButton(app:Window, app_APP_WIDTH:float) =
    let featureZoneButton = new Button(Content="This Zone\nw/ Details")
    let featureZone() = MakeFeatureMap(app.Owner, Array2D.init 1 1 (fun _ _ -> Some(ZoneMemory.Get(theGame.CurZone))))
(*
            // This is code to feature multiple zones at once, which doesn't seem too useful
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
*)
    let dualFeatureButton = new Button(Content="Two Zones\nat once")
    let dualFeature() = 
        let W = 220
        let diag = Utils.makeGrid(2,3,W,20)
        Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="Left Zone"), 0, 0)
        let lInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="0", CaretIndex=1)
        Utils.gridAdd(diag, lInput, 1, 0)
        Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="Right Zone"), 0, 1)
        let rInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="1", CaretIndex=1)
        Utils.gridAdd(diag, rInput, 1, 1)
        Utils.gridAdd(diag, new TextBlock(FontSize=12., Text="minx,miny,maxx,maxy"), 0, 2)
        let boundsInput = new TextBox(FontSize=12., Width=float W, IsReadOnly=false, Text="", CaretIndex=0)
        Utils.gridAdd(diag, boundsInput, 1, 2)
        let closeEv = new Event<unit>()
        lInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then rInput.Focus() |> ignore)
        rInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then 
                                        boundsInput.Focus() |> ignore
                                        try
                                            let gr1 = ComputeRange(ZoneMemory.Get(int lInput.Text))
                                            let gr2 = ComputeRange(ZoneMemory.Get(int rInput.Text))
                                            boundsInput.Text <- sprintf "%d,%d,%d,%d" (min gr1.MinX gr2.MinX) (min gr1.MinY gr2.MinY) (max gr1.MaxX gr2.MaxX) (max gr1.MaxY gr2.MaxY)
                                            boundsInput.CaretIndex <- boundsInput.Text.Length
                                        with _ -> ()
                                        )
        boundsInput.KeyUp.Add(fun ke -> if ke.Key = Input.Key.Enter then closeEv.Trigger())
        Utils.DoModalDialogCore(app, diag, "Select two zones to feature side-by-side", closeEv.Publish, (fun () -> lInput.Focus() |> ignore))
        // TODO better validate
        try
            let l = lInput.Text |> int
            let r = rInput.Text |> int
            let [|a;b;c;d|] = boundsInput.Text.Split([|','|], System.StringSplitOptions.None) |> Array.map (fun s -> int s)
            MakeDualFeatureMap(app, ZoneMemory.Get(l), ZoneMemory.Get(r), GridRange(a,b,c,d))
        with e ->
            System.Console.Beep()
            printfn "FEATURE error: %s" (e.ToString())
    let zoomable1x1Button = new Button(Content="Zoom & Pan\nZone Map")
    let zoomable1x1() = DoFullMapPanZoomFeatureWindow(app,false)
    let zoomable2x2Button = new Button(Content="Zoom & Pan\nZone Map(2x2)")
    let zoomable2x2() = DoFullMapPanZoomFeatureWindow(app,true)
    let mkTxt(txt) = new TextBlock(FontSize=16., Text=txt, Foreground=Brushes.Black, TextWrapping=TextWrapping.Wrap, 
                                        Margin=Thickness(3., 0., 3., 0.), VerticalAlignment=VerticalAlignment.Center)
    let sp = new StackPanel(Orientation=Orientation.Vertical, Width=app_APP_WIDTH-16., Background=Brushes.White)
    sp.Children.Add(mkTxt("""The Feature Window is a large window that presents one of a few different useful projections of your screenshots and Notes.

Choose one of these options:""")) |> ignore
    let desc1 = """This one shows the current zone's map grid, where mousing each cell shows a large screenshot, the Notes, and any hyperlinked cells."""
    let desc2 = """This one shows two zones side by side; mousing either cell grid shows large screenshots from both zones at the same coordinates at once."""
    let desc3 = """This one displays a fullsize map of the current zone, which you can pan and zoom (using left-click-drag and mouse-scroll-wheel)."""
    let desc4 = """This one is like the previous one, only the map is replicated four times in a 2x2 grid (so you can see across world-wrap-edges, for games whose maps wrap around on a topological torus)."""
    let border(e) = new Border(BorderBrush=Brushes.Black, BorderThickness=Thickness(1.0), Child=e)
    let g = new Grid()
    g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(104.)))
    g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(app_APP_WIDTH-120.)))
    for _i = 1 to 4 do g.RowDefinitions.Add(new RowDefinition())
    let mutable row = 0
    let closeEv = new Event<_>()
    let data = [|
        zoomable1x1Button, desc3, zoomable1x1
        zoomable2x2Button, desc4, zoomable2x2
        featureZoneButton, desc1, featureZone
        dualFeatureButton, desc2, dualFeature
        |]
    for b,desc,f in data do
        b.Margin <- Thickness(6.)
        b.Click.Add(fun _ -> closeEv.Trigger(); f())
        Utils.gridAdd(g, border(b), 0, row)
        Utils.gridAdd(g, border(mkTxt(desc)), 1, row)
        row <- row + 1
    sp.Children.Add(Utils.centerWithGrid(g)) |> ignore
    sp.Children.Add(mkTxt("""Try them out!

You can also Feature a single cell of the map grid by right-clicking on its cell in the main app window.

(Note: The Feature Window always has the window title "FEATURE", making it easy to target with OBS.)""")) |> ignore
    let border = new Border(Child=sp, Padding=Thickness(4.), BorderThickness=Thickness(1.), BorderBrush=Brushes.Black)
    Utils.DoModalDialog(app, border, "Choose a Feature Window", closeEv.Publish)
