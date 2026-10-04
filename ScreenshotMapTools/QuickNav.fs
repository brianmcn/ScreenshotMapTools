module QuickNav

open System.Windows
open System.Windows.Controls
open System.Windows.Media
open BackingStoreData
open InMemoryStore

type SavedViewportSettings(theGame:Game) =
    let centerX = theGame.CenterX
    let centerY = theGame.CenterY
    let curX = theGame.CurX
    let curY = theGame.CurY
    let curZoom = theGame.CurZoom
    member this.Restore() =
        if curX = theGame.CurX && curY = theGame.CurY then  this.RestoreAll()       // they didn't move
        else                                                this.RestoreZoom()      // they moved
    member private this.RestoreAll() =
        theGame.CenterX <- centerX
        theGame.CenterY <- centerY
        theGame.CurX <- curX
        theGame.CurY <- curY
        theGame.CurZoom <- curZoom
    member private this.RestoreZoom() =
        theGame.CurZoom <- curZoom

let mutable recentEncompassingGridRange = null
let MakeTheGameViewportEncompassAll(kbdX, kbdY) =
    let zm = ZoneMemory.Get(theGame.CurZone)
    let gr = FeatureWindow.GridRange(MAX,MAX,0,0)
    gr.Extend(kbdX,kbdY)
    for i = 0 to MAX-1 do
        for j = 0 to MAX-1 do
            let bmp = zm.FullImgArray.GetCopyOfBmp(i,j)
            if bmp <> null then
                gr.Extend(i,j)
    let zoomLevel = 1 + ((max gr.Width gr.Height)+1)/2      // TODO does not take into account aspect ratio, height might not fit onscreen in rare cases
    let centerX = gr.MinX + gr.Width/2
    let centerY = gr.MinY + gr.Height/2
    theGame.CurZoom <- zoomLevel
    theGame.CenterX <- centerX
    theGame.CenterY <- centerY
    recentEncompassingGridRange <- gr

let MoveLeftRight(dx) =
    if dx <> 1 && dx <> -1 then failwith "bad MoveLeftRight call"
    let mutable targetCol = theGame.CurX + dx
    let mutable found = false
    while not(found) && targetCol <> theGame.CurX do
        // wrap
        if targetCol > recentEncompassingGridRange.MaxX then
            targetCol <- recentEncompassingGridRange.MinX
        elif targetCol < recentEncompassingGridRange.MinX then
            targetCol <- recentEncompassingGridRange.MaxX
        else
            // look for a target in this column
            let i = targetCol
            let founds = ResizeArray()
            for j = recentEncompassingGridRange.MinY to recentEncompassingGridRange.MaxY do
                let loc = GenericMetadata.Location(theGame.CurZone,i,j)
                for ht in theGame.HashtagTargetsForQuickNav do
                    let keyedLocations = metadataStore.LocationsForKey(ht)
                    if keyedLocations.Contains(loc) then
                        founds.Add(struct(i,j))
            if founds.Count = 0 then
                targetCol <- targetCol + dx     // keep looking farther out
            else
                // 1 or more in this column, find closest dy, they can arrow up/down for others
                let founds = founds.ToArray()
                founds |> Array.sortInPlaceBy(fun (struct(_i,j)) -> abs(j-theGame.CurY))
                let struct(i,j) = founds.[0]
                theGame.CurX <- i
                theGame.CurY <- j
                found <- true
    // either found is true, or we cycled around all other columns without finding one
    found

let MoveUpDown(dy) =
    if dy <> 1 && dy <> -1 then failwith "bad MoveUpDown call"
    let mutable targetRow = theGame.CurY + dy
    let mutable found = false
    while not(found) && targetRow <> theGame.CurY do
        // wrap
        if targetRow > recentEncompassingGridRange.MaxY then
            targetRow <- recentEncompassingGridRange.MinY
        elif targetRow < recentEncompassingGridRange.MinY then
            targetRow <- recentEncompassingGridRange.MaxY
        else
            // look for a target in this row
            let j = targetRow
            let founds = ResizeArray()
            for i = recentEncompassingGridRange.MinX to recentEncompassingGridRange.MaxX do
                let loc = GenericMetadata.Location(theGame.CurZone,i,j)
                for ht in theGame.HashtagTargetsForQuickNav do
                    let keyedLocations = metadataStore.LocationsForKey(ht)
                    if keyedLocations.Contains(loc) then
                        founds.Add(struct(i,j))
            if founds.Count = 0 then
                targetRow <- targetRow + dy     // keep looking farther out
            else
                // 1 or more in this column, find closest dx, they can arrow left/right for others
                let founds = founds.ToArray()
                founds |> Array.sortInPlaceBy(fun (struct(i,_j)) -> abs(i-theGame.CurX))
                let struct(i,j) = founds.[0]
                theGame.CurX <- i
                theGame.CurY <- j
                found <- true
    // either found is true, or we cycled around all other rows without finding one
    found

let MakeInstructionsPane(parentWindow,appWidth,w,h) =
    let mkTxt(txt,bt) = new TextBox(IsHitTestVisible=false, FontSize=16., FontWeight=FontWeights.Bold, Text=txt, Foreground=Brushes.Black, Background=Brushes.Transparent, 
                                    BorderBrush=Brushes.Black, BorderThickness=Thickness(float bt))
    let g = new Grid()
    g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(50.)))
    g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength.Auto))
    let data = [|
            2, "5",            "end Quick\nNav Mode"
            1, "9 *",          "cycle zone +1"
            1, "7",            "cycle zone -1"
            2, ".",            "follow first\nhyperlink"
            3, "8 \n4 6\n2 ",  "move to\nnext hashtag\ntarget"
        |]
    let COUNT = data.Length
    for i = 0 to COUNT-1 do
        let n,a,b = data.[i]
        g.RowDefinitions.Add(new RowDefinition(Height=GridLength((float n) * 24.)))
        let at = mkTxt(a,1)
        at.TextAlignment <- TextAlignment.Right
        at.Padding <- Thickness(0.,0.,8.,0.)
        if at.Text="." then
            at.FontSize <- at.FontSize + 4.0
            at.Margin <- Thickness(0., -2., 0., 0.)
        Utils.gridAdd(g, at, 0, i)
        Utils.gridAdd(g, mkTxt(b,1), 1, i)
    let sp = new StackPanel(Orientation=Orientation.Vertical, Background=Brushes.LightSteelBlue, Visibility=Visibility.Hidden, Width=w, Height=h)
    sp.Children.Add(mkTxt("--QuickNav Mode--",1)) |> ignore
    sp.Children.Add(mkTxt("QuickNav Mode\nchanges NumPad\nhotkeys:",0)) |> ignore
    g.Margin <- Thickness(0.,10.,0.,10.)
    sp.Children.Add(g) |> ignore
    sp.Children.Add(mkTxt("Arrows 2468 go\nto cells with\nhashtag targets:",0)) |> ignore
    let b = new Button(Content="change\nhashtag\ntargets", Margin=Thickness(6.))
    b.Click.Add(fun _ ->
        let orig = System.String.Join(",", theGame.HashtagTargetsForQuickNav)
        let extra = "Type in a comma-separated list of hashtags, without octothorpes (no '#')" 
                        + "\nCells whose Notes contain those hashtags will be legal targets for QuickNav"
                        + "\nExample:"
                        + "\nsave,fastTravel"
        let save, r = Utils.DoBasicModalTextDialogCore(parentWindow, "Hashtags for QuickNav", extra, orig, appWidth, 500., false)
        if save then
            let a = r.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
            let mutable error = null
            for ht in a do
                let ht = if ht.StartsWith("#") && ht.Length > 1 then ht.Substring(1) else ht
                for c in ht do
                    if not(GenericMetadata.IsHashtagChar(c)) then
                        error <- sprintf "'%s' is not a legal hashtag value (alphanumerics only)" ht
            if error <> null then
                MessageBox.Show(parentWindow, error) |> ignore
            else
                theGame.HashtagTargetsForQuickNav <- a
                theGame.Save()
                MapIcons.redrawMapIconHoverOnly.Trigger()   // refresh the overlay
        )
    sp.Children.Add(b) |> ignore
    sp
    
open System.Windows.Media.Animation
let StartHashtagTargetsAnimation(window:Window, elementToAnimate:Image, storyboard:Storyboard) =
    let pulseAnimation = new DoubleAnimation(From=1.0, To=0.2, Duration=System.TimeSpan.FromSeconds(0.3), AutoReverse=true, RepeatBehavior=RepeatBehavior.Forever)
    Storyboard.SetTarget(pulseAnimation, elementToAnimate)
    Storyboard.SetTargetProperty(pulseAnimation, new PropertyPath(UIElement.OpacityProperty))
    storyboard.Children.Add(pulseAnimation) |> ignore
    storyboard.Begin(window, true)
let StopHashtagTargetsAnimation(window:Window, elementToAnimate:Image, storyboard:Storyboard) =
    storyboard.Stop(window)
    elementToAnimate.Opacity <- 1.0