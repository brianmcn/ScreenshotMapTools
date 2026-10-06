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

type QuickNav() =
    let mutable recentEncompassingGridRange = null
    let mutable updateInstructionPane = fun() -> ()
    let mutable whichHashtagTarget = 0        // index into theGame.HashtagTargetsForQuickNav that is currently active, where theGame.HashtagTargetsForQuickNav.Length is valid meaning 'any cell'
    let mutable currentTargets = ResizeArray()      // is empty if 'any cell' or if no targets for the current hashtag
    let mutable hChain = null
    let mutable vChain = null
    let MoveCore(d, cursor, chain:_[]) =
        if d <> 1 && d <> -1 then failwith "bad MoveCore call"
        let mutable i = 0
        while i < chain.Length && chain.[i] < cursor do
            i <- i + 1
        let r = 
            if d = 1 then  // Right/Down
                if i = chain.Length || ((i = chain.Length-1) && (chain.[i] = cursor)) then
                    chain.[0]
                elif chain.[i] = cursor then
                    chain.[i+1]
                else
                    chain.[i]
            else    // Left/Up
                if i = 0 then
                    chain.[chain.Length-1]
                elif chain.[i] = cursor then
                    chain.[i-1]
                else
                    chain.[i]
        r
    member this.IsValidByVirtueOfEmptyTargetList() = currentTargets.Count=0
    member this.IsValidByVirtueOfMatchingHashtagTarget(i,j) = currentTargets.Contains(struct(i,j))
    member this.CurrentlyTargetedHashtag() = if whichHashtagTarget = theGame.HashtagTargetsForQuickNav.Length then null else theGame.HashtagTargetsForQuickNav.[whichHashtagTarget]
    member this.SetUpdateInstructionPaneFunc(f) = updateInstructionPane <- f
    member this.MakeTheGameViewportEncompassAll(kbdX, kbdY) =
        whichHashtagTarget <- 0
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
        this.CycleWhichHashtagTarget(0)
    member this.CycleWhichHashtagTarget(delta) =
        let len = theGame.HashtagTargetsForQuickNav.Length
        whichHashtagTarget <- (whichHashtagTarget + len+1 + delta) % (len+1)
        currentTargets.Clear()
        if whichHashtagTarget = theGame.HashtagTargetsForQuickNav.Length then
            () // currentTargets empty means arrow to any cell
        else
            for i = recentEncompassingGridRange.MinX to recentEncompassingGridRange.MaxX do
                for j = recentEncompassingGridRange.MinY to recentEncompassingGridRange.MaxY do
                    let loc = GenericMetadata.Location(theGame.CurZone,i,j)
                    if metadataStore.LocationsForKey(theGame.HashtagTargetsForQuickNav.[whichHashtagTarget]).Contains(loc) then
                        currentTargets.Add(struct(i,j))
        hChain <- currentTargets.ToArray() |> Array.sort
        vChain <- currentTargets.ToArray() |> Array.map (fun (struct(x,y)) -> struct(y,x)) |> Array.sort
        updateInstructionPane()
        MapIcons.redrawMapIconHoverOnly.Trigger()
    member this.MoveLeftRight(dx) =
        if currentTargets.Count=0 then
            theGame.CurX <- theGame.CurX + dx
        else
            let cursor = struct(theGame.CurX, theGame.CurY)
            let r = MoveCore(dx, cursor, hChain)
            let struct(x,y) = r
            theGame.CurX <- x
            theGame.CurY <- y
    member this.MoveUpDown(dy) =
        if currentTargets.Count=0 then
            theGame.CurY <- theGame.CurY + dy
        else
            let cursor = struct(theGame.CurY, theGame.CurX)
            let r = MoveCore(dy, cursor, vChain)
            let struct(y,x) = r
            theGame.CurX <- x
            theGame.CurY <- y

let theQuickNav = new QuickNav()

let MakeInstructionsPane(parentWindow,appWidth,w,h) =
    let mkTxt(txt) = new TextBlock(IsHitTestVisible=false, FontSize=16., FontWeight=FontWeights.Bold, Text=txt, Foreground=Brushes.Black, Background=Brushes.Transparent) 
    let border(e) = new Border(BorderBrush=Brushes.Black, BorderThickness=Thickness(1.0), Child=e)
    let g = new Grid()
    g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength(32.)))
    g.ColumnDefinitions.Add(new ColumnDefinition(Width=GridLength.Auto))
    let data = [|
            2, ".",            "end Quick\nNav Mode"
            1, "9 *",          "next zone"
            1, "7",            "prior zone"
            2, "5",            "follow first\nhyperlink"
            1, "1",            "next hashtag"
            1, "3",            "prior hashtag"
            3, "8 \n4 6\n2 ",  ""
        |]
    let COUNT = data.Length
    for i = 0 to COUNT-1 do
        let n,a,b = data.[i]
        let h = (float n) * 24.
        g.RowDefinitions.Add(new RowDefinition(Height=GridLength(h)))
        let at = mkTxt(a)
        at.Height <- h
        at.TextAlignment <- TextAlignment.Right
        at.Padding <- Thickness(6.,0.,6.,0.)
        if at.Text="." then
            at.FontSize <- at.FontSize + 8.0
        Utils.gridAdd(g, border(at), 0, i)
        let bt = mkTxt(b)
        bt.Padding <- Thickness(6.,0.,6.,0.)
        Utils.gridAdd(g, border(bt), 1, i)
    let navInstructionTextBox = (g.Children.[g.Children.Count-1] :?> Border).Child :?> TextBlock
    let sp = new StackPanel(Orientation=Orientation.Vertical, Background=Brushes.LightSteelBlue, Visibility=Visibility.Hidden, Width=w, Height=h)
    sp.Children.Add(border(mkTxt("--QuickNav Mode--"))) |> ignore
    sp.Children.Add(mkTxt("NumPad hotkeys:")) |> ignore
    g.Margin <- Thickness(0.,4.,0.,4.)
    sp.Children.Add(g) |> ignore
    sp.Children.Add(mkTxt("Current target:")) |> ignore
    let curTargetTb = mkTxt("")
    curTargetTb.Margin <- Thickness(0.,0.,0.,4.)
    sp.Children.Add(curTargetTb) |> ignore
    let update() = 
        let curHashtagTarget = theQuickNav.CurrentlyTargetedHashtag()
        if curHashtagTarget=null then 
            navInstructionTextBox.Text <- "move to\nnext grid\ncell" 
            curTargetTb.Foreground <- Brushes.Black
            curTargetTb.Text <- "(any cell)"
        else 
            navInstructionTextBox.Text <- "move to\nnext hashtag\ntarget"
            curTargetTb.Foreground <- Brushes.Red
            curTargetTb.Text <- "#" + curHashtagTarget
    update()
    theQuickNav.SetUpdateInstructionPaneFunc(update)
    sp.Children.Add(mkTxt("Click below to\nchange which\nhashtags are targets")) |> ignore
    let b = new Button(Content="change\nhashtag targets", Margin=Thickness(6.))
    b.Click.Add(fun _ ->
        let orig = System.String.Join(",", theGame.HashtagTargetsForQuickNav)
        let extra = "Type in a comma-separated list of hashtags, without octothorpes (no '#')" 
                        + "\nCells whose Notes contain those hashtags will be legal targets for QuickNav"
                        + "\nCycle through them using NumPad1 and NumPad3 in QuickNav Mode"
                        + "\nExample:"
                        + "\nrespawn,fastTravel,save"
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

