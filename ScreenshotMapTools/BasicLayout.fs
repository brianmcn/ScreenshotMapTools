module BasicLayout

open System.Windows
open System.Windows.Controls
open System.Windows.Media

let NewGridWithColumns(gridlengthsInfo) =
    let g = new Grid()
    let cols = ResizeArray()
    for i,t in gridlengthsInfo do
        let col = new ColumnDefinition(Width=GridLength(i,t))
        g.ColumnDefinitions.Add(col)
        cols.Add(col)
    g, cols.ToArray()
let NewGridWithRows(gridlengthsInfo) =
    let g = new Grid()
    let rows = ResizeArray()
    for i,t in gridlengthsInfo do
        let row = new RowDefinition(Height=GridLength(i,t))
        g.RowDefinitions.Add(row)
        rows.Add(row)
    g, rows.ToArray()

let MakeSaveOrDiscardButtons(changeEnablingSaveButton : IEvent<unit>) =
    let saveOrDiscardGrid,_ = NewGridWithColumns([1.,GridUnitType.Star;1.,GridUnitType.Star;1.,GridUnitType.Star;1.,GridUnitType.Star;1.,GridUnitType.Star])
    saveOrDiscardGrid.Height <- 28.
    let savedEv = new Event<bool>()
    let saveButton = new Button(Content="Save Changes", IsEnabled=false)
    changeEnablingSaveButton.Add(fun () -> saveButton.IsEnabled <- true)
    let discardButton = new Button(Content="Discard Changes")
    saveButton.Click.Add(fun _ ->
        savedEv.Trigger(true)
        )
    discardButton.Click.Add(fun _ ->
        savedEv.Trigger(false)
        )
    Utils.gridAdd(saveOrDiscardGrid, Utils.centerWithGrid(saveButton), 3, 0)
    Utils.gridAdd(saveOrDiscardGrid, Utils.centerWithGrid(discardButton), 1, 0)
    saveOrDiscardGrid, savedEv.Publish

type IDataPane<'T> =    // 'T must work with JsonSerializer
    abstract member CreateEmptyData : unit->'T          // splitting a pane causes a new empty pane to appear, define how is that represented in your data type 'T
    abstract member ProjectDataToUI : 'T -> UIElement   // convert your data into a UI element
    abstract member ChangeDataSelection : 'T -> 'T      // (called on UI thread) here is your original data, use your own logic including popping up modal UI to maybe change it
    abstract member Changed : unit -> unit              // broadcast changes to tree structure

type ISplittable =
    abstract member Highlight : unit->unit
    abstract member UnHighlight : unit->unit
    abstract member SwitchToDepthSelectionMode : unit->unit
    abstract member SwitchToNormalMode : unit->unit
    abstract member Split : int -> unit

let testColors = [| Brushes.Black; Brushes.White; Brushes.Red; Brushes.Green; Brushes.Blue; Brushes.Yellow; |]
let rng = new System.Random(1)
let nextColor() = testColors.[rng.Next(testColors.Length)]

[<AllowNullLiteral>]
type JsonableTree<'T>(kind, data, pct, child1, child2) =
    new() = JsonableTree(0, Unchecked.defaultof<_>, 0, null, null)
    member val Kind : int = kind with get,set       // 0,1,2 = None,LeftRight,TopBottom
    member val Data : 'T = data with get,set
    member val Pct : int = pct with get,set
    member val Child1 : JsonableTree<'T> = child1 with get,set
    member val Child2 : JsonableTree<'T> = child2 with get,set
    member this.ToLayoutNode(idp) =
        if this.Kind = 0 then
            new LayoutNode<_>(SplitKind.None(this.Data), idp)
        elif this.Kind = 1 then
            new LayoutNode<_>(SplitKind.LeftRight(this.Pct, this.Child1.ToLayoutNode(idp), this.Child2.ToLayoutNode(idp)), idp)
        elif this.Kind = 2 then
            new LayoutNode<_>(SplitKind.TopBottom(this.Pct, this.Child1.ToLayoutNode(idp), this.Child2.ToLayoutNode(idp)), idp)
        else
            failwith "bad JsonableTree data"
and [<RequireQualifiedAccess>] SplitKind<'T> =
    | None of 'T
    | LeftRight of int * LayoutNode<'T> * LayoutNode<'T>
    | TopBottom of int * LayoutNode<'T> * LayoutNode<'T>
and LayoutNode<'T>(orig, idp:IDataPane<'T>) =
    let mutable kind = orig
    let T = 6.
    let MIN = 10
    let FRBRUSH = Brushes.Cyan
    let HIBRUSH = Brushes.Yellow
    let FROP = 0.2
    member this.ToJsonableTree() =
        match kind with
        | SplitKind.None(data)                  -> JsonableTree<'T>(0, data, 0, null, null)
        | SplitKind.LeftRight(pct, left, right) -> JsonableTree<'T>(1, Unchecked.defaultof<'T>, pct, left.ToJsonableTree(), right.ToJsonableTree())
        | SplitKind.TopBottom(pct, top, bottom) -> JsonableTree<'T>(2, Unchecked.defaultof<'T>, pct, top.ToJsonableTree(), bottom.ToJsonableTree())
    member this.MakeLeaf(data) = new LayoutNode<_>(SplitKind<_>.None(data), idp)
    member this.Kind = kind
    member this.IDP = idp
    member this.ClampPct(pct) = min (100-MIN) (max MIN pct)
    member this.Resist(pct) =   // resistance suggestive UI
        let RESIST = int(float MIN * 1.5)   
        if pct < RESIST then 
            let d = RESIST - pct
            RESIST - d/2
        elif pct > (100 - RESIST) then 
            let d = pct - (100 - RESIST)
            (100 - RESIST) + d/2
        else pct  
    member this.MakeDeletionOverlay() = new DockPanel(Background=Brushes.Red, Opacity=0.5, Visibility=Visibility.Hidden)
    member this.MakeFrameRectangle() = 
        new Shapes.Rectangle(VerticalAlignment=VerticalAlignment.Stretch, HorizontalAlignment=HorizontalAlignment.Stretch, Stroke=FRBRUSH, StrokeThickness=T, Opacity=FROP, IsHitTestVisible=false)
    member this.AsPanel(isEditable) =
        let relayout = Event<unit>()
        let split(whichWay) =
            if whichWay=0 then
                kind <- SplitKind.TopBottom(50, this.MakeLeaf(idp.CreateEmptyData()), new LayoutNode<_>(kind, idp))
            elif whichWay=1 then
                kind <- SplitKind.LeftRight(50, new LayoutNode<_>(kind,idp), this.MakeLeaf(idp.CreateEmptyData()))
            elif whichWay=2 then
                kind <- SplitKind.TopBottom(50, new LayoutNode<_>(kind, idp), this.MakeLeaf(idp.CreateEmptyData()))
            else
                kind <- SplitKind.LeftRight(50, this.MakeLeaf(idp.CreateEmptyData()), new LayoutNode<_>(kind, idp))
            relayout.Trigger()
        let layout() =
            match kind with
            | SplitKind.None(data) -> 
                let gCont = new Grid()
                let x = idp.ProjectDataToUI(data)
                if x <> null then gCont.Children.Add(x) |> ignore
                gCont.Background <- Brushes.Transparent // get hits
                let frameRect = 
                    if isEditable then
                        let frameRect = this.MakeFrameRectangle()
                        gCont.Children.Add(frameRect) |> ignore
                        frameRect
                    else
                        null
                gCont.Tag <- {  new ISplittable with
                                    member _.Highlight()   = if isEditable then (frameRect.Stroke <- HIBRUSH; frameRect.Opacity <- 1.0)
                                    member _.UnHighlight() = if isEditable then (frameRect.Stroke <- FRBRUSH; frameRect.Opacity <- FROP)
                                    member _.SwitchToDepthSelectionMode() = ()
                                    member _.SwitchToNormalMode() = ()
                                    member _.Split (whichWay) = split(whichWay)
                                }
                if isEditable then
                    gCont.MouseLeftButtonDown.Add(fun ea ->
                        ea.Handled <- true
                        let newData = idp.ChangeDataSelection(data)
                        kind <- SplitKind.None(newData)
                        relayout.Trigger()
                        )
                gCont
            | SplitKind.LeftRight(pct, left, right) ->
                let pct = this.ClampPct(pct)
                let g,[|leftCol;_;rightCol|] = NewGridWithColumns([float pct, GridUnitType.Star; 0., GridUnitType.Pixel; float(100-pct), GridUnitType.Star])
                Utils.gridAdd(g, left.AsPanel(isEditable), 0, 0)
                Utils.gridAdd(g, right.AsPanel(isEditable), 2, 0)
                if isEditable then
                    let ldo,rdo = this.MakeDeletionOverlay(),this.MakeDeletionOverlay()
                    Utils.gridAdd(g, ldo, 0, 0)
                    Utils.gridAdd(g, rdo, 2, 0)
                    let draggableRect = new Shapes.Rectangle(VerticalAlignment=VerticalAlignment.Stretch, Fill=Brushes.Magenta, Width=2.*T, Cursor=Input.Cursors.SizeWE)
                    g.SizeChanged.Add(fun _ -> draggableRect.Margin <- Thickness(-T,0.1*g.ActualHeight,-T,0.1*g.ActualHeight))
                    Utils.gridAdd(g, draggableRect, 1, 0)
                    draggableRect.MouseLeftButtonDown.Add(fun _ea -> draggableRect.CaptureMouse() |> ignore)
                    draggableRect.MouseMove.Add(fun ea ->
                        if draggableRect.IsMouseCaptured then
                            let pct = this.Resist(int(100. * ea.GetPosition(g).X / g.ActualWidth + 0.5))
                            ldo.Visibility <- if pct < MIN then Visibility.Visible else Visibility.Hidden
                            rdo.Visibility <- if pct > (100-MIN) then Visibility.Visible else Visibility.Hidden
                            let pct = this.ClampPct(pct)
                            leftCol.Width <- new GridLength(float pct, GridUnitType.Star)
                            rightCol.Width <- new GridLength(float(100-pct), GridUnitType.Star)
                        )
                    draggableRect.MouseLeftButtonUp.Add(fun ea ->
                        if draggableRect.IsMouseCaptured then
                            let pct = this.Resist(int(100. * ea.GetPosition(g).X / g.ActualWidth + 0.5))
                            if pct < MIN then           kind <- right.Kind
                            elif pct > (100-MIN) then   kind <- left.Kind
                            else                        kind <- SplitKind.LeftRight(pct, left, right)
                            draggableRect.ReleaseMouseCapture()
                            relayout.Trigger()
                        )
                    let gCont = Utils.centerWithGrid(g)
                    gCont.Background <- Brushes.Transparent // get hits
                    let frameRect = this.MakeFrameRectangle()
                    gCont.Children.Add(frameRect) |> ignore
                    gCont.Tag <- {  new ISplittable with
                                        member _.Highlight()   = frameRect.Stroke <- HIBRUSH; frameRect.Opacity <- 1.0
                                        member _.UnHighlight() = frameRect.Stroke <- FRBRUSH; frameRect.Opacity <- FROP
                                        member _.SwitchToDepthSelectionMode() = draggableRect.Visibility <- Visibility.Hidden
                                        member _.SwitchToNormalMode() = draggableRect.Visibility <- Visibility.Visible
                                        member _.Split (whichWay) = split(whichWay)
                                    }
                    gCont
                else
                    g
            | SplitKind.TopBottom(pct, top, bottom) ->
                let pct = this.ClampPct(pct)
                let g,[|topRow;_;bottomRow|] = NewGridWithRows([float pct, GridUnitType.Star; 0., GridUnitType.Pixel; float(100-pct), GridUnitType.Star])
                Utils.gridAdd(g, top.AsPanel(isEditable), 0, 0)
                Utils.gridAdd(g, bottom.AsPanel(isEditable), 0, 2)
                if isEditable then
                    let tdo,bdo = this.MakeDeletionOverlay(),this.MakeDeletionOverlay()
                    Utils.gridAdd(g, tdo, 0, 0)
                    Utils.gridAdd(g, bdo, 0, 2)
                    let draggableRect = new Shapes.Rectangle(HorizontalAlignment=HorizontalAlignment.Stretch, Fill=Brushes.Magenta, Height=2.*T, Cursor=Input.Cursors.SizeNS)
                    g.SizeChanged.Add(fun _ -> draggableRect.Margin <- Thickness(0.1*g.ActualWidth,-T,0.1*g.ActualWidth,-T))
                    Utils.gridAdd(g, draggableRect, 0, 1)
                    draggableRect.MouseLeftButtonDown.Add(fun _ea -> draggableRect.CaptureMouse() |> ignore)
                    draggableRect.MouseMove.Add(fun ea ->
                        if draggableRect.IsMouseCaptured then
                            let pct = this.Resist(int(100. * ea.GetPosition(g).Y / g.ActualHeight + 0.5))
                            tdo.Visibility <- if pct < MIN then Visibility.Visible else Visibility.Hidden
                            bdo.Visibility <- if pct > (100-MIN) then Visibility.Visible else Visibility.Hidden
                            let pct = this.ClampPct(pct)
                            topRow.Height <- new GridLength(float pct, GridUnitType.Star)
                            bottomRow.Height <- new GridLength(float(100-pct), GridUnitType.Star)
                        )
                    draggableRect.MouseLeftButtonUp.Add(fun ea ->
                        if draggableRect.IsMouseCaptured then
                            let pct = this.Resist(int(100. * ea.GetPosition(g).Y / g.ActualHeight + 0.5))
                            if pct < MIN then           kind <- bottom.Kind
                            elif pct > (100-MIN) then   kind <- top.Kind
                            else                        kind <- SplitKind.TopBottom(pct, top, bottom)
                            draggableRect.ReleaseMouseCapture()
                            relayout.Trigger()
                        )
                    let gCont = Utils.centerWithGrid(g)
                    gCont.Background <- Brushes.Transparent // get hits
                    let frameRect = this.MakeFrameRectangle()
                    gCont.Children.Add(frameRect) |> ignore
                    gCont.Tag <- {  new ISplittable with
                                        member _.Highlight()   = frameRect.Stroke <- HIBRUSH; frameRect.Opacity <- 1.0
                                        member _.UnHighlight() = frameRect.Stroke <- FRBRUSH; frameRect.Opacity <- FROP
                                        member _.SwitchToDepthSelectionMode() = draggableRect.Visibility <- Visibility.Hidden
                                        member _.SwitchToNormalMode() = draggableRect.Visibility <- Visibility.Visible
                                        member _.Split (whichWay) = split(whichWay)
                                    }
                    gCont
                else
                    g
        let dp = new DockPanel()
        dp.Children.Add(layout()) |> ignore
        relayout.Publish.Add(fun () ->
            dp.Children.Clear()
            dp.Children.Add(layout()) |> ignore
            idp.Changed()
            )
        dp
        

let RunBasicLayoutEditorDialog<'T>(parentWindow, title, width, tree:LayoutNode<'T>, treeChanged:IEvent<unit>) =
    let GM = 8.
    let g = Utils.centerWithGrid(tree.AsPanel(true))
    g.Width <- width - 2. * GM
    g.Height <- g.Width * 9. / 16.
    g.Margin <- Thickness(GM)
    let instructions = new TextBox(IsReadOnly=true, FontSize=16., FontWeight=FontWeights.Bold, Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.Gray, 
                                    Margin=Thickness(2.), TextWrapping=TextWrapping.Wrap, Height=140.)
    let NORMAL() = 
        let r = """Editor instructions:
LEFT CLICK on a pane to change its contents
or
RIGHT CLICK on a pane to split a pane in two"""
        if not(tree.Kind.IsNone) then
            r + """
or
DRAG MAGENTA bars to resize panes (drag to very end to remove a pane)"""
        else
            r
    let mutable hits : ResizeArray<ISplittable> = null     // if non-null, we're in a selection mode
    let mutable hitsIndex = -1
    let updateInstructions() =
        if hits = null then
            instructions.Text <- NORMAL()
            instructions.Foreground <- Brushes.Black
        else
            instructions.Text <- """Choose which pane to split:
first 
SCROLL WHEEL up/down in order to select larger/smaller pane/group to split
then
RIGHT CLICK to split it in half"""
            instructions.Foreground <- Brushes.DarkRed
    updateInstructions()
    treeChanged.Add(fun () -> updateInstructions())
    let mkContextMenu() =
        let cm = new ContextMenu()
        let a = [|
            "Split with empty pane above"
            "Split with empty pane to the right"
            "Split with empty pane below"
            "Split with empty pane to the left"
            |]
        for x = 0 to a.Length-1 do
            let mi = new MenuItem(Header=a.[x])
            mi.Click.Add(fun _ -> 
                for hit in hits do
                    hit.SwitchToNormalMode()
                instructions.Text <- NORMAL()
                instructions.Foreground <- Brushes.Black
                let guyToSplit = hits.[hitsIndex]
                hits <- null
                hitsIndex <- -1
                guyToSplit.Split(x)     // do it after we updated hits, so that the changed event will swap instructions appropriately
                g.ContextMenu <- null
                )
            cm.Items.Add(mi) |> ignore
        g.ContextMenu <- cm
    g.PreviewMouseLeftButtonDown.Add(fun ea ->
        if hits <> null then
            // we're in depth selection mode after one right click, and should disable the left-click portion of the UI for all panes
            ea.Handled <- true
        )
    g.MouseRightButtonDown.Add(fun ea ->
        if hits <> null then
            //ea.Handled <- true    // not handled so context menu handles it
            mkContextMenu()
        else
            hits <- ResizeArray()
            VisualTreeHelper.HitTest(g, null, 
                HitTestResultCallback(fun result ->
                    let mutable obj = result.VisualHit
                    while not (isNull obj) && obj <> g do
                        match obj with
                        | :? FrameworkElement as fe -> 
                            match fe.Tag with
                            | :? ISplittable as s -> if not (hits.Contains(s)) then hits.Add(s)
                            | _ -> ()
                        | _ -> ()
                        obj <- VisualTreeHelper.GetParent(obj)
                    HitTestResultBehavior.Continue
                ), PointHitTestParameters(ea.GetPosition(g)))
            if hits.Count = 0 then
                ea.Handled <- true
                hits <- null
            elif hits.Count = 1 then
                //ea.Handled <- true    // not handled so context menu handles it
                hitsIndex <- 0
                mkContextMenu()
            else
                ea.Handled <- true
                for hit in hits do
                    hit.SwitchToDepthSelectionMode()
                updateInstructions()
                hitsIndex <- hits.Count-1
                hits.[hitsIndex].Highlight()
        )
    g.MouseWheel.Add(fun ea ->
        if hits <> null then
            ea.Handled <- true
            if ea.Delta < 0 then
                if hitsIndex < hits.Count-1 then
                    hits.[hitsIndex].UnHighlight()
                    hitsIndex <- hitsIndex + 1
                    hits.[hitsIndex].Highlight()
            else
                if hitsIndex > 0 then
                    hits.[hitsIndex].UnHighlight()
                    hitsIndex <- hitsIndex - 1
                    hits.[hitsIndex].Highlight()
        )
    
    let saveOrDiscardGrid,saved = MakeSaveOrDiscardButtons(treeChanged)
    let mutable result = None
    let closeEv = new Event<unit>()
    saved.Add(fun b ->
        if b then
            result <- Some(tree.ToJsonableTree())
        closeEv.Trigger()
        )
    let sp = new StackPanel(Orientation=Orientation.Vertical, Width=width, Background=Brushes.Gray)
    sp.Children.Add(instructions) |> ignore
    sp.Children.Add(saveOrDiscardGrid) |> ignore
    sp.Children.Add(g) |> ignore
    Utils.DoModalDialog(parentWindow, sp, title, closeEv.Publish)
    result

let runBLEW(parentWindow, width) =        
    let leafdata() = nextColor()
    let changedEv = new Event<unit>()
    let idp = { new IDataPane<SolidColorBrush> with
                    member _.CreateEmptyData() = leafdata()
                    member _.ProjectDataToUI(scb) = new DockPanel(Background=scb)
                    member _.ChangeDataSelection(scb) = 
                        // just picks a new different color
                        let mutable r = leafdata()
                        while r = scb do
                            r <- leafdata()
                        r
                    member _.Changed() = changedEv.Trigger()
                }
    let leaf() = new LayoutNode<_>(SplitKind.None(leafdata()), idp)
    let blah = (new LayoutNode<_>(SplitKind.LeftRight(30, leaf(), new LayoutNode<_>(SplitKind.TopBottom(20, leaf(), leaf()),idp)),idp))
    let _ = RunBasicLayoutEditorDialog(parentWindow, "Test Layout", width, blah, changedEv.Publish)
    ()