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

type ISplittable =
    abstract member Highlight : unit->unit
    abstract member UnHighlight : unit->unit
    abstract member SwitchToDepthSelectionMode : unit->unit
    abstract member SwitchToNormalMode : unit->unit
    abstract member Split : int -> unit

let testColors = [| Brushes.Black; Brushes.White; Brushes.Red; Brushes.Green; Brushes.Blue; Brushes.Yellow; |]
let rng = new System.Random(1)
let nextColor() = testColors.[rng.Next(testColors.Length)]

[<RequireQualifiedAccess>]
type SplitKind<'T> =
    | None of 'T * ('T -> UIElement)
    | LeftRight of int * LayoutNode * LayoutNode
    | TopBottom of int * LayoutNode * LayoutNode
and LayoutNode(orig) =
    let mutable kind = orig
    let T = 4.
    let MIN = 10
    let FRBRUSH = Brushes.Cyan
    let FROP = 0.2
    static member MakeLeaf() = 
        let c = nextColor()
        new LayoutNode(SplitKind.None(c, fun(c)->new DockPanel(Background=c)))
    member this.Kind = kind
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
                kind <- SplitKind.TopBottom(50, LayoutNode.MakeLeaf(), new LayoutNode(kind))
            elif whichWay=1 then
                kind <- SplitKind.LeftRight(50, new LayoutNode(kind), LayoutNode.MakeLeaf())
            elif whichWay=2 then
                kind <- SplitKind.TopBottom(50, new LayoutNode(kind), LayoutNode.MakeLeaf())
            else
                kind <- SplitKind.LeftRight(50, LayoutNode.MakeLeaf(), new LayoutNode(kind))
            relayout.Trigger()
        let layout() =
            match kind with
            | SplitKind.None(data, projF) -> 
                let gCont = new Grid()
                let x = projF(data)
                if x <> null then gCont.Children.Add(x) |> ignore
                gCont.Background <- Brushes.Transparent // get hits
                let frameRect = this.MakeFrameRectangle()
                gCont.Children.Add(frameRect) |> ignore
                gCont.Tag <- {  new ISplittable with
                                    member _.Highlight()   = frameRect.Stroke <- Brushes.Lime; frameRect.Opacity <- 1.0
                                    member _.UnHighlight() = frameRect.Stroke <- FRBRUSH; frameRect.Opacity <- FROP
                                    member _.SwitchToDepthSelectionMode() = ()
                                    member _.SwitchToNormalMode() = ()
                                    member _.Split (whichWay) = split(whichWay)
                                }
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
                                        member _.Highlight()   = frameRect.Stroke <- Brushes.Lime; frameRect.Opacity <- 1.0
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
                                        member _.Highlight()   = frameRect.Stroke <- Brushes.Lime; frameRect.Opacity <- 1.0
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
            )
        dp

type TestLayoutWindow(width) as this =
    inherit Window()
    do
        let leaf() = LayoutNode.MakeLeaf()
        let blah = (new LayoutNode(SplitKind.LeftRight(30, leaf(), new LayoutNode(SplitKind.TopBottom(20, leaf(), leaf()))))).AsPanel(true)
        let g = Utils.centerWithGrid(blah)
        g.Width <- width
        g.Height <- width
        g.Margin <- Thickness(8.)
        let instructions = new TextBox(IsReadOnly=true, FontSize=16., Text="", BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.Gray, Margin=Thickness(2.))
        let NORMAL = "drag magenta\nLMB populate pane (TODO)\nRMB split pane"
        instructions.Text <- NORMAL
        let mutable hits : ResizeArray<ISplittable> = null     // if non-null, we're in a selection mode
        let mutable hitsIndex = -1
        g.MouseRightButtonDown.Add(fun ea ->
            if hits <> null then
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
                        hits.[hitsIndex].Split(x)
                        for hit in hits do
                            hit.SwitchToNormalMode()
                            instructions.Text <- NORMAL
                        hits <- null
                        )
                    cm.Items.Add(mi) |> ignore
                g.ContextMenu <- cm
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
                    hits <- null
                else
                    for hit in hits do
                        hit.SwitchToDepthSelectionMode()
                    instructions.Text <- "scroll to select pane to split\nRMB to split it"
                    hitsIndex <- 0
                    hits.[hitsIndex].Highlight()
            )
        g.MouseWheel.Add(fun ea ->
            if hits <> null then
                if ea.Delta > 0 then
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
        let sp = new StackPanel(Orientation=Orientation.Vertical)
        sp.Children.Add(instructions) |> ignore
        sp.Children.Add(g) |> ignore
        this.Title <- "Test Layout"
        this.Background <- Brushes.Gray
        this.Content <- sp
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.ResizeMode <- ResizeMode.NoResize
        

