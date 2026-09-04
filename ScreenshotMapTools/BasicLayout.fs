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

[<RequireQualifiedAccess>]
type SplitKind<'T> =
    | None of 'T * ('T -> UIElement)
    | LeftRight of int * LayoutNode * LayoutNode
    | TopBottom of int * LayoutNode * LayoutNode
and LayoutNode(orig) =
    let mutable kind = orig
    let T = 4.
    let MIN = 10
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
        new Shapes.Rectangle(VerticalAlignment=VerticalAlignment.Stretch, HorizontalAlignment=HorizontalAlignment.Stretch, Stroke=Brushes.Cyan, StrokeThickness=T, Opacity=0.5)
    member this.AsPanel(isEditable) =
        let mutable relayout = Event<unit>()
        let layout() =
            match kind with
            | SplitKind.None(data, projF) -> 
                let g = new Grid()
                let x = projF(data)
                if x <> null then g.Children.Add(x) |> ignore
                g.Children.Add(this.MakeFrameRectangle()) |> ignore
                g
            | SplitKind.LeftRight(pct, left, right) ->
                let pct = this.ClampPct(pct)
                let g,[|leftCol;_;rightCol|] = NewGridWithColumns([float pct, GridUnitType.Star; 0., GridUnitType.Pixel; float(100-pct), GridUnitType.Star])
                Utils.gridAdd(g, left.AsPanel(isEditable), 0, 0)
                Utils.gridAdd(g, right.AsPanel(isEditable), 2, 0)
                if isEditable then
                    Utils.gridAdd(g, this.MakeFrameRectangle(), 0, 0)
                    Utils.gridAdd(g, this.MakeFrameRectangle(), 2, 0)
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
                g
            | SplitKind.TopBottom(pct, top, bottom) ->
                let pct = this.ClampPct(pct)
                let g,[|topRow;_;bottomRow|] = NewGridWithRows([float pct, GridUnitType.Star; 0., GridUnitType.Pixel; float(100-pct), GridUnitType.Star])
                Utils.gridAdd(g, top.AsPanel(isEditable), 0, 0)
                Utils.gridAdd(g, bottom.AsPanel(isEditable), 0, 2)
                if isEditable then
                    Utils.gridAdd(g, this.MakeFrameRectangle(), 0, 0)
                    Utils.gridAdd(g, this.MakeFrameRectangle(), 0, 2)
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
                            if pct < MIN then           kind <- top.Kind
                            elif pct > (100-MIN) then   kind <- bottom.Kind
                            else                        kind <- SplitKind.TopBottom(pct, top, bottom)
                            draggableRect.ReleaseMouseCapture()
                            relayout.Trigger()
                        )
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
        let leaf = new LayoutNode(SplitKind.None((), fun()->null))
        let blah = (new LayoutNode(SplitKind.LeftRight(30, leaf, new LayoutNode(SplitKind.TopBottom(20, leaf, leaf))))).AsPanel(true)
        let g = Utils.centerWithGrid(blah)
        g.Width <- width
        g.Height <- width
        g.Margin <- Thickness(8.)
        this.Title <- "Test Layout"
        this.Background <- Brushes.Gray
        this.Content <- g
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.ResizeMode <- ResizeMode.NoResize
        

