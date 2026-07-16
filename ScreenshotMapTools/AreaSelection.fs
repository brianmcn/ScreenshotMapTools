module AreaSelection

open System
open System.Windows
open System.Windows.Media
open System.Threading
open System.Runtime.InteropServices
open System.Text
open System.Windows.Controls


let mutable recentAreaSelectionResult = None
type AreaSelectionWindow(windowArea, selectionArea, label) as this =
    inherit Window()
    let x,y,w,h = windowArea
    let X,Y,W,H = x-1, y-1, w+2, h+2    // lowercase is target window we cover; uppercase is our window, with pixel frame around area
    do
        this.Title <- "Area Selection"
        this.SizeToContent <- SizeToContent.Manual
        this.WindowStartupLocation <- WindowStartupLocation.Manual
        this.Background <- Brushes.Transparent
        this.AllowsTransparency <- true
        this.WindowStyle <- WindowStyle.None
        this.Cursor <- System.Windows.Input.Cursors.None
        this.Topmost <- true
        this.Left <- float X
        this.Top <- float Y
        this.Width <- float W
        this.Height <- float H
        let c = new Canvas(Width=float w, Height=float h, Background=new SolidColorBrush(Color.FromArgb(1uy,0uy,0uy,0uy)))
        let b = new Border(Child=c, BorderBrush=Brushes.DarkGray, BorderThickness=Thickness(1.))
        this.Content <- b
        let tb = new TextBox(IsReadOnly=true, FontSize=12., BorderThickness=Thickness(1.), BorderBrush=Brushes.Yellow, 
                                Foreground=Brushes.White, Background=Brushes.Black, Margin=Thickness(2.), Opacity=0.8,
                                TextAlignment=TextAlignment.Center, HorizontalContentAlignment=HorizontalAlignment.Center, VerticalContentAlignment=VerticalAlignment.Center)
        let g = Utils.centerWithGrid(Utils.dontFillSurroundingSpace(tb))
        g.Width <- c.Width
        g.Height <- c.Height
        Utils.canvasAdd(c, g, 0, 0)
        this.Loaded.Add(fun _ ->
            let mutable allDone = false
            let mutable rectx,recty,rectw,recth = selectionArea
            let mutable isUL = true
            let ulColor,ulBrush = Colors.Lime,Brushes.Lime
            let lrColor,lrBrush = Colors.Cyan,Brushes.Cyan
            let rectBrush =
                let startStop = GradientStop(ulColor, 0.0)
                let endStop   = GradientStop(lrColor, 1.0)
                let diagonalBrush = LinearGradientBrush()
                diagonalBrush.StartPoint <- Point(0.0, 0.0)
                diagonalBrush.EndPoint <- Point(1.0, 1.0)
                diagonalBrush.GradientStops.Add(startStop)
                diagonalBrush.GradientStops.Add(endStop)
                diagonalBrush
            let updateTB() =
                tb.Foreground <- if isUL then ulBrush else lrBrush
                let cor = if isUL then "upper-left" else "lower-right"
                let wxh = sprintf "@(%d,%d) - (%d x %d)" rectx recty rectw recth
                tb.Text <- sprintf "%s\nuse WASD to move %s corner\nENTER switch corners, ESC when done\n%s" label cor wxh
            async {
                let rect = new System.Windows.Shapes.Rectangle(Width=float rectw, Height=float recth, Stroke=rectBrush, StrokeThickness=1.)
                Utils.canvasAdd(c, rect, float rectx, float recty)
                updateTB()
                while not allDone do
                    if isUL then
                        // move top left
                        let! key = Async.AwaitEvent this.PreviewKeyDown
                        if key.Key = Input.Key.W then
                            key.Handled <- true
                            if recty > 0 then
                                recty <- recty - 1
                                Canvas.SetTop(rect, recty)
                                recth <- recth + 1
                                rect.Height <- float recth
                        elif key.Key = Input.Key.A then
                            key.Handled <- true
                            if rectx > 0 then
                                rectx <- rectx - 1
                                Canvas.SetLeft(rect, rectx)
                                rectw <- rectw + 1
                                rect.Width <- float rectw
                        elif key.Key = Input.Key.S then
                            key.Handled <- true
                            if h > 1 then
                                recty <- recty + 1
                                Canvas.SetTop(rect, recty)
                                recth <- recth - 1
                                rect.Height <- float recth
                        elif key.Key = Input.Key.D then
                            key.Handled <- true
                            if w > 1 then
                                rectx <- rectx + 1
                                Canvas.SetLeft(rect, rectx)
                                rectw <- rectw - 1
                                rect.Width <- float rectw
                        elif key.Key = Input.Key.Escape then
                            allDone <- true
                        elif key.Key = Input.Key.Return then
                            isUL <- false
                    else
                        // move bottom right
                        let! key = Async.AwaitEvent this.PreviewKeyDown
                        if key.Key = Input.Key.W then
                            key.Handled <- true
                            if h > 1 then
                                recth <- recth - 1
                                rect.Height <- float recth
                        elif key.Key = Input.Key.A then
                            key.Handled <- true
                            if w > 1 then
                                rectw <- rectw - 1
                                rect.Width <- float rectw
                        elif key.Key = Input.Key.S then
                            key.Handled <- true
                            if recty+recth < h then
                                recth <- recth + 1
                                rect.Height <- float recth
                        elif key.Key = Input.Key.D then
                            key.Handled <- true
                            if rectx+rectw < w then
                                rectw <- rectw + 1
                                rect.Width <- float rectw
                        elif key.Key = Input.Key.Escape then
                            allDone <- true
                        elif key.Key = Input.Key.Return then
                            isUL <- true
                    updateTB()
                recentAreaSelectionResult <- Some(rectx, recty, rectw, recth)
                this.Close()
            } |> Async.StartImmediate
            )

let DoAreaSelection(windowArea,selectionArea,label) =
    let w = new AreaSelectionWindow(windowArea,selectionArea,label)
    w.ShowDialog() |> ignore
    let r = recentAreaSelectionResult
    recentAreaSelectionResult <- None
    r

