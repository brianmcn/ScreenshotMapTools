module AreaSelection

open System.Windows
open System.Windows.Input
open System.Windows.Media
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
        let tb = new TextBox(IsReadOnly=true, FontSize=16., BorderThickness=Thickness(0.), 
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
            let ulColor,ulBrush = Colors.Yellow,Brushes.Yellow
            let midColor = Color.FromRgb(0uy,0x99uy,0uy)
            let lrColor,lrBrush = Colors.Cyan,Brushes.Cyan
            let R = 15.
            let rectBrushUL =
                let radialBrush = RadialGradientBrush()
                radialBrush.GradientStops.Add(GradientStop(ulColor, 0.0))
                radialBrush.GradientStops.Add(GradientStop(ulColor, 0.9))
                radialBrush.GradientStops.Add(GradientStop(midColor, 1.0))
                radialBrush.MappingMode <- BrushMappingMode.Absolute
                radialBrush.Center <- Point(0.,0.)
                radialBrush.GradientOrigin <- Point(0.,0.)
                radialBrush.RadiusX <- R
                radialBrush.RadiusY <- R
                radialBrush
            let rectBrushLR =
                let radialBrush = RadialGradientBrush()
                radialBrush.GradientStops.Add(GradientStop(lrColor, 0.0))
                radialBrush.GradientStops.Add(GradientStop(lrColor, 0.9))
                radialBrush.GradientStops.Add(GradientStop(midColor, 1.0))
                radialBrush.MappingMode <- BrushMappingMode.Absolute
                radialBrush.RadiusX <- R
                radialBrush.RadiusY <- R
                radialBrush
            let rect = new System.Windows.Shapes.Rectangle(Width=float rectw, Height=float recth, StrokeThickness=1.)
            let mutable wasUL = false
            let ctxt = System.Threading.SynchronizationContext.Current
            let highlightCircle = new Shapes.Ellipse(Fill=Brushes.White, Width=2.*R, Height=2.*R, Opacity=0.5, Visibility=Visibility.Hidden)
            Utils.canvasAdd(c, highlightCircle, float rectx, float recty)
            let lineToUL = new Shapes.Line(X1=0., Y1=0., Stroke=ulBrush, StrokeThickness=0.5)
            Utils.canvasAdd(c, lineToUL, 0., 0.)
            let lineToLR = new Shapes.Line(X2=w, Y2=h, Stroke=lrBrush, StrokeThickness=0.5)
            Utils.canvasAdd(c, lineToLR, 0., 0.)
            let updateTB() =
                if wasUL <> isUL then
                    if isUL then
                        Canvas.SetTop(highlightCircle, float recty - R)
                        Canvas.SetLeft(highlightCircle, float rectx - R)
                        lineToUL.Visibility <- Visibility.Visible
                        lineToLR.Visibility <- Visibility.Hidden
                    else
                        Canvas.SetTop(highlightCircle, float recty + float recth - R)
                        Canvas.SetLeft(highlightCircle, float rectx + float rectw - R)
                        lineToUL.Visibility <- Visibility.Hidden
                        lineToLR.Visibility <- Visibility.Visible
                    highlightCircle.Visibility <- Visibility.Visible
                    Async.StartImmediate(async { 
                        do! Async.Sleep(30)
                        do! Async.SwitchToContext ctxt
                        highlightCircle.Visibility <- Visibility.Hidden
                        })
                    wasUL <- isUL
                tb.Foreground <- if isUL then ulBrush else lrBrush
                if isUL then
                    rect.Stroke <- rectBrushUL
                    lineToUL.X2 <- float rectx
                    lineToUL.Y2 <- float recty
                else
                    rect.Stroke <- rectBrushLR
                    rectBrushLR.Center <- Point(float rectw, float recth)
                    rectBrushLR.GradientOrigin <- Point(float rectw, float recth)
                    lineToLR.X1 <- float rectx + float rectw
                    lineToLR.Y1 <- float recty + float recth
                let cor = if isUL then "upper-left" else "lower-right"
                let wxh = sprintf "@(%d,%d) - (%d x %d)" rectx recty rectw recth
                tb.Text <- sprintf "%s\nuse WASD/arrows to move %s corner\nCTRL+WASD/arrows for 10 pixels at a time\nENTER switch corners, ESC when done\n%s" label cor wxh
            async {
                Utils.canvasAdd(c, rect, float rectx, float recty)
                updateTB()
                while not allDone do
                    if isUL then
                        // move top left
                        let! key = Async.AwaitEvent this.PreviewKeyDown
                        let delta = if ((Keyboard.Modifiers &&& ModifierKeys.Control) = ModifierKeys.Control) then 10 else 1
                        if key.Key = Input.Key.W || key.Key = Input.Key.Up then
                            key.Handled <- true
                            let oldrecty = recty
                            recty <- recty - delta
                            recty <- max 0 recty
                            Canvas.SetTop(rect, recty)
                            recth <- recth + (oldrecty - recty)
                            rect.Height <- float recth
                        elif key.Key = Input.Key.A || key.Key = Input.Key.Left then
                            key.Handled <- true
                            let oldrectx = rectx
                            rectx <- rectx - delta
                            rectx <- max 0 rectx
                            Canvas.SetLeft(rect, rectx)
                            rectw <- rectw + (oldrectx - rectx)
                            rect.Width <- float rectw
                        elif key.Key = Input.Key.S || key.Key = Input.Key.Down then
                            key.Handled <- true
                            recty <- recty + delta
                            recty <- min (h-2) recty
                            Canvas.SetTop(rect, recty)
                            recth <- recth - delta
                            recth <- max 1 recth
                            rect.Height <- float recth
                        elif key.Key = Input.Key.D || key.Key = Input.Key.Right then
                            key.Handled <- true
                            rectx <- rectx + delta
                            rectx <- min (w-2) rectx
                            Canvas.SetLeft(rect, rectx)
                            rectw <- rectw - delta
                            rectw <- max 1 rectw
                            rect.Width <- float rectw
                        elif key.Key = Input.Key.Escape then
                            allDone <- true
                        elif key.Key = Input.Key.Return then
                            isUL <- false
                    else
                        // move bottom right
                        let! key = Async.AwaitEvent this.PreviewKeyDown
                        let delta = if ((Keyboard.Modifiers &&& ModifierKeys.Control) = ModifierKeys.Control) then 10 else 1
                        if key.Key = Input.Key.W || key.Key = Input.Key.Up then
                            key.Handled <- true
                            recth <- recth - delta
                            recth <- max 1 recth
                            rect.Height <- float recth
                        elif key.Key = Input.Key.A || key.Key = Input.Key.Left then
                            key.Handled <- true
                            rectw <- rectw - delta
                            rectw <- max 1 rectw
                            rect.Width <- float rectw
                        elif key.Key = Input.Key.S || key.Key = Input.Key.Down then
                            key.Handled <- true
                            recth <- recth + delta
                            recth <- min (h-recty-1) recth
                            rect.Height <- float recth
                        elif key.Key = Input.Key.D || key.Key = Input.Key.Right then
                            key.Handled <- true
                            rectw <- rectw + delta
                            rectw <- min (w-rectx-1) rectw
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
    Utils.nestedModalDialogCount <- Utils.nestedModalDialogCount + 1
    w.Closed.Add(fun _ -> Utils.nestedModalDialogCount <- Utils.nestedModalDialogCount - 1)
    w.ShowDialog() |> ignore
    let r = recentAreaSelectionResult
    recentAreaSelectionResult <- None
    r

