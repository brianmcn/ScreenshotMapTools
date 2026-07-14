module AreaSelection

open System
open System.Windows
open System.Windows.Media
open System.Threading
open System.Runtime.InteropServices
open System.Text
open System.Windows.Controls


type AreaSelectionWindow(x, y, w, h) as this =
    inherit Window()
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
        let b = new Border(Child=c, BorderBrush=Brushes.Yellow, BorderThickness=Thickness(1.))
        this.Content <- b
        this.Loaded.Add(fun _ ->
            let mutable finished = false
            let mutable rectx,recty,rectw,recth = 0,0,w,h
            async {
                let rect = new System.Windows.Shapes.Rectangle(Width=w, Height=h, Stroke=Brushes.Cyan, StrokeThickness=1.)
                Utils.canvasAdd(c, rect, 0, 0)
                // TODO make a display UI with instructions
                while not finished do
                    // move top left
                    let! key = Async.AwaitEvent this.PreviewKeyDown
                    if key.Key = Input.Key.W then
                        key.Handled <- true
                        if recty > 0 then
                            recty <- recty - 1
                            Canvas.SetTop(rect, recty)
                            recth <- recth + 1
                            rect.Height <- recth
                    elif key.Key = Input.Key.A then
                        key.Handled <- true
                        if rectx > 0 then
                            rectx <- rectx - 1
                            Canvas.SetLeft(rect, rectx)
                            rectw <- rectw + 1
                            rect.Width <- rectw
                    elif key.Key = Input.Key.S then
                        key.Handled <- true
                        if h > 1 then
                            recty <- recty + 1
                            Canvas.SetTop(rect, recty)
                            recth <- recth - 1
                            rect.Height <- recth
                    elif key.Key = Input.Key.D then
                        key.Handled <- true
                        if w > 1 then
                            rectx <- rectx + 1
                            Canvas.SetLeft(rect, rectx)
                            rectw <- rectw - 1
                            rect.Width <- rectw
                    elif key.Key = Input.Key.Return then
                        while not finished do
                            // move bottom right
                            let! key = Async.AwaitEvent this.PreviewKeyDown
                            if key.Key = Input.Key.W then
                                key.Handled <- true
                                if h > 1 then
                                    recth <- recth - 1
                                    rect.Height <- recth
                            elif key.Key = Input.Key.A then
                                key.Handled <- true
                                if w > 1 then
                                    rectw <- rectw - 1
                                    rect.Width <- rectw
                            elif key.Key = Input.Key.S then
                                key.Handled <- true
                                if recty+recth < h then
                                    recth <- recth + 1
                                    rect.Height <- recth
                            elif key.Key = Input.Key.D then
                                key.Handled <- true
                                if rectx+rectw < w then
                                    rectw <- rectw + 1
                                    rect.Width <- rectw
                            elif key.Key = Input.Key.Return then
                                finished <- true
                this.Close()
            } |> Async.StartImmediate
            )

