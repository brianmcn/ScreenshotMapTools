module MinimapWindow

open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Utils.Extensions

let MAX = InMemoryStore.MAX

let WIDTH = 420.   // how wide want minimap to be; height will be based on aspect ratio

let MAP_ASPECT = 
    let _,_,w,h = GameSpecific.MapArea
    float w / float h

let MMW_W, MMW_H = WIDTH, WIDTH/MAP_ASPECT
[<AllowNullLiteral>]
type MinimapWindow(owner,zoomLevel,updateEv:IEvent<int*int*InMemoryStore.ZoneMemory>) as this =
    inherit Window()
    static let mutable theMinimapWindow : MinimapWindow = null
    let N = zoomLevel*2 - 1
    do
        this.Owner <- owner
        this.Title <- "MINIMAP"
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Loaded.Add(fun _ ->
            theMinimapWindow <- this
            )
        this.Closed.Add(fun _ ->
            theMinimapWindow <- null
            )
        let W,H = int MMW_W/N, int MMW_H/N
        let c = new Canvas(Width=float(W*N), Height=float(H*N))
        this.Content <- c
        let b = new Border(Width=c.Width, Height=c.Height, BorderThickness=Thickness(0.), Background=Brushes.Black)
        c.Children.Add(b) |> ignore
        Utils.canvasAdd(c, new Shapes.Rectangle(Width=float(W+1), Height=float(H+1), Stroke=Brushes.Yellow, StrokeThickness=1.), float(W*(N/2)), float(H*(N/2)))
        let g = Utils.makeGrid(N, N, W, H)
        b.Child <- g
        let bs = Array2D.init N N (fun x y -> 
            let r = new Border(BorderThickness=Thickness(1.,1.,0.,0.), Background=Brushes.DarkSlateBlue, Width=float W, Height=float H)
            Utils.gridAdd(g, r, x, y)
            r
            )
        updateEv.Add(fun (x,y,zm) ->
            for di = 0 to N-1 do
                for dj = 0 to N-1 do
                    let i = x-zoomLevel+di+1
                    let j = y-zoomLevel+dj+1
                    if i>=0 && i<MAX && j>=0 && j<MAX then
                        bs.[di,dj].Child <- zm.MapImgArray.GetCopyOfBmp(i,j) |> (fun bmp -> if bmp=null then null else 
                                                                                                let i = Utils.BMPtoImage bmp
                                                                                                i.Width <- float(W-1)
                                                                                                i.Height <- float(H-1)
                                                                                                i.Stretch <- Stretch.Uniform
                                                                                                i)
                    else
                        bs.[di,dj].Child <- null
            )
    static member TheFeatureWindow with get() = theMinimapWindow and set(x) = theMinimapWindow <- x
