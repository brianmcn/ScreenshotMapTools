module HS

let qrdata = """
XXXXXXX..X..XXX...XXXXXXX
X.....X.X..XX.X...X.....X
X.XXX.X.XXXXX..XX.X.XXX.X
X.XXX.X..XXX..X.X.X.XXX.X
X.XXX.X.X......X..X.XXX.X
X.....X.XX.X.XXXX.X.....X
XXXXXXX.X.X.X.X.X.XXXXXXX
........XX..XXX.X........
XX.X..XX..X.......XXX.XX.
.X.XXX..X.XX.XXXXXX...XXX
XXXXX.X..XX...XXX........
X.X.XX.XX....X.X.X.XX.X..
....XXX.X...X..X..XXXX.XX
X...XX....XXX.XXXXX.XXXX.
.XX.XXXX..X.X..XXX.X.X..X
X....X.XXXX.X.XX..XXXX.XX
..X...X.XXXXX..XXXXXXX.XX
........XXX.X.XXX...X.XX.
XXXXXXX.X.X.X...X.X.X..X.
X.....X..XXX....X...X.X..
X.XXX.X...XXXX.XXXXXX..XX
X.XXX.X.X..XX..XXX..XX...
X.XXX.X..XXXX...X...XXX.X
X.....X.X.XXXXX.XX.X..X.X
XXXXXXX.X.X..X.XX..XX.X.X
"""
// GREEN DOOR INSIDE LOWER LEFT BOX, BOTTOM RIGHT OF MIDDLE DOT

let lines = qrdata.Split([|'\n'|]).[1..]
let qr(i,j) = lines.[j].Chars(i)

open System.Windows
open System.Windows.Controls
open System.Windows.Media

let gridAdd(g:Grid, x, c, r) =
    g.Children.Add(x) |> ignore
    Grid.SetColumn(x, c)
    Grid.SetRow(x, r)
let makeGrid(nc, nr, cw, rh) =
    let grid = new Grid()
    for i = 0 to nc-1 do
        let w = if cw = -1 then GridLength.Auto else GridLength(float cw)
        grid.ColumnDefinitions.Add(new ColumnDefinition(Width=w))
    for i = 0 to nr-1 do
        let h = if rh = -1 then GridLength.Auto else GridLength(float rh)
        grid.RowDefinitions.Add(new RowDefinition(Height=h))
    grid

type QRWindow() as this =
    inherit Window()
    do
        this.Title <- "QR code"
        this.Left <- 300.
        this.Top <- 300.
        this.Topmost <- true
        this.SizeToContent <- SizeToContent.WidthAndHeight

        let N = 7 + 11 + 7

        let g = makeGrid(N,N,20,20)
        for x = 0 to N-1 do
            for y = 0 to N-1 do
                let c = 
                    if qr(x,y)='X' then
                        new Canvas(Background=Brushes.Black)
                    else
                        new Canvas(Background=Brushes.White)
                gridAdd(g,c,x,y)

        this.Content <- g
