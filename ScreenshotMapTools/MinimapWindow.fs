module MinimapWindow

open System.Windows
open System.Windows.Controls
open System.Windows.Media
open Utils.Extensions

let MAX = InMemoryStore.MAX

let WIDTH = 492.   // how wide want minimap to be; height will be based on aspect ratio
let LEFT = 785.

let MAP_ASPECT = 
    let _,_,w,h = GameSpecific.MapArea
    float w / float h

let MMW_W, MMW_H = WIDTH, WIDTH/MAP_ASPECT
let EXTRA = 0.35
[<AllowNullLiteral>]
type MinimapWindow(owner,zoomLevel,updateEv:IEvent<int*int*InMemoryStore.ZoneMemory>) as this =
    inherit Window()
    static let mutable theMinimapWindow : MinimapWindow = null
    let N = zoomLevel*2 - 1   // size to show, e.g. 3 means 5x5 (but peeks at extra 7x7 at edges)
    do
        this.Owner <- owner
        this.Title <- "MINIMAP"
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.UseLayoutRounding <- true
        this.Loaded.Add(fun _ ->
            theMinimapWindow <- this
            this.Left <- LEFT
            this.Top <- 10.
            )
        this.Closed.Add(fun _ ->
            theMinimapWindow <- null
            )
        let NE = float N+2.*EXTRA
        let W,H = int(MMW_W/NE), int(MMW_H/NE)
        let WE,HE = float W*EXTRA, float H*EXTRA
        let c = new Canvas(Width=float(W)*NE, Height=float(H)*NE, Background=new SolidColorBrush(Color.FromRgb(0x60uy,0x60uy,0x60uy))) // Brushes.Black)
        this.Content <- c
        let g = Utils.makeGrid(N+2, N+2, W, H)
        Utils.canvasAdd(c, g, WE - float W, HE - float H)
        let cursorRect = new Shapes.Rectangle(Width=float(W+1), Height=float(H+1), Stroke=Brushes.Yellow, StrokeThickness=3.)
        Utils.canvasAdd(c, cursorRect, WE + float(W*(N/2)), HE + float(H*(N/2)))
        let bs = Array2D.init (N+2) (N+2) (fun x y -> 
            let r = new Border(BorderThickness=Thickness(1.,1.,0.,0.), Width=float W, Height=float H, Background=new SolidColorBrush(Color.FromRgb(0x30uy,0uy,0x50uy))) // Brushes.DarkOliveGreen)
            Utils.gridAdd(g, r, x, y)
            r
            )
        let drawViewCenteredAt(x,y,zm:InMemoryStore.ZoneMemory) =
            for di = 0 to N+1 do
                for dj = 0 to N+1 do
                    let i = x-zoomLevel+di
                    let j = y-zoomLevel+dj
                    if i>=0 && i<MAX && j>=0 && j<MAX then
                        bs.[di,dj].Child <- zm.MapImgArray.GetCopyOfBmp(i,j) |> (fun bmp -> if bmp=null then null else 
                                                                                                let i = Utils.BMPtoImage bmp
                                                                                                i.Width <- float(W-1)
                                                                                                i.Height <- float(H-1)
                                                                                                i.Stretch <- Stretch.Fill
                                                                                                i)
                    else
                        bs.[di,dj].Child <- null
        updateEv.Add(fun (x,y,zm) -> 
            let mutable dx,dy = 0,0
            let yRange = [|y-zoomLevel .. y-zoomLevel+N+1|]
            let leftEdgeIsEmpty() = yRange |> Array.exists (fun j -> zm.MapImgArray.HasBmp(x+dx-zoomLevel+1, j)) |> not
            let rightEdgeIsEmpty() = yRange |> Array.exists (fun j -> zm.MapImgArray.HasBmp(x+dx-zoomLevel+N, j)) |> not
            while dx < zoomLevel-1 && leftEdgeIsEmpty() && not(rightEdgeIsEmpty()) do
                dx <- dx + 1
            while dx > -zoomLevel+1 && not(leftEdgeIsEmpty()) && rightEdgeIsEmpty() do
                dx <- dx - 1
            let xRange = [|x-zoomLevel .. x-zoomLevel+N+1|]
            let topEdgeIsEmpty() = xRange |> Array.exists (fun i -> zm.MapImgArray.HasBmp(i, y+dy-zoomLevel+1)) |> not
            let bottomEdgeIsEmpty() = xRange |> Array.exists (fun i -> zm.MapImgArray.HasBmp(i, y+dy-zoomLevel+N)) |> not
            while dy < zoomLevel-1 && topEdgeIsEmpty() && not(bottomEdgeIsEmpty()) do
                dy <- dy + 1
            while dy > -zoomLevel+1 && not(topEdgeIsEmpty()) && bottomEdgeIsEmpty() do
                dy <- dy - 1
            //printfn "dx,dy = %d,%d" dx dy
            drawViewCenteredAt(x+dx,y+dy,zm)
            Canvas.SetLeft(cursorRect, WE + float(W*(N/2 - dx)))
            Canvas.SetTop(cursorRect, HE + float(H*(N/2 - dy)))
            )
    static member TheMinimapWindow with get() = theMinimapWindow and set(x) = theMinimapWindow <- x

///////////////////////////////////////////////////////

type AbstractFixedMinimapWindow(owner,updateEv:IEvent<int*int*InMemoryStore.ZoneMemory>) as this =
    inherit Window()
    do
        this.Owner <- owner
        this.Title <- "ABSTRACT MINIMAP"
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.UseLayoutRounding <- true
        this.Loaded.Add(fun _ ->
            this.Left <- LEFT
            this.Top <- 470.
            )
        let minx,maxx = 49,66
        let miny,maxy = 49,66
        let nx, ny = maxx-minx+1, maxy-miny+1
        let W = int(0.5 * MMW_W/float(nx))
        let H = int(float W / MAP_ASPECT)
        let c = new Canvas(Width=float(W*nx), Height=float(H*ny), Background=new SolidColorBrush(Color.FromRgb(0x60uy,0x60uy,0x60uy))) // Brushes.Black)
        this.Content <- c
        let g = Utils.makeGrid(nx, ny, W, H)
        Utils.canvasAdd(c, g, 0., 0.)
        let cursorRect = new Shapes.Rectangle(Width=float(W+1), Height=float(H+1), Stroke=Brushes.Yellow, StrokeThickness=3.)
        Utils.canvasAdd(c, cursorRect, 0., 0.)
        // area lines
        for i = 0 to 4 do
            Utils.canvasAdd(c, new Shapes.Line(X1=0., X2=0., Y1=0., Y2=float(H*(ny-2)), StrokeThickness=1., Stroke=Brushes.Red), float(W*(4*i+1)), float H)
        for j = 0 to 4 do
            Utils.canvasAdd(c, new Shapes.Line(X1=0., X2=float(W*(nx-2)), Y1=0., Y2=0., StrokeThickness=1., Stroke=Brushes.Red), float W, float(H*(4*j+1)))
        let borderBg = new SolidColorBrush(Color.FromRgb(0x30uy,0uy,0x50uy)) // Brushes.DarkOliveGreen)
        let bs = Array2D.init nx ny (fun x y -> 
            let r = new Border(BorderThickness=Thickness(1.,1.,0.,0.), Width=float W, Height=float H, Background=borderBg)
            Utils.gridAdd(g, r, x, y)
            r
            )
        let draw(_x,_y,zm:InMemoryStore.ZoneMemory) =
            let ok = zm.Zone = 1
            let saves = InMemoryStore.metadataStore.LocationsForKey("save") |> Seq.filter (fun k -> k.Zone = 1) |> Seq.map (fun k -> k.X,k.Y) |> Seq.toArray
            for i = minx to maxx do
                for j = miny to maxy do
                    bs.[i-minx, j-miny].Background <- 
                        if ok && zm.MapImgArray.HasBmp(i,j) then
                            if saves |> Array.contains (i,j) then
                                Brushes.Green
                            else
                                Brushes.CornflowerBlue
                        else
                            borderBg
        updateEv.Add(fun (x,y,zm) -> 
            draw(x,y,zm)
            Canvas.SetLeft(cursorRect, float(W*(x-minx)))
            Canvas.SetTop(cursorRect, float(H*(y-miny)))
            )

///////////////////////////////////////////////////////

let MakeRichTextBox(margin) = 
    new RichTextBox(IsReadOnly=true, FontSize=20., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue, // SolidColorBrush(Color.FromRgb(0x84uy,0xB5uy,0xFDuy)), 
                                FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, SelectionBrush=Brushes.Orange,
                                HorizontalAlignment=HorizontalAlignment.Stretch, IsDocumentEnabled=true,
                                Height=200., VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(margin))
let UpdateRichTextBox(tb:RichTextBox, i, j, z, mt:BackingStoreData.MapTile) =
    let fd = new System.Windows.Documents.FlowDocument()
    let p = new System.Windows.Documents.Paragraph()
    p.Inlines.Add(sprintf "(%02d,%02d) %d screenshots\n" i j (mt.NumScreenshots()))
    let mutable note = mt.Note
    if note <> null then
        let linkages = GenericMetadata.FindAllLinkages(mt.Note, z, i, j)
        while linkages.Count > 0 do
            let si,substr,loc,li = linkages |> Seq.mapi (fun i (loc,substr) -> note.IndexOf(substr), substr, loc, i) |> Seq.sortBy (fun (a,_,_,_)->a) |> Seq.head
            linkages.RemoveAt(li)
            p.Inlines.Add(note.Substring(0,si))
            p.Inlines.Add(new System.Windows.Documents.Hyperlink(System.Windows.Documents.Run(substr), NavigateUri=new System.Uri(sprintf "http://foo.bar/%02d/%02d/%02d" loc.Zone loc.X loc.Y)))
            note <- note.Substring(si+substr.Length)
        p.Inlines.Add(note)
    fd.Blocks.Add(p)
    tb.Document <- fd

///////////////////////////////////////////////////////

[<AllowNullLiteral>]
type NotesWindow(owner,updateEv:IEvent<int*int*InMemoryStore.ZoneMemory>) as this =
    inherit Window()
    static let mutable theNotesWindow : NotesWindow = null
    do
        this.Owner <- owner
        this.Title <- "NOTES"
        this.UseLayoutRounding <- true
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Loaded.Add(fun _ ->
            theNotesWindow <- this
            this.Left <- LEFT
            this.Top <- 700.
            )
        this.Closed.Add(fun _ ->
            theNotesWindow <- null
            )
        let b = new Border(Width=WIDTH, Height=140., BorderThickness=Thickness(0.), Background=Brushes.Black)
        this.Content <- b
        let tb = MakeRichTextBox(0.)
        tb.Height <- System.Double.NaN
        b.Child <- tb
        updateEv.Add(fun (x,y,zm) ->
            UpdateRichTextBox(tb, x, y, zm.Zone, zm.MapTiles.[x,y])
            )
    static member TheNotesWindow with get() = theNotesWindow and set(x) = theNotesWindow <- x

