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
let EXTRA = 0.2
[<AllowNullLiteral>]
type MinimapWindow(owner,zoomLevel,updateEv:IEvent<int*int*InMemoryStore.ZoneMemory>) as this =
    inherit Window()
    static let mutable theMinimapWindow : MinimapWindow = null
    let N = zoomLevel*2 - 1   // size to show, e.g. 3 means 5x5 (but peeks at extra 7x7 at edges)
    do
        this.Owner <- owner
        this.Title <- "MINIMAP"
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Loaded.Add(fun _ ->
            theMinimapWindow <- this
            this.Left <- 850.
            this.Top <- 50.
            )
        this.Closed.Add(fun _ ->
            theMinimapWindow <- null
            )
        let NE = float N+2.*EXTRA
        let W,H = int(MMW_W/NE), int(MMW_H/NE)
        let WE,HE = float W*EXTRA, float H*EXTRA
        let c = new Canvas(Width=float(W)*NE, Height=float(H)*NE, Background=Brushes.Black)
        this.Content <- c
        let g = Utils.makeGrid(N+2, N+2, W, H)
        Utils.canvasAdd(c, g, WE - float W, HE - float H)
        // TODO move
        Utils.canvasAdd(c, new Shapes.Rectangle(Width=float(W+1), Height=float(H+1), Stroke=Brushes.Yellow, StrokeThickness=1.), WE + float(W*(N/2)), HE + float(H*(N/2)))
        let bs = Array2D.init (N+2) (N+2) (fun x y -> 
            let r = new Border(BorderThickness=Thickness(1.,1.,0.,0.), Background=Brushes.DarkSlateBlue, Width=float W, Height=float H)
            Utils.gridAdd(g, r, x, y)
            r
            )
        updateEv.Add(fun (x,y,zm) ->
            for di = 0 to N+1 do
                for dj = 0 to N+1 do
                    let i = x-zoomLevel+di
                    let j = y-zoomLevel+dj
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
    static member TheMinimapWindow with get() = theMinimapWindow and set(x) = theMinimapWindow <- x

///////////////////////////////////////////////////////

let MakeRichTextBox(margin) = 
    new RichTextBox(IsReadOnly=true, FontSize=20., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.CornflowerBlue, // SolidColorBrush(Color.FromRgb(0x84uy,0xB5uy,0xFDuy)), 
                                FontFamily=FontFamily("Consolas"), FontWeight=FontWeights.Bold, SelectionBrush=Brushes.Orange,
                                HorizontalAlignment=HorizontalAlignment.Stretch, IsDocumentEnabled=true,
                                Height=200., VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(margin))
let UpdateRichTextBox(tb:RichTextBox, i, j, z, mt:BackingStoreData.MapTile) =
    let fd = new System.Windows.Documents.FlowDocument()
    let p = new System.Windows.Documents.Paragraph()
    p.Inlines.Add(sprintf "(%02d,%02d)        %d screenshots\n" i j (mt.NumScreenshots()))
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
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Loaded.Add(fun _ ->
            theNotesWindow <- this
            this.Left <- 850.
            this.Top <- 500.
            )
        this.Closed.Add(fun _ ->
            theNotesWindow <- null
            )
        let b = new Border(Width=420., Height=300., BorderThickness=Thickness(0.), Background=Brushes.Black)
        this.Content <- b
        let tb = MakeRichTextBox(0.)
        tb.Height <- System.Double.NaN
        b.Child <- tb
        updateEv.Add(fun (x,y,zm) ->
            UpdateRichTextBox(tb, x, y, zm.Zone, zm.MapTiles.[x,y])
            )
    static member TheNotesWindow with get() = theNotesWindow and set(x) = theNotesWindow <- x

