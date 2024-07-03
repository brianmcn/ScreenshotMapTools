module MapIcons

module DU =
    open Microsoft.FSharp.Reflection

    let toString (x:'a) = 
        let (case, _ ) = FSharpValue.GetUnionFields(x, typeof<'a>)
        case.Name

    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None

[<RequireQualifiedAccess>]
type IconShape =
    | LargeOval
    | SmallOval
    | LargeBox
    | SmallBox
    | X
    static member All = [| 
        IconShape.LargeOval
        IconShape.SmallOval
        IconShape.LargeBox
        IconShape.SmallBox
        IconShape.X 
        |]
    static member FromString(s) = DU.fromString<IconShape>(s)
    member this.AddToCanvas(c:System.Windows.Controls.Canvas,brush,w,h) =
        match this with
        | IconShape.LargeOval -> 
            let s = new System.Windows.Shapes.Ellipse(Width=w, Height=h, Stroke=brush, StrokeThickness=3.)
            Utils.canvasAdd(c, s, 0., 0.)
        | IconShape.SmallOval ->
            let s = new System.Windows.Shapes.Ellipse(Width=w*0.6, Height=h*0.6, Stroke=brush, StrokeThickness=3.)
            Utils.canvasAdd(c, s, w*0.2, h*0.2)
        | IconShape.LargeBox -> 
            let s = new System.Windows.Shapes.Rectangle(Width=w*0.9, Height=h*0.9, Stroke=brush, StrokeThickness=3.)
            Utils.canvasAdd(c, s, w*0.05, h*0.05)
        | IconShape.SmallBox -> 
            let s = new System.Windows.Shapes.Rectangle(Width=w*0.5, Height=h*0.5, Stroke=brush, StrokeThickness=3.)
            Utils.canvasAdd(c, s, w*0.25, h*0.25)
        | IconShape.X -> 
            let s = new System.Windows.Shapes.Line(X1=w*0.1, X2=w*0.9, Y1=h*0.1, Y2=h*0.9, Stroke=brush, StrokeThickness=3.)
            Utils.canvasAdd(c, s, 0., 0.)
            let s = new System.Windows.Shapes.Line(X2=w*0.1, X1=w*0.9, Y1=h*0.1, Y2=h*0.9, Stroke=brush, StrokeThickness=3.)
            Utils.canvasAdd(c, s, 0., 0.)
        
[<AllowNullLiteral>]
type Icon() =
    member val Hashtag : string = null with get,set
    member val HexColorRGB : string = null with get,set       // e.g. 00FF00
    member val Shape : string = null with get,set
    member val IsEnabled : bool = true with get,set
    member this.IsValid =
        not(System.String.IsNullOrEmpty(this.Hashtag))
        && this.HexColorRGB<>null && this.HexColorRGB.Length=6 && System.Text.RegularExpressions.Regex.IsMatch(this.HexColorRGB, "[0-9a-fA-F]*")
        && IconShape.FromString(this.Shape)<>None

let LoadMapIconData() =
    let iconFile = System.IO.Path.Combine(BackingStoreData.GetRootFolder(), "icons.json")
    if not(System.IO.File.Exists(iconFile)) then
        let sample = new Icon(Hashtag="thisIsASample", HexColorRGB="00aaFF", Shape="LargeOval", IsEnabled=true)
        let json = System.Text.Json.JsonSerializer.Serialize<Icon[]>( [|sample|] )
        BackingStoreData.WriteAllText(iconFile, json)
        new System.Collections.Generic.Dictionary<_,_>()
    else
        let json = System.IO.File.ReadAllText(iconFile)
        let data = System.Text.Json.JsonSerializer.Deserialize<Icon[]>(json)
        let r = new System.Collections.Generic.Dictionary<_,_>()
        for i in data do
            if not i.IsValid then
                printfn "Bad data in icon file '%s':" iconFile
                printfn "    %s" (System.Text.Json.JsonSerializer.Serialize(i))
                printfn "is being ignored."
            else
                r.[i.Hashtag] <- i
        r

open System.Windows
open System.Windows.Controls
open System.Windows.Media

let KEYS_LIST_BOX_WIDTH = 150

let redrawPanelEv = new Event<unit>()          // when to redraw the keys panel
let redrawMapIconsEv = new Event<unit>()       // when to just redraw map icons (e.g. changed display for #foo)
let redrawMapIconHoverOnly = new Event<unit>() // when to just redraw the map icon hover layer (e.g. due to mouse rolling over different keys)
let allIconsDisabledCheckbox = new CheckBox(IsChecked=false, Margin=Thickness(0.,0.,6.,0.)) // IsChecked is the readable global for whether to draw any icons or not
do
    allIconsDisabledCheckbox.Checked.Add(fun _ -> redrawMapIconsEv.Trigger())
    allIconsDisabledCheckbox.Unchecked.Add(fun _ -> redrawMapIconsEv.Trigger())
let mutable mapIconData = LoadMapIconData()  // TODO writing it back to disk
let mutable currentlyHoveredHashtagKey = null
let CW, CH = 32., 18.
let drawHoverIcon(c,w,h) = IconShape.SmallOval.AddToCanvas(c, Brushes.Cyan, w, h)
let keyDrawFuncs = new System.Collections.Generic.Dictionary<_,(Canvas*_*_->_)>()
let MakeIconUI() =
    let keys = InMemoryStore.metadataStore.AllKeys() |> Array.sort
    let g = Utils.makeGrid(1, keys.Length+1, KEYS_LIST_BOX_WIDTH, 20)
    let mkTxt(s) = new TextBox(IsReadOnly=true, FontSize=12., Text=s, BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=Brushes.White, IsHitTestVisible=false)
    let AddBorderMouseEnterLeaveBehaviors(b:Border, enterFunc, leaveFunc) =
        b.MouseEnter.Add(fun _ -> b.BorderBrush <- Brushes.Cyan; enterFunc())
        b.MouseLeave.Add(fun _ -> b.BorderBrush <- Brushes.Transparent; leaveFunc())
    do
        let dp = new DockPanel(LastChildFill=true)
        DockPanel.SetDock(allIconsDisabledCheckbox, Dock.Left)
        Utils.deparent(allIconsDisabledCheckbox)
        dp.Children.Add(allIconsDisabledCheckbox) |> ignore
        dp.Children.Add(mkTxt("(disable all)")) |> ignore
        let b = new Border(Child=dp, BorderThickness=Thickness(1.), BorderBrush=Brushes.Transparent, Background=Brushes.White)
        Utils.gridAdd(g, b, 0, 0)
    let mutable i = 1
    for k in keys do
        let dp = new DockPanel(LastChildFill=true)
        let c = new Canvas(Width=CW, Height=CH, Margin=Thickness(0.,0.,4.,0.), Background=Brushes.Black)
        let eval() =
            c.Children.Clear()
            keyDrawFuncs.[k] <- fun _ -> ()
            match mapIconData.TryGetValue(k) with
            | false, _ -> ()
            | true, icon ->
                if icon.IsEnabled then
                    let brush = new SolidColorBrush(ColorConverter.ConvertFromString("#FF" + icon.HexColorRGB) :?> Color)
                    let shape = IconShape.FromString(icon.Shape).Value
                    let draw(c,w,h) = 
                        shape.AddToCanvas(c, brush, w, h)
                    draw(c, CW, CH)
                    keyDrawFuncs.[k] <- draw
        eval()
        DockPanel.SetDock(c, Dock.Left)
        dp.Children.Add(c) |> ignore
        dp.Children.Add(mkTxt(k)) |> ignore                    // TODO consider displaying count
        let b = new Border(Child=dp, BorderThickness=Thickness(1.), BorderBrush=Brushes.Transparent, Background=Brushes.White)
        Utils.gridAdd(g, b, 0, i)
        i <- i + 1
        AddBorderMouseEnterLeaveBehaviors(b, 
            (fun _ -> currentlyHoveredHashtagKey <- k; redrawMapIconHoverOnly.Trigger()),
            (fun _ -> currentlyHoveredHashtagKey <- null; redrawMapIconHoverOnly.Trigger())
            )
        b.MouseDown.Add(fun me ->
            // TODO deeper UI on right click
            match mapIconData.TryGetValue(k) with
            | false, _ ->
                mapIconData.[k] <- new Icon(Hashtag=k, HexColorRGB="00FF00", Shape="LargeOval", IsEnabled=true)
            | true, icon ->
                icon.IsEnabled <- not icon.IsEnabled
            eval()
            redrawMapIconsEv.Trigger()
            )
    let sv = new ScrollViewer(Width=float KEYS_LIST_BOX_WIDTH, Margin=Thickness(4.), MaxHeight=350., Content=g,
                                VerticalScrollBarVisibility=ScrollBarVisibility.Auto, HorizontalScrollBarVisibility=ScrollBarVisibility.Auto)
    sv

