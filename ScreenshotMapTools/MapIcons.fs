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

let IST(w,h) = 
    let m = min w h
    if m > 30. then 3. else 2.
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
    member this.AsString() =
        match this with
        | IconShape.LargeOval -> "LargeOval"
        | IconShape.SmallOval -> "SmallOval"
        | IconShape.LargeBox -> "LargeBox"
        | IconShape.SmallBox -> "SmallBox"
        | IconShape.X -> "X"
    member this.AddToCanvas(c:System.Windows.Controls.Canvas,brush,w,h) =
        match this with
        | IconShape.LargeOval -> 
            let s = new System.Windows.Shapes.Ellipse(Width=w, Height=h, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, 0., 0.)
        | IconShape.SmallOval ->
            let s = new System.Windows.Shapes.Ellipse(Width=w*0.6, Height=h*0.6, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, w*0.2, h*0.2)
        | IconShape.LargeBox -> 
            let s = new System.Windows.Shapes.Rectangle(Width=w*0.9, Height=h*0.9, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, w*0.05, h*0.05)
        | IconShape.SmallBox -> 
            let s = new System.Windows.Shapes.Rectangle(Width=w*0.5, Height=h*0.7, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, w*0.25, h*0.15)
        | IconShape.X -> 
            let s = new System.Windows.Shapes.Line(X1=w*0.1, X2=w*0.9, Y1=h*0.1, Y2=h*0.9, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, 0., 0.)
            let s = new System.Windows.Shapes.Line(X2=w*0.1, X1=w*0.9, Y1=h*0.1, Y2=h*0.9, Stroke=brush, StrokeThickness=IST(w,h))
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
    member this.GetColor() = Icon.GetColor(this.HexColorRGB)
    static member GetColor(hexColorRGB) = System.Windows.Media.ColorConverter.ConvertFromString("#FF" + hexColorRGB) :?> System.Windows.Media.Color

let hexColorUniverse = new System.Collections.Generic.HashSet<_>()
do
    hexColorUniverse.Add("FF0000") |> ignore  // red
    hexColorUniverse.Add("00FF00") |> ignore  // lime
    hexColorUniverse.Add("FFFF00") |> ignore  // yellow

let GetIconFilename() = System.IO.Path.Combine(BackingStoreData.GetRootFolder(), "icons.json")
let LoadMapIconData() =
    let iconFile = GetIconFilename()
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
                hexColorUniverse.Add(i.HexColorRGB) |> ignore
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
///////////////////////////
// text box search
let REGEX_DUMMY = "AAAAA"    // TODO a bit of a kludgy way to shoehorn it in
let mutable userRegex = ""   // use this instead of key for k=REGEX_DUMMY in mapIconData.[k]
///////////////////////////
let mutable mapIconData = LoadMapIconData()
let SaveMapIconData() =
    let file = GetIconFilename()
    let icons = [| for k in mapIconData.Keys do yield mapIconData.[k] |] |> Array.sortBy (fun i -> i.Hashtag)
    let json = System.Text.Json.JsonSerializer.Serialize<Icon[]>(icons)
    BackingStoreData.WriteAllText(file, json)
let mutable currentlyHoveredHashtagKey = null
let CW, CH = 32., 18.
let drawHoverIcon(c,w,h) =
    let s = new System.Windows.Shapes.Ellipse(Width=w*0.4, Height=h*0.8, Stroke=Brushes.Cyan, StrokeThickness=IST(w,h))
    Utils.canvasAdd(c, s, w*0.3, h*0.1)
let keyDrawFuncs = new System.Collections.Generic.Dictionary<_,option<(Canvas*_*_->_)> >()
let MakeIconUI(parentWindow) =
    let keys = InMemoryStore.metadataStore.AllKeys() |> Array.sort
    let g = Utils.makeGrid(1, keys.Length+2, KEYS_LIST_BOX_WIDTH, 20)  // +2 for (disable all,regex)
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
    for k in Seq.append [REGEX_DUMMY] keys do
        let dp = new DockPanel(LastChildFill=true)
        let c = new Canvas(Width=CW, Height=CH, Margin=Thickness(0.,0.,4.,0.), Background=Brushes.Black)
        let eval() =
            c.Children.Clear()
            keyDrawFuncs.[k] <- None
            match mapIconData.TryGetValue(k) with
            | false, _ -> ()
            | true, icon ->
                if icon.IsEnabled then
                    let brush = new SolidColorBrush(icon.GetColor())
                    let shape = IconShape.FromString(icon.Shape).Value
                    let draw(c,w,h) = 
                        shape.AddToCanvas(c, brush, w, h)
                    draw(c, CW, CH)
                    keyDrawFuncs.[k] <- Some draw
        eval()
        DockPanel.SetDock(c, Dock.Left)
        dp.Children.Add(c) |> ignore
        if k = REGEX_DUMMY then
            let regexButton = new Button()
            let refreshButton() =
                regexButton.Content <- (let tb = mkTxt(if userRegex="" then "(click here)" else userRegex) in tb.FontSize<-8. ; tb)
            refreshButton()
            regexButton.Click.Add(fun _ ->
                let label = mkTxt("Type a regex")
                label.Margin <- Thickness(2.)
                let edit = new TextBox(IsReadOnly=false, FontSize=12., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.White, Text=userRegex, Margin=Thickness(2.))
                let closeEv = new Event<unit>()
                let closeButton = new Button(Content="Done", Margin=Thickness(2.))
                closeButton.Click.Add(fun _ -> closeEv.Trigger())
                let dp = new DockPanel(LastChildFill=true)
                DockPanel.SetDock(label, Dock.Top)
                DockPanel.SetDock(closeButton, Dock.Bottom)
                dp.Children.Add(label) |> ignore
                dp.Children.Add(closeButton) |> ignore
                dp.Children.Add(edit) |> ignore
                Utils.DoModalDialog(parentWindow, dp, "Change user regex", closeEv.Publish)
                userRegex <- edit.Text
                refreshButton()
                )
            let but = new DockPanel(LastChildFill=false)
            let desc = mkTxt("RE: ")
            DockPanel.SetDock(desc, Dock.Left)
            but.Children.Add(desc) |> ignore
            but.Children.Add(regexButton) |> ignore
            dp.Children.Add(but) |> ignore
        else
            dp.Children.Add(mkTxt(k)) |> ignore                    // TODO consider displaying count
        let b = new Border(Child=dp, BorderThickness=Thickness(1.), BorderBrush=Brushes.Transparent, Background=Brushes.White)
        Utils.gridAdd(g, b, 0, i)
        i <- i + 1
        AddBorderMouseEnterLeaveBehaviors(b, 
            (fun _ -> currentlyHoveredHashtagKey <- k; redrawMapIconHoverOnly.Trigger()),
            (fun _ -> currentlyHoveredHashtagKey <- null; redrawMapIconHoverOnly.Trigger())
            )
        b.MouseDown.Add(fun me ->
            if me.RightButton = Input.MouseButtonState.Pressed then
                // dialog window to select color and shape
                let cur = 
                    match mapIconData.TryGetValue(k) with
                    | false, _ ->
                        let icon = new Icon(Hashtag=k, HexColorRGB="00FF00", Shape="LargeOval", IsEnabled=true)
                        mapIconData.[k] <- icon
                        icon
                    | true, icon ->
                        icon.IsEnabled <- true
                        icon

                let dp = new DockPanel(MinWidth=200., MaxWidth=400., MaxHeight=400., LastChildFill=true)

                // shapes
                let mk() =
                    let curBrush = new SolidColorBrush(cur.GetColor())
                    let shapeSelector = Utils.makeGrid(1, IconShape.All.Length, 72, 44)
                    let mutable i = 0
                    let all = ResizeArray()
                    for s in IconShape.All do
                        let c = new Canvas(Width=64., Height=36., Background=Brushes.Black)   // TODO contrast bg with cur color?
                        s.AddToCanvas(c, curBrush, c.Width, c.Height)
                        let b = new Border(Child=c, BorderThickness=Thickness(4.), BorderBrush=Brushes.Transparent)
                        Utils.gridAdd(shapeSelector, b, 0, i)
                        i <- i + 1
                        if cur.Shape = s.AsString() then
                            b.BorderBrush <- Brushes.Cyan
                        all.Add(b)
                        b.MouseDown.Add(fun _ ->
                            for x in all do
                                x.BorderBrush <- Brushes.Transparent
                                b.BorderBrush <- Brushes.Cyan
                                cur.Shape <- s.AsString()
                            )
                    shapeSelector
                
                // colors
                let colorSelector = Utils.makeGrid(1, hexColorUniverse.Count, 48, 48)
                let mutable i = 0
                let all = ResizeArray()
                for c in hexColorUniverse do
                    let col = Icon.GetColor(c)
                    let swatch = new DockPanel(Width=40., Height=40., Background=new SolidColorBrush(col))
                    let b = new Border(Child=swatch, BorderThickness=Thickness(4.), BorderBrush=Brushes.Transparent)
                    Utils.gridAdd(colorSelector, b, 0, i)
                    i <- i + 1
                    if cur.HexColorRGB = c then
                        b.BorderBrush <- Brushes.Cyan
                    all.Add(b)
                    b.MouseDown.Add(fun _ ->
                        for x in all do
                            x.BorderBrush <- Brushes.Transparent
                            b.BorderBrush <- Brushes.Cyan
                            cur.HexColorRGB <- c
                            dp.Children.RemoveAt(dp.Children.Count-1)
                            dp.Children.Add(new ScrollViewer(Content=mk(), VerticalScrollBarVisibility=ScrollBarVisibility.Auto)) |> ignore
                        )

                dp.Children.Add(new ScrollViewer(Content=colorSelector, VerticalScrollBarVisibility=ScrollBarVisibility.Auto)) |> ignore
                dp.Children.Add(new ScrollViewer(Content=mk(), VerticalScrollBarVisibility=ScrollBarVisibility.Auto)) |> ignore
                let closeEv = new Event<unit>()
                let total = new DockPanel(LastChildFill=true, Margin=Thickness(4.))
                let doneButton = new Button(Content="Done", MaxWidth=150., Margin=Thickness(4.))
                doneButton.Click.Add(fun _ -> closeEv.Trigger())
                DockPanel.SetDock(doneButton, Dock.Bottom)
                total.Children.Add(doneButton) |> ignore
                total.Children.Add(dp) |> ignore
                Utils.DoModalDialog(parentWindow, total, "Choose appearance", closeEv.Publish)
                mapIconData.[k] <- cur
            else
                // just toggle enable on left click (and initialize if no default yet)
                match mapIconData.TryGetValue(k) with
                | false, _ ->
                    mapIconData.[k] <- new Icon(Hashtag=k, HexColorRGB="00FF00", Shape="LargeOval", IsEnabled=true)
                | true, icon ->
                    icon.IsEnabled <- not icon.IsEnabled
            eval()
            redrawMapIconsEv.Trigger()
            SaveMapIconData()
            )
    let sv = new ScrollViewer(Width=float KEYS_LIST_BOX_WIDTH, Margin=Thickness(4.), MaxHeight=350., Content=g,
                                VerticalScrollBarVisibility=ScrollBarVisibility.Auto, HorizontalScrollBarVisibility=ScrollBarVisibility.Auto)
    sv

