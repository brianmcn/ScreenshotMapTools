module MapIcons

module NativeMethods =
    // Gemini
    open System.Runtime.InteropServices
    // Import the native Windows User32 library for keyboard event synthesis
    [<DllImport("user32.dll", SetLastError = true)>]
    extern void keybd_event(byte bVk, byte bScan, uint32 dwFlags, uint32 dwExtraInfo)
    // Virtual Key Constant Hex Codes
    let [<Literal>] VK_LWIN = 0x5Buy      // Left Windows Key
    let [<Literal>] VK_PERIOD = 0xBEuy    // Period / Dot Key
    // Key Event Flag Constants
    let [<Literal>] KEYEVENTF_KEYDOWN = 0x0000u
    let [<Literal>] KEYEVENTF_KEYUP = 0x0002u
    /// Synthesizes a Win + . (Windows + Period) keystroke to open the native OS Emoji Picker
    let ShowEmojiPicker() =
        // 1. Press Left Windows Key down
        keybd_event(VK_LWIN, 0uy, KEYEVENTF_KEYDOWN, 0u)
        // 2. Press Period Key down
        keybd_event(VK_PERIOD, 0uy, KEYEVENTF_KEYDOWN, 0u)
        // 3. Release Period Key
        keybd_event(VK_PERIOD, 0uy, KEYEVENTF_KEYUP, 0u)
        // 4. Release Left Windows Key
        keybd_event(VK_LWIN, 0uy, KEYEVENTF_KEYUP, 0u)
module Emoji =
    // Gemini
    open System
    open System.Linq
    open System.Globalization
    /// Safely inspects a string and returns true if any visual text element 
    /// is a simple emoji, surrogate pair, or complex composite emoji sequence.
    let ContainsEmoji(text: string) =
        if String.IsNullOrEmpty(text) then 
            false
        else
            // 1. Initialize an enumerator to break down the string into human-perceived visual blocks
            let enumerator = StringInfo.GetTextElementEnumerator(text)
            let mutable foundEmoji = false
            // 2. Loop through each text element (grapheme cluster)
            while enumerator.MoveNext() && not foundEmoji do
                let currentCluster = enumerator.GetTextElement()
                // 3. Evaluate the cluster structure
                let isSurrogate = Char.IsSurrogatePair(currentCluster, 0)
                let isEmojiCodePoint = currentCluster.Any(fun ch -> ch > '\u2600')
                if isSurrogate || isEmojiCodePoint then
                    foundEmoji <- true
            foundEmoji

open Utils.Extensions
let DoEmojiPickDialog(parentWindow) =
    let mkTB() = new System.Windows.Controls.TextBox(IsReadOnly=false, FontSize=12., Text="", BorderThickness=System.Windows.Thickness(1.), 
                                                        Foreground=System.Windows.Media.Brushes.Black, Background=System.Windows.Media.Brushes.White,
                                                        Width=300., Height=20., TextWrapping=System.Windows.TextWrapping.NoWrap, AcceptsReturn=false)
    let tb = mkTB()
    let closeEv = new Event<unit>()
    let mutable result = null
    tb.PreviewTextInput.Add(fun ea ->
        if Emoji.ContainsEmoji(ea.Text) then
            result <- ea.Text
        closeEv.Trigger()
        )
    Utils.DoModalDialogCore(parentWindow, tb, "Choose Emoji", closeEv.Publish, (fun() -> 
                                async {
                                    let ctxt = System.Threading.SynchronizationContext.Current
                                    System.Windows.Input.Keyboard.Focus(tb) |> ignore
                                    do! Async.Sleep(100)
                                    do! Async.SwitchToContext(ctxt)
                                    let currentSelection = tb.SelectionStart
                                    tb.CaretIndex <- currentSelection   // force WPF to send event to OS locating keyboard focus control
                                    do! Async.Sleep(100)
                                    do! Async.SwitchToContext(ctxt)
                                    NativeMethods.ShowEmojiPicker()
                                } |> Async.StartImmediate
                                ))
    if result = null then
        System.Windows.MessageBox.Show("Failed to capture this emoji") |> ignore
        null
    else
        result
        (*
        let closeEv = new Event<unit>()
        let mutable ok = false
        let cb = new System.Windows.Controls.Button(Content=" Cancel ", Margin=System.Windows.Thickness(4.))
        let sb = new System.Windows.Controls.Button(Content=" Ok ", Margin=System.Windows.Thickness(4.))
        cb.Click.Add(fun _ -> closeEv.Trigger())
        sb.Click.Add(fun _ -> ok <- true; closeEv.Trigger())
        let tb = new Emoji.Wpf.TextBox(IsReadOnly=true, FontSize=16., BorderThickness=System.Windows.Thickness(1.), 
                                                        Foreground=System.Windows.Media.Brushes.Black, Background=System.Windows.Media.Brushes.White,
                                                        Width=300., Height=70., TextWrapping=System.Windows.TextWrapping.NoWrap, AcceptsReturn=false)
        tb.Text <- sprintf "You chose\n%s\nIf that looks ok, click Ok" result
        let dp = (new System.Windows.Controls.DockPanel(LastChildFill=true)).AddLeft(cb).AddRight(sb).Add(new System.Windows.Controls.DockPanel())
        let sp = new System.Windows.Controls.StackPanel(Orientation=System.Windows.Controls.Orientation.Vertical)
        sp.Children.Add(tb) |> ignore
        sp.Children.Add(dp) |> ignore
        Utils.DoModalDialog(parentWindow, sp, "Verify Emoji", closeEv.Publish)
        if ok then
            result
        else
            null
        *)

(*
module DU =
    open Microsoft.FSharp.Reflection

    let toString (x:'a) = 
        let (case, _ ) = FSharpValue.GetUnionFields(x, typeof<'a>)
        case.Name

    let fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None
*)
let IST(w,h) =          // icon stroke thickness
    let m = min w h
    if m > 30. then 4. else 3.
[<RequireQualifiedAccess>]
type IconShape =
    | LargeOval
    | SmallOval
    | LargeBox
    | SmallBox
    | X
    | AlphaNum of char
    | AlphaNumLeft of char
    | AlphaNumRight of char
    | Emoji of string
    | EmojiLeft of string
    | EmojiRight of string
    static let writeAlpha(ch, c, w, brush, offset) =
        let typeface = new System.Windows.Media.Typeface(new System.Windows.Media.FontFamily("Courier New"), 
                                                            System.Windows.FontStyles.Normal, System.Windows.FontWeights.Bold, System.Windows.FontStretches.Normal)
        let mutable glyphTypeface = Unchecked.defaultof<System.Windows.Media.GlyphTypeface>
        if not(typeface.TryGetGlyphTypeface(&glyphTypeface)) then
            failwith "Glyph typeface could not be loaded"
        let glyphIndex = glyphTypeface.CharacterToGlyphMap.[int ch]   // rather than 'int ch' should be unicode code point for ch, but whatevs
        let mk(colorBrush) =
            let glyphs = new System.Windows.Documents.Glyphs()
            glyphs.FontUri <- glyphTypeface.FontUri
            glyphs.FontRenderingEmSize <- w/2.   // TODO adjust, espescially if weird w/h aspect ratio
            glyphs.Fill <- colorBrush
            glyphs.Indices <- glyphIndex.ToString()
            glyphs
        // faux-outline
        Utils.canvasAdd(c, mk(System.Windows.Media.Brushes.Black), offset + w/3.2 - 1., -1.)
        Utils.canvasAdd(c, mk(System.Windows.Media.Brushes.Black), offset + w/3.2 + 1., +1.)
        // the text
        Utils.canvasAdd(c, mk(brush), offset + w/3.2, 0.)
    static let writeEmoji(s, c, w, offset) =
        let tb = new Emoji.Wpf.TextBlock(FontSize=w/2.8, Text=s)
        Utils.canvasAdd(c, tb, offset+w/4.25, 0.)
    static member AllBasicShapes = [|      // the non-AlphaNum/Emoji
        IconShape.LargeOval
        IconShape.SmallOval
        IconShape.LargeBox
        IconShape.SmallBox
        IconShape.X 
        |]
    static member FromString(s) =
        match s with
        | "LargeOval" -> Some IconShape.LargeOval
        | "SmallOval" -> Some IconShape.SmallOval
        | "LargeBox" -> Some IconShape.LargeBox
        | "SmallBox" -> Some IconShape.SmallBox
        | "X" -> Some IconShape.X
        | _ -> 
            if s.StartsWith("AlphaNumLeft ") && s.Length = 14 && System.Char.IsLetterOrDigit(s.Chars(13)) then
                Some(IconShape.AlphaNumLeft(s.Chars(13)))
            elif s.StartsWith("AlphaNumRight ") && s.Length = 15 && System.Char.IsLetterOrDigit(s.Chars(14)) then
                Some(IconShape.AlphaNumRight(s.Chars(14)))
            elif s.StartsWith("AlphaNum ") && s.Length = 10 && System.Char.IsLetterOrDigit(s.Chars(9)) then
                Some(IconShape.AlphaNum(s.Chars(9)))
            elif s.StartsWith("EmojiLeft ") && Emoji.ContainsEmoji(s.Substring(10)) then
                Some(IconShape.EmojiLeft(s.Substring(10)))
            elif s.StartsWith("EmojiRight ") && Emoji.ContainsEmoji(s.Substring(11)) then
                Some(IconShape.EmojiRight(s.Substring(11)))
            elif s.StartsWith("Emoji ") && Emoji.ContainsEmoji(s.Substring(6)) then
                Some(IconShape.Emoji(s.Substring(6)))
            else
                None
    member this.AsString() =
        match this with
        | IconShape.LargeOval -> "LargeOval"
        | IconShape.SmallOval -> "SmallOval"
        | IconShape.LargeBox -> "LargeBox"
        | IconShape.SmallBox -> "SmallBox"
        | IconShape.X -> "X"
        | IconShape.AlphaNum c -> sprintf "AlphaNum %c" c
        | IconShape.AlphaNumLeft c -> sprintf "AlphaNumLeft %c" c
        | IconShape.AlphaNumRight c -> sprintf "AlphaNumRight %c" c
        | IconShape.Emoji s -> sprintf "Emoji %s" s
        | IconShape.EmojiLeft s -> sprintf "EmojiLeft %s" s
        | IconShape.EmojiRight s -> sprintf "EmojiRight %s" s
    member this.AddToCanvas(c:System.Windows.Controls.Canvas,brush,w,h) =
        match this with
        | IconShape.LargeOval -> 
            let s = new System.Windows.Shapes.Ellipse(Width=w*0.7, Height=h*0.7, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, w*0.15, h*0.15)
        | IconShape.SmallOval ->
            let s = new System.Windows.Shapes.Ellipse(Width=w*0.6, Height=h*0.6, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, w*0.2, h*0.2)
        | IconShape.LargeBox -> 
            let s = new System.Windows.Shapes.Rectangle(Width=w*0.7, Height=h*0.7, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, w*0.15, h*0.15)
        | IconShape.SmallBox -> 
            let s = new System.Windows.Shapes.Rectangle(Width=w*0.5, Height=h*0.6, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, w*0.25, h*0.2)
        | IconShape.X -> 
            let s = new System.Windows.Shapes.Line(X1=w*0.1, X2=w*0.9, Y1=h*0.1, Y2=h*0.9, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, 0., 0.)
            let s = new System.Windows.Shapes.Line(X2=w*0.1, X1=w*0.9, Y1=h*0.1, Y2=h*0.9, Stroke=brush, StrokeThickness=IST(w,h))
            Utils.canvasAdd(c, s, 0., 0.)
        | IconShape.AlphaNum ch -> writeAlpha(ch,c,w,brush,0.)
        | IconShape.AlphaNumLeft ch -> writeAlpha(ch,c,w,brush,-w/3.5)
        | IconShape.AlphaNumRight ch -> writeAlpha(ch,c,w,brush,w/3.5)
        | IconShape.Emoji s -> writeEmoji(s,c,w,0.)
        | IconShape.EmojiLeft s -> writeEmoji(s,c,w,-w/3.9)
        | IconShape.EmojiRight s -> writeEmoji(s,c,w,w/3.9)
    member this.GetAlphaNum() =
        match this with
        | IconShape.AlphaNum ch -> Some(ch)
        | IconShape.AlphaNumLeft ch -> Some(ch)
        | IconShape.AlphaNumRight ch -> Some(ch)
        | _ -> None
    member this.GetEmoji() =
        match this with
        | IconShape.Emoji s -> Some(s)
        | IconShape.EmojiLeft s -> Some(s)
        | IconShape.EmojiRight s -> Some(s)
        | _ -> None
        
[<AllowNullLiteral>]
type Icon() =
    static member IsValidRGBString(s:string) = s<>null && s.Length=6 && System.Text.RegularExpressions.Regex.IsMatch(s, "[0-9a-fA-F]*")
    member val Hashtag : string = null with get,set
    member val HexColorRGB : string = null with get,set       // e.g. 00FF00
    member val Shape : string = null with get,set
    member val IsEnabled : bool = true with get,set
    member this.IsValid =
        not(System.String.IsNullOrEmpty(this.Hashtag))
        && Icon.IsValidRGBString(this.HexColorRGB)
        && IconShape.FromString(this.Shape)<>None
    member this.GetColor() = Icon.GetColor(this.HexColorRGB)
    static member GetColor(hexColorRGB) = System.Windows.Media.ColorConverter.ConvertFromString("#FF" + hexColorRGB) :?> System.Windows.Media.Color
    static member Default(k) = new Icon(Hashtag=k, HexColorRGB="00FF00", Shape="LargeOval", IsEnabled=true)

// a (dx,dy) direction to offset icons, so e.g. a red X and a green X won't draw right on top of each other
let ColorJitters = [| 0,0; 0,-1; -1,0; 1,0; 0,1; -1,-1; 1,-1; -1,1; 1,1 |]
let NextColorJitter = 
    let mutable next = 0
    let f() =
        let r = next
        next <- (next + 1) % ColorJitters.Length
        r
    f

let hexColorUniverse = new System.Collections.Generic.Dictionary<_,_>()
do
    hexColorUniverse.Add("FF0000", NextColorJitter()) |> ignore  // red
    hexColorUniverse.Add("00FF00", NextColorJitter()) |> ignore  // lime
    hexColorUniverse.Add("FFFF00", NextColorJitter()) |> ignore  // yellow
//    hexColorUniverse.Add("EEDDEE", NextColorJitter()) |> ignore  // offwhitepurple
    // cyan gets added by virtue of the hover highlight existing, I think?

let GetIconFilename() = System.IO.Path.Combine(BackingStoreData.GetRootFolder(), "icons.json")
let LoadMapIconData() =
    let iconFile = GetIconFilename()
    if not(System.IO.File.Exists(iconFile)) then
        let sample = new Icon(Hashtag="thisIsASample", HexColorRGB="00aaFF", Shape="LargeOval", IsEnabled=true)
        let json = System.Text.Json.JsonSerializer.Serialize<Icon[]>( [|sample|] )
        GameSpecific.WriteAllText(iconFile, json)
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
                if not(hexColorUniverse.ContainsKey(i.HexColorRGB)) then
                    hexColorUniverse.Add(i.HexColorRGB, NextColorJitter()) |> ignore
        r

open System.Windows
open System.Windows.Controls
open System.Windows.Media

let KEYS_LIST_BOX_WIDTH = 150

let redrawPanelEv = new Event<unit>()          // when to redraw the keys panel
let redrawMapIconsEv = new Event<unit>()       // when to just redraw map icons (e.g. changed display for #foo)
let redrawMapIconHoverOnly = new Event<unit>() // when to just redraw the map icon hover layer (e.g. due to mouse rolling over different keys)  // also hijacked by QuickNav
let allIconsDisabledCheckbox = new CheckBox(IsChecked=false, Margin=Thickness(0.,0.,6.,0.)) // IsChecked is the readable global for whether to draw any icons or not
do
    allIconsDisabledCheckbox.Checked.Add(fun _ -> redrawMapIconsEv.Trigger())
    allIconsDisabledCheckbox.Unchecked.Add(fun _ -> redrawMapIconsEv.Trigger())
///////////////////////////
// text box search
let HOVER_DUMMY = "AAAAAhover"    // TODO a bit of a kludgy way to shoehorn it in, just for mapMarkerCaches
let REGEX_DUMMY = "AAAAA"    // TODO a bit of a kludgy way to shoehorn it in
let mutable userRegex = ""   // use this instead of key for k=REGEX_DUMMY in mapIconData.[k]
///////////////////////////
let mutable mapIconData = LoadMapIconData()
do
    match mapIconData.TryGetValue(REGEX_DUMMY) with
    | false, _ -> 
        let icon = Icon.Default(REGEX_DUMMY)
        icon.IsEnabled <- false
        mapIconData.Add(REGEX_DUMMY, icon)
    | true, _ -> ()
let SaveMapIconData() =
    let file = GetIconFilename()
    let icons = [| for k in mapIconData.Keys do yield mapIconData.[k] |] |> Array.sortBy (fun i -> i.Hashtag)
    let json = System.Text.Json.JsonSerializer.Serialize<Icon[]>(icons)
    GameSpecific.WriteAllText(file, json)
/////////////////////////
type SingleMapMarkerCache(origDraw) =    // caches a drawn map marker (BGRA pixel array) at a given W,H size
    let cache = new System.Collections.Generic.Dictionary<int*int,byte[]>()
    let mutable draw = origDraw
    member this.Get(w,h) =
        match cache.TryGetValue((w,h)) with
        | false, _ ->
            let c = new Canvas(Width=float w, Height=float h)
            draw(c,w,h)
            c.Measure(new Size(float w, float h))
            c.Arrange(new Rect(new Size(float w, float h)))
            let rtbmp = new System.Windows.Media.Imaging.RenderTargetBitmap(w, h, 96., 96., PixelFormats.Pbgra32)
            rtbmp.Render(c)
            let encoder = new System.Windows.Media.Imaging.PngBitmapEncoder()
            encoder.Frames.Add(System.Windows.Media.Imaging.BitmapFrame.Create(rtbmp))
            let ms = new System.IO.MemoryStream()
            encoder.Save(ms)
            let bmp = new System.Drawing.Bitmap(ms)
            let byteArray = Utils.ConvertBmpToBGRA(bmp)
            bmp.Dispose()
            cache.Add((w,h), byteArray)
            byteArray
        | true, r -> r
    member this.Invalidate(newDraw) =
        cache.Clear()
        draw <- newDraw
let mapMarkerCaches = new System.Collections.Generic.Dictionary<string,SingleMapMarkerCache>()
let updateMMC(k,icon:Icon) =
    let brush = new SolidColorBrush(icon.GetColor())
    let shape = IconShape.FromString(icon.Shape).Value
    let jitterDx, jitterDy =
        if shape.GetEmoji().IsSome then
            0,0 // don't jitter emojis, they have own colors and are thick such that jittering won't let others peek thru well
        else
            let idx = 
                match hexColorUniverse.TryGetValue(icon.HexColorRGB) with
                | true, r -> r
                | _ -> 0
            ColorJitters.[idx]
    let JITTERSCALE = 0.07
    let draw(c,w,h) = 
        let oc = new Canvas(Width=float w, Height=float h)
        shape.AddToCanvas(oc, brush, float w, float h)
        Utils.canvasAdd(c, oc, float w * JITTERSCALE * float jitterDx, float h * JITTERSCALE * float jitterDy)
    mapMarkerCaches.[k] <- new SingleMapMarkerCache(draw)
let GetOrCreateMapMarkerCaches(k) =
    if not(mapMarkerCaches.ContainsKey(k)) then
        updateMMC(k, Icon.Default(k))
    mapMarkerCaches.[k]
let drawHoverIcon(c,w,h) =
    let s = new System.Windows.Shapes.Ellipse(Width=float w*0.4, Height=float h*0.8, Stroke=Brushes.Cyan, StrokeThickness=IST(float w,float h))
    Utils.canvasAdd(c, s, float w*0.3, float h*0.1)
do
    for KeyValue(k,icon) in mapIconData do
        updateMMC(k,icon)
    mapMarkerCaches.[HOVER_DUMMY] <- new SingleMapMarkerCache(drawHoverIcon)
/////////////////////////
let mutable currentlyHoveredHashtagKey = null
let CW, CH = 32., 18.
let keyDrawFuncs = new System.Collections.Generic.Dictionary<_,option<(Canvas*_*_->_)> >()
let BG = Brushes.LightSteelBlue
let MakeIconUI(parentWindow, appMAPX) =
    let keys = InMemoryStore.metadataStore.AllKeys() |> Array.sort
    let g = Utils.makeGrid(1, keys.Length+2, KEYS_LIST_BOX_WIDTH, 20)  // +2 for (disable all,regex)
    g.Background <- BG
    let mkTxt(s) = new TextBox(IsReadOnly=true, FontSize=12., Text=s, BorderThickness=Thickness(0.), Foreground=Brushes.Black, Background=BG, IsHitTestVisible=false)
    let AddBorderMouseEnterLeaveBehaviors(b:Border, enterFunc, leaveFunc) =
        b.MouseEnter.Add(fun _ -> b.BorderBrush <- Brushes.Cyan; enterFunc())
        b.MouseLeave.Add(fun _ -> b.BorderBrush <- Brushes.Transparent; leaveFunc())
    do
        let dp = new DockPanel(LastChildFill=true, Background=BG)
        DockPanel.SetDock(allIconsDisabledCheckbox, Dock.Left)
        Utils.deparent(allIconsDisabledCheckbox)
        dp.Children.Add(allIconsDisabledCheckbox) |> ignore
        dp.Children.Add(mkTxt("(disable all)")) |> ignore
        let b = new Border(Child=dp, BorderThickness=Thickness(1.), BorderBrush=Brushes.Transparent, Background=BG)
        Utils.gridAdd(g, b, 0, 0)
    let mutable i = 1
    for k in Seq.append [REGEX_DUMMY] keys do
        let dp = new DockPanel(LastChildFill=true, Background=BG)
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
                let save, result = Utils.DoBasicModalTextDialogCore(parentWindow, "Change regex", "Type in a regular expression to match in Notes", userRegex, 300., 60., false)
                if save then
                    userRegex <- result
                    refreshButton()
                    mapIconData.[REGEX_DUMMY].IsEnabled <- true  // ensure enabled after editing
                    eval()
                    redrawMapIconsEv.Trigger()
                )
            let but = new DockPanel(LastChildFill=false)
            let desc = mkTxt("RE: ")
            DockPanel.SetDock(desc, Dock.Left)
            but.Children.Add(desc) |> ignore
            but.Children.Add(regexButton) |> ignore
            dp.Children.Add(but) |> ignore
        else
            let mutable all, here = 0,0
            let zoneCounts = Array.zeroCreate 100
            for loc in InMemoryStore.metadataStore.LocationsForKey(k) do
                if loc.Zone = BackingStoreData.theGame.CurZone then
                    here <- here + 1
                all <- all + 1
                zoneCounts.[loc.Zone] <- zoneCounts.[loc.Zone] + 1
            let textBox = mkTxt(sprintf "%s (%d/%d)" k here all)
            let mutable tooltip = ""
            for i = 0 to 99 do
                if zoneCounts.[i] > 0 then
                    tooltip <- tooltip + sprintf "%d zone%02d: %s\n" zoneCounts.[i] i BackingStoreData.theGame.ZoneNames.[i]
            textBox.IsHitTestVisible <- true
            ToolTipService.SetToolTip(textBox, tooltip)
            dp.Children.Add(textBox) |> ignore
        let b = new Border(Child=dp, BorderThickness=Thickness(1.), BorderBrush=Brushes.Gray, Background=Brushes.White)
        Utils.gridAdd(g, b, 0, i)
        i <- i + 1
        AddBorderMouseEnterLeaveBehaviors(b, 
            (fun _ -> currentlyHoveredHashtagKey <- k; redrawMapIconHoverOnly.Trigger()),
            (fun _ -> currentlyHoveredHashtagKey <- null; redrawMapIconHoverOnly.Trigger())
            )
        b.MouseDown.Add(fun me ->
            let currentSelectionBorder = Brushes.Magenta
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

                let dp = new DockPanel(MaxWidth=400., MaxHeight=400., LastChildFill=true, HorizontalAlignment=HorizontalAlignment.Center)

                // shapes
                let rec redrawShapeSelector() =
                    dp.Children.RemoveAt(dp.Children.Count-1)
                    dp.Children.Add(new ScrollViewer(Content=makeShapeSelector(), VerticalScrollBarVisibility=ScrollBarVisibility.Auto)) |> ignore
                and makeShapeSelector() =   // function, as it references current color/glyph/emoji, draw a new one when change
                    let curBrush = new SolidColorBrush(cur.GetColor())
                    let shapeSelector = Utils.makeGrid(2, max 6 IconShape.AllBasicShapes.Length, 80, 52)
                    let mutable i = 0
                    let allBorders = ResizeArray()
                    for s in IconShape.AllBasicShapes do
                        let c = new Canvas(Width=64., Height=36., Background=Brushes.Black)   // TODO contrast bg with cur color?
                        s.AddToCanvas(c, curBrush, c.Width, c.Height)
                        let b = new Border(Child=new Border(Child=c, BorderThickness=Thickness(4.), BorderBrush=BG), BorderThickness=Thickness(4.), BorderBrush=Brushes.Transparent)
                        Utils.gridAdd(shapeSelector, b, 0, i)
                        i <- i + 1
                        if cur.Shape = s.AsString() then
                            b.BorderBrush <- currentSelectionBorder
                        allBorders.Add(b)
                        b.MouseDown.Add(fun _ ->
                            for x in allBorders do
                                x.BorderBrush <- Brushes.Transparent
                            b.BorderBrush <- currentSelectionBorder
                            cur.Shape <- s.AsString()
                            )
                    // AlphaNum options
                    i <- 0
                    let center(ch) = IconShape.AlphaNum(ch)
                    let left(ch) = IconShape.AlphaNumLeft(ch)
                    let right(ch) = IconShape.AlphaNumRight(ch)
                    for which in [center; left; right] do
                        let c = new Canvas(Width=64., Height=36., Background=Brushes.Black)   // TODO contrast bg with cur color?
                        let curShape = IconShape.FromString(cur.Shape)
                        let s,curGlyph = 
                            let r = match curShape with | Some(sh) -> sh.GetAlphaNum() | _ -> None
                            match r with 
                            | Some(ch) -> which(ch), ch
                            | _ -> which('A'), 'A'
                        s.AddToCanvas(c, curBrush, c.Width, c.Height)
                        let b = new Border(Child=new Border(Child=c, BorderThickness=Thickness(4.), BorderBrush=BG), BorderThickness=Thickness(4.), BorderBrush=Brushes.Transparent)
                        Utils.gridAdd(shapeSelector, b, 1, i)
                        i <- i + 1
                        if cur.Shape = s.AsString() then
                            b.BorderBrush <- currentSelectionBorder
                        allBorders.Add(b)
                        b.MouseDown.Add(fun _ ->
                            for x in allBorders do
                                x.BorderBrush <- Brushes.Transparent
                            b.BorderBrush <- currentSelectionBorder
                            Winterop.Win32.SetForegroundWindow((new System.Windows.Interop.WindowInteropHelper(parentWindow)).Handle) |> ignore
                            let save, result = 
                                Utils.DoBasicModalTextDialog(parentWindow, "Single alphanumeric character label", curGlyph.ToString(), float(appMAPX/2), float(appMAPX/2), false)
                            let r = 
                                if save && result.Length = 1 && System.Char.IsLetterOrDigit(result.Chars(0)) then
                                    result.Chars(0)
                                else
                                    curGlyph
                            let newShape = which(r)
                            cur.Shape <- newShape.AsString()
                            // redraw UI, we're still in a modal dialog where they may change color after editing glyph
                            redrawShapeSelector()
                            )
                    // Emoji options
                    let center(ch) = IconShape.Emoji(ch)
                    let left(ch) = IconShape.EmojiLeft(ch)
                    let right(ch) = IconShape.EmojiRight(ch)
                    for which in [center; left; right] do
                        let c = new Canvas(Width=64., Height=36., Background=Brushes.Black)
                        let curShape = IconShape.FromString(cur.Shape)
                        let s = 
                            let r = match curShape with | Some(sh) -> sh.GetEmoji() | _ -> None
                            match r with 
                            | Some(s) -> which(s)
                            | _ -> which("😊")
                        s.AddToCanvas(c, curBrush, c.Width, c.Height)
                        let b = new Border(Child=new Border(Child=c, BorderThickness=Thickness(4.), BorderBrush=BG), BorderThickness=Thickness(4.), BorderBrush=Brushes.Transparent)
                        Utils.gridAdd(shapeSelector, b, 1, i)
                        i <- i + 1
                        if cur.Shape = s.AsString() then
                            b.BorderBrush <- currentSelectionBorder
                        allBorders.Add(b)
                        b.MouseDown.Add(fun _ ->
                            for x in allBorders do
                                x.BorderBrush <- Brushes.Transparent
                            b.BorderBrush <- currentSelectionBorder
                            Winterop.Win32.SetForegroundWindow((new System.Windows.Interop.WindowInteropHelper(parentWindow)).Handle) |> ignore
                            let result = DoEmojiPickDialog(parentWindow)
                            let newShape = if result=null then s else which(result)
                            cur.Shape <- newShape.AsString()
                            // redraw UI, we're still in a modal dialog where they may change color after editing glyph
                            redrawShapeSelector()
                            )
                    shapeSelector
                
                // colors
                let COLORW,COLORH,BORDER = 40, 30, 4
                let colorSelector = Utils.makeGrid(1, hexColorUniverse.Count, COLORW+4*BORDER, COLORH+4*BORDER)
                let mutable i = 0
                let all = ResizeArray()
                for KeyValue(c,_) in hexColorUniverse do
                    let col = Icon.GetColor(c)
                    let swatch = new DockPanel(Width=float COLORW, Height=float COLORH, Background=new SolidColorBrush(col))
                    ToolTipService.SetToolTip(swatch, col)
                    let b = new Border(Child=new Border(Child=swatch, BorderThickness=Thickness(float BORDER), BorderBrush=BG), BorderThickness=Thickness(float BORDER), BorderBrush=Brushes.Transparent)
                    Utils.gridAdd(colorSelector, b, 0, i)
                    i <- i + 1
                    if cur.HexColorRGB = c then
                        b.BorderBrush <- currentSelectionBorder
                    all.Add(b)
                    b.MouseDown.Add(fun _ ->
                        for x in all do
                            x.BorderBrush <- Brushes.Transparent
                        b.BorderBrush <- currentSelectionBorder
                        cur.HexColorRGB <- c
                        redrawShapeSelector()
                        )

                dp.Children.Add(new ScrollViewer(Content=colorSelector, VerticalScrollBarVisibility=ScrollBarVisibility.Auto, Margin=Thickness(0.,0.,10.,0.))) |> ignore
                dp.Children.Add(new ScrollViewer(Content=makeShapeSelector(), VerticalScrollBarVisibility=ScrollBarVisibility.Auto)) |> ignore
                let closeEv = new Event<unit>()
                let total = new DockPanel(LastChildFill=true, Margin=Thickness(4.), Background=BG, MinWidth=250., MaxWidth=400.)
                let addColorButton = new Button(Content="Add color", MaxWidth=150., Margin=Thickness(4.))
                addColorButton.Click.Add(fun _ ->
                    let save, result = Utils.DoBasicModalTextDialog(parentWindow, "New hex color RRGGBB", "00FF00", float(appMAPX/2), float(appMAPX/2), false)
                    if save then
                        if Icon.IsValidRGBString(result) then
                            hexColorUniverse.Add(result, NextColorJitter()) |> ignore
                    closeEv.Trigger()   // rather than redraw UI, just closing it, user can open again
                    )
                let doneButton = new Button(Content="Done", MaxWidth=150., Margin=Thickness(4.))
                doneButton.Click.Add(fun _ -> closeEv.Trigger())
                let sp = new StackPanel(Orientation=Orientation.Horizontal, HorizontalAlignment=HorizontalAlignment.Center)
                sp.Children.Add(addColorButton) |> ignore
                sp.Children.Add(doneButton) |> ignore
                DockPanel.SetDock(sp, Dock.Bottom)
                total.Children.Add(sp) |> ignore
                let header = mkTxt(sprintf "Choose appearance for #%s" k)
                header.BorderThickness <- Thickness(1.)
                header.HorizontalAlignment <- HorizontalAlignment.Center
                DockPanel.SetDock(header, Dock.Top)
                total.Children.Add(header) |> ignore
                total.Children.Add(dp) |> ignore
                Utils.DoModalDialog(parentWindow, total, "Appearance", closeEv.Publish)
                mapIconData.[k] <- cur
                updateMMC(k,cur)
            else
                // just toggle enable on left click (and initialize if no default yet)
                match mapIconData.TryGetValue(k) with
                | false, _ ->
                    let icon = Icon.Default(k)
                    mapIconData.[k] <- icon
                    updateMMC(k,icon)
                | true, icon ->
                    icon.IsEnabled <- not icon.IsEnabled
            eval()
            redrawMapIconsEv.Trigger()
            SaveMapIconData()
            )
    let sv = new ScrollViewer(Width=float KEYS_LIST_BOX_WIDTH, Margin=Thickness(4.), Content=g,
                                VerticalScrollBarVisibility=ScrollBarVisibility.Auto, HorizontalScrollBarVisibility=ScrollBarVisibility.Auto)
    sv

