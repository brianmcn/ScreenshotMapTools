module PopoutsSettings

open System.Windows
open System.Windows.Controls
open System.Windows.Media

type IPopoutWindowBehavior =
    abstract member Activate : unit -> unit
    abstract member Close : unit -> unit
    abstract member GetJson : unit -> AppSettings.PopoutDetailJson

let mkTxt(txt) = new TextBlock(FontSize=16., Text=txt, Foreground=Brushes.Black, Background=Brushes.Gray, TextWrapping=TextWrapping.Wrap, 
                                    Margin=Thickness(3., 0., 3., 0.), VerticalAlignment=VerticalAlignment.Center)
let mkTxtWithStarStarBold(txt:string) = 
    let tb = mkTxt("")
    tb.Inlines.Clear()
    let parts = txt.Split([|"**"|], System.StringSplitOptions.None)
    for i = 0 to parts.Length-1 do
        let isBold = (i%2 = 1)
        let run = new System.Windows.Documents.Run(parts.[i])
        run.FontWeight <- if isBold then FontWeights.Bold else FontWeights.Normal
        tb.Inlines.Add(run)
    tb
let Bold(tb:TextBlock) = tb.FontWeight <- FontWeights.Bold; tb

let makePopoutSettingsDialogElement(ccs: IPopoutWindowBehavior, lm:IPopoutWindowBehavior, ln:IPopoutWindowBehavior, mp:IPopoutWindowBehavior, pn:IPopoutWindowBehavior, width) =
    let instructions1 = mkTxt("There are a variety of popout windows which you can choose to enable.")
    let instructions2 = mkTxtWithStarStarBold("""**Each window remembers its size and location you last used it**, for convenience.  
But if a window ever gets 'lost' (offscreen, on another monitor, etc) you can press its 'Reset' button (above) to restore it.

Popout windows have no 'Window Chrome' and are controlled thusly:
 - **move popout**: just **left-click and drag** the window
 - **resize popout**: grab and **drag the edges** of the window like a normal window
 - **close popout**: simply **right-click** a popout window to close it

Most of these windows are designed to serve one or both of the following use-cases:
(1) if you have limited primary screen real estate, you might leave the app itself open on a secondary monitor, and just have e.g. the AppGridPane and CursorNote in the corner of your primary monitor
(2) if you are capturing video (e.g. for Twitch) it usually does not make sense to capture the app window, but often makes sense to incorporate e.g. the Minimap and CursorNote windows into your OBS layout so that the audience can see some map/notes as you play

The app remembers which popouts you enabled, so next time you start the app, it will also open your enabled popouts.""")
    let g,_ = BasicLayout.NewGridWithColumns([0.,GridUnitType.Auto; 0.,GridUnitType.Auto; 0.,GridUnitType.Auto; 0.,GridUnitType.Auto; 1., GridUnitType.Star; 0.,GridUnitType.Auto; 0.,GridUnitType.Auto])
    let mutable row = 0
    let vruleElement() = new DockPanel(Background=Brushes.Black, Width=1.)
    let addRow() = 
        Utils.gridAdd(g, vruleElement(), 1, row)
        Utils.gridAdd(g, vruleElement(), 3, row)
        Utils.gridAdd(g, vruleElement(), 5, row)
        g.RowDefinitions.Add(new RowDefinition())
        row <- row + 1
    let hruleElement() = new DockPanel(Background=Brushes.Black, Height=2.)
    let hrule() =    // horizontal rule
        Utils.gridAdd(g, hruleElement(), 0, row)
        Utils.gridAdd(g, hruleElement(), 2, row)
        Utils.gridAdd(g, hruleElement(), 4, row)
        Utils.gridAdd(g, hruleElement(), 6, row)
        addRow()
    Utils.gridAdd(g, Bold(mkTxt("Enabled")), 0, row)
    Utils.gridAdd(g, Bold(mkTxt("Popout Name")), 2, row)
    Utils.gridAdd(g, Bold(mkTxt("Popout description")), 4, row)
    addRow()
    let data = [|
        ccs, "ControlsCheatsheet",  "A 'cheatsheet' list of the main keyboard controls for the app"
        ln,  "CursorNote",          "A resizable window displaying only the Note for the cell the cursor is on (including 'live' edits you make to that note)"
        lm,  "Minimap",             "A resizeable minimap window which displays a group of nearby cells around the cursor." +
                                        "\nMouse-Scroll-Wheel over this window to change how much grid is displayed."
        mp,  "AppGridPane",         "A resizeable window displaying the top half (map grid) portion of the app."
        pn,  "GlobalNote",          "A resizeable window displaying the 'global' Note." +
                                        "\nThis is a text note not tied to any particular cell, which you can edit for general notes not related to a particular map cell." +
                                        "\nThis note can be edited only via the shortcut Ctrl+Numpad/"
        |]
    for ipwb, name, desc in data do
        // layout
        hrule()
        let popupJson = ipwb.GetJson()
        let cb = new CheckBox(IsChecked=popupJson.IsActive, IsThreeState=false, HorizontalAlignment=HorizontalAlignment.Center, VerticalAlignment=VerticalAlignment.Center)
        let tb = mkTxt(desc)
        let but = new Button(Content="Reset", Height=20., VerticalAlignment=VerticalAlignment.Center, Margin=Thickness(2.))
        Utils.gridAdd(g, cb, 0, row)
        Utils.gridAdd(g, Bold(mkTxt(name)), 2, row)
        Utils.gridAdd(g, tb, 4, row)
        Utils.gridAdd(g, but, 6, row)
        addRow()
        // behavior
        cb.Checked.Add(fun _ -> 
            popupJson.IsActive <- true
            AppSettings.theAppSettingsJson.Save()
            ipwb.Activate()
            )
        cb.Unchecked.Add(fun _ ->
            popupJson.IsActive <- false
            AppSettings.theAppSettingsJson.Save()
            ipwb.Close()
            )
        but.Click.Add(fun _ -> 
            ipwb.Close()
            popupJson.IsActive <- true
            popupJson.XYWH <- AppSettings.WindowPosition.DEFAULTXYWH
            AppSettings.theAppSettingsJson.Save()
            cb.IsChecked <- false
            cb.IsChecked <- true
            //ipwb.Activate()
            )
    let sp = new StackPanel(Orientation=Orientation.Vertical)
    sp.Children.Add(instructions1) |> ignore
    sp.Children.Add(new DockPanel(Height=10.)) |> ignore
    sp.Children.Add(new Border(Child=g, BorderBrush=Brushes.Black, BorderThickness=Thickness(1.))) |> ignore
    sp.Children.Add(new DockPanel(Height=10.)) |> ignore
    sp.Children.Add(instructions2) |> ignore
    new Border(Child=sp, Background=Brushes.Gray, Padding=Thickness(6.), Width=width, BorderBrush=Brushes.Black, BorderThickness=Thickness(2.))

