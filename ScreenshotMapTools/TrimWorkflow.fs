module TrimWorkflow

open System.Windows
open System.Windows.Controls
open System.Windows.Media

open GameSpecific
open BackingStoreData

let GetGameWindowRectOrFailWithAppropriateUI() =
    match TryFindHwndForTheChosenGame() with
    | Some(hwnd) -> 
        let r = WinteropUtils.GetWindowClientRect(hwnd)
        Some(r)
    | None -> 
        System.Console.Beep()
        MessageBox.Show("Could not find the game window, to do trimming") |> ignore
        None

let DeleteImageCaches() =
    // for each zone, delete caches
    for z=0 to theGame.ZoneNames.Length-1 do
        // we need to delete all caches, as code assume all caches stay in sync
        let caches = [| InMemoryStore.MAP_FOLDER_NAME; InMemoryStore.FULL_FOLDER_NAME |]
        for cache in caches do
            let folderToDelete = System.IO.Path.Combine([|GetZoneFolder(z);cache|])
            if System.IO.Directory.Exists(folderToDelete) then
                let files = System.IO.Directory.GetFiles(folderToDelete)
                for f in files do
                    System.IO.File.Delete(f)

let doTheTrimButton(parentWindow, appShutdownF, appWidth) = async {
    let closeEv = Event<unit>()
    let element = new StackPanel(Orientation=Orientation.Vertical, Width=appWidth*3./4., Margin=Thickness(2.))
    let mkTB(txt) = new TextBox(IsReadOnly=true, FontSize=16., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.White, Margin=Thickness(2.), Padding=Thickness(4.),
                                TextWrapping=TextWrapping.Wrap, Text=txt)
    let description = """Trims allow you to capture a rectangular portion of an image.

Trims serve two distinct purposes:

The MAP Trim lets you cut out black bars at the edges of your game window, or cut off fixed HUDs in some games, so that your map cells stitch together seamlessly in the grid.

CUSTOM Trims allow you to customize the appearance of the Preview Pane in the lower portion of the app, which shows information about the cell the cursor is currently on.

Which do you want to do?"""
    element.Children.Add(mkTB(description)) |> ignore
    element.Children.Add(new DockPanel(Height=8.)) |> ignore
    let mutable whichPressed = 0 // default if user closes choice window without pressing a button
    let haveCustom = not(theGame.CustomProjections = null || theGame.CustomProjections.Length=0)
    let choices = [|
        yield "Modify the MAP Trim",                                      true,  (fun _ -> whichPressed <- 1; closeEv.Trigger())
        yield "Define a new CUSTOM Trim",                                 false, (fun _ -> whichPressed <- 2; closeEv.Trigger())
        if haveCustom then
            yield "Modify an existing CUSTOM Trim",                       false, (fun _ -> whichPressed <- 3; closeEv.Trigger())
        yield "Modify the Preview Pane layout for the current zone",      true,  (fun _ -> whichPressed <- 4; closeEv.Trigger())
        yield "Cancel",                                                   false, (fun _ -> whichPressed <- 0; closeEv.Trigger())
        |]
    for label, extraSpaceBelow, effect in choices do
        let b = new Button(Content=label, Width=appWidth*0.5, Height=24., Margin=if extraSpaceBelow then Thickness(2.,2.,2.,12.) else Thickness(2.))
        b.Click.Add(effect)
        element.Children.Add(b) |> ignore
    element.Children.Add(new DockPanel(Height=4.)) |> ignore
    Utils.DoModalDialog(parentWindow, element, "Choose a Trim Type", closeEv.Publish)
    if whichPressed = 1 then
        match GetGameWindowRectOrFailWithAppropriateUI() with
        | None -> ()
        | Some(r) ->
            let! area = AreaSelection.DoAreaSelection(parentWindow, (r.left, r.top, r.right-r.left, r.bottom-r.top), TheChosenGame.MapArea,  "select area to display on map") 
            match area with
            | Some(x,y,w,h) ->
                // update game json
                let json = System.IO.File.ReadAllText(TheChosenGame.GamefileFilename)
                let data = System.Text.Json.JsonSerializer.Deserialize<ChosenGameJson>(json)
                data.MapArea <- (x,y,w,h)
                let json = System.Text.Json.JsonSerializer.Serialize<ChosenGameJson>(data)
                WriteAllText(TheChosenGame.GamefileFilename, json)
                // delete image caches
                DeleteImageCaches()
                // display a modal UI telling user app will restart
                MessageBox.Show("The app will now restart to clear and reload the map image cache, which will take a moment") |> ignore
                appShutdownF()
            | None ->
                MessageBox.Show("No area was selected and no changes were made") |> ignore
    elif whichPressed = 2 then
        match GetGameWindowRectOrFailWithAppropriateUI() with
        | None -> ()
        | Some(r) ->
            // load into temporaries to work with
            let projs = if theGame.CustomProjections = null then ResizeArray() else ResizeArray(theGame.CustomProjections)
            let save,label = Utils.DoBasicModalTextDialog(parentWindow, "Name this Custom Trim", "", appWidth/2., 50., false)
            if save then
                let! area = AreaSelection.DoAreaSelection(parentWindow, (r.left, r.top, r.right-r.left, r.bottom-r.top), TheChosenGame.MapArea,  "select area for this custom trim") 
                match area with
                | Some(x,y,w,h) ->
                    // add the new one
                    projs.Add(CustomProjection(label,(x,y,w,h)))
                    // put them back in the actual data structure
                    theGame.CustomProjections <- projs.ToArray()
                    theGame.Save()
                    MessageBox.Show(sprintf "Custom Trim custom%02d: '%s' saved" (projs.Count-1) label) |> ignore
                | None ->
                    MessageBox.Show("No area was selected and no changes were made") |> ignore
    elif whichPressed = 3 then
        match GetGameWindowRectOrFailWithAppropriateUI() with
        | None -> ()
        | Some(r) ->
            let sp = new StackPanel(Orientation=Orientation.Vertical)
            sp.Children.Add(PreviewPane.mkTxt("You have the following Custom Trims already defined:")) |> ignore
            for i = 0 to theGame.CustomProjections.Length-1 do
                let desc = sprintf "  custom%02d: '%s'\n" i theGame.CustomProjections.[i].Label
                let b = new Button(Content=desc, Width=appWidth*0.5, Height=24., Margin=Thickness(2.))
                b.Click.Add(fun _ -> whichPressed <- i; closeEv.Trigger())
                sp.Children.Add(b) |> ignore
            let b = new Button(Content="Cancel", Width=appWidth*0.5, Height=24., Margin=Thickness(2.))
            b.Click.Add(fun _ -> whichPressed <- -1; closeEv.Trigger())
            sp.Children.Add(b) |> ignore
            sp.Children.Add(PreviewPane.mkTxt("Click on the one you want to modify")) |> ignore
            Utils.DoModalDialog(parentWindow, sp, "Modify a Trim", closeEv.Publish)
            if whichPressed <> -1 then
                let! area = AreaSelection.DoAreaSelection(parentWindow, (r.left, r.top, r.right-r.left, r.bottom-r.top), theGame.CustomProjections.[whichPressed].XYWH,  "select area for this custom trim") 
                match area with
                | Some(x,y,w,h) ->
                    theGame.CustomProjections.[whichPressed].XYWH <- (x,y,w,h)
                    theGame.Save()
                    MessageBox.Show(sprintf "Custom Trim custom%02d: '%s' saved" whichPressed theGame.CustomProjections.[whichPressed].Label) |> ignore
                | None ->
                    MessageBox.Show("No area was selected and no changes were made") |> ignore
    elif whichPressed = 4 then
        PreviewPane.ModifyPreviewPaneForCurrentZone(parentWindow, appWidth)
    else
        () // nothing, they canceled
    }

/////////////////////////////////////////////////////
// color tweak workflow

open Utils.Extensions

let moreBackgroundBrush = new SolidColorBrush(Color.FromRgb(0x88uy,0xAAuy,0xBBuy))

let doTheColorTweakButton(parentWindow, appShutdownF, updateBHKE, appWidth) =
    let zm = InMemoryStore.ZoneMemory.Get(theGame.CurZone)
    let cmt = zm.MapTiles.[theGame.CurX, theGame.CurY]
    if cmt.NumScreenshots() = 0 then
        MessageBox.Show("First put cursor on a cell with at least one screenshot") |> ignore
    else
        let swk = cmt.ScreenshotsWithKinds.[0]
        let ssid = swk.Id
        let thisBmp = InMemoryStore.bmpDict.[ssid]
        // make a scaled reference bmp without tweaks
        let twoThirds = appWidth*2./3.
        let refBmp = new System.Drawing.Bitmap(thisBmp, System.Drawing.Size(int twoThirds, int(twoThirds * float thisBmp.Height / float thisBmp.Width)))
        let tweak(minL) =
            let cf = MultipleScreenshotForOneScreen.FancyTweak.adjustMapContrast(minL)
            let clone = new System.Drawing.Bitmap(refBmp, System.Drawing.Size(refBmp.Width, refBmp.Height))
            Utils.TransformColorFromLockedFormat32BppArgb(clone, fun(struct(a,r,g,b)) ->
                let c = System.Drawing.Color.FromArgb(int r,int g,int b)
                let c = cf(c)
                struct(a, c.R, c.G, c.B)
                )
            clone
        // hundredths of lightness, ranging 1 to 50 for 0.01 to 0.50, as a value for minL, where minL of 0.5 is 'unchanged' and smaller values boost lightness in [minL..0.5] range
        let mutable n = 2 * (min 25 (max 1 (int(GameSpecific.TheChosenGame.MinLightnessForColorBoosting * 50.))))
        let sp = new StackPanel(Orientation=Orientation.Vertical, Width=appWidth, Margin=Thickness(8.))
        let tb = new TextBlock(Text="Some games use some dark colors that don't provide good contrast for seeing the map when zoomed out. SnapMap allows you to tweak the "+
                                    "display of darker colors by specifying a lightness threshold, where colors below the threshold are unchanged (to preserve black and off-black colors) "+
                                    "but colors just above the threshold get boosted (for more contrast/brightness).\n\n" +
                                    "Experiment with the color contrast using controls explained below, and watch how this screenshot display is modified."
                                    , TextWrapping=TextWrapping.Wrap)
        sp.Children.Add(tb) |> ignore
        let imageHolder = new DockPanel(Width=appWidth, LastChildFill=true, Margin=Thickness(4.))
        let updateImage() =
            let minL = float n * 0.01
            let bmp = tweak(minL)
            imageHolder.Children.Clear()
            imageHolder.Children.Add(Utils.BMPtoImage(bmp)) |> ignore
        updateImage()
        sp.Children.Add(imageHolder) |> ignore
        let tb = new TextBlock(Text="Press NumPad 4 (and 6) to adjust threshold left (and right) until desired color contrast is seen")
        sp.Children.Add(tb) |> ignore
        let fauxSlider = new DockPanel(Height=30., Background=Brushes.Blue, HorizontalAlignment=HorizontalAlignment.Left, Margin=Thickness(0.,4.,0.,4.))
        let updateFauxSlider() = fauxSlider.Width <- float n * appWidth / 50.
        updateFauxSlider()
        sp.Children.Add(fauxSlider) |> ignore
        let tb = new TextBlock(Text="As the blue bar shrinks to the left, darker and darker colors will get boosted more and more.\n" +
                                    "Expand the blue bar all the way to the right (with NumPad 6) for no display modifications (that is, colors displayed by SnapMap match "+
                                    "colors displayed in the game).", TextWrapping=TextWrapping.Wrap)
        sp.Children.Add(tb) |> ignore
        // buttons
        let closeEv = new Event<unit>()
        let mutable save = false
        let cb = new Button(Content=" Cancel ", Margin=Thickness(4.))
        let sb = new Button(Content=" Save Changes ", Margin=Thickness(4.))
        cb.Click.Add(fun _ -> closeEv.Trigger())
        sb.Click.Add(fun _ -> save <- true; closeEv.Trigger())
        let dp = (new DockPanel(LastChildFill=true)).AddLeft(cb).AddRight(sb).Add(new DockPanel())
        sp.Children.Add(dp) |> ignore
        use _idisp = updateBHKE(fun (key, _ctrlHeld) ->
            if key = Elephantasy.Winterop.VK_NUMPAD4 then
                if n > 2 then
                    n <- n - 2
                    updateImage()
                    updateFauxSlider()
            elif key = Elephantasy.Winterop.VK_NUMPAD6 then
                if n < 49 then
                    n <- n + 2
                    updateImage()
                    updateFauxSlider()
            )
        let all = new DockPanel(Background=moreBackgroundBrush, LastChildFill=true)
        all.Children.Add(sp) |> ignore
        Utils.DoModalDialog(parentWindow, all, "Modify Dark Color Contrast", closeEv.Publish)
        if save then
            // update game json
            let json = System.IO.File.ReadAllText(TheChosenGame.GamefileFilename)
            let data = System.Text.Json.JsonSerializer.Deserialize<ChosenGameJson>(json)
            data.MinLightnessForColorBoosting <- float n * 0.01
            let json = System.Text.Json.JsonSerializer.Serialize<ChosenGameJson>(data)
            WriteAllText(TheChosenGame.GamefileFilename, json)
            // delete image caches
            DeleteImageCaches()
            // display a modal UI telling user app will restart
            MessageBox.Show("The app will now restart to clear and reload the map image cache, which will take a moment") |> ignore
            appShutdownF()

/////////////////////////////////////////////////////
// "more..." button workflow

let doTheMoreButton(parentWindow, appShutdownF, updateBHKE, appWidth) =
    let closeEv = new Event<unit>()
    let sp = new StackPanel(Orientation=Orientation.Vertical, Margin=Thickness(8.))
    let makeHr() =
        let dp = new DockPanel(Height=2., Background=Brushes.Black, Margin=Thickness(0.,4.,0.,4.))
        sp.Children.Add(dp) |> ignore

    let tb = new TextBlock(Text="Miscellaneous functionality", FontSize=18.)
    sp.Children.Add(tb) |> ignore
    makeHr()

    // version
    let tb = new TextBlock(Text=GameSpecific.AppTitle + "\nThis app is found on GitHub, which you can visit here:", TextWrapping=TextWrapping.Wrap)
    sp.Children.Add(tb) |> ignore
    // github link
    let githubButton = new Button(Content="Open GitHub page", Margin=Thickness(4.), HorizontalAlignment=HorizontalAlignment.Left)
    githubButton.Click.Add(fun _ -> 
        System.Diagnostics.Process.Start("https://github.com/brianmcn/ScreenshotMapTools") |> ignore    // open a URL in default browser
        )
    sp.Children.Add(githubButton) |> ignore
    // documentation callout
    let tb = new TextBlock(Text="Full documentation for SnapMap is linked at the bottom of the GitHub page.", TextWrapping=TextWrapping.Wrap)
    sp.Children.Add(tb) |> ignore

    makeHr()

    // color contrast tweaker
    let tb = new TextBlock(Text="Dark colors can sometimes be hard to see on zoomed-out maps.  SnapMap can boost the lightness/brightness of "+
                                "some dark colors, while preserving black blacks, to improve constrast for some games.", TextWrapping=TextWrapping.Wrap)
    sp.Children.Add(tb) |> ignore
    let i = 
        let bi = new System.Windows.Media.Imaging.BitmapImage()
        bi.BeginInit()
        bi.UriSource <- new System.Uri("pack://application:,,,/esaBothSmall.png", System.UriKind.Absolute)
        bi.CacheOption <- System.Windows.Media.Imaging.BitmapCacheOption.OnLoad
        bi.EndInit()
        new Image(Source = bi, Width=bi.PixelWidth, Height=bi.PixelHeight, HorizontalAlignment=HorizontalAlignment.Left, Margin=Thickness(4.))
    sp.Children.Add(i) |> ignore

    let tb = new TextBlock(Text="You can try this feature yourself with the button below.", TextWrapping=TextWrapping.Wrap)
    sp.Children.Add(tb) |> ignore
    let mdccButton = new Button(Content="Modify Dark Color Contrast", Margin=Thickness(4.), HorizontalAlignment=HorizontalAlignment.Left)
    mdccButton.Click.Add(fun _ -> 
        doTheColorTweakButton(parentWindow, appShutdownF, updateBHKE, appWidth)
        closeEv.Trigger()
        )
    sp.Children.Add(mdccButton) |> ignore

    // anything else?

    let all = new DockPanel(Background=moreBackgroundBrush, LastChildFill=true, Width=appWidth*2./3.)
    all.Children.Add(sp) |> ignore
    Utils.DoModalDialog(parentWindow, all, "More functionality...", closeEv.Publish)
