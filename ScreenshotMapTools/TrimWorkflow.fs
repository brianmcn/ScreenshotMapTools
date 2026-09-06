module TrimWorkflow

open System.Windows
open System.Windows.Controls
open System.Windows.Media

open GameSpecific
open BackingStoreData


let doTheTrimButton(parentWindow, appShutdownF, appWidth) =
    match TryFindHwndForTheChosenGame() with
    | Some(hwnd) -> 
        let r = WinteropUtils.GetWindowClientRect(hwnd)
        let closeEv = Event<unit>()
        let element = new StackPanel(Orientation=Orientation.Vertical, Width=appWidth*2./3., Margin=Thickness(2.))
        let mkTB(txt) = new TextBox(IsReadOnly=true, FontSize=16., BorderThickness=Thickness(1.), Foreground=Brushes.Black, Background=Brushes.White, Margin=Thickness(2.),
                                    TextWrapping=TextWrapping.Wrap, Text=txt)
        let description = """Trims allow you to capture a rectangular portion of an image.

Trims can be used in two main ways:

The MAP Trim lets you cut out black bars at the edges of your game window, or cut off fixed HUDs in some games, so that your map cells stitch together seamlessly.

CUSTOM Trims allow you to customize the appearance of the preview pane in the lower portion of the app, which shows information about the cell the cursor is currently on.

Which do you want to do?"""
        element.Children.Add(mkTB(description)) |> ignore
        let mutable whichPressed = 0 // default if user closes choice window without pressing a button
        let haveCustom = not(theGame.CustomProjections = null || theGame.CustomProjections.Length=0)
        let choices = [|
            yield "Modify the MAP Trim",                                      (fun _ -> whichPressed <- 1; closeEv.Trigger())
            yield "Define a new CUSTOM Trim",                                 (fun _ -> whichPressed <- 2; closeEv.Trigger())
            if haveCustom then
                yield "Modify an existing CUSTOM Trim",                           (fun _ -> whichPressed <- 3; closeEv.Trigger())
            yield "Modify the Preview Pane layout for the current zone",      (fun _ -> whichPressed <- 4; closeEv.Trigger())
            yield "Cancel",                                                   (fun _ -> whichPressed <- 0; closeEv.Trigger())
            |]
        for label, effect in choices do
            let b = new Button(Content=label, Width=appWidth*0.5, Height=24., Margin=Thickness(2.))
            b.Click.Add(effect)
            element.Children.Add(b) |> ignore
        Utils.DoModalDialog(parentWindow, element, "Choose a Trim Type", closeEv.Publish)
        if whichPressed = 1 then
            let area = AreaSelection.DoAreaSelection((r.left, r.top, r.right-r.left, r.bottom-r.top), TheChosenGame.MapArea,  "select area to display on map") 
            match area with
            | Some(x,y,w,h) ->
                // update MapArea in CurrentGame
                let json = System.IO.File.ReadAllText(TheChosenGame.GamefileFilename)
                let data = System.Text.Json.JsonSerializer.Deserialize<ChosenGameJson>(json)
                data.MapArea <- (x,y,w,h)
                let json = System.Text.Json.JsonSerializer.Serialize<ChosenGameJson>(data)
                WriteAllText(TheChosenGame.GamefileFilename, json)
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
                // display a modal UI telling user app will restart
                MessageBox.Show("The app will now restart to clear and reload the map image cache, which will take a moment") |> ignore
                appShutdownF()
            | None ->
                MessageBox.Show("No area was selected and no changes were made") |> ignore
        elif whichPressed = 2 then
            // load into temporaries to work with
            let projs = if theGame.CustomProjections = null then ResizeArray() else ResizeArray(theGame.CustomProjections)
            let save,label = Utils.DoBasicModalTextDialog(parentWindow, "Provide a descriptive label for this Custom Trim", "", appWidth, 50., false, fun _ -> ())
            if save then
                let area = AreaSelection.DoAreaSelection((r.left, r.top, r.right-r.left, r.bottom-r.top), TheChosenGame.MapArea,  "select area for this custom trim") 
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
                let area = AreaSelection.DoAreaSelection((r.left, r.top, r.right-r.left, r.bottom-r.top), theGame.CustomProjections.[whichPressed].XYWH,  "select area for this custom trim") 
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
    | None -> 
        System.Console.Beep()
        MessageBox.Show("Could not find the game window, to do trimming") |> ignore
