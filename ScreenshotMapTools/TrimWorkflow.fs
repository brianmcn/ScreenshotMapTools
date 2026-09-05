module TrimWorkflow

open System.Windows

open GameSpecific
open BackingStoreData


let doTheTrimButton(appShutdownF) =
    match TryFindHwndForTheChosenGame() with
    | Some(hwnd) -> 
        let r = WinteropUtils.GetWindowClientRect(hwnd)
        if theGame.CurProjection = 0 then
            System.Console.Beep()      // nothing to trim in full-screenshots view
        else
            let isMap,area = 
                if theGame.CurProjection = 1 then     // map
                    true,  AreaSelection.DoAreaSelection((r.left, r.top, r.right-r.left, r.bottom-r.top), TheChosenGame.MapArea,  "select area to display on map") 
                elif theGame.CurProjection = 2 then   // meta (hud, metadata, whatever)
                    false, AreaSelection.DoAreaSelection((r.left, r.top, r.right-r.left, r.bottom-r.top), TheChosenGame.MetaArea, "select area with HUD/metadata") 
                else
                    failwith "impossible CurProjection"
            match area with
            | Some(x,y,w,h) ->
                // update MapArea/MetaArea in CurrentGame
                let json = System.IO.File.ReadAllText(TheChosenGame.GamefileFilename)
                let data = System.Text.Json.JsonSerializer.Deserialize<ChosenGameJson>(json)
                if isMap then
                    data.MapArea <- (x,y,w,h)
                else
                    data.MetaArea <- (x,y,w,h)
                let json = System.Text.Json.JsonSerializer.Serialize<ChosenGameJson>(data)
                WriteAllText(TheChosenGame.GamefileFilename, json)
                // for each zone, delete caches
                for z=0 to theGame.ZoneNames.Length-1 do
                    // we need to delete all caches, as code assume all caches stay in sync
                    let caches = [| InMemoryStore.MAP_FOLDER_NAME; InMemoryStore.FULL_FOLDER_NAME; InMemoryStore.META_FOLDER_NAME |]
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
                System.Console.Beep()
    | None -> 
        System.Console.Beep()
