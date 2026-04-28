module GameSpecific

// recompile for different uses, for now

//let GAME = "Void Stranger"
//let WINDOW_TITLE = "ScreenshotMapTools (Running) - Microsoft Visual Studio"  //"Void Stranger"

[<AllowNullLiteral>]
type ChosenGameJson() =
    member val GameFolder : string = null with get,set              // name of save folder for this program's assets (screenshots etc)
    member val WindowTitle : string = null with get,set             // of the process to find
    member val GameWidth : int = 0 with get,set     
    member val GameHeight : int = 0 with get,set    
    member val MapArea : int*int*int*int = 0,0,0,0 with get,set     // x,y,w,h
    member val MetaArea : int*int*int*int = 0,0,0,0 with get,set  

let WriteAllText(filename, text) =
    let dir = System.IO.Path.GetDirectoryName(filename)
    System.IO.Directory.CreateDirectory(dir) |> ignore   // ensure directory exists
    System.IO.File.WriteAllText(filename, text)

type ChosenGame() =
    let mutable data = null
    do
        let gameFile = "CurrentGame.json"
        (*
        let oneTime =
            let cgj = ChosenGameJson()
            cgj.GameFolder <- "IoSaS"
            cgj.WindowTitle <- "Isles of Sea and Sky"
            cgj.GameWidth <- 1152
            cgj.GameHeight <- 648
            cgj.MapArea <- 0, 96, 1152, 552
            cgj.MetaArea <- 200, 0, 10, 1
            let json = System.Text.Json.JsonSerializer.Serialize<ChosenGameJson>(cgj)
            System.IO.File.WriteAllText(gameFile, json)
        *)
        let subdirs = System.IO.Directory.EnumerateDirectories(".")
        let possibleArray = [|
            for dir in subdirs do
                let file = System.IO.Path.Combine(dir, gameFile)
                if System.IO.File.Exists(file) then
                    try
                        let json = System.IO.File.ReadAllText(file)
                        yield System.Text.Json.JsonSerializer.Deserialize<ChosenGameJson>(json)
                    with _ -> ()
            |]
        Utils.Win32.SetForegroundWindow(Elephantasy.Winterop.GetConsoleWindow()) |> ignore
        System.Console.WriteLine "Choose startup option:"
        System.Console.WriteLine "0: New Game"
        let mutable i = 1
        for p in possibleArray do
            System.Console.WriteLine(sprintf "%d: %s (%s)" i p.GameFolder p.WindowTitle)
            i <- i + 1
        let r = System.Console.ReadLine()
        try 
            let j = int r
            if j=0 then
                let wins = Elephantasy.Screenshot.GetOpenWindows()
                System.Console.WriteLine("Choose an open window process, to load its window size data")
                System.Console.WriteLine("and start a new screenshot directory.")
                System.Console.WriteLine("0: <abort>")
                i <- 1
                let names = [|
                    for KeyValue(_hwnd,(name,rect)) in wins do
                        System.Console.WriteLine(sprintf "%d: %s" i name)
                        i <- i + 1
                        yield name, rect.right-rect.left, rect.bottom-rect.top
                        |]
                let r = System.Console.ReadLine()
                let j = int r
                if j=0 then
                    failwith "aborted"
                elif j <= names.Length then
                    let n, w, h = names.[j-1]
                    System.Console.WriteLine(sprintf "You chose window: (%d x %d) %s" w h n)
                    System.Console.WriteLine("If this is correct, then choose a directory name to start saving screenshots")
                    System.Console.WriteLine("or else just press enter to abort.")
                    let r = System.Console.ReadLine()
                    if System.String.IsNullOrWhiteSpace(r) then
                        failwith "aborted"
                    elif System.Text.RegularExpressions.Regex.IsMatch(r, "[a-zA-Z][a-zA-Z0-9 ]*") then
                        System.Console.WriteLine(sprintf "You chose '%s'" r)
                        let cgj = ChosenGameJson()
                        cgj.GameFolder <- r
                        cgj.GameHeight <- h
                        cgj.GameWidth <- w
                        cgj.MapArea <- 0,0,w,h
                        cgj.MetaArea <- 0,0,10,1
                        cgj.WindowTitle <- n
                        let file = System.IO.Path.Combine(".", r, gameFile)
                        let json = System.Text.Json.JsonSerializer.Serialize<ChosenGameJson>(cgj)
                        WriteAllText(file, json)
                        data <- cgj
                    else
                        failwith "Directory name must be alphanumeric and start with a letter"
                else
                    failwith "invalid selection"
            elif j<=possibleArray.Length then
                data <- possibleArray.[j-1]
            else
                failwith "out of range"
        with e ->
            System.Console.WriteLine(sprintf "%s, aborting..." e.Message)
            System.Environment.Exit(1)
    member this.GAME = data.GameFolder
    member this.WINDOW_TITLE = data.WindowTitle
    member this.GAMESCREENW = data.GameWidth
    member this.GAMESCREENH = data.GameHeight
    member this.MapArea  = data.MapArea
    member this.MetaArea = data.MetaArea
let TheChosenGame = ChosenGame()
let ActivateGameWindow() = 
    let mutable r = None
    for KeyValue(hwnd,(title,_rect)) in Elephantasy.Screenshot.GetOpenWindows() do
        if title.StartsWith(TheChosenGame.WINDOW_TITLE) then
            r <- Some hwnd
    match r with
    | Some(hwnd) -> 
        System.Console.WriteLine(sprintf "bringing game '%s' to the foreground" TheChosenGame.WINDOW_TITLE)
        if Utils.Win32.SetForegroundWindow(hwnd) = false then
            System.Console.WriteLine("...but that failed for some reason")
    | None ->
        System.Console.WriteLine("game window not found")


#if !IOSAS

(*
let GAME = "IoSaS"               // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Isles of Sea and Sky"  // of the process to find
let GAMESCREENW, GAMESCREENH = 1152, 648
let MapArea  = 0, 96, 1152, 552         // x,y,w,h
let MetaArea = 200, 0, 10, 1
*)

#else

#if FOO

let GAME = "Leafs Odyssey"   // recompile for different uses, for now
let WINDOW_TITLE = "Leaf's Odyssey"
let NATIVE_FACTOR = 2         // e.g. I am running it at 2x native pixel size
let GAMESCREENW, GAMESCREENH = 960, 540
let VIEWX = 630   // multiple of 9, 7, 5 so that we can so various zooms well onscreen
let MapArea  =  96, 14, 384, 256         // x,y,w,h
let MetaArea = 230,  0, 120,  14

#else

#if !BAR

let GAME = "Animal Well"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "ANIMAL WELL"  // of the process to find
let NATIVE_FACTOR = 1         // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 1280, 720
let MapArea  = 0, 0, 1280, 720         // x,y,w,h
let MetaArea = 0, 0, 100, 1

#else

#if BAZ

let GAME = "Zelda"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "FCEUX 2.6.4"  // of the process to find
let NATIVE_FACTOR = 1         // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 768, 672
let MapArea  = 0, 168, 768, 504         // x,y,w,h
let MetaArea = 0, 0, 100, 1

#else

#if QUX

let GAME = "Master Key"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Master Key"  // of the process to find
let NATIVE_FACTOR = 1            // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 1136, 640
let MapArea  = 176, 0, 960, 640         // x,y,w,h
let MetaArea = 0, 0, 100, 1

#else

#if SS

let GAME = "Side Scape"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Side Scape"  // of the process to find
let NATIVE_FACTOR = 1            // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 768, 704
let MapArea  = 0, 64, 768, 640         // x,y,w,h
let MetaArea = 200, 0, 10, 1

#else

let GAME = "Hatchwell"               // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Hatchwell"  // of the process to find
let GAMESCREENW, GAMESCREENH = 1280, 720
let MapArea  = 0, 0, 1280, 620         // x,y,w,h
let MetaArea = 200, 0, 10, 1

#endif

#endif

#endif

#endif 

#endif

#endif

let MapAreaRectangle =
    let x,y,w,h = TheChosenGame.MapArea
    new System.Drawing.Rectangle(x,y,w,h)
let MetaAreaRectangle =
    let x,y,w,h = TheChosenGame.MetaArea
    new System.Drawing.Rectangle(x,y,w,h)

//////////////////////////////////////////////////////////////////////////
// common computations

let GAMEASPECT = float(TheChosenGame.GAMESCREENW) / float(TheChosenGame.GAMESCREENH)


