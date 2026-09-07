module AppSettings

// settings that are not specific to any particular game (e.g. window locations)

let appRootFolder = System.AppDomain.CurrentDomain.BaseDirectory

let WriteAllText(filename, text) =
    let dir = System.IO.Path.GetDirectoryName(filename)
    System.IO.Directory.CreateDirectory(dir) |> ignore   // ensure directory exists
    System.IO.File.WriteAllText(filename, text)

module WindowPosition =
    // When Windows minimizes, or is closing a window, it moves it to -32000,-32000 and WPF reports those values as Left and Top.
    // However, on a high-DPI device, that number gets scaled, e.g. at 1.75, you see -18285 get reported.
    // Since I am using the value as a way to detect 'junk' coordinates, there's am issue deciding which coordinates are junk versus
    // large and negative real.
    // In practice, it seems today devices are unlikely to have a DPI scale of more than 2.0, which means -16000 would be a useful cutoff.
    // So if a Left/Top coordinate is less than this value, assume junk:
    let MINIMIZED_THRESHOLD = -15999.
    // DEFAULT: set this value e.g. in a save file to load this window wherever Windows would open a new window, rather than trying to position it
    let DEFAULT = -32000    
    let DEFAULTXYWH = -32000,-32000,0,0
    let SetInitialWindowPosition(win:System.Windows.Window, xywh) =
        let x,y,w,h = xywh
        if not(float x< MINIMIZED_THRESHOLD) && not(float y< MINIMIZED_THRESHOLD) && w>5 && h>5 then    // if onscreen and large enough to be visible, then
            win.Left <- float x
            win.Top <- float y
            win.Width <- float w
            win.Height <- float h

[<AllowNullLiteral>]
type PopoutDetailJson() =
    member val IsActive : bool = false with get,set
    member val XYWH : (int*int*int*int) = (WindowPosition.DEFAULT,WindowPosition.DEFAULT,0,0) with get,set

[<AllowNullLiteral>]
type AppSettingsJson() =
    static let theAppSettingsJson = AppSettingsJson()
    ////
    member val ControlsCheatSheetPopout : PopoutDetailJson = null with get,set
    member val LiveNotesPopout : PopoutDetailJson = null with get,set
    member val LiveMinimapPopout : PopoutDetailJson = null with get,set
    member val MapPanePopout : PopoutDetailJson = null with get,set
    ////
    static member TheAppSettingsJson = theAppSettingsJson 
    member this.Save() = // assumes just one global instance
        let appFile = System.IO.Path.Combine(appRootFolder, "app.json")
        let json = System.Text.Json.JsonSerializer.Serialize<AppSettingsJson>(theAppSettingsJson)
        WriteAllText(appFile, json)
    member this.EnsurePopoutData() =
        if this.ControlsCheatSheetPopout = null then
            this.ControlsCheatSheetPopout <- new PopoutDetailJson()
        if this.LiveNotesPopout = null then
            this.LiveNotesPopout <- new PopoutDetailJson()
        if this.LiveMinimapPopout = null then
            this.LiveMinimapPopout <- new PopoutDetailJson()
        if this.MapPanePopout = null then
            this.MapPanePopout <- new PopoutDetailJson()
let theAppSettingsJson = AppSettingsJson.TheAppSettingsJson

do  // load at startup
    let appFile = System.IO.Path.Combine(appRootFolder, "app.json")
    if System.IO.File.Exists(appFile) then
        let json = System.IO.File.ReadAllText(appFile)
        let data = System.Text.Json.JsonSerializer.Deserialize<AppSettingsJson>(json)
        theAppSettingsJson.ControlsCheatSheetPopout <- data.ControlsCheatSheetPopout
        theAppSettingsJson.LiveNotesPopout <- data.LiveNotesPopout
        theAppSettingsJson.LiveMinimapPopout <- data.LiveMinimapPopout
        theAppSettingsJson.MapPanePopout <- data.MapPanePopout
    theAppSettingsJson.EnsurePopoutData()


