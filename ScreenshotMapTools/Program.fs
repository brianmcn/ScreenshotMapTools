open System
open System.Windows

let GetConsoleWindowInFrontThen(f) =
    //async {
        let handle = Elephantasy.Winterop.GetConsoleWindow()
        Elephantasy.Winterop.ShowWindow(handle, Elephantasy.Winterop.SW_RESTORE) |> ignore
        Winterop.Win32.SetForegroundWindow(handle) |> ignore
    //    let ctxt = System.Threading.SynchronizationContext.Current
    //    do! Async.Sleep(500)
    //    do! Async.SwitchToContext(ctxt)
        f()
    //} |> Async.StartImmediate

// in order for multiple app windows to not have a forced Z-Order from Owner-Child relationship, need a hidden dummy window to own all the visible windows
type DummyWindow() as this =
    inherit Window()
    let mutable handledException = false
    do
        this.ShowInTaskbar <- false
        this.Title <- "start-up..."
        this.Width <- 300.
        this.Height <- 100.
        this.WindowState <- WindowState.Minimized
        this.Visibility <- Visibility.Hidden
        let handleException(ex:System.Exception) =
            if not(handledException) then
                printfn "%s" (ex.ToString())
                if not(Generic.appShutItselfDownAfterTrim) then
                    GetConsoleWindowInFrontThen(fun () ->
                        printfn ""
                        printfn "closing app, press enter to exit this window"
                        System.Console.ReadLine() |> ignore
                        )
                handledException <- true
        System.Windows.Application.Current.DispatcherUnhandledException.Add(fun e -> 
            if System.Diagnostics.Debugger.IsAttached then
                System.Diagnostics.Debugger.Break()
            let ex = e.Exception
            handleException(ex)
            e.Handled <- true
            this.Close()
            )
        System.AppDomain.CurrentDomain.UnhandledException.Add(fun e -> 
            if System.Diagnostics.Debugger.IsAttached then
                System.Diagnostics.Debugger.Break()
            match e.ExceptionObject with
            | :? System.Exception as ex ->
                handleException(ex)
            | _ ->
                printf "An unhandled exception from background thread occurred."
            )
        this.Loaded.Add(fun _ ->
            this.Activate() |> ignore
            printfn "Starting up..."
            let handle = Elephantasy.Winterop.GetConsoleWindow()
            let _hwnd = Elephantasy.Winterop.SetActiveWindow(handle)
            //printfn "%A" _hwnd
            let mainW = new Generic.MyWindow(fun () -> 
                let startInfo = new System.Diagnostics.ProcessStartInfo()
                startInfo.FileName <- System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName
                startInfo.UseShellExecute <- false
                startInfo.Arguments <- "--glass"
                System.Diagnostics.Process.Start(startInfo) |> ignore
                )
            mainW.Owner <- this
            mainW.Show()
            mainW.Closed.Add(fun _ -> 
                try this.Close() 
                with ex -> handleException(ex)
                )  
            )

[<assembly: System.Resources.NeutralResourcesLanguage("en-US", System.Resources.UltimateResourceFallbackLocation.MainAssembly)>]
do
    () 

[<STAThread>]
[<EntryPoint>]
let main _argv =
    System.Threading.Tasks.Task.Run(fun () -> System.Text.Json.JsonSerializer.Serialize<GameSpecific.ChosenGameJson>(null) |> ignore) |> ignore  // warm up the serializer so it doesn't take a long time on first use

    if GameSpecific.CommandLine.glass then
        if false then
            System.Console.WriteLine("press enter to start glass")
            System.Console.ReadLine() |> ignore
        // glass is a separate application to decouple its window activation/z-order from the rest of the app
        let app = new Application()
        app.Run(new Glass.DrawingGlassWindow())
    else
        let app = new Application()
        let r = app.Run(new DummyWindow())
        //let r = app.Run(new Glass.DrawingGlassWindow())
        r

        