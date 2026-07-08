open Utils
open System
open System.Windows



// in order for multiple app windows to not have a forced Z-Order from Owner-Child relationship, need a hidden dummy window to own all the visible windows
type DummyWindow() as this =
    inherit Window()
    do
        this.ShowInTaskbar <- false
        this.Title <- "start-up..."
        this.Width <- 300.
        this.Height <- 100.
        this.WindowState <- WindowState.Minimized
        this.Visibility <- Visibility.Hidden
        let handleException(ex:System.Exception) =
            match ex with
            | _ ->
                printfn "%s" (ex.ToString())
                printfn ""
                printfn "closing app, press enter to exit this window"
                System.Console.ReadLine() |> ignore
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
            let mainW = new Generic.MyWindow()
            mainW.Owner <- this
            mainW.Show()
            mainW.Closed.Add(fun _ -> 
                try this.Close() 
                with ex -> handleException(ex)
                )  
            )


[<STAThread>]
[<EntryPoint>]
let main _argv =
    (*
    let app = new Application()
    app.Run(new Knytt.MyWindow())
    *)
    
    (*
    let app = new Application()
    app.Run(new HS.QRWindow())
    *)
    
    // DeepRune.drMain()
    
    //StitchView.svMain()

    //Elephantasy.Screenshot.testMain()

    (*
    printfn "%A" (GenericMetadata.AllHashtags("#foo"))
    printfn "%A" (GenericMetadata.AllHashtags("yadda #bar baz #qux yadda"))
    printfn "%A" (GenericMetadata.AllHashtags("#_ #a #b #c #1 #f4 #"))
    printfn "%A" (GenericMetadata.AllHashtags("yadda #bar ## ###a #qux yadda"))


    let _,n = System.Numerics.BigInteger.TryParse("2926619871974812")
    let mutable r = n
    while not r.IsZero do
        let x,y = System.Numerics.BigInteger.DivRem(r, System.Numerics.BigInteger(36))
        let i = int y - 10
        printfn "%c" (char(i + int 'A'))
        r <- x
    *)

    if false then    
        let fcPath = "C:\Users\Brian\Desktop\EMUUROM\AfterE37\ForestCover2x2.png"
        let homePath = "C:\Users\Brian\Desktop\EMUUROM\AfterE37\Home2x2.png"
        let fcBmp = new System.Drawing.Bitmap(fcPath)
        let homeBmp = new System.Drawing.Bitmap(homePath)
        // make home partly transparent
        let homeData = homeBmp.LockBits(System.Drawing.Rectangle(0,0,homeBmp.Width,homeBmp.Height), System.Drawing.Imaging.ImageLockMode.ReadWrite, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        let tf = (fun (_a,r,g,b) -> (64uy,r,g,b))  // 64 is 25%
        for x = 0 to homeBmp.Width-1 do
            for y = 0 to homeBmp.Height-1 do
                Utils.SetAndGetAndTransformColorFromLockedFormat32BppArgb(x, y, homeData, x, y, homeData, tf)
        homeBmp.UnlockBits(homeData)
        // draw it atop forest
        let g = System.Drawing.Graphics.FromImage(fcBmp)
        g.CompositingMode <- System.Drawing.Drawing2D.CompositingMode.SourceOver
        homeBmp.MakeTransparent();
        g.DrawImage(homeBmp, new System.Drawing.Point(0, 0))
        fcBmp.Save("C:\Users\Brian\Desktop\EMUUROM\AfterE37\Both2x2.png")
    

    let app = new Application()
    //app.Run(new Elephantasy.MyWindow())
    let r = app.Run(new DummyWindow())
    //let r = app.Run(new Glass.DrawingGlassWindow())

    r