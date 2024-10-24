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
        this.Loaded.Add(fun _ ->
            this.Activate() |> ignore
            printfn "Starting up..."
            let handle = Elephantasy.Winterop.GetConsoleWindow()
            let _hwnd = Elephantasy.Winterop.SetActiveWindow(handle)
            //printfn "%A" _hwnd
            let mainW = new Generic.MyWindow()
            mainW.Owner <- this
            mainW.Show()
            mainW.Closed.Add(fun _ -> try this.Close() with e -> ())  // ignore exceptions on close
            )


[<STAThread>]
[<EntryPoint>]
let main argv =
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

        

    let app = new Application()
    //app.Run(new Elephantasy.MyWindow())
    app.Run(new DummyWindow())
    //app.Run(new Glass.DrawingGlassWindow())

    //0