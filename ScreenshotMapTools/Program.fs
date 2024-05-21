open Utils
open System
open System.Windows

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
    *)

    let app = new Application()
    //app.Run(new Elephantasy.MyWindow())
    app.Run(new Generic.MyWindow())

    //0