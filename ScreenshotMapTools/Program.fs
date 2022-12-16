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
    let app = new Application()
    app.Run(new Elephantasy.MyWindow())

    //0