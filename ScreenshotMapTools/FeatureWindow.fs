module FeatureWindow

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media

[<AllowNullLiteral>]
type FeatureWindow(owner) as this =
    inherit Window()
    static let mutable theFeatureWindow : FeatureWindow = null
    let b = new Border(Width=1280., Height=720., BorderThickness=Thickness(0.), Background=Brushes.Olive)
    do
        this.Owner <- owner
        this.Title <- "FEATURE"
        this.SizeToContent <- SizeToContent.WidthAndHeight
        this.Content <- b
        this.Loaded.Add(fun _ ->
            theFeatureWindow <- this
            )
        this.Closed.Add(fun _ ->
            theFeatureWindow <- null
            )
    member this.SetContent(c) = b.Child <- c
    static member TheFeatureWindow with get() = theFeatureWindow and set(x) = theFeatureWindow <- x

let EnsureFeature(rootOwner, content) =
    if FeatureWindow.TheFeatureWindow=null then
        FeatureWindow.TheFeatureWindow <- new FeatureWindow(rootOwner)
        FeatureWindow.TheFeatureWindow.Show()
    FeatureWindow.TheFeatureWindow.SetContent(content)



