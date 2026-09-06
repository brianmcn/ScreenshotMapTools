module PreviewPane

open BackingStoreData

let idp = { new BasicLayout.IDataPane<PreviewPaneSource> with
            member _.CreateEmptyData() = failwith "we're not editing these"
            member _.ProjectDataToUI(pps) = 
                if pps.ZoneToRead = -2 then
                    null
                else
                    let ztr = if pps.ZoneToRead = -1 then theGame.CurZone else pps.ZoneToRead
                    if ztr < 0 || ztr >= theGame.ZoneNames.Length then
                        printfn "bad ZoneToRead %d in json" ztr
                        System.Console.Beep()
                        null
                    else
                        let zm = InMemoryStore.ZoneMemory.Get(ztr)
                        let full = zm.FullImgArray.[theGame.CurX, theGame.CurY]
                        if full = null then
                            null
                        else
                            let Project(area) =
                                let i = Utils.ImageProjection(full, area)
                                i.StretchDirection <- System.Windows.Controls.StretchDirection.Both
                                i.Stretch <- System.Windows.Media.Stretch.Uniform
                                i.Width <- System.Double.NaN
                                i.Height <- System.Double.NaN
                                i.HorizontalAlignment <- System.Windows.HorizontalAlignment.Center
                                i.VerticalAlignment <- System.Windows.VerticalAlignment.Center
                                i
                            if pps.Projection = -2 then
                                Project(0,0,GameSpecific.TheChosenGame.GAMESCREENW,GameSpecific.TheChosenGame.GAMESCREENH)
                            elif pps.Projection = -1 then
                                Project(GameSpecific.TheChosenGame.MapArea)
                            else
                                if pps.Projection < 0 || pps.Projection >= theGame.CustomProjections.Length then
                                    printfn "bad ppsProjection %d in json (tG.CP.L was %d)" ztr theGame.CustomProjections.Length
                                    System.Console.Beep()
                                    null
                                else
                                    let cp = theGame.CustomProjections.[pps.Projection].XYWH
                                    Project(cp)
            member _.ChangeDataSelection(orig) = failwith "we're not editing these"
            member _.Changed() = failwith "we're not editing these"
            }

let makePreviewPaneCore() =
    // basic validation
    let numZones = theGame.ZoneNames.Length
    if theGame.PreviewPaneLayoutPerZone = null || theGame.PreviewPaneLayoutPerZone.Length < numZones then
        let ra = if theGame.PreviewPaneLayoutPerZone = null then ResizeArray() else ResizeArray(theGame.PreviewPaneLayoutPerZone)
        while ra.Count < numZones do
            ra.Add(null)
        for i = 0 to ra.Count-1 do
            if ra.[i] = null then
                let pps = PreviewPaneSource()
                pps.ZoneToRead <- -1     // current zone
                pps.Projection <- -2     // full
                let t = BasicLayout.JsonableTree<PreviewPaneSource>(0, pps, 0, null, null)      // single pane, full screenshot, current zone
                ra.[i] <- t
        theGame.PreviewPaneLayoutPerZone <- ra.ToArray()
        theGame.Save()
    theGame.PreviewPaneLayoutPerZone.[theGame.CurZone]
let makePreviewPane() =
    let ppl = makePreviewPaneCore()
    let pane = ppl.ToLayoutNode(idp).AsPanel(false)
    pane

open System.Windows
open System.Windows.Controls
open System.Windows.Media

let mkTxt(txt) = new TextBlock(FontSize=12., Text=txt, Foreground=Brushes.Black, Background=Brushes.Gray)

let PPStoDescription(pps:PreviewPaneSource) =
    let zone = if pps.ZoneToRead = -1 then "currentZone" else sprintf "zone%02d" pps.ZoneToRead
    let which = if pps.Projection = -2 then "Full screenshot" elif pps.Projection = -1 then "Map-Trim screenshot" else sprintf "Trim:custom%02d" pps.Projection
    let txt = if pps.ZoneToRead = -2 then "<Empty Pane>" else sprintf "<image read from %s,\nprojected as %s>" zone which
    let tb = mkTxt(txt)
    tb

let ModifyPreviewPaneForCurrentZone(parentWindow, width) = 
    let changedEv = new Event<unit>()
    let editableIdp = 
        { new BasicLayout.IDataPane<PreviewPaneSource> with
            member _.CreateEmptyData() = 
                let pps = PreviewPaneSource()
                pps.ZoneToRead <- -2     // empty pane
                pps
            member _.ProjectDataToUI(pps) = 
                let r = idp.ProjectDataToUI(pps)
                if r = null && pps.ZoneToRead <> -2 then
                    // it's describing data, there just happens to not be any given the current cursor location
                    PPStoDescription(pps)
                else
                    r
            member _.ChangeDataSelection(orig) = 
                let mutable zi = -999
                let zoneOptions = System.Collections.ObjectModel.ObservableCollection<string>()
                let zoneIndexData = ResizeArray()
                for i = 0 to theGame.ZoneNames.Length-1 do
                    zoneOptions.Add(sprintf "zone%02d: %s" i (theGame.ZoneNames.[i]))
                    zoneIndexData.Add(i)
                    if orig.ZoneToRead = i then
                        zi <- i
                zoneOptions.Add("current cursor zone")
                zoneIndexData.Add(-1)
                if orig.ZoneToRead = -1 then
                    zi <- zoneOptions.Count-1
                zoneOptions.Add("this pane should just always be empty")
                zoneIndexData.Add(-2)
                if orig.ZoneToRead = -2 then
                    zi <- zoneOptions.Count-1
                zi <- if zi = -999 then zoneOptions.Count-1 else zi
                let zoneComboBox = new ComboBox(ItemsSource=zoneOptions, IsReadOnly=true, IsEditable=false, SelectedIndex=zi, Margin=Thickness(4.))

                let mutable pi = -999
                let projOptions = System.Collections.ObjectModel.ObservableCollection<string>()
                let projIndexData = ResizeArray()
                for i = 0 to theGame.CustomProjections.Length-1 do
                    projOptions.Add(sprintf "Trim:custom%02d: %s" i (theGame.CustomProjections.[i].Label))
                    projIndexData.Add(i)
                    if orig.Projection = i then
                        pi <- i
                projOptions.Add("Full screenshot")
                projIndexData.Add(-2)
                if orig.Projection = -2 then
                    pi <- projOptions.Count-1
                projOptions.Add("Map-Trim screenshot")
                projIndexData.Add(-1)
                if orig.Projection = -1 then
                    pi <- projOptions.Count-1
                pi <- if pi = -999 then projOptions.Count-2 else pi
                let projComboBox = new ComboBox(ItemsSource=projOptions, IsReadOnly=true, IsEditable=false, SelectedIndex=pi, Margin=Thickness(4.))

                let SafeProject(x) = 
                    let r = idp.ProjectDataToUI(x)
                    if r = null then new DockPanel(Width=160., Height=90.) :> UIElement else r
                let origDesc = PPStoDescription(orig)
                let origPane = SafeProject(orig)
                let curPane = Utils.centerWithGrid(SafeProject(orig))
                let mutable curPPS = orig
                let updateEv = new Event<unit>()
                let update() =
                    let z = zoneIndexData.[zoneComboBox.SelectedIndex]
                    let p = projIndexData.[projComboBox.SelectedIndex]
                    curPPS <- PreviewPaneSource(z,p)
                    curPane.Children.Clear()
                    curPane.Children.Add(SafeProject(curPPS)) |> ignore
                    updateEv.Trigger()
                zoneComboBox.SelectionChanged.Add(fun _ -> update())
                projComboBox.SelectionChanged.Add(fun _ -> update())

                let sp = new StackPanel(Orientation=Orientation.Vertical, Width=width)
                sp.Children.Add(mkTxt("Originally you had")) |> ignore
                sp.Children.Add(origDesc) |> ignore
                let rule() = sp.Children.Add(new DockPanel(Width=0.9*width, Height=2., Background=Brushes.Black)) |> ignore
                rule()
                let gLabels,_ = BasicLayout.NewGridWithColumns([1., GridUnitType.Star; 1., GridUnitType.Star])
                Utils.gridAdd(gLabels, mkTxt("Original"), 0, 0)
                Utils.gridAdd(gLabels, mkTxt("Current"), 1, 0)
                let gImages,_ = BasicLayout.NewGridWithColumns([1., GridUnitType.Star; 1., GridUnitType.Star])
                Utils.gridAdd(gImages, origPane, 0, 0)
                Utils.gridAdd(gImages, curPane, 1, 0)
                gImages.Height <- 100.
                rule()
                let gComboLabels,_ = BasicLayout.NewGridWithColumns([1., GridUnitType.Star; 1., GridUnitType.Star])
                Utils.gridAdd(gComboLabels, mkTxt("Image read from which zone:"), 0, 0)
                Utils.gridAdd(gComboLabels, mkTxt("Image projection:"), 1, 0)
                let gCombos,_ = BasicLayout.NewGridWithColumns([1., GridUnitType.Star; 1., GridUnitType.Star])
                Utils.gridAdd(gCombos, zoneComboBox, 0, 0)
                Utils.gridAdd(gCombos, projComboBox, 1, 0)
                sp.Children.Add(gLabels) |> ignore
                sp.Children.Add(gImages) |> ignore
                sp.Children.Add(gComboLabels) |> ignore
                sp.Children.Add(gCombos) |> ignore
                rule()
                let saveOrDiscardGrid,saved = BasicLayout.MakeSaveOrDiscardButtons(updateEv.Publish)
                let mutable result = orig
                let closeEv = new Event<unit>()
                saved.Add(fun b ->
                    if b then
                        result <- curPPS
                    closeEv.Trigger()
                    )
                sp.Children.Add(saveOrDiscardGrid) |> ignore
                // nested Dialog should have parent be prior dialog
                let activeOwner = Application.Current.Windows |> Seq.cast<Window> |> Seq.tryFind (fun w -> w.IsActive) |> (function | Some(w) -> w | None -> parentWindow)
                Utils.DoModalDialog(activeOwner, sp, "Select new pane source", closeEv.Publish)
                result
            member _.Changed() = changedEv.Trigger()
            }
    let ppl = makePreviewPaneCore()
    let tree = ppl.ToLayoutNode(editableIdp)
    let r = BasicLayout.RunBasicLayoutEditorDialog(parentWindow, sprintf "Modify Preview Pane Layout for zone%02d" theGame.CurZone, width, tree, changedEv.Publish)
    match r with
    | None -> ()
    | Some(newTree) ->
        theGame.PreviewPaneLayoutPerZone.[theGame.CurZone] <- newTree
        theGame.Save()
