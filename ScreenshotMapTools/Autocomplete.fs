module Autocomplete

open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Input
open System.Windows.Media
open Utils
open Utils.Extensions

let DoAutocompleteModalTextDialog(parentWindow, windowTitle, postExplainerText, origText, winWidth, winHeight, textChangedCallback) =
    let tb = new TextBox(IsReadOnly=false, FontSize=12., Text=(if origText=null then "" else origText), BorderThickness=Thickness(1.), 
                            Foreground=Brushes.Black, Background=Brushes.White,
                            Width=winWidth, Height=winHeight, TextWrapping=TextWrapping.Wrap, AcceptsReturn=true, AcceptsTab=true,
                            VerticalScrollBarVisibility=ScrollBarVisibility.Visible)
    let tagUniverse = InMemoryStore.metadataStore.AllKeys() |> Array.sort       // autocompletion source
    let listBox = ListBox(MaxHeight=140.0, BorderThickness=Thickness(1.0))
    do  // style listbox selection
        let itemStyle = new Style(typeof<ListBoxItem>)
        let selectedTrigger = new Trigger(Property=ListBoxItem.IsSelectedProperty, Value=true)
        selectedTrigger.Setters.Add(new Setter(ListBoxItem.FontWeightProperty, FontWeights.Bold))
        itemStyle.Triggers.Add(selectedTrigger)
        listBox.ItemContainerStyle <- itemStyle
    let popup = Popup(Placement=PlacementMode.Custom, StaysOpen=false, Child=listBox, PlacementTarget=tb, 
                        CustomPopupPlacementCallback=CustomPopupPlacementCallback(fun popupSize targetSize offset ->
                            let rect = tb.GetRectFromCharacterIndex(tb.CaretIndex)
                            if not rect.IsEmpty then    [| CustomPopupPlacement(Point(rect.Left, rect.Bottom + 2.0), PopupPrimaryAxis.Horizontal) |]
                            else                        [| CustomPopupPlacement(Point(0.0, targetSize.Height), PopupPrimaryAxis.Horizontal) |]
                        ))
    let mutable preCommitmentTag : struct (int * string) option = None  // for backspace-recovery after accidental tag completion
    let getActiveTagContext() =
        let text = tb.Text
        let caret = tb.CaretIndex
        if caret = 0 then None
        else
            let mutable i = caret - 1
            while i > 0 && GenericMetadata.IsHashtagChar(text.[i]) do
                i <- i - 1
            let startIdx = i
            if startIdx < text.Length && text.[startIdx] = '#' then
                let tagLength = caret - startIdx
                let tag = text.Substring(startIdx, tagLength)
                Some(startIdx, tag)
            else 
                None
    let updatePopupVisibility() =
        match getActiveTagContext() with
        | Some(_,tag) ->
            let tag = tag.Substring(1)  // strip the '#'
            let filtered = tagUniverse |> Array.filter (fun s -> s.StartsWith(tag, System.StringComparison.OrdinalIgnoreCase))
            if filtered.Length > 0 then
                listBox.ItemsSource <- filtered
                if not popup.IsOpen then
                    popup.IsOpen <- true
                    listBox.SelectedIndex <- -1
                if filtered.Length = 1 then
                    listBox.SelectedIndex <- 0  // special case to auto-select when only one item
            else
                popup.IsOpen <- false
        | None -> popup.IsOpen <- false
    let mutable dontFireChangedEvents = false
    let commitSelection() =
        if listBox.SelectedItem <> null then
            let selectedTag = listBox.SelectedItem :?> string
            match getActiveTagContext() with
            | Some(startIdx, tag) ->
                let replacement = "#" + selectedTag + " "
                preCommitmentTag <- Some(startIdx, tag)
                let currentText = tb.Text
                dontFireChangedEvents <- true
                tb.Text <- currentText.Remove(startIdx, tag.Length).Insert(startIdx, replacement)
                tb.CaretIndex <- startIdx + replacement.Length
                textChangedCallback(tb.Text, tb.CaretIndex, tb.SelectionStart, tb.SelectionLength)
                dontFireChangedEvents <- false
                popup.IsOpen <- false
            | None -> ()
    tb.LostFocus.Add(fun _ -> popup.IsOpen <- false)
    tb.TextChanged.Add(fun _ -> if not(dontFireChangedEvents) then (updatePopupVisibility(); textChangedCallback(tb.Text, tb.CaretIndex, tb.SelectionStart, tb.SelectionLength)))
    tb.SelectionChanged.Add(fun _ -> if not(dontFireChangedEvents) then textChangedCallback(tb.Text, tb.CaretIndex, tb.SelectionStart, tb.SelectionLength))
    let closeEv = new Event<unit>()
    let mutable save = false
    tb.PreviewKeyDown.Add(fun ea ->
        if ea.Key <> Key.Back then 
            preCommitmentTag <- None   // anything other than Backspace clears the oops-undo-last-commit tracker
        if popup.IsOpen then
            match ea.Key with
            | Key.Down ->
                ea.Handled <- true
                if listBox.SelectedIndex < listBox.Items.Count - 1 then
                    listBox.SelectedIndex <- listBox.SelectedIndex + 1
                    listBox.ScrollIntoView(listBox.SelectedItem)
            | Key.Up ->
                ea.Handled <- true
                if listBox.SelectedIndex > 0 then
                    listBox.SelectedIndex <- listBox.SelectedIndex - 1
                    listBox.ScrollIntoView(listBox.SelectedItem)
            | Key.Tab ->
                ea.Handled <- true
                commitSelection()
            | Key.Escape ->
                ea.Handled <- true
                popup.IsOpen <- false
            | Key.Left | Key.Right ->
                // don't handle, but close popup since cursor is moving
                popup.IsOpen <- false
            | _ -> ()
        if ea.Key = Key.Back then
            match preCommitmentTag with
            | Some(startIdx, tag) ->
                ea.Handled <- true
                preCommitmentTag <- None
                let caret = tb.CaretIndex
                dontFireChangedEvents <- true
                tb.Text <- tb.Text.Remove(startIdx, caret-startIdx).Insert(startIdx, tag)
                tb.CaretIndex <- startIdx + tag.Length
                textChangedCallback(tb.Text, tb.CaretIndex, tb.SelectionStart, tb.SelectionLength)
                dontFireChangedEvents <- false
                updatePopupVisibility()
            | _ -> ()
        elif ea.Key = Key.Enter then
            if ((Keyboard.Modifiers &&& ModifierKeys.Control) = ModifierKeys.Control || (Keyboard.Modifiers &&& ModifierKeys.Shift) = ModifierKeys.Shift) then 
                // make <shift/ctrl>-enter behave like a normal textbox 'return'
                ea.Handled <- true
                let caretIndex = tb.CaretIndex
                tb.Text <- tb.Text.Insert(caretIndex, System.Environment.NewLine)
                tb.CaretIndex <- caretIndex + 1
            else
                // enter behaves like they click save
                ea.Handled <- true
                save <- true
                closeEv.Trigger()
        )
    let cb = new Button(Content=" Cancel ", Margin=Thickness(4.))
    let sb = new Button(Content=" Save ", Margin=Thickness(4.))
    cb.Click.Add(fun _ -> closeEv.Trigger())
    sb.Click.Add(fun _ -> save <- true; closeEv.Trigger())
    let dp = (new DockPanel(LastChildFill=true)).AddLeft(cb).AddRight(sb).Add(new DockPanel())
    let explainerText = "Press **Enter** to Save\nPress **Ctrl+Enter** for newline\n\nFor #hashtags, suggested completions may appear\nand you can press **Up**/**Down** to choose among them,"
                            + "\n**Tab** to complete the chosen selection,\nor **Esc** to dismiss the completion popup"
    let explainerTb = new TextBlock(FontSize=12.,Foreground=Brushes.Black, Background=Brushes.White, TextWrapping=TextWrapping.NoWrap, Margin=Thickness(5.))
    Utils.StarStarBold(explainerText, explainerTb)
    let sp = new StackPanel(Orientation=Orientation.Vertical, Margin=Thickness(5.))
    let explainerTb : UIElement = 
        if postExplainerText <> null then
            let sp = new StackPanel(Orientation=Orientation.Vertical)
            sp.Children.Add(new TextBlock(FontSize=12.,Foreground=Brushes.Black, Background=Brushes.White,Text="General Textbox Instructions:")) |> ignore
            explainerTb.Margin <- Thickness(20.,0.,0.,0.)
            sp.Children.Add(explainerTb) |> ignore
            sp.Children.Add(new DockPanel(Height=2., Background=Brushes.Gray, Margin=Thickness(2.,2.,2.,0.))) |> ignore
            sp
        else
            explainerTb
    sp.Children.Add(explainerTb) |> ignore
    if postExplainerText <> null then
        let postExplainerTb = new TextBlock(FontSize=16.,Foreground=Brushes.Black, Background=Brushes.White,
                                            Width=winWidth,TextWrapping=TextWrapping.Wrap)
        Utils.StarStarBold(postExplainerText, postExplainerTb)
        sp.Children.Add(new DockPanel(Height=2.)) |> ignore
        sp.Children.Add(postExplainerTb) |> ignore
        sp.Children.Add(new DockPanel(Height=2.)) |> ignore
    sp.Children.Add(tb) |> ignore
    sp.Children.Add(popup) |> ignore        // popup is own window with own layout/lifetime management, just needs to live somewhere in our tree
    sp.Children.Add(dp) |> ignore
    tb.Loaded.Add(fun _ ->
        tb.Select(tb.Text.Length, 0)   // position the cursor at the end
        System.Windows.Input.Keyboard.Focus(tb) |> ignore
        )
    DoModalDialog(parentWindow, sp, windowTitle, closeEv.Publish)
    save, tb.Text
