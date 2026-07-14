module WindowFocusTrackingUtils

open System
open System.Runtime.InteropServices
open System.Text

open Winterop

let mutable previousForegroundWindow = IntPtr.Zero
let mutable curId = 0
let mutable theHookDelegate : WinEventDelegate = null    // need to be kept alive and not GC'd, so a global var
let watchers = new System.Collections.Generic.Dictionary<int,(IntPtr*IntPtr)->unit>()   // IntPtrs are hwnds to (prev, newly) focused windows
let addWindowFocusWatcher(f) =
    curId <- curId + 1
    watchers.Add(curId, f)
    curId
let removeWindowFocusWatcher(id) =
    watchers.Remove(id) |> ignore
let testWindowFocusWatcher() =  // just a test function to try it out
    addWindowFocusWatcher(fun (p,n) -> printfn "prev: %A, new %A" p n) |> ignore
do  // do this once at startup to install the callback hook
    let winEventCallback (_hWinEventHook: IntPtr) (_eventType: uint32) (hwnd: IntPtr) (idObject: int) (idChild: int) (_dwEventThread: uint32) (_dwmsEventTime: uint32) =
        if idObject = 0 && idChild = 0 then
            if hwnd <> previousForegroundWindow && previousForegroundWindow <> IntPtr.Zero then
                // we may receive this Windows callback from a non-UI thread, so marshal it
                System.Windows.Application.Current.Dispatcher.BeginInvoke(new System.Action(fun () -> 
                    for wf in watchers.Values do
                        wf(previousForegroundWindow, hwnd)
                    )) |> ignore
            previousForegroundWindow <- hwnd
    previousForegroundWindow <- NativeMethods.GetForegroundWindow()
    theHookDelegate <- WinEventDelegate(winEventCallback)
    let hookHandle = NativeMethods.SetWinEventHook(EVENT_SYSTEM_FOREGROUND, EVENT_SYSTEM_FOREGROUND, IntPtr.Zero, theHookDelegate, 0u, 0u, WINEVENT_OUTOFCONTEXT)
    if hookHandle = IntPtr.Zero then
        failwith "Failed to install win32 event hook."


