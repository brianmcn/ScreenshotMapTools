module WindowFocusTrackingUtils

open System
open System.Runtime.InteropServices
open System.Text

[<StructLayout(LayoutKind.Sequential)>]
type POINT =
    struct
        val x: int
        val y: int
    end

type WinEventDelegate = delegate of IntPtr * uint32 * IntPtr * int * int * uint32 * uint32 -> unit

module NativeMethods =
    [<DllImport("user32.dll", SetLastError = true)>]
    extern IntPtr SetWinEventHook(uint32 eventMin, uint32 eventMax, IntPtr hmodWinEventProc, WinEventDelegate lpfnWinEventProc, uint32 idProcess, uint32 idThread, uint32 dwFlags)

    [<DllImport("user32.dll", SetLastError = true)>]
    extern bool UnhookWinEvent(IntPtr hWinEventHook)

    [<DllImport("user32.dll")>]
    extern IntPtr GetForegroundWindow()

    [<DllImport("user32.dll", SetLastError = true)>]
    extern uint32 GetWindowThreadProcessId(IntPtr hWnd, [<Out>] uint32& lpdwProcessId)

    [<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern int GetWindowText(IntPtr hWnd, StringBuilder lpString, int nMaxCount)

let EVENT_SYSTEM_FOREGROUND = 0x0003u
let WINEVENT_OUTOFCONTEXT = 0x0000u

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


