module WinteropUtils

open System.Text
open Winterop

let GetWindowClientRect(handle) =
    let mutable r : Winterop.RECT = Unchecked.defaultof<_>
    if Win32.GetClientRect(handle, &r) = false then failwith "bad window"
    let mutable p : Winterop.POINT = Unchecked.defaultof<_>
    if Win32.ClientToScreen(handle, &p) = false then failwith "bad window"
    r.left <- r.left + p.x
    r.right <- r.right + p.x
    r.top <- r.top + p.y
    r.bottom <- r.bottom + p.y
    r
let GetWindowTitle(hwnd) =
    let N = 256
    let buff = new StringBuilder(N)
    if (Win32.GetWindowText(hwnd, buff, N) > 0) then
        buff.ToString()
    else
        null
let GetActiveWindowTitle() =
    let handle = Win32.GetForegroundWindow()
    GetWindowTitle(handle)

let GetActiveWindowClientRect() =
    let handle = Win32.GetForegroundWindow()
    GetWindowClientRect(handle)

