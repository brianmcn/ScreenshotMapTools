module Winterop

open System
open System.Runtime.InteropServices
open System.Text

[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type POINT =
    val mutable x:int
    val mutable y:int
[<Struct>]
[<StructLayout(LayoutKind.Sequential)>]
type RECT =
    val mutable left:int
    val mutable top:int
    val mutable right:int
    val mutable bottom:int

type WinEventDelegate = delegate of IntPtr * uint32 * IntPtr * int * int * uint32 * uint32 -> unit
type EnumWindowsProc = delegate of IntPtr * IntPtr -> bool
type HWND = IntPtr
type HANDLE = IntPtr
type HMODULE = IntPtr
type DWORD = int

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

//////////////////////////////////////////////////////

// note https://stackoverflow.com/questions/20444735/issue-with-setforegroundwindow-in-net

module Win32 =
    open System
    open System.Runtime.InteropServices
    [<DllImport("USER32.DLL")>]
    extern bool SetForegroundWindow(IntPtr hwnd)
    [<DllImport("USER32.DLL")>]
    extern bool ShowWindow(IntPtr hWnd, int nCmdShow)
    [<DllImport("USER32.DLL")>]
    extern IntPtr SetWindowLongPtrA(IntPtr hWnd, int nIndex, IntPtr dwNewLong)
    [<DllImport("USER32.DLL")>]
    extern IntPtr GetWindowLongPtrA(IntPtr hWnd, int nIndex)
    [<DllImport("User32.dll")>]
    extern bool PrintWindow(System.IntPtr hwnd, nativeint hdcBlt, uint32 nFlags)

//////////////////////////////////////////////////////

    [<DllImport("USER32.DLL")>]
    extern bool EnumWindows(EnumWindowsProc enumFunc, IntPtr lParam)
    [<DllImport("USER32.DLL")>]
    extern int GetWindowText(HWND hWnd, StringBuilder lpString, int nMaxCount)
    [<DllImport("USER32.DLL")>]
    extern int GetWindowTextLength(HWND hWnd)
    [<DllImport("USER32.DLL")>]
    extern DWORD GetWindowThreadProcessId(HWND hWnd, [<Out>] DWORD& lpdwProcessId)
    [<DllImport("USER32.DLL", SetLastError = true)>]
    extern bool IsWindowVisible(HWND hWnd)
    [<DllImport("USER32.DLL")>]
    extern HWND GetShellWindow()
    [<DllImport("KERNEL32.DLL")>]
    extern HANDLE OpenProcess(DWORD dwDesiredAccess, bool bInheritHandle, DWORD dwProcessId)
    [<DllImport("KERNEL32.DLL")>]
    extern bool QueryFullProcessImageNameA(HANDLE hProcess, DWORD dwFlags, StringBuilder lpExeName, DWORD& lpdwSize)
    [<DllImport("KERNEL32.DLL")>]
    extern bool CloseHandle(HANDLE hObject)
    [<DllImport("user32.dll", SetLastError = true)>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool GetWindowRect(IntPtr hWnd, [<Out>] RECT& lpRect)
    [<DllImport("user32.dll", SetLastError = true)>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool GetClientRect(IntPtr hWnd, [<Out>] RECT& lpRect)

    //////////////////////////////////////////////////////

    [<DllImport("user32.dll")>]
    extern IntPtr GetForegroundWindow()

    [<DllImport("USER32.DLL", SetLastError = true)>]
    extern bool ClientToScreen(IntPtr hWnd, [<In;Out>] POINT& lpPoint)

    [<DllImport("user32.dll", SetLastError = true)>]
    extern bool SetWindowPos(IntPtr hWnd, IntPtr hWndInsertAfter, int x, int y, int cx, int cy, uint32 uFlags)
    [<DllImport("user32.dll", SetLastError = true)>]
    extern IntPtr GetWindow(IntPtr hWnd, uint32 uCmd)
    [<DllImport("user32.dll", SetLastError = true)>]
    extern IntPtr GetTopWindow(IntPtr hWnd)

// Win32 Constants
let GW_HWNDNEXT = 2u
let GW_HWNDPREV = 3u
let HWND_NOTOPMOST = nativeint -2
let HWND_TOP = nativeint 0
let SWP_NOMOVE     = 0x0002u
let SWP_NOSIZE     = 0x0001u
let SWP_NOACTIVATE = 0x0010u


