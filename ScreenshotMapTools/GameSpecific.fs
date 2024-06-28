module GameSpecific

// recompile for different uses, for now

//let GAME = "Void Stranger"
//let WINDOW_TITLE = "ScreenshotMapTools (Running) - Microsoft Visual Studio"  //"Void Stranger"

#if FOO

let GAME = "Leafs Odyssey"   // recompile for different uses, for now
let WINDOW_TITLE = "Leaf's Odyssey"
let NATIVE_FACTOR = 2         // e.g. I am running it at 2x native pixel size
let GAMESCREENW, GAMESCREENH = 960, 540
let VIEWX = 630   // multiple of 9, 7, 5 so that we can so various zooms well onscreen
let MapArea  =  96, 14, 384, 256         // x,y,w,h
let MetaArea = 230,  0, 120,  14

#else

let GAME = "Animal Well"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "ANIMAL WELL"  // of the process to find
let NATIVE_FACTOR = 1         // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 1280, 720
let MapArea  = 0, 0, 1280, 720         // x,y,w,h
let MetaArea = 0, 0, 100, 1

#endif



