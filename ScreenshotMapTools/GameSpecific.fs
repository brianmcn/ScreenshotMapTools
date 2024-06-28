module GameSpecific

//let GAME = "Void Stranger"   // recompile for different uses, for now
//let WINDOW_TITLE = "ScreenshotMapTools (Running) - Microsoft Visual Studio"  //"Void Stranger"
let GAME = "Leafs Odyssey"   // recompile for different uses, for now
let WINDOW_TITLE = "Leaf's Odyssey"
let GAMESCREENW, GAMESCREENH = 960, 540
let NATIVE_FACTOR = 2         // e.g. I am running it at 2x native pixel size
let GAMENATIVEW, GAMENATIVEH = GAMESCREENW/NATIVE_FACTOR, GAMESCREENH/NATIVE_FACTOR
let GAMEASPECT = float(GAMENATIVEW) / float(GAMENATIVEH)
let VIEWX = 630   // multiple of 9, 7, 5 so that we can so various zooms well onscreen
let MapArea  =  96, 14, 384, 256         // x,y,w,h
let MetaArea = 230,  0, 120,  14

