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

#if !BAR

let GAME = "Animal Well"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "ANIMAL WELL"  // of the process to find
let NATIVE_FACTOR = 1         // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 1280, 720
let MapArea  = 0, 0, 1280, 720         // x,y,w,h
let MetaArea = 0, 0, 100, 1

#else

#if BAZ

let GAME = "Zelda"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "FCEUX 2.6.4"  // of the process to find
let NATIVE_FACTOR = 1         // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 768, 672
let MapArea  = 0, 168, 768, 504         // x,y,w,h
let MetaArea = 0, 0, 100, 1

#else

#if QUX

let GAME = "Master Key"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Master Key"  // of the process to find
let NATIVE_FACTOR = 1            // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 1136, 640
let MapArea  = 176, 0, 960, 640         // x,y,w,h
let MetaArea = 0, 0, 100, 1

#else

#if SS

let GAME = "Side Scape"          // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Side Scape"  // of the process to find
let NATIVE_FACTOR = 1            // e.g. I am running it at 1x native pixel size
let GAMESCREENW, GAMESCREENH = 768, 704
let MapArea  = 0, 64, 768, 640         // x,y,w,h
let MetaArea = 200, 0, 10, 1

#else

#if IOSAS

let GAME = "IoSaS"               // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Isles of Sea and Sky"  // of the process to find
let GAMESCREENW, GAMESCREENH = 1152, 648
let MapArea  = 0, 96, 1152, 552         // x,y,w,h
let MetaArea = 200, 0, 10, 1

#else

let GAME = "Hatchwell"               // name of save folder for this program's assets (screenshots etc)
let WINDOW_TITLE = "Hatchwell"  // of the process to find
let GAMESCREENW, GAMESCREENH = 1280, 720
let MapArea  = 0, 0, 1280, 620         // x,y,w,h
let MetaArea = 200, 0, 10, 1

#endif

#endif

#endif

#endif 

#endif

#endif

let MapAreaRectangle =
    let x,y,w,h = MapArea
    new System.Drawing.Rectangle(x,y,w,h)
let MetaAreaRectangle =
    let x,y,w,h = MetaArea
    new System.Drawing.Rectangle(x,y,w,h)

//////////////////////////////////////////////////////////////////////////
// common computations

let GAMEASPECT = float(GAMESCREENW) / float(GAMESCREENH)


