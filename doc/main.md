
## On this page

* [Downloading and running the tool](#downloading)
* [Setting up a new game for the tool](#newgamesetup)
* [Basic Controls - Screenshots on a grid](#basiccontrols)
* [Multiple screenshots per grid cell](#multiplescreenshots)
* [Cut and paste](#cutandpaste)
* [Text notes and #hashtag labels](#textnotes)
* [Popout windows](#popouts)
* [Glass tool for scribbling](#glass)

Jump to any of the linked topics above, or scroll down to read them one-by-one.

## <a id="downloading"></a>Downloading and running the tool

See the [download page](download.md) for information.

## <a id="newgamesetup"></a>Setting up a new game for the tool

TODO

## <a id="basiccontrols"></a>Basic Controls - Screenshots on a grid

The tool is designed to visible to player (off to the side, or on a separate monitor)
while they are playing the game (via keyboard or controller).  The basic controls of
the tool run via num-pad hotkeys that allow you to move the cursor around the map grid 
and take screenshots without ever needing to alt-tab or have the game lose focus.

The top area of the tool window displays a grid for screenshots.  A yellow box around
one cell of the grid is the current location cursor.  The most essential controls:

* `(Numpad) 8 2 4 6`: move the cursor Up Down Left Right on the grid

* `(Numpad) 0`: takes a screenshot of the game window, and drops it into the current grid cell

* `(Numpad) 7` and `9`: zoom In/Out on the map grid, to get a more focused/wider view

The tool supports a grid of 100x100 cells, and starts you by default at coordinate (50,50).
So when you start the game, you can put the first screenshot at (50,50) and then just start 
building your map from there and have plenty of room to build in all 4 directions.  

For an example, here's how you'd use the tool at the very beginning of playing EMUUROM.
When you spawn into the world, press `Numpad 0` to take a screenshot of the first screen.
The only exit is to the left, so after the screen transition in game, press `Numpad 4` to
move the tool's cursor to the left, and then press `Numpad 0` again to take a screenshot
of the second screen and place it left of the first one on the grid.  Repeat the same 
process for the third screen.  Then the only exit from the room is to fall down, so after
that screen transition, press `Numpad 2` to move the cursor down before `Numpad 0` to 
screenshot the 4th room.  The map would now look like this:

![map tool after first 4 screens of EMUUROM](img/DemoScreenshots.png)

Each 100x100 grid is called a 'zone', and you can make multiple zones for games with 
multiple maps (e.g. Zelda1 has an overworld map and 9 dungeon maps).  
TODO link to more info on zones


## <a id="multiplescreenshots"></a>Multiple screenshots per grid cell

You can put multiple screenshots in a single cell of the map grid.  By default this will 
just 'blend' the screenshots into a composite image, which is useful for many games where
the player gives off a 'light halo' and can only clearly see the portion of the screen they 
are currently in.

Here's an example from the game Minit, where 5 different screenshots taken in the same dark
room can be dropped into the same grid cell to generate a composite image for the map which
illuminates the whole room:

![Minit composite image example](img/MinitExample.png)


## <a id="cutandpaste"></a>Cut and paste

You will inevitably make a mistake and put a screenshot in the wrong grid cell.  The tool
has basic cut-and-paste functionality to patch up mistaken screenshots.

Controls:

* `(Numpad) -`: the minus key cuts the most recent screenshot out of the current cell

* `(Numpad) +`: the plus key pastes the most recently cut screenshot to the current cell

The most-recently-cut screenshot appears in a little preview pane in the bottom right corner
of the tool.


## <a id="textnotes"></a>Text notes and #hashtag labels

You can make text notes for each cell on the grid.  

* `(Numpad) /`: pressing slash gives the tool focus and opens a textbox dialog to edit the text
		in the current cell.  

Type with the keyboard.  Press Ctrl-Enter to add a newline in the text box; press Enter when 
you are all done.

When the cursor is on a cell with notes, the bottom left pane of the tool shows a larger
screenshot of that cell, along with your text notes	for that call.

Your text can contain **#hashtags**, alphanumeric labels prefixed by an #octothorpe, which 
enable some more advanced features of tool, like, for example, displaying all the #save checkpoint 
locations you have found and marked up, or all the #shop or #dungeon locations you find, or 
whatever is suitable to the game you are playing. 

Here's an example from Minit where I marked up the different places the player could #spawn, as 
well as left myself some #hmm notes on places I wanted to return to investigate further.  
Left-clicking a #hashtag in the tool's bottom-right-pane list will highlight the #hashtag's 
locations on the map, and right-clicking lets you change the color and shape of the highlight markings.

![hashtags example](img/HashtagsExample.png)

You can also create clickable **hyperlinks** to jump to other cells by typing e.g. coordinates 
in formats like "(45,55)" or "(zone02,51,52)" which can be useful for marking up fast
travel systems in a game, or doors that lead to different dungeon maps, or whatnot.

There is also a 'global note', that is, a note which is not associated with any particular
map cell.  You can edit the global note with

* `(Numpad) Ctrl-/`: pressing ctrl-slash gives the tool focus and opens a textbox dialog to edit the text
		of the global note.  It also creates the global note [popout window](#popouts) if that popout
		was not already active.

The global note is displayed in a popout window, which you can move or resize to keep it visible anywhere 
on your desktop, if desired; see [popouts](#popouts) for more info.

## <a id="popouts"></a>Popout windows

Popouts are little chrome-less windows that display useful projections of information in the app.  One example
popout that new users are likely to want is the 'Controls Cheatsheet' which summarizes the NumPad keyboard
controls of the app:

![controls cheatsheet](img/ControlsCheatsheet.png)

All popouts have the same mouse controls for interacting:

* `Left-click-and-drag` within the window: Move a popout window around on your desktop
* `Right click`: Close this popout window
* `Left-click-and-drag` the window edge/corner: Resize the popout window (if applicable)
* `Scroll-wheel`: change the zoom level of the popout (if applicable)

You can manage popouts with the "Popouts" button at the top of the main app.

See the [popouts page](popouts.md) for more information about each popout.

## <a id="glass"></a>Glass tool for scribbling

See the [glass page](glass.md) for more information on the glass tool, which lets you draw on any window.

![glass example](img/GlassExample.png)


## Other features

TODO eventually document other features (custom trim, preview pane, feature window, ...)


