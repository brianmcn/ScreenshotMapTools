## Glass

The 'glass' button in the upper right launches a separate app window which allow you to put 
a "pane of transparent glass you can draw on" over any window (such as the game).  You can 
use it for basic temporary 'drawing' tasks to display an idea to livestream viewers, for example.

![glass example](img/GlassExample.png)

Each glass window has the title "LorgonGlass X" where "X" is the title of the window it is
targeting.  This helps manage OBS layouts where you might want to draw atop multiple windows;
you can have multiple 'Window title must match' sources in OBS to capture multiple instances of
glass, each with the same layout/size as its target window in your OBS scene.

The controls for scribbling are straightforward; left-click-drag draws a thick line.  You can switch
among 8 colors by clicking the color in the GlassControl window next to the glass pane.  If you click
the 'arrowheads' checkbox, then each of your strokes will automatically get an arrowhead at the end of
it.  `Ctrl+Z` and `Ctrl-Y` have a very simple undo/redo buffer for strokes.  The 'erase all' button in
the GlassControl window will remove all the scribbles.  

The GlassControl is modal, toggling between 'drawing' and 'click-thru' modes.  It starts out in 
'drawing' mode, where clicks on the glass pane are interpreted as scribbling strokes.  If you want to 
interact with the window below the glass pane with the mouse, click the 'switch to click-thru' button
on the GlassControl.  That will toggle the mode, and mouse clicks will now go through the glass pane
and be received by the window beneath it.  To switch back, click the 'switch to drawing' button in
the GlassControl.

There is currently no mechanism for 'saving' glass drawings; they are just temporary scribble windows.
