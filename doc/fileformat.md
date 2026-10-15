## File Format

This is how the directory structure of the app looks:

```
InstallFolder\
├── ScreenshotMapTools.exe
├── <a bunch of DLLs>
├── app.json
├── Game1Folder\
	└── ...
├── Game2Folder\
	├── CurrentGame.json
	├── game.json
	├── icons.json
	├── screenshots\
		├── 2026-12-31-23-59-48.png
		├── 2026-12-31-23-59-59.png
		└── ...
	├── zone00\
		└── ...
	├── zone01\
		├── full-cache\
			├── 50-50.png
			└── ...
		├── map-cache\
			├── 50-50.png
			└── ...
		├── tile50-50.json
		└── ...
	└── ...
└── ...
```

Here's a quick summary of data in the various json files:

- `app.json`: window locations and sizes
- `CurrentGame.json`: game window title and process name, window size, Map Trim
- `game.json`: zone names, cursor location, global note, custom projections and preview pane customizations, quicknav hashtags
- `icons.json`: hashtag colors and icons
- `tileRR-CC.json`: screenshots and Note for this tile (e.g. (50,50) row and column)

The screenshots are all full-window screenshots stored with timestamp filenames YYYY-MM-DD-HH-MM-SS.png 
(yes, you can take at most one screenshot per second) and are referenced by name in tileRR-CC.json files.

The full-cache and map-cache directories store cached composite images of full screenshots and Map-Trims
for each grid tile (since each grid tile may contain multiple overlaid screenshots, the composite overlay 
is cached to disk so that the app can load faster on subsequent runs).

For full details of the file format, see the source code; `JsonSerializer` is used on a few class types 
to read and write the data.