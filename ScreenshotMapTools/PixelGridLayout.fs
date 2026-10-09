module PixelGridLayout

type PixelGridLayout(gridW:int, gridH:int, imgW:int, imgH:int) =
    let centerULX = (gridW-imgW)/2
    let centerULY = (gridH-imgH)/2
    // i,j of 0,0 represents the image in the center of the grid
    let minI =
        let mutable i = 0
        let mutable x = centerULX
        while x > 0 do
            i <- i - 1
            x <- x - imgW
        i
    let minJ =
        let mutable j = 0
        let mutable y = centerULY
        while y > 0 do
            j <- j - 1
            y <- y - imgH
        j
    let maxI =
        let mutable i = 0
        let mutable x = centerULX
        while x + imgW < gridW-1 do
            i <- i + 1
            x <- x + imgW
        i
    let maxJ =
        let mutable j = 0
        let mutable y = centerULY
        while y + imgH < gridH-1 do
            j <- j + 1
            y <- y + imgH
        j
    let minX = centerULX + minI * imgW
    let minY = centerULY + minJ * imgH
    let maxX = centerULX + (maxI+1) * imgW
    let maxY = centerULY + (maxJ+1) * imgH
    // i,j will take on values from e.g. -3,-2 to 3,2; image coordinates represented as delta from the 0,0 center, ranging over all the images that have any portion visible in the grid
    // the maxes are inclusive e.g. [MinI..MaxI]
    member this.MinI = minI
    member this.MaxI = maxI
    member this.MinJ = minJ
    member this.MaxJ = maxJ
    // x,y will take on values from e.g. -25,-20 to 680,470 and be like the size of the canvas you need to draw the grid containing all the images that are visible in the grid
    // x,y of 0,0 represents the top left corner of the grid, which is probably somewhere in the middle of an image that is partially cut off in the upper left
    // the maxes are not inclusive e.g. [MinX..MaxX) as (MaxX,MaxY) is the upper left corner of the image that didn't fit into the grid off the bottom right
    member this.MinX = minX
    member this.MaxX = maxX
    member this.MinY = minY
    member this.MaxY = maxY
    member this.XW = maxX - minX
    member this.YH = maxY - minY
    member this.GetIJFromXY(x:int, y:int) =
        let i = minI + (x - minX) / imgW
        let j = minJ + (y - minY) / imgH
        struct(i,j)
    member this.GetULXYFromIJ(i:int, j:int) =
        let x = minX + (i - minI) * imgW
        let y = minY + (j - minJ) * imgH
        struct(x,y)
    member this.Setup(e:System.Windows.FrameworkElement) =
        System.Windows.Controls.Canvas.SetLeft(e, this.MinX)
        System.Windows.Controls.Canvas.SetTop(e, this.MinY)
        e.Width <- float this.XW
        e.Height <- float this.YH

let DecideWHBasedOnHowManyWeWantToFit(gridW, gridH, imgAspect, howMany) =
    let gridAspect = float gridW / float gridH
    if gridAspect > imgAspect then
        let H = int(float gridH / howMany)
        let W = int(float H * imgAspect)
        struct(W,H)
    else
        let W = int(float gridW / howMany)
        let H = int(float W / imgAspect)
        struct(W,H)


