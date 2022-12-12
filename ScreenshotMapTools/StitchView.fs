module StitchView

open Utils
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media

type MyWindow(bmps, diffs) as this =
    inherit Window()
    do
        this.Title <- "Screenshot Stitching Viewer"
        this.Left <- 10.
        this.Top <- 10.
        //this.Topmost <- true
        this.SizeToContent <- SizeToContent.WidthAndHeight


        (*
        512x512s
        RL: 136 6 (val: 2161.264693)
        RL: 121 5 (val: 1879.785269)
        RL: 48 193 (val: 2740.186165)
        RL: 96 474 (val: 2939.87096)
        *)
        let c = new Canvas(Width=1600., Height=900.)
        let diffs = 
            [|
            yield 0, 10
            yield! diffs
            |]
        let imgs = bmps |> Array.map BMPtoImage
        imgs |> Array.iter (fun i -> i.Opacity <- 0.6)

        let cumulativeDiffs = Array.init diffs.Length (fun j -> (diffs.[0..j] |> Array.sumBy fst, diffs.[0..j] |> Array.sumBy snd))
        
        let add(n) = let x,y = cumulativeDiffs.[n] in canvasAdd(c, imgs.[n], float x, float y)
            
        add(0)
        this.KeyDown.Add(fun ea ->
            let numOpt = 
                match ea.Key with
                | Input.Key.D1 -> Some 1
                | Input.Key.D2 -> Some 2
                | Input.Key.D3 -> Some 3
                | Input.Key.D4 -> Some 4
                | Input.Key.D5 -> Some 5
                | Input.Key.D6 -> Some 6
                | Input.Key.D7 -> Some 7
                | Input.Key.D8 -> Some 8
                | _ -> None
            match numOpt with
            | Some x ->
                if x-1 < imgs.Length then
                    imgs.[x-1].Opacity <- if imgs.[x-1].Opacity = 0. then 0.6 else 0.
            | _ -> ()
            )
        this.Loaded.Add(fun _ ->
            let ctxt = System.Threading.SynchronizationContext.Current
            let delay = 500
            async {
                for i = 1 to bmps.Length-1 do
                    do! Async.Sleep(delay)
                    do! Async.SwitchToContext ctxt
                    add(i)
            } |> Async.StartImmediate
            )

        this.Content <- c

open Utils
open System.Numerics

let svMain() =
    if false then
        let a = [| 0; 1; 3; 4; 6; 7; 2; 3 |]
        let ca = a |> Array.map (fun x -> Complex(float x, 0.))
        let r1 = Fourier.dft(ca)
        printfn "%A" r1
        Fourier.fft(ca, false)
        printfn "%A" ca

        let a2 = Array2D.init 4 4 (fun x y -> Complex(float(x*4+y), 0.))
        let r = Fourier.fft2(a2, false)
        printfn "%A" r

    if false then
        printfn ""
        let bmp = new System.Drawing.Bitmap("""C:\Users\Admin1\Source\Repos\ScreenshotMapTools\ScreenshotMapTools\bin\Release\net48\GiantSansMini.png""")
        let N = 128
        let x,y = 300,300
        let dx,dy = 25,35
        let rng = new System.Random()
        let grey(c:System.Drawing.Color) = float(int c.R + int c.G + int c.B) + 3.0*rng.NextDouble()
        let arr1 = Array2D.init N N (fun i j -> Complex(grey(bmp.GetPixel(x+i, y+j)), 0.))
        let arr2 = Array2D.init N N (fun i j -> Complex(grey(bmp.GetPixel(x+dx+i, y+dy+j)), 0.))
        let cps = Fourier.crossPowerSpectrum(arr1, arr2)
        //let cps = Fourier.crossPowerSpectrum(arr2, arr1)
        let mutable maxx,maxy = 0,0
        cps |> Array2D.iteri (fun i j z -> 
                    if z.Magnitude > cps.[maxx,maxy].Magnitude then 
                        maxx <- i
                        maxy <- j)
        printfn "%d %d" maxx maxy
        //printfn "%A" arr1
        //printfn "%A" cps

    if false then
        let mutable n = 1
        while true do
            printfn "press enter to take screenshot"
            System.Console.ReadLine() |> ignore
            let bmp = Knytt.Screenshot.getScreenBitmap()
            bmp.Save(sprintf """RL%02d.png""" n, System.Drawing.Imaging.ImageFormat.Png)
            n <- n + 1

    if false then
        for i = 1 to 5 do
            let orig = System.Drawing.Bitmap.FromFile(sprintf "RL%02d.png" i) :?> System.Drawing.Bitmap
            let scaled = new System.Drawing.Bitmap(orig, System.Drawing.Size(512, 512))
            scaled.Save(sprintf "RLSQ%02d.png" i, System.Drawing.Imaging.ImageFormat.Png)
        0
        
    else

    if false then
        printfn ""
        printfn ""
        let N = 8
        let test = Array2D.init N N (fun x y -> Complex(float(x*N+y), 0.))
        let window = Array2D.create N N 1
        window.[0,0] <- 0
        window.[2,2] <- 0
        window.[N-1,0] <- 0
        let newssd = Fourier.naiveEwssd(test, test, window, window)
        let pretty(a:Complex[,]) = a |> Array2D.map (fun z -> z.Magnitude |> int)
        printfn "orig:   %A" (pretty test)
        printfn "newssd: %A" (pretty newssd)
        let ewssd = Fourier.ewssd(test, test, window, window)
        printfn "ewssd:  %A" (pretty ewssd)
    else
        (*
        
        TODO

        DONE: generalize the display viewer
        DONE via wrap(): figure out why only good at down & right moves
        PART mask window out just the noteworthy HUD components, and maybe also player
        DONE good masks yielding imprecise results, wonder if can use phase correlation nearby local maxima as a hint?
            DONE (is it possible to do masks with phase correlation? i think in principle no, but i noised the mask, so kinda?)
        try again with normal aspect ratio pics, but with HUD windowed out

        *)
        //let w = Fourier.hamming2(N)
        
        
        //let N = 1024
        //let bmps = [| for i = 1 to 5 do yield System.Drawing.Bitmap.FromFile(sprintf "RL%02d.png" i) :?> System.Drawing.Bitmap |]
        let N = 512
        let bmps = [| for i = 1 to 5 do yield System.Drawing.Bitmap.FromFile(sprintf "RLSQ%02d.png" i) :?> System.Drawing.Bitmap |]
        let maskfile = "RLSQmask.png"
        let mask = System.Drawing.Bitmap.FromFile(maskfile) :?> System.Drawing.Bitmap
        let getGreyAndWindow(bmp:System.Drawing.Bitmap) = 
            // center image in the NxN box
            let dx = (N - bmp.Width) / 2
            let dy = (N - bmp.Height) / 2
            let window = Array2D.zeroCreate N N
            let grey = Array2D.zeroCreate N N
            for i = dx to dx + bmp.Width-1 do
                for j = dy to dy+bmp.Height-1 do
                    let c = bmp.GetPixel(i-dx, j-dy)
                    let g = float(int c.R + int c.G + int c.B)
                    //let g = float(int c.G)
                    grey.[i,j] <- Complex(g, 0.)
                    if N=512 then
                        //if j > 150 then  // this if-line is a kludge (remove HUD) BUT VERY GOOD
                        //  if j < 480 then  // this if-line is a kludge (remove HUD) BUT VERY GOOD
                        //   if i>4 && i<508 then 
                        if mask.GetPixel(i,j).ToArgb() <> System.Drawing.Color.White.ToArgb() then   // remove HUD (whited out in my custom mask file)
                            window.[i,j] <- 1
                    else
                        if j > 150 then  // this if-line is a kludge (remove HUD) BUT VERY GOOD
//                              if j < 500 then  // this if-line is a kludge (remove HUD) BUT VERY GOOD
                            window.[i,j] <- 1
            grey, window
        let wrap(N,d) = if d > N/2 then d-N else d
        let naiveWindowOverlapArea(dx,dy) =
            // account for toroidal wrapping, e.g. right N-5 really means left 5
            let dx = wrap(N,dx) |> abs
            let dy = wrap(N,dy) |> abs
            // if non-square bmp sits inside square area
            let w = max 0 (bmps.[0].Width - dx)
            let h = max 0 (bmps.[0].Height - dy)
            w*h
        let windowOverlapArea(w1:int[,],w2:int[,],dx,dy) =
            let mutable r = 0
            for i = 0 to N-1 do
                for j = 0 to N-1 do
                    r <- r + w1.[i,j] * w2.[(i+dx)%N, (j+dy)%N]
            r
        // all windows are same, so precompute
        let _g1,w1 = getGreyAndWindow(bmps.[0])
        let maskdatafile = maskfile + ".woa.txt"
        let woa = 
            if System.IO.File.Exists(maskdatafile) then
                let lines = System.IO.File.ReadAllLines(maskdatafile)
                let r = Array2D.zeroCreate N N
                for y = 0 to N-1 do
                    let nums = lines.[y].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    for x = 0 to N-1 do
                        r.[x,y] <- int nums.[x]
                r
            else
                let r = Array2D.init N N (fun dx dy -> (if dy=0 then printf "%d " dx); windowOverlapArea(w1,w1,dx,dy))
                let lines = Array.init N (fun y ->
                    let sb = new System.Text.StringBuilder()
                    for x = 0 to N-1 do
                        sb.Append(r.[x,y]).Append(' ') |> ignore
                    sb.ToString()
                    )
                System.IO.File.WriteAllLines(maskdatafile, lines)
                r
        printfn ""
        let diffs = [|
            for d = 0 to bmps.Length-2 do
                let g1,w1 = getGreyAndWindow(bmps.[d])
                let g2,w2 = getGreyAndWindow(bmps.[d+1])
                let essd = Fourier.ewssd(g1, g2, w1, w2)
                let scaledEssdMagnitude = essd |> Array2D.mapi (fun i j z -> z.Magnitude / float(woa.[i,j])) //windowOverlapArea(w1,w2,i,j)))
                let cps = scaledEssdMagnitude
                let mutable minx,miny = 0,0
                cps |> Array2D.iteri (fun i j z -> 
                            if z < cps.[minx,miny] && not(Double.IsInfinity(z)) then
                                minx <- i
                                miny <- j)
                printfn "RLessd: %d %d (val: %A)" minx miny cps.[minx,miny]
                if true then
                    let cpsBmp = new System.Drawing.Bitmap(N, N)
                    cps |> Array2D.iteri (fun i j z -> 
                                let THRESH = 9000.
                                let m = if Double.IsInfinity(z) || z > THRESH then 0 else int(255.*((THRESH-z)/THRESH))
                                cpsBmp.SetPixel(i, j, System.Drawing.Color.FromArgb(m,m,m)))
                    cpsBmp.Save(sprintf "RLessd%02d.png" d, System.Drawing.Imaging.ImageFormat.Png)
                let essdBest = wrap(N,minx),wrap(N,miny)
                // The essd is often a few pixels off.  phase correlation usually has a local maximum nearby at the correct value.
                // So try to refine the estimate...


                // my idea: add noise in parts we try to mask out, since phase correlation is robust to noise
                let rng = new System.Random()
                let SCA = 20.
                let ng1 = Array2D.init N N (fun i j -> Complex(g1.[i,j].Real + (if w1.[i,j] = 0 then (rng.NextDouble()-0.5)*SCA else 0.), g1.[i,j].Imaginary))
                let ng2 = Array2D.init N N (fun i j -> Complex(g2.[i,j].Real + (if w1.[i,j] = 0 then (rng.NextDouble()-0.5)*SCA else 0.), g2.[i,j].Imaginary))
                // zero-pad to remove edge effects
                let zg1 = Array2D.init (2*N) (2*N) (fun i j -> if i>=N/2 && i<3*N/2 && j>=N/2 && j<3*N/2 then ng1.[i-N/2, j-N/2] else Complex.Zero)
                let zg2 = Array2D.init (2*N) (2*N) (fun i j -> if i>=N/2 && i<3*N/2 && j>=N/2 && j<3*N/2 then ng2.[i-N/2, j-N/2] else Complex.Zero)
                let cps = Fourier.crossPowerSpectrum(zg1, zg2)
                // map magnitude
                let cps = cps |> Array2D.map (fun z -> z.Magnitude)
                if false then
                    // kludge it (get rid of artificial spike at 0,0, by zeroing out edges)
                    for dx = 0 to 1 do
                        for y = 0 to 2*N-1 do
                            cps.[dx,y] <- 0.
                            cps.[2*N-1-dx,y] <- 0.
                    for dy = 0 to 1 do
                        for x = 0 to 2*N-1 do
                            cps.[x,dy] <- 0.
                            cps.[x,2*N-1-dy] <- 0.
                // make it less intense (for plotting)
                let cps = cps |> Array2D.map (fun z -> System.Math.Log10 (1.00001 + z))
                let mutable maxx,maxy = 0,0
                cps |> Array2D.iteri (fun i j z -> 
                            if z > cps.[maxx,maxy] then 
                                maxx <- i
                                maxy <- j)
                printfn "RLcps: %d %d" maxx maxy
                let maxMag = cps.[maxx,maxy]
                let cpsBmp = new System.Drawing.Bitmap(2*N, 2*N)
                cps |> Array2D.iteri (fun i j z -> 
                            let m = int(255.*z/maxMag) 
                            cpsBmp.SetPixel(i, j, System.Drawing.Color.FromArgb(m,m,m)))
                cpsBmp.Save(sprintf "RLcps%02d.png" d, System.Drawing.Imaging.ImageFormat.Png)


                // use cps to refine essd
                let NEIGHBORHOOD = 15
                let essdX,essdY = essdBest
                maxx <- (2*N + essdX - NEIGHBORHOOD) % (2*N)
                maxy <- (2*N + essdY - NEIGHBORHOOD) % (2*N)
                for dx = -NEIGHBORHOOD to NEIGHBORHOOD do
                    for dy = -NEIGHBORHOOD to NEIGHBORHOOD do
                        let i = (2*N + essdX + dx)%(2*N)
                        let j = (2*N + essdY + dy)%(2*N)
                        if cps.[i,j] > cps.[maxx,maxy] then
                            maxx <- i
                            maxy <- j

                // yield best nearby phase correlation to essd global estimate as the final result
                let revised = wrap(2*N,maxx), wrap(2*N,maxy)
                printfn "revised: %A" revised
                yield revised
            |]
                (*
        let g1Bmp = new System.Drawing.Bitmap(N, N)
        g1 |> Array2D.iteri (fun i j z -> 
                    let m = int(z.Magnitude/3.)
                    g1Bmp.SetPixel(i, j, System.Drawing.Color.FromArgb(m,m,m)))
        g1Bmp.Save("RLg1.png", System.Drawing.Imaging.ImageFormat.Png)
        let g2Bmp = new System.Drawing.Bitmap(N, N)
        g2 |> Array2D.iteri (fun i j z -> 
                    let m = int(z.Magnitude/3.)
                    g2Bmp.SetPixel(i, j, System.Drawing.Color.FromArgb(m,m,m)))
        g2Bmp.Save("RLg2.png", System.Drawing.Imaging.ImageFormat.Png)

        printfn "cps.[255,6] = %A" cps.[255,6]
        printfn "cps.[(min)] = %A" cps.[maxx,maxy]
        *)
        
        let app = new Application()
        app.Run(new MyWindow(bmps, diffs)) |> ignore

            
(*
        let getGrey(bmp:System.Drawing.Bitmap, i, j) = 
            // center image in the NxN box
            let dx = (N - bmp.Width) / 2
            let x = i - dx
            let dy = (N - bmp.Height) / 2
            let y = j - dy
            if x >= 0 && x < bmp.Width && y >= 0 && y < bmp.Height then
                // get pixel grey color
                let c = bmp.GetPixel(x,y)
                let g = float(int c.R + int c.G + int c.B)
                //printfn "%A" g
                // apply Hamming window for smoothing
                g * w.[i,j]
            else
                // 0. // 200. * w.[i,j]
                // mirror at edges
                let nx = if x < 0 then -x elif x >= bmp.Width then bmp.Width-x else x
                let ny = if y < 0 then -y elif y >= bmp.Height then bmp.Height-y else y
                if nx >= 0 && nx < bmp.Width && ny >= 0 && ny < bmp.Height then
                    // get pixel grey color
                    let c = bmp.GetPixel(nx,ny)
                    let g = float(int c.R + int c.G + int c.B)
                    // apply Hamming window for smoothing
                    g * w.[i,j]
                else
                    0.

        let arr1 = Array2D.init N N (fun i j -> Complex(getGrey(im1, i, j), 0.))
        let arr2 = Array2D.init N N (fun i j -> Complex(getGrey(im2, i, j), 0.))
        let cps = Fourier.crossPowerSpectrum(arr1, arr2)
        // map magnitude
        let cps = cps |> Array2D.map (fun z -> z.Magnitude)
        // kludge it (get rid of artificial spike at 0,0)
        cps.[0,0] <- 0.
        // kludge it (use window to make less likely to see spikes in corners)
        let cps = cps |> Array2D.mapi (fun i j z -> w.[i,j] * z)
        // make it less intense (for plotting)
        let cps = cps |> Array2D.map (fun z -> System.Math.Log10 (1.00001 + z))
        let mutable maxx,maxy = 0,0
        cps |> Array2D.iteri (fun i j z -> 
                    if z > cps.[maxx,maxy] then 
                        maxx <- i
                        maxy <- j)
        printfn "RL: %d %d" maxx maxy
        //printfn "%A" (cps |> Array2D.map (fun z -> z.Magnitude |> int))
        let maxMag = cps.[maxx,maxy]
        let cpsBmp = new System.Drawing.Bitmap(N, N)
        cps |> Array2D.iteri (fun i j z -> 
                    let m = int(255.*z/maxMag) 
                    cpsBmp.SetPixel(i, j, System.Drawing.Color.FromArgb(m,m,m)))
        cpsBmp.Save("RLcps.png", System.Drawing.Imaging.ImageFormat.Png)

        let app = new Application()
        app.Run(new StitchView.MyWindow()) |> ignore
*)
    0
