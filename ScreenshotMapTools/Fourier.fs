module Fourier

open System.Numerics

let dft(a:Complex[]) =
    let n = a.Length
    let r = Array.zeroCreate n
    for k = 0 to n-1 do
        let mutable real = 0.
        let mutable imag = 0.
        for t = 0 to n-1 do
            let angle = 2. * System.Math.PI * float t * float k / float n
            real <- real + a.[t].Real * System.Math.Cos(angle) + a.[t].Imaginary * System.Math.Sin(angle)
            imag <- imag + a.[t].Real * System.Math.Sin(angle) + a.[t].Imaginary * System.Math.Cos(angle)
        r.[k] <- Complex(real, imag)
    r

let revbits(x, w) =
    let mutable r,x = 0,x
    for i = 0 to w-1 do
        r <- (r <<< 1) ||| (x &&& 1)
        x <- x >>> 1
    r

let fft(a:Complex[], invert) =
    let n = a.Length
    let mutable m,tmp = 1, 2
    while tmp < n && m < 16 do
        m <- m + 1
        tmp <- tmp * 2
    if tmp <> n then
        failwith "array length must be power of 2 in range 2..32768"

    let co = 2. * System.Math.PI / float n * (if invert then 1. else -1.)
    let table = Array.init (n/2) (fun i -> Complex.FromPolarCoordinates(1., float i * co))

    for i = 0 to n-1 do
        let j = revbits(i, m)
        if j>i then
            let t = a.[i]
            a.[i] <- a.[j]
            a.[j] <- t

    let mutable size = 2
    while size <= n do
        let h = size / 2
        let step = n / size
        for i in [0 .. size .. n-1] do
            let mutable j,k = i,0
            while j < i+h do
                let t = a.[j+h] * table.[k]
                a.[j+h] <- a.[j] - t
                a.[j] <- a.[j] + t
                j <- j + 1
                k <- k + step
        size <- size * 2

let fft2(a:Complex[,], invert) =
    let n1 = a.GetLength(0)
    let n2 = a.GetLength(1)

    let r = Array2D.zeroCreate n1 n2

    for i = 0 to n1-1 do
        let col = Array.init n2 (fun j -> a.[i,j])
        fft(col, invert)
        for j = 0 to n2-1 do
            r.[i,j] <- col.[j]

    for j = 0 to n2-1 do
        let row = Array.init n1 (fun i -> r.[i,j])
        fft(row, invert)
        for i = 0 to n1-1 do
            r.[i,j] <- row.[i]

    r

let ensureSameDimensions(a:_[,], b:_[,]) = 
    if a.GetLength(0) <> b.GetLength(0) || a.GetLength(1) <> b.GetLength(1) then
        failwith "arrays must have same dimensions"

let crossPowerSpectrum(a:Complex[,], b:Complex[,]) =
    ensureSameDimensions(a,b)
    let a = fft2(a, false)
    let b = fft2(b, false)
    let r = Array2D.init (a.GetLength(0)) (a.GetLength(1)) (fun i j -> 
        let conj = Complex.Conjugate(b.[i,j])
        let z = Complex.Multiply(a.[i,j], conj)
        Complex.Divide(z, Complex(z.Magnitude, 0.)))
    fft2(r, true)

let hamming(m) = Array.init m (fun n -> 0.53836 - 0.46164 * System.Math.Cos(2.*System.Math.PI*float n/float(m-1)))

let outer(a:float[], b:float[]) = Array2D.init a.Length b.Length (fun i j -> a.[i] * b.[j])

let hamming2(m) =
    let h = hamming(m)
    outer(h, h) |> Array2D.map (fun x -> sqrt x)
    
////////////////////////////////////////////////////////////////////////

let get(a:_[,], i, j, zero) = if i>=0 && i<a.GetLength(0) && j>=0 && j<a.GetLength(1) then a.[i,j] else zero

// error from weighted sum of squared differences, using brute computation
let naiveEwssd(a:Complex[,], b:Complex[,], wa:int[,], wb:int[,]) = 
    ensureSameDimensions(a,b)
    let COL = a.GetLength(0)
    let ROW = a.GetLength(1)
    Array2D.init COL ROW (fun i j ->
//        if j = ROW-1 then 
//            printfn "%d of %d" i COL
        let mutable r = Complex.Zero
        for x = 0 to COL-1 do
            for y = 0 to ROW-1 do
                let w = wa.[x,y] * wb.[(x+i)%COL, (y+j)%ROW]
                //let w = wa.[x,y] * get(wb, x+i, y+j, 0)
                let diff = b.[(x+i)%COL, (y+j)%ROW] - a.[x,y]
                //let diff = get(b, x+i, y+j, Complex.Zero) - a.[x,y]
                let term = Complex.Multiply(Complex(float w, 0.), Complex.Multiply(diff, diff))
                r <- r + term
        r
        )

////////////////////////////////////////////////////////////////////////

let multiplyElementwise(a:Complex[,], b:Complex[,]) =
    ensureSameDimensions(a,b)
    Array2D.init (a.GetLength(0)) (a.GetLength(1)) (fun i j -> a.[i,j] * b.[i,j])

let (<*>) a b = multiplyElementwise(a, b)

let addElementwise(a:Complex[,], b:Complex[,]) =
    ensureSameDimensions(a,b)
    Array2D.init (a.GetLength(0)) (a.GetLength(1)) (fun i j -> a.[i,j] + b.[i,j])

let (<+>) a b = addElementwise(a, b)

let conjugate(a:Complex[,]) =
    Array2D.init (a.GetLength(0)) (a.GetLength(1)) (fun i j -> Complex(a.[i,j].Real, - a.[i,j].Imaginary))

// error from weighted sum of squared differences, using fourier computation
// weights are 1 where images overlap and 0 otherwise
// this method biases small overlaps when comparing total errors, so divide by pixel area overlap to compare different dx/dy
let ewssd(a:Complex[,], b:Complex[,], wa:int[,], wb:int[,]) = 
    let wa = wa |> Array2D.map (fun x -> Complex(float x,0.))
    let wb = wb |> Array2D.map (fun x -> Complex(float x,0.))

    // https://courses.cs.washington.edu/courses/cse576/05sp/papers/MSR-TR-2004-92.pdf
    // equation (66)
    let w0 = fft2(wa, false)
    let w1 = fft2(wb, false)
    let i0 = fft2(wa<*>a, false)
    let i1 = fft2(wb<*>b, false)
    let s0 = fft2(wa<*>(a<*>a), false)
    let s1 = fft2(wb<*>(b<*>b), false)

    let fewssd = (w0<*>conjugate(s1)) <+> (s0<*>conjugate(w1)) <+> (i0<*>conjugate(i1) |> Array2D.map (fun z -> Complex(-2. * z.Real, -2. * z.Imaginary)))

    let scale = a.GetLength(0) * a.GetLength(1) // answer seems to get scaled by this much, fix it
    fft2(fewssd, true) |> Array2D.map (fun z -> Complex(z.Real / float scale, z.Imaginary / float scale))
