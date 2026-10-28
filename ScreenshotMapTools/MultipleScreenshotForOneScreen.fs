module MultipleScreenshotForOneScreen

open System.Drawing

module FancyTweak =
    // Gemini
    let maxL = 0.5
    /// Takes a perceptual Lightness value (0.0 to 1.0) and applies an asymmetric 
    /// contrast boost (left-skewed bell curve) targeting a window between minL and maxL.
    let skewShadowSpline (minL:float) (l: float) =
        if l <= minL || l >= maxL then
            l
        else
            // 1. Map L into a normalized 0.0 to 1.0 window progress parameter
            let t = (l - minL) / (maxL - minL)
            // 2. Asymmetric bell curve profile: peaks sharply at t = 0.25 (L = 0.2)
            // f(t) = t * (1-t)^3
            let rawProfile = t * System.Math.Pow(1.0 - t, 3.0)
            // The maximum value of t*(1-t)^3 occurs at t=1/4, which is 0.25 * (0.75)^3 = 0.10546875.
            // Multiplying by (1.0 / 0.10546875) ≈ 9.4814815 normalizes the peak value to exactly 1.0.
            let normalizedBell = rawProfile * 9.4814815
            // 3. Define the maximum brightness addition at the peak (L = 0.2)
            let maxGain = (maxL - minL) / 3.0
            let currentGain = maxGain * normalizedBell
            l + currentGain

    let adjustMapContrast (minL:float) (c: Color) =
        let l, c_chroma, h = ColorUtils.OklchHelper.ColorToOklch(c)
        // Adjust lightness to boost darker colors
        let newL = skewShadowSpline minL l
        // Scale Chroma proportionally to the change in Lightness
        // This keeps the "vibrancy ratio" stable so colors don't wash out into gray
        let newC = 
            if l > 0.01 then 
                c_chroma * (newL / l)
            else 
                c_chroma
        // Reconstruct the color, keeping the Hue angle completely locked
        ColorUtils.OklchHelper.ColorFromOklch(newL, newC, h)

let tweak(c) = FancyTweak.adjustMapContrast GameSpecific.TheChosenGame.MinLightnessForColorBoosting c

let MoreLuminous(c1:Color,c2:Color) =
    if c1.GetBrightness() > c2.GetBrightness() then
        c1
    else
        c2

let ComputeUnion(bmps:ResizeArray<Bitmap>) =
    if bmps.Count = 0 then
        null
    else
        let w,h = bmps.[0].Width, bmps.[0].Height
        for bmp in bmps do
            if bmp.Width <> w || bmp.Height <> h then
                failwith "different screenshot sizes"
        let r = new Bitmap(w,h)
        let rData = r.LockBits(Rectangle(0,0,w,h), Imaging.ImageLockMode.WriteOnly, Imaging.PixelFormat.Format32bppArgb)
        let datas = Array.zeroCreate bmps.Count
        for i = 0 to bmps.Count-1 do
            datas.[i] <- bmps.[i].LockBits(Rectangle(0,0,w,h), Imaging.ImageLockMode.ReadOnly, Imaging.PixelFormat.Format32bppArgb)
        for i = 0 to w-1 do
            for j = 0 to h-1 do
                let mutable color = Color.FromArgb(0,0,0)
                for data in datas do
                    color <- MoreLuminous(color, Utils.GetColorFromLockedFormat32BppArgb(i,j,data))
                color <- tweak(color)
                Utils.SetColorFromLockedFormat32BppArgb(i,j,rData,color)
        for i = 0 to bmps.Count-1 do
            bmps.[i].UnlockBits(datas.[i])
        r.UnlockBits(rData)
        r

[<RequireQualifiedAccess>]
type RepresentativePolicy =
    | Last
    | Union

let thePolicy = RepresentativePolicy.Union  // I have not used Last in a while

let GetRepresentative(a:ResizeArray<Bitmap>) =
    if a.Count = 0 then 
        null
    else
        match thePolicy with
        | RepresentativePolicy.Last -> a.[a.Count-1]
        | RepresentativePolicy.Union -> ComputeUnion(a)

            
