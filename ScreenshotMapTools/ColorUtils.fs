module ColorUtils

open System
open System.Drawing

// https://stackoverflow.com/questions/1335426/is-there-a-built-in-c-net-system-api-for-hsv-to-rgb
let ColorToHSV(color:Color) =
    let max = Math.Max(color.R, Math.Max(color.G, color.B))
    let min = Math.Min(color.R, Math.Min(color.G, color.B))
    //let hue = (float(color.GetHue()) + 180.) % 360.
    let hue = float(color.GetHue())
    let saturation = if (max = 0uy) then 0. else 1. - (1. * float min / float max)
    let value = float max / 255.
    hue, saturation, value

let ColorFromHSV(hue, saturation, value) =
    let hi = Convert.ToInt32(Math.Floor(hue / 60.)) % 6
    let f = hue / 60. - Math.Floor(hue / 60.)

    let value = value * 255.
    let v = Convert.ToInt32(value);
    let p = Convert.ToInt32(value * (1. - saturation))
    let q = Convert.ToInt32(value * (1. - f * saturation))
    let t = Convert.ToInt32(value * (1. - (1. - f) * saturation))

    if (hi = 0) then
        Color.FromArgb(255, v, t, p)
    elif (hi = 1) then
        Color.FromArgb(255, q, v, p)
    elif (hi = 2) then
        Color.FromArgb(255, p, v, t)
    elif (hi = 3) then
        Color.FromArgb(255, p, q, v)
    elif (hi = 4) then
        Color.FromArgb(255, t, p, v)
    else
        Color.FromArgb(255, v, p, q)

//////////////////////////////////////////////////////////////////////

// from https://stackoverflow.com/questions/309149/generate-distinctly-different-rgb-colors-in-graphs
let DistinctColors = [|
    System.Windows.Media.Color.FromRgb(0x00uy, 0x00uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xFFuy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x00uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x00uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x01uy, 0xFFuy, 0xFEuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xA6uy, 0xFEuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xDBuy, 0x66uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x64uy, 0x01uy)
    System.Windows.Media.Color.FromRgb(0x01uy, 0x00uy, 0x67uy)
    System.Windows.Media.Color.FromRgb(0x95uy, 0x00uy, 0x3Auy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x7Duy, 0xB5uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x00uy, 0xF6uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xEEuy, 0xE8uy)
    System.Windows.Media.Color.FromRgb(0x77uy, 0x4Duy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x90uy, 0xFBuy, 0x92uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x76uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0xD5uy, 0xFFuy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x93uy, 0x7Euy)
    System.Windows.Media.Color.FromRgb(0x6Auy, 0x82uy, 0x6Cuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x02uy, 0x9Duy)
    System.Windows.Media.Color.FromRgb(0xFEuy, 0x89uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x7Auy, 0x47uy, 0x82uy)
    System.Windows.Media.Color.FromRgb(0x7Euy, 0x2Duy, 0xD2uy)
    System.Windows.Media.Color.FromRgb(0x85uy, 0xA9uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x00uy, 0x56uy)
    System.Windows.Media.Color.FromRgb(0xA4uy, 0x24uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xAEuy, 0x7Euy)
    System.Windows.Media.Color.FromRgb(0x68uy, 0x3Duy, 0x3Buy)
    System.Windows.Media.Color.FromRgb(0xBDuy, 0xC6uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0x26uy, 0x34uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0xBDuy, 0xD3uy, 0x93uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xB9uy, 0x17uy)
    System.Windows.Media.Color.FromRgb(0x9Euy, 0x00uy, 0x8Euy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x15uy, 0x44uy)
    System.Windows.Media.Color.FromRgb(0xC2uy, 0x8Cuy, 0x9Fuy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x74uy, 0xA3uy)
    System.Windows.Media.Color.FromRgb(0x01uy, 0xD0uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x47uy, 0x54uy)
    System.Windows.Media.Color.FromRgb(0xE5uy, 0x6Fuy, 0xFEuy)
    System.Windows.Media.Color.FromRgb(0x78uy, 0x82uy, 0x31uy)
    System.Windows.Media.Color.FromRgb(0x0Euy, 0x4Cuy, 0xA1uy)
    System.Windows.Media.Color.FromRgb(0x91uy, 0xD0uy, 0xCBuy)
    System.Windows.Media.Color.FromRgb(0xBEuy, 0x99uy, 0x70uy)
    System.Windows.Media.Color.FromRgb(0x96uy, 0x8Auy, 0xE8uy)
    System.Windows.Media.Color.FromRgb(0xBBuy, 0x88uy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x43uy, 0x00uy, 0x2Cuy)
    System.Windows.Media.Color.FromRgb(0xDEuy, 0xFFuy, 0x74uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xFFuy, 0xC6uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xE5uy, 0x02uy)
    System.Windows.Media.Color.FromRgb(0x62uy, 0x0Euy, 0x00uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x8Fuy, 0x9Cuy)
    System.Windows.Media.Color.FromRgb(0x98uy, 0xFFuy, 0x52uy)
    System.Windows.Media.Color.FromRgb(0x75uy, 0x44uy, 0xB1uy)
    System.Windows.Media.Color.FromRgb(0xB5uy, 0x00uy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0xFFuy, 0x78uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0x6Euy, 0x41uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x5Fuy, 0x39uy)
    System.Windows.Media.Color.FromRgb(0x6Buy, 0x68uy, 0x82uy)
    System.Windows.Media.Color.FromRgb(0x5Fuy, 0xADuy, 0x4Euy)
    System.Windows.Media.Color.FromRgb(0xA7uy, 0x57uy, 0x40uy)
    System.Windows.Media.Color.FromRgb(0xA5uy, 0xFFuy, 0xD2uy)
    System.Windows.Media.Color.FromRgb(0xFFuy, 0xB1uy, 0x67uy)
    System.Windows.Media.Color.FromRgb(0x00uy, 0x9Buy, 0xFFuy)
    System.Windows.Media.Color.FromRgb(0xE8uy, 0x5Euy, 0xBEuy)
    |]

//////////////////////////////////////////////////////////////////////

module OklchHelper =
    // Gemini
    let cbrt (x: float) =
        if x < 0.0 then -Math.Pow(-x, 1.0 / 3.0)
        else Math.Pow(x, 1.0 / 3.0)

    // Helper to safely clamp values between 0.0 and 1.0
    let inline private clamp (v: float) = Math.Max(0.0, Math.Min(1.0, v))

    /// Converts standard sRGB (0.0 to 1.0) to Linear RGB (removing gamma companding)
    let sRgbToLinearRgb (r: float, g: float, b: float) =
        let transform c =
            if c <= 0.04045 then c / 12.92
            else Math.Pow((c + 0.055) / 1.055, 2.4)
        (transform r, transform g, transform b)

    /// Converts Linear RGB back to standard sRGB (re-applying gamma companding)
    let linearRgbToSRgb (r: float, g: float, b: float) =
        let transform c =
            if c <= 0.0031308 then 12.92 * c
            else 1.055 * Math.Pow(c, 1.0 / 2.4) - 0.055
        (clamp (transform r), clamp (transform g), clamp (transform b))

    /// Converts standard sRGB (values 0.0 to 1.0) to Oklch
    let sRgbToOklch (sR: float, sG: float, sB: float) =
        // 1. To Linear RGB
        let r, g, b = sRgbToLinearRgb (sR, sG, sB)
        
        // 2. To LMS cone space via matrix multiplication
        let l_ = 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b
        let m_ = 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b
        let s_ = 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b
        
        // 3. Apply non-linear cube root encoding to simulate human eye response
        let l = cbrt(l_)
        let m = cbrt(m_)
        let s = cbrt(s_)
        
        // 4. To Oklab components (L, a, b)
        let oklabL = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
        let oklabA = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
        let oklabB = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
        
        // 5. To Oklch (Cylindrical form)
        let chroma = Math.Sqrt(oklabA * oklabA + oklabB * oklabB)
        
        // Calculate hue in radians, convert to degrees (0 to 360)
        let mutable hue = Math.Atan2(oklabB, oklabA) * (180.0 / Math.PI)
        if hue < 0.0 then hue <- hue + 360.0
        
        (oklabL, chroma, hue)

    /// Converts Oklch back to standard sRGB (values 0.0 to 1.0)
    let oklchToSRgb (l: float, c: float, hDeg: float) =
        // 1. Back to Cartesian Oklab
        let hRad = hDeg * (Math.PI / 180.0)
        let oklabA = c * Math.Cos(hRad)
        let oklabB = c * Math.Sin(hRad)
        
        // 2. Back to LMS cube root space via inverse matrix
        let l_cone = l + 0.3963377774 * oklabA + 0.2036494716 * oklabB
        let m_cone = l - 0.1055613458 * oklabA - 0.0630164625 * oklabB
        let s_cone = l - 0.0894841775 * oklabA - 1.2914855480 * oklabB
        
        // 3. Undo cube root encoding
        let l_ = l_cone * l_cone * l_cone
        let m_ = m_cone * m_cone * m_cone
        let s_ = s_cone * s_cone * s_cone
        
        // 4. Back to Linear RGB via inverse matrix
        let r =  4.0767416621 * l_ - 3.3077115913 * m_ + 0.2309699292 * s_
        let g = -1.2684380046 * l_ + 2.6097574011 * m_ - 0.3413193965 * s_
        let b = -0.0041960863 * l_ - 0.7034186147 * m_ + 1.7076147010 * s_
        
        // 5. Back to Gamma-corrected standard sRGB
        linearRgbToSRgb (r, g, b)

    /// Converts a byte value (0 to 255) to a normalized float (0.0 to 1.0)
    let byteToFloat (b: byte) = 
        float b / 255.0

    /// Converts a normalized float (0.0 to 1.0) back to a byte (0 to 255)
    let floatToByte (v: float) =
        // Rounding to the nearest integer matches standard graphics pipelines.
        // We clamp strictly between 0 and 255 before casting to prevent overflow exceptions.
        Math.Round(v * 255.0)
        |> max (0.0)
        |> min (255.0)
        |> byte

    let ColorToOklch(c:Color) =
        let rFloat = byteToFloat c.R
        let gFloat = byteToFloat c.G
        let bFloat = byteToFloat c.B
        sRgbToOklch(rFloat, gFloat, bFloat)

    let ColorFromOklch(l,c,h) =
        let rOutFloat, gOutFloat, bOutFloat = oklchToSRgb(l, c, h)
        let r = floatToByte rOutFloat
        let g = floatToByte gOutFloat
        let b = floatToByte bOutFloat
        Color.FromArgb(int r,int g,int b)

