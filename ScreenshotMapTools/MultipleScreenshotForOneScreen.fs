﻿module MultipleScreenshotForOneScreen

open System.Drawing

let MoreLuminous(c1:Color,c2:Color) =
    if c1.GetBrightness() > c2.GetBrightness() then
        c1
    else
        c2

let ComputeUnion(bmps:ResizeArray<Bitmap>) =
    if bmps.Count = 0 then
        null
    elif bmps.Count = 1 then
        bmps.[0]
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
                Utils.SetColorFromLockedFormat32BppArgb(i,j,rData,color)
        for i = 0 to bmps.Count-1 do
            bmps.[i].UnlockBits(datas.[i])
        r.UnlockBits(rData)
        r

[<RequireQualifiedAccess>]
type RepresentativePolicy =
    | Last
    | Union

let thePolicy = RepresentativePolicy.Union   // TODO when toggling it, invalidate all the onscreen images

let GetRepresentative(a:ResizeArray<Bitmap>) =
    if a.Count = 0 then 
        null
    else
        match thePolicy with
        | RepresentativePolicy.Last -> a.[a.Count-1]
        | RepresentativePolicy.Union -> ComputeUnion(a)

            
