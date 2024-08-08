module GenericMetadata

(*

maybe instead of true metadata I should just have full text search?
can decide if maybe want #key as convention or official thingy for true metadata, hm
also means I need some kind of text repository with like
    (location, text)
pairs where i can search all text for #key and report all locations containing, and also replace a text by location in the db
and location is a (zone,x,y) triple I guess
and I guess every time i edit a note and insert it, it should parse it then for all the keys and keep a table of <key,location[]>
hm, and then i guess basically need a ui which allows
 - selecting any key and highlighting all map locations (and zones? hm) with that key (show count? list them? clickable list takes you there?)
 - or not currently doing any highlighting
and maybe optionally full text search for other stuff, though that's a stretch goal
key list could maybe always appear somewhere, tall list with slider bar?
conventionally perhaps I use #greenkey and #Zgreenkey to mean 'needed' and 'done' (unlocked) maybe to track things changing over time?

*)

let AllHashtags(s:string) =
    if s=null then 
        []
    else
        let mutable keys = []
        for i = 0 to s.Length-2 do
            if s.Chars(i)='#' then
                let mutable j = i+1
                while j < s.Length && System.Char.IsLetterOrDigit(s.Chars(j)) do
                    j <- j + 1
                if j>i+1 then
                    let key = s.Substring(i+1, j-i-1)
                    keys <- key :: keys
        keys

type Location(zone,x,y) =
    do
        if zone > 99 || x > 99 || y > 99 || zone < 0 || x < 0 || y < 0 then
            failwith "bad Location data"
    member this.Zone = zone
    member this.X = x
    member this.Y = y
    override this.GetHashCode() = zone*10000 + x*100 + y
    override this.Equals(o) =
        match o with
        | :? Location as l -> l.Zone = this.Zone && l.X = this.X && l.Y = this.Y
        | _ -> false

type MetadataStore() =
    let data = System.Collections.Generic.Dictionary<string,System.Collections.Generic.HashSet<Location> >()
    member this.ChangeNote(loc, oldNote, newNote) =
        let oldKeys = AllHashtags(oldNote)
        let newKeys = AllHashtags(newNote)
        for k in oldKeys do
            match data.TryGetValue(k) with
            | false, _ -> ()
            | true, hs -> hs.Remove(loc) |> ignore
        for k in newKeys do
            match data.TryGetValue(k) with
            | false, _ -> 
                let hs = System.Collections.Generic.HashSet()
                hs.Add(loc) |> ignore
                data.Add(k, hs)
            | true, hs -> 
                hs.Add(loc) |> ignore
    member this.LocationsForKey(k) =
        match data.TryGetValue(k) with
        | false, _ -> System.Collections.Generic.HashSet()
        | true, hs -> System.Collections.Generic.HashSet(hs)
    member this.AllKeys() = [| for z in data.Keys do if data.[z].Count > 0 then yield z |]

////////////////////////////////////////////////////////
// linkages
//
// (30,40) in text means links to 30,40 in this zone
// (zone02) in text means links to same coords but zone02
// (zone02,30,40) in text means links to that location

let compiled = System.Text.RegularExpressions.RegexOptions.Compiled
let coordsRegex = new System.Text.RegularExpressions.Regex("\((\d\d),(\d\d)\)", compiled)   // x,y
let fullRegex = new System.Text.RegularExpressions.Regex("\(zone(\d\d),(\d\d),(\d\d)\)", compiled)   // z,x,y
let zoneRegex = new System.Text.RegularExpressions.Regex("\(zone(\d\d)\)", compiled)   // z

let FindAllLinkages(note:string, curZone, curX, curY) =
    let locs = ResizeArray()
    for m in coordsRegex.Matches(note) do
        let x,y = int m.Groups.[1].Value, int m.Groups.[2].Value
        locs.Add(new Location(curZone, x, y))
    for m in fullRegex.Matches(note) do
        let z,x,y = int m.Groups.[1].Value, int m.Groups.[2].Value, int m.Groups.[3].Value
        locs.Add(new Location(z, x, y))
    for m in zoneRegex.Matches(note) do
        let z = int m.Groups.[1].Value
        locs.Add(new Location(z, curX, curY))
    locs

            