module ViagioCodeOff.Problem1

type Trough = int[]

type Cell = Dirt | Unknown | Water | Air

module Cell =
    let print c =
        match c with
        | Dirt -> printf "#"
        | Air -> printf " "
        | Water -> printf "."
        | Unknown -> printf "!"
        
        
type WalkDirection = Left | Right

let buildMap (trough: Trough) =
    let depth = trough |> Seq.max
    
    // create an array of depth and width and fill with Unknown
    let map = Array2D.create depth trough.Length Unknown
    
    // run through each cell and figure out if it is dirt
    for x in 0 .. trough.Length - 1 do
        for y = depth-1 downto 0 do
            let height = trough.[x]
            
            // tricky because y is inverted (positive is down)
            if depth - y - 1 < height then
                map.[y, x] <- Dirt
            
            

    // determines if a position is contained by dirt or water in a given direction
    let rec isContained (x, y) dir =
        if x > trough.Length - 1 || x < 0 then
            // fell of the left or right edge, so the spot is not contained
            false
        else
            let current = map.[y, x]
            
            match current with
                | Dirt -> true // contained by dirt!
                | Water -> true // contained by water we discovered earlier, just a short-circuit
                | Air -> false // fill into air, so its going to leak off the side
                | Unknown ->
                    // keep waking in the desired direction
                    let x0 = if dir = Left then x - 1 else x + 1
                    isContained (x0, y) dir
                
    let fill (x, y) =
        if map.[y, x] = Unknown then
            let containedLeft = isContained (x - 1, y) Left
            let containedRight = isContained (x + 1, y) Right
            
            if containedLeft && containedRight then
                map.[y, x] <- Water
            else
                map.[y, x] <- Air
                
           
    // fill each cell
    for y in 0..depth-1 do
        for x in 0 .. trough.Length - 1 do
            fill (x, y)
            
    // print the map and count water while we're at it
    let mutable water = 0
    for d in 0..depth-1 do
        for x in 0 .. trough.Length - 1 do
            Cell.print map.[d, x]
            if map.[d, x] = Water then
                water <- water + 1
        printfn ""
        
    printfn $"Water: %d{water}"
        
    

let execute =
    buildMap [|1;2;3;2;1;3;2;2;3;1;3;2;1|]
