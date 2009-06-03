#light

let answer =
    let t = new System.Collections.Generic.Dictionary<int * int, bigint>() 
    let rec routes (x,y) acc =
        if t.ContainsKey((x,y))
        then acc * t.[(x,y)]
        else
            let res = 
                if x=0 || y=0
                then acc
                else
                    let right = routes(x-1,y) 1I
                    let down = routes(x,y-1) 1I
                    right + down
            t.Add((x,y), res)
            res            
    routes (20,20) 0I
    
printfn "routes=%A" answer 