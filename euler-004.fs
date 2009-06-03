#light

let isSymmetric str = 
    let rec isSymmetric_aux (str:string) s e =
        if e - s > 0
        then
            if (str.[s] <> str.[e])
            then false
            else isSymmetric_aux str (s+1) (e-1)
        else true   
    isSymmetric_aux str 0 ((String.length str) - 1)    
   
let findPalindromes (products: #seq <int * int>) = 
    products |> Seq.map (fun (a,b) -> 
        let p = a * b    
        let ps = p.ToString()
        ((a,b), p, isSymmetric ps)) |> Seq.filter (fun ((a,b), p, sym) -> sym) |> Seq.map (fun ((a,b), p, sym) -> ((a,b), p))
    
let productPairs max min = { for i in max .. -1 .. min do
                             for j in i .. -1 .. min do
                             yield (i,j) }

let productPairs3 = productPairs 999 100

let ((a,b), p) = productPairs3 |> findPalindromes |> Seq.orderBy (fun ((a,b),p) -> -p) |> Seq.first (fun v -> Some (v)) |> Option.get

printfn "Largest palindrome made from the product of two 3-digit numbers: %A x %A = %A" a b p