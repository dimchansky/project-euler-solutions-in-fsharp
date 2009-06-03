#light

let iexp_aux x n f =
   let nbin = n |> Seq.unfold (fun n -> if n=0 then None else Some(n % 2, n / 2))
   let (rez, p) = nbin |> Seq.fold (fun (y,p) i -> if i = 1 then (f (y*p), f (p*p)) else (f (y), f (p*p))) (1I, x)
   rez
   
let iexp x n = iexp_aux x n (fun v -> v)

let mod10_10 v = v % 10000000000I

let answer = 
    let twoPow = iexp_aux 2I 7830457 mod10_10
    twoPow * 28433I + 1I |> mod10_10
    
print_any answer