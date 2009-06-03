#light 

// Greater Common Divisor 
let rec gcd a b = 
    if b = 0I
    then a
    else gcd b (a % b) 

// Least Common Multiple 
let lcm a b = a * b / (gcd a b) 

printfn "Smallest number that is evenly divisible by all of the numbers from 1 to 20 is %A" (Seq.fold1 lcm [1I .. 20I])
