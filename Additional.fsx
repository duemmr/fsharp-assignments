open System

// 1
let squareList (nums: int list) : int list =
    nums |> List.map (fun x -> x * x)

let input = [3; 5; 7; 9]
let result = squareList input
printfn "%A" result

// 2
let filterEvens (nums: int list) : int list =
    nums |> List.filter (fun x -> x % 2 = 0)

let input = [1; 2; 3; 4]
let result = filterEvens input
printfn "%A" result 

// 3
let sumPositives (nums: int list) : int =
    nums
    |> List.filter (fun x -> x > 0)
    |> List.sum

let input = [-2; -3; 1; 0; 3; -13; 4]
let result = sumPositives input
printfn "%d" result

// 4
let capitalize (names: string list) : string list =
    names |> List.map (fun name ->
        if String.IsNullOrWhiteSpace(name) then name
        else name.Substring(0, 1).ToUpper() + name.Substring(1).ToLower())

let input = ["joseph"; "jonNy"; "John"; ""]
let result = capitalize input
printfn "%A" result

// 5
let filterByLength (strings: string list) (n: int) : string list =
    strings |> List.filter (fun s -> s.Length > n)

let input = ["world"; "long"; "abc"; "c"; "test"; ""]
let result = filterByLength input 4
printfn "%A" result

// 6
let countDivisibleBy (nums: int list) (divisor: int) : int =
    nums |> List.filter (fun x -> x % divisor = 0) |> List.length

let input = [5; 30; 14; 3; 4; 6; 66; 16]
let result = countDivisibleBy input 4
printfn "%d" result

// 7
let findIndicesOf (list: 'a list) (elem: 'a) : int list =
    list
    |> List.mapi (fun idx x -> if x = elem then Some idx else None)
    |> List.choose id

let input = ['a'; 'a'; 'b'; 'c']
let result = findIndicesOf input 'a'
printfn "%A" result

// 8
let concatLongerThan (strings: string list) (n: int) : string =
    strings
    |> List.filter (fun s -> s.Length > n)
    |> String.concat ""

let input = ["fsharp"; "is"; "a"; "functional"; "lang"]
let result = concatLongerThan input 5
printfn "%s" result 

// 9
let findMaxTuple (tuples: ('id * int) list) : ('id * int) =
    tuples |> List.maxBy snd

let input = [("a", 10); ("b", 25); ("c", 17)]
let result = findMaxTuple input
printfn "%A" result

// 10
let countOccurence l =
    l
    |> List.groupBy id
    |> List.map (fun (key, group) -> (key, List.length group))

let input = ["b"; "b"; "c"; "a"]
let result = countOccurence input
printfn "%A" result

// 11
type Lights =
    | Red
    | Yellow
    | Green

let nextState light =
    match light with
    | Red -> Green
    | Green -> Yellow
    | Yellow -> Red

let cur = Red
let nex = nextState cur
printfn "%A" nex

// 12
type ArithmeticOperation =
    | Add
    | Subtract
    | Multiply
    | Divide

let calc op x y =
    match op with
    | Subtract -> x - y
    | Multiply -> x * y
    | Add -> x + y 
    | Divide -> 
        if y = 0.0 then failwith "Zero divison"
        else x / y

let result = calc Subtract 10.0 2.0
printfn "%f" result

// 13
type Shape =
    | Rectangle of width: float * height: float
    | Square of side: float
    | Circle of radius: float
    | Triangle of baseL: float * height: float

let area shape =
    match shape with
    | Rectangle (w, h) -> w * h
    | Square s -> s * s
    | Circle r -> System.Math.PI * r * r
    | Triangle (b, h) -> 0.5 * b * h

let sh = Rectangle (4.0, 5.0)
let result = area sh
printfn "The area is: %f" result

// 14
type TemperatureScale =
    | Celsius
    | Fahrenheit

let convTemp value fromS toS =
    match (fromS, toS) with
    | (Celsius, Fahrenheit) -> (value * 9.0 / 5.0) + 32.0
    | (Fahrenheit, Celsius) -> (value - 32.0) * 5.0 / 9.0
    | _ -> value 

let tempF = convTemp 120.0 Celsius Fahrenheit
printfn "Temp in F is: %f" tempF

// 20
let rec isPalindrome (str: string) =
    let len = String.length str
    if len <= 1 then
        true
    else
        if str.[0] = str.[len - 1] then
            isPalindrome (str.Substring(1, len - 2))
        else
            false

printfn "%b" (isPalindrome "car")
printfn "%b" (isPalindrome "ji")
printfn "%b" (isPalindrome "hello")
printfn "%b" (isPalindrome "madam")

let rec merge left right =
    match (left, right) with
    | [], r -> r
    | l, [] -> l
    | h1::t1, h2::t2 ->
        if h1 <= h2 then
            h1 :: merge t1 right
        else
            h2 :: merge left t2

// 18
let rec mergeSort lst =
    match lst with
    | [] -> []
    | [_] -> lst 
    | _ ->
        let mid = List.length lst / 2
        let left = lst |> List.take mid
        let right = lst |> List.skip mid
        merge (mergeSort left) (mergeSort right)

let list = [3; 4; 1; 2; 10; 13]
let sorted = mergeSort list
printfn "%A" sorted

// 17
let rec binarySearch (arr: 'a array) (target: 'a) (left: int) (right: int) : int =
    if left > right then
        -1
    else
        let mid = left + (right - left) / 2
        if arr.[mid] = target then
            mid
        elif arr.[mid] > target then
            binarySearch arr target left (mid - 1)
        else
            binarySearch arr target (mid + 1) right


let sortedArray = [|1; 5; 4; 2; 3; 10; 15|]
let target = 3
let index = binarySearch sortedArray target 0 (Array.length sortedArray - 1)
printfn "Index of %d is: %d" target index

// 16
let rec fibonacci n =
    if n <= 0 then 0
    elif n = 1 then 1
    else fibonacci (n - 1) + fibonacci (n - 2)

printfn "%d" (fibonacci 13)

// 19
type BinaryTree<'T> =
    | Empty
    | Node of 'T * BinaryTree<'T> * BinaryTree<'T>

let rec depth tree =
    match tree with
    | Empty -> 0
    | Node (_, left, right) ->
        1 + max (depth left) (depth right)

let tree =
    Node(42,
         Node(43, Empty, Empty),
         Node(44,
              Node(45, Empty, Empty),
              Node(46, Empty, Empty)))

printfn "Depth is %d" (depth tree)
