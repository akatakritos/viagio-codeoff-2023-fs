module ViagioCodeOff.Problem2

/// enum: Represents the face of a card, can hold one of the picture cards or one of numbers
type Face =
    | Number of int // F# enums are cool because they can also hold different data in each case
    | Jack
    | Queen
    | King
    | Ace
    
/// Create some methods that operate on Face objects
module Face =
    
    /// Parse a char into a Face
    let fromChar c =
        match c with
        | '2' -> Number 2
        | '3' -> Number 3
        | '4' -> Number 4
        | '5' -> Number 5
        | '6' -> Number 6
        | '7' -> Number 7
        | '8' -> Number 8
        | '9' -> Number 9
        | 'T' -> Number 10
        | 'J' -> Jack
        | 'Q' -> Queen
        | 'K' -> King
        | 'A' -> Ace
        | _ -> failwith "Invalid face" // this throws an exception
    
/// enum: Represents the suit of a card
type Suit = 
    | Spades
    | Hearts
    | Diamonds
    | Clubs
    
module Suit =
    
    /// Parse a char into a Suit
    let fromChar c =
        match c with
        | 'S' -> Spades
        | 'H' -> Hearts
        | 'D' -> Diamonds
        | 'C' -> Clubs
        | _ -> failwith "Invalid suit"
    
/// We'll define a Card as the tuple (pair) of Face and Suit
type Card = Face * Suit


module Card =
    
    /// Parse a string into a sequence of cards
    let parse (s: string) =
        let mutable i = 0
        
        // seq is like IEnumerable in C#, lets us yield return Card values as it iterates through the string
        seq {
            while i < s.Length do
                let face = Face.fromChar s.[i]
                let suit = Suit.fromChar s.[i + 1]
                
                yield face, suit
                i <- i + 2
        }
        
    /// A function to check if a card has a given Face
    let isFace (face: Face) ((f, _): Card) =
        f = face
        
        
    /// Alias the "fst" function that returns the first element of a tuple to a more meaningful name: extracts the Face from a card
    let face = fst
    
    /// Alias the "snd" function that returns the second element of a tuple to a more meaningful name: extracts the Suit from a card
    let suit = snd
        
        
/// <summary>A function to count the number of groups in a sequence of groups that have a length matching a predicate</summary>
/// <param name="predicate">Checks the length of a grouped element</param>
/// <param name="groups">The group to process</param>
let countGroupsWithLength (predicate: int -> bool) groups =
    Seq.filter (fun (_, g) -> g |> Seq.length |> predicate) groups |> Seq.length

let execute =
    // let cards = Card.parse "3S3H2HQHAD8DACQD4SAS5H4HKC" |> Seq.toList
    // let cards = Card.parse "6S7HQS2S5D4HJCAD3C7DQHJH3S" |> Seq.toList
    // let cards = Card.parse "2DJC5D8H3DAHJD2SJH8S4D2C4C" |> Seq.toList
    let cards = Card.parse "AHAC2HKDQHQSADTCKSTH8H6C3D" |> Seq.toList
    
    // print them out for debugging
    //cards |> Seq.iter (fun (face, suit) -> printfn "%A %A" face suit)
    
    // find the count of cards matching each desired face type
    let aces = cards
               |> Seq.filter (Card.isFace Ace)
               |> Seq.length
               
    let kings = cards |> Seq.filter (Card.isFace King) |> Seq.length
    let queens = cards |> Seq.filter (Card.isFace Queen) |> Seq.length
    let jacks = cards |> Seq.filter (Card.isFace Jack) |> Seq.length
    
    let groupedBySuit = cards |> Seq.groupBy Card.suit
    
    let singletons = groupedBySuit |> countGroupsWithLength (fun n -> n = 1)
    let voids = groupedBySuit |> countGroupsWithLength (fun n -> n = 0)
    let fives = groupedBySuit |> countGroupsWithLength (fun n -> n = 5)
    let sixPlus = groupedBySuit |> countGroupsWithLength (fun n -> n >= 6)
    
    // compute score according to rules
    let score = (aces * 4) + (kings * 3) + (queens * 2) + jacks + singletons + (voids * 2) + fives + (sixPlus * 2)
    printfn $"Score = %d{score}"
        
