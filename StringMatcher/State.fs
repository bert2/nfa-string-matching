namespace StringMatcher

// Suppress "'State' has an explicit implementation of 'Object.Equals'. Consider 
// implementing a matching override for 'Object.GetHashCode()'"
#nowarn "0346"

type Letter = 
    | Letter of char 
    | Range of char * char 
    | Any

[<CustomEquality; CustomComparison>]
type State = 
    | State of Letter * State
    | Split of State * State
    | Final
    
    override x.Equals y = System.Object.ReferenceEquals (x, y)
    
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? State as y -> compare (x.GetHashCode ()) (y.GetHashCode ())
            | _ -> invalidArg "yobj" "cannot compare value of different types"
