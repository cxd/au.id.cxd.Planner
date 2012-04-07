namespace au.id.cxd.Planner

open System


module QueueADT =

    type Queue<'a> =
        { f: 'a List; r: 'a List }
       
    exception QueueEmpty

    let isEmpty queue =
        match queue with
            { f = []; r = [] } -> true
            | _ -> false

    let peek queue =
        match queue with
        | { f = []; r = [] } -> raise QueueEmpty
        | { f = x::_; r = _ } -> x

    
    
