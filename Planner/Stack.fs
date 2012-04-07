namespace au.id.cxd.Planner

open System

/// <summary>
/// A stack data type to assist with managing depth first iteration.
/// </summary>
module StackADT =

    type Stack<'a> =
        | Nil
        | Cons of 'a * 'a Stack

    exception StackEmpty

    let isEmpty stack =
        match stack with
            | Nil -> true
            | _ -> false

    let top stack =
        match stack with
        | Nil -> raise StackEmpty
        | Cons (a, _) -> a

    let pop stack =
        match stack with
        | Nil -> raise StackEmpty
        | Cons (a, stack') -> (a, stack')

    let push stack a =
        match stack with
        | Nil -> Cons (a, Nil)
        | Cons (a', _) -> Cons (a, stack)
    
        

        
