namespace au.id.cxd.Planner


open System


/// <summary>
/// A tree search algorithm used to perform
/// searching within the state space of the planner.
/// A series of other algorithms may be written
/// to improve the search performance in
/// large state spaces.
/// This is based on the tree search in AIMA ch3
/// </summary>
module TreeSearch =

    type SearchAction<'a,'b,'c> = 'a * 'b * 'c

    // test if fringe is an empty sequence
    let empty fringe = (Seq.length fringe) = 0
    // remove the first item from the sequence
    let removeFirst n = (Seq.take 1 n) |> Seq.nth 0
    // expand the current node based on the problem
    let expand depth (prevAction, prevCost, prevState) problem successorFn =
        successorFn problem (prevAction, prevCost, prevState)
        |> Seq.map (fun (nextAction, nextCost, nextState) ->
                    (depth + 1, (nextAction, nextCost + prevCost, nextState)) )
    

    /// <summary>
    /// A tree search algorithm
    /// This algorithm is equivalent to the breadth first search
    /// as it expands the successor states it adds them to the back of
    /// the fringe to explore (FIFO) order.
    /// <param name="problem">The problem to solve</param>
    /// <param name="initialState">the initial action and state represented as a tuple
    /// of type <A, C, S>
    /// where A = the action, C = the cost (must support (+)) and S = current state
    /// </param>
    /// <param name="successorFn">A successor function
    /// that is used to expand the next search space of the form
    /// ('problem -> ('A * 'C * 'S) -> #seq<'A * 'C  * 'S>) 
    /// </param>
    /// <param name="goalTestFn">A goal test function
    /// that is used to test if the current node in the search reaches the goal
    /// It has the form:
    /// ('problem -> ('A * 'C * 'S) -> bool)
    /// Test if the problem is satisfied by the solution in the tuple ('A * 'C * 'S)
    /// </param>
    /// <returns>
    /// An Option type. If no solution then None
    /// If a solution is found then tuple Some (depth, ('A * 'C * 'S) )
    /// Where depth is the depth at which the solution was found in the state space.
    /// </returns>
    /// <summary>
    let treeSearch problem (initialState:SearchAction<'a,'b,'c>) successorFn goalTestFn =
        // main body of the search agorithm
        let rec search problem fringe =
            if empty fringe then None
            else
                let next = removeFirst fringe
                let depth' = fst next
                let successor = snd next
                let rest = Seq.skip 1 fringe
                if goalTestFn problem successor then
                    Some next
                else
                    // this causes the search to operate in the same manner
                    // as a queue breadth first.
                    let fringe' = Seq.append rest (expand depth' successor problem successorFn)
                    search problem fringe'
        
        search problem (seq { yield (0, initialState) })

    /// <summary>
    /// The DFS search uses a depth limited search
    /// to traverse the state space and expand the state
    /// using the supplied successor fn.
    /// the goal test and successor functions are the same signature as
    /// in the treeSearch function above.
    /// <param name="depthLimit">The depth at which to cutoff the search</param>
    /// <param name="problem">The problem to solve</param>
    /// <param name="initialState">the initial action and state represented as a tuple
    /// of type <A, C, S>
    /// where A = the action, C = the cost (must support (+)) and S = current state
    /// </param>
    /// <param name="successorFn">A successor function
    /// that is used to expand the next search space of the form
    /// ('problem -> ('A * 'C * 'S) -> #seq<'A * 'C  * 'S>) 
    /// </param>
    /// <param name="goalTestFn">A goal test function
    /// that is used to test if the current node in the search reaches the goal
    /// It has the form:
    /// ('problem -> ('A * 'C * 'S) -> bool)
    /// Test if the problem is satisfied by the solution in the tuple ('A * 'C * 'S)
    /// </param>
    /// <returns>
    /// An Option type. If no solution then None
    /// If a solution is found then tuple Some (depth, ('A * 'C * 'S) )
    /// Where depth is the depth at which the solution was found in the state space.
    /// </returns>
    /// </summary>
    let dfsTreeSearch depthLimit problem (initialState:SearchAction<'a,'b,'c>) successorFn goalTestFn =
        let rec dfssearch problem stack =
            if (StackADT.isEmpty stack) then
                None
            else
                let next = (StackADT.pop stack)
                let ((depth', successor), cstack) = next
                if goalTestFn problem successor then
                    Some (depth', successor)
                else if depth' = depthLimit then
                    None
                else
                    let fringe = expand depth' successor problem successorFn
                    let stack'' =
                        Seq.fold (fun stack' item -> StackADT.push stack' item) cstack fringe
                    dfssearch problem stack''
        let stack = StackADT.push StackADT.Nil (0, initialState)
        dfssearch problem stack



    /// <summary>
    /// A*- tree search algorithm
    /// The difference between this algorithm  and the default tree search
    /// is the use of the heuristic cost function heuristicCost
    /// which is used during the expand process to select nodes that
    /// are 
    /// This algorithm is equivalent to the breadth first search
    /// as it expands the successor states it adds them to the back of
    /// the fringe to explore (FIFO) order.
    /// <param name="problem">The problem to solve</param>
    /// <param name="initialState">the initial action and state represented as a tuple
    /// of type <A, C, S>
    /// where A = the action, C = the cost (must support (+)) and S = current state
    /// </param>
    /// <param name="successorFn">A successor function
    /// that is used to expand the next search space of the form
    /// ('problem -> ('A * 'C * 'S) -> #seq<'A * 'C  * 'S>) 
    /// </param>
    /// <param name="goalTestFn">A goal test function
    /// that is used to test if the current node in the search reaches the goal
    /// It has the form:
    /// ('problem -> ('A * 'C * 'S) -> bool)
    /// Test if the problem is satisfied by the solution in the tuple ('A * 'C * 'S)
    /// </param>
    /// <param name="heuristicFn">A heuristic function that calculates
    /// the estimated cost of the current node to the goal
    /// (estimated distance from goal)
    /// It has the signature:
    /// (problem -> ('A * 'C * 'S) -> int)
    /// </param>
    /// <returns>
    /// An Option type. If no solution then None
    /// If a solution is found then tuple Some (depth, ('A * 'C * 'S) )
    /// Where depth is the depth at which the solution was found in the state space.
    /// </returns>
    /// <summary>
    let aStarTreeSearch problem (initialState:SearchAction<'a,'b,'c>) successorFn goalTestFn heuristicFn =
        // main body of the search agorithm
        let rec search problem fringe =
            if empty fringe then None
            else
                let (h, (depth', successor)) = removeFirst fringe
                let rest = Seq.skip 1 fringe
                if goalTestFn problem successor then
                    Some (depth', successor)
                else
                    let (action, cost, state) = successor
                    // this causes the search to operate in the same manner
                    // as a queue breadth first.
                    // the set of successors
                    // are sorted in ascending order by the value of hcost
                    let successors =
                        expand depth' successor problem successorFn
                        |> Seq.map (
                            fun (depthN, (action', cost', state')) ->
                                // g = the actual cost of the search
                                let g = cost + cost'
                                // h = the estimated distance from the goal for this action
                                let h = heuristicFn problem (action', cost', state')
                                // f(cost) = g + h
                                (g + h, (depthN, (action', cost', state'))))
                        |> (Seq.append rest)
                        // order the search space by ascending f(cost)
                        |> Seq.sortBy (fun (hcost, (depthN, (action', cost', state'))) -> hcost)
                        
                    search problem successors
        // each member of the fringe is a tuple
        // of (f(cost), (depth, state))
        search problem (seq { yield (0, (0, initialState)) })


    ()
