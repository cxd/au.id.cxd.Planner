namespace au.id.cxd.Planner

open System
open au.id.cxd.Planner.PlanDefinition
open au.id.cxd.Planner.TreeSearch

/// <summary>
/// This is a simple GPS solver algorithm based on the
/// implementation provided by norvigs paradigm of ai.
/// </summary>
module GPS =

  
    /// <summary>
    /// Achieve all goals in the target of the plan.
    /// Return a tuple a flag that is true
    /// if it is successful and false otherwise, and the resulting plan
    /// when a new action is taken the resulting state
    /// is supplied to a state update function
    /// TODO: test the simple gps function.
    /// </summary>
    let gps (Plan (target, actions, results, current)) goalEqualsFn actionEqualsFn stateUpdateFn =
        /// <summary>
        /// The expand function
        /// this is a forward state search
        /// </summary>
        let expandPlan (state) (action, cost, Plan (target, actions, results, current)) =

            let currentGoals = getGoalState current
           
            let satisfiable action =
                (isSatisfiable action (Plan (target, actions, results, current)) goalEqualsFn)

            let possible =
                List.filter (fun action ->
                             (satisfiable action)) actions

            if (List.length possible) = 0 then
                printfn "No possible actions found for current state\n%A" currentGoals
            // using the target find the actions
            // which as part of their post condition contain
            // subgoals appearing in the target and
            // also do not remove any existing goals from the current state
            // that are compatible with the target
            
            possible
            |> List.toSeq
            |> Seq.map (fun action ->
                        let otherActions =
                            List.filter (fun otherAction -> not (actionEqualsFn action otherAction)) actions
                        let newState = takeAction action (Plan (target, otherActions, results, current)) goalEqualsFn |> stateUpdateFn
                        let act = getAction action
                        printfn "Next Action:\n%A" act
                        let (Plan (newTarget, newActions, newResults, newCurrent)) = newState
                        printfn "Current Goals \n%A" currentGoals
           
                        printfn "NextState: \n%A" newCurrent
                        printfn "\n"
                        ( action, 1, newState ) )

        /// <summary>
        /// the goal test function to use for the state search.
        /// </summary>
        let goalTestFn plan (action, cost, actionPlanState) =
            areAllGoalsAchieved actionPlanState goalEqualsFn

        let firstAction = List.nth actions 0

        let result =
            treeSearch (Plan (target, actions, results, current)) (firstAction, 0, (Plan (target, actions, results, current)) ) expandPlan goalTestFn
        match result with
            | None -> (false, (Plan (target, actions, results, current)))
            | Some (depth, (action, cost, state)) ->
                (true, state)
        
        
    ()
