namespace au.id.cxd.Planner

open System

/// <summary>
/// This module provides
/// the abstract definition of a planning problem.
/// A planning problem consists of a goal state
/// a set of actions that can be taken to reach a goal state
/// and an initial condition.
/// </summary>
module PlanDefinition =

    /// <summary>
    /// A goal may contain items that are positive or negative.
    /// A negation of a goal item in a set of subgoals removes
    /// the corresponding goal from the accumulated goal state of the current world.
    /// </summary>
    type GoalItem<'a> =
        | And of 'a
        | Or of 'a
        | Not of 'a


    /// <summary>
    /// The goal state represented as a
    /// list of <'a>
    /// </summary>
    type GoalState<'a> =
        Goal of GoalItem<'a> List

    /// <summary>
    /// The effect of an action produces a "subgoal" state.
    /// </summary>
    type ActionEffect<'a> =
        Effect of GoalState<'a>

    /// <summary>
    /// An action precondition is a subgoal state that must
    /// be satisfied prior to allowing an action to occur.
    /// </summary>
    type ActionPrecondition<'a> =
        Precond of GoalState<'a>

    /// <summary>
    /// An action can be performed if the action precondition is met
    /// after the action occurs the action effect is added to the goal state.
    /// </summary>
    type ActionOp<'a, 'b, 'c> =
        Action of 'a * ActionPrecondition<'b> * ActionEffect<'c>

    /// <summary>
    /// A planning problem is represented by
    /// a Target Goal State
    /// a list of available actions
    /// and a resulting list of actions that have been chosen
    /// to execute in order to achieve the plan.
    //// and a current goal state that contains the current state of having
    //// executed the plan during the search.
    /// </summary>
    type PlanningProblem<'a, 'b> =
        Plan of GoalState<'b> * ActionOp<'a,'b,'b> list * 'a seq * GoalState<'b>

    (* a set of helper methods below *)

    /// <summary>
    /// Get the action that can be executed from the
    /// action operation.
    /// </summary>
    let getAction (Action (a, b, c)) = a

    /// <summary>
    /// Get the precondition of an action
    /// </summary>
    let getPrecondition (Action (a, b, c)) = b

    /// <summary>
    /// Get the side effects of having executed an action
    /// </summary>
    let getEffect (Action (a, b, c)) = c

    /// <summary>
    /// Get the Sub Goal of the Effect
    /// </summary>
    let getEffectGoal (Effect goal) = goal

    /// <summary>
    /// Get the sub goal contained within the precondition.
    /// </summary>
    let getPrecondGoal (Precond goal) = goal

    /// <summary>
    /// Get the data list of the Sub Goal
    /// </summary>
    let getGoalState (Goal state) = state

    /// <summary>
    /// Get the target goal state
    /// </summary>
    let getTargetState (Plan (target, actions, results, current)) = target

    /// <summary>
    /// Get the actions available to the plan
    /// </summary>
    let getActionList (Plan (target, actions, results, current)) = actions

    /// <summary>
    /// Get the set of planned actions to execute to achieve the goal
    /// </summary>
    let getPlannedActions (Plan (target, actions, results, current)) = results

    /// <summary>
    /// Get the current state.
    /// </summary>
    let getCurrentState (Plan (target, actions, results, current)) = current

    /// <summary>
    /// Get the data from a goal item
    /// </summary>
    let goalData item =
        match item with
            | And a
            | Or a
            | Not a -> a

    /// <summary>
    /// Determine if goalA conforms with goalB
    /// any Or And goal is accepted if the data is equal.
    /// only two Not goals with same data are considered equal.
    /// </summary>
    let conformsWith goalA goalB goalDataIsEqualFn =
        match goalA with
            | And a
            | Or a ->
                match goalB with
                    | And b
                    | Or b -> goalDataIsEqualFn a b
                    | _ -> false
            | Not a ->
                match goalB with
                    | Not b -> goalDataIsEqualFn a b
                    | _ -> false
            
    /// <summary>
    /// Compare 2 goal items
    /// </summary>
    let applyToGoalItems itemA itemB binaryFn =
        binaryFn (goalData itemA) (goalData itemB)


    /// <summary>
    /// Determine whether a goal has been achieved within the current state.
    /// </summary>
    let isGoalAchieved goal (Plan (target, actions, results, current)) goalEqualsFn =
        let currentGoals = getGoalState current
        currentGoals |>
        List.exists (fun currentGoal -> conformsWith goal currentGoal goalEqualsFn)


    /// <summary>
    /// Take an action and change the current sub goal state and
    /// add the action to the sequence of actions to execute.
    /// </summary>
    let takeAction action (Plan (target, actions, results, current)) goalEqualsFn =
        let act = getAction action
        let subgoals = getEffect action |> getEffectGoal |> getGoalState
        let currentGoals = getGoalState current
        let positiveGoals =
            List.filter (fun item ->
                         match item with
                         | And a
                         | Or a ->
                              // include only the goals that have not yet been achieved.
                              not (isGoalAchieved item (Plan (target, actions, results, current)) goalEqualsFn)
                         | _ -> false) subgoals
        let negativeGoals =
            List.filter (fun item ->
                         match item with
                         | Not a -> true
                         | _ -> false) subgoals

        let mergedGoals = List.append currentGoals positiveGoals
        
        let effectedGoals =
            if (List.length negativeGoals) > 0 then
                mergedGoals 
                |> List.filter(
                    fun item -> not (List.exists (
                                                  fun search ->
                                                      (applyToGoalItems item search goalEqualsFn)) negativeGoals) )
            else
                mergedGoals
        let newActions = Seq.append results (seq { yield act } )
        Plan (target, actions, newActions, Goal effectedGoals)

    /// <summary>
    /// Determine if setA contains all goals in setB
    /// </summary>
    let goalSetContainsAll setA setB goalEqualsFn =
        setB |> 
        List.forall (fun goalB ->
                         setA |>   
                         List.exists (fun goalA -> conformsWith goalA goalB goalEqualsFn) )

    /// <summary>
    /// Determine if setA contains no goals in setB
    /// </summary>
    let goalSetContainsNone setA setB goalEqualsFn =
        setB |> 
        List.forall (fun goalB ->
                         not (setA |>   
                              List.exists (fun goalA -> conformsWith goalA goalB goalEqualsFn)) )



    
    /// <summary>
    /// Determine if the supplied action preconditions are satisfied by
    /// the current set of goals stored in the plan.
    /// Use the "containsAllFn" to determine whether the current set of goals
    /// contains all requirements from the precondition.
    /// The signature of containsAllFn is
    /// (fun 'a List -> a' List -> bool)
    /// the first list argument is the parent set that must contain all values
    /// of the second list argument.
    /// </summary>
    let isSatisfiable (Action (action, preconds, effects)) (Plan (target, actions, results, current)) goalEqualsFn =
        
        let positiveGoals =
            List.filter (fun item ->
                         match item with
                         | Not a -> false
                         | _ -> true)
        let negativeGoals =
            List.filter (fun item ->
                         match item with
                         | Not a -> true
                         | _ -> false)
        let currentGoals = getGoalState current
        let preGoals = getPrecondGoal preconds |> getGoalState
        let currentPositive = positiveGoals currentGoals
        let currentNegative = negativeGoals currentGoals
        let actionPositive = positiveGoals preGoals
        let actionNegative = negativeGoals preGoals

        // find out if there are any goals that match at all
        (goalSetContainsAll currentPositive actionPositive goalEqualsFn) &&
        (goalSetContainsNone currentPositive actionNegative goalEqualsFn) &&
        (goalSetContainsAll currentNegative actionNegative goalEqualsFn) &&
        (goalSetContainsNone currentNegative actionPositive goalEqualsFn)

    

    /// <summary>
    /// Determine whether the target goal state in the plan has been achieved by
    /// the current goal state in the plan.
    /// The function "goalSetsAreEqualFn" takes two sets (types are of 'a List)
    /// This function needs to check whether all items in the target goal set exist
    /// in the current goal set. If they do it should return true.
    /// otherwise it will return false.
    /// The function signature is
    /// (fun 'a List -> 'a List -> bool)
    /// </summary>
    let areAllGoalsAchieved (Plan (target, actions, results, current)) goalEqualsFn =
        let targetGoals = getGoalState target
        let currentGoals = getGoalState current
        targetGoals |>
        List.forall (fun goalA ->
                         currentGoals |>   
                         List.exists (fun goalB -> conformsWith goalA goalB goalEqualsFn))

   
    /// <summary>
    /// An action is appropriate to a goal if the effects contain the goal
    /// as an And or an Or
    /// </summary>
    let isAppropriate goal (Action (action, preconds, effects)) goalEqualsFn =
        let (Goal effectGoals) = getEffectGoal effects
        List.exists (fun effectGoal ->
                          conformsWith goal effectGoal goalEqualsFn) effectGoals

    /// <summary>
    /// Find an appropriate action for the goal
    /// and check whether the preconditions for the action allow the action
    /// </summary>
    let findAppropriateAction (Plan (target, actions, results, current)) goalEqualsFn =
        let targetGoals = getGoalState target
        actions |>
        List.filter (fun action ->
                         List.exists (fun goal -> 
                                      (isAppropriate goal action goalEqualsFn) ) targetGoals)


    let canBeApplied action (Plan (target, actions, results, current)) goalEqualsFn =
        let targetGoals = getGoalState target
        List.exists (fun goal -> (isAppropriate goal action goalEqualsFn)) targetGoals

    
    ()
