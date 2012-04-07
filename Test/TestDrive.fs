namespace au.id.cxd.Planner

open System
open au.id.cxd.Planner.PlanDefinition
open au.id.cxd.Planner.GPS

module TestDrive =

    type DriveGoals =
        | AtHome
        | CarWorks
        | AtSchool
        | InCommunicationWithShop
        | ShopKnowsProblem
        | KnowPhoneNumber
        | HavePhoneBook
        | HaveMoney
        | CarNeedsBattery
        | ShopHasMoney

    type DriveActions =
        | DriveToSchool
        | ShopInstallsBattery
        | TellShopProblem
        | TelephoneShop
        | LookupNumber
        | GiveShopMoney

    let preconds goals = Precond (Goal (List.map (fun item -> And item) goals))

    let postconds goals = Effect (Goal goals)

    let actions =
        [Action (DriveToSchool, preconds [AtHome; CarWorks], postconds [Not AtHome; And AtSchool]);
         Action (ShopInstallsBattery, preconds [CarNeedsBattery; ShopKnowsProblem; ShopHasMoney ], postconds [And CarWorks]);
         Action (TellShopProblem, preconds [InCommunicationWithShop], postconds [And ShopKnowsProblem] );
         Action (TelephoneShop, preconds [KnowPhoneNumber], postconds [And InCommunicationWithShop]);
         Action (LookupNumber, preconds [HavePhoneBook], postconds [And KnowPhoneNumber]);
         Action (GiveShopMoney, preconds [HaveMoney; ShopKnowsProblem], postconds [ Not HaveMoney; And CarWorks; Not CarNeedsBattery; ])
         ]

    let currentState = Goal [And AtHome; And CarNeedsBattery; And HaveMoney; And HavePhoneBook]

    let targetState = Goal [And AtSchool]

    let drivePlan  = Plan (targetState, actions, Seq.empty, currentState)

    let actionEqualsFn action1 action2 =
        let act1 = getAction action1
        let act2 = getAction action2
        act1 = act2

    let testGps = (fun () ->
                   let result = gps drivePlan (=) actionEqualsFn (fun plan -> plan)
                   printfn "Result: \n%A" result)

    ()
