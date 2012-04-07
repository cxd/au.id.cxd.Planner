namespace au.id.cxd.Planner

open System
open au.id.cxd.Planner.TreeSearch


module TestTreeSearch =

    type TravelDest =
         | Arad
         | Bucharest
         | Craiova
         | Drobeta
         | Eforie
         | Fagaras
         | Giurgiu
         | Hirsova
         | Iasi
         | Lugoj
         | Mehadia
         | Neamt
         | Oradea
         | Pitesti
         | RimnicuVilcea
         | Sibiu
         | Timisoara
         | Urziceni
         | Vaslui
         | Zerind

    type Problem<'a> =
        {
            Start:TravelDest;
            Dest:TravelDest;
            Map:'a;
            Visited:TravelDest List;
        }

    /// <summary>
    /// The links between cities and the distance between them.
    /// </summary>
    let mapRomania =
        [ (Arad, [(75, Zerind);
                  (118, Timisoara);
                  (140, Sibiu)]);
          (Zerind, [(75, Arad);
                    (71, Oradea)]);
          (Oradea, [(151, Sibiu);
                    (71, Zerind)]);
          (Timisoara, [(118, Arad);
                       (111, Lugoj)]);
          (Sibiu, [(140, Arad);
                   (99, Fagaras);
                   (80, RimnicuVilcea)]);
          (Lugoj, [(111, Timisoara);
                   (70, Mehadia)];);
          (Mehadia, [(70, Lugoj);
                     (75, Drobeta)]);
          (RimnicuVilcea, [(80, Sibiu);
                           (97, Pitesti);
                           (146,Craiova)]);
          (Fagaras, [(99,Sibiu);
                     (211,Bucharest)]);
          (Craiova, [(120, Drobeta);
                     (146, RimnicuVilcea);
                     (138, Pitesti)]);
          (Pitesti, [(138, Craiova);
                     (97, RimnicuVilcea);
                     (101, Bucharest)]);
          (Bucharest, [(211, Fagaras);
                       (101, Pitesti);
                       (90, Giurgiu);
                       (85, Urziceni)]);
          (Giurgiu, [(90, Bucharest)]);
          (Urziceni, [(85, Bucharest);
                      (98, Hirsova);
                      (142, Vaslui)]);
          (Hirsova, [(98, Urziceni);
                     (86, Eforie)]);
          (Eforie, [(86, Hirsova)]);
          (Vaslui, [(142, Urziceni);
                    (92, Iasi)]);
          (Iasi, [(92, Vaslui);
                  (87, Neamt)]);
          (Neamt, [(87, Iasi)]);
          ]

    /// <summary>
    /// Straight line distances from bucharest
    /// used in the A* search example heuristicFn
    /// </summary>
    let sldBucharest =
        [ (Arad, 366);
          (Bucharest, 0);
          (Craiova, 160);
          (Drobeta, 242);
          (Eforie, 161);
          (Fagaras, 176);
          (Giurgiu, 77);
          (Hirsova, 151);
          (Iasi, 226);
          (Lugoj, 244);
          (Mehadia, 241);
          (Neamt, 234);
          (Oradea, 380);
          (Pitesti, 100);
          (RimnicuVilcea, 193);
          (Sibiu, 253);
          (Timisoara, 329);
          (Urziceni, 80);
          (Vaslui, 199);
          (Zerind, 374); ]

    /// <summary>
    /// The heuristic function that takes a current node and
    /// estimates the cost to the goal using the distance from bucharest.
    /// </summary>
    let heuristicFn problem (action, cost, state) =
        sldBucharest 
        |> List.find(fun (city, sld) -> city = action)
        |> snd

          
    let travelProblem =
        { Start = Oradea;
          Dest = Bucharest;
          Map = mapRomania;
          Visited = List.empty;
          }

    let initialState = (Oradea, 0, [Oradea;])


    let travelProblem2 =
        { Start = Eforie;
          Dest = Drobeta;
          Map = mapRomania;
          Visited = List.empty; }

    let initialState2 = (Eforie, 0, [Eforie])

    /// <summary>
    /// This function tests if we have reached the goal city (dest) from the problem
    /// </summary>
    let goalTestFn problem (action, cost, state) =
        printfn "Test %A = %A" action problem.Dest
        action = problem.Dest

    /// <summary>
    /// This function selects a set of cities that are adjacent
    /// to the current city
    /// </summary>
    let successorFn problem (atCity, cost, visitedCities) =
        printfn "Expand %A" atCity
        let (findCity, adjacent) =
            List.find (fun (city, neighbours) -> city = atCity) problem.Map
        let notVisited =
            adjacent |>
            List.filter (fun (distance, city) ->
                             not (List.exists (fun visited -> city = visited) visitedCities))
        let fringe =
            notVisited
            |> List.toSeq
            |> Seq.map (fun (distance, city) ->
                        (city, distance, List.append visitedCities [city]))
        printfn "Next %A" fringe
        fringe

    let testSearch runFn =
        match runFn with
        | None ->
            printfn "Search Failed"
        | Some solution ->
            printfn "Search Result \n%A\n" solution

    let treeSearch1 problem state =
        (treeSearch problem state successorFn goalTestFn)

    

    let test1 () =
        treeSearch1 travelProblem initialState
        |> testSearch
        
    let test2 () =
        treeSearch1 travelProblem2 initialState2
        |> testSearch

    let dfsSearch1 limit problem state =
        (dfsTreeSearch limit problem state successorFn goalTestFn) 

    let testDFS1 () =
        dfsSearch1 1000 travelProblem initialState
        |> testSearch
        
    let testDFS2 () =
        dfsSearch1 1000 travelProblem2 initialState2
        |> testSearch
        
    // test that the limit causes the search to fail early.
    let testDFSCutOff () =
        dfsSearch1 1 travelProblem2 initialState2
        |> testSearch

    let aStarSearch1 problem state =
        aStarTreeSearch problem state successorFn goalTestFn heuristicFn

    let testAStarSearch1 () =
        aStarSearch1 travelProblem initialState
        |> testSearch
    
    ()

    
