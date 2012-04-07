namespace au.id.cxd.Planner

open System
open System.Collections.Generic
open PlanDefinition
open au.id.cxd.Planner.GPS
open OpenTK
open OpenTK.Graphics.OpenGL
open OpenTK.Graphics
open System.Drawing
open System.Drawing.Imaging
open System.Threading

module BlocksTest =

    /// <summary>
    /// A description of the objects within the blocks world.
    /// </summary>
    type Entities =
        | BlockA
        | BlockB
        | BlockC
        | BlockD
        | Table

    /// <summary>
    /// A description of the possible states of the blocks world.
    /// </summary>
    type StateDesc<'a,'b> =
        | NotValid
        | On of 'a * 'b
        | Clear of 'a

    /// <summary>
    /// The action that can be performed in the blocks world
    /// </summary>
    type BlockAction<'a, 'b> =
        MoveOnto of 'a * 'b


    /// <summary>
    /// This is the target state description.
    /// We want to build a tower made of blocks
    /// The tower is Table -> A -> B -> D / Table -> C
    /// </summary>
    let goal = Goal [ And (On (BlockA, BlockB));
                      And (On (Table, BlockA));
                      And (On (Table, BlockC));
                      And (Clear BlockC);
                      And (On (BlockB, BlockD));
                      And (Clear BlockD);
                      ]

    /// <summary>
    /// This is the current state of the blocks world.
    /// Table -> C -> A
    /// Table -> D -> B
    /// </summary>
    let currentState =
        Goal [ And (On (Table, BlockC));
               And (On (BlockD, BlockB));
               And (On (Table, BlockD));
               And (On (BlockC, BlockA));
               And (Clear BlockA);
               And (Clear BlockB);]

    /// <summary>
    /// The blocks found in the world.
    /// </summary>
    let blocks = [BlockA; BlockB; BlockC; BlockD; ]


    let areEqual stateA stateB =
        match stateA with
        | (Invalid) ->
            match stateB with
            | (Invalid) -> true
            | _ -> false
        | (On (a, b)) ->
            match stateB with
            | (On (c, d)) -> (a = c) && (b = d)
            | _ -> false
        | (Clear a) ->
            match stateB with
            | (Clear b) -> (a = b)
            | _ -> false
            

    let actionEqualsFn action1 action2 =
        let (MoveOnto (a,b)) = getAction action1
        let (MoveOnto (c,d)) = getAction action2
        (a = c) && (b = d)
    

    /// <summary>
    /// Make the preconditions to test against
    /// </summary>
    let makeBlockPreconditions b =
        [ And (Clear b) ]

    let makeEffectOfMove (MoveOnto (a, b)) states =
        let negate =
            List.filter (fun state ->
                             match state with
                             | On (c, d) -> (d = a)
                             | Clear c -> (c = b)
                             | _ -> false) states
        List.map (fun neg -> Not neg) negate

    /// <summary>
    /// Update the state.
    /// </summary>
    let updateState (Plan (goal, actions, actionSeq, currentState)) =
        // find all items that are under another block
        let current = getGoalState currentState
        let under = List.fold (fun accum item ->
                               let data = goalData item
                               match data with
                               | On (b, a) -> b::accum
                               | _ -> accum) List.empty current |>
                    List.filter (fun item -> item <> Table)
        let notunder = List.filter (fun item ->
                                    not (List.exists (fun other -> other = item) under)) blocks
        let removed = List.filter (fun item ->
                                   match item with
                                   | And (Clear a) -> false
                                   | _ -> true) current
        let updated = List.append removed (List.map (fun item -> And (Clear item)) notunder)
        Plan (goal, actions, actionSeq, Goal updated)
        

    /// <summary>
    /// Create all actions possible
    /// for moving a block to a table
    /// </summary>
    let tableActions =
        blocks |>
        List.map (fun item ->
                     let preconds = Precond (Goal (makeBlockPreconditions item))
                     let others = List.filter (fun block -> item <> block) blocks
                     let table = [And (On (Table, item)); And (Clear item); ]
                     let not = List.map (fun other -> Not (On (other, item))) others
                     let effect = List.append table not
                     Action ((MoveOnto (item, Table)), preconds, Effect (Goal effect) ) )

    /// <summary>
    /// Generate all actions that are possible
    /// for moving one block onto another block
    /// </summary>
    let blockActions =
        let blockSeq = List.toSeq blocks
        let results =
            seq { for block in blockSeq do
                  let others = List.filter (fun item -> item <> block) blocks
                  yield! List.map (fun item -> (block, item) ) others } |> Seq.toList
        List.map (fun (a, b) ->
                      let aPreconds = makeBlockPreconditions a
                      let bPreconds = makeBlockPreconditions b
                      let preconds =
                          List.append aPreconds bPreconds
                      let others = List.filter (fun item -> item <> a && item <> b) blocks
                      let not = List.fold (fun accum other -> (Not (On (other, a)) :: accum)) List.empty others
                      let goals = List.append [And (On (b, a) ); Not (Clear b); Not (On (Table, a))] not
                      Action ( MoveOnto (a, b),  Precond (Goal preconds), Effect (Goal (goals) ) ) ) results
        
    /// <summary>
    /// This is the starting plan for the blocks problem.
    /// </summary>
    let blocksPlan1 =
        Plan (goal, List.append tableActions blockActions, Seq.empty, currentState)
           
   
    /// this class allows us to visualise
    /// the search process within the blocks world.
    type BlocksTestVis() =
        inherit GameWindow(800, 600)
        //this is the mutable state
        let queue =
            new Queue<PlanningProblem<BlockAction<Entities,Entities>, StateDesc<Entities,Entities>>>()

        let lockObj = new Object()

        let lock f =
            Monitor.Enter(lockObj)
            try
                f()
            finally
                Monitor.Exit(lockObj)

        
        let f (n:float32) = Convert.ToDouble(n)

        let f32 (n:float) = float32(n)

        let norm (vector:Vector3) =
             let x = f vector.X
             let y = f vector.Y
             let z = f vector.Z
             let len = Math.Sqrt(x*x + y*y + z*z)
             if (len = 0.0) then vector
             else
                 new Vector3(f32 (x/len), f32 (y/len), f32 (z/len))


        let normalise vectorArray = Array.map norm vectorArray

        let normcross (vectorA:Vector3) (vectorB:Vector3) =
            new Vector3(
                vectorA.Y * vectorB.Z - vectorA.Z * vectorB.Y,
                vectorA.Z*vectorB.X - vectorA.X*vectorB.Z,
                vectorA.X*vectorB.Y - vectorA.Y*vectorB.Y)
            |> norm

        let tableCoords = (0.0, -0.5, 0.0)
        
        let blockColors =
            [(BlockA, Color.Red);
             (BlockB, Color.Green);
             (BlockC, Color.Blue);
             (BlockD, Color.Yellow);
             (Table, Color.Black)]

        let startcoords =
            [(BlockA, (-0.4, -0.5, 0.0));
             (BlockB, (-0.1, -0.5, 0.0));
             (BlockC, (0.1, -0.5, 0.0));
             (BlockD, (0.4, -0.5, 0.0));
             (Table, (0.0, -0.5, 0.0))]

        let startX =
            [(BlockA, -1.0);
             (BlockB, -0.6);
             (BlockC, 0.2);
             (BlockD, 0.6)]

        let find block vals =
            let matches = List.find (fun (a, b) -> a = block) vals
            snd matches

        let replace block newval vals =
            List.map (fun (a, b) -> if (a = block) then (a, newval)
                                    else (a, b)) vals

        let moveBlockOnTo coords a b =
            let (bX, bY, bZ) = find b coords
            match b with
                | Table ->
                    let x = find a startX
                    replace a (x, bY + 0.4, bZ) coords
                | _ ->
                    replace a (bX, bY + 0.4, bZ) coords
        
            

        let cube (cX, cY, cZ) (color:Color) =
            GL.PushMatrix()
            
            GL.Color3(color)
            let posX = 0.1 + cX/2.0 |> f32
            let negX = -0.1 + cX/2.0 |> f32
            let posY = 0.1 + cY/2.0 |> f32
            let negY = -0.1 + cY/2.0 |> f32
            let posZ = 0.1 + cZ / 2.0 |> f32
            let negZ = -0.1 + cZ / 2.0 |> f32
            let vertices =
                [|
                 new Vector3(negX, negY, posZ);
                 new Vector3(posX, negY, posZ);
                 new Vector3(posX, posY, posZ);
                 new Vector3(negX, posY, posZ);
                 new Vector3(negX, negY, negZ);
                 new Vector3(posX, negY, negZ);
                 new Vector3(posX, posY, negZ);
                 new Vector3(negX, posY, negZ); |]

            let indices = [| (0, 1, 2); (2, 3, 0);
                           (3, 2, 6); (6, 7, 3);
                           (7, 6, 5); (5, 4, 7);
                           (4, 0, 3); (3, 7, 4);
                           (0, 1, 5); (5, 4, 0);
                           (1, 5, 6); (6, 2, 1); |]
            let norms =
                indices |> 
                Array.map
                      (fun (i,j,k) ->
                       let v1 = vertices.[i]
                       let v2 = vertices.[j]
                       let v3 = vertices.[k]
                       let vertA = new Vector3(
                          v1.X - v2.X,
                          v1.Y - v2.Y,
                          v1.Z - v2.Z
                          )
                       let vertB = new Vector3(
                          v2.X - v3.X,
                          v2.Y - v3.Y,
                          v2.Z - v3.Z
                          )
                       normcross vertA vertB)
            GL.Begin(BeginMode.Triangles)
            Array.zip indices norms |>
            Array.iter(
                fun ((i, j, k), (vnorm:Vector3)) ->
                    let v1 = vertices.[i]
                    let v2 = vertices.[j]
                    let v3 = vertices.[k]
                    GL.Normal3([|vnorm.X; vnorm.Y; vnorm.Z|])
                    GL.Vertex3([|v1.X;v1.Y;v1.Z|])

                    GL.Vertex3([|v2.X;v2.Y;v2.Z|])

                    GL.Vertex3([|v3.X;v3.Y;v3.Z|]) )

            GL.End()
        
            GL.PopMatrix()

        let updateCoords coords (Plan (goal, actions, actionSeq, currentState)) =
            let current = getGoalState currentState |>
                          List.filter( fun goal ->
                                       match goal with
                                           | Not a -> false
                                           | _ -> true) |> List.map goalData
            // put the resulting coords onto the queue
            List.fold
                 (fun accum item ->
                     match item with
                         | On (b, a) ->
                             moveBlockOnTo accum a b
                         | _ -> accum) coords current
            
        /// draw the coordinates
        let drawCoords coords =
            List.iter (fun (block, coord) ->
                           let col = find block blockColors
                           cube coord col) coords
            coords

        // mutable state variables
        let mutable copycoords = startcoords
        let mutable isInit = false
        let mutable lastPlan = blocksPlan1


        override m.OnLoad(e) =
            base.OnLoad(e)
            
            Tasks.TaskFactory().StartNew(fun () ->
                let result = gps blocksPlan1 (=) actionEqualsFn (fun plan ->
                                                                 lock (fun () ->
                                                                       queue.Enqueue(plan)
                                                                       )
                                                                 updateState plan)
                ())     
            GL.ClearColor(Color.Black)

            (*
            GL.Enable(EnableCap.Lighting)
            let mat_specular = new Vector4( 1.0f, 1.0f, 1.0f, 1.0f )
            let mat_shininess = [| 50.0f |]
            let light_position = new Vector4( 1.0f, 1.0f, 1.0f, 0.0f )
            let light_ambient = new Vector4( 0.5f, 0.5f, 0.5f, 1.0f )
            GL.ShadeModel(ShadingModel.Smooth)
 
            GL.Material(MaterialFace.Front, MaterialParameter.Specular, mat_specular)
            GL.Material(MaterialFace.Front, MaterialParameter.Shininess, mat_shininess)
            GL.Light(LightName.Light0, LightParameter.Position, light_position)
            GL.Light(LightName.Light0, LightParameter.Ambient, light_ambient)
            GL.Light(LightName.Light0, LightParameter.Diffuse, mat_specular)
            GL.Enable(EnableCap.Lighting)
            GL.Enable(EnableCap.Light0)
            GL.Enable(EnableCap.DepthTest)
            GL.Enable(EnableCap.ColorMaterial)
            //GL.Enable(EnableCap.CullFace)
            *)

        override m.OnResize(e) =
            base.OnResize(e)
            GL.Viewport(0, 0, m.Width, m.Height)
            GL.MatrixMode(MatrixMode.Projection)
            GL.LoadIdentity()
            GL.Ortho(-1.0, 1.0, -1.0, 1.0, 0.0, 4.0)

       
        override m.OnRenderFrame(fe) =
            base.OnRenderFrame(fe)
            GL.Clear(ClearBufferMask.ColorBufferBit)
            GL.MatrixMode(MatrixMode.Modelview)
            GL.LoadIdentity()
            // drawing here.
            //cube (0.0, 0.0, 0.0) Color.Red
            
            lock (fun () -> 
                  if (not isInit) then
                      copycoords <- updateCoords startcoords blocksPlan1
                      isInit <- true
                      copycoords <-
                                 updateCoords startcoords blocksPlan1
                                 |> drawCoords
                      lastPlan <- blocksPlan1
                  else if (queue.Count > 0) then
                      lastPlan <-  queue.Dequeue()
                      copycoords <- (updateCoords copycoords lastPlan)
                      copycoords <- drawCoords (updateCoords copycoords lastPlan)
                  else
                      copycoords <- (updateCoords copycoords lastPlan)
                      copycoords <- drawCoords (updateCoords copycoords lastPlan)
                  )
            m.SwapBuffers()
            
            
        
        
    let testBlocksGps = (fun () ->
                         let gw = new BlocksTestVis()
                         gw.Run()
                         ) //printfn "Result\n %A" result)

    ()
