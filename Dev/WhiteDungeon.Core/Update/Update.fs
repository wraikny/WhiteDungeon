module WhiteDungeon.Core.Update.Update

open wraikny.Tart.Helper.Extension
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon

open WhiteDungeon.Core
open WhiteDungeon.Core.Model

open FSharpPlus
open FSharpPlus.Math.Applicative

open System.Collections.Generic


let inline incrCount (model : Model) : Model =
    { model with count = model.count + 1u }

let inline setPlayers players model =
    { model with players = players }

let inline mapPlayers f (model : Model) : Model =
    { model with players = f model.players }

let inline mapEnemies f (model : Model) =
    { model with enemies = f model.enemies }


let inline updateEachPlayer f (model : Model) : Model =
    model
    |> setPlayers(f <!> model.players)


let mapPlayerOf id f (model : Model) =
    model.players
    |> Map.tryFind id
    |>> (fun player ->
        model
        |> mapPlayers (Map.add id (f player))
    )
    |> Option.defaultValue model

let inline mapEnemy f (model : Model) : Model =
    { model with enemies = model.enemies |>> f }

let inline mapEnemyOf id f (model : Model) =
    model.enemies
    |> Map.tryFind id
    |>> (fun enemy ->
        model
        |> mapEnemies (Map.add id (f enemy)))
    |> Option.defaultValue model

open wraikny.Tart.Math
open WhiteDungeon.Core

let inline updateSkillList f (model : Model) : Model =
    { model with skillList = f model.skillList }


let inline appendSkills (actor : ^a) (skills : ^a -> SkillEmitBuilder list) (model : Model) : Model =
    model |> SkillList.map(
        SkillList.appendActorSkills actor skills
    )


let updateEnemies model =
    let playerPoses =
        model.players
        |> Map.toSeq
        |>> (fun (_, p) -> ObjectBase.position p)
        |> toList

    // mutable
    let enemies = List<_>(Map.count model.enemies)
    let skillEmits = List<_>()
    let enemyCmds = List<_>()
    let exPoints = Dictionary<_, _>()
        
    for (id, enemy) in (Map.toSeq model.enemies) do
        let pos = ObjectBase.position enemy
        let d = model.gameSetting.enemyUpdateDistance

        if (playerPoses |> Seq.exists(fun p -> Vector.squaredLength(p - pos) < d * d )) then
                        
            let enemy, cmds = (Enemy.update model) enemy
            let enemy, skill = Enemy.getSKill model.gameSetting enemy

            // mutable
            enemyCmds.Add(cmds)
            skill |> Option.iter(fun skill ->
                skillEmits.Add(enemy, skill)
            )

            let pos = ObjectBase.position enemy

            let isAlive = (Actor.statusCurrent enemy).hp > 0.0f
            let notNan = not(pos.x = nanf || pos.y = nanf)

            match (isAlive, notNan) with
            | _, false ->
            #if DEBUG
                printfn "Enemy(%d)'s position is nanf: (%f, %f)" enemy.id.Value pos.x pos.y
            #endif
                ()
            | true, true ->
                enemies.Add(id, enemy)
            | false, true ->
                let setting = model.gameSetting.enemySettings |> HashMap.find enemy.kind
                // 経験値
                let hates = enemy.hateMap |> Map.toList
                let hatesSum = hates |>> snd |> sum
                for (id, v) in hates do
                    //let player = model.players |> Map.find id
                    let exp =
                        ( float32 enemy.actor.level ) * v / hatesSum * float32 setting.exPoint + 1.0f
                        |> uint16

                    if exPoints.ContainsKey(id) then
                        exPoints.[id] <- exPoints.[id] + exp
                    else
                        exPoints.Add(id, exp)

        elif not(pos.x = nanf || pos.y = nanf) then
            enemies.Add(id, enemy)

    let skillList =
        seq {
            for (enemy, skill) in skillEmits ->
                SkillList.appendActorSkills enemy (skill model)
        }
        |> Seq.fold (|>) model.skillList

    let cmd : Cmd<Msg, ViewMsg> =
        seq{
            for cmds in enemyCmds do
                for cmd in cmds ->
                    cmd |> function
                    | Enemy.EnemyCmd.Rotate id ->
                        monad {
                            let! x = Random.double01
                            let x = pi * 2.0f * float32 x
                            return
                                Msg.UpdateEnemyOf(id, EnemyMsg.RotateMsg x)
                        }
                        |> SideEffect.perform
                    
        }
        |> Cmd.batch


    { model with
        enemies = Map.ofSeq enemies
        skillList = skillList
        players =
            model.players
            |> Map.map(fun id player ->
                exPoints.TryGetValue(id) |> function
                | true, v ->
                    printfn "exp: Player(%d) got %d" id.Value v
                    Player.addExp model.gameSetting v player
                | _ -> player
            )
    }, cmd


//let checkGateCollision model =
//    model.players
//    |> Map.toSeq
//    |>> snd
//    |> exists(ObjectBase.collidedCells model.gameSetting model.dungeonGateCells)
//    |> function
//    | true when model.mode = GameMode ->
//        { model with mode = GateMode }
//    | _ -> model


let buildingsCollision (model : Model) =
    let player = model.players |> Map.find model.localPlayerId
    let frame = model.inBuildingFrame

    let pos = ObjectBase.position player

    model.buildingCells
    |> HashMap.tryFind(DungeonModel.coordinateToCell model.gameSetting.dungeonCellSize pos)
    |> function
    | Some b ->
        { Building.whenInside b.kind model with
            inBuildingFrame = frame + 1u
            currentBuilding = Some b
        }
    | None when frame = 0u -> model
    | _ -> { model with inBuildingFrame = 0u; currentBuilding = None }


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    let model = { model with timePassed = false }

    msg |> function
    | SetGameMode mode ->
        { model with mode = mode }, Cmd.none
    | TimePasses ->
        let m, cmd =
            model
            |> updateEachPlayer Player.update
            |> updateEnemies
        let m, ds = m |> SkillList.update

        let m = { m with timePassed = true }

        m.players
        |> Map.toSeq
        |>> snd
        |> exists(fun (x : Player) -> x.actor.statusCurrent.hp > 0.0f)
        |> function
            | false -> { m with mode = GameFinished false }
            | true ->
                //checkGateCollision m
                m
                |> buildingsCollision

        , (ds |> function
            | [||] -> cmd
            | _ ->
                //let player = m.players |> Map.find model.localPlayerId
                let ds = ds |>> fun(p, v) -> (p, v)
                Cmd.batch[ Cmd.ofPort(ViewMsg.DamagesView ds); cmd ]
        )

    | PlayerInputs (playerId, inputSet) ->
        let move, direction =
            let isDash = inputSet |> Set.contains Dash
            let dirs =
                inputSet
                |> Seq.choose(function | Direction x -> Some x | _ -> None)
                |> Set.ofSeq
            PlayerInput.getPlayerMoveFromInputs isDash dirs

        let skills =
            inputSet
            |> Seq.choose(function
                | Skill x -> Some x
                | _ -> None
            )
            |> toList

        let player = model.players |> Map.find playerId

        let fireSkill kind model =
            let coolTime, skill =
                model.gameSetting.occupationSettings
                |> HashMap.find player.character.currentOccupation
                |> OccupationSetting.skillOf kind
                            
                
            model
            |> ifThen (player |> Player.coolTime kind = 0us) (
                mapPlayerOf playerId (Player.mapCoolTime kind <| fun _ -> coolTime)
                >> appendSkills player (skill model)
            )

        model
        |> ifThen (Vector.squaredLength direction > 0.1f) (
            Update.Actor.move
                model.gameSetting
                model.dungeonModel
                move
                direction
            |> Actor.map
            |> mapPlayerOf playerId
        )
        |> (
            skills
            |>> fireSkill
            |> Seq.fold (>>) id
        )  
        , Cmd.none

    | UpdateEnemyOf(id, msg) ->
        model
        |> mapEnemyOf id (Enemy.updateOfMsg msg)
        , Cmd.none

    | GenerateNewDungeon ->
        let dungeonBuilder =
            model.gameSetting.createDungeonBuilder
                model.dungeonFloor model.initSize
        //Dungeon.generateTask model.gameSetting model.dungeonBuilder (length model.dungeonGateCells)
        Dungeon.generateDungeonModel dungeonBuilder
        |> SideEffect.performWith(function
            | Ok a -> GeneratedDungeonModel a
            | Error e ->
#if DEBUG
            printfn "%A" e
#endif
            raise e
        )
        |> fun cmd ->
            { model with mode = GameSceneMode.WaitingGenerating }, cmd

    | GeneratedDungeonModel (dungeonBuilder, dungeonModel) ->
        let cmd =
            Dungeon.generateDungeonParams
                model.gameSetting
                (model.gameSetting.gateCount model.dungeonFloor 1us)
                dungeonBuilder
                dungeonModel
                GeneratedDungeonParams
        model, cmd

    | GeneratedDungeonParams dungeonParams ->
        model |> Model.updateDungeon dungeonParams
        , Cmd.ofPort ( ViewMsg.UpdateDungeonView(dungeonParams.dungeonModel) )


    //#if DEBUG

    //| AppendSkillEmits ->
    //    let id = PlayerID 0u
    //    let player0 = model.players |> Map.find id
    //    let dir = 
    //        player0.actor.objectBase.direction
    //        |> Model.MoveDirection.toVector

    //    let pos =
    //        player0.actor.objectBase.position
    //        + (100.0f *. dir)

    //    let emit : Skill.SkillEmitBuilder =
    //        Skill.AreaBuilder {
    //            skillBase =
    //                {
    //                    delay = 0u
    //                    effects = [|
    //                        Skill.Damage(fun atk def ->
    //                            0.0f
    //                        )
    //                    |]
    //                }
    //            objectBase = ObjectBase.init (one .* 100.0f) pos

    //            target = Skill.AreaTarget.Enemies

    //            removeWhenHitWall = true
    //            removeWhenHitActor = true

    //            move = seq {
    //                for _ in 1..10 -> Skill.Stay
    //                for _ in 1..60 -> Skill.Move(dir .* 5.0f)
    //                for _ in 1..60 -> Skill.Scale(one .* 5.0f)
    //            } |> toList
    //        }

    //    let emits = [emit]

    //    model |> appendSkills player0.actor emits, Cmd.none

    //#endif