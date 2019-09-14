namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Helper.Extension
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Game.Update

open FSharpPlus
open FSharpPlus.Math.Applicative

open System.Collections.Generic

module Update =
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

    open wraikny.Tart.Helper.Math
    open WhiteDungeon.Core.Game.Msg

    let inline updateSkillList f (model : Model) : Model =
        { model with skillList = f model.skillList }


    let inline appendSkills (actor : ^a) (skills : ^a -> Skill.SkillEmitBuilder list) (model : Model) : Model =
        model |> Skill.SkillList.map(
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
                        
                let enemy, cmds = (Actor.Enemy.update model) enemy
                let enemy, skill = Actor.Enemy.getSKill model.gameSetting enemy

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
                        let player = model.players |> Map.find id
                        let exp =
                            ( float32 enemy.actor.level / float32 player.actor.level
                            ) * v / hatesSum * float32 setting.exPoint + 1.0f
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

        let cmd : Cmd<Msg, ViewMsg.ViewMsg> =
            seq{
                for cmds in enemyCmds do
                    for cmd in cmds ->
                        cmd |> function
                        | Actor.Enemy.EnemyCmd.Rotate id ->
                            Random.double01
                            |> Random.map(float32 >> (*) (Angle.pi * 2.0f))
                            |> Random.generate(fun x ->
                                Msg.UpdateEnemyOf(id, EnemyMsg.RotateMsg x)
                            )
                    
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
                        Actor.Player.addExp model.gameSetting v player
                    | _ -> player
                )
        }, cmd


    let checkGateCollision model =
        model.players
        |> Map.toSeq
        |>> ( snd >> ObjectBase.get )
        |> exists(ObjectBase.collidedCells model.gameSetting model.dungeonGateCells)
        |> function
        | true ->
            if model.lastCollidedGate then
                { model with lastCollidedGate = true }
            else
                { model with
                    mode = Stair
                    lastCollidedGate = true  }
        | false ->
            { model with lastCollidedGate = false }

    let update (msg : Msg.Msg) (model : Model) : Model * Cmd<Msg.Msg, ViewMsg.ViewMsg> =
        let model = { model with timePassed = false }

        msg |> function
        | SetGameMode mode ->
            { model with mode = mode }, Cmd.none
        | TimePasses ->
            let m, cmd =
                model
                |> updateEachPlayer Actor.Player.update
                |> updateEnemies
            let m, ds = m |> SkillList.update

            let m = { m with timePassed = true }

            m.players
            |> Map.toSeq
            |>> snd
            |> exists(fun (x : Actor.Player) -> x.actor.statusCurrent.hp > 0.0f)
            |> function
                | false -> { m with mode = GameFinished false }
                | true ->
                    checkGateCollision m

            , (ds |> function
                | [||] -> cmd
                | _ -> Cmd.batch[ Cmd.port(ViewMsg.DamagesView ds); cmd ]
            )

        | PlayerInputs (playerId, inputSet) ->
            let move, direction = Msg.PlayerInput.getPlayerMoveFromInputs inputSet

            model
            |> ifThen (Vector.squaredLength direction > 0.1f) (
                Update.Actor.Actor.move
                    model.gameSetting
                    model.dungeonModel
                    move
                    direction
                |> Actor.map
                |> mapPlayerOf playerId
            )
            , Cmd.none

        | PlayerSkill (playerId, kind) ->
            let player = (model.players |> Map.find playerId)

            let coolTime, skill =
                model.gameSetting.occupationSettings
                |> HashMap.find player.character.currentOccupation
                |> OccupationSetting.skillOf kind
            

            model
            |> ifThen (player |> Player.coolTime kind = 0us) (
                mapPlayerOf playerId (Player.mapCoolTime kind <| fun _ -> coolTime)
                >> appendSkills player (skill model)
            )
            , Cmd.none

        | UpdateEnemyOf(id, msg) ->
            model
            |> mapEnemyOf id (Actor.Enemy.updateOfMsg msg)
            , Cmd.none

        | GenerateNewDungeon ->
            //Dungeon.generateTask model.gameSetting model.dungeonBuilder (length model.dungeonGateCells)
            Dungeon.generateDungeonModel model.dungeonBuilder
            |> TartTask.perform (fun e ->
#if DEBUG
                System.Console.WriteLine(e)
#endif
                GenerateNewDungeon) GeneratedDungeonModel
            |> fun cmd ->
                { model with mode = GameSceneMode.WaitingGenerating }, cmd

        | GeneratedDungeonModel (dungeonBuilder, dungeonModel) ->
            let cmd =
                Dungeon.generateDungeonParams
                    model.gameSetting
                    (length model.dungeonGateCells)
                    dungeonBuilder
                    dungeonModel
                    GeneratedDungeonParams
            model, cmd

        | GeneratedDungeonParams dungeonParams ->
            let model =
                model
                |> updateEachPlayer (fun p ->
                    let pos = (dungeonParams.initPosition - (Vec2.init (float32 p.id.Value) 0.0f) * (ObjectBase.size p))
                    
                    ObjectBase.map (ObjectBase.mapPosition <| fun _ -> pos) p
                )

            let dungeonFloor = model.dungeonFloor + 1us

            { model with
                mode = GameSceneMode.GameMode
                enemies =
                    Model.cellsToEnemies
                        model.gameSetting
                        dungeonFloor
                        dungeonParams.enemyCells
                        model.gameSetting.dungeonCellSize

                skillList = Model.Actor.Skill.SkillList.init

                dungeonBuilder = dungeonParams.dungeonBuilder
                dungeonModel = dungeonParams.dungeonModel
                dungeonGateCells = dungeonParams.gateCells
                dungeonFloor = dungeonFloor
            }
            , Cmd.port ( ViewMsg.UpdateDungeonView(dungeonParams.dungeonModel, dungeonParams.gateCells) )


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