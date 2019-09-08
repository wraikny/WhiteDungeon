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
        let skillEmits = List<_>()

        let enemies =
            seq {
                for (id, enemy) in (Map.toSeq model.enemies) do
                    let pos = ObjectBase.position enemy
                    let d = model.gameSetting.enemyUpdateDistance

                    if (playerPoses |> Seq.exists(fun p -> Vector.squaredLength(p - pos) < d * d )) then
                        
                        let enemy = (Actor.Enemy.update model) enemy
                        let enemy, skill = Actor.Enemy.getSKill model.gameSetting enemy

                        // mutable
                        skill |> Option.iter(fun skill ->
                            skillEmits.Add(enemy, skill)
                        )

                        let pos = ObjectBase.position enemy
                        if (Actor.statusCurrent enemy).hp > 0.0f && not(pos.x = nanf || pos.y = nanf) then
                            yield (id, enemy)
                    elif not(pos.x = nanf || pos.y = nanf) then
                        yield (id, enemy)
            }
            |> Map.ofSeq

        let skillList =
            seq {
                for (enemy, skill) in skillEmits ->
                    SkillList.appendActorSkills enemy (skill model)
            }
            |> Seq.fold (|>) model.skillList


        { model with enemies = enemies; skillList = skillList }


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
            let m, ds =
                model
                |> updateEachPlayer Actor.Player.update
                |> updateEnemies
                |> SkillList.update

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
                | [||] -> Cmd.none
                | _ -> Cmd.port(ViewMsg.DamagesView ds)
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
            let size = model.gameSetting.characterSize
            let model =
                model
                |> updateEachPlayer (fun p ->
                    let pos = (dungeonParams.initPosition - (Vec2.init (float32 p.id.Value) 0.0f) * size)
                    
                    ObjectBase.map (ObjectBase.mapPosition <| fun _ -> pos) p
                )

            { model with
                mode = GameSceneMode.GameMode
                enemies =
                    Model.cellsToEnemies
                        model.gameSetting
                        dungeonParams.enemyCells
                        model.gameSetting.dungeonCellSize

                skillList = Model.Actor.Skill.SkillList.init

                dungeonBuilder = dungeonParams.dungeonBuilder
                dungeonModel = dungeonParams.dungeonModel
                dungeonGateCells = dungeonParams.gateCells
                dungeonFloor = model.dungeonFloor + 1u
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