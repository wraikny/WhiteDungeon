namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Skill

open WhiteDungeon.Core.Game.Update

open FSharpPlus
open FSharpPlus.Math.Applicative

module Update =
    let inline incrCount (model : Model) : Model =
        { model with count = model.count + 1u }

    let inline setPlayers players model =
        { model with players = players }

    let inline updatePlayers f (model : Model) : Model =
        { model with players = f model.players }


    let inline updateEachPlayer f (model : Model) : Model =
        model
        |> setPlayers(f <!> model.players)


    let updatePlayerOf id f (model : Model) =
        model.players
        |> Map.tryFind id
        |>> (fun player ->
            model
            |> updatePlayers (Map.add id (f player))
        )
        |> Option.defaultValue model

    let inline updateEachEnemy f (model : Model) : Model = {
        model with
            enemies = model.enemies |>> f
    }

    open wraikny.Tart.Helper.Math
    open WhiteDungeon.Core.Game.Msg

    let inline updateSkillList f (model : Model) : Model =
        { model with skillList = f model.skillList }


    let appendSkills (skills : seq<Actor.Actor * Skill.SkillEmitBuilder>) (model : Model) : Model =
        model
        |> updateSkillList (
            skills
            |>> fun (a, b) -> Skill.SkillEmitBuilder.build a b
            |> Skill.SkillList.append
        )


    let applySkills (model : Model) : Model =
        let gameSetting = model.gameSetting
        let skillList = model.skillList

        { model with
            players =
                let f =
                    Skill.AreaSkill.applyToActorHolders
                        gameSetting
                        Actor.Player.updateActor

                model.players
                |> f skillList.areaPlayer
                |> f skillList.areaAll

            enemies =
                let f =
                    Skill.AreaSkill.applyToActorHolders
                        gameSetting
                        Actor.Enemy.updateActor

                model.enemies
                |> f skillList.areaEnemy
                |> f skillList.areaAll
        }

    let update (msg : Msg.Msg) (model : Model) : Model * Cmd<Msg.Msg, ViewMsg.ViewMsg> =
        let model = { model with timePassed = false }

        msg |> function
        | SetGameMode mode ->
            { model with mode = mode }, Cmd.none
        | TimePasses ->
            let model =
                model
                |> updateEachPlayer Actor.Player.update
                |> updateEachEnemy Actor.Enemy.update
            let model =
                model
                |> updateSkillList (Skill.SkillList.update model)
                |> applySkills
                |> fun m -> { m with timePassed = true }
                |> fun m ->
                    m.players
                    |> Map.toSeq
                    |>> snd
                    |> exists(fun (x : Actor.Player) -> x.actor.statusCurrent.hp > 0.0f)
                    |> function
                    | false -> { m with mode = GameFinished false }
                    | true ->
                        m.players
                        |> Map.toSeq
                        |>> ( snd >> (fun p -> p.actor.objectBase) )
                        |> exists(ObjectBase.collidedCells model.gameSetting model.dungeonGateCells)
                        |> function
                        | true ->
                            if m.lastCollidedGate then
                                { m with lastCollidedGate = true }
                            else
                                { m with mode = Stair; lastCollidedGate = true}
                        | false ->
                            { m with lastCollidedGate = false }

            model, Cmd.none

        | PlayerInputs (id, inputSet) ->
            let move, direction = Msg.PlayerInput.getPlayerMoveFromInputs inputSet
            
            let model =
                if direction <> zero then
                    model
                    |> updatePlayerOf id (
                        Update.Actor.Player.updateActor <|
                            Update.Actor.Actor.move
                                model.gameSetting
                                model.dungeonModel
                                move
                                direction
                    )
                else
                    model


            model, Cmd.none

        | GenerateNewDungeon ->
            Dungeon.generateTask model.gameSetting model.dungeonBuilder (length model.dungeonGateCells)
            |> TartTask.perform (fun _ -> GenerateNewDungeon) GeneratedDungeon
            |> fun cmd ->
                { model with mode = GameSceneMode.WaitingGenerating }, cmd

        | GeneratedDungeon dungeonParams ->
            let size = model.gameSetting.characterSize
            let model =
                model
                |> updateEachPlayer (fun p ->
                    let pos = (dungeonParams.initPosition - (Vec2.init (float32 p.id.Value) 0.0f) * size)
                    
                    Actor.Player.updateObjectBase (ObjectBase.setPosition pos) p
                )

            { model with
                mode = GameSceneMode.GameMode
                dungeonBuilder = dungeonParams.dungeonBuilder
                dungeonModel = dungeonParams.dungeonModel
                dungeonGateCells = dungeonParams.gateCells
            }
            , Cmd.port ( ViewMsg.UpdateDungeonView(dungeonParams.dungeonModel, dungeonParams.gateCells) )


        #if DEBUG

        | AppendSkillEmits ->
            let id = PlayerID 0u
            let player0 = model.players |> Map.find id
            let dir = 
                player0.actor.objectBase.direction
                |> Model.MoveDirection.toVector

            let pos =
                player0.actor.objectBase.position
                + (100.0f *. dir)

            let emit : Skill.SkillEmitBuilder =
                {
                    skillBase =
                        {
                            delay = 0u
                            effects = [|
                                Skill.Damage(fun gs atk def ->
                                    0.0f
                                )
                            |]
                        }
                    objectBase = ObjectBase.init (one .* 100.0f) pos

                    target = Skill.AreaTarget.Enemies

                    removeWhenHitWall = true
                    removeWhenHitActor = true

                    move = seq {
                        for _ in 1..10 -> Skill.Stay
                        for _ in 1..60 -> Skill.Move(dir .* 5.0f)
                        for _ in 1..60 -> Skill.Scale(one .* 5.0f)
                    } |> toList
                } |> Skill.AreaBuilder

            let emits = [player0.actor, emit]

            model |> appendSkills emits, Cmd.none

        #endif