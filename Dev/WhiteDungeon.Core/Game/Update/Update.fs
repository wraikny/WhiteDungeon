namespace WhiteDungeon.Core.Game.Update

open wraikny.Tart.Core

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
            enemies =
                model.enemies
                |>> f
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
        | StartGame ->
            { model with uiMode = GameMode }, Cmd.none
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

            //let emit : Skill.Skill.EmitBase = {
            //    invokerActor = player0.actor
            //    target = Skill.Skill.Target.Area {
            //            removeWhenHitActor = false
            //            removeWhenHitWall = true
            //            area = ObjectBase.init (Vec2.init(100.0f, 100.0f)) pos
            //            move = seq {
            //                for _ in 1..10 -> Skill.Skill.Stay
            //                for _ in 1..60 -> Skill.Skill.Move(dir *. 5.0f)
            //                for _ in 1..60 -> Skill.Skill.Scale(Vec2.init(3.0f, 3.0f))
            //            } |> Seq.toList
            //            emits = [||]
            //            collidedActors = Set.empty
            //        }
                
                
            //    delay = 0u
            //    effects = [|
            //        Skill.Damage(fun gs atk def ->
            //            0.0f
            //        )
            //    |]
            //}
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