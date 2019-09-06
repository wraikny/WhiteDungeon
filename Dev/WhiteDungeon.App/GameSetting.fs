module WhiteDungeon.App.GameSetting

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.UI
open WhiteDungeon
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Game.Model.Skill

open FSharpPlus.Math.Applicative
open FSharpPlus

let gameSetting : Model.GameSetting = {
    Model.GameSetting.dungeonCellSize = Vec2.init 250.0f 250.0f
    minPlayerCount = 1
    maxPlayerCount = 1
    binarySearchCountMovingOnWall = 4
    characterSize = Vec2.init 100.0f 100.0f
    damageCalculation = fun v invoker target ->
        v * float32 invoker.statusCurrent.level / float32 target.statusCurrent.level

    occupationSettings = Map.ofList [
        Model.Seeker, {
            status =
                {
                    Model.ActorStatus.level = 1
                    hp = 100.0f
                    walkSpeed = 12.0f
                    dashSpeed = 18.0f
                }
            skill1 = fun model actor ->
                let dir = 
                    actor.objectBase.direction
                    |> Model.MoveDirection.toVector

                let pos = actor.objectBase.position + (100.0f *. dir)

                [
                    Skill.AreaBuilder {
                        skillBase =
                            {
                                delay = 0u
                                effects = [|
                                    Skill.Damage 10.0f
                                |]
                            }
                        objectBase = ObjectBase.init (one .* 100.0f) pos

                        target = Skill.AreaTarget.Enemies

                        removeWhenHitWall = true
                        removeWhenHitActor = true

                        move = seq {
                            //for _ in 1..10 -> Skill.Stay
                            for _ in 1..60 -> Skill.Move (dir .* 10.0f)
                            for _ in 1..60 -> Skill.Scale(one .* 10.0f)
                        } |> toList
                    }
                ]
            skill2 = fun model actor ->
                let dir = 
                    actor.objectBase.direction
                    |> Model.MoveDirection.toVector

                let verticalDir =
                    Vec2.init -dir.y dir.x

                let pos = actor.objectBase.position + (100.0f *. dir)

                let f dir =
                    let dir = Vector.normalize dir
                    Skill.AreaBuilder {
                        skillBase =
                            {
                                delay = 0u
                                effects = [|
                                    Skill.Damage 10.0f
                                |]
                            }
                        objectBase = ObjectBase.init (one .* 100.0f) pos

                        target = Skill.AreaTarget.Enemies

                        removeWhenHitWall = true
                        removeWhenHitActor = true

                        move = seq {
                            //for _ in 1..10 -> Skill.Stay
                            for _ in 1..60 -> Skill.Move (dir .* 10.0f)
                            for _ in 1..60 -> Skill.Scale(one .* 10.0f)
                        } |> toList
                    }

                [
                    f dir
                    f (dir + verticalDir .* 0.3f)
                    f (dir - verticalDir .* 0.3f)
                ]

            skill1CoolTime = 20us
            skill2CoolTime = 60us
        }

        Model.Bushi, Character.Bushi.setting
    ]
}