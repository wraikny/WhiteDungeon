module WhiteDungeon.App.Character.Bushi

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor

open WhiteDungeon.Core.Game.Model.Actor.Skill
open WhiteDungeon.View

open FSharpPlus
open FSharpPlus.Math.Applicative

let setting : OccupationSetting = {
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

        let verticalDir = Vec2.init -dir.y dir.x

        let attackDir = Vector.normalize(dir + verticalDir)

        let area = actor.objectBase |> ObjectBase.area
        let centerPos = area |> Rect.centerPosition
        let topPos = Vec2.init centerPos.x area.position.y

        let targetPos = topPos + dir .* 150.0f

        //let size = 100.0f

        let actorSpeed =
            (actor.objectBase.position - actor.objectBase.lastPosition)

        let createSkill delay diffPos size damage =
            Skill.AreaBuilder {
                skillBase =
                    {
                        delay = delay
                        effects = [|
                            Skill.Damage (damage)
                        |]
                    }
                objectBase =
                    ObjectBase.init
                        (one .* size)
                        (targetPos + diffPos + (float32 delay *. actorSpeed) )

                target = Skill.AreaTarget.Enemies

                removeWhenHitWall = false
                removeWhenHitActor = true

                move = seq {
                    //for _ in 1..20 -> Skill.Move (dir .* 3.0f)
                    for _ in 1..20 -> Skill.Scale(one .* 1.0f)
                    //for _ in 1..20 -> Skill.Stay
                } |> toList
            }

        let posDiff = 70.0f
        let size1, size2 = 100.0f, 120.0f
        let damage1, damage2 = 10.0f, 20.0f
        let delay = 5u

        [
            createSkill 0u (attackDir .* -posDiff) size1 damage1
            createSkill delay zero size2 damage2
            createSkill (delay * 2u) (attackDir .* posDiff) size1 damage1
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

let private images = ActorImages.fromGraphicmaker 8u 4u "Image/Game/Occupation/hunter.png"

let viewSetting =
    {
        name = "武士"
        characterImages = images
    }