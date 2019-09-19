module WhiteDungeon.App.Character.Bushi

open wraikny.Tart.Helper
open wraikny.Tart.Helper.Math


open WhiteDungeon.Core
open WhiteDungeon.Core.Model
open WhiteDungeon.View

open FSharpPlus
open FSharpPlus.Math.Applicative

let setting : OccupationSetting = {
    status =
        {
            hp = 100.0f
            atk = 50.0f
            def = 50.0f
            walkSpeed = 12.0f
            dashSpeed = 18.0f
        }

    size = one .* 128.0f

    growthEasing = Easing.Linear

    skill1 = fun model player ->
        let actor = player.actor
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
            AreaBuilder {
                skillBase =
                    {
                        delay = delay
                        effects = [|
                            Damage (damage)
                        |]
                    }
                objectBase =
                    ObjectBase.init
                        (one .* size)
                        (targetPos + diffPos + (float32 delay *. actorSpeed) )

                target = AreaTarget.Enemies

                removeWhenHitWall = false
                removeWhenHitActor = true

                move = seq {
                    //for _ in 1..20 -> Skill.Move (dir .* 3.0f)
                    for _ in 1..20 -> Scale(one .* 1.0f)
                    //for _ in 1..20 -> Skill.Stay
                } |> toList
            }

        let posDiff = 70.0f
        let size1, size2 = 100.0f, 120.0f
        let damage1, damage2 = 7.5f, 10.0f
        let delay = 5u

        [
            createSkill 0u (attackDir .* -posDiff) size1 damage1
            createSkill delay zero size2 damage2
            createSkill (delay * 2u) (attackDir .* posDiff) size1 damage1
        ]
    skill2 = fun model player ->
        let actor = player.actor
        let dir = 
            actor.objectBase.direction
            |> Model.MoveDirection.toVector

        let verticalDir =
            Vec2.init -dir.y dir.x

        let pos = actor.objectBase.position + (100.0f *. dir)

        let f dir =
            let dir = Vector.normalize dir
            AreaBuilder {
                skillBase =
                    {
                        delay = 0u
                        effects = [|
                            Damage 5.0f
                        |]
                    }
                objectBase = ObjectBase.init (one .* 100.0f) pos

                target = AreaTarget.Enemies

                removeWhenHitWall = true
                removeWhenHitActor = true

                move = seq {
                    //for _ in 1..10 -> Skill.Stay
                    for _ in 1..60 -> Move (dir .* 10.0f)
                    for _ in 1..60 -> Scale(one .* 10.0f)
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

let private images = ActorImages.occupationImage 8u 4u "Image/Game/character/bushi.png"

let viewSetting =
    {
        name = "武士"
        characterImages = images
    }