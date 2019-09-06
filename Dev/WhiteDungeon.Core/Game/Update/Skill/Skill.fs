﻿namespace WhiteDungeon.Core.Game.Update.Skill

open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Update
open WhiteDungeon.Core.Game.Model.Actor
open WhiteDungeon.Core.Game.Model.Actor.Skill

open FSharpPlus
open wraikny.Tart.Helper.Extension

open System.Collections.Generic

module Effect =
    let apply
        (gameSetting : GameSetting)
        (invoker : Actor)
        (effect : Effect)
        (actor : Actor.Actor)
        : (Actor.Actor * SkillEmit [])
        =
        effect |> function
        | Damage v ->
            let damage =
                gameSetting.damageCalculation
                    v
                    invoker
                    actor

            Actor.Actor.addHP -damage actor, empty

        | AddHP v ->
            Actor.Actor.addHP v actor, empty

        | DamageF f ->
            let damage = f invoker actor
            Actor.Actor.addHP -damage actor, empty

        | F f -> f invoker actor


//module IDSkill =
//    let count (idSkill : 'ID IDSkill) : 'ID IDSkill option =
//        if idSkill.frame = 0u then
//            None
//        else
//            Some { idSkill with frame = idSkill.frame - 1u }


module AreaSkill =
    let move gameSetting dungeonModel (areaSkill : AreaSkill) : AreaSkill option =
        areaSkill.move |> function
        | [] -> None
        | x::xs ->
            let areaSkill =
                { areaSkill with
                    move = xs
                    emits = [||]
                    frame = areaSkill.frame - 1u
                }

            x |> function
            | Stay -> Some areaSkill
            | Move diff ->
                let nextAreaSkill, isCollided =
                    areaSkill
                    |> ObjectBase.moveXYTogether gameSetting dungeonModel diff

                if areaSkill.removeWhenHitWall && isCollided then
                    None
                else
                    Some nextAreaSkill

            | Scale diff ->
                let newAreaSkill = areaSkill |> ObjectBase.mapSize ((+) diff)

                let inline insideDungeon () =
                    ObjectBase.insideDungeon
                        gameSetting
                        dungeonModel
                        newAreaSkill

                if areaSkill.removeWhenHitWall && (not <| insideDungeon()) then
                    None
                else
                    Some newAreaSkill

            | Generate emits ->
                Some { areaSkill with emits = emits areaSkill }

    let inline isCollided (areaSkill : AreaSkill) (x) : bool =
        Rect2.isCollided
            (ObjectBase.area x)
            (ObjectBase.area areaSkill)

    let inline private apply (gameSetting) (areaSkill : AreaSkill) (actor : Actor.Actor) : Actor.Actor * SkillEmit [] =
        areaSkill.skillBase.effects
        |>> (Effect.apply gameSetting areaSkill.skillBase.invokerActor)
        |> fold (fun (a, s) f ->
            let a, s_ = f a
            (a, Array.append s s_)
        ) (actor, empty)


    let inline applyToActorHolders
        gameSetting
        (skills : Map<_, AreaSkill>)
        (holders : Map<'ID, ^a>) : Map<'ID, ^a> * SkillEmit [] =

        let holders = Map.toArray holders
        let skills = Map.toArray skills

        let resultHolders = List<'ID * ^a>(holders.Length)
        let resultEmits = List<_>()

        for (hId, x) in holders do
            let fs =
                skills
                |> Seq.map snd
                |> Seq.filter (flip isCollided x)
                |> Seq.map(apply gameSetting)

            let mutable actor = Actor.get x

            for f in fs do
                let a, es = f actor
                actor <- a
                for e in es do resultEmits.Add(e)

            resultHolders.Add(hId, Actor.Actor.set actor x)

        (Map.ofSeq resultHolders), resultEmits.ToArray()


    let inline hitActorsFilter (areaSkill : AreaSkill) : AreaSkill option =
        if areaSkill.removeWhenHitActor && (areaSkill.emits |> Array.isEmpty |> not) then
            None
        else
            Some areaSkill


module SkillEmit =
    let decrDelay (emit : SkillEmit) : SkillEmit =
        emit |> function
        | Area area ->
            Area { area with skillBase = { area.skillBase with delay = area.skillBase.delay - 1u } }



module SkillList =
    let append skills skillList =
        let skillsCount = skills |> length
        {
            skillList with
                nextID = skillList.nextID + uint32 skillsCount
                waitings =
                    skillList.waitings
                    |> Map.toSeq
                    |> (<|>) (
                        skills
                        |> mapi (fun i x -> (skillList.nextID + uint32 i, x))
                    )
                    |> Map.ofSeq
        }

    let inline mapArea f skillList =
        { skillList with
            areaPlayer = f <!> skillList.areaPlayer
            areaEnemy = f <!> skillList.areaEnemy
            areaAll = f <!> skillList.areaAll
        }

    let inline filterMapArea f skillList =
        let g =
            Map.toSeq >> filterMap(fun (id, x) -> monad{
                let! x = f x
                yield (id, x)
            }) >> Map.ofSeq

        { skillList with
            areaPlayer = g skillList.areaPlayer
            areaEnemy = g skillList.areaEnemy
            areaAll = g  skillList.areaAll
        }

    open System.Collections.Generic

    let private popWaitingSkills (skillList : SkillList) : SkillList =
        let waitings = new List<SkillID * SkillEmit>()
        let areaPlayer = new List<SkillID * AreaSkill>()
        let areaEnemy = new List<SkillID * AreaSkill>()
        let areaAll = new List<SkillID * AreaSkill>()

        for (id, emit) in skillList.waitings |> Map.toSeq do
            if emit |> SkillEmit.delay = 0u then
                emit |> function
                | Area area ->
                    area.target |> function
                    | Players ->
                        areaPlayer.Add(id, area)
                    | Enemies ->
                        areaEnemy.Add(id, area)
                    | All ->
                        areaAll.Add(id, area)
            else
                waitings.Add(id, emit |> SkillEmit.decrDelay)

        let append a b =
            seq {
                for x in a -> x
                for x in b |> Map.toSeq -> x
            } |> Map.ofSeq

        {
            nextID = skillList.nextID
            waitings = waitings |> Map.ofSeq
            areaPlayer = append areaPlayer skillList.areaPlayer
            areaEnemy = append areaEnemy skillList.areaEnemy
            areaAll = append areaAll skillList.areaAll
        }

    let private appendGeneratedEmits skillList : SkillList =
        let f : Map<_, AreaSkill> -> _ =
            Map.toSeq
            >> map(snd >> fun areaSkill ->
                areaSkill.emits
                |>> (AreaSkillBuilder.build areaSkill.skillBase.invokerActor >> Skill.Area)
            )
            >> Seq.concat

        skillList
        |> append (f skillList.areaPlayer)
        |> append (f skillList.areaEnemy)
        |> append (f skillList.areaAll)

    let update (model : Model) =
        SkillList.map(
            popWaitingSkills
            >> filterMapArea (AreaSkill.hitActorsFilter)
            >> filterMapArea (AreaSkill.move model.gameSetting model.dungeonModel)
            //>> checkCollision model.players model.enemies
            >> appendGeneratedEmits
        ) model