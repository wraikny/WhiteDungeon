﻿module WhiteDungeon.Core.Game.Update.Actor.Actor

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model.Actor

open FSharpPlus

let inline setObjectBase (objectBase : Model.ObjectBase) (actor : Actor) =
    { actor with objectBase = objectBase }


let inline updateObjectBase f (actor : Actor) =
    actor
    |> setObjectBase (f actor.objectBase)


let inline setActorStatusCurrent status actor : Actor =
    { actor with statusCurrent = status }

let inline updateActorStatusCurrent f actor : Actor =
    { actor with statusCurrent = f actor.statusCurrent }


let addHP damage (actor : Actor) =
    let hp =
        (actor.statusCurrent.hp + damage)
        |> max 0.0f
        |> min actor.statusDefault.hp

    actor
    |> updateActorStatusCurrent(fun status ->
        { status with hp = hp }
    )


open WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math


let move (gameSetting) (dungeonModel) (move : ActorMove) (direction : float32 Vec2) (actor : Actor) : Actor =
    let speed = move |> function
        | Walk -> actor.statusCurrent.walkSpeed
        | Dash -> actor.statusCurrent.dashSpeed

    let direction = direction |> Vector.normalize

    actor
    |> updateObjectBase(
        Update.ObjectBase.moveXYAnother
            gameSetting
            dungeonModel
            (speed *. direction)
        >> fst
    )

//let calcStatusCurrent (actor : Actor) =
//    let rec applyCorrection corrections result =
//        corrections |> function
//        | [] -> result
//        | (c : Model.Skill.Condition)::cs ->
//            let result = c.kind |> function
//                | Model.Skill.StatusAdd s ->
//                    result + s
//                | Model.Skill.StatusMul s ->
//                    result * s

//            applyCorrection cs result


//    { actor with
//        statusCurrent =
//            applyCorrection actor.conditions actor.statusDefault
//            |> ActorStatus.max ( ActorStatus.zero() )
//    }


//let setConditions conditions (actor : Actor) =
//    { actor with conditions = conditions }


//let appendConditions (conditions : Model.Skill.Condition list) (actor : Actor) : Actor =
//    let conditions =
//        conditions
//        |> List.filter(fun c -> c.frame > 0u)

//    if conditions |> List.length > 0 then
//        let conditions =
//            actor.conditions
//            |> List.append conditions
//            |> List.sortBy (fun c -> c.priority)

//        actor
//        |> setConditions conditions
//        |> calcStatusCurrent

//    else
//        actor


//let updateConditions (actor : Actor) : Actor =
//    let rec search cs result recalc =
//        cs |> function
//        | [] -> result, recalc
//        | (c : Model.Skill.Condition)::cs ->
//            if c.frame = 0u then
//                search cs result true
//            else
//                search cs ({c with frame = c.frame - 1u}::result) recalc

//    let conditions, recalc = search actor.conditions [] false

//    if recalc then
//        actor
//        |> setConditions conditions
//        |> calcStatusCurrent
//    else
//        actor
//        |> setConditions conditions

open WhiteDungeon.Core.Game.Update

let inline update actor : Actor =
    actor
    |> updateObjectBase ObjectBase.update
    //|> updateConditions