module WhiteDungeon.Core.Update.Actor

open WhiteDungeon.Core
open WhiteDungeon.Core.Model

open FSharpPlus

let inline updateActorStatusCurrent f actor : Actor =
    { actor with statusCurrent = f actor.statusCurrent }


let addHP diff (actor : Actor) =
    let hp =
        (actor.statusCurrent.hp + diff)
        |> max 0.0f
        |> min actor.statusDefault.hp

    actor
    |> Actor.mapStatus (fun status ->
        { status with hp = hp }
    )


let inline levelUp newLevel status (x : ^a) =
    x |> Actor.map(fun actor ->
        { actor with
            level = newLevel
            statusDefault = status
            statusCurrent = { status with hp = actor.HPRate() * status.hp }
        }
    )
    

open Affogato

let move (gameSetting) (dungeonModel) (move : ActorMove) (direction : float32 Vector2) (actor : Actor) : Actor =
    let speed = move |> function
        | ActorMove.Walk -> actor.statusCurrent.walkSpeed
        | ActorMove.Dash -> actor.statusCurrent.dashSpeed

    let direction = direction |> Vector.normalize

    actor
    |> fun x -> { x with currentMove = move }
    |> Update.ObjectBase.moveXYAnother
            gameSetting
            dungeonModel
            (speed *. direction)
    |> fst

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

open WhiteDungeon.Core.Update

let inline update (actor : ^a) =
    actor
    |> ObjectBase.update
    //|> updateConditions