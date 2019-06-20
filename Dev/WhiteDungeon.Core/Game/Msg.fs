namespace WhiteDungeon.Core.Game.Msg

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model


[<Struct>]
type ActorMove =
    | Walk
    | Dash


[<Struct>]
type InputDirection =
    | Right
    | Left
    | Up
    | Down



type PlayerInput =
    | RightKey
    | LeftKey
    | UpKey
    | DownKey
    | DashKey


module PlayerInput =
    let inputs =
        [
            RightKey
            LeftKey
            UpKey
            DownKey
            DashKey
        ]

    let getPlayerMoveFromInputs (inputSet : PlayerInput Set) : ActorMove * float32 Vec2 =
        let actorMove =
            if inputSet |> Set.contains PlayerInput.DashKey then Dash else Walk

        let moveDirs = [|
            UpKey, Vec2.init(0.0f, -1.0f)
            DownKey, Vec2.init(0.0f, 1.0f)
            RightKey, Vec2.init(1.0f, 0.0f)
            LeftKey, Vec2.init(-1.0f, 0.0f)
        |]

        let direction =
            moveDirs
            |> Array.filter(fun (key, _) ->
                inputSet |> Set.contains key
            )
            |> Array.map snd
            |> Array.fold (+) (Vec2.zero())
            |> VectorClass.normalize
    
        actorMove, direction



type Msg =
    | TimePasses
    | PlayerInputs of PlayerID * PlayerInput Set
    /// for Debug
    | AppendSkillEmits