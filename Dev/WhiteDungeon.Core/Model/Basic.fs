namespace WhiteDungeon.Core.Model

type Occupation = string
type EnemyKind = string
type SkillID = uint32

[<Struct>]
type CharacterID = CharacterID of int


[<Struct>]
type PlayerID = PlayerID of id : uint32 with
    member inline this.Value = this |> function | PlayerID x -> x

[<Struct>]
type EnemyID = EnemyID of id : uint32 with
    member inline this.Value = this |> function | EnemyID x -> x


type ActorID =
    | OfPlayerID of PlayerID
    | OfEnemyID of EnemyID
with
    member x.GetPlayerId =
        x |> function
            | OfPlayerID id -> ValueSome id
            | _ -> ValueNone
        |> fun x -> ValueOption.get x
    member x.GetEnemyId =
        x |> function
            | OfEnemyID id -> ValueSome id
            | _ -> ValueNone
        |> fun x -> ValueOption.get x


[<Struct>]
type ActorMove =
    | Walk
    | Dash


type ActorStatus = {
    hp : float32
    atk : float32
    def : float32

    walkSpeed : float32
    dashSpeed : float32
} with
    static member Zero = {
        hp = 0.0f
        atk = 0.0f
        def = 0.0f
        walkSpeed = 0.0f
        dashSpeed = 0.0f
    }

    static member Map2 (f, a, b) = {
        hp = f a.hp b.hp
        atk = f a.atk b.atk
        def = f a.def b.def
        walkSpeed = f a.walkSpeed b.walkSpeed
        dashSpeed = f a.dashSpeed b.dashSpeed
    }

    static member (+) (a, b) = ActorStatus.Map2 ((+), a, b)

    static member (*) (a, b) = ActorStatus.Map2 (( * ), a, b)


module ActorStatus =
    let inline hp a = a.hp

    let inline walkSpeed a = a.walkSpeed

    let inline dashSpeed a = a.dashSpeed

    let inline min a b = ActorStatus.Map2 (min, a, b)

    let inline max a b = ActorStatus.Map2 (max, a, b)


open wraikny.Tart.Helper.Math
open FSharpPlus


[<Struct>]
type MoveDirection =
    | Front
    | Back
    | Right
    | Left
    | FrontRight
    | FrontLeft
    | BackRight
    | BackLeft


module MoveDirection =
    let fromVector v =
        let pi2 = 2.0f * Pi
        let angle = (pi2 + Vec2.angle v) % pi2
        let a = angle * 8.0f / Pi
        let bw s t = s <= a && a < t
        let result =
            if bw 1.0f 3.0f then
                FrontRight
            elif bw 3.0f 5.0f then
                Front
            elif bw 5.0f 7.0f then
                FrontLeft
            elif bw 7.0f 9.0f then
                Left
            elif bw 9.0f 11.0f then
                BackLeft
            elif bw 11.0f 13.0f then
                Back
            elif bw 13.0f 15.0f then
                BackRight
            else
                Right
        
        // printfn "%A %A %A" v angle result

        result


    let toVector dir =
        dir |> function
        | Front -> (0, 1)
        | Back -> (0, -1)
        | Right -> (1, 0)
        | Left -> (-1, 0)
        | FrontRight -> (1, 1)
        | FrontLeft -> (-1, 1)
        | BackRight -> (1, -1)
        | BackLeft -> (-1, -1)
        |> uncurry Vec2.init
        |>> float32
        |> Vector.normalize