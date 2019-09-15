namespace WhiteDungeon.Core.Model


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
        

type Occupation = string


[<Struct>]
type CharacterID = CharacterID of int


type Character = {
    id : CharacterID
    name : string
    currentOccupation : Occupation
    //occupations : Map<Occupation, ActorStatus>
}

type EnemyKind = string
