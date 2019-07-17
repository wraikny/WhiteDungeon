namespace WhiteDungeon.Core.Model


//type ObjectStatus = {
//    hp : HP
//}

//module ObjectStatus =
//    let hp a = a.hp


type ActorStatus = {
    level : Level
    hp : HP
    walkSpeed : Speed
    dashSpeed : Speed
} with

    static member (+) (a, b) = {
        level = a.level + b.level
        hp = a.hp + b.hp
        walkSpeed = a.walkSpeed + b.walkSpeed
        dashSpeed = a.dashSpeed + b.dashSpeed
    }

    static member (*) (a, b) = {
        level = a.level * b.level
        hp = a.hp * b.hp
        walkSpeed = a.walkSpeed * b.walkSpeed
        dashSpeed = a.dashSpeed * b.dashSpeed
    }


module ActorStatus =
    let inline level a = a.level

    let inline hp a = a.hp

    let inline walkSpeed a = a.walkSpeed

    let inline dashSpeed a = a.dashSpeed

    let zero = {
        level = 0
        hp = 0.0f
        walkSpeed = 0.0f
        dashSpeed = 0.0f
    }

    let inline min a b = {
        level = min a.level b.level
        hp = min a.hp b.hp
        walkSpeed = min a.walkSpeed b.walkSpeed
        dashSpeed = min a.dashSpeed b.dashSpeed
    }

    let inline max a b = {
        level = max a.level b.level
        hp = max a.hp b.hp
        walkSpeed = max a.walkSpeed b.walkSpeed
        dashSpeed = max a.dashSpeed b.dashSpeed
    }
        


type Occupation =
    | Hunter


[<Struct>]
type CharacterID = CharacterID of int
    //with
    //member this.Value = this |> function | CharacterID x -> x


type Character = {
    id : CharacterID
    name : string
    currentOccupation : Occupation
    occupations : Map<Occupation, ActorStatus>
}


type EnemyKind =
    | Slime