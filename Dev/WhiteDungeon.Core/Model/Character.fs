namespace WhiteDungeon.Core.Model


type Level = int


type ActorStatus = {
    level : Level
    hp : int
    walkSpeed : float32
    dashSpeed : float32
}

module ActorStatus =
    let hp a = a.hp

    let walkSpeed a = a.walkSpeed

    let dashSpeed a = a.dashSpeed


[<Struct>]
type Occupation =
    | Sword
    | Magic


type Character = {
    name : string
    status : ActorStatus
    occupation : Occupation
    occupations : Map<Occupation, Level>
}