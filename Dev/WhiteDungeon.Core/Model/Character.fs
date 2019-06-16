namespace WhiteDungeon.Core.Model


type Level = int


type ActorStatus = {
    level : Level
    hp : float32
    walkSpeed : float32
    dashSpeed : float32
}

module ActorStatus =
    let level a = a.level

    let hp a = a.hp

    let walkSpeed a = a.walkSpeed

    let dashSpeed a = a.dashSpeed


[<Struct>]
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