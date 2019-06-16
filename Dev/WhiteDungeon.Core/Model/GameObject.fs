namespace WhiteDungeon.Core.Model


type ObjectStatus = {
    level : Level
    hp : HP
}

module ObjectStatus =
    let level a = a.level

    let hp a = a.hp


type ActorStatus = {
    level : Level
    hp : HP
    walkSpeed : Speed
    dashSpeed : Speed
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