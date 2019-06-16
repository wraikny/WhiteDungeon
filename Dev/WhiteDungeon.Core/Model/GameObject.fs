namespace WhiteDungeon.Core.Model


type Level = int


type ObjectStatus = {
    level : Level
    hp : float32
}

module ObjectStatus =
    let level a = a.level

    let hp a = a.hp


type ActorStatus = {
    walkSpeed : float32
    dashSpeed : float32
}

module ActorStatus =
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
    occupations : Map<Occupation, ObjectStatus * ActorStatus>
}