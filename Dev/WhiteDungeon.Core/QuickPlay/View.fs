namespace WhiteDungeon.Core.QuickPlay

open wraikny.Tart.Core

open WhiteDungeon.Core
open WhiteDungeon.Core.QuickPlay

open FSharpPlus


type ViewModel = {
    playerCount : int
    characters : (uint32 * string * Model.Occupation option) list
}

module ViewModel =
    let view (model : Model) : ViewModel = {
        playerCount = model.playerCount
        characters =
            model.players
            |> Map.toList
            |>> (fun (id, (name, occ)) -> id, name, occ)
    }