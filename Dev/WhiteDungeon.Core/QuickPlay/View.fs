namespace WhiteDungeon.Core.QuickPlay

open wraikny.Tart.Core

open WhiteDungeon.Core
open WhiteDungeon.Core.QuickPlay


type ViewModel = {
    playerCount : int
    characters : (uint32 * Model.Character option) list
}

open wraikny.Tart.Helper.Monad

module ViewModel =
    let view (model : Model) : ViewModel = {
        playerCount = model.playerCount
        characters =
            model.players
            |> Map.toList
            |> List.map(fun (id, charaID) ->
                id, maybe {
                    let! id = charaID
                    return!
                        model.savedData.charactersList
                        |> Map.tryFind id
                }
            )
    }