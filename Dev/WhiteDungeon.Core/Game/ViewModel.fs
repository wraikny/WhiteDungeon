namespace WhiteDungeon.Core.Game.ViewModel

open WhiteDungeon.Core.Game
// open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Core.View


type ObjectBaseView = {
    position : float32 Vec2
}

module ObjectBaseView =
    let fromModel (objectBase : Model.ObjectBase) = {
        position =
            objectBase
            |> Model.ObjectBase.area
            |> Rect.position
    }


type ActorView = {
    objectBase : ObjectBaseView
}

module ActorView =
    let fromModel (actor : Model.Actor.Actor) = {
        objectBase = actor.objectBase |> ObjectBaseView.fromModel
    }


type PlayerView = {
    actorView : ActorView
}

module PlayerView =
    let fromModel (player : Model.Actor.Player) = {
        actorView = player.actor |> ActorView.fromModel
    }

    let playersView =
        List.map(fun (id : Model.Actor.PlayerID, player) ->
            (id.Value, fromModel player)
        )
        >> Map.ofList


type CameraView = {
    position : float32 Vec2
}

module CameraView =
    let init position = {
        position = position
    }

    let fromPlayers (players : (Model.Actor.PlayerID * Model.Actor.Player) list) =
        players
        |> List.sortBy (fun (id, _) -> id.Value)
        |> List.map (fun (_, p) -> p.actor.objectBase.position)
        |> List.map init



type ViewModel = {
    camera : CameraView list
    players : UpdaterViewModel<PlayerView>
}


open WhiteDungeon.Core.Game.Model

module ViewModel =
    let view (model : Model) : ViewModel = {
        camera =
            model.players
            |> CameraView.fromPlayers

        players = {
            nextID = model.nextPlayerID
            objects =
                model
                |> Model.players
                |> PlayerView.playersView
        }
    }