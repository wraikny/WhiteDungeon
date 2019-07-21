﻿namespace WhiteDungeon.Core.Game.ViewModel

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
// open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core.View
open WhiteDungeon.Core.Game.Model

open FSharpPlus


type ObjectBaseView = {
    area : float32 Vec2 Rect
    direction : MoveDirection
}

module ObjectBaseView =
    let inline fromModel (objectBase : ObjectBase) = {
        area =
            objectBase
            |> Model.ObjectBase.area
        direction = objectBase.direction
    }


type ActorView = {
    objectBaseView : ObjectBaseView
}

module ActorView =
    let inline fromModel (actor : Actor.Actor) = {
        objectBaseView = actor.objectBase |> ObjectBaseView.fromModel
    }


type PlayerView = {
    character : Model.Character
    actorView : ActorView
}

module PlayerView =
    let inline fromModel (player : Actor.Player) = {
        character = player.character
        actorView = player.actor |> ActorView.fromModel
    }

    let playersView =
        Map.toList
        >> map(fun (id : PlayerID, player) ->
            (id.Value, fromModel player)
        )


type AreaSkillEmitView = {
   baseView : ObjectBaseView
   frameCurrent : uint32
   frameFirst : uint32
}

module AreaSkillEmitView =
    open WhiteDungeon.Core.Game.Model.Skill

    let inline fromModel (areaSkill : Model.Skill.AreaSkill) =
        {
            baseView = ObjectBaseView.fromModel areaSkill.objectBase
            frameCurrent = areaSkill.frame
            frameFirst = areaSkill.frameFirst
        }

    let fromModels : Map<uint32, _> -> (uint32 * AreaSkillEmitView) list =
        Map.toList
        >> map (fun (id, a) -> (id, fromModel a))


type CameraView = {
    position : float32 Vec2
}

module CameraView =
    let inline init position = {
        position = position
    }

    let inline fromPlayers (players : Map<Model.PlayerID, Model.Actor.Player>) =
        players
        |> Map.toList
        |> sortBy (fun (id, _) -> id.Value)
        |>> ((fun (_, p) -> p.actor.objectBase.position) >> init)



type ViewModel = {
    camera : CameraView list
    players : UpdaterViewModel<PlayerView>
    areaPlayer : UpdaterViewModel<AreaSkillEmitView>
    areaEnemy : UpdaterViewModel<AreaSkillEmitView>
    areaAll : UpdaterViewModel<AreaSkillEmitView>
}


module ViewModel =
    let inline selectPlayers (viewModel : ViewModel) : UpdaterViewModel<PlayerView> =
        viewModel.players

    let view (model : Model) : ViewModel = {
        camera =
            model.players
            |> CameraView.fromPlayers

        players = {
            objects =
                model
                |> Model.players
                |> PlayerView.playersView
        }

        areaPlayer = {
            objects =
                model.skillList.areaPlayer
                |> AreaSkillEmitView.fromModels
        }

        areaEnemy = {
            objects =
                model.skillList.areaEnemy
                |> AreaSkillEmitView.fromModels
        }

        areaAll = {
            objects =
                model.skillList.areaAll
                |> AreaSkillEmitView.fromModels
        }
    }