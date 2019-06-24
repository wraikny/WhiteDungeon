﻿namespace WhiteDungeon.Core.Game.ViewModel

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
// open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Monad
open wraikny.Tart.Core.View
open WhiteDungeon.Core.Game.Model


type ObjectBaseView = {
    area : float32 Vec2 Rect
    direction : MoveDirection
}

module ObjectBaseView =
    let fromModel (objectBase : ObjectBase) = {
        area =
            objectBase
            |> Model.ObjectBase.area
        direction = objectBase.direction
    }


type ActorView = {
    objectBaseView : ObjectBaseView
}

module ActorView =
    let fromModel (actor : Actor.Actor) = {
        objectBaseView = actor.objectBase |> ObjectBaseView.fromModel
    }


type PlayerView = {
    character : Model.Character
    actorView : ActorView
}

module PlayerView =
    let fromModel (player : Actor.Player) = {
        character = player.character
        actorView = player.actor |> ActorView.fromModel
    }

    let playersView =
        Map.toList
        >> List.map(fun (id : PlayerID, player) ->
            (id.Value, fromModel player)
        )


type AreaSkillEmitView = {
   baseView : ObjectBaseView
   frameCurrent : uint32
   frameFirst : uint32
}

module AreaSkillEmitView =
    open WhiteDungeon.Core.Game.Model.Skill

    let fromModel (emit : Model.Skill.SkillEmit) =
        emit.skillEmitBase.target |> function
        | Skill.Friends { area = o }
        | Skill.Others { area = o }
        | Skill.Area { area = o } ->
            Some {
                baseView =
                    o |> ObjectBaseView.fromModel
                frameCurrent = emit.frame
                frameFirst = emit.frameFirst
            }
        | _ -> None
        |> Option.get


type CameraView = {
    position : float32 Vec2
}

module CameraView =
    let init position = {
        position = position
    }

    let fromPlayers (players : Map<Model.PlayerID, Model.Actor.Player>) =
        players
        |> Map.toList
        |> List.sortBy (fun (id, _) -> id.Value)
        |> List.map (fun (_, p) -> p.actor.objectBase.position)
        |> List.map init



type ViewModel = {
    camera : CameraView list
    players : UpdaterViewModel<PlayerView>
    toPlayersSkillEmits : UpdaterViewModel<AreaSkillEmitView>
    toEnemiesSkillEmits : UpdaterViewModel<AreaSkillEmitView>
    areaSkillEmits : UpdaterViewModel<AreaSkillEmitView>
}


module ViewModel =
    let selectPlayers (viewModel : ViewModel) : UpdaterViewModel<PlayerView> =
        viewModel.players

    let selectAreaSkillEmits (viewModel : ViewModel) : UpdaterViewModel<AreaSkillEmitView> =
        viewModel.areaSkillEmits

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

        toPlayersSkillEmits = {
            objects =
                model.skillList.playerEffects
                |> List.map (fun (id, e) -> (id, AreaSkillEmitView.fromModel e))
        }

        toEnemiesSkillEmits = {
            objects =
                model.skillList.enemyEffects
                |> List.map (fun (id, e) -> (id, AreaSkillEmitView.fromModel e))
        }

        areaSkillEmits = {
            objects =
                model.skillList.areaEffects
                |> List.map (fun (id, e) -> (id, AreaSkillEmitView.fromModel e))
        }
    }