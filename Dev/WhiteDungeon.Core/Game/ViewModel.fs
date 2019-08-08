namespace WhiteDungeon.Core.Game.ViewModel

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
    timePassed : bool
    area : float32 Vec2 Rect
    direction : MoveDirection
}

module ObjectBaseView =
    let inline fromModel timePassed (objectBase : ObjectBase) = {
        timePassed = timePassed
        area =
            objectBase
            |> Model.ObjectBase.area
        direction = objectBase.direction
    }


type ActorView = {
    objectBaseView : ObjectBaseView
}

module ActorView =
    let inline fromModel timePassed (actor : Actor.Actor) = {
        objectBaseView = actor.objectBase |> ObjectBaseView.fromModel timePassed
    }


type PlayerView = {
    character : Model.Character
    actorView : ActorView
}

module PlayerView =
    let inline fromModel timePassed (player : Actor.Player) = {
        character = player.character
        actorView = player.actor |> ActorView.fromModel timePassed
    }

    let playersView timePassed =
        Map.toList
        >> map(fun (id : PlayerID, player) ->
            (id.Value, fromModel timePassed player)
        )


type AreaSkillEmitView = {
   baseView : ObjectBaseView
   frameCurrent : uint32
   frameFirst : uint32
}

module AreaSkillEmitView =
    open WhiteDungeon.Core.Game.Model.Skill

    let inline fromModel timePassed (areaSkill : Model.Skill.AreaSkill) =
        {
            baseView = ObjectBaseView.fromModel timePassed areaSkill.objectBase
            frameCurrent = areaSkill.frame
            frameFirst = areaSkill.frameFirst
        }

    let fromModels timePassed : Map<uint32, _> -> (uint32 * AreaSkillEmitView) list =
        Map.toList
        >> map (fun (id, a) -> (id, fromModel timePassed a))


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
    let inline getCameras v = v.camera
    let inline getPlayers v = v.players
    let inline getSkillAreaPlayer v = v.areaPlayer
    let inline getSkillAreaEnemy v = v.areaEnemy
    let inline getSkillAreaAll v = v.areaAll

    let view (model : Model) : ViewModel = {
        camera =
            model.players
            |> CameraView.fromPlayers

        players =
            model
            |> Model.players
            |> PlayerView.playersView model.timePassed

        areaPlayer =
            model.skillList.areaPlayer
            |> AreaSkillEmitView.fromModels model.timePassed

        areaEnemy =
            model.skillList.areaEnemy
            |> AreaSkillEmitView.fromModels model.timePassed

        areaAll =
            model.skillList.areaAll
            |> AreaSkillEmitView.fromModels model.timePassed
    }