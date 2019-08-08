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
    isMoved : bool
    area : float32 Vec2 Rect
    direction : MoveDirection
}

module ObjectBaseView =
    let inline fromModel (objectBase : ObjectBase) = {
        isMoved = objectBase.isMoved
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

    let fromModels  : Map<uint32, _> -> (uint32 * AreaSkillEmitView) list =
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


open WhiteDungeon.Core.Game.Msg


type UIItem =
    | HeaderText of string
    | Text of string
    | Button of string * Msg
    | Separator
    | TitleButton of string
    | CloseButton of string


module UIItem =
    let howToControll = [
        HeaderText "操作方法"
        Separator
        Text "移動: WASDキー"
        Text "攻撃: マウス左クリック"
        Text "一時停止: Escキー"
        Button ("始める", SetGameMode Model.GameMode)
    ]

    let stair = [
        HeaderText "次の階層に移動しますか？"
        Separator
        //Button("はい")
        //Button("タイトルに戻る")
    ]

    let pause = [
        HeaderText "一時停止"
        Separator
        Button ("再開する", SetGameMode Model.GameMode)
        TitleButton "タイトルに戻る"
        CloseButton "ゲームを終了する"
    ]




type ViewModel = {
    uiMode : GameSceneMode

    camera : CameraView list
    players : UpdaterViewModel<PlayerView>
    areaPlayer : UpdaterViewModel<AreaSkillEmitView>
    areaEnemy : UpdaterViewModel<AreaSkillEmitView>
    areaAll : UpdaterViewModel<AreaSkillEmitView>

    mainUIWindow : UIItem list option
}


module ViewModel =
    let inline getCameras v = v.camera
    let inline getPlayers v = v.players
    let inline getSkillAreaPlayer v = v.areaPlayer
    let inline getSkillAreaEnemy v = v.areaEnemy
    let inline getSkillAreaAll v = v.areaAll

    let view (model : Model) : ViewModel = {
        uiMode = model.uiMode
        camera =
            model.players
            |> CameraView.fromPlayers

        players =
            model
            |> Model.players
            |> PlayerView.playersView

        areaPlayer =
            model.skillList.areaPlayer
            |> AreaSkillEmitView.fromModels

        areaEnemy =
            model.skillList.areaEnemy
            |> AreaSkillEmitView.fromModels

        areaAll =
            model.skillList.areaAll
            |> AreaSkillEmitView.fromModels

        mainUIWindow =
            model.uiMode |> function
            | HowToControl -> Some UIItem.howToControll
            | Pause -> Some UIItem.pause
            | Stair -> Some UIItem.stair
            | GameMode -> None
    }