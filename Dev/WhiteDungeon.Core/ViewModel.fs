﻿namespace WhiteDungeon.Core.ViewModel

open WhiteDungeon.Core
// open WhiteDungeon.Core.Game.Model

open Affogato
open wraikny.Tart.Helper
open wraikny.Tart.Core
open WhiteDungeon.Core.Model

open FSharpPlus


type ObjectBaseView = Model.ObjectBase
type ActorView = Actor
type PlayerView = Player
type EnemyView = Enemy

type AreaSkillEmitView = AreaSkill



type CameraView = {
    position : float32 Vector2
}

module CameraView =
    let inline init position = {
        position = position
    }

    let inline fromPlayers (players : Map<Model.PlayerID, Model.Player>) =
        players
        |> Map.toList
        |> sortBy (fun (id, _) -> id.Value)
        |>> ((fun (_, p) -> p.actor.objectBase.position) >> init)


open WhiteDungeon.Core


type UIItem =
    | HeaderText of string
    | Text of string
    | Button of string * Msg
    | Separator
    | URLButton of string * string
    | TitleButton of string
    | CloseButton of string
    | Space of float32


module UIItem =
    let howToControll = [
        HeaderText "操作方法"
        Separator
        Text "移動: WASDキー"
        Text "攻撃: マウスクリック"
        Text "一時停止: Escキー"
        Separator
        Text "黒いマスに触れて階層を移動できます"
        Button ("始める", SetGameMode Model.GameMode)
        Separator
    ]

    let stair = [
        HeaderText "次の階層に移動しますか？"
        Separator
        Button("移動する", GenerateNewDungeon)
        Button("このまま続ける", SetGameMode Model.GameMode)
        Separator
        Button ("ゲームを終了する", SetGameMode <| GameFinished true)
        Separator
    ]

    let pause = [
        HeaderText "一時停止"
        Separator
        Button ("再開する", SetGameMode Model.GameMode)
        Button ("ゲームを終了する", SetGameMode <| GameFinished true)
        Separator
    ]

    let gameFinished header = [
        HeaderText header
        Separator
        URLButton("ツイートする", "https://twitter.com/intent/tweet?text=「九十九のラビリンス C96体験版」をプレイしました！ \n@LepusPluvia")
        Separator
        TitleButton "タイトルに戻る"
        CloseButton "閉じる"
        Separator
    ]

    let errorUI (_model : Model) (e : exn) =
        let url title body =
            let encode : string -> string = System.Web.HttpUtility.UrlEncode
            let title = encode title
            let body = encode body
            sprintf "https://github.com/wraikny/WhiteDungeon/issues/new?assignee=wraikny&labels=bug&title=%s&body=%s" title body
        let title = sprintf "[不具合報告] C96体験版 GameScene, %A" <| e.GetType()

        [
            HeaderText "エラーが発生しました"
            Separator
            Text <| e.GetType().ToString()
            Text e.Message
            Separator
            URLButton(
                "Githubで報告(ブラウザを開きます)",
                url title (sprintf "# Error\n%A\n" e))
            //TitleButton("タイトルに戻る")
            Separator
        ]




type ViewModel = {
    dungeonFloor : uint16
    uiMode : GameSceneMode

    camera : CameraView // list
    players : UpdaterViewModel<uint32, PlayerView>
    enemies : UpdaterViewModel<uint32, EnemyView>

    buildings : UpdaterViewModel<uint32, BuildingKind * float32 Rectangle2>

    areaPlayer : UpdaterViewModel<uint32, AreaSkillEmitView>
    areaEnemy : UpdaterViewModel<uint32, AreaSkillEmitView>
    areaAll : UpdaterViewModel<uint32, AreaSkillEmitView>

    mainUIWindow : UIItem list option
}

open Affogato.Advanced

module ViewModel =
    let inline getCameras v = v.camera
    let inline getPlayers v = v.players
    let inline getEnemies v = v.enemies
    let inline getSkillAreaPlayer v = v.areaPlayer
    let inline getSkillAreaEnemy v = v.areaEnemy
    let inline getSkillAreaAll v = v.areaAll

    let view (model : Model) : ViewModel =
        let inline mapToUpdateVM (x : Map< ^a, _ >) =
            [ for (i, e) in Map.toSeq x -> ((^a : (member Value : _) i) , e) ]

        let p0Pos =
            model.players
            |> Map.find model.localPlayerId
            |> ObjectBase.position

        let inline minusP0Pos (i, p) = (i, p |> ObjectBase.map(fun o -> { o with position = o.position - p0Pos}))

        let cellSize = model.gameSetting.dungeonCellSize

        {
            dungeonFloor = model.dungeonFloor
            uiMode = model.mode
            camera = { position = p0Pos }

            players =
                model.players
                |> mapToUpdateVM
                |>> minusP0Pos

            enemies = model.enemies |> mapToUpdateVM |>> minusP0Pos

            buildings =
                model.buildings
                |>> fun building ->
                    let pos =
                        Dungeon.Model.cellToCoordinate cellSize building.luCell
                    let area = Rectangle.init (pos - p0Pos) building.size
                    (building.id, (building.kind, area))

            areaPlayer =
                model.skillList.areaPlayer
                |> Map.toList
                |>> minusP0Pos

            areaEnemy =
                model.skillList.areaEnemy
                |> Map.toList
                |>> minusP0Pos

            areaAll =
                model.skillList.areaAll
                |> Map.toList
                |>> minusP0Pos

            mainUIWindow =
                model.mode |> function
                | HowToControl -> Some UIItem.howToControll
                | Pause -> Some UIItem.pause
                | GateMode -> Some UIItem.stair
                | GameFinished true -> Some( UIItem.gameFinished "ゲーム終了")
                | GameFinished false -> Some( UIItem.gameFinished "ゲームオーバー")
                | ErrorUI e -> Some <| UIItem.errorUI model e
                | WaitingGenerating ->
                    Some [
                        Separator
                        Space 200.0f
                        HeaderText("迷宮生成中……")
                        Space 200.0f
                        Separator
                    ]
                | GameMode ->
                    None
        }