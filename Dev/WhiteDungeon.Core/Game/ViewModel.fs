namespace WhiteDungeon.Core.Game.ViewModel

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
// open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core.View
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.Core.Game.Model.Actor

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
    statusCurrent : Model.ActorStatus
    statusDefault : Model.ActorStatus

    objectBaseView : ObjectBaseView
}

module ActorView =
    let inline fromModel (actor : Actor.Actor) = {
        statusCurrent = actor.statusCurrent
        statusDefault = actor.statusDefault

        objectBaseView = actor.objectBase |> ObjectBaseView.fromModel
    }

    let inline hpRate (actorView : ActorView) =
        float32 actorView.statusCurrent.hp / float32 actorView.statusDefault.hp


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

type EnemyView = {
    actorView : ActorView
}

module EnemyView =
    let inline fromModel (enemy : Actor.Enemy) = {
        actorView = enemy.actor |> ActorView.fromModel
    }

    let enemiesView =
        Map.toList
        >> map(fun (id : EnemyID, enemy) -> (id.Value, fromModel enemy) )


type AreaSkillEmitView = {
   baseView : ObjectBaseView
   frameCurrent : uint32
   frameFirst : uint32
}

module AreaSkillEmitView =
    open WhiteDungeon.Core.Game.Model.Actor.Skill

    let inline fromModel (areaSkill : AreaSkill) =
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

    let errorUI (model : Model) (e : exn) =
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
    dungeonFloor : uint32
    uiMode : GameSceneMode

    camera : CameraView list
    players : UpdaterViewModel<PlayerView>
    enemies : UpdaterViewModel<EnemyView>
    areaPlayer : UpdaterViewModel<AreaSkillEmitView>
    areaEnemy : UpdaterViewModel<AreaSkillEmitView>
    areaAll : UpdaterViewModel<AreaSkillEmitView>

    mainUIWindow : UIItem list option
}


module ViewModel =
    let inline getCameras v = v.camera
    let inline getPlayers v = v.players
    let inline getEnemies v = v.enemies
    let inline getSkillAreaPlayer v = v.areaPlayer
    let inline getSkillAreaEnemy v = v.areaEnemy
    let inline getSkillAreaAll v = v.areaAll

    let view (model : Model) : ViewModel = {
        dungeonFloor = model.dungeonFloor
        uiMode = model.mode
        camera =
            model.players
            |> CameraView.fromPlayers

        players =
            model.players
            |> PlayerView.playersView

        enemies =
            model.enemies
            |> EnemyView.enemiesView

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
            model.mode |> function
            | HowToControl -> Some UIItem.howToControll
            | Pause -> Some UIItem.pause
            | Stair -> Some UIItem.stair
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