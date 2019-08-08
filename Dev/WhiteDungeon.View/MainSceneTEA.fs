module WhiteDungeon.View.MainSceneTEA

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon

open WhiteDungeon.Core
//open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Model

open FSharpPlus

type 'Msg MenuItem =
    | TitleText of string
    | HeaderText of string
    | Text of string
    | Button of string * 'Msg
    | WebsiteButton of string * string
    | InputField of int * string * string * (string -> 'Msg)
    | Separator
    | Space of float32


type 'Msg ViewModel =
    | Window1 of 'Msg MenuItem list
    | Window2 of 'Msg MenuItem list * 'Msg MenuItem list


type SelectMode =
    | CharacterSelect
    | DungeonSelect
    | CheckSettiing


type CreditMode =
    | CreditProject
    | CreditLibs
    | CreditBGM


type UIMode =
    | Title
    | Select of SelectMode
    | Credit of CreditMode
    | WaitingGenerating
    | ErrorUI of exn


type Msg =
    | SetUI of UIMode
    | OccupationListToggle of bool
    | SelectOccupation of Occupation
    | InputName of string

    | SetDungeonParameters of count:int * minSize:int * maxSize:int * range:float32 * corridor:int

    | GenerateDungeon
    | GeneratedGameModel of Game.Model.Model
    | CloseGameMsg


type ViewMsg =
    | CloseGame
    | StartGame of Game.Model.Model

type Model = {
    uiMode : UIMode
    occupationListToggle : bool

    playerName : string
    selectOccupation : Occupation

    dungeonBuilder : DungeonBuilder

    gameSetting : GameSetting
}


let initModel (gameSetting : GameSetting) = {
    uiMode = Title
    occupationListToggle = false

    playerName = "Player1"
    selectOccupation = Seeker

    dungeonBuilder = {
        seed = 0
        roomCount = 200

        roomGeneratedRange = (60.0f, 60.0f)

        minRoomSize = (6, 6)
        maxRoomSize = (12, 12)

        roomMoveRate = 0.2f
        roomMeanThreshold = 1.25f
        restoreEdgeRate = 0.1f
        corridorWidth = 2
    }

    gameSetting = gameSetting
}


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    msg |> function
    | SetUI uiMode ->
        { model with
            uiMode = uiMode
            occupationListToggle = false }, Cmd.none

    | OccupationListToggle x ->
        { model with occupationListToggle = x }, Cmd.none

    | InputName s ->
        { model with playerName = s }, Cmd.none

    | SelectOccupation x ->
        { model with
            selectOccupation = x
            occupationListToggle = false }, Cmd.none

    | SetDungeonParameters(count, minSize, maxSize, range, corridor) ->
        { model with
            dungeonBuilder =
                { model.dungeonBuilder with
                    roomCount = count
                    minRoomSize = (minSize, minSize)
                    maxRoomSize = (maxSize, maxSize)
                    roomGeneratedRange = (range, range)
                    corridorWidth = corridor
                }
        }, Cmd.none

    | GenerateDungeon ->
        monad {
            let! seed = Random.int minValue<int> maxValue<int>
            let! roomIndex = Random.int 0 (model.dungeonBuilder.roomCount - 1)
            return (seed, roomIndex)
        }
        |> TartTask.withEnv(fun (seed, roomIndex) -> async {
            let dungeonModel =
                { model.dungeonBuilder with seed = seed }
                |> DungeonBuilder.generate

            let largeRooms = toList dungeonModel.largeRooms

            let largeRoomsCount = length largeRooms

            let targetRoomIndex = roomIndex % largeRoomsCount

            let targetRoom = snd largeRooms.[targetRoomIndex]

            let fromCell =
                model.gameSetting.dungeonCellSize
                |> DungeonModel.cellToCoordinate

            let targetPosition =
                targetRoom.rect
                |>> fromCell
                |> Rect.centerPosition

            let size = model.gameSetting.characterSize
            let players =
                [ model.playerName, model.selectOccupation ]
                |> Seq.indexed
                |> Seq.map(fun (index, (name, occupation)) ->
                    let status =
                        model.gameSetting.occupationDefaultStatus
                        |> Map.find occupation

                    let character : Model.Character = {
                        id = Model.CharacterID -index
                        name = name
                        currentOccupation = occupation
                        occupations = [
                            occupation, status
                        ] |> Map.ofList
                    }


                    let playerId = Game.Model.PlayerID (uint32 index)

                    let player =
                        Game.Model.Actor.Player.init
                            size
                            (targetPosition - (Vec2.init (float32 index) 0.0f) * size)
                            status
                            playerId
                            character

                    (playerId, player)
                )
                |> Map.ofSeq

            let gameModel =
                Game.Model.Model.init
                    players
                    model.dungeonBuilder
                    dungeonModel
                    model.gameSetting

            return gameModel
        })
        |> TartTask.perform (ErrorUI >> SetUI) GeneratedGameModel
        |> fun cmd ->
            { model with uiMode = WaitingGenerating }, cmd

    | GeneratedGameModel gameModel ->
        model, Cmd.port(ViewMsg.StartGame gameModel)

    | CloseGameMsg ->
        model, Cmd.port CloseGame


let titleUI = [
    Space 50.0f
    TitleText "九十九のラビリンス"
    Text "C96体験版 / Lepus Pluvia"
    Separator
    Button("始める", SetUI <| Select CharacterSelect)
    Button("クレジット", SetUI <| Credit CreditProject)
    Button("終わる", CloseGameMsg)
    Separator
    Space 50.0f
]


let selectUISide (_ : Model) = [
    Button("もどる", SetUI Title)
    Button("キャラクター", SetUI <| Select CharacterSelect)
    Button("迷宮", SetUI <| Select DungeonSelect)
    Button("始める", SetUI <| Select CheckSettiing)
]


let playerView (model : Model) =
    let x = model.selectOccupation
    let status = model.gameSetting.occupationDefaultStatus |> Map.find x
    [
        Text(sprintf "名前: %s" model.playerName)
        Text(sprintf "キャラクター: %A" x)
        Text(sprintf "レベル: %d" status.level)
        Text(sprintf "体力: %.1f" status.hp)
        Text(sprintf "速さ: %.1f / %.1f" status.walkSpeed status.dashSpeed)
    ]

let selectUIChara (model : Model) : Msg MenuItem list =
    seq {
        yield HeaderText("キャラクター")
        yield Separator

        if model.occupationListToggle then
            yield!
                model.gameSetting.occupationDefaultStatus
                |> Map.toSeq
                |> Seq.map(fun (x, _) ->
                    Button(string x, SelectOccupation x)
                )

            yield! [
                Separator
                Button("閉じる", OccupationListToggle false)
                Separator
            ]

        else
            yield! [
                InputField(16, "名前を入力(英数)", model.playerName, InputName)
                Button("キャラクター選択", OccupationListToggle true)
                Separator
            ]
            yield! (playerView model)
            yield Separator
    
    } |> Seq.toList


let dungeonView (model : Model) =
    let db = model.dungeonBuilder
    [
        Text(sprintf "生成数: %d" db.roomCount)
        Text(sprintf "最小部屋サイズ: %A" db.minRoomSize)
        Text(sprintf "最大部屋サイズ: %A" db.maxRoomSize)
        Text(sprintf "廊下幅: %d" db.corridorWidth)
    ]


let selectUIDungeon (model : Model) =
    seq {
        yield HeaderText("迷宮")
        yield Separator

        yield! (dungeonView model)
        for i in 1..3 ->
            Button(sprintf "サイズ%d" i,
                SetDungeonParameters(
                    100 * i,
                    2 * (i + 1),
                    4 * (i + 1),
                    33.3f * float32 i, 1 + i ))
        yield Separator
    }
    |> Seq.toList


let selectUICheck (model : Model) =
    [
        [
            Text("以下の内容でゲームを開始します")
            Separator
            Text(sprintf "%s / %A" model.playerName model.selectOccupation)
            Separator
        ]
        dungeonView model
        [
            Separator
            Button("始める", GenerateDungeon)
            Separator
        ]
    ] |> List.concat


let creditUISide = [
    Button("もどる", SetUI Title)
    Button("制作", SetUI <| Credit CreditProject)
    Button("ライブラリ", SetUI <| Credit CreditLibs)
    Button("BGM", SetUI <| Credit CreditBGM)
]


let creditUIProj = [
    HeaderText "制作"
    Separator
    Text("Lepus Pluvia")
    WebsiteButton("Website", "http://LepusPluvia.com")
    WebsiteButton("Twitter", "http://twitter.com/LepusPluvia")
    Separator
]


let creditUILibs = [
    HeaderText "ライブラリ"
    Separator
    WebsiteButton("FSharp.Core", "https://github.com/dotnet/fsharp")
    WebsiteButton("FSharpPlus", "https://github.com/fsprojects/FSharpPlus")
    WebsiteButton("Altseed", "http://altseed.github.io/")
    WebsiteButton("Tart", "https://github.com/wraikny/Tart")
    WebsiteButton("Mille Feuille", "https://github.com/wraikny/Mille-Feuille")
    Separator
]


let creditUIBGM = [
    HeaderText "音楽"
    Separator
    Text "てすと"
    Text "あああああ"
    Text "あああああ"
    Separator
]

let creditUIImages = [
    HeaderText "イラスト"
    Separator
    Text "てすと"
    Text "あああああ"
    Text "あああああ"
    Separator
]

// https://twitter.com/intent/tweet?text=「九十九のラビリンス C96体験版」をプレイしました！ @LepusPluvia


let view (model : Model) : Msg ViewModel =
    model.uiMode |> function
    | Title ->
        Window1 titleUI

    | Select x ->
        Window2(selectUISide model, x |> function
            | CharacterSelect -> selectUIChara model
            | DungeonSelect -> selectUIDungeon model
            | CheckSettiing -> selectUICheck model
        )

    | Credit x ->
        Window2(creditUISide, x |> function
            | CreditProject -> creditUIProj
            | CreditLibs -> creditUILibs
            | CreditBGM -> creditUIBGM
        )

    | WaitingGenerating ->
        Window1 [
            Separator
            Space 200.0f
            HeaderText("迷宮生成中……")
            Space 200.0f
            Separator
        ]

    | ErrorUI e ->
        Window1 [
            HeaderText "エラーが発生しました"
            Separator
            Text <| e.GetType().ToString()
            Text e.Message
            Button("もどる", SetUI (Select CheckSettiing))
        ]


type WindowKind =
    | W1
    | W2