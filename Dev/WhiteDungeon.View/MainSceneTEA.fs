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
    | InputField of int * string * string option * (string -> 'Msg)
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
    | CreditMusic
    | CreditImage
    //| CreditTool


type SettingMode =
    | SoundVolume


type UIMode =
    | Title
    | Select of SelectMode
    | Credit of CreditMode
    | Setting of SettingMode
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
    | AddBGMVolume of int


type ViewMsg =
    | SetBGMVolume of float32
    | CloseGame
    | StartGame of Game.Model.Model * float32

type Model = {
    uiMode : UIMode
    occupationListToggle : bool

    playerName : string option
    selectOccupation : Occupation

    dungeonBuilder : DungeonBuilder

    gameSetting : GameSetting

    bgmVolume : int
}


let initModel (gameSetting : GameSetting) = {
    uiMode = Title
    occupationListToggle = false

    playerName = None
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

    bgmVolume = 5
}


let update (msg : Msg) (model : Model) : Model * Cmd<Msg, ViewMsg> =
    msg |> function
    | SetUI uiMode ->
        { model with
            uiMode = uiMode
            occupationListToggle = false }, Cmd.none

    | AddBGMVolume i ->
        let v = model.bgmVolume + i |> min 10 |> max 0 
        { model with bgmVolume = v}, Cmd.port(SetBGMVolume (float32 v / 10.0f))

    | OccupationListToggle x ->
        { model with occupationListToggle = x }, Cmd.none

    | InputName s ->
        { model with playerName = if s = "" then None else Some s }, Cmd.none

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
                    let name = Option.defaultValue (sprintf "Player%d" index) name

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
        model, Cmd.port(ViewMsg.StartGame (gameModel, float32 model.bgmVolume / 10.0f))

    | CloseGameMsg ->
        model, Cmd.port CloseGame


let titleUI = [
    TitleText "九十九のラビリンス"
    Text "C96体験版 / Lepus Pluvia"
    Separator
    Button("始める", SetUI <| Select CharacterSelect)
    Button("設定", SetUI <| Setting SoundVolume)
    Button("クレジット", SetUI <| Credit CreditProject)
    Button("終わる", CloseGameMsg)
    Separator
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
        Text(sprintf "名前: %s" <| Option.defaultValue "Player1" model.playerName)
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
            Text(sprintf "%s / %A" (Option.defaultValue "Player1" model.playerName) model.selectOccupation)
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
    Button("音楽", SetUI <| Credit CreditMusic)
    Button("画像", SetUI <| Credit CreditImage)
    //Button("ツール", SetUI <| Credit CreditTool)
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
    WebsiteButton("フリー音楽素材 H/MIX GALLERY", "http://www.hmix.net/")
    Text "精霊の街"
    Text "原生の楽園"
    Text "ブリキのコーヒーメーカー"
    Separator
    WebsiteButton("くらげ工匠", "http://www.kurage-kosho.info/")
    Text "ページめくり03"
    Text "ボタン47"
    Separator
]

let creditUIImage = [
    HeaderText "画像"
    Separator
    WebsiteButton("彩黄月/8方向キャラチップ合成素材", "http://kaengouraiu.yu-nagi.com/")
    WebsiteButton("ヒバナ/紙の壁紙", "http://hibana.rgr.jp/")
    Separator
    HeaderText "ツール"
    WebsiteButton("グラフィック合成器", "http://www.silversecond.com/WolfRPGEditor/")
    Separator
]

//let creditUITool = [
//    HeaderText "ツール"
//    Separator
//    WebsiteButton("グラフィック合成器", "http://www.silversecond.com/WolfRPGEditor/")
//    Separator
//]

// https://twitter.com/intent/tweet?text=「九十九のラビリンス C96体験版」をプレイしました！ @LepusPluvia

let settingUISide = [
    Button("もどる", SetUI Title)
    Button("音量", SetUI <| Setting SoundVolume)
]

let settingUISoundVolume model =
    [
        HeaderText "音量"
        Separator
        Text <| sprintf "BGM: %d/10" model.bgmVolume
    ] @ (
        let upButton = Button("上げる", AddBGMVolume 1)
        let downButton = Button("下げる", AddBGMVolume -1)
        model.bgmVolume |> function
        | 0 -> 
            [ upButton; Text "下げる" ]
        | 10 ->
            [ Text "上げる"; downButton ]
        | _ ->
            [ upButton; downButton ]

    ) @ [
        Separator
    ]


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
            | CreditMusic -> creditUIBGM
            | CreditImage -> creditUIImage
            //| CreditTool -> creditUITool
        )

    | Setting x ->
        Window2(settingUISide, x |> function
            | SoundVolume -> settingUISoundVolume model
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