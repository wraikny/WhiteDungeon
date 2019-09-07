module WhiteDungeon.View.MainScene.ViewModel

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon

open WhiteDungeon.Core
//open WhiteDungeon.Core.Game


open FSharpPlus

open WhiteDungeon.View.MainScene.Model

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


let titleUI = [
    TitleText "九十九のラビリンス"
    Text "C96体験版 / Lepus Pluvia"
    Text "ボタンをクリック"
    Separator
    Button("始める", SetUIWithHistory <| Select CharacterSelect)
    Button("設定", SetUIWithHistory <| Setting SoundVolume)
    Button("クレジット", SetUI <| Credit CreditProject)
    Button("終わる", CloseGameMsg)
    Separator
]


let selectUISide (_ : Model) = [
    Button("もどる", UndoModel)
    Button("キャラクター", SetUI <| Select CharacterSelect)
    Button("迷宮", SetUI <| Select DungeonSelect)
    Button("始める", SetUI <| Select CheckSettiing)
]

let playerView (model : Model) =
    let x = model.selectOccupation

    let occupationName =
        model.setting.gameViewSetting.occupationSetting
        |> HashMap.find x
        |> fun x -> x.name
    let status = (model.setting.gameSetting.occupationSettings |> HashMap.find x).status
    [
        Text(sprintf "名前: %s" <| Option.defaultValue "Player1" model.playerName)
        Text(sprintf "キャラクター: %s" occupationName)
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
                model.setting.gameViewSetting.occupationSetting
                |> HashMap.toSeq
                |> Seq.map(fun (x, characterSetting) ->
                    Button(characterSetting.name, SelectOccupation x)
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
            //yield Separator

    } |> Seq.toList


let dungeonView (model : Model) =
    let db = model.dungeonBuilder
    [
        Text(sprintf "生成数: %d" db.roomCount)
        Text(sprintf "部屋サイズ: %A~%A" db.minRoomSize db.maxRoomSize)
        Text(sprintf "廊下幅: %d / 門の数: %d" db.corridorWidth model.gateCount)
    ]


let selectUIDungeon (model : Model) =
    seq {
        yield HeaderText("迷宮")
        yield Separator

        yield! (dungeonView model)
        for i in 1..3 ->
            Button(sprintf "サイズ%d" i, SetDungeonParameters i)
        //yield Separator
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
]


let creditUILibs = [
    HeaderText "ライブラリ"
    Separator
    WebsiteButton("FSharp.Core", "https://github.com/dotnet/fsharp")
    WebsiteButton("FSharpPlus", "https://github.com/fsprojects/FSharpPlus")
    WebsiteButton("Altseed", "http://altseed.github.io/")
    WebsiteButton("Tart", "https://github.com/wraikny/Tart")
    WebsiteButton("Mille Feuille", "https://github.com/wraikny/Mille-Feuille")
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
]

let creditUIImage = [
    HeaderText "画像"
    Separator
    WebsiteButton("彩黄月/8方向キャラチップ合成素材", "http://kaengouraiu.yu-nagi.com/")
    WebsiteButton("ヒバナ/紙の壁紙", "http://hibana.rgr.jp/")
    Separator
    HeaderText "ツール"
    WebsiteButton("グラフィック合成器", "http://www.silversecond.com/WolfRPGEditor/")
]


// https://twitter.com/intent/tweet?text=「九十九のラビリンス C96体験版」をプレイしました！ @LepusPluvia

let settingUISide = [
    Button("もどる", UndoModel)
    Button("音量", SetUI <| Setting SoundVolume)
    Button("確定", SetUI Title)
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
            Space 100.0f
            HeaderText("迷宮生成中……")
            Space 50.0f
            Button ("戻る", SetUI <| Select CheckSettiing)
            Space 100.0f
            Separator
        ]

    | ErrorUI e ->
        let url title body =
            let encode : string -> string = System.Web.HttpUtility.UrlEncode
            let title = encode title
            let body = encode body
            sprintf "https://github.com/wraikny/WhiteDungeon/issues/new?assignee=wraikny&labels=bug&title=%s&body=%s" title body
        let title = sprintf "[不具合報告] C96体験版 MainScene, %A" <| e.GetType()

        let text =
            let msgs =
                let n = 20
                (
                if model.msgHistory.Length > n then
                    take n model.msgHistory
                else
                    model.msgHistory
                )
                |>> function
                    | SetUI x -> "SetUI" + x.ToString()
                    | x -> x.ToString()

            sprintf "# Env\n%A\n# Msgs\n%A\n" model.env msgs
        Window1 [
            HeaderText "エラーが発生しました"
            Separator
            Text <| e.GetType().ToString()
            Text e.Message
            Separator
            WebsiteButton(
                "Githubで報告(ブラウザを開きます)",
                url title (sprintf "# Error\n%A\n\n%s" e text))
            //Button("もどる", SetUI (Select CheckSettiing))
            Separator
        ]


type WindowKind =
    | W1
    | W2