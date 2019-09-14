module WhiteDungeon.View.MainScene.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Helper.Collections
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon

open WhiteDungeon.Core
//open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model
open WhiteDungeon

open FSharpPlus

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
with
    override x.ToString() =
        x |> function
        | Title -> "Title"
        | Select a -> "Select " + a.ToString()
        | Credit a -> "Credit " + a.ToString()
        | Setting a -> "Setting " + a.ToString()
        | WaitingGenerating -> "WaitingGenerating"
        | ErrorUI e -> "ErrorUI " + e.GetType().ToString()


type Msg =
    | SetGameSceneRandomSeed of int
    | UndoModel
    | SetUI of UIMode
    | SetUIWithHistory of UIMode
    | OccupationListToggle of bool
    | SelectOccupation of Occupation
    | InputName of string

    | SetDungeonParameters of uint16

    | GenerateDungeon
    | GeneratedDungeonModel of DungeonBuilder * DungeonModel
    | GeneratedDungeonParams of Game.Model.Dungeon.GeneratedDungeonParams
    | CloseGameMsg
    | AddBGMVolume of int


type Model =
    {
        env : TartEnvBuilder

        uiMode : UIMode
        occupationListToggle : bool

        playerName : string option
        selectOccupation : Occupation

        initSize : uint16

        setting : View.AppSetting

        bgmVolume : int

        prevModel : Model option

        gameSceneRandomSeed : int

        msgHistory : Msg list
    }


let initModel env (setting : View.AppSetting) =
    {
        env = env
        uiMode = Title
        occupationListToggle = false

        playerName = None
        selectOccupation =
            setting.gameViewSetting.occupationSetting
            |> HashMap.toList
            |> fun x -> fst (x.[0])

        initSize = 1us

        setting = setting

        bgmVolume = 5

        prevModel = None

        gameSceneRandomSeed = 0

        msgHistory = []
    }