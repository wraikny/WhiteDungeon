module WhiteDungeon.View.MainScene.TEA

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
    | UndoModel
    | SetUI of UIMode
    | SetUIWithHistory of UIMode
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

    prevModel : Model option
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

    prevModel = None
}