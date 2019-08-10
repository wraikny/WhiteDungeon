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
    | SetGameSceneRandomSeed of int
    | UndoModel
    | SetUI of UIMode
    | SetUIWithHistory of UIMode
    | OccupationListToggle of bool
    | SelectOccupation of Occupation
    | InputName of string

    | SetDungeonParameters of int

    | GenerateDungeon
    | GeneratedDungeonModel of DungeonBuilder * DungeonModel
    | GeneratedDungeonParams of Game.Model.Dungeon.GeneratedDungeonParams
    | CloseGameMsg
    | AddBGMVolume of int


type Model = {
    uiMode : UIMode
    occupationListToggle : bool

    playerName : string option
    selectOccupation : Occupation

    gateCount : int
    dungeonBuilder : DungeonBuilder

    gameSetting : GameSetting

    bgmVolume : int

    prevModel : Model option

    gameSceneRandomSeed : int
}


let updateDungeonBuilder i (model : Model) =
    let i1 = i + 1
    { model with
        gateCount = i1
        dungeonBuilder =
            { model.dungeonBuilder with
                roomCount = 100 * i
                minRoomSize = (3 * i1, 2 * i1)
                maxRoomSize = (6 * i1, 4 * i1)
                roomGeneratedRange = (2.0f * 30.0f * float32 i, 1.0f * 30.0f * float32 i)
                corridorWidth = i1
            }
    }


let initModel (gameSetting : GameSetting) =
    {
        uiMode = Title
        occupationListToggle = false

        playerName = None
        selectOccupation = Seeker

        gateCount = zero
        dungeonBuilder =
            {
                seed = 0
                roomCount = zero

                roomGeneratedRange = zero, zero

                minRoomSize = zero, zero
                maxRoomSize = zero, zero

                roomMoveRate = 0.3f
                roomMeanThreshold = 1.25f
                restoreEdgeRate = 0.2f
                corridorWidth = 3
            }

        gameSetting = gameSetting

        bgmVolume = 5

        prevModel = None

        gameSceneRandomSeed = 0
    }
    |> updateDungeonBuilder 2
