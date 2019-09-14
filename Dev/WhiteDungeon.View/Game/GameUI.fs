namespace WhiteDungeon.View.Game


open wraikny.Tart.Helper.Utils
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry
open wraikny.Tart.Core
open wraikny.Tart.Advanced
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Fs.Input
open wraikny.MilleFeuille.Core
open wraikny.MilleFeuille.Core.Input

open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open wraikny.Tart.Helper.Collections
open wraikny.Tart.Helper.Extension
open wraikny.MilleFeuille.Fs
open wraikny.MilleFeuille.Fs.Objects
open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.Geometry

open FSharpPlus

open System
open System.Collections.Generic
open System.Linq
open System.Reactive
open System.Reactive.Linq


type GameSceneArgs = {
    windowSetting : UI.WindowSetting
    headerFont : asd.Font
    textFont : asd.Font
    buttonFont : asd.Font

    bgmVolume : float32

    createMainScene : unit -> asd.Scene

    randomSeed : int
}

type private PlayerValues = {
    status : Model.ActorStatus
    expoint : uint16
    occupation : Model.Occupation
}


type GameUI(gameViewSetting : GameViewSetting, gameSetting : Model.GameSetting, gameSceneArgs : GameSceneArgs) =
    inherit asd.GeometryObject2D()

    let ws = asd.Engine.WindowSize.To2DF()

    let mouse = new UI.MouseButtonSelecter(0.0f)

    let toAsdArea area =
        asd.RectF(ws * Vec2.toVector2DF area.position, ws * Vec2.toVector2DF area.size)

    let dungeonFloorArea = gameViewSetting.gameUIDungeonFloor |> toAsdArea

    let createDynamicFont size color =
        asd.Engine.Graphics.CreateDynamicFont(
            gameViewSetting.gameUITextFont,
            size, color,
            0, asd.Color()
        )

    let dungeonFloorSetting = 
        let col = Vec3.toColor gameViewSetting.gameUITextColor
        { gameSceneArgs.windowSetting with
            itemMargin = 10.0f
            itemAlignment = UI.WindowSetting.Alignment.Right (0.02f * ws.X)
            
            //windowSize = UI.WindowSetting.Fixed(dungeonFloorArea.Size, true)
            windowSize = UI.WindowSetting.FixWidth(dungeonFloorArea.Size.X)

            toggleDirection = UI.WindowSetting.ToggleDirection.Y
            centerPositionRate = Vec2.init 0.0f 0.0f
            togglePositionRate = Vec2.init 0.5f 0.0f

            frameColor = Vec4.toColor gameViewSetting.gameUIFrameColor
            rectColor = col

            textFont = createDynamicFont gameViewSetting.gameUITextSize col
        }


    let dungeonFloorWindow =
        new UI.MouseWindow(dungeonFloorSetting, mouse
            , Position = dungeonFloorArea.Position
        )

    let playerArea = toAsdArea gameViewSetting.gameUIPlayerArea


    let playerStatusWindow =
        let playerStatusSetting =
            { dungeonFloorSetting with
                itemAlignment = UI.WindowSetting.Alignment.Left (0.02f * ws.X)
                //windowSize = UI.WindowSetting.Fixed(playerArea.Size, false)
                windowSize = UI.WindowSetting.FixWidth(playerArea.Size.X)
                toggleDirection = UI.WindowSetting.ToggleDirection.Y
                centerPositionRate = Vec2.init 0.0f 1.0f
                togglePositionRate = Vec2.init 0.5f 1.0f
            }
        new UI.MouseWindow(playerStatusSetting, mouse
            , Position = playerArea.Position
            //, Position = ws * 0.5f
        )


    let mutable lastFloor = maxValue<uint16>

    let mutable lastPlayerValues = None

    override this.OnAdded() =
        this.AddChildWindow(dungeonFloorWindow)
        this.AddChildWindow(playerStatusWindow)


    member __.OnNext(viewModel : ViewModel.ViewModel) =
        if lastFloor <> viewModel.dungeonFloor then
            lastFloor <- viewModel.dungeonFloor
            dungeonFloorWindow.UIContents <- [
                UI.Text ("Menu: Esc")
                UI.Text (sprintf "Floor: %d" viewModel.dungeonFloor)
            ]

        let player = snd viewModel.players.[0]

        let occSetting =
            gameViewSetting.occupationSetting
            |> HashMap.find player.character.currentOccupation


        (lastPlayerValues) |> function
        | Some {status = x; occupation = y; expoint = z } when
            x = player.actor.statusCurrent
            && y = player.character.currentOccupation
            && z = player.expoint
            -> ()
        | _ ->
            lastPlayerValues <- Some {
                status = player.actor.statusCurrent
                expoint = player.expoint
                occupation = player.character.currentOccupation
            }
            
            let hpCurrent = player.actor.statusCurrent.hp
            let hpDefault = player.actor.statusDefault.hp

            let nextExp = gameSetting.lvUpExp player.actor.level
            
            playerStatusWindow.UIContents <- [
                UI.Text (sprintf "%s / %s" player.character.name occSetting.name )
                UI.Text (sprintf "Level: %d" player.actor.level)
                UI.Text (sprintf "Exp: %d/%d" player.expoint nextExp)
                UI.RectWith (5.0f, 0.8f * (float32 player.expoint / float32 nextExp), asd.Color(200, 255, 255, 255))
                UI.Text (sprintf "HP: %d/%d" (int hpCurrent) (int hpDefault) )
                UI.RectWith (5.0f, 0.8f * (hpCurrent / hpDefault), asd.Color(0, 255, 0, 255))
            ]


    member __.Toggle(cond, ?callback) =
        let callback = defaultArg callback ignore

        if cond then
            dungeonFloorWindow.Toggle(true, fun () ->
                playerStatusWindow.Toggle(true, callback)
            )
        else
            playerStatusWindow.Toggle(false, fun () ->
                dungeonFloorWindow.Toggle(false, callback)
            )

    member __.IsToggleOn
        with get() =
            dungeonFloorWindow.IsToggleOn
            || playerStatusWindow.IsToggleOn

    member __.IsToggleAnimating
        with get() =
            dungeonFloorWindow.IsToggleAnimating
            || playerStatusWindow.IsToggleAnimating


    interface UI.IToggleWindow with
        member this.Toggle(x) = this.Toggle(x)
        member this.Toggle(x, callback) = this.Toggle(x, callback)
        member this.IsToggleOn with get() = this.IsToggleOn
        member this.IsToggleAnimating with get() = this.IsToggleAnimating



    member private this.AddChildWindow(x) =
        this.AddDrawnChild(x,
            asd.ChildManagementMode.IsDrawn
            ||| asd.ChildManagementMode.IsUpdated
            ||| asd.ChildManagementMode.Disposal
            ||| asd.ChildManagementMode.RegistrationToLayer,
            asd.ChildTransformingMode.Nothing,
            asd.ChildDrawingMode.DrawingPriority
        )