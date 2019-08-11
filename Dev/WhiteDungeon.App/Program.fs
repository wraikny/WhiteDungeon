module WhiteDungeon.App.Program

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open wraikny.MilleFeuille.Fs.Math
open wraikny.MilleFeuille.Fs.UI
open WhiteDungeon
open WhiteDungeon.Core
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model
open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color

open WhiteDungeon.Core.Game.Model.Skill

open FSharpPlus.Math.Applicative
open FSharpPlus

let black = Vec3.init 0uy 0uy 0uy
let white = Vec3.init 255uy 255uy 255uy

let ume = Vec3.init 234uy 173uy 189uy

let sumire = Vec3.init 85uy 69uy 98uy

let sakura = Vec3.init 250uy 219uy 224uy

let windowSize = (Vec2.init 16 9) .* 75

let windowRSizeUnit = (map float32 windowSize) ./ float32 windowSize.x

let textFontPath = "Font/mplus-1c-light.ttf"

let appSetting : View.AppSetting = {
    windowSize = windowSize

    menuSceneSetting =
        let createButtonColor col x y z =
            let c a = col |>> float32 |>> ( * ) a |>> byte |> Vec3.toColor
            {
                defaultColor = c x
                hoverColor = c y
                holdColor = c z
            }
            
        {
            backColor = white
            frameColor = Vec4.init ume.x ume.y ume.z 200uy
            buttonColor = createButtonColor sumire 1.0f 0.8f 0.6f
            inputColor = createButtonColor sakura 1.0f 0.8f 0.6f
            inputFocusColor = createButtonColor sakura 1.0f 1.0f 1.0f

            inputFont = textFontPath
            inputFontSize = 30
            textSize = 30
            inputFontColor = black

            windowWidthWRate = 0.8f
            window2MarginRate = 0.02f
            window2HeightRate = 0.9f

            bgm = "bgm/seirei_no_machi.ogg"

            #if !DEBUG
            titleFont = "Font/titleFont.aff"
            headerFont = "Font/headerFont.aff"
            #endif
        }

    gameViewSetting =
        let windowMargin = 0.02f /. windowRSizeUnit
        let floorSize = (Vec2.init 0.15f 0.03f) / windowRSizeUnit
        let playerStatusSize = (Vec2.init 0.2f 0.15f)

        {
            occupationImages = [
                Model.Seeker, ActorImages.fromGraphicmaker 8u 4u "Image/Game/Occupation/hunter.png"
            ] |> Map.ofList

            bgms = [
                "bgm/gensei_no_rakuen.ogg"
                "bgm/buriki_no_coffee_maker.ogg"
            ]

            gameUIFrameColor = Vec4.init sumire.x sumire.y sumire.z 240uy
            gameUITextColor = sakura
            gameUITextFont = textFontPath
            gameUITextSize = 20
            gameUIDungeonFloor =
                Rect.init
                    ( Vec2.init (1.0f - windowMargin.x - floorSize.x) windowMargin.y )
                    floorSize
            gameUIPlayerArea =
                Rect.init
                    ( Vec2.init windowMargin.x (1.0f - windowMargin.y) )
                    playerStatusSize
        }

    gameSetting = {
        dungeonCellSize = Vec2.init 250.0f 250.0f
        minPlayerCount = 1
        maxPlayerCount = 1
        binarySearchCountMovingOnWall = 4
        characterSize = Vec2.init 100.0f 200.0f
        occupationSettings = Map.ofList [
            Model.Seeker, {
                status =
                    {
                        Model.ActorStatus.level = 1
                        hp = 100.0f
                        walkSpeed = 6.0f
                        dashSpeed = 12.0f
                    }
                skill1 = fun actor ->
                    let dir = 
                        actor.objectBase.direction
                        |> Model.MoveDirection.toVector

                    let pos = actor.objectBase.position + (100.0f *. dir)

                    [
                        Skill.AreaBuilder {
                            skillBase =
                                {
                                    delay = 0u
                                    effects = [|
                                        Skill.Damage(fun atk def ->
                                            (atk.level - def.level) |> max 1 |> ( * ) 5 |> float32
                                        )
                                    |]
                                }
                            objectBase = ObjectBase.init (one .* 100.0f) pos

                            target = Skill.AreaTarget.Enemies

                            removeWhenHitWall = true
                            removeWhenHitActor = true

                            move = seq {
                                //for _ in 1..10 -> Skill.Stay
                                for _ in 1..60 -> Skill.Move (dir .* 10.0f)
                                for _ in 1..60 -> Skill.Scale(one .* 10.0f)
                            } |> toList
                        }
                    ]
                skill2 = fun actor ->
                    let dir = 
                        actor.objectBase.direction
                        |> Model.MoveDirection.toVector

                    let verticalDir =
                        Vec2.init -dir.y dir.x

                    let pos = actor.objectBase.position + (100.0f *. dir)

                    let f dir =
                        let dir = Vector.normalize dir
                        Skill.AreaBuilder {
                            skillBase =
                                {
                                    delay = 0u
                                    effects = [|
                                        Skill.Damage(fun atk def ->
                                            0.0f
                                        )
                                    |]
                                }
                            objectBase = ObjectBase.init (one .* 100.0f) pos

                            target = Skill.AreaTarget.Enemies

                            removeWhenHitWall = true
                            removeWhenHitActor = true

                            move = seq {
                                //for _ in 1..10 -> Skill.Stay
                                for _ in 1..60 -> Skill.Move (dir .* 10.0f)
                                for _ in 1..60 -> Skill.Scale(one .* 10.0f)
                            } |> toList
                        }

                    [
                        f dir
                        f (dir + verticalDir .* 0.3f)
                        f (dir - verticalDir .* 0.3f)
                    ]

                skill1CoolTime = 20us
                skill2CoolTime = 120us
            }
        ]
    }
}


open System


[<EntryPoint>]
let main _ =
    try
        let windowSize = appSetting.windowSize |> Vec2.toVector2DI
        asd.Engine.Initialize(
            "九十九のラビリンス"
            , windowSize.X, windowSize.Y
            , new asd.EngineOption(WindowPosition = asd.WindowPositionType.Centering)
        )
        |> function
        | false -> ()
        | true ->
    #if DEBUG
            asd.Engine.File.AddRootDirectory("Resources")
    #else
            asd.Engine.File.AddRootPackageWithPassword("Resources.pack", "password")
    #endif

            let errorHandler = Utils.ErrorHandler()

            let scene = new MainScene.MainScene(errorHandler, appSetting)

            asd.Engine.ChangeScene(scene)

            let rec loop n =
                if n < 0 then
                    //System.Console.ReadLine() |> ignore
                    ()
                else
                    try

                        while asd.Engine.DoEvents() do
                            asd.Engine.Update()
                    with e ->
                        errorHandler.CallBack(e)
                        System.Console.WriteLine(e)

                        loop (n - 1)

            loop 1

            asd.Engine.Terminate()

    with e ->
        Console.WriteLine(e)
        Console.ReadLine() |> ignore

    0
