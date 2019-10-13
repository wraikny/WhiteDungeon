module WhiteDungeon.App.Program

open Affogato
open wraikny.MilleFeuille
open WhiteDungeon.Core
open WhiteDungeon.View

open WhiteDungeon.App.Setting

open System
open System.Threading

[<EntryPoint>]
let main _ =
    try
        let windowSize = appSetting.windowSize |> Vector2.toVector2DI
        asd.Engine.Initialize(
            "九十九のラビリンス"
            , windowSize.X, windowSize.Y
            , new asd.EngineOption(WindowPosition = asd.WindowPositionType.Centering)
        )
        |> function
        | false ->
            failwith "Failed at asd.Engine.Initialize"
        | true ->
    #if DEBUG
            asd.Engine.File.AddRootDirectory("Resources")
    #else
            asd.Engine.File.AddRootPackageWithPassword("Resources.pack", "password")
    #endif

            let errorHandler = Utils.ErrorHandler()

            let scene = new MainScene.MainScene(errorHandler, appSetting)

            asd.Engine.ChangeScene(scene)

            let sc = QueueSynchronizationContext()
            SynchronizationContext.SetSynchronizationContext(sc)

            let rec loop n =
                if n < 0 then
                    ()
                else
                    try

                        while asd.Engine.DoEvents() do
                            sc.Execute()
                            asd.Engine.Update()
                    with e ->
                        errorHandler.CallBack(e)
                        System.Console.WriteLine(e)

                        loop (n - 1)

            loop 1

            asd.Engine.Terminate()

    with
    | e ->
        // わからん……
        // n : 1 ~ 3
        // System.Exception: 未開放のインスタンスがn個存在します。 
        // 場所 asd.Particular.Helper.ThrowUnreleasedInstanceException(Int32 count)
        // 場所 asd.Engine.Terminate()
        
        Console.WriteLine(e)
        Console.ReadLine() |> ignore

    0
