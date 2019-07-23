namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Fs.Objects
open WhiteDungeon.Core.Game
open wraikny.MilleFeuille.Fs.Math

open System

open FSharpPlus

type GameCamera() =
    inherit asd.CameraObject2D(
        Dst = new asd.RectI(
            new asd.Vector2DI(0, 0)
            , asd.Engine.WindowSize
        )
    )


    let mutable srcPosition = zero

    member val Zoom = 1.0f with get, set

    interface IObserver<ViewModel.CameraView list> with
        member this.OnNext(cameras) =
            // TODO
            let cameraView = cameras |> head
            this.SetSrc(cameraView.position |>> int)

        member __.OnError(e) = raise e
        member __.OnCompleted() = printfn "GameCamera Completed"


    member this.SetSrc(srcPos) =
        srcPosition <- srcPos

        let size = asd.Engine.WindowSize.To2DF() / this.Zoom
        let size = size.To2DI()

        this.Src <- new asd.RectI(
            (Vec2.toVector2DI srcPos) - size / 2
            , size
        )