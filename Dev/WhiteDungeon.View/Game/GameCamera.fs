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

    
    let mutable targetPosition = ValueNone
    let mutable currentPosition = ValueNone

    member val Zoom = 0.8f with get, set
    member val Speed = 0.4f with get, set

    override this.OnUpdate() =
        targetPosition
        |> ValueOption.iter(fun targetPosition ->
            currentPosition |> function
            | ValueNone ->
                this.SetSrc(targetPosition)
                currentPosition <- ValueSome targetPosition

            | ValueSome currentPosition' ->
                let dir = targetPosition - currentPosition'
                let length = Vector.length dir

                let update(diff) =
                    let nextPosition = currentPosition' + diff
                    currentPosition <- ValueSome nextPosition
                    this.SetSrc(nextPosition)
                    
                length |> function
                | 0.0f -> ()
                | x when x <= 1.0f ->
                    currentPosition <- ValueSome targetPosition
                    this.SetSrc(targetPosition)
                | _ ->
                    let diff = (Vector.normalize dir) .* this.Speed .* length
                    let nextPosition = currentPosition' + diff
                    currentPosition <- ValueSome nextPosition
                    this.SetSrc(nextPosition)
        )

    interface IObserver<ViewModel.CameraView list> with
        member this.OnNext(cameras) =
            // TODO
            let cameraView = cameras |> head
            targetPosition <- ValueSome cameraView.position

        member __.OnError(e) = raise e
        member __.OnCompleted() = printfn "GameCamera Completed"


    member this.SetSrc(srcPos : float32 Vec2) =

        let size = asd.Engine.WindowSize.To2DF() / this.Zoom
        let size = size.To2DI()

        this.Src <- new asd.RectI(
            (Vec2.toVector2DI (int <!> srcPos)) - size / 2
            , size
        )