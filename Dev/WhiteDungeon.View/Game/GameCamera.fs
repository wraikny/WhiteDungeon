namespace WhiteDungeon.View.Game

open wraikny
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Utils
open wraikny.Tart.Core
open wraikny.Tart.Core.View
open wraikny.MilleFeuille.Objects
open wraikny.MilleFeuille
open WhiteDungeon.Core
open WhiteDungeon.View

open System

open FSharpPlus
open FSharpPlus.Math.Applicative

type GameCamera(isMapChip) =
    inherit asd.CameraObject2D(
        Dst = new asd.RectI(
            new asd.Vector2DI(0, 0)
            , asd.Engine.WindowSize
        )
    )
    
    let mutable targetPosition = ValueNone
    let mutable currentPosition = ValueNone

    member val Zoom = 0.3f with get, set
    member val Speed = 0.2f with get, set

    override this.OnUpdate() =
        targetPosition
        |> ValueOption.iter (fun targetPosition ->
            let nextCurrent =
                currentPosition |> function
                | ValueNone -> targetPosition

                | ValueSome currentPosition' ->
                    let dir = targetPosition - currentPosition'
                    let length = Vector.length dir
                    
                    length |> function
                    | 0.0f -> currentPosition'
                    | x when x <= 1.0f -> targetPosition
                    | _ ->
                        let diff = (Vector.normalize dir) .* this.Speed .* length
                        let nextPosition = currentPosition' + diff
                        nextPosition

            currentPosition <- ValueSome nextCurrent

            let x = nextCurrent, targetPosition
            this.SetSrc(x)

#if DEBUG
    
            let pushed = asd.Engine.Keyboard.GetKeyState >> (=) asd.ButtonState.Push

            if pushed asd.Keys.Num1 then
                this.Zoom <- this.Zoom + 0.02f |> min 1.0f
                this.SetSrc(x)
            elif pushed asd.Keys.Num2 then
                this.Zoom <- this.Zoom - 0.02f |> max 0.02f
                this.SetSrc(x)
            elif pushed asd.Keys.Num3 then
                this.Zoom <- 0.4f
                this.SetSrc(x)
#endif
        )



    member this.SetSrc(current, target) =
        let srcPos = if isMapChip then current else current - target

        let size = asd.Engine.WindowSize.To2DF() / this.Zoom
        let size = size.To2DI()

        this.Src <- new asd.RectI(
            (Vec2.toVector2DI (map int srcPos)) - size / 2
            , size
        )

    interface IObserver<ViewModel.CameraView> with
        member this.OnNext(camera) =
            // TODO
            let cameraView = camera
            targetPosition <- ValueSome cameraView.position

        member __.OnError(e) = raise e
        member __.OnCompleted() = printfn "GameCamera Completed"