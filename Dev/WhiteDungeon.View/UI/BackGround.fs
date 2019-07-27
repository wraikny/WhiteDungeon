namespace WhiteDungeon.View.UI

open wraikny.MilleFeuille.Core

type BackGroundLayer(color) =
    inherit asd.Layer2D()

    let mutable windowSize = asd.Engine.WindowSize

    let rectangle =
        new asd.RectangleShape(
            DrawingArea =
                new asd.RectF(
                    new asd.Vector2DF(0.0f, 0.0f)
                    , windowSize.To2DF()
                )
        )

    let backGroundObject =
        new asd.GeometryObject2D(
            Shape = rectangle
            , Color = color
        )

    override this.OnAdded() =
        this.AddObject(backGroundObject)


    //override this.OnUpdated() =
    //    let currentWindowSize = asd.Engine.WindowSize
    //    if windowSize <> currentWindowSize then
    //        windowSize <- currentWindowSize
    //        rectangle.DrawingArea <-
    //            new asd.RectF(
    //                new asd.Vector2DF(0.0f, 0.0f)
    //                , windowSize.To2DF()
    //            )