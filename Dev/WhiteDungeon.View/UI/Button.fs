namespace WhiteDungeon.View.UI

open wraikny.MilleFeuille.Core.UI


[<AbstractClass>]
type ButtonRectangle< 'Owner
    when 'Owner :> ButtonRectangle<'Owner>
    >(name, size, font) =
    inherit asd.GeometryObject2D(
        Shape =
            new asd.RectangleShape(
                DrawingArea = new asd.RectF(-size / 2.0f, size)
            )
    )

    let mutable hoverCount = 0


    let font = font

    let textObject =
        new asd.TextObject2D(
            Text = ""
            , Font = font
        )


    member __.HoverCount with get() = hoverCount


    member val Button = new ControllerButtonComponent<'Owner>(name) with get


    member this.Text
        with get() = textObject.Text
        and  set(value) =
            let size =
                font.CalcTextureSize
                    (value, asd.WritingDirection.Horizontal)

            textObject.Text <- value
            textObject.Position <- this.Position - size.To2DF() / 2.0f


    member val DefaultColor =
        Some <| new asd.Color(0uy, 0uy, 0uy)
        with get, set

    member val HoverColor = fun (count : int) ->
        let t = float32 count / 60.0f
        let b = ((1.0f - cos t) / 2.0f * 100.0f + 50.0f) |> byte
        Some <| new asd.Color(b, b, b)
        with get, set

    member val HoldColor =
        Some <| new asd.Color(150uy, 150uy, 150uy)
        with get, set

    abstract SetButtonColor : unit -> unit


    override this.OnAdded() =
        this.AddDrawnChild(
            textObject
            , asd.ChildManagementMode.RegistrationToLayer
            ||| asd.ChildManagementMode.Disposal
            ||| asd.ChildManagementMode.IsUpdated
            ||| asd.ChildManagementMode.IsDrawn
            , asd.ChildTransformingMode.All
            , asd.ChildDrawingMode.DrawingPriority
        )

        this.AddComponent(this.Button, this.Button.Name)

        this.Button.add_OnEnteredEvent(fun (_ : 'Owner) ->
            hoverCount <- 0
        )

        this.Button.add_OnReleasedEvent(fun (_ : 'Owner) ->
            hoverCount <- 0
        )

        this.Button.add_HoverEvent(fun (_ : 'Owner) ->
            hoverCount <- hoverCount + 1
        )

        this.SetButtonColor()


type SelectButtonRectangle(name, size, font) =
    inherit ButtonRectangle<SelectButtonRectangle>(name, size, font)

    override this.SetButtonColor() =
        let setDefaultColor (owner : SelectButtonRectangle) =
            owner.DefaultColor |> function
            | Some color ->
                owner.Color <- color
            | None -> ()
        
        setDefaultColor(this)


        this.Button.add_OnPushedEvent(fun (owner : SelectButtonRectangle) ->
            owner.HoldColor |> function
            | Some color ->
                owner.Color <- color
            | None -> ()
        )

        this.Button.add_HoverEvent(fun (owner : SelectButtonRectangle) ->
            (owner.HoverColor owner.HoverCount) |> function
            | Some color ->
                owner.Color <- color
            | None -> ()
        )

        this.Button.add_OnExitedEvent(fun owner ->
            setDefaultColor(owner)
        )



//type ListButtonRectangle(name, size, font) =
//    inherit ButtonRectangle<ListButtonRectangle>(name, size, font)

//    member val IsSelected = false with get, set

//    member val SelectedDefaultColor = new asd.Color(200uy, 100uy, 100uy) with get, set
//    member val SelectedDefaultTextColor = new asd.Color(255uy, 255uy, 255uy) with get, set

//    member val SelectedHoverColor = fun (count : int) -> new asd.Color(100uy, 200uy, 100uy) with get, set
//    member val SelectedHoverTextColor = fun (count : int) -> new asd.Color(255uy, 255uy, 255uy) with get, set

//    member val SelectedHoldColor = new asd.Color(150uy, 150uy, 200uy) with get, set
//    member val SelectedHoldTextColor = new asd.Color(255uy, 255uy, 255uy) with get, set


//    override this.SetButtonColor() =
//        let setHoverColor (owner : ListButtonRectangle) =
//            if owner.IsSelected then
//                owner.Color <- owner.SelectedHoverColor owner.HoverCount
//                owner.TextColor <- owner.SelectedHoverTextColor owner.HoverCount
//            else
//                owner.Color <- owner.HoverColor owner.HoverCount
//                owner.TextColor <- owner.HoverTextColor owner.HoverCount
    

//        this.Button.add_OnEnteredEvent(fun owner -> setHoverColor owner)

//        this.Button.add_OnPushedEvent(fun (owner : ListButtonRectangle) ->
//            if owner.IsSelected then
//                owner.Color <- owner.SelectedHoldColor
//                owner.TextColor <- owner.SelectedHoldTextColor
//            else
//                owner.Color <- owner.HoldColor
//                owner.TextColor <- owner.HoldTextColor
//        )

//        this.Button.add_OnReleasedEvent(fun owner -> setHoverColor owner)

//        this.Button.add_OnExitedEvent(fun (owner : ListButtonRectangle) ->
//            if owner.IsSelected then
//                owner.Color <- owner.SelectedDefaultColor
//                owner.TextColor <- owner.SelectedDefaultTextColor
//            else
//                owner.Color <- owner.DefaultColor
//                owner.TextColor <- owner.DefaultTextColor
//        )


open WhiteDungeon.View
open WhiteDungeon.View.Utils.Color


module ButtonRectangle =
    let setReiwaColor (button : ButtonRectangle<'a>) =
        button.DefaultColor <- Some ColorPalette.sakura

        button.HoldColor <- Some ColorPalette.ume

        button.HoverColor <- fun count ->
            let t = float32 count / 30.0f
            let x = (1.0f - cos t) / 2.0f
            let c0 = ColorPalette.sakura
            let c1 = ColorPalette.ume

            let f a b =
                let a, b = float32 a, float32 b
                let c = int (a + (b - a) * x) % 255
                byte c

            new asd.Color(
                f c0.R c1.R
                , f c0.G c1.G
                , f c0.B c1.B
            ) |> Some

        button

    let inline create text font position size =
        new SelectButtonRectangle(
            text + "_Button" 
            , size
            , font
            , Text = text
            , Position = position
        )
        |> setReiwaColor