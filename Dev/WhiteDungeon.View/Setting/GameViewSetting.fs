namespace WhiteDungeon.View.Setting

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

type ActorImages = {
    front : string
    frontRight : string
    frontLeft : string
    back : string
    backRight : string
    backLeft : string
    right : string
    left : string
}

module ActorImages =
    let fromDirection dir images =
        let path= dir |> function
            | Actor.ActorDirection.Front -> images.front
            | Actor.ActorDirection.FrontRight -> images.frontRight
            | Actor.ActorDirection.FrontLeft -> images.frontLeft
            | Actor.ActorDirection.Back -> images.back
            | Actor.ActorDirection.BackRight -> images.backRight
            | Actor.ActorDirection.BackLeft -> images.backLeft
            | Actor.ActorDirection.Right -> images.right
            | Actor.ActorDirection.Left -> images.left

        path

type GameViewSetting = {
    occupationImages : Map<Occupation, ActorImages>
}