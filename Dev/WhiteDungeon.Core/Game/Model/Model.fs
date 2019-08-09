namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Advanced

open FSharpPlus

type GameSceneMode =
    | HowToControl
    | Stair
    | Pause
    | GameMode
    //| GameOver
    | GameFinished of back:bool


type Model = {
    count : uint32

    nextPlayerID : uint32
    players : Map<PlayerID, Actor.Player>

    enemies : Map<EnemyID, Actor.Enemy>

    dungeonBuilder: Dungeon.DungeonBuilder
    dungeonModel : Dungeon.DungeonModel
    dungeonGateCells : int Vec2 list

    skillList : Skill.SkillList

    gameSetting : GameSetting

    timePassed : bool

    mode : GameSceneMode
}


module Model =
    let inline count (model : Model) = model.count

    let inline nextPlayerID (model : Model) = model.nextPlayerID
    let inline players (model : Model) = model.players

    let inline dungeonBuilder (model : Model) = model.dungeonBuilder
    let inline dungeonModel (model : Model) = model.dungeonModel

    let inline gameSetting (model : Model) = model.gameSetting

    let inline init players dungeonBuilder dungeonModel dungeonGateCells gameSetting = {
        count = 0u

        nextPlayerID = players |> Map.count |> uint32
        players = players

        enemies = Map.empty

        skillList = Skill.SkillList.init()

        dungeonBuilder = dungeonBuilder
        dungeonModel = dungeonModel
        dungeonGateCells = Seq.toList dungeonGateCells //|> Set.ofSeq

        gameSetting = gameSetting

        timePassed = false

        mode = HowToControl
    }