namespace WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Advanced

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

    let inline init players dungeonBuilder dungeonModel gameSetting = {
        count = 0u

        nextPlayerID = players |> Map.count |> uint32
        players = players

        enemies = Map.empty

        skillList = Skill.SkillList.init()

        dungeonBuilder = dungeonBuilder
        dungeonModel = dungeonModel

        gameSetting = gameSetting

        timePassed = false

        mode = HowToControl
    }