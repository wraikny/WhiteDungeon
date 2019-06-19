namespace WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Advanced



type Model = {
    count : uint32

    nextPlayerID : uint32
    players : Map<PlayerID, Actor.Player>

    enemies : Map<EnemyID, Actor.Enemy>

    dungeonBuilder: Dungeon.DungeonBuilder
    dungeonModel : Dungeon.DungeonModel

    skillList : Skill.SkillList

    gameSetting : GameSetting
}


module Model =
    let count (model : Model) = model.count

    let nextPlayerID (model : Model) = model.nextPlayerID
    let players (model : Model) = model.players

    let dungeonBuilder (model : Model) = model.dungeonBuilder
    let dungeonModel (model : Model) = model.dungeonModel

    let gameSetting (model : Model) = model.gameSetting

    let init players dungeonBuilder dungeonModel gameSetting = {
        count = 0u

        nextPlayerID = players |> Map.count |> uint32
        players = players

        enemies = Map.empty

        skillList = Skill.SkillList.init()

        dungeonBuilder = dungeonBuilder
        dungeonModel = dungeonModel

        gameSetting = gameSetting
    }