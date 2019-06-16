namespace WhiteDungeon.Core.Game.Model

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Advanced



type Model = {
    count : uint32

    nextPlayerID : uint32
    players : (PlayerID * Actor.Player) list

    enemies : (EnemyID * Actor.Enemy) list

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

        nextPlayerID = players |> List.length |> uint32
        players = players

        enemies = []

        skillList = Skill.SkillList.init()

        dungeonBuilder = dungeonBuilder
        dungeonModel = dungeonModel

        gameSetting = gameSetting
    }