module WhiteDungeon.Core.Game.Update.Player

//open wraikny.Tart.Helper.Extension
open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Collections
open WhiteDungeon.Core.Game
open WhiteDungeon.Core.Game.Model


let inline decrCoolTimes player =
    let inline decr x = if x = 0us then 0us else x - 1us
    player
    |> Player.mapCoolTime Skill1 decr
    |> Player.mapCoolTime Skill2 decr


let inline addExp (gameSetting : GameSetting) v (player : Player) =
    if v > 0us then
        let rec loop level exp =
            let nextExp = gameSetting.lvUpExp level
            if nextExp < exp then
                loop (level + 1us) (exp - nextExp)
            else
                (level, exp)

        let newLevel, exp = loop (Actor.level player) (player.expoint + v)

        let player =
            { player with
                expoint = exp
                expointSum = player.expointSum + v
            }
            

        if newLevel > (Actor.level player) then
            let setting =
                gameSetting.occupationSettings
                |> HashMap.find player.character.currentOccupation

            let status =
                Actor.calcStatusOf
                    setting.growthEasing
                    gameSetting.playerGrowthRateOverMax
                    gameSetting.maxLevel
                    newLevel
                    setting.status

            Actor.levelUp newLevel status player
        else
            player
            
    else
        player


let inline update player : Player =
    player
    |> Actor.update
    |> decrCoolTimes

