namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math
open wraikny.Tart.Helper.Geometry

open FSharpPlus

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model.Actor
// open WhiteDungeon.Core.Model

//type Calcs =
//    {
//        damage : Atk -> Def -> HP
//    }


type 'Model OccupationSetting =
    {
        status : ActorStatus
        skill1 : 'Model -> Actor.Actor -> Skill.SkillEmitBuilder list
        skill2 : 'Model -> Actor.Actor -> Skill.SkillEmitBuilder list

        skill1CoolTime : uint16
        skill2CoolTime : uint16
    }

module OccupationSetting =
    let inline skillOf kind x =
        kind |> function
        | Actor.Skill1 -> x.skill1CoolTime, x.skill1
        | Actor.Skill2 -> x.skill2CoolTime, x.skill2


type 'Model GameSetting_ = {
    dungeonCellSize : float32 Vec2
    minPlayerCount : int
    maxPlayerCount : int
    binarySearchCountMovingOnWall : int
    characterSize : float32 Vec2
    damageCalculation : float32 -> Actor.Actor -> Actor.Actor -> float32
    occupationSettings : Map<Occupation, 'Model OccupationSetting>
}