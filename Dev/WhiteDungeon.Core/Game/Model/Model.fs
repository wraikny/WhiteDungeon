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
    | WaitingGenerating
    | ErrorUI of exn
    //| GameOver
    | GameFinished of back:bool


type Model = {
    count : uint32

    nextPlayerID : uint32
    players : Map<PlayerID, Actor.Player>

    enemies : Map<EnemyID, Actor.Enemy>

    dungeonBuilder: Dungeon.DungeonBuilder
    dungeonModel : Dungeon.DungeonModel
    dungeonGateCells : int Vec2 Set

    skillList : Skill.SkillList

    gameSetting : GameSetting

    timePassed : bool

    mode : GameSceneMode

    lastCollidedGate : bool
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
        dungeonGateCells = Seq.toList dungeonGateCells |> Set.ofSeq

        gameSetting = gameSetting

        timePassed = false

        mode = HowToControl

        lastCollidedGate = false
    }

open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon
open wraikny.Tart.Helper.Geometry

module Dungeon =
    type GeneratedDungeonParams = {
        dungeonBuilder : DungeonBuilder
        dungeonModel : DungeonModel
        gateCells : int Vec2 Set
        initPosition : float32 Vec2
    }

    let generateTask gameSetting (dungeonBuilder : DungeonBuilder) gateCount : GeneratedDungeonParams TartTask =
        monad {
            let! seed = Random.int minValue<int> maxValue<int>
            
            let gen = (Random.int 0 (dungeonBuilder.roomCount - 1) )

            let! roomIndex = gen
            let gateCellsGen = Random.list gateCount (Random.pair gen gen)
            let rec loop gates = monad {
                let gates1 = gates |>> fst
                if gates1 |> List.contains roomIndex || (Seq.distinct gates1 |> length <> gateCount) then
                    let! gateCells = gateCellsGen
                    return! loop gateCells
                else
                    return gates
            }
            let! gateCells = gateCellsGen
            let! gateCellIndexs = loop gateCells
            return (seed, roomIndex, gateCellIndexs)
        }
        |> TartTask.withEnv(fun (seed, roomIndex, gateCellIndexs) -> async {
            let dungeonModel =
                { dungeonBuilder with seed = seed }
                |> DungeonBuilder.generate

            let largeRooms = toList dungeonModel.largeRooms

            let largeRoomsCount = length largeRooms

            let initRoomIndex = roomIndex % largeRoomsCount

            let initRoom = snd largeRooms.[initRoomIndex]

            let fromCell =
                gameSetting.dungeonCellSize
                |> DungeonModel.cellToCoordinate

            let initPosition =
                initRoom.rect
                |>> fromCell
                |> Rect.centerPosition

            let gateCells =
                seq {
                    for (a, b) in gateCellIndexs ->
                        let room = snd largeRooms.[a % largeRoomsCount]
                        let cells = room |> Space.cells
                        let cell = fst cells.[ b % length cells]
                        cell
                }
                |> Set.ofSeq

            return {
                dungeonBuilder = dungeonBuilder
                dungeonModel = dungeonModel
                gateCells = gateCells
                initPosition = initPosition
            }
        })