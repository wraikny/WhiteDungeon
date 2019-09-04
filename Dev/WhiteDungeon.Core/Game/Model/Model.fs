namespace WhiteDungeon.Core.Game.Model

open wraikny.Tart.Helper.Math

open WhiteDungeon.Core.Model
open WhiteDungeon.Core.Game.Model

open wraikny.Tart.Advanced
open wraikny.Tart.Core
open wraikny.Tart.Core.Libraries
open wraikny.Tart.Advanced.Dungeon
open wraikny.Tart.Helper.Geometry
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

    dungeonFloor : uint32
}


module Model =
    let inline count (model : Model) = model.count

    let inline nextPlayerID (model : Model) = model.nextPlayerID
    let inline players (model : Model) = model.players
    
    let inline dungeonBuilder (model : Model) = model.dungeonBuilder
    let inline dungeonModel (model : Model) = model.dungeonModel

    let inline gameSetting (model : Model) = model.gameSetting

    let cellsToEnemies (enemyCells : (int * int Vec2) []) cellSize =
        enemyCells
        |> Seq.indexed
        |> Seq.map(fun (index, (i, cell)) ->
            let enemyId = EnemyID <| uint32 index
            enemyId
            , Actor.Enemy.init
                (Vec2.init 100.0f 100.0f)
                ( (DungeonModel.cellToCoordinate cellSize cell) + (cellSize .* 0.5f) )
                enemyId
                {   ActorStatus.level = 1
                    hp = 100.0f
                    walkSpeed = 10.0f
                    dashSpeed = 16.0f
                }
        )
        |> Map.ofSeq

    let inline init players dungeonBuilder dungeonModel dungeonGateCells enemyCells gameSetting = {
        gameSetting = gameSetting
        count = 0u

        nextPlayerID = players |> Map.count |> uint32
        players = players

        enemies = cellsToEnemies enemyCells gameSetting.dungeonCellSize

        skillList = Skill.SkillList.init()

        dungeonBuilder = dungeonBuilder
        dungeonModel = dungeonModel
        dungeonGateCells = Seq.toList dungeonGateCells |> Set.ofSeq


        timePassed = false

        mode = HowToControl

        lastCollidedGate = false

        dungeonFloor = 1u
    }

module Dungeon =
    type GeneratedDungeonParams = {
        dungeonBuilder : DungeonBuilder
        dungeonModel : DungeonModel
        gateCells : int Vec2 Set
        enemyCells : (int * int Vec2) []
        initPosition : float32 Vec2
    }

    let generateDungeonModel (dungeonBuilder : DungeonBuilder) =
        Random.int minValue<int> maxValue<int>
        |> TartTask.withEnv(fun seed -> async {
            let builder = { dungeonBuilder with seed = seed }
            return(
                builder,
                { dungeonBuilder with seed = seed }
                |> DungeonBuilder.generate
            )
        })

    let generateDungeonParams gameSetting gateCount dungeonBuilder (dungeonModel : DungeonModel) =
        let largeRooms = toList dungeonModel.largeRooms
        let smallRooms = toList dungeonModel.smallRooms
        
        let largeRoomsCount = length largeRooms
        let smallRoomsCount = length smallRooms

        let fromCell =
            gameSetting.dungeonCellSize
            |> DungeonModel.cellToCoordinate

        let rec loop() = monad {
            try
                let smallRoomGen = Random.int 0 (smallRoomsCount - 1)
                let largeRoomGen = Random.int 0 (largeRoomsCount - 1)

                let! roomIndex = largeRoomGen

                let initRoomIndex = roomIndex % largeRoomsCount

                let initRoom = snd largeRooms.[initRoomIndex]

                let initPosition =
                    initRoom.rect
                    |>> fromCell
                    |> Rect.centerPosition

                let! gateCellIndexs =
                    Random.distinctList gateCount (Random.pair largeRoomGen largeRoomGen)
                    |> Random.until(List.map fst >> List.contains roomIndex >> not)

                let gateCells =
                    seq {
                        for (a, b) in gateCellIndexs ->
                            let room = snd largeRooms.[a % largeRoomsCount]
                            let cells = room |> Space.cells
                            let cell = fst cells.[ b % length cells]
                            cell
                    }
                    |> Set.ofSeq

                let! enemyCellIndexs=
                    Random.list smallRoomsCount smallRoomGen

                let enemyCells =
                    seq {
                        for (index, (_, space)) in Seq.indexed smallRooms ->
                            let cells = space |> Space.cells
                            let cellIndex = (enemyCellIndexs : int list).[index]
                            let cell = fst cells.[ cellIndex % length cells]
                            (cellIndex, cell)
                    }
                    |> Seq.toArray

                return {
                    dungeonBuilder = dungeonBuilder
                    dungeonModel = dungeonModel
                    gateCells = gateCells
                    enemyCells = enemyCells
                    initPosition = initPosition
                }
            with _ -> return! loop()
        }

        flip Random.generate (loop())
