namespace UnitTests

open System 
open ProcessingGame
open NUnit.Framework

module Helpers =
    let adoFromStatus = function | Ready x | Running x | Waiting x | Done x -> x

    let createProgramWithAdoSizes programId (adoSizes: int list) =
        let folder (p: Program) (adoSize: int) =
            let ado = {id=Guid.NewGuid(); size=adoSize; adone=0; programId=p.id}
            {p with readyAdos=p.readyAdos.Add(ado.id, Ready(ado)); size=p.size+ado.size}
        adoSizes |> List.fold folder {id=programId; size=0; readyAdos=Map.empty}

    let tickFromResult = function
        | Success env' -> env'.ticks
        | Result.Error(p, _) -> failwith p 

    let processorsFromResult = function
        | Success env' -> env'.processors |> Map.toList |> List.map snd
        | Result.Error(p, _) -> failwith p 
        
    let retrievedProcessor (processors:Processor list) (processor:Processor) =
        let find = 
            processors
            |> List.tryFind (fun p -> p.id=processor.id)
        match find with
        | Some p -> p
        | _ -> failwith "couldn't find processor"
        
    let retrieveAdoStatus (ado:Ado) (processor:Processor) =
        let find =
            processor.ados
            |> List.tryFind (function | Ready x | Running x | Waiting x | Done x -> x.id=ado.id)
        match find with
        | Some s -> s
        | _ -> failwith "couldn't find ado in processor"

    let moveAdo (ado:Ado) (proc:Processor) lastOutput =
        match lastOutput with
        | Success(e) -> Game.transform (Move (ado, proc)) e
        | Result.Error (p,_) -> failwith(p)

    let tick lastOutput =
        let env' =
            match lastOutput with
            | Success e -> e
            | Result.Error (p,_) -> failwith(p)
        Game.transform (Tick 1) env'

open Helpers

[<TestFixture>]
type TestGeneral () =
    [<Test>]
    member this. ``One Tick Empty`` () =
        let env = { Game.emptyEnvironment with ticks = 0 }
        let outcome = Game.transform (Tick 1) env
        let expected = Success({env with ticks=1})
        Assert.AreEqual(outcome, expected)

[<TestFixture>]
type TestSimpleMove () =
    [<Test>]
    member this. ``One Ado One Processor`` () =
        let programId = Guid.NewGuid()
        let program = createProgramWithAdoSizes programId [3]
        let processor = Game.emptyProcessor 10 1
        let env = { Game.emptyEnvironment with
                        programs = Map.ofList [(programId, program)]
                        processors = Map.ofList [(processor.id, processor)] }
        let ado = (program.readyAdos |> Map.toList |> List.head |> snd |> adoFromStatus)

        let result =
            Success env
            |> moveAdo ado processor
            |> tick |> tick |> tick

        let processors = processorsFromResult result
        let p = retrievedProcessor processors processor
        let status = retrieveAdoStatus ado p
        Assert.AreEqual(3, (adoFromStatus status).adone)
        Assert.IsTrue(match status with | Status.Done _ -> true | _ -> false)

[<TestFixture>]
type TestComplexMove () =
    [<Test>]
    member this. ``Complex Move`` () =
        let p1Id = Guid.NewGuid()
        let p2Id = Guid.NewGuid()
        let p1 = createProgramWithAdoSizes p1Id [2; 3]
        let p2 = createProgramWithAdoSizes p2Id [1; 4]
        let programs = Map.ofList [ (p1Id, p1); (p2Id, p2) ]
        let cpus =
            (Seq.initInfinite (fun _ -> Game.emptyProcessor 10 1))
            |> Seq.take 2
            |> Seq.toList
        let cpu1 = cpus |> List.head
        let cpu2 = cpus |> List.tail |> List.head
        let env = { Game.emptyEnvironment with programs = programs; processors = Map.ofList [(cpu1.id, cpu1); (cpu2.id, cpu2)] }

        let ado1_s2 = (p1.readyAdos |> Map.values |> List.ofSeq |> List.find (fun s -> (adoFromStatus s).size = 2) |> adoFromStatus)
        let ado1_s3 = (p1.readyAdos |> Map.values |> List.ofSeq |> List.find (fun s -> (adoFromStatus s).size = 3) |> adoFromStatus)
        let ado2_s1 = (p2.readyAdos |> Map.values |> List.ofSeq |> List.find (fun s -> (adoFromStatus s).size = 1) |> adoFromStatus)
        
        let result =
            Success env
            |> moveAdo ado1_s3 cpu1
            |> moveAdo ado2_s1 cpu1
            |> moveAdo ado1_s2 cpu2
            |> tick |> tick |> tick |> tick |> tick

        let processors = processorsFromResult result
        let cpu1' = retrievedProcessor processors cpu1
        let cpu2' = retrievedProcessor processors cpu2

        let status1_s3 = retrieveAdoStatus ado1_s3 cpu1'
        let status1_s2 = retrieveAdoStatus ado1_s2 cpu2'
        let status2_s1 = retrieveAdoStatus ado2_s1 cpu1'

        Assert.AreEqual(3, (adoFromStatus status1_s3).adone)
        Assert.IsTrue(match status1_s3 with | Status.Done _ -> true | _ -> false)

        Assert.AreEqual(1, (adoFromStatus status2_s1).adone)
        Assert.IsTrue(match status2_s1 with | Status.Waiting _ -> true | _ -> false)

        Assert.AreEqual(2, (adoFromStatus status1_s2).adone)
        Assert.IsTrue(match status1_s2 with | Status.Done _ -> true | _ -> false)
