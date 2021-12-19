module UnitTests

open System 
open ProcessingGame
open NUnit.Framework

let private env = EnvironmentBuilder.sampleEnv
let private oneTick = TimeSpan.FromSeconds(1.0)

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``One Tick Empty`` () =
    let outcome = Game.transform (Tick oneTick) env
    let expected = Success({env with ticks=1})
    Assert.AreEqual(outcome, expected)

let private tickFromResult = function
    | Success env' -> env'.ticks
    | Result.Error(p, _) -> failwith p 

let private processorsFromResult = function
    | Success env' -> env'.processors |> Map.toList |> List.map snd
    | Result.Error(p, _) -> failwith p 
    
let private retrievedProcessor (processors:Processor list) (processor:Processor) =
    let find = 
        processors
        |> List.tryFind (fun p -> p.id=processor.id)
    match find with
    | Some p -> p
    | _ -> failwith "couldn't find processor"
    
let private retrieveAdoStatus (ado:Ado) (processor:Processor) =
    let find =
        processor.ados
        |> List.tryFind (function | Ready x | Running x | Waiting x | Done x -> x.id=ado.id)
    match find with
    | Some s -> s
    | _ -> failwith "couldn't find ado in processor" 

[<Test>]
let ``One Move`` () =
    let adoCreated skipProgram skipAdos =
        env.programs
        |> Map.toList
        |> List.skip skipProgram
        |> List.head
        |> (fun (_,b) -> b.readyAdos)
        |> Map.toList
        |> List.skip skipAdos
        |> List.head
        |> snd
        |> function | Ready x | Running x | Waiting x | Done x -> x
        
    let processorCreated skipProcessor =
        env.processors
        |> Map.toList
        |> List.skip skipProcessor
        |> List.head
        |> snd
        
    let ado = adoCreated 0 0    // get the first "ready ado" from the first program
    let ado2 = adoCreated 0 1   // get the second "ready ado" from the first program
    let ado3 = adoCreated 2 0   // get the first "ready ado" from the third program
    let ado4 = adoCreated 2 1   // get the second "ready ado" from the third program
    let aProcessor = processorCreated 0     // get the first processor
    let aProcessor2 = processorCreated 1    // get the second processor
    let aProcessor3 = processorCreated 2    // get the third processor
    
    let move a proc lastOutput =
        match lastOutput with
        | Success(e) -> Game.transform (Move (a, proc)) e
        | Result.Error (p,_) -> failwith(p)
        
    let tick lastOutput =
        let env' =
            match lastOutput with
            | Success e -> e
            | Result.Error (p,_) -> failwith(p)
        Game.transform (Tick oneTick) env'
        
    let result =
        Success env
        |> move ado4 aProcessor3// move second "ready ado" of the third program to the third processor
        |> move ado3 aProcessor // move first "ready ado" of the third program to the first processor
        |> move ado aProcessor  // move first "ready ado" of the first program to the first processor
        |> move ado2 aProcessor2// move second "ready ado" of the first program to the second processor
        |> tick |> tick |> tick
    
    // === ASSERTIONS ===
    // 3 ticks undertaken
    Assert.AreEqual(tickFromResult result, 3)

    let getProcessor = retrievedProcessor (processorsFromResult result) 
    let rAdoStatus = retrieveAdoStatus ado (getProcessor aProcessor)
    let rAdo2Status = retrieveAdoStatus ado2 (getProcessor aProcessor2)
    let rAdo3Status = retrieveAdoStatus ado3 (getProcessor aProcessor)
    let rAdo4Status = retrieveAdoStatus ado4 (getProcessor aProcessor3) 
    
    // ado4 and ado3 are done (they ran first)
    Assert.IsTrue(rAdo4Status |> (function | Done _ -> true | _ -> false))
    Assert.IsTrue(rAdo3Status |> (function | Done _ -> true | _ -> false))
    
    // ado2 is waiting for the first "ready ado" to complete in the first processor, which did ado3 first
    Assert.IsTrue(rAdo2Status |> (function | Waiting _ -> true | _ -> false))
    
    // ado is running now that ado3 is finished in the first processor
    Assert.IsTrue(rAdoStatus |> (function | Running _ -> true | _ -> false))