namespace ProcessingGame
open System
open System

(*
Process is a keyword for future use; so "process" is "Ado" in this program

Overview of game ...
Programs hold "ados" that take a certain amount of time to process.
Processors take "ados". They process them in the order the user places them.
Once the user places an "ado", it can't be moved.
Each tick, the processor processes a certain portion of the "ado" in it.
If a processor processes all the "ado", the "ado" leaves the processor only if all "ados" that are a part of the program from which the "ado" came are also done.
If not, the processor continues to process other "ados" in it; however, the limit of "ados" that can fit in it does not decrease.

The number of programs processed in a set number of ticks is the score.
The challenge is to separate out the "ados" in such a way as to get the most programs completed (i.e., out of the processors) in the least amount of ticks.

Maybe add "power" to the processor to make it more challenging ...
some processors are faster than others
*)

type Ado = {id: Guid; size: int; adone: int; programId: Guid}
type Status =
    | Ready of Ado      // ready to start processing
    | Running of Ado    // not finished processing
    | Waiting of Ado    // finished processing but program still not done
    | Done of Ado       // finished processing and program is done

type Processor = {id: Guid; size: int; ados: Status list; power: int}
type Program = {id: Guid; size: int; readyAdos: Map<Guid, Status>}
type Environment = {programs: Map<Guid, Program>; 
                    processors: Map<Guid, Processor>; 
                    ticks: int}
type Request =
    | Move of Ado * Processor
    | Tick of TimeSpan
    | End
type Result =
    | Success of Environment
    | Error of string * Environment 

type MaybeBuilder() =
    member x.Bind(m, f) = Option.bind f m
    member x.Return(r) = Some r

[<RequireQualifiedAccess>]
module Game =

    // ==== Move Transform ====
    module private Move =
        let statusTransform (X: Ado->Status) = function
            Ready y | Running y | Waiting y | Done y -> X y 
        
        let statusId: (Status->Guid) = function
            Ready y | Running y | Waiting y | Done y -> y.id
        
        let statusSizeRunWait: (Status->int) = function
            | Ready _ | Done _ -> 0
            | Running y | Waiting y -> y.size

        let size (ados: Status list) = 
            ados
            |> List.sumBy statusSizeRunWait

        let transform ado (processor: Processor) env =
            // get the states of "ready" program, ado, and processor
            let maybe = MaybeBuilder()
            let readyProgramAndAdo = maybe {
                let! program = env.programs.TryFind ado.programId
                let! readyAdo = program.readyAdos.TryFind ado.id
                return (program, readyAdo)
            }
            let readyProcessor = maybe {
                let! cpu = env.processors.TryFind processor.id
                let! ready = // notice how ready is not optional (that's because of bind)
                    let hasRoom = cpu.size > (size cpu.ados) + ado.size
                    if hasRoom then Some(cpu)
                    else None
                return ready
            }

            // direct transformations depending on "ready" states
            match readyProgramAndAdo, readyProcessor with
            | Some (program, s), Some processor' ->
                let program' =
                    {program with readyAdos = program.readyAdos.Remove (statusId s)}
                let processor'' =
                    let s' = statusTransform Running s // unwraps current ado to wrap it in a Running status
                    {processor' with ados = s'::processor'.ados}
                let programs = env.programs |> Map.add program'.id program'
                let processors = env.processors |> Map.add processor''.id processor''
                Success({env with programs=programs; processors=processors})
            | _, None ->
                Error("Processor can't accept process!", env)
            | None, _ ->
                Error("Program doesn't have requested process!", env)

    // ==== Tick Transform ====
    module private Tick =
    
        let tickProcessor _ p : Processor =
            // transforms power and status depending on how much power is left and how much ado is left
            let tickAdo power adoStatus =
                match adoStatus with
                | Running ado ->
                    let minAdd = Math.Min((ado.size - ado.adone), power)
                    let leftOverPower = power - minAdd
                    let ado' = {ado with adone=ado.adone + minAdd}
                    if ado'.adone >= ado'.size then (leftOverPower, Waiting ado')
                    else (leftOverPower, Running ado')
                | _ -> (power, adoStatus)
            
            // transforms ados by ticking if the previous isn't currently in Running status
            let tickAdoProcess power ados =
                let rec looper powerLeft ados' result = seq{
                    match powerLeft, ados' with
                    | _, [] -> yield result
                    | 0, _ -> yield ados'@(result |> List.rev)
                    | _, f::rest ->
                        let powerLeft', adoStatus' = tickAdo powerLeft f
                        yield! looper powerLeft' rest (adoStatus'::result)
                }
                (looper power ados []) |> Seq.last 
            
            {p with ados = p.ados |> List.rev |> tickAdoProcess p.power}
        
        // the ados that are ready in a program but aren't being processed yet
        let theReadies (_, program) =
            let statusValue : (Status -> Ado) = function
                Ready y | Running y | Waiting y | Done y -> y
            
            program.readyAdos
            |> Map.toSeq
            |> Seq.map (fun (_, status) -> statusValue status)
        
        // the ados that are running
        let theRunners (_, processor) =
            processor.ados
            |> List.choose (fun x -> match x with | Running w -> Some(w) | _ -> None)
            |> Set.ofList
        
        // the ados that are waiting
        let theWaiters (_, processor) =
            processor.ados
            |> List.choose (fun x-> match x with | Waiting w -> Some(w) | _ -> None)
            |> Set.ofList
        
        let filteredAdos condition lat =
            lat
            |> Map.toSeq
            |> Seq.collect condition
            |> Set.ofSeq
            |> Set.map (fun r -> r.programId)
        
        let transformDones (dones: Guid list) p =
            let switch ds = function
                | Ready x -> Ready x
                | Running x -> Running x
                | Waiting x when ds |> List.exists (fun d -> d = x.programId) -> Done x
                | Waiting x -> Waiting x
                | Done x -> Done x
            let switched = p.ados |> List.map (switch dones)
            
            let (|NotComplete|Complete|) = function
                | Ready _ | Running _ | Waiting _ -> NotComplete
                | Done _ -> Complete
            let categorize = (|NotComplete|Complete|)
            let categorized = switched |> List.groupBy categorize |> Map.ofList
            
            let notDones = categorized.TryFind (Choice<unit,unit>.Choice1Of2())
            let dones' = categorized.TryFind (Choice<unit,unit>.Choice2Of2())
            
            match dones', notDones with
            | Some(d), Some(n) -> {p with ados=n@d}
            | Some(d), None -> {p with ados=d}
            | _, _ -> p 
        
        let transform env =
            // this is where you tick by one ... if the process is done, then it leaves
            // the processor if all program's ados are in Waiting status
            let processors = env.processors |> Map.map tickProcessor
            let newlyDones =
                (filteredAdos theWaiters processors) - (filteredAdos theRunners processors) - (filteredAdos theReadies env.programs)
                |> Set.toList
            if newlyDones.IsEmpty then Success({env with processors=processors; ticks=env.ticks+1})
            else
                processors
                |> Map.map (fun _ p -> if p.ados.IsEmpty then p else transformDones newlyDones p)
                |> (fun processors' -> Success({env with processors=processors'; ticks=env.ticks+1}))
    
    // ==== constructors, etc.
    let emptyProgram () =
        {id=Guid.NewGuid(); size=0; readyAdos=Map.empty}

    let programWithAdo adoSize program =
        let ado = {id=Guid.NewGuid(); size=adoSize; adone=0; programId=program.id}
        {program with readyAdos=program.readyAdos.Add(ado.id, Ready(ado)); size=program.size+ado.size}

    let emptyProcessor size power =
        {id=Guid.NewGuid(); size=size; ados=[]; power=power}

    let emptyEnvironment =
        {programs=Map.empty; processors=Map.empty; ticks=0}

    let buildMoveRequest ado processor =
        Move(ado, processor)

    let buildTickRequest amount =
        Tick(amount)

    let buildEndRequest =
        End 

    let transform request env = 
        match request with
        | Move(ado, processor) -> Move.transform ado processor env 
        | Tick _ -> Tick.transform env
        | End -> Success(env)