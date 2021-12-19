namespace ProcessingGame

open ProcessingGame
open System

[<RequireQualifiedAccess>]
type UserRequest =
    | MoveUsing of Ado * Processor
    | TickAmount of int
    | Exit
    | Status
    | Help
    | Error of string

[<RequireQualifiedAccess>]    
type Message =
    | MoveComplete of ProcessingGame.Environment
    | TickDone of ProcessingGame.Environment
    | ExitReady of string
    | Status of string * ProcessingGame.Environment
    | Help of string * ProcessingGame.Environment
    | Error of string * ProcessingGame.Environment
    
type MessageRequest = UserRequest * AsyncReplyChannel<Message>

type FullEnvironment = FullEnvironment of UserRequest * Result

[<RequireQualifiedAccess>]
module GameInterface =
    
    let private envFromResult = function
        | Success(e) -> e
        | Result.Error(_, e) -> e
    
    let private tick lastOutput =
        let env =
            match lastOutput with
            | Success(e) -> e
            | Result.Error(p, _) -> failwith(p)
        Game.transform (Tick (TimeSpan.FromSeconds 1.0)) env
    
    let play help env (input: MailboxProcessor<MessageRequest>) =
        let rec looper result = async {
            let! request, reply = input.Receive()
            match request with
            | UserRequest.Exit -> reply.Reply(Message.ExitReady("done"))
            | UserRequest.MoveUsing(ado, processor) ->
                let env' = (Game.transform (Request.Move (ado, processor)) (envFromResult result))
                match env' with
                | Result.Success e -> reply.Reply(Message.MoveComplete(e))
                | Result.Error(msg, e) -> reply.Reply(Message.Error(msg, e))
                do! looper env'
            | UserRequest.TickAmount a ->
                let rec tick' last a' =
                    let latest = tick last
                    if a' - 1 = 0 then latest
                    else tick' latest (a'-1)
                let env' = tick' result a
                match env' with
                | Result.Success e -> reply.Reply(Message.TickDone(e))
                | Result.Error (msg, e) -> reply.Reply(Message.Error(msg, e))
                do! looper env'
            | UserRequest.Error error -> reply.Reply(Message.Error($"Error! {error}", envFromResult result)); do! looper result
            | UserRequest.Status -> reply.Reply(Message.Status(sprintf $"Status:\n %A{result}", envFromResult result)); do! looper result
            | UserRequest.Help -> reply.Reply(Message.Help(help, envFromResult result)); do! looper result }
        looper env
    
    let private tryMatch f v = if f then Some(v) else None
        
    let readyAdoFromId id env =
        let readyMatch status (p:string) =
            match status with
            | Ready ado -> tryMatch (ado.id.ToString().StartsWith p) ado
            | _ -> None
        let fromReadyAdos (_, program) =
            program.readyAdos
            |> Map.tryPick (fun _ status -> readyMatch status id)
        env.programs
        |> Map.toList
        |> List.tryPick fromReadyAdos
        
    let processorFromId (id:string) env =
        env.processors
        |> Map.tryPick (fun k -> tryMatch (k.ToString().StartsWith id))
        
    let (|Int|_|) (str:string) =
        match Int32.TryParse(str) with
        | true,int -> Some(int)
        | _ -> None 