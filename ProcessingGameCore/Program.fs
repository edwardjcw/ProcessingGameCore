// Learn more about F# at http://fsharp.org

open ProcessingGame
open System

[<EntryPoint>]
let main _ =
    
    GamePlay.start()
    Console.ReadKey() |> ignore 
    
    0 // return an integer exit code
