namespace ProcessingGame

[<RequireQualifiedAccess>]
module EnvironmentBuilder =
    let private processors =
        (Seq.initInfinite (fun _ -> Game.emptyProcessor 10 1))
        |> Seq.take 5
        |> Seq.map (fun p -> (p.id, p))
        |> Map.ofSeq
        
    let private programs =
        (Seq.initInfinite (fun _ -> Game.emptyProgram()))
        |> Seq.take 3
        |> Seq.map (
            (Game.programWithAdo 2)
            >> (Game.programWithAdo 3)
            >> (fun p -> p.id, p))
        |> Map.ofSeq
        |> (fun a -> printfn $"{a}"; a)
        
    let sampleEnv = {Game.emptyEnvironment with programs=programs; processors=processors}