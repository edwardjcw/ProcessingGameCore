namespace ProcessingGame

[<RequireQualifiedAccess>]
module EnvironmentBuilder =
    let private createProcessors count power =
        (Seq.initInfinite (fun _ -> Game.emptyProcessor 10 power))
        |> Seq.take count
        |> Seq.map (fun p -> (p.id, p))
        |> Map.ofSeq

    let private createPrograms count ados =
        (Seq.initInfinite (fun _ -> Game.emptyProgram()))
        |> Seq.take count
        |> Seq.map (
            (fun p ->
                Seq.init ados (fun _ -> System.Random().Next(1, 5))
                |> Seq.fold (fun p' adoSize -> Game.programWithAdo adoSize p') p)
            >> (fun p -> p.id, p))
        |> Map.ofSeq

    let newEnv programs processors ados =
        { Game.emptyEnvironment with
            programs = createPrograms programs ados
            processors = createProcessors processors 1 }
        
    let sampleEnv = newEnv 5 3 3