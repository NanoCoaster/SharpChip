open System

open Cpu

[<EntryPoint>]
let main argv =
    let cpu = Cpu.Cpu ()
    printfn "State: %O" (cpu.GetState ())

    0