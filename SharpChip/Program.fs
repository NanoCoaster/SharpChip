open System

open Cpu

let romPath = "/home/michi/programmieren/emulatoren/chip8/ROMs/PONG"


[<EntryPoint>]
let main argv =
    let ram = System.IO.File.ReadAllBytes romPath
    let mmu = HelperTypes.Mmu ram
    let cpu = Cpu.Cpu (mmu = mmu)

    let state = cpu.GetState ()
    printfn "State: %O" state
    let ram = mmu.GetRam ()
    
    let ramAt start count = seq {
        for i in [start..(start + count)] do 
            yield ram.[i]
    }

    printf "First 30 bytes of RAM:"
    Seq.iter (printf "%X ") <| ramAt 0 30

    printf "\nFirst 30 bytes of ROM:"
    Seq.iter (printf "%X ") <| ramAt 0x201 30

    0