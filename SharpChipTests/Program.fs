// Learn more about F# at http://fsharp.org

open System
open Expecto
open Cpu
open Instructions
open HelperTypes

//type Cpu(?mmu: Mmu, ?pc: uint16, ?sp: int, ?i: uint16, 
//         ?reg: uint8 [], ?dt: int, ?st: int) =

let makeTestingReg x y =
    seq { yield uint8 x; yield uint8 y; for n in 0 .. 13 do yield 0uy }
    |> Seq.toArray

let opcodeTests =
    testList "Opcodes" [
        test "SNE VX VY -> Should skip to index 2" {
            let cpu = Cpu(reg = makeTestingReg 1 2)
            cpu.RunInstruction (SNE_VX_VY(0, 1))
            let { pc = pc } = cpu.GetState ()
            Expect.equal pc 2us "2"
        }

        test "ADD VX Byte -> Should add Vx(10) and KK (20) and store 30 in Vx" {
            let cpu = Cpu(reg = makeTestingReg 10 0)
            cpu.RunInstruction (ADD_VX_KK(0, 20uy))
            let { reg = reg } = cpu.GetState ()
            Expect.equal reg.[0] 30uy "30"
        }
    ]

let decodeTests =
    testList "Decoding" [
        test "Should be SUBN 2 13" {
            let decoded = Instructions.Decode 0x82D7us
            Expect.equal decoded (SUBN_VX_VY(2, 13)) "SUBN(2, 13)"
        }

        test "Should be CALL 0x01A8" {
            let decoded = Instructions.Decode 0x21A8us
            Expect.equal decoded (CALL_NNN(0x01A8us)) "CALL(0x01A8)"
        }

        test "Should be 0x01A8" {
            let result = Instructions.MakeAddress 1 0xA 8
            Expect.equal result 0x01A8us "0x01A8"
        }   
    ]

[<EntryPoint>]
let main argv =
    let config = { defaultConfig with verbosity = Logging.LogLevel.Verbose; mySpiritIsWeak = true }
    runTests config opcodeTests &&&
    runTests config decodeTests
