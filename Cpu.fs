module Cpu

open HelperTypes
open Instructions

let (|?) = defaultArg

/// Generates a list containing the BCD-encoded digits of x in reverse (smallest first) order
let rec ToBcd x =
    if x <= 10uy then
        [x]
    else
        x % 10uy :: ToBcd (x / 10uy)

// Constructor arguments can be used for testing
type Cpu(?mmu: Mmu, ?pc: uint16, ?sp: int, ?i: uint16, 
         ?reg: uint8 [], ?dt: int, ?st: int) =
    let mmu = mmu |? Mmu ()
    let mutable pc = pc |? 0us
    let mutable sp = sp |? 0xEA0
    let mutable regI  = i |? 0us
    let reg : uint8 [] = reg |? Array.zeroCreate 16
    let mutable dt = dt |? 0
    let mutable st = st |? 0

    let rng = System.Random ()

    let push addr = 
        sp <- sp + 1
        mmu.Write16 sp addr
    
    let pop () =
        let value = mmu.Read16 sp
        sp <- sp - 1
        value

    let setFlag x =
        reg.[0xF] <- if x then 1uy else 0uy

    let bitwiseRegisterOp x y op =
        reg.[x] <- op reg.[x] reg.[y]
    

    let run instruction =
        match instruction with
        | RET           -> 
            pc <- pop ()

        | JP_NNN addr   -> 
            pc <- addr

        | CALL_NNN addr ->
            push addr
            pc <- addr

        | SE_VX_KK (x, value) ->
            if reg.[x] = value then
                pc <- pc + 2us

        | SNE_VX_KK (x, value) ->
            if reg.[x] <> value then
                pc <- pc + 2us

        | SE_VX_VY (x, y) ->
            if reg.[x] = reg.[y] then
                pc <- pc + 2us
        
        | LD_VX_KK (x, value) -> 
            reg.[x] <- value
        
        | ADD_VX_KK (x, value) ->
            reg.[x] <- reg.[x] + value
        
        | LD_VX_VY (x, y) ->
            reg.[x] <- reg.[y]

        | OR_VX_VY (x, y)  -> bitwiseRegisterOp x y (|||)
        | AND_VX_VY (x, y) -> bitwiseRegisterOp x y (&&&)
        | XOR_VX_VY (x, y) -> bitwiseRegisterOp x y (^^^)
        
        | ADD_VX_VY (x, y) -> 
            let result = int reg.[x] + int reg.[y]
            setFlag (result > 255)
            reg.[x] <- uint8 result

        | SUB_VX_VY (x, y) ->
            setFlag (reg.[x] > reg.[y])
            bitwiseRegisterOp x y (-)
        
        | SHR_VX_VY x ->
            setFlag ((reg.[x] &&& 1uy) = 1uy)
            reg.[x] <- reg.[x] >>> 1
        
        | SUBN_VX_VY (x, y) ->
            setFlag (reg.[x] < reg.[y])
            bitwiseRegisterOp y x (-)
        
        | SHL_VX_VY x ->
            setFlag ((reg.[x] &&& 0b1000_0000uy) = 1uy)
            reg.[x] <- reg.[x] <<< 1
        
        | SNE_VX_VY (x, y) ->
            if reg.[x] <> reg.[y] then
                pc <- pc + 2us
        
        | LD_I_NNN addr -> 
            regI <- addr
        
        | JP_V0_NNN addr ->
            pc <- (addr + uint16 reg.[0])

        | RND_VX_KK (x, value) -> 
            let rnd = uint8 (rng.Next (0, 255))
            let result = rnd &&& value
            reg.[x] <- result

        | LD_VX_DT x ->
            reg.[x] <- uint8 dt

        | LD_DT_VX x ->
            dt <- int reg.[x]
        
        | LD_ST_VX x ->
            st <- int reg.[x]

        | ADD_I_VX x ->
            regI <- regI + uint16 reg.[x]

        | LD_B_VX x ->
            let bcd = ToBcd reg.[x]
            mmu.Write8 (int regI) bcd.[0]
            mmu.Write8 (int regI + 1) bcd.[1]
            mmu.Write8 (int regI + 2) bcd.[2]

        | LD_I_VX x ->
            for i in [0..x] do
                mmu.Write8 (int regI + i) reg.[i]

        | LD_VX_I x ->
            for i in [0..x] do
                reg.[i] <- mmu.Read8 (int regI + i)

        | DRW_VX_VY_K _
        | SKP_VX _
        | SKNP_VX _ 
        | LD_F_VX _
        | CLS
        | LD_VX_K _ -> printf "NOT IMPLEMENTED" 


    member x.Cycle () =
        let opcode = mmu.Read16 (int pc)
        let instruction = 
            try Decode opcode
            with UnknownOpcodeException ex -> reraise ()
        
        run instruction

    member x.RunInstruction instruction =
        run instruction

    member x.GetState () = {
        mmu = mmu;
        pc = pc;
        sp = sp;
        reg = reg;
        delayTimer = dt;
        soundTimer = st;
    }