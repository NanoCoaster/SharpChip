module Instructions

// TODO: Benchmark this. Maybe use an array?
/// Returns a list containing all nibbles of the number (Big Endian)
let GetNibbles (number: uint16) =
    [
        int (number >>> 12);
        (int number >>> 8) &&& 0x000F;
        (int number >>> 4) &&& 0x000F;
        int number &&& 0x000F;
    ]

/// Shifts the values a b c together to create a 12-bit number
let MakeAdress a b c = 
    (((uint16 a <<< 8) ||| (uint16 b) <<< 4) ||| uint16 c)

/// Shifts a b together to create an 8-bit number
let MakeByte a b =
    (uint8 a <<< 4) ||| uint8 b

type IntPair = int * int

type Instruction =
    // Bitwise Register Operations
    | OR_VX_VY of IntPair
    | AND_VX_VY of IntPair
    | XOR_VX_VY of IntPair
    // Arithmetic Register Operations
    | ADD_VX_VY of IntPair
    | ADD_VX_KK of int * uint8
    | SUB_VX_VY of IntPair
    | SHR_VX_VY of int
    | SUBN_VX_VY of IntPair
    | SHL_VX_VY of int
    | ADD_I_VX of int
    // Jumps & Calls
    | JP_NNN of uint16
    | CALL_NNN of uint16
    | JP_V0_NNN of uint16
    | RET
    // Branching
    | SE_VX_KK of int * uint8
    | SNE_VX_KK of int * uint8
    | SE_VX_VY of IntPair
    | SNE_VX_VY of IntPair
    // Keyboard Input
    | SKP_VX of int
    | SKNP_VX of int
    | LD_VX_K of int
    // Loads
    | LD_VX_KK of int * uint8
    | LD_VX_VY of IntPair
    | LD_I_NNN of uint16
    | LD_VX_DT of int
    | LD_DT_VX of int
    | LD_ST_VX of int
    | LD_I_VX of int
    | LD_VX_I of int
    // PPU
    | CLS
    | LD_F_VX of int
    | DRW_VX_VY_K of uint8 * uint8 * uint8
    // Misc
    | RND_VX_KK of int * uint8 /// Generate random 0 <= n <= 255, AND with kk, store in Reg[x]
    | LD_B_VX of int /// Store BCD representation of Reg[x] in memory, starting at I

exception UnknownOpcodeException of uint16

let Decode opcode = 
    match GetNibbles opcode with
    | [0; 0; 0xE; 0]   -> CLS
    | [0; 0; 0xE; 0xE] -> RET
    | [1; a; b; c]     -> JP_NNN (MakeAdress a b c)
    | [2; a; b; c]     -> CALL_NNN (MakeAdress a b c)
    | [3; x; k1; k2]   -> SE_VX_KK (x, (MakeByte k1 k2))
    | [4; x; k1; k2]   -> SNE_VX_KK (x, (MakeByte k1 k2))
    | [5; x; y; 0]     -> SE_VX_VY (x, y)
    | [6; x; k1; k2]   -> LD_VX_KK (x, (MakeByte k1 k2))
    | [7; x; k1; k2]   -> ADD_VX_KK (x, (MakeByte k1 k2))
    | [8; x; y; 0]     -> LD_VX_VY (x, y)
    | [8; x; y; 1]     -> OR_VX_VY (x, y)
    | [8; x; y; 2]     -> AND_VX_VY (x, y)
    | [8; x; y; 3]     -> XOR_VX_VY (x, y)
    | [8; x; y; 4]     -> ADD_VX_VY (x, y)
    | [8; x; y; 5]     -> SUB_VX_VY (x, y)
    | [8; x; y; 6]     -> SHR_VX_VY x
    | [8; x; y; 7]     -> SUBN_VX_VY (x, y)
    | [8; x; y; 0xE]   -> SHL_VX_VY x
    | [9; x; y; 0]     -> SNE_VX_VY (x, y)
    | [0xA; a; b; c]   -> LD_I_NNN (MakeAdress a b c)
    | [0xB; a; b; c]   -> JP_V0_NNN (MakeAdress a b c)
    | [0xC; x; k1; k2] -> RND_VX_KK (x, (MakeByte k1 k2))
    | [0xD; x; y; n]   -> DRW_VX_VY_K (uint8 x, uint8 y, uint8 n)
    | [0xE; x; 9; 0xE] -> SKP_VX x
    | [0xE; x; 0xA; 1] -> SKNP_VX x
    | [0xF; x; 0; 7]   -> LD_VX_DT x
    | [0xF; x; 0; 0xA] -> LD_VX_K x
    | [0xF; x; 1; 5]   -> LD_DT_VX x
    | [0xF; x; 1; 8]   -> LD_ST_VX x
    | [0xF; x; 1; 0xE] -> ADD_I_VX x
    | [0xF; x; 2; 9]   -> LD_F_VX x
    | [0xF; x; 3; 3]   -> LD_B_VX x
    | [0xF; x; 5; 5]   -> LD_I_VX x
    | [0xF; x; 6; 5]   -> LD_VX_I x
    | _                -> raise (UnknownOpcodeException opcode)