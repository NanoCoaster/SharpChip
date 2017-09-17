namespace HelperTypes

type Mmu(rom: uint8 []) =
    let ram : uint8 [] = 
        // 0..0x200 are set to 0  ::  the ROM bytes follow, whole array is initialized with 4kb
        let ram': uint8 [] = Array.zeroCreate 4096
        Array.blit rom 0 ram' 0x201 rom.Length
        ram'
    

    member x.Read8 addr =   
        ram.[int addr]
    
    member x.Read16 addr =
        ((uint16 ram.[int addr]) <<< 8) ||| (uint16 ram.[int (addr + 1)])
    
    member x.Write8 addr value =  
        ram.[int addr] <- value
    
    member x.Write16 addr (value: uint16) =   
        ram.[int addr] <- uint8 ((value >>> 8) &&& 0x000Fus)
        ram.[int (addr + 1)] <- uint8 value
    
    /// Just for testing
    member x.GetRam () = ram

type CpuState = {
    mmu: Mmu;
    pc: uint16;
    sp: int;
    reg: uint8 [];
    delayTimer: int;
    soundTimer: int;
}