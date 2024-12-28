
/* Logical Shift */
def ZEXT(i: Int): UINT_16 = toUint(i)  

/* Arithmetic Shift (preserves sign) */
def SEXT(i: Int, len: Int): UINT_16 =   // (Instr, Bit Len) 
	val sign = i & (1 << (len - 1))     // Get sign bit
	val value = if(sign == 0) then i else (i | (0xFFFF << len))
	val retval = toUint(value) 
	retval

/* Opcodes for LC3 */ 
enum OP:
	case BR, 	/* branch */
	ADD,    	/* add  */
	LD,     	/* load */
	ST,     	/* store */
	JSR,    	/* jump register */
	AND,    	/* bitwise and */
	LDR,    	/* load register */
	STR,    	/* store register */
	RTI,    	/* return from interrupt */
	NOT,    	/* bitwise not */
	LDI,    	/* load indirect */
	STI,    	/* store indirect */
	JMP,    	/* jump */
	RES,    	/* reserved (unused) */
	LEA,    	/* load effective address */
	TRAP    	/* execute trap */


object OP: 

    def add(instr: UINT_16): Unit = /* ADD */

        val dr = (instr >> 9) & 0x7			
        val sr1 = (instr >> 6) & 0x7		
        val mode = (instr >> 5) & 0x1
        
        val sr2 = mode match 
            case 0 => registers(instr & 0x7)
            case 1 => SEXT(toUint(instr & 0x1F), 5)
        
        registers(dr) = toUint(registers(sr1) + sr2)
        CF.update_flags(toUint(dr))

        if(debug) println(s"ADD > DR: $dr\tSR1: $sr1\t\tmode: $mode\t\tSR2: $sr2") 


    def and(instr: UINT_16): Unit = /* AND */

        val dr = (instr >> 9) & 0x7			
        val sr1 = (instr >> 6) & 0x7	
        val mode = (instr >> 5) & 0x1

        val sr2 = mode match 
            case 0 => registers(instr & 0x7)
            case 1 => SEXT(instr & 0x1F, 5)

        registers(dr) = toUint(registers(sr1) & sr2) 
        CF.update_flags(toUint(dr))
        
        if(debug) println(s"AND > DR: $dr\tSR1: $sr1\tmode: $mode\tSR2: $sr2")


    def br(instr: UINT_16): Unit = /* BRANCH */

        val n = (instr >> 11) & 0x1	
        val z = (instr >> 10) & 0x1	
        val p = (instr >>  9) & 0x1	
        val coffset = instr & 0x1FF	

        val pos  = (p == 1) && (registers(Reg.COND.ordinal) == CF.POS.flg)
        val zero = (z == 1) && (registers(Reg.COND.ordinal) == CF.ZERO.flg)
        val neg  = (n == 1) && (registers(Reg.COND.ordinal) == CF.NEG.flg)

        if(pos || zero || neg) // If any of the conditions is met, then update the program counter 
            registers(Reg.PC.ordinal) = toUint(registers(Reg.PC.ordinal) + SEXT(coffset, 9)) 

        if(debug){
            println(s"BR > P: $p\tZ: $z\t\tN:$n\t\toffset: $coffset\t\t[test for] ")
            println(s"Pos: $pos\tZero: $zero\tNeg: $neg ")
        }
        
    
    def jmp(instr: UINT_16): Unit = /* JUMP && RET - RETURN FROM SUBROUTINE */
        
        val baser = (instr >> 6) & 0x7	

        // Return is special case of jmp where BaseR == b"111"
        registers(Reg.PC.ordinal) = (baser & 0x7) match
            case 7 => registers(Reg.R7.ordinal) 
            case _ => registers(baser) 	
    
        if(debug) println(s"JMP > BaseR: $baser")


    def jsr(instr: UINT_16): Unit = /* JUMP TO SUBROUTINE */
        
        // Store current program counter, so can jump back
        registers(Reg.R7.ordinal) = registers(Reg.PC.ordinal)

        val mode = (instr >> 11) & 0x1

        val nextPC = mode match	
            case 0 => {
                val baser = (instr >> 6) & 0x7
                registers(baser)
            }
            case 1 => toUint(registers(Reg.PC.ordinal) + SEXT(instr & 0x7FF, 11)) 

        // Update program counter to next specified value
        registers(Reg.PC.ordinal) = nextPC

        if(debug) println(s"JSR > mode: $mode\tNext PC: $nextPC") 


    def ld(instr: UINT_16): Unit = /* LOAD */
        
        val dr = (instr >> 9) & 0x7 			
        val coffset = instr & 0x1FF				

        registers(dr) = Memory.mem_read( toUint(registers(Reg.PC.ordinal) + SEXT(coffset, 9)) ) 
        CF.update_flags(toUint(dr))

        if(debug) println(s"LD > DR: $dr\toffset: $coffset")


    def ldi(instr: UINT_16): Unit = /* LOAD INDIRECT */

        val dr = (instr >> 9) & 0x7	    	
        val coffset = instr & 0x1FF				

        val addr = toUint(registers(Reg.PC.ordinal) + SEXT(coffset, 9))  
        registers(dr) = Memory.mem_read( Memory.mem_read(addr) )
        CF.update_flags(toUint(dr))

        if(debug) println(s"LDI > DR: $dr\toffset: $coffset")
    

    def ldr(instr: UINT_16): Unit = /* LOAD Base + offset */

        val dr = (instr >> 9) & 0x7				
        val baser = (instr >> 6) & 0x7			
        val offset = instr & 0x3F				

        registers(dr) = Memory.mem_read( toUint(registers(baser) + SEXT(offset, 6)) )
        CF.update_flags(toUint(dr))

        if(debug) println(s"LDR > DR: $dr\tBaseR: $baser\toffset: $offset")
    

    def lea(instr: UINT_16): Unit = /* LOAD EFFECTIVE ADDRESS */

        val dr = (instr >> 9) & 0x7 			
        val coffset = instr & 0x1FF				

        registers(dr) = toUint(registers(Reg.PC.ordinal) + SEXT(coffset, 9))
        CF.update_flags(toUint(dr))
    
        if(debug) println(s"LEA > DR: $dr\toffset: $coffset")


    def not(instr: UINT_16): Unit = /* BIT-WISE COMPLEMENT */

        val dr = (instr >> 9) & 0x7				
        val sr = (instr >> 6) & 0x7				

        registers(dr) = toUint(~registers(sr))
        CF.update_flags(toUint(dr))

        if(debug) println(s"NOT > DR: $dr\tSR: $sr")
    

    def rti(instr: UINT_16): Unit = /* RETURN FROM INTERRUPT */

        /* NOT YET FINISHED */
        ()
        // if (PSR[15] == 0)
        // 	PC = mem[R6]; R6 is the SSP
        // 	R6 = R6+1;
        // 	TEMP = mem[R6];
        // 	R6 = R6+1;
        // 	PSR = TEMP; the privilege mode and condition codes of
        // 	the interrupted process are restored
        // else initiate a privilege mode exception
    
        // if(debug) println("RTI > ")  


    def st(instr: UINT_16): Unit = /* STORE */

        val sr = (instr >> 9) & 0x7 			
        val coffset = instr & 0x1FF				

        val addr = toUint(registers(Reg.PC.ordinal) + SEXT(coffset, 9))
        Memory.mem_write(addr, registers(sr))

        if(debug) println(s"ST > SR: $sr\tcoffset: $coffset")
    

    def sti(instr: UINT_16): Unit = /* STORE INDIRECT */

        val sr = (instr >> 9) & 0x7				
        val coffset = instr & 0x1FF				

        val addr = toUint(registers(Reg.PC.ordinal) + SEXT(coffset, 9))
        Memory.mem_write(Memory.mem_read(addr), registers(sr))

        if(debug) println(s"STI > SR: $sr\tcoffset: $coffset")
    

    def str(instr: UINT_16): Unit = /* STORE Base + offset */

        val sr = (instr >> 9) & 0x7		
        val baser = (instr >> 6) & 0x7	
        val offset = instr & 0x3F		

        val addr = toUint(registers(baser) + SEXT(offset, 6))
        Memory.mem_write(addr, registers(sr)) 

        if(debug) println(s"STR > SR: $sr\tBaseR: $baser\toffset: $offset")
    

    def trap(instr: UINT_16): Unit = /* SYSTEM CALL */
        
        if(debug) print("TRAP > ")

        // Get zero extended trap vector
        val trapvect: UINT_16 = ZEXT(instr & 0x00FF)		

        // Store current program counter, so can jump back
        registers(Reg.R7.ordinal) = registers(Reg.PC.ordinal)

        // PC is loaded with the starting address of the system call specified by trapvector8
        registers(Reg.PC.ordinal) = Memory.mem_read(trapvect)

        if(debug) {
            val trapvec = (instr & 0x00FF)	
            print(s"${trapvec} ${f"${trapvec.toBinaryString}"} ")
            println(s"${TRAP_V.match_t(toUint(trapvec))}  <~< ")
        }

        TRAP_V.match_t(trapvect) match // Match on trap code 
            case TRAP_V.GETC  => TRAP_V.getc()
            case TRAP_V.OUT   => TRAP_V.out()
            case TRAP_V.PUTS  => TRAP_V.puts()
            case TRAP_V.IN    => TRAP_V.in()
            case TRAP_V.PUTSP => TRAP_V.putsp()
            case TRAP_V.HALT  => TRAP_V.halt()
        
        // Return program counter to previous instruction
        registers(Reg.PC.ordinal) = registers(Reg.R7.ordinal) 


    // Resevered for future use
    def res(intr: UINT_16): Unit = { println("ERROR: RES"); running = false }
