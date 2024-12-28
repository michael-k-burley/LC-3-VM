
/* IMPORTS */
import scala.sys.process.Process 

/* GLOBALS */
var running = true 
val debug = false

/* DEBUG */
var instr_cnt = 0
val MAX_NUM_INSTR = 50000 

// Type alias for signed short (2 bytes)
type UINT_16 = Short 
def toUint(i: Int): UINT_16 = i.toShort 

/* Registers for LC3 */
enum Reg:
    case R0, R1, R2, R3, R4, R5, R6, R7, PC, COND, COUNT

/* Array of mutable memory */
var registers: Array[UINT_16] = Array.ofDim[UINT_16](Reg.COUNT.ordinal)

/* Conditional Flag */
enum CF(val flg: UINT_16):
    case POS  extends CF(1 << 0)
    case ZERO extends CF(1 << 1)
    case NEG  extends CF(1 << 2)

/* Companion Object for CF */  // Only 1 flag should be set at a time && at least 1 flag set at all times 
object CF:
	def update_flags = (dr: UINT_16) => 
		registers(Reg.COND.ordinal) = 0
		registers(Reg.COND.ordinal) = registers(dr).signum match // Update condition register
			case -1 => CF.NEG.flg
			case  0 => CF.ZERO.flg
			case  1 => CF.POS.flg
			

// [TESTING] Print out all registers contents as binary strings  
def print_registers(): Unit = 
	
	val label_value = (i: Int, reg: Short) =>  
		i match {
			case 9 => s"Cond: ${ if(reg == 1) then "POS" else if(reg == 2) then "ZERO" else "NEG" }"
			case 8 => s"PC: $reg"
			case _ => s"R$i: $reg"
		} 

	registers.zipWithIndex.foreach( (r, i) => 
			val rb = (0+r).toBinaryString.takeRight(16)
			println(s"${"0" * (16-rb.length) ++ rb}  ${label_value(i, r)}"))


@main
def main(args: String*): Unit =  

	if(debug) println(">>> START <<<")

	// Returns the runtime object associated with the current Java application.
	val runt: Runtime = Runtime.getRuntime() 

	// Update terminal settings (turn off echo and set 1 char as minimum char for read) 
	var cmd = Array("/bin/sh", "-c", "stty -F /dev/tty -echo -icanon min 1")
	val proc/*: Process */ = runt.exec(cmd)
	if(debug) println(s"exitcode-1: ${ proc.waitFor() }") 
	
	/* Read binary LC3 object file into memory */
	Memory.read_image_to_memory("./LC3_programs/bin/2048.obj")

	/* Set the Zero flag */
	registers(Reg.COND.ordinal) = CF.ZERO.flg

    /* Set the PC to program origin in VM memory */
	registers(Reg.PC.ordinal) = 12288  

	lazy val program = while running do { 
		
		/* Fetch next instruction from memory, according to address in program counter */ 
		val addr: UINT_16 = registers(Reg.PC.ordinal)
		val instr: UINT_16 = Memory.mem_read(addr)
		
		/* Get op code from next instruction */
		val op_code = (instr >> 12) & 0xF
		val op = OP.fromOrdinal(op_code) 

		/* Increment program counter */
		registers(Reg.PC.ordinal) = toUint(registers(Reg.PC.ordinal) + 1) 

		if(debug){ 
			if(instr_cnt > MAX_NUM_INSTR) return // early exit
			//Thread.sleep(100) 				 // milliseconds
			println(s"\n>>> I :: $instr_cnt")
			instr_cnt += 1

			val bts = (0+instr).toBinaryString.takeRight(16)
			val hxs = (0+instr).toHexString.takeRight(4)
			println(s"${addr} --> ${hxs} ${op} ${"0" * (16-bts.length) ++ bts} ")
			print_registers() 
		}
		
		op match // Match on opcode
			case OP.ADD  => OP.add(instr)
			case OP.AND  => OP.and(instr)
			case OP.BR   => OP.br(instr)
			case OP.JMP  => OP.jmp(instr)
			case OP.JSR  => OP.jsr(instr)
			case OP.LD   => OP.ld(instr)
			case OP.LDI  => OP.ldi(instr)
			case OP.LDR  => OP.ldr(instr)
			case OP.LEA  => OP.lea(instr)
			case OP.NOT  => OP.not(instr)
			case OP.RTI  => OP.rti(instr)
			case OP.ST   => OP.st(instr)
			case OP.STI  => OP.sti(instr)
			case OP.STR  => OP.str(instr)
			case OP.TRAP => OP.trap(instr)
			case OP.RES  => OP.res(instr)
	}

	if(debug){ // Redirect STD OUT to a log file for debugging
		import java.io.{File, FileOutputStream} 
		Console.withOut(new FileOutputStream(new File("Instruction_Log.txt")))(program)
	} else {
		program
	}
	
	// Reset terminal mode (Might not be strictly necesssary)
	cmd = Array("/bin/sh", "-c", "stty -F /dev/tty echo icanon")
	if(debug) println(s"exitcode-2: ${ runt.exec(cmd).waitFor() }")

	if(debug) println(">>> END <<<")

end main
