
/* IMPORTS */
import java.util.regex.{Pattern, Matcher}

/* GLOBALS */
val reg = raw"\\e|\\033|\\u([0-9a-fA-f]{4})|\\x([0-9a-fA-f]{2,4})\[" 
val pattern: Pattern = Pattern.compile(reg)


/* Trap Flags */ 
enum TRAP_V(val flg: Int): 
    case GETC  extends TRAP_V(0x20)   /* get character from keyboard, not echoed onto the terminal */
    case OUT   extends TRAP_V(0x21)   /* output a character */
    case PUTS  extends TRAP_V(0x22)   /* output a word string */
    case IN    extends TRAP_V(0x23)   /* get character from keyboard, echoed onto the terminal */
    case PUTSP extends TRAP_V(0x24)   /* output a byte string */
    case HALT  extends TRAP_V(0x25)   /* halt the program */


/* Companion Object for Trap */ 
object TRAP_V:  

    def match_t(param: UINT_16): TRAP_V = 
        param match 
            case 0x20 => TRAP_V.GETC 
            case 0x21 => TRAP_V.OUT  
            case 0x22 => TRAP_V.PUTS 
            case 0x23 => TRAP_V.IN   
            case 0x24 => TRAP_V.PUTSP
            case 0x25 => TRAP_V.HALT 


    def getc(): Unit  = 

        val ch: Char = Console.in.read().toChar 
        registers(Reg.R0.ordinal) = toUint(0x00FF & ch.toShort) // Clear high 8 bits


    def out(): Unit  = 

        val value = 0x00FF & registers(Reg.R0.ordinal)
        val ch: Char = value.toChar
        print(f"$ch%c") // Same as: printf("%c", ch)
    
    
    def puts(): Unit  = 

        var addr: UINT_16 = registers(Reg.R0.ordinal) 
        var word: UINT_16 = Memory.mem_read(addr) 
        val strbuf: StringBuffer = new StringBuffer 

        while word != 0 do
            strbuf.append((word & 0x00FF).toChar)  
            addr = toUint(addr + 1)                 
            word = Memory.mem_read(addr) 

        print(ansi_escape_code(strbuf.toString))
    
    
    def in(): Unit  = 

        println("TRAP: IN -- Enter a character")
        val ch = Console.in.read().toChar  
        registers(Reg.R0.ordinal) = toUint(0x00FF & ch.toShort) // Clear high 8 bits 
        print(f"${registers(Reg.R0.ordinal)}%c") // Same as: printf("%c",  registers(Reg.R0.ordinal));


    def putsp(): Unit  =  

        var addr: UINT_16 = registers(Reg.R0.ordinal)
        var word: UINT_16 = Memory.mem_read(addr) 
        val strbuf: StringBuffer = new StringBuffer 
        // A character string consisting of an odd number of characters to be written will have
        // x00 in bits [15:8] of the memory location containing the last character to be written
        while word != 0 do
            strbuf.append(s"${(word & 0x00FF).toChar}${(word & 0xFF00).toChar}")  
            addr = toUint(addr + 1)                                              
            word = Memory.mem_read(addr) 
        
        print(ansi_escape_code(strbuf.toString))	
    
    
    def halt(): Unit  = { println("TRAP: HALT"); running = false } 


/*
	@ brief : Converts string of ansi escape sequences to single unicode codepoint [ Uses regex. Probably not ideal ]
                Replaces control sequence introducer \e, \033, \u, \x from ansi cmds with unicode \u
    
    @ Called By : puts, putsp
*/
def ansi_escape_code(input: String): String = 

	val matcher: Matcher = pattern.matcher(input) 
	val sb = new StringBuffer()  

	while(matcher.find()) do 

		val codepoint = matcher.group().take(2) match {
			case "\\e" | "\\0"	=> s"${'\u001b'}"
			case "\\u"		=> { 
				val hx = Integer.parseInt(matcher.group(1), 16)
				val ch = Character.toChars(hx) 	// This could return char array of length 1 or 2. 
				ch.length match {
					case 1 => s"${ch(0)}"
					case 2 => s"${ch(0)}" 		// Incorrect - Need to combine surrgate pair to get unicode codepoint
					case _ => s"${'\u001b'}"
				}
			} 
			case "\\x"		=> {
				val hx = Integer.decode(s"0x${matcher.group(2)}")
				val ch = Character.toChars(hx) 	
				ch.length match {
					case 1 => s"${ch(0)}["
					case 2 => s"${ch(0)}[" 		// ^^^ Same issue as above ^^^
					case _ => s"${'\u001b'}["
				} 
			}
		}

		// Append chars from input string until current match
		matcher.appendReplacement(sb, codepoint)
	
	// Append remaining chars from input string after final match
	matcher.appendTail(sb)

	// Return string buffer converted to string
	sb.toString
