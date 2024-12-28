
object Memory:

    val MEMORY_MAX = 1 << 16 // 65,535
    var memory: Array[UINT_16] = Array.fill(MEMORY_MAX)(0)  // Same as: new Array[UINT_16](MEMORY_MAX)

    /* Memory Mapped Registers a.k.a. Memory Mapped Regions */  
    enum MMR(val flg: UINT_16): 				
        case KBSR extends MMR(toUint(0xFE00)) // Status Register - Bit [15] indicates if the keyboard has received a new character
        case KBDR extends MMR(toUint(0xFE02)) // Data Register - Bits [7:0] contain the last character typed on the keyboard

    /*
	    @ brief : Function to get positive integer from signed short value
        @ example : 0xFE00 as signed short is -512 but as signed int is 65024
    */
    def pos_addr(addr: UINT_16): Int = addr & 0xFFFF

    /*
        @ brief : Function for MMR to write a dbyte into VM memory
    */
    def mem_write(addr: UINT_16, value: UINT_16): Unit = Memory.memory(pos_addr(addr)) = value 

    /*
        @ brief : Function for MMR to read a dbyte from VM memory 
    */
    def mem_read(addr: UINT_16): UINT_16 = // Why no bounds checking (?) // Could make memory method to inc addr 

        //if(addr >= 0 && addr < MEMORY_MAX){ 

        addr match {
            case MMR.KBSR.flg => {
                
                Memory.memory(pos_addr(MMR.KBSR.flg)) = toUint(1 << 15) 
                val ch: Char = Console.in.read().toChar 	 
                Memory.memory(pos_addr(MMR.KBDR.flg)) = toUint(ch) 
            }
            case _ => Memory.memory(pos_addr(MMR.KBSR.flg)) = 0
        }
        
        Memory.memory(pos_addr(addr)) 

        // }else{
        //     println("ERROR: Memory access out of bounds")
        // }


    /*
        @ brief : Function to read LC3 Binary into VM memory
    */
    def read_image_to_memory(file_path: String): Unit = 

        // Version 1 - Reads hex character files into memory (See: https://lc3.cs.umanitoba.ca/) 
        // val hexArray: Array[String] = io.Source.fromFile(file_path).mkString.split("\\s+")
        // val littleEndian: Array[UINT_16] = hexArray.map( x => toUint(Integer.parseInt(x, 16)) )
        // End V1

        // Version 2 - Reads binary file into memory
        import java.nio.file.{Files, Paths}  
        val path = Paths.get(file_path)

        // Map array of bytes to ints for bit shifting below
        val byteArray: Array[Int] = Files.readAllBytes(path).map(_ & 0xFF)

        // Elements of array are sub arrays of length 2
        val shortArray = byteArray.grouped(2).toArray 

        // Get array of signed short ints (2 bytes)
        val littleEndian = shortArray.map{ pr => toUint({pr(0) << 8} | {if(pr.length == 2) pr(1) else 0}) } 
        // End V2

        // Split array to program's origin and data portions
        val (origin, data): (Array[UINT_16], Array[UINT_16]) = littleEndian.splitAt(1) 

        if(debug) println(s"Highest Valid Program Address: ${origin(0) + data.length} == ${origin(0)} ++ ${data.length}")

        // Read program data into LC3 memory
        Array.copy(data, 0, Memory.memory, origin(0), data.length) 
