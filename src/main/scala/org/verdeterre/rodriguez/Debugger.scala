package org.verdeterre.rodriguez

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream

import scala.collection.mutable.Map

object Debugger {

    val cpu = new MOS6502(new MemoryMapper)

    var cursor = 0

    var breakpoints = Set.empty[Int]

    val markers = Map.empty[String, Int]

    def parseInt(str: String): Int = {
        val marker = """@([A-Za-z0-9_]+)""".r
        val negativeHex = """(-)\$([0-9A-Za-z]+)""".r
        val hex = """\$([0-9A-Za-z]+)""".r
        val dec = """(-?\d+)""".r
        str match {
            case marker(name) => markers(name)
            case "pc" => cpu.c
            case negativeHex(sign, hexStr) => Integer.parseInt(sign + hexStr, 16)
            case hex(hexStr) => Integer.parseInt(hexStr, 16)
            case dec(decStr) => Integer.parseInt(decStr)
        }
    }

    // Colors -----------------------------------------------------

    val BLACK   = 30
    val RED     = 31
    val GREEN   = 32
    val YELLOW  = 33
    val BLUE    = 34
    val MAGENTA = 35
    val CYAN    = 36
    val WHITE   = 37
    val RESET   = 0

    def colorCode(color: Int): String = "\033[" + color + "m"

    def setColor(color: Int) {
        print(colorCode(color))
    }

    // Formatting strings -----------------------------------------

    val BYTE_FORMAT                      = "$%02X"
    val TWO_BYTE_FORMAT                  = "$%04X"
    val MEMORY_FORMAT                    = colorCode(YELLOW) + TWO_BYTE_FORMAT + colorCode(RESET) + "    " + colorCode(CYAN) + BYTE_FORMAT + colorCode(RESET)
    val MNEMONIC_FORMAT                  = "%3s"
    val INSTRUCTION_PREFIX_FORMAT        = colorCode(YELLOW) + TWO_BYTE_FORMAT + colorCode(RESET) + "    " + colorCode(GREEN) + MNEMONIC_FORMAT + colorCode(RESET)
    val IMPLIED_OPERAND_FORMAT           = ""
    val ACCUMULATOR_OPERAND_FORMAT       = "A"
    val IMMEDIATE_OPERAND_FORMAT         = colorCode(MAGENTA) + "#" + colorCode(RESET) + BYTE_FORMAT
    val RELATIVE_OPERAND_FORMAT          = colorCode(CYAN) + BYTE_FORMAT + colorCode(RESET)
    val ZERO_PAGE_OPERAND_FORMAT         = colorCode(CYAN) + BYTE_FORMAT + colorCode(RESET)
    val ZERO_PAGE_X_OPERAND_FORMAT       = colorCode(CYAN) + BYTE_FORMAT + colorCode(RESET) + ",X"
    val ZERO_PAGE_Y_OPERAND_FORMAT       = colorCode(CYAN) + BYTE_FORMAT + colorCode(RESET) + ",Y"
    val ABSOLUTE_OPERAND_FORMAT          = colorCode(CYAN) + TWO_BYTE_FORMAT + colorCode(RESET)
    val ABSOLUTE_X_OPERAND_FORMAT        = colorCode(CYAN) + TWO_BYTE_FORMAT + colorCode(RESET) + ",X"
    val ABSOLUTE_Y_OPERAND_FORMAT        = colorCode(CYAN) + TWO_BYTE_FORMAT + colorCode(RESET) + ",Y"
    val INDEXED_INDIRECT_OPERAND_FORMAT  = "(" + colorCode(CYAN) + BYTE_FORMAT + colorCode(RESET) + ",X)"
    val INDIRECT_INDEXED_OPERAND_FORMAT  = "(" + colorCode(CYAN) + BYTE_FORMAT + colorCode(RESET) + "),Y"
    val INDIRECT_ABSOLUTE_OPERAND_FORMAT = "(" + colorCode(CYAN) + TWO_BYTE_FORMAT + colorCode(RESET) + ")"

    // Instructions -----------------------------------------------

    val opcodes = Map[Int, (String, AddressingMode.Value)](
        0x69 -> ("ADC", AddressingMode.Immediate),
        0x65 -> ("ADC", AddressingMode.ZeroPage),
        0x75 -> ("ADC", AddressingMode.ZeroPageX),
        0x6D -> ("ADC", AddressingMode.Absolute),
        0x7D -> ("ADC", AddressingMode.AbsoluteX),
        0x79 -> ("ADC", AddressingMode.AbsoluteY),
        0x61 -> ("ADC", AddressingMode.IndexedIndirect),
        0x71 -> ("ADC", AddressingMode.IndirectIndexed),

        0x29 -> ("AND", AddressingMode.Immediate),
        0x25 -> ("AND", AddressingMode.ZeroPage),
        0x35 -> ("AND", AddressingMode.ZeroPageX),
        0x2D -> ("AND", AddressingMode.Absolute),
        0x3D -> ("AND", AddressingMode.AbsoluteX),
        0x39 -> ("AND", AddressingMode.AbsoluteY),
        0x21 -> ("AND", AddressingMode.IndexedIndirect),
        0x31 -> ("AND", AddressingMode.IndirectIndexed),

        0x0A -> ("ASL", AddressingMode.Accumulator),
        0x06 -> ("ASL", AddressingMode.ZeroPage),
        0x16 -> ("ASL", AddressingMode.ZeroPageX),
        0x0E -> ("ASL", AddressingMode.Absolute),
        0x1E -> ("ASL", AddressingMode.AbsoluteX),

        0x90 -> ("BCC", AddressingMode.Relative),

        0xB0 -> ("BCS", AddressingMode.Relative),

        0xF0 -> ("BEQ", AddressingMode.Relative),

        0x24 -> ("BIT", AddressingMode.ZeroPage),
        0x2C -> ("BIT", AddressingMode.Absolute),

        0x30 -> ("BMI", AddressingMode.Relative),

        0xD0 -> ("BNE", AddressingMode.Relative),

        0x10 -> ("BPL", AddressingMode.Relative),

        0x00 -> ("BRK", AddressingMode.Implied),

        0x50 -> ("BVC", AddressingMode.Relative),

        0x70 -> ("BVS", AddressingMode.Relative),

        0x18 -> ("CLC", AddressingMode.Implied),

        0xD8 -> ("CLD", AddressingMode.Implied),

        0x58 -> ("CLI", AddressingMode.Implied),

        0xB8 -> ("CLV", AddressingMode.Implied),

        0xC9 -> ("CMP", AddressingMode.Immediate),
        0xC5 -> ("CMP", AddressingMode.ZeroPage),
        0xD5 -> ("CMP", AddressingMode.ZeroPageX),
        0xCD -> ("CMP", AddressingMode.Absolute),
        0xDD -> ("CMP", AddressingMode.AbsoluteX),
        0xD9 -> ("CMP", AddressingMode.AbsoluteY),
        0xC1 -> ("CMP", AddressingMode.IndexedIndirect),
        0xD1 -> ("CMP", AddressingMode.IndirectIndexed),

        0xE0 -> ("CPX", AddressingMode.Immediate),
        0xE4 -> ("CPX", AddressingMode.ZeroPage),
        0xEC -> ("CPX", AddressingMode.Absolute),

        0xC0 -> ("CPY", AddressingMode.Immediate),
        0xC4 -> ("CPY", AddressingMode.ZeroPage),
        0xCC -> ("CPY", AddressingMode.Absolute),

        0xC6 -> ("DEC", AddressingMode.ZeroPage),
        0xD6 -> ("DEC", AddressingMode.ZeroPageX),
        0xCE -> ("DEC", AddressingMode.Absolute),
        0xDE -> ("DEC", AddressingMode.AbsoluteX),

        0xCA -> ("DEX", AddressingMode.Implied),

        0x88 -> ("DEY", AddressingMode.Implied),

        0x49 -> ("EOR", AddressingMode.Immediate),
        0x45 -> ("EOR", AddressingMode.ZeroPage),
        0x55 -> ("EOR", AddressingMode.ZeroPageX),
        0x4D -> ("EOR", AddressingMode.Absolute),
        0x5D -> ("EOR", AddressingMode.AbsoluteX),
        0x59 -> ("EOR", AddressingMode.AbsoluteY),
        0x41 -> ("EOR", AddressingMode.IndexedIndirect),
        0x51 -> ("EOR", AddressingMode.IndexedIndirect),

        0xE6 -> ("INC", AddressingMode.ZeroPage),
        0xF6 -> ("INC", AddressingMode.ZeroPageX),
        0xEE -> ("INC", AddressingMode.Absolute),
        0xFE -> ("INC", AddressingMode.AbsoluteX),

        0xE8 -> ("INX", AddressingMode.Implied),

        0xC8 -> ("INY", AddressingMode.Implied),

        0x4C -> ("JMP", AddressingMode.Absolute),
        0x6C -> ("JMP", AddressingMode.IndirectAbsolute),

        0x20 -> ("JSR", AddressingMode.Absolute),

        0xA9 -> ("LDA", AddressingMode.Immediate),
        0xA5 -> ("LDA", AddressingMode.ZeroPage),
        0xB5 -> ("LDA", AddressingMode.ZeroPageX),
        0xAD -> ("LDA", AddressingMode.Absolute),
        0xBD -> ("LDA", AddressingMode.AbsoluteX),
        0xB9 -> ("LDA", AddressingMode.AbsoluteY),
        0xA1 -> ("LDA", AddressingMode.IndexedIndirect),
        0xB1 -> ("LDA", AddressingMode.IndirectIndexed),

        0xA2 -> ("LDX", AddressingMode.Immediate),
        0xA6 -> ("LDX", AddressingMode.ZeroPage),
        0xB6 -> ("LDX", AddressingMode.ZeroPageY),
        0xAE -> ("LDX", AddressingMode.Absolute),
        0xBE -> ("LDX", AddressingMode.AbsoluteY),

        0xA0 -> ("LDY", AddressingMode.Immediate),
        0xA4 -> ("LDY", AddressingMode.ZeroPage),
        0xB4 -> ("LDY", AddressingMode.ZeroPageX),
        0xAC -> ("LDY", AddressingMode.Absolute),
        0xBC -> ("LDY", AddressingMode.AbsoluteX),

        0x4A -> ("LSR", AddressingMode.Accumulator),
        0x46 -> ("LSR", AddressingMode.ZeroPage),
        0x56 -> ("LSR", AddressingMode.ZeroPageX),
        0x4E -> ("LSR", AddressingMode.Absolute),
        0x5E -> ("LSR", AddressingMode.AbsoluteX),

        0xEA -> ("NOP", AddressingMode.Implied),

        0x09 -> ("ORA", AddressingMode.Immediate),
        0x05 -> ("ORA", AddressingMode.ZeroPage),
        0x15 -> ("ORA", AddressingMode.ZeroPageX),
        0x0D -> ("ORA", AddressingMode.Absolute),
        0x1D -> ("ORA", AddressingMode.AbsoluteX),
        0x19 -> ("ORA", AddressingMode.AbsoluteY),
        0x01 -> ("ORA", AddressingMode.IndexedIndirect),
        0x11 -> ("ORA", AddressingMode.IndirectIndexed),

        0x48 -> ("PHA", AddressingMode.Implied),

        0x08 -> ("PHP", AddressingMode.Implied),

        0x68 -> ("PLA", AddressingMode.Implied),

        0x28 -> ("PLP", AddressingMode.Implied),

        0x2A -> ("ROL", AddressingMode.Accumulator),
        0x26 -> ("ROL", AddressingMode.ZeroPage),
        0x36 -> ("ROL", AddressingMode.ZeroPageX),
        0x2E -> ("ROL", AddressingMode.Absolute),
        0x3E -> ("ROL", AddressingMode.AbsoluteX),

        0x6A -> ("ROR", AddressingMode.Accumulator),
        0x66 -> ("ROR", AddressingMode.ZeroPage),
        0x76 -> ("ROR", AddressingMode.ZeroPageX),
        0x6E -> ("ROR", AddressingMode.Absolute),
        0x7E -> ("ROR", AddressingMode.AbsoluteX),

        0x40 -> ("RTI", AddressingMode.Implied),

        0x60 -> ("RTS", AddressingMode.Implied),

        0xE9 -> ("SBC", AddressingMode.Immediate),
        0xE5 -> ("SBC", AddressingMode.ZeroPage),
        0xF5 -> ("SBC", AddressingMode.ZeroPageX),
        0xED -> ("SBC", AddressingMode.Absolute),
        0xFD -> ("SBC", AddressingMode.AbsoluteX),
        0xF9 -> ("SBC", AddressingMode.AbsoluteY),
        0xE1 -> ("SBC", AddressingMode.IndexedIndirect),
        0xF1 -> ("SBC", AddressingMode.IndirectIndexed),

        0x38 -> ("SEC", AddressingMode.Implied),

        0xF8 -> ("SED", AddressingMode.Implied),

        0x78 -> ("SEI", AddressingMode.Implied),

        0x85 -> ("STA", AddressingMode.ZeroPage),
        0x95 -> ("STA", AddressingMode.ZeroPageX),
        0x8D -> ("STA", AddressingMode.Absolute),
        0x9D -> ("STA", AddressingMode.AbsoluteX),
        0x99 -> ("STA", AddressingMode.AbsoluteY),
        0x81 -> ("STA", AddressingMode.IndexedIndirect),
        0x91 -> ("STA", AddressingMode.IndirectIndexed),

        0x86 -> ("STX", AddressingMode.ZeroPage),
        0x96 -> ("STX", AddressingMode.ZeroPageY),
        0x8E -> ("STX", AddressingMode.Absolute),

        0x84 -> ("STY", AddressingMode.ZeroPage),
        0x94 -> ("STY", AddressingMode.ZeroPageX),
        0x8C -> ("STY", AddressingMode.Absolute),

        0xAA -> ("TAX", AddressingMode.Implied),

        0xA8 -> ("TAY", AddressingMode.Implied),

        0xBA -> ("TSX", AddressingMode.Implied),

        0x8A -> ("TXA", AddressingMode.Implied),

        0x9A -> ("TXS", AddressingMode.Implied),

        0x98 -> ("TYA", AddressingMode.Implied)
    )

    val instructionFormats = Map[AddressingMode.Value, (Int, String)](
        AddressingMode.Implied          -> (1, INSTRUCTION_PREFIX_FORMAT + IMPLIED_OPERAND_FORMAT),
        AddressingMode.Accumulator      -> (1, INSTRUCTION_PREFIX_FORMAT + " " + ACCUMULATOR_OPERAND_FORMAT),
        AddressingMode.Immediate        -> (2, INSTRUCTION_PREFIX_FORMAT + " " + IMMEDIATE_OPERAND_FORMAT),
        AddressingMode.Relative         -> (2, INSTRUCTION_PREFIX_FORMAT + " " + RELATIVE_OPERAND_FORMAT),
        AddressingMode.ZeroPage         -> (2, INSTRUCTION_PREFIX_FORMAT + " " + ZERO_PAGE_OPERAND_FORMAT),
        AddressingMode.ZeroPageX        -> (2, INSTRUCTION_PREFIX_FORMAT + " " + ZERO_PAGE_X_OPERAND_FORMAT),
        AddressingMode.ZeroPageY        -> (2, INSTRUCTION_PREFIX_FORMAT + " " + ZERO_PAGE_Y_OPERAND_FORMAT),
        AddressingMode.Absolute         -> (3, INSTRUCTION_PREFIX_FORMAT + " " + ABSOLUTE_OPERAND_FORMAT),
        AddressingMode.AbsoluteX        -> (3, INSTRUCTION_PREFIX_FORMAT + " " + ABSOLUTE_X_OPERAND_FORMAT),
        AddressingMode.AbsoluteY        -> (3, INSTRUCTION_PREFIX_FORMAT + " " + ABSOLUTE_Y_OPERAND_FORMAT),
        AddressingMode.IndexedIndirect  -> (2, INSTRUCTION_PREFIX_FORMAT + " " + INDEXED_INDIRECT_OPERAND_FORMAT),
        AddressingMode.IndirectIndexed  -> (2, INSTRUCTION_PREFIX_FORMAT + " " + INDIRECT_INDEXED_OPERAND_FORMAT),
        AddressingMode.IndirectAbsolute -> (3, INSTRUCTION_PREFIX_FORMAT + " " + INDIRECT_ABSOLUTE_OPERAND_FORMAT)
    )

    def instructionString: String = {
        val opcode = cpu.readByte(cursor)
        val (mnemonic, addressingMode) = opcodes(opcode)
        val (bytes, instructionFormat) = instructionFormats(addressingMode)
        val format = bytes match {
            case 1 => instructionFormat.format(cursor, mnemonic)
            case 2 => instructionFormat.format(cursor, mnemonic, cpu.readByte(cursor + 1))
            case 3 => instructionFormat.format(cursor, mnemonic, cpu.readTwoBytes(cursor + 1))
        }
        cursor += bytes
        format
    }

    def list(startAddress: Int, endAddress: Int) {
        cursor = startAddress
        while (cursor <= endAddress) println(instructionString)
    }

    def listToOp(startAddress: Int, opcode: Int) {
        cursor = startAddress
        while (cpu.readByte(cursor) != opcode) println(instructionString)
    }

    // Memory -----------------------------------------------------

    def read(startAddress: Int, endAddress: Int) {
        for (i <- startAddress to endAddress) println(MEMORY_FORMAT.format(i, cpu.readByte(i)))
    }

    def readFor(startAddress: Int, count: Int) {
        for (i <- startAddress until startAddress + count) println(MEMORY_FORMAT.format(i, cpu.readByte(i)))
    }

    def write(startAddress: Int, values: Array[Int]) {
        for (i <- 0 until values.size) cpu.writeByte(startAddress + i, values(i))
    }

    def zero(startAddress: Int, endAddress: Int) {
        for (i <- startAddress to endAddress) cpu.writeByte(i, 0)
    }

    // Processor information --------------------------------------

    def printRegs() {
        println("pc: " + TWO_BYTE_FORMAT.format(cpu.c))
        println("a:  " + BYTE_FORMAT.format(cpu.a))
        println("x:  " + BYTE_FORMAT.format(cpu.x))
        println("y:  " + BYTE_FORMAT.format(cpu.y))
        println("p:  " + BYTE_FORMAT.format(cpu.p))
        println("s:  " + BYTE_FORMAT.format(cpu.s))
    }

    def printReg(reg: String) {
        reg match {
            case "pc" => println(TWO_BYTE_FORMAT.format(cpu.c))
            case "a"  => println(BYTE_FORMAT.format(cpu.a))
            case "x"  => println(BYTE_FORMAT.format(cpu.x))
            case "y"  => println(BYTE_FORMAT.format(cpu.y))
            case "p"  => println(BYTE_FORMAT.format(cpu.p))
            case "s"  => println(BYTE_FORMAT.format(cpu.s))
        }
    }

    def setReg(reg: String, value: Int) {
        reg match {
            case "pc" => cpu.c = value
            case "a"  => cpu.a = value
            case "x"  => cpu.x = value
            case "y"  => cpu.y = value
            case "p"  => cpu.p = value
            case "s"  => cpu.s = value
        }
    }

    def reg(command: Array[String]) {
        command.size match {
            case 1 => printRegs()
            case 2 => printReg(command(1))
            case 3 => setReg(command(1), parseInt(command(2)))
        }
    }

    def printFlags() {
        println("c: " + (if (cpu.isFlagSet(cpu.C_FLAG)) 1 else 0))
        println("z: " + (if (cpu.isFlagSet(cpu.Z_FLAG)) 1 else 0))
        println("i: " + (if (cpu.isFlagSet(cpu.I_FLAG)) 1 else 0))
        println("d: " + (if (cpu.isFlagSet(cpu.D_FLAG)) 1 else 0))
        println("b: " + (if (cpu.isFlagSet(cpu.B_FLAG)) 1 else 0))
        println("u: " + (if (cpu.isFlagSet(cpu.U_FLAG)) 1 else 0))
        println("v: " + (if (cpu.isFlagSet(cpu.V_FLAG)) 1 else 0))
        println("n: " + (if (cpu.isFlagSet(cpu.N_FLAG)) 1 else 0))
    }

    def printFlag(flag: String) {
        flag match {
            case "c" => println(if (cpu.isFlagSet(cpu.C_FLAG)) 1 else 0)
            case "z" => println(if (cpu.isFlagSet(cpu.Z_FLAG)) 1 else 0)
            case "i" => println(if (cpu.isFlagSet(cpu.I_FLAG)) 1 else 0)
            case "d" => println(if (cpu.isFlagSet(cpu.D_FLAG)) 1 else 0)
            case "b" => println(if (cpu.isFlagSet(cpu.B_FLAG)) 1 else 0)
            case "u" => println(if (cpu.isFlagSet(cpu.U_FLAG)) 1 else 0)
            case "v" => println(if (cpu.isFlagSet(cpu.V_FLAG)) 1 else 0)
            case "n" => println(if (cpu.isFlagSet(cpu.N_FLAG)) 1 else 0)
        }
    }

    def setFlag(flag: String, value: Boolean) {
        flag match {
            case "c" => cpu.setFlag(cpu.C_FLAG, value)
            case "z" => cpu.setFlag(cpu.Z_FLAG, value)
            case "i" => cpu.setFlag(cpu.I_FLAG, value)
            case "d" => cpu.setFlag(cpu.D_FLAG, value)
            case "b" => cpu.setFlag(cpu.B_FLAG, value)
            case "u" => cpu.setFlag(cpu.U_FLAG, value)
            case "v" => cpu.setFlag(cpu.V_FLAG, value)
            case "n" => cpu.setFlag(cpu.N_FLAG, value)
        }
    }

    def flag(command: Array[String]) {
        command.size match {
            case 1 => printFlags()
            case 2 => printFlag(command(1))
            case 3 => setFlag(command(1), command(2) != "0")
        }
    }

    // Breakpoints ------------------------------------------------

    def bp(command: Array[String]) {
        command.size match {
            case 1 => breakpoints.foreach(address => println(TWO_BYTE_FORMAT.format(address)))
            case 2 => if (command(1) == "clear") breakpoints = Set.empty[Int]
            case 3 => {
                val address = parseInt(command(2))
                command(1) match {
                    case "set"   => breakpoints += address
                    case "clear" => breakpoints -= address
                }
            }
        }
    }

    // Markers ----------------------------------------------------

    def mark(command: Array[String]) {
        command.size match {
            case 1 => markers.foreach(entry => println(entry._1 + ": " + TWO_BYTE_FORMAT.format(entry._2)))
            case 3 => command(1) match {
                case "clear" => markers -= command(2)
                case _ => markers(command(1)) = parseInt(command(2))
            }
        }
    }

    // File IO ----------------------------------------------------

    def save(filename: String, startAddress: Int, endAddress: Int) {
        val out = new BufferedOutputStream(new FileOutputStream(filename))
        out.write(startAddress & 0xFF)
        out.write(startAddress >> 8)
        for (i <- startAddress to endAddress) out.write(cpu.readByte(i))
        out.close()
    }

    def load(command: Array[String]) {
        val in = new BufferedInputStream(new FileInputStream(command(1)))
        var address = in.read() | (in.read() << 8)
        if (command.size == 3) address = parseInt(command(2))
        println("Loading at " + TWO_BYTE_FORMAT.format(address))
        var c = in.read()
        while (c != -1) {
            cpu.writeByte(address, c)
            address += 1
            c = in.read()
        }
        in.close()
    }

    // Execution --------------------------------------------------

    def run() {
        var shouldStop = false
        while (! shouldStop) {
            cpu.step()
            if (breakpoints(cpu.c)) shouldStop = true
        }
    }

    // Main -------------------------------------------------------

    def message() {
        setColor(CYAN)
        println("+----------------------------------------------------------+")
        println("| Rodriguez Debugger                                       |")
        println("+----------------------------------------------------------+")
        setColor(RESET)
    }

    def main(args: Array[String]) {
        message()
        var quit = false
        while (! quit) {
            try {
                val command = System.console.readLine("> ").split(" ")
                command(0) match {
                    case "quit"    => quit = true

                    case "res"      => { cpu.isResetRequested = true; cpu.step() }
                    case "irq"      => { cpu.isInterruptRequested = true; cpu.step() }
                    case "nmi"      => { cpu.isNonmaskableInterruptRequested = true; cpu.step() }

                    case "reg"      => reg(command)
                    case "pc"       => println(TWO_BYTE_FORMAT.format(cpu.c))
                    case "flag"     => flag(command)
                    case "list"     => list(parseInt(command(1)), parseInt(command(2)))
                    case "listtoop" => listToOp(parseInt(command(1)), parseInt(command(2)))
                    case "cycles"   => println(cpu.cycles)

                    case "ins"      => list(cpu.c, cpu.c)

                    case "read"     => read(parseInt(command(1)), parseInt(command(2)))
                    case "readfor"  => readFor(parseInt(command(1)), parseInt(command(2)))
                    case "write"    => write(parseInt(command(1)), command.slice(2, command.size).map(s => parseInt(s)))
                    case "zero"     => zero(parseInt(command(1)), parseInt(command(2)))

                    case "mark"     => mark(command)

                    case "bp"       => bp(command)

                    case "step"     => cpu.step()
                    case "run"      => run()

                    case "save"     => save(command(1), parseInt(command(2)), parseInt(command(3)))
                    case "load"     => load(command)

                    case _          => println("invalid command")
                }
            }
            catch {
                case e: Exception => println("error: " + e)
            }
        }
    }

}
