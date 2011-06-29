package org.verdeterre.rodriguez

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream

object Debugger {

    implicit def hexStrToInt(str: String): Int = Integer.parseInt(str, 16)

    var quit = false
    val console = System.console()
    var command = Array.empty[String]
    var position = 0

    val cpu = new MOS6502(new MemoryMapper)

    def formatByte(byte: Int): String = "$%02X".format(byte)
    def formatTwoBytes(bytes: Int): String = "$%04X".format(bytes)

    def printByte(byte: Int) {
        println(formatByte(byte))
    }

    def printTwoBytes(bytes: Int) {
        println(formatTwoBytes(bytes))
    }

    def opcodeInfo(opcode: Int): (String, AddressingMode.Value) = {
        opcode match {
            case 0x69 => ("ADC", AddressingMode.Immediate)
            case 0x65 => ("ADC", AddressingMode.ZeroPage)
            case 0x75 => ("ADC", AddressingMode.ZeroPageX)
            case 0x6D => ("ADC", AddressingMode.Absolute)
            case 0x7D => ("ADC", AddressingMode.AbsoluteX)
            case 0x79 => ("ADC", AddressingMode.AbsoluteY)
            case 0x61 => ("ADC", AddressingMode.IndexedIndirect)
            case 0x71 => ("ADC", AddressingMode.IndirectIndexed)

            case 0x29 => ("AND", AddressingMode.Immediate)
            case 0x25 => ("AND", AddressingMode.ZeroPage)
            case 0x35 => ("AND", AddressingMode.ZeroPageX)
            case 0x2D => ("AND", AddressingMode.Absolute)
            case 0x3D => ("AND", AddressingMode.AbsoluteX)
            case 0x39 => ("AND", AddressingMode.AbsoluteY)
            case 0x21 => ("AND", AddressingMode.IndexedIndirect)
            case 0x31 => ("AND", AddressingMode.IndirectIndexed)

            case 0x0A => ("ASL", AddressingMode.Accumulator)
            case 0x06 => ("ASL", AddressingMode.ZeroPage)
            case 0x16 => ("ASL", AddressingMode.ZeroPageX)
            case 0x0E => ("ASL", AddressingMode.Absolute)
            case 0x1E => ("ASL", AddressingMode.AbsoluteX)

            case 0x90 => ("BCC", AddressingMode.Relative)

            case 0xB0 => ("BCS", AddressingMode.Relative)

            case 0xF0 => ("BEQ", AddressingMode.Relative)

            case 0x24 => ("BIT", AddressingMode.ZeroPage)
            case 0x2C => ("BIT", AddressingMode.Absolute)

            case 0x30 => ("BMI", AddressingMode.Relative)

            case 0xD0 => ("BNE", AddressingMode.Relative)

            case 0x10 => ("BPL", AddressingMode.Relative)

            case 0x00 => ("BRK", AddressingMode.Implied)

            case 0x50 => ("BVC", AddressingMode.Relative)

            case 0x70 => ("BVS", AddressingMode.Relative)

            case 0x18 => ("CLC", AddressingMode.Implied)

            case 0xD8 => ("CLD", AddressingMode.Implied)

            case 0x58 => ("CLI", AddressingMode.Implied)

            case 0xB8 => ("CLV", AddressingMode.Implied)

            case 0xC9 => ("CMP", AddressingMode.Immediate)
            case 0xC5 => ("CMP", AddressingMode.ZeroPage)
            case 0xD5 => ("CMP", AddressingMode.ZeroPageX)
            case 0xCD => ("CMP", AddressingMode.Absolute)
            case 0xDD => ("CMP", AddressingMode.AbsoluteX)
            case 0xD9 => ("CMP", AddressingMode.AbsoluteY)
            case 0xC1 => ("CMP", AddressingMode.IndexedIndirect)
            case 0xD1 => ("CMP", AddressingMode.IndirectIndexed)

            case 0xE0 => ("CPX", AddressingMode.Immediate)
            case 0xE4 => ("CPX", AddressingMode.ZeroPage)
            case 0xEC => ("CPX", AddressingMode.Absolute)

            case 0xC0 => ("CPY", AddressingMode.Immediate)
            case 0xC4 => ("CPY", AddressingMode.ZeroPage)
            case 0xCC => ("CPY", AddressingMode.Absolute)

            case 0xC6 => ("DEC", AddressingMode.ZeroPage)
            case 0xD6 => ("DEC", AddressingMode.ZeroPageX)
            case 0xCE => ("DEC", AddressingMode.Absolute)
            case 0xDE => ("DEC", AddressingMode.AbsoluteX)

            case 0xCA => ("DEX", AddressingMode.Implied)

            case 0x88 => ("DEY", AddressingMode.Implied)

            case 0x49 => ("EOR", AddressingMode.Immediate)
            case 0x45 => ("EOR", AddressingMode.ZeroPage)
            case 0x55 => ("EOR", AddressingMode.ZeroPageX)
            case 0x4D => ("EOR", AddressingMode.Absolute)
            case 0x5D => ("EOR", AddressingMode.AbsoluteX)
            case 0x59 => ("EOR", AddressingMode.AbsoluteY)
            case 0x41 => ("EOR", AddressingMode.IndexedIndirect)
            case 0x51 => ("EOR", AddressingMode.IndexedIndirect)

            case 0xE6 => ("INC", AddressingMode.ZeroPage)
            case 0xF6 => ("INC", AddressingMode.ZeroPageX)
            case 0xEE => ("INC", AddressingMode.Absolute)
            case 0xFE => ("INC", AddressingMode.AbsoluteX)

            case 0xE8 => ("INX", AddressingMode.Implied)

            case 0xC8 => ("INY", AddressingMode.Implied)

            case 0x4C => ("JMP", AddressingMode.Absolute)
            case 0x6C => ("JMP", AddressingMode.IndirectAbsolute)

            case 0x20 => ("JSR", AddressingMode.Absolute)

            case 0xA9 => ("LDA", AddressingMode.Immediate)
            case 0xA5 => ("LDA", AddressingMode.ZeroPage)
            case 0xB5 => ("LDA", AddressingMode.ZeroPageX)
            case 0xAD => ("LDA", AddressingMode.Absolute)
            case 0xBD => ("LDA", AddressingMode.AbsoluteX)
            case 0xB9 => ("LDA", AddressingMode.AbsoluteY)
            case 0xA1 => ("LDA", AddressingMode.IndexedIndirect)
            case 0xB1 => ("LDA", AddressingMode.IndirectIndexed)

            case 0xA2 => ("LDX", AddressingMode.Immediate)
            case 0xA6 => ("LDX", AddressingMode.ZeroPage)
            case 0xB6 => ("LDX", AddressingMode.ZeroPageY)
            case 0xAE => ("LDX", AddressingMode.Absolute)
            case 0xBE => ("LDX", AddressingMode.AbsoluteY)

            case 0xA0 => ("LDY", AddressingMode.Immediate)
            case 0xA4 => ("LDY", AddressingMode.ZeroPage)
            case 0xB4 => ("LDY", AddressingMode.ZeroPageX)
            case 0xAC => ("LDY", AddressingMode.Absolute)
            case 0xBC => ("LDY", AddressingMode.AbsoluteX)

            case 0x4A => ("LSR", AddressingMode.Accumulator)
            case 0x46 => ("LSR", AddressingMode.ZeroPage)
            case 0x56 => ("LSR", AddressingMode.ZeroPageX)
            case 0x4E => ("LSR", AddressingMode.Absolute)
            case 0x5E => ("LSR", AddressingMode.AbsoluteX)

            case 0xEA => ("NOP", AddressingMode.Implied)

            case 0x09 => ("ORA", AddressingMode.Immediate)
            case 0x05 => ("ORA", AddressingMode.ZeroPage)
            case 0x15 => ("ORA", AddressingMode.ZeroPageX)
            case 0x0D => ("ORA", AddressingMode.Absolute)
            case 0x1D => ("ORA", AddressingMode.AbsoluteX)
            case 0x19 => ("ORA", AddressingMode.AbsoluteY)
            case 0x01 => ("ORA", AddressingMode.IndexedIndirect)
            case 0x11 => ("ORA", AddressingMode.IndirectIndexed)

            case 0x48 => ("PHA", AddressingMode.Implied)

            case 0x08 => ("PHP", AddressingMode.Implied)

            case 0x68 => ("PLA", AddressingMode.Implied)

            case 0x28 => ("PLP", AddressingMode.Implied)

            case 0x2A => ("ROL", AddressingMode.Accumulator)
            case 0x26 => ("ROL", AddressingMode.ZeroPage)
            case 0x36 => ("ROL", AddressingMode.ZeroPageX)
            case 0x2E => ("ROL", AddressingMode.Absolute)
            case 0x3E => ("ROL", AddressingMode.AbsoluteX)

            case 0x6A => ("ROR", AddressingMode.Accumulator)
            case 0x66 => ("ROR", AddressingMode.ZeroPage)
            case 0x76 => ("ROR", AddressingMode.ZeroPageX)
            case 0x6E => ("ROR", AddressingMode.Absolute)
            case 0x7E => ("ROR", AddressingMode.AbsoluteX)

            case 0x40 => ("RTI", AddressingMode.Implied)

            case 0x60 => ("RTS", AddressingMode.Implied)

            case 0xE9 => ("SBC", AddressingMode.Immediate)
            case 0xE5 => ("SBC", AddressingMode.ZeroPage)
            case 0xF5 => ("SBC", AddressingMode.ZeroPageX)
            case 0xED => ("SBC", AddressingMode.Absolute)
            case 0xFD => ("SBC", AddressingMode.AbsoluteX)
            case 0xF9 => ("SBC", AddressingMode.AbsoluteY)
            case 0xE1 => ("SBC", AddressingMode.IndexedIndirect)
            case 0xF1 => ("SBC", AddressingMode.IndirectIndexed)

            case 0x38 => ("SEC", AddressingMode.Implied)

            case 0xF8 => ("SED", AddressingMode.Implied)

            case 0x78 => ("SEI", AddressingMode.Implied)

            case 0x85 => ("STA", AddressingMode.ZeroPage)
            case 0x95 => ("STA", AddressingMode.ZeroPageX)
            case 0x8D => ("STA", AddressingMode.Absolute)
            case 0x9D => ("STA", AddressingMode.AbsoluteX)
            case 0x99 => ("STA", AddressingMode.AbsoluteY)
            case 0x81 => ("STA", AddressingMode.IndexedIndirect)
            case 0x91 => ("STA", AddressingMode.IndirectIndexed)

            case 0x86 => ("STX", AddressingMode.ZeroPage)
            case 0x96 => ("STX", AddressingMode.ZeroPageY)
            case 0x8E => ("STX", AddressingMode.Absolute)

            case 0x84 => ("STY", AddressingMode.ZeroPage)
            case 0x94 => ("STY", AddressingMode.ZeroPageX)
            case 0x8C => ("STY", AddressingMode.Absolute)

            case 0xAA => ("TAX", AddressingMode.Implied)

            case 0xA8 => ("TAY", AddressingMode.Implied)

            case 0xBA => ("TSX", AddressingMode.Implied)

            case 0x8A => ("TXA", AddressingMode.Implied)

            case 0x9A => ("TXS", AddressingMode.Implied)

            case 0x98 => ("TYA", AddressingMode.Implied)
        }
    }

    def instruction(address: Int): (Int, String) = {
        val opcode = cpu.readByte(address)
        val (mnemonic, mode) = opcodeInfo(opcode)
        val (bytes, operand) = mode match {
            case AddressingMode.Implied          => (1, "")
            case AddressingMode.Accumulator      => (1, "A")
            case AddressingMode.Immediate        => (2, "#" + formatByte(cpu.readByte(address + 1)))
            case AddressingMode.Relative         => (2, formatByte(cpu.readByte(address + 1)))
            case AddressingMode.ZeroPage         => (2, formatByte(cpu.readByte(address + 1)))
            case AddressingMode.ZeroPageX        => (2, formatByte(cpu.readByte(address + 1)) + ",X")
            case AddressingMode.ZeroPageY        => (2, formatByte(cpu.readByte(address + 1)) + ",Y")
            case AddressingMode.Absolute         => (3, formatTwoBytes(cpu.readTwoBytes(address + 1)))
            case AddressingMode.AbsoluteX        => (3, formatTwoBytes(cpu.readTwoBytes(address + 1)) + ",X")
            case AddressingMode.AbsoluteY        => (3, formatTwoBytes(cpu.readTwoBytes(address + 1)) + ",Y")
            case AddressingMode.IndexedIndirect  => (2, "(" + formatByte(cpu.readByte(address + 1) + ",X)"))
            case AddressingMode.IndirectIndexed  => (2, "(" + formatByte(cpu.readByte(address + 1) + "),Y"))
            case AddressingMode.IndirectAbsolute => (3, "(" + formatTwoBytes(cpu.readByte(address + 1)) + ")")
        }
        (bytes, "$%04X    %s %s".format(address, mnemonic, operand))
    }

    def instructions(startingAddress: Int, num: Int = 1): Array[String] = {
        var address = startingAddress
        val results = new Array[String](num)
        for (i <- 0 until num) {
            val (bytes, instructionString) = instruction(address)
            results(i) = instructionString
            address += bytes
        }
        results
    }

    def pc() {
        if (command.size == 1) printTwoBytes(cpu.c)
        else if (command.size == 2) cpu.c = command(1)
    }

    def reg() {
        if (command.size == 1) {
            println("pc: " + formatTwoBytes(cpu.c))
            println("a:  " + formatByte(cpu.a))
            println("x:  " + formatByte(cpu.x))
            println("y:  " + formatByte(cpu.y))
            println("p:  " + formatByte(cpu.p))
            println("s:  " + formatByte(cpu.s))
        }
        else if (command.size == 2) {
            command(1) match {
                case "c" => printTwoBytes(cpu.c)
                case "a" => printByte(cpu.a)
                case "x" => printByte(cpu.x)
                case "y" => printByte(cpu.y)
                case "p" => printByte(cpu.p)
                case "s" => printByte(cpu.s)
                case _   => println("invalid register")
            }
        }
        else if (command.size == 3) {
            command(1) match {
                case "c" => cpu.c = command(2)
                case "a" => cpu.a = command(2)
                case "x" => cpu.x = command(2)
                case "y" => cpu.y = command(2)
                case "p" => cpu.p = command(2)
                case "s" => cpu.s = command(2)
                case _   => println("invalid register")
            }
        }
    }

    def list() {
        val count: Int = if (command.size == 1) 8 else command(1)
        instructions(cpu.c, count).foreach(println)
    }

    def ins() {
        val count: Int = if (command.size == 1) 8 else command(1)
        instructions(position, count).foreach(println)
    }

    def move() {
        if (command.size == 1) printByte(position)
        else if (command.size == 2) position = command(1)
        else println("error")
    }

    def read() {
        for (i <- 0 until command(1)) printByte(cpu.readByte(position + i))
    }

    def write() {
        for (i <- 1 until command.size) cpu.writeByte(position + i - 1, command(i))
    }

    def flag() {
        if (command.size == 1) {
            println("c: " + (if (cpu.isFlagSet(cpu.C_FLAG)) 1 else 0))
            println("z: " + (if (cpu.isFlagSet(cpu.Z_FLAG)) 1 else 0))
            println("i: " + (if (cpu.isFlagSet(cpu.I_FLAG)) 1 else 0))
            println("d: " + (if (cpu.isFlagSet(cpu.D_FLAG)) 1 else 0))
            println("b: " + (if (cpu.isFlagSet(cpu.B_FLAG)) 1 else 0))
            println("u: " + (if (cpu.isFlagSet(cpu.U_FLAG)) 1 else 0))
            println("v: " + (if (cpu.isFlagSet(cpu.V_FLAG)) 1 else 0))
            println("n: " + (if (cpu.isFlagSet(cpu.N_FLAG)) 1 else 0))
        }
        else if (command.size == 2) {
            command(1) match {
                case "c" => println(if (cpu.isFlagSet(cpu.C_FLAG)) 1 else 0)
                case "z" => println(if (cpu.isFlagSet(cpu.Z_FLAG)) 1 else 0)
                case "i" => println(if (cpu.isFlagSet(cpu.I_FLAG)) 1 else 0)
                case "d" => println(if (cpu.isFlagSet(cpu.D_FLAG)) 1 else 0)
                case "b" => println(if (cpu.isFlagSet(cpu.B_FLAG)) 1 else 0)
                case "u" => println(if (cpu.isFlagSet(cpu.U_FLAG)) 1 else 0)
                case "v" => println(if (cpu.isFlagSet(cpu.V_FLAG)) 1 else 0)
                case "n" => println(if (cpu.isFlagSet(cpu.N_FLAG)) 1 else 0)
                case _   => println("invalid flag")
            }
        }
        else if (command.size == 3) {
            command(1) match {
                case "c" => cpu.setFlag(cpu.C_FLAG, command(2) != "0")
                case "z" => cpu.setFlag(cpu.Z_FLAG, command(2) != "0")
                case "i" => cpu.setFlag(cpu.I_FLAG, command(2) != "0")
                case "d" => cpu.setFlag(cpu.D_FLAG, command(2) != "0")
                case "b" => cpu.setFlag(cpu.B_FLAG, command(2) != "0")
                case "u" => cpu.setFlag(cpu.U_FLAG, command(2) != "0")
                case "v" => cpu.setFlag(cpu.V_FLAG, command(2) != "0")
                case "n" => cpu.setFlag(cpu.N_FLAG, command(2) != "0")
                case _   => println("invalid flag")
            }
        }
    }

    def bp() {
        if (command.size == 1) cpu.breakpoints.foreach(breakpoint => printTwoBytes(breakpoint))
        else if (command.size == 2 && command(1) == "clear") cpu.breakpoints = Set.empty[Int]
        else if (command(1) == "clear") for (i <- 2 until command.size) cpu.breakpoints -= command(i)
        else if (command(1) == "set") for (i <- 2 until command.size) cpu.breakpoints += command(i)
    }

    def load() {
        val filename = command(1)
        val in = new BufferedInputStream(new FileInputStream(filename))
        val position = in.read() | (in.read() << 8)
        val size = in.read() | (in.read() << 8)
        for (i <- position until position + size) cpu.writeByte(i, in.read())
        in.close()
    }

    def save() {
        val filename = command(1)
        val size = command(2)
        val out = new BufferedOutputStream(new FileOutputStream(filename))
        out.write(position & 0xFF)
        out.write(position >> 8)
        out.write(size & 0xFF)
        out.write(size >> 8)
        for (i <- position until position + size) out.write(cpu.readByte(i))
        out.close()
    }

    def cycles() {
        if (command.size == 1) println(cpu.cycles)
        else if (command(1) == "clear") cpu.cycles = 0
    }

    def run() {
        cpu.breakpointsEnabled = true

        while (! quit) {
            try {
                command = console.readLine("> ").split(" ")
                command(0) match {
                    case "quit" => quit = true

                    case "res"    => { cpu.isResetRequested = true; cpu.step() }
                    case "irq"    => { cpu.isInterruptRequested = true; cpu.step() }
                    case "nmi"    => { cpu.isNonmaskableInterruptRequested = true; cpu.step() }

                    case "reg"    => reg()
                    case "pc"     => pc()
                    case "flag"   => flag()
                    case "list"   => list()
                    case "cycles" => cycles()

                    case "pos"    => printTwoBytes(position)
                    case "move"   => move()
                    case "gopc"   => position = cpu.c
                    case "ins"    => ins()

                    case "read"   => read()
                    case "write"  => write()

                    case "bp"     => bp()

                    case "step"   => cpu.step()
                    case "run"    => cpu.run()

                    case "load"   => load()
                    case "save"   => save()

                    case _        => println("invalid command")
                }
            }
            catch {
                case e: Exception => println("error: " + e)
            }
        }
    }

    def main(args: Array[String]) {
        run()
    }

}
