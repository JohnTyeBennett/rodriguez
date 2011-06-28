package org.verdeterre.rodriguez

object Debugger {

    implicit def hexStrToInt(str: String): Int = Integer.parseInt(str, 16)

    var quit = false
    val console = System.console()
    var command = Array.empty[String]
    var cursorLocation = 0

    val cpu = new MOS6502(new MemoryMapper)

    def formatByte(byte: Int): String = "$%02X".format(byte)
    def formatTwoBytes(bytes: Int): String = "$%04X".format(bytes)

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

            case 0x00 => ("BRK", AddressingMode.Implied)

            case _ => (formatByte(opcode), AddressingMode.Unknown)
        }
    }

    def instruction(address: Int): String = {
        val (mnemonic, mode) = opcodeInfo(cpu.readByte(address))
        val operand = mode match {
            case AddressingMode.Implied     => ""
            case AddressingMode.Accumulator => "A"
            case AddressingMode.Immediate   => "#" + formatByte(cpu.readByte(address + 1))
            case AddressingMode.ZeroPage    => formatByte(cpu.readByte(address + 1))
            case AddressingMode.ZeroPageX   => formatByte(cpu.readByte(address + 1)) + ",X"
            case AddressingMode.ZeroPageY   => formatByte(cpu.readByte(address + 1)) + ",Y"
            case AddressingMode.Unknown     => "???"
            case _                          => "Not implemented"
        }
        "%s %s".format(mnemonic, operand)
    }

    def printByte(byte: Int) {
        println(formatByte(byte))
    }

    def printTwoBytes(bytes: Int) {
        println(formatTwoBytes(bytes))
    }

    def pc() {
        if (command.size == 1) printTwoBytes(cpu.c)
        else if (command.size == 2) cpu.c = command(1)
        else println("error")
    }

    def mem() {
        if (command.size == 2) printByte(command(1))
        else if (command.size == 3) cpu.writeByte(command(1), command(2))
        else println("error")
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
        else println("error")
    }

    def ins() {
        println(instruction(cpu.c))
    }

    def cins() {
        println(instruction(cursorLocation))
    }

    def cursor() {
        if (command.size == 1) printByte(cursorLocation)
        else if (command.size == 2) cursorLocation = command(1)
        else println("error")
    }

    def write() {
        for (i <- 1 until command.size) cpu.writeByte(cursorLocation + i - 1, command(i))
    }

    def read() {
        for (i <- 0 until command(1)) printByte(cpu.readByte(cursorLocation + i))
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
        else println("error")
    }

    def step() {
        cpu.step()
    }

    def run() {
        while (! quit) {
            command = console.readLine().split(" ")
            command(0) match {
                case "quit" => quit = true

                case "res"    => cpu.isResetRequested = true
                case "irq"    => cpu.isInterruptRequested = true
                case "nmi"    => cpu.isNonmaskableInterruptRequested = true
                case "pc"     => pc()
                case "mem"    => mem()
                case "reg"    => reg()
                case "ins"    => ins()
                case "cins"   => cins()
                case "cursor" => cursor()
                case "write"  => write()
                case "read"   => read()
                case "flag"   => flag()
                case "step"   => step()

                case _ => println("invalid command")
            }
        }
    }

    def main(args: Array[String]) {
        run()
    }

}
