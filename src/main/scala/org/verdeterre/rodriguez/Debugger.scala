package org.verdeterre.rodriguez

abstract class Debugger(cpu: MOS6502) {

    def formatByte(byte: Int): String = "0x%02X".format(byte)

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

            // Undocumented opcodes
            case _ => (formatByte(opcode), AddressingMode.Unknown)
        }
    }

    def doSomething() {
        // Or nothing...
    }

}

class BasicDebugger(cpu: MOS6502) extends Debugger(cpu) {

    override def doSomething() {
        val (mnemonic, mode) = opcodeInfo(cpu.readByte(cpu.c))
        
        val operand = mode match {
            case AddressingMode.Implied     => ""

            case AddressingMode.Accumulator => "A"
            
            case AddressingMode.Immediate   => formatByte(cpu.readByte(cpu.c + 1))
            
            case AddressingMode.ZeroPage    => formatByte(cpu.readByte(cpu.readByte(cpu.c + 1)))

            case AddressingMode.ZeroPageX   => formatByte(cpu.readByte(cpu.readByte(cpu.c + 1) + cpu.x))

            case AddressingMode.ZeroPageY   => formatByte(cpu.readByte(cpu.readByte(cpu.c + 1) + cpu.y))

            case AddressingMode.Unknown     => "???"

            case _                          => "Not implemented"
        }

        printf("%s %s\n", mnemonic, operand)
    }

}
