package org.verdeterre.rodriguez

class MOS6502(val memory: MemoryMapper) {

    // Utility ----------------------------------------------------

    implicit def boolToInt(b: Boolean): Int = if (b) 1 else 0
    implicit def intToBool(i: Int): Boolean = i != 0

    def padToTwoBytes(byte: Int): Int = if (byte & 0x80) 0xFF00 | byte else byte

    // Debugging --------------------------------------------------

    var debug = false
    var debugger: Debugger = new BasicDebugger(this)
    var shouldHalt = false
    var isBreakpointEnabled = false
    var breakpoints = Set.empty[Int]

    def shouldBreak: Boolean = shouldHalt || (isBreakpointEnabled && breakpoints(c))

    // CPU clock --------------------------------------------------

    var maxCycles = 100000
    var cycles = 0

    // Registers --------------------------------------------------

    var c = 0x0000 // Program counter
    var a = 0x00   // Accumulator
    var x = 0x00   // X index
    var y = 0x00   // Y index
    var p = 0x00   // Processor status
    var s = 0x00   // Stack pointer

    // Flags ------------------------------------------------------

    val C_FLAG = 1 << 0 // Carry
    val Z_FLAG = 1 << 1 // Zero
    val I_FLAG = 1 << 2 // IRQ disable
    val D_FLAG = 1 << 3 // Decimal
    val B_FLAG = 1 << 4 // Break
    val U_FLAG = 1 << 5 // Unused
    val V_FLAG = 1 << 6 // Overflow
    val N_FLAG = 1 << 7 // Negative

    def setFlag(flag: Int, value: Boolean = true) {
        if (value) p |= flag else p &= ~flag & 0xFF
    }

    def clearFlag(flag: Int) {
        setFlag(flag, false)
    }

    def isFlagSet(flag: Int): Boolean = p & flag

    // Memory functions -------------------------------------------

    def readByte(address: Int): Int = memory.read(address)

    def readTwoBytes(address: Int): Int = memory.read(address) | (memory.read(address + 1) << 8)

    def writeByte(address: Int, byte: Int) {
        memory.write(address, byte)
    }

    def readNextByte: Int = {
        val byte = memory.read(c)
        c += 1
        byte
    }

    def readNextTwoBytes: Int = readNextByte | (readNextByte << 8)

    // Stack functions --------------------------------------------

    def pushByte(byte: Int) {
        writeByte(0x0100 | s, byte)
        s -= 1
    }

    def pushTwoBytes(twoBytes: Int) {
        pushByte(twoBytes >> 8)
        pushByte(twoBytes & 0xFF)
    }

    def pullByte: Int = {
        s += 1
        readByte(0x0100 | s)
    }

    def pullTwoBytes: Int = pullByte | (pullByte << 8)

    // Addressing -------------------------------------------------

    var opcode  = 0x00
    var mode    = AddressingMode.Implied
    var address = 0x0000
    var operand = 0x00

    def crossesPageBoundary(address: Int, index: Int): Boolean = (address & 0xFF00) != ((address + index) & 0xFF00)

    def modeImplied() {
        mode = AddressingMode.Implied
    }

    def modeAccumulator() {
        operand = a
        mode = AddressingMode.Accumulator
    }

    def modeImmediate() {
        operand = readNextByte
        mode = AddressingMode.Immediate
    }

    def modeZeroPage(index: Int = 0) {
        address = (readNextByte + index) & 0xFF
        operand = readByte(address)
        mode = AddressingMode.ZeroPage
    }

    def modeZeroPageX() {
        modeZeroPage(x)
        mode = AddressingMode.ZeroPageX
    }

    def modeZeroPageY() {
        mode = AddressingMode.ZeroPageY
        modeZeroPage(y)
    }

    def modeAbsolute(index: Int = 0, checkForPageBoundaryCrossing: Boolean = false) {
        address = readNextTwoBytes
        if (checkForPageBoundaryCrossing && crossesPageBoundary(address, index)) cycles += 1
        address += index
        operand = readByte(address)
        mode = AddressingMode.Absolute
    }

    def modeAbsoluteX(checkForPageBoundaryCrossing: Boolean = false) {
        modeAbsolute(x, checkForPageBoundaryCrossing)
        mode = AddressingMode.AbsoluteX
    }

    def modeAbsoluteY(checkForPageBoundaryCrossing: Boolean = false) {
        modeAbsolute(y, checkForPageBoundaryCrossing)
        mode = AddressingMode.AbsoluteY
    }

    def modeIndexedIndirect() {
        address = readTwoBytes(readNextByte + x)
        operand = readByte(address)
        mode = AddressingMode.IndexedIndirect
    }

    def modeIndirectIndexed(checkForPageBoundaryCrossing: Boolean = false) {
        address = readTwoBytes(readNextByte)
        if (checkForPageBoundaryCrossing && crossesPageBoundary(address, y)) cycles += 1
        address += y
        operand = readByte(address)
        mode = AddressingMode.IndirectIndexed
    }

    def modeIndirectAbsolute() {
        address = readTwoBytes(readNextTwoBytes)
        mode = AddressingMode.IndirectAbsolute
    }

    def modeRelative() {
        operand = padToTwoBytes(readNextByte)
        address = c + operand
        mode = AddressingMode.Relative
    }

    // Service requests -------------------------------------------

    var isResetRequested = true
    var isInterruptRequested = false
    var isNonmaskableInterruptRequested = false

    def clearRequests() {
        isResetRequested = false
        isInterruptRequested = false
        isNonmaskableInterruptRequested = false
    }

    def handleReset() {
        p = I_FLAG | U_FLAG
        clearRequests()
        c = readTwoBytes(0xFFFC)
        cycles += 6
    }

    def handleInterrupt() {
        if (! (p & I_FLAG)) {
            pushTwoBytes(c)
            pushByte(p)
            setFlag(I_FLAG)
            clearRequests()
            c = readTwoBytes(0xFFFE)
            cycles += 8
        }
    }

    def handleNonmaskableInterrupt() {
        pushTwoBytes(c)
        pushByte(p)
        setFlag(I_FLAG)
        clearRequests()
        c = readTwoBytes(0xFFFA)
        cycles += 8
    }

    // Execution --------------------------------------------------

    def run(): Int = {
        while (cycles < maxCycles && ! shouldBreak) {
            if (isResetRequested) handleReset()
            else if (isInterruptRequested) handleInterrupt()
            else if (isNonmaskableInterruptRequested) handleNonmaskableInterrupt()
            step()
        }
        cycles
    }

    def step() {
        if (debug) debugger.doSomething()

        opcode = readNextByte
        opcode match {
            // ADC
            case 0x69 => { cycles += 2; modeImmediate();           adc() } // ADC #dd
            case 0x65 => { cycles += 3; modeZeroPage();            adc() } // ADC aa
            case 0x75 => { cycles += 4; modeZeroPageX();           adc() } // ADC aa,X
            case 0x6D => { cycles += 4; modeAbsolute();            adc() } // ADC aaaa
            case 0x7D => { cycles += 4; modeAbsoluteX(true);       adc() } // ADC aaaa,X
            case 0x79 => { cycles += 4; modeAbsoluteY(true);       adc() } // ADC aaaa,Y
            case 0x61 => { cycles += 6; modeIndexedIndirect();     adc() } // ADC (aa,X)
            case 0x71 => { cycles += 5; modeIndirectIndexed(true); adc() } // ADC (aa),Y

            // AND
            case 0x29 => { cycles += 2; modeImmediate();           and() } // AND #dd
            case 0x25 => { cycles += 3; modeZeroPage();            and() } // AND aa
            case 0x35 => { cycles += 4; modeZeroPageX();           and() } // AND aa,X
            case 0x2D => { cycles += 4; modeAbsolute();            and() } // AND aaaa
            case 0x3D => { cycles += 4; modeAbsoluteX(true);       and() } // AND aaaa,X
            case 0x39 => { cycles += 4; modeAbsoluteY(true);       and() } // AND aaaa,Y
            case 0x21 => { cycles += 6; modeIndexedIndirect();     and() } // AND (aa,X)
            case 0x31 => { cycles += 5; modeIndirectIndexed(true); and() } // AND (aa),Y

            // ASL
            case 0x0A => { cycles += 2; modeAccumulator();         asl() } // ASL A
            case 0x06 => { cycles += 5; modeZeroPage();            asl() } // ASL aa
            case 0x16 => { cycles += 6; modeZeroPageX();           asl() } // ASL aa,X
            case 0x0E => { cycles += 6; modeAbsolute();            asl() } // ASL aaaa
            case 0x1E => { cycles += 7; modeAbsoluteX();           asl() } // ASL aaaa,X

            // BCC aa
            case 0x90 => { cycles += 2; modeRelative();            bcc() }

            // BCS aa
            case 0xB0 => { cycles += 2; modeRelative();            bcs() }

            // BEQ aa
            case 0xF0 => { cycles += 2; modeRelative();            beq() }

            // BIT
            case 0x24 => { cycles += 3; modeZeroPage();            bit() } // BIT aa
            case 0x2C => { cycles += 4; modeAbsolute();            bit() } // BIT aaaa

            // BMI aa
            case 0x30 => { cycles += 2; modeRelative();            bmi() }

            // BNE aa
            case 0xD0 => { cycles += 2; modeRelative();            bne() }

            // BPL aa
            case 0x10 => { cycles += 2; modeRelative();            bpl() }

            // BRK
            case 0x00 => { cycles += 7; modeImplied();             brk() }

            // BVC aa
            case 0x50 => { cycles += 2; modeRelative();            bvc() }

            // BVS aa
            case 0x70 => { cycles += 2; modeRelative();            bvs() }

            // CLC
            case 0x18 => { cycles += 2; modeImplied();             clc() }

            // CLD
            case 0xD8 => { cycles += 2; modeImplied();             cld() }

            // CLI
            case 0x58 => { cycles += 2; modeImplied();             cli() }

            // CLV
            case 0xB8 => { cycles += 2; modeImplied();             clv() }

            // CMP
            case 0xC9 => { cycles += 2; modeImmediate();           cmp() } // CMP #dd
            case 0xC5 => { cycles += 3; modeZeroPage();            cmp() } // CMP aa
            case 0xD5 => { cycles += 4; modeZeroPageX();           cmp() } // CMP aa,X
            case 0xCD => { cycles += 4; modeAbsolute();            cmp() } // CMP aaaa
            case 0xDD => { cycles += 4; modeAbsoluteX(true);       cmp() } // CMP aaaa,X
            case 0xD9 => { cycles += 4; modeAbsoluteY(true);       cmp() } // CMP aaaa,Y
            case 0xC1 => { cycles += 6; modeIndexedIndirect();     cmp() } // CMP (aa,X)
            case 0xD1 => { cycles += 5; modeIndirectIndexed(true); cmp() } // CMP (aa),Y

            // CPX
            case 0xE0 => { cycles += 2; modeImmediate();           cpx() } // CPX #dd
            case 0xE4 => { cycles += 3; modeZeroPage();            cpx() } // CPX aa
            case 0xEC => { cycles += 4; modeAbsolute();            cpx() } // CPX aaaa

            // CPY
            case 0xC0 => { cycles += 2; modeImmediate();           cpy() } // CPY #dd
            case 0xC4 => { cycles += 3; modeZeroPage();            cpy() } // CPY aa
            case 0xCC => { cycles += 4; modeAbsolute();            cpy() } // CPY aaaa

            // DEC
            case 0xC6 => { cycles += 5; modeZeroPage();            dec() } // DEC aa
            case 0xD6 => { cycles += 6; modeZeroPageX();           dec() } // DEC aa,X
            case 0xCE => { cycles += 6; modeAbsolute();            dec() } // DEC aaaa
            case 0xDE => { cycles += 7; modeAbsoluteX();           dec() } // DEC aaaa,X

            // DEX
            case 0xCA => { cycles += 2; modeImplied();             dex() }

            // DEY
            case 0x88 => { cycles += 2; modeImplied();             dey() }

            // EOR
            case 0x49 => { cycles += 2; modeImmediate();           eor() } // EOR #dd
            case 0x45 => { cycles += 3; modeZeroPage();            eor() } // EOR aa
            case 0x55 => { cycles += 4; modeZeroPageX();           eor() } // EOR aa,X
            case 0x4D => { cycles += 4; modeAbsolute();            eor() } // EOR aaaa
            case 0x5D => { cycles += 4; modeAbsoluteX();           eor() } // EOR aaaa,X
            case 0x59 => { cycles += 4; modeAbsoluteY();           eor() } // EOR aaaa,Y
            case 0x41 => { cycles += 6; modeIndexedIndirect();     eor() } // EOR (aa,X)
            case 0x51 => { cycles += 5; modeIndexedIndirect();     eor() } // EOR (aa),Y

            // INC
            case 0xE6 => { cycles += 5; modeZeroPage();            inc() } // INC aa
            case 0xF6 => { cycles += 6; modeZeroPageX();           inc() } // INC aa,X
            case 0xEE => { cycles += 6; modeAbsolute();            inc() } // INC aaaa
            case 0xFE => { cycles += 7; modeAbsoluteX();           inc() } // INC aaaa,X

            // INX
            // case 0xE8 => { cycles += 2; modeImplied();             inx() }

            // INY
            // case 0xC8 => { cycles += 2; modeImplied();             iny() }

            // JMP
            // case 0x4C => { cycles += 3; modeAbsolute();            jmp() } // JMP aaaa
            // case 0x6C => { cycles += 5; modeIndirectAbsolute();    jmp() } // JMP (aaaa)

            // JSR
            // case 0x20 => { cycles += 6; modeAbsolute();            jsr() }

            // LDA
            // case 0xA9 => { cycles += 2; modeImmediate();           lda() } // LDA #dd
            // case 0xA5 => { cycles += 3; modeZeroPage();            lda() } // LDA aa
            // case 0xB5 => { cycles += 4; modeZeroPageX()            lda() } // LDA aa,X
            // case 0xAD => { cycles += 4; modeAbsolute()             lda() } // LDA aaaa
            // case 0xBD => { cycles += 4; modeAbsoluteX(true)        lda() } // LDA aaaa,X
            // case 0xB9 => { cycles += 4; modeAbsoluteY(true)        lda() } // LDA aaaa,Y
            // case 0xA1 => { cycles += 6; modeIndexedIndirect()      lda() } // LDA (aa,X)
            // case 0xB1 => { cycles += 5; modeIndirectIndexed(true)  lda() } // LDA (aa),Y

            // LDX
            // case 0xA2 => { cycles += 2; modeImmediate();           ldx() } // LDX #dd
            // case 0xA6 => { cycles += 3; modeZeroPage();            ldx() } // LDX aa
            // case 0xB6 => { cycles += 4; modeZeroPageY();           ldx() } // LDX aa,Y
            // case 0xAE => { cycles += 4; modeAbsolute();            ldx() } // LDX aaaa
            // case 0xBE => { cycles += 4; modeAbsoluteY(true)        ldx() } // LDX aaaa,Y

            // LDY
            // case 0xA0 => { cycles += 2; modeImmediate();           ldy() } // LDY #dd
            // case 0xA4 => { cycles += 3; modeZeroPage();            ldy() } // LDY aa
            // case 0xB4 => { cycles += 4; modeZeroPageX();           ldy() } // LDY aa,X
            // case 0xAC => { cycles += 4; modeAbsolute();            ldy() } // LDY aaaa
            // case 0xBC => { cycles += 4; modeAbsoluteX(true)        ldy() } // LDY aaaa,X

            // LSR
            // case 0x4A => { cycles += 2; modeAccumulator();         lsr() } // LSR A
            // case 0x46 => { cycles += 5; modeZeroPage();            lsr() } // LSR aa
            // case 0x56 => { cycles += 6; modeZeroPageX();           lsr() } // LSR aa,X
            // case 0x4E => { cycles += 6; modeAbsolute();            lsr() } // LSR aaaa
            // case 0x5E => { cycles += 7; modeAbsoluteX();           lsr() } // LSR aaaa,X

            // NOP
            // case 0xEA => { cycles += 2; modeImplied();             nop() }

            // ORA
            // case 0x09 => { cycles += 2; modeImmediate();           ora() } // ORA #dd
            // case 0x05 => { cycles += 3; modeZeroPage();            ora() } // ORA aa
            // case 0x15 => { cycles += 4; modeZeroPageX();           ora() } // ORA aa,X
            // case 0x0D => { cycles += 4; modeAbsolute();            ora() } // ORA aaaa
            // case 0x1D => { cycles += 4; modeAbsoluteX(true);       ora() } // ORA aaaa,X
            // case 0x19 => { cycles += 4; modeAbsoluteY(true);       ora() } // ORA aaaa,Y
            // case 0x01 => { cycles += 6; modeIndexedIndirect();     ora() } // ORA (aa,X)
            // case 0x11 => { cycles += 5; modeIndirectIndexed(true); ora() } // ORA (aa),Y

            // PHA
            // case 0x48 => { cycles += 3; modeImplied();             pha() }

            // PHP
            // case 0x08 => { cycles += 3; modeImplied();             php() }

            // PLA
            // case 0x68 => { cycles += 4; modeImplied();             pla() }

            // PLP
            // case 0x28 => { cycles += 4; modeImplied();             plp() }

            // ROL
            // case 0x2A => { cycles += 2; modeAccumulator();         rol() } // ROL A
            // case 0x26 => { cycles += 5; modeZeroPage();            rol() } // ROL aa
            // case 0x36 => { cycles += 6; modeZeroPageX();           rol() } // ROL aa,X
            // case 0x2E => { cycles += 6; modeAbsolute();            rol() } // ROL aaaa
            // case 0x3E => { cycles += 7; modeAbsoluteX();           rol() } // ROL aaaa,X

            // ROR
            // case 0x6A => { cycles += 2; modeAccumulator();         ror() } // ROR A
            // case 0x66 => { cycles += 5; modeZeroPage();            ror() } // ROR aa
            // case 0x76 => { cycles += 6; modeZeroPageX();           ror() } // ROR aa,X
            // case 0x6E => { cycles += 6; modeAbsolute();            ror() } // ROR aaaa
            // case 0x7E => { cycles += 7; modeAbsoluteX();           ror() } // ROR aaaa,X

            // RTI
            // case 0x40 => { cycles += 6; modeImplied();             rti() }

            // RTS
            // case 0x60 => { cycles += 6; modeImplied();             rts() }

            // SBC
            // case 0xE9 => { cycles += 2; modeImmediate();           sbc() } // SBC #dd
            // case 0xE5 => { cycles += 3; modeZeroPage();            sbc() } // SBC aa
            // case 0xF5 => { cycles += 4; modeZeroPageX();           sbc() } // SBC aa,X
            // case 0xED => { cycles += 4; modeAbsolute();            sbc() } // SBC aaaa
            // case 0xFD => { cycles += 4; modeAbsoluteX(true);       sbc() } // SBC aaaa,X
            // case 0xF9 => { cycles += 4; modeAbsoluteY(true);       sbc() } // SBC aaaa,Y
            // case 0xE1 => { cycles += 6; modeIndexedIndirect();     sbc() } // SBC (aa,X)
            // case 0xF1 => { cycles += 5; modeIndirectIndexed(true)  sbc() } // SBC (aa),Y

            // SEC
            // case 0x38 => { cycles += 2; modeImplied();             sec() }

            // SED
            // case 0xF8 => { cycles += 2; modeImplied();             sec() }

            // SEI
            // case 0x78 => { cycles += 2; modeImplied();             sei() }

            // STA
            // case 0x85 => { cyles += 3; modeZeroPage();             sta() } // STA aa
            // case 0x95 => { cyles += 4; modeZeroPageX();            sta() } // STA aa,X
            // case 0x8D => { cyles += 4; modeAbsolute();             sta() } // STA aaaa
            // case 0x9D => { cyles += 5; modeAbsoluteX();            sta() } // STA aaaa,X
            // case 0x99 => { cyles += 5; modeAbsoluteY();            sta() } // STA aaaa,Y
            // case 0x81 => { cyles += 6; modeIndexedIndirect();      sta() } // STA (aa,X)
            // case 0x91 => { cyles += 6; modeIndirectIndexed();      sta() } // STA (aa),Y

            // STX
            // case 0x86 => { cycles += 3; modeZeroPage();            stx() } // STX aa
            // case 0x96 => { cycles += 4; modeZeroPageY();           stx() } // STX aa,Y
            // case 0x8E => { cycles += 4; modeAbsolute();            stx() } // STX aaaa

            // STY
            // case 0x84 => { cycles += 3; modeZeroPage();            sty() } // STY aa
            // case 0x94 => { cycles += 4; modeZeroPageX();           sty() } // STY aa,X
            // case 0x8C => { cycles += 4; modeAbsolute();            sty() } // STY aaaa

            // TAX
            // case 0xAA => { cycles += 2; modeImplied();             tax() }

            // TAY
            // case 0xA8 => { cycles += 2; modeImplied();             tay() }

            // TSX
            // case 0xBA => { cycles += 2; modeImplied();             tsx() }

            // TXA
            // case 0x8A => { cycles += 2; modeImplied();             txa() }

            // TXS
            // case 0x9A => { cycles += 2; modeImplied();             txs() }

            // TYA
            // case 0x98 => { cycles += 2; modeImplied();             tya() }
        }
    }

    // Instructions -----------------------------------------------

    def adc() {
        val result = a + operand
        setFlag(C_FLAG, result & 0x100)
        setFlag(Z_FLAG, (result & 0xFF) == 0)
        setFlag(V_FLAG, ~(a ^ operand) & (a ^ result) & 0x80)
        setFlag(N_FLAG, result & 0x80)
        a = result & 0xFF
    }

    def and() {
        a &= operand
        setFlag(Z_FLAG, a == 0)
        setFlag(N_FLAG, a & 0x80)
    }

    def asl() {
        val result = (operand << 1) & 0xFF
        setFlag(C_FLAG, operand & 0x80)
        setFlag(Z_FLAG, result == 0)
        setFlag(N_FLAG, result & 0x80)
        if (mode == AddressingMode.Accumulator) a = result
        else writeByte(address, result)
    }

    def branch(condition: Boolean) {
        if (condition) {
            if (crossesPageBoundary(c, operand)) cycles += 2 else cycles += 1
            c = address
        }
    }

    def bcc() {
        branch(! isFlagSet(C_FLAG))
    }

    def bcs() {
        branch(isFlagSet(C_FLAG))
    }

    def beq() {
        branch(isFlagSet(Z_FLAG))
    }

    def bit() {
        val result = a & operand
        setFlag(Z_FLAG, result == 0)
        setFlag(V_FLAG, operand & 0x40)
        setFlag(N_FLAG, operand & 0x80)
    }

    def bmi() {
        branch(isFlagSet(N_FLAG))
    }

    def bne() {
        branch(! isFlagSet(Z_FLAG))
    }

    def bpl() {
        branch(! isFlagSet(N_FLAG))
    }

    def brk() {
        setFlag(B_FLAG)
        c += 1
        pushTwoBytes(c)
        pushByte(p)
        c = readTwoBytes(0xFFFE)
    }

    def bvc() {
        branch(! isFlagSet(V_FLAG))
    }

    def bvs() {
        branch(isFlagSet(V_FLAG))
    }

    def clc() {
        clearFlag(C_FLAG)
    }

    def cld() {
        clearFlag(D_FLAG)
    }

    def cli() {
        clearFlag(I_FLAG)
    }

    def clv() {
        clearFlag(V_FLAG)
    }

    def compare(regVal: Int) {
        val result = regVal + ((~operand + 1) & 0xFF)
        setFlag(C_FLAG, result & 0x100)
        setFlag(Z_FLAG, ! result)
        setFlag(N_FLAG, result & 0x80)
    }

    def cmp() {
        compare(a)
    }

    def cpx() {
        compare(x)
    }

    def cpy() {
        compare(y)
    }

    def dec() {
        val result = (operand - 1) & 0xFF
        setFlag(Z_FLAG, ! result)
        setFlag(N_FLAG, result & 0x80)
        writeByte(address, result)
    }

    def dex() {
        x = (x - 1) & 0xFF
        setFlag(Z_FLAG, ! x)
        setFlag(N_FLAG, x & 0x80)
    }

    def dey() {
        y = (y - 1) & 0xFF
        setFlag(Z_FLAG, ! y)
        setFlag(N_FLAG, y & 0x80)
    }

    def eor() {
        a ^= operand
        setFlag(Z_FLAG, ! a)
        setFlag(N_FLAG, a & 0x80)
    }

    def inc() {
        val result = (operand + 1) & 0xFF
        setFlag(Z_FLAG, ! result)
        setFlag(N_FLAG, result & 0x80)
        writeByte(address, result)
    }

    def inx() {
        x = (x + 1) & 0xFF
        setFlag(Z_FLAG, ! x)
        setFlag(N_FLAG, x & 0x80)
    }

    def iny() {
        y = (y + 1) & 0xFF
        setFlag(Z_FLAG, ! y)
        setFlag(N_FLAG, y & 0x80)
    }

}

object AddressingMode extends Enumeration {
    
    val Implied          = Value
    val Accumulator      = Value
    val Immediate        = Value
    val ZeroPage         = Value
    val ZeroPageX        = Value
    val ZeroPageY        = Value
    val Absolute         = Value
    val AbsoluteX        = Value
    val AbsoluteY        = Value
    val IndexedIndirect  = Value
    val IndirectIndexed  = Value
    val IndirectAbsolute = Value
    val Relative         = Value
    val Unknown          = Value

}
