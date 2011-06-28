package org.verdeterre.rodriguez

import org.junit._
import org.junit.Assert._

class MOS6502Test {

    val mem = new MemoryMapper
    val cpu = new MOS6502(mem)

    // Utility ----------------------------------------------------

    @Test def testBoolToInt() {
        assertEquals(0, cpu.boolToInt(false))
        assertEquals(1, cpu.boolToInt(true))
    }

    @Test def testIntToBool() {
        assertEquals(false, cpu.intToBool(0))
        assertEquals(true,  cpu.intToBool(1))
        assertEquals(true,  cpu.intToBool(-99))
    }

    @Test def testPadToTwoBytes() {
        assertEquals(0x0000, cpu.padToTwoBytes(0x00))
        assertEquals(0x007F, cpu.padToTwoBytes(0x7F))
        assertEquals(0xFFAB, cpu.padToTwoBytes(0xAB))
    }

    // Flags ------------------------------------------------------

    @Test def testSetFlag() {
        cpu.p = 0
        cpu.setFlag(cpu.C_FLAG)
        cpu.setFlag(cpu.N_FLAG)
        assertEquals(cpu.C_FLAG | cpu.N_FLAG, cpu.p)
    }

    @Test def clearFlag() {
        cpu.p = cpu.C_FLAG | cpu.N_FLAG
        cpu.clearFlag(cpu.N_FLAG)
        assertEquals(cpu.C_FLAG, cpu.p)
    }

    @Test def isFlagSet() {
        cpu.p = cpu.C_FLAG | cpu.N_FLAG
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.I_FLAG))
        assert(! cpu.isFlagSet(cpu.D_FLAG))
        assert(! cpu.isFlagSet(cpu.B_FLAG))
        assert(! cpu.isFlagSet(cpu.U_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    // Memory functions -------------------------------------------

    @Test def testReadByte() {
        mem.write(0xABCD, 0xAC)
        assertEquals(0xAC, cpu.readByte(0xABCD))
    }

    @Test def testWriteByte() {
        cpu.writeByte(0x1234, 0xBD)
        assertEquals(0xBD, mem.read(0x1234))
    }

    @Test def testReadNextByte() {
        mem.write(0xABCD, 0x4B)
        cpu.c = 0xABCD
        assertEquals(0x4B, cpu.readNextByte)
        assertEquals(0xABCE, cpu.c)
    }

    @Test def testReadNextTwoBytes() {
        mem.write(0x1324, 0xCD)
        mem.write(0x1325, 0xAB)
        cpu.c = 0x1324
        assertEquals(0xABCD, cpu.readNextTwoBytes)
        assertEquals(0x1326, cpu.c)
    }

    // Stack functions --------------------------------------------

    @Test def testPushByte() {
        mem.clear(0x0100, 0x01FF)
        cpu.s = 0xFC
        cpu.pushByte(0xEE)
        assertEquals(0xEE, mem.read(0x01FC))
        assertEquals(0xFB, cpu.s)
    }

    @Test def testPushTwoBytes() {
        mem.clear(0x0100, 0x01FF)
        cpu.s = 0xFC
        cpu.pushTwoBytes(0xABCD)
        assertEquals(0xCD, mem.read(0x01FB))
        assertEquals(0xAB, mem.read(0x01FC))
        assertEquals(0xFA, cpu.s)
    }

    @Test def testPullByte() {
        mem.clear(0x0100, 0x01FF)
        mem.write(0x01FC, 0x99)
        cpu.s = 0xFB
        assertEquals(0x99, cpu.pullByte)
        assertEquals(0xFC, cpu.s)
    }

    @Test def testPullTwoBytes() {
        mem.clear(0x0100, 0x01FF)
        mem.write(0x01FB, 0xBD)
        mem.write(0x01FC, 0xAC)
        cpu.s = 0xFA
        assertEquals(0xACBD, cpu.pullTwoBytes)
        assertEquals(0xFC, cpu.s)
    }

    // Service Requests -------------------------------------------

    @Test def testHandleReset() {
        mem.write(0xFFFC, 0x34)
        mem.write(0xFFFD, 0x12)
        cpu.cycles = 0
        cpu.handleReset()
        assertEquals(0x1234, cpu.c)
        assertEquals(0x24, cpu.p)
        assertEquals(6, cpu.cycles)
    }

    @Test def testHandleInterrupt() {
        mem.write(0xFFFE, 0xCD)
        mem.write(0xFFFF, 0xAB)
        cpu.c = 0x1234
        cpu.p = cpu.N_FLAG | cpu.U_FLAG | cpu.B_FLAG | cpu.C_FLAG
        cpu.s = 0xFF
        cpu.cycles = 0
        cpu.handleInterrupt()
        assertEquals(0xABCD, cpu.c)
        assertEquals(cpu.N_FLAG | cpu.U_FLAG | cpu.B_FLAG | cpu.I_FLAG | cpu.C_FLAG, cpu.p)
        assertEquals(0xFC, cpu.s)
        assertEquals(cpu.N_FLAG | cpu.U_FLAG | cpu.C_FLAG, mem.read(0x01FD))
        assertEquals(0x34, mem.read(0x01FE))
        assertEquals(0x12, mem.read(0x01FF))
        assertEquals(8, cpu.cycles)
    }

    @Test def testHandleNonmaskableInterrupt() {
        mem.write(0xFFFA, 0xCD)
        mem.write(0xFFFB, 0xAB)
        cpu.c = 0x1234
        cpu.p = cpu.N_FLAG | cpu.U_FLAG | cpu.B_FLAG | cpu.C_FLAG
        cpu.s = 0xFF
        cpu.cycles = 0
        cpu.handleNonmaskableInterrupt()
        assertEquals(0xABCD, cpu.c)
        assertEquals(cpu.N_FLAG | cpu.U_FLAG | cpu.B_FLAG | cpu.I_FLAG | cpu.C_FLAG, cpu.p)
        assertEquals(0xFC, cpu.s)
        assertEquals(cpu.N_FLAG | cpu.U_FLAG | cpu.C_FLAG, mem.read(0x01FD))
        assertEquals(0x34, mem.read(0x01FE))
        assertEquals(0x12, mem.read(0x01FF))
        assertEquals(8, cpu.cycles)
    }

    // Addressing -------------------------------------------------

    @Test def testCrossesPageBoundary() {
        assert(! cpu.crossesPageBoundary(0x1234, 0x12))
        assert(cpu.crossesPageBoundary(0x1234, 0xDE))
    }

    @Test def testModeAccumulator() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.a = 0xAB
        cpu.modeAccumulator()
        assertEquals(0x0000, cpu.address)
        assertEquals(0xAB, cpu.operand)
        assertEquals(0x1234, cpu.c)
    }

    @Test def testModeImmediate() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        mem.write(0x1234, 0xAB)
        cpu.modeImmediate()
        assertEquals(0x0000, cpu.address)
        assertEquals(0xAB, cpu.operand)
        assertEquals(0x1235, cpu.c)
    }

    @Test def testModeZeroPage() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        mem.write(0x1234, 0x56)
        mem.write(0x0056, 0xCC)
        cpu.modeZeroPage()
        assertEquals(0x0056, cpu.address)
        assertEquals(0xCC, cpu.operand)
        assertEquals(0x1235, cpu.c)

        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        mem.write(0x1234, 0x56)
        mem.write(0x0056, 0xCC)
        mem.write(0x0020, 0xAA)
        cpu.modeZeroPage(0xCA)
        assertEquals(0x0020, cpu.address)
        assertEquals(0xAA, cpu.operand)
        assertEquals(0x1235, cpu.c)
    }

    @Test def testModeZeroPageX() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.x = 0x23
        mem.write(0x1234, 0x56)
        mem.write(0x0079, 0xCC)
        cpu.modeZeroPageX()
        assertEquals(0x0079, cpu.address)
        assertEquals(0xCC, cpu.operand)
        assertEquals(0x1235, cpu.c)
    }

    @Test def testModeZeroPageY() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.y = 0x42
        mem.write(0x1234, 0x56)
        mem.write(0x0098, 0xDE)
        cpu.modeZeroPageY()
        assertEquals(0x0098, cpu.address)
        assertEquals(0xDE, cpu.operand)
        assertEquals(0x1235, cpu.c)
    }

    @Test def testModeAbsolute() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        mem.write(0x1234, 0xCD)
        mem.write(0x1235, 0xAB)
        mem.write(0xABCD, 0xEF)
        cpu.modeAbsolute()
        assertEquals(0xABCD, cpu.address)
        assertEquals(0xEF, cpu.operand)
        assertEquals(0x1236, cpu.c)
    }

    @Test def testModeAbsoluteX() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.x = 0x22
        mem.write(0x1234, 0xCD)
        mem.write(0x1235, 0xAB)
        mem.write(0xABEF, 0xEF)
        cpu.modeAbsoluteX()
        assertEquals(0xABEF, cpu.address)
        assertEquals(0xEF, cpu.operand)
        assertEquals(0x1236, cpu.c)

        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.x = 0x22
        mem.write(0x1234, 0xCD)
        mem.write(0x1235, 0xAB)
        mem.write(0xABEF, 0xEF)
        cpu.modeAbsoluteX(true)
        assertEquals(0xABEF, cpu.address)
        assertEquals(0xEF, cpu.operand)
        assertEquals(0x1236, cpu.c)
        assertEquals(0, cpu.cycles)

        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.cycles = 0
        cpu.c = 0x1234
        cpu.x = 0x52
        mem.write(0x1234, 0xCD)
        mem.write(0x1235, 0xAB)
        mem.write(0xAC1F, 0xEF)
        cpu.modeAbsoluteX(true)
        assertEquals(0xAC1F, cpu.address)
        assertEquals(0xEF, cpu.operand)
        assertEquals(0x1236, cpu.c)
        assertEquals(1, cpu.cycles)
    }

    @Test def testModeAbsoluteY() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.y = 0x31
        mem.write(0x1234, 0xCD)
        mem.write(0x1235, 0xAB)
        mem.write(0xABFE, 0xEF)
        cpu.modeAbsoluteY()
        assertEquals(0xABFE, cpu.address)
        assertEquals(0xEF, cpu.operand)
        assertEquals(0x1236, cpu.c)
    }

    @Test def testModeIndexedIndirect() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.x = 0x4B
        mem.write(0x1234, 0x2A)
        mem.write(0x0075, 0xB4)
        mem.write(0x0076, 0x12)
        mem.write(0x12B4, 0xEA)
        cpu.modeIndexedIndirect()
        assertEquals(0x12B4, cpu.address)
        assertEquals(0xEA, cpu.operand)
        assertEquals(0x1235, cpu.c)
    }

    @Test def testModeIndirectIndexed() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        cpu.y = 0x4B
        mem.write(0x1234, 0x2A)
        mem.write(0x002A, 0xB4)
        mem.write(0x002B, 0x12)
        mem.write(0x12B4, 0x99)
        mem.write(0x12FF, 0xEA)
        cpu.modeIndirectIndexed()
        assertEquals(0x12FF, cpu.address)
        assertEquals(0xEA, cpu.operand)
        assertEquals(0x1235, cpu.c)
    }

    @Test def testModeIndirectAbsolute() {
        cpu.address = 0x0000
        cpu.operand = 0x00
        cpu.c = 0x1234
        mem.write(0x1234, 0x03)
        mem.write(0x1235, 0x02)
        mem.write(0x0203, 0xBC)
        mem.write(0x0204, 0x04)
        cpu.modeIndirectAbsolute()
        assertEquals(0x04BC, cpu.address)
        assertEquals(0x00, cpu.operand)
        assertEquals(0x1236, cpu.c)
    }

    // Instructions -----------------------------------------------

    @Test def testAdc() {
        cpu.a = 0x01
        cpu.operand = 0x00
        cpu.p = 0
        cpu.adc()
        assertEquals(0x01, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x00
        cpu.operand = 0x00
        cpu.p = 0
        cpu.adc()
        assertEquals(0x00, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0xAE
        cpu.operand = 0x52
        cpu.p = 0
        cpu.adc()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0xAE
        cpu.operand = 0x51
        cpu.p = 0
        cpu.adc()
        assertEquals(0xFF, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0xAE
        cpu.operand = 0xA1
        cpu.p = 0
        cpu.adc()
        assertEquals(0x4F, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x73
        cpu.operand = 0x6A
        cpu.p = 0
        cpu.adc()
        assertEquals(0xDD, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x01
        cpu.operand = 0x00
        cpu.p = cpu.C_FLAG
        cpu.adc()
        assertEquals(0x02, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x00
        cpu.operand = 0x00
        cpu.p = cpu.C_FLAG
        cpu.adc()
        assertEquals(0x01, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0xAE
        cpu.operand = 0x52
        cpu.p = cpu.C_FLAG
        cpu.adc()
        assertEquals(0x01, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0xAE
        cpu.operand = 0x51
        cpu.p = cpu.C_FLAG
        cpu.adc()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0xAE
        cpu.operand = 0xA1
        cpu.p = cpu.C_FLAG
        cpu.adc()
        assertEquals(0x50, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x73
        cpu.operand = 0x6A
        cpu.p = cpu.C_FLAG
        cpu.adc()
        assertEquals(0xDE, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x01
        cpu.operand = 0x01
        cpu.p = 0
        cpu.adc()
        assertEquals(0x02, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x01
        cpu.operand = 0xFF
        cpu.p = 0
        cpu.adc()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x7F
        cpu.operand = 0x01
        cpu.p = 0
        cpu.adc()
        assertEquals(0x80, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x80
        cpu.operand = 0xFF
        cpu.p = 0
        cpu.adc()
        assertEquals(0x7F, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testAdcUsingBcd() {
        cpu.p = cpu.D_FLAG
        cpu.a = 0x00
        cpu.operand = 0x00
        cpu.adc()
        assertEquals(0x00, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.a = 0x26
        cpu.operand = 0x37
        cpu.adc()
        assertEquals(0x63, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.a = 0x56
        cpu.operand = 0x37
        cpu.adc()
        assertEquals(0x93, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.a = 0x56
        cpu.operand = 0x47
        cpu.adc()
        assertEquals(0x03, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.a = 0x56
        cpu.operand = 0x44
        cpu.adc()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.a = 0x81
        cpu.operand = 0x83
        cpu.adc()
        assertEquals(0x64, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.a = 0x80
        cpu.operand = 0xFA
        cpu.adc()
        assertEquals(0x40, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testAnd() {
        cpu.a = 0x01
        cpu.operand = 0x01
        cpu.p = 0
        cpu.and()
        assertEquals(0x01, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x41
        cpu.operand = 0x12
        cpu.p = 0
        cpu.and()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0xB3
        cpu.operand = 0xE2
        cpu.p = 0
        cpu.and()
        assertEquals(0xA2, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testAsl() {
        cpu.address = 0x1234
        cpu.operand = 0x00
        cpu.mode = AddressingMode.Absolute
        cpu.a = 0
        cpu.p = 0
        cpu.asl()
        assertEquals(0x00, cpu.a)
        assertEquals(0x00, mem.read(0x1234))
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.address = 0x1234
        cpu.operand = 0x3A
        cpu.mode = AddressingMode.Absolute
        cpu.a = 0
        cpu.p = 0
        cpu.asl()
        assertEquals(0x00, cpu.a)
        assertEquals(0x74, mem.read(0x1234))
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.address = 0x1234
        cpu.operand = 0x84
        cpu.mode = AddressingMode.Absolute
        cpu.a = 0
        cpu.p = 0
        cpu.asl()
        assertEquals(0x00, cpu.a)
        assertEquals(0x08, mem.read(0x1234))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.address = 0x1234
        cpu.operand = 0xC4
        cpu.mode = AddressingMode.Absolute
        cpu.a = 0
        cpu.p = 0
        cpu.asl()
        assertEquals(0x00, cpu.a)
        assertEquals(0x88, mem.read(0x1234))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0x1234, 0x00)
        cpu.address = 0x1234
        cpu.operand = 0xC4
        cpu.mode = AddressingMode.Accumulator
        cpu.a = 0xC4
        cpu.p = 0
        cpu.asl()
        assertEquals(0x88, cpu.a)
        assertEquals(0x00, mem.read(0x1234))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    // Tests main functionality of bcc, bcs, beq, bmi, bne, bpl, bvc, bvs 
    @Test def testBranch() {
        cpu.c = 0xABCD
        cpu.address = 0xABDF
        cpu.operand = 0x12
        cpu.mode = AddressingMode.Relative
        cpu.cycles = 2
        cpu.branch(false)
        assertEquals(0xABCD, cpu.c)
        assertEquals(2, cpu.cycles)

        cpu.c = 0xABCD
        cpu.address = 0xAC0F
        cpu.operand = 0x42
        cpu.mode = AddressingMode.Relative
        cpu.cycles = 2
        cpu.branch(false)
        assertEquals(0xABCD, cpu.c)
        assertEquals(2, cpu.cycles)

        cpu.c = 0xABCD
        cpu.address = 0xABDF
        cpu.operand = 0x12
        cpu.mode = AddressingMode.Relative
        cpu.cycles = 2
        cpu.branch(true)
        assertEquals(0xABDF, cpu.c)
        assertEquals(3, cpu.cycles)

        cpu.c = 0xABCD
        cpu.address = 0xAC0F
        cpu.operand = 0x42
        cpu.mode = AddressingMode.Relative
        cpu.cycles = 2
        cpu.branch(true)
        assertEquals(0xAC0F, cpu.c)
        assertEquals(4, cpu.cycles)
    }

    @Test def testBit() {
        cpu.operand = 0x00
        cpu.a = 0x00
        cpu.p = 0
        cpu.bit()
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0x80
        cpu.a = 0xA0
        cpu.p = 0
        cpu.bit()
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0xC2
        cpu.a = 0xB1
        cpu.p = 0
        cpu.bit()
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testBrk() {
        cpu.c = 0xABCD
        mem.write(0xFFFE, 0x34)
        mem.write(0xFFFF, 0x12)
        cpu.s = 0xFF
        cpu.p = cpu.U_FLAG
        cpu.brk()
        assertEquals(0x1234, cpu.c)
        assertEquals(cpu.U_FLAG | cpu.I_FLAG, cpu.p)
        assertEquals(0xFC, cpu.s)
        assertEquals(0x30, mem.read(0x01FD))
        assertEquals(0xCE, mem.read(0x01FE))
        assertEquals(0xAB, mem.read(0x01FF))
    }

    @Test def testClc() {
        cpu.p = 0
        cpu.clc()
        assert(! cpu.isFlagSet(cpu.C_FLAG))

        cpu.p = cpu.C_FLAG
        cpu.clc()
        assert(! cpu.isFlagSet(cpu.C_FLAG))
    }

    @Test def testCld() {
        cpu.p = 0
        cpu.cld()
        assert(! cpu.isFlagSet(cpu.D_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.cld()
        assert(! cpu.isFlagSet(cpu.D_FLAG))
    }

    @Test def testCli() {
        cpu.p = 0
        cpu.cli()
        assert(! cpu.isFlagSet(cpu.I_FLAG))

        cpu.p = cpu.I_FLAG
        cpu.cli()
        assert(! cpu.isFlagSet(cpu.I_FLAG))
    }

    @Test def testClv() {
        cpu.p = 0
        cpu.clv()
        assert(! cpu.isFlagSet(cpu.V_FLAG))

        cpu.p = cpu.V_FLAG
        cpu.clv()
        assert(! cpu.isFlagSet(cpu.V_FLAG))
    }

    // Tests main functionality of cmp, cpx, cpy
    @Test def testCompare() {
        cpu.operand = 0x00
        cpu.compare(0x00)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0x0A
        cpu.compare(0xFF)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0xFF
        cpu.compare(0x0A)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0x8A
        cpu.compare(0x0A)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testDec() {
        mem.write(0xABCD, 0x00)
        cpu.address = 0xABCD
        cpu.operand = 0x00
        cpu.p = 0
        cpu.dec()
        assertEquals(0xFF, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x01)
        cpu.address = 0xABCD
        cpu.operand = 0x01
        cpu.p = 0
        cpu.dec()
        assertEquals(0x00, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x02)
        cpu.address = 0xABCD
        cpu.operand = 0x02
        cpu.p = 0
        cpu.dec()
        assertEquals(0x01, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testDex() {
        cpu.x = 0x00
        cpu.p = 0
        cpu.dex()
        assertEquals(0xFF, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0x01
        cpu.p = 0
        cpu.dex()
        assertEquals(0x00, cpu.x)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0x02
        cpu.p = 0
        cpu.dex()
        assertEquals(0x01, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testDey() {
        cpu.y = 0x00
        cpu.p = 0
        cpu.dey()
        assertEquals(0xFF, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.y = 0x01
        cpu.p = 0
        cpu.dey()
        assertEquals(0x00, cpu.y)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.y = 0x02
        cpu.p = 0
        cpu.dey()
        assertEquals(0x01, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testEor() {
        cpu.a = 0x00
        cpu.operand = 0x00
        cpu.p = 0
        cpu.eor()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x33
        cpu.operand = 0x33
        cpu.p = 0
        cpu.eor()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x93
        cpu.operand = 0x53
        cpu.p = 0
        cpu.eor()
        assertEquals(0xC0, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x94
        cpu.operand = 0xD3
        cpu.p = 0
        cpu.eor()
        assertEquals(0x47, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testInc() {
        mem.write(0xABCD, 0xFE)
        cpu.address = 0xABCD
        cpu.operand = 0xFE
        cpu.p = 0
        cpu.inc()
        assertEquals(0xFF, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0xFF)
        cpu.address = 0xABCD
        cpu.operand = 0xFF
        cpu.p = 0
        cpu.inc()
        assertEquals(0x00, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x00)
        cpu.address = 0xABCD
        cpu.operand = 0x00
        cpu.p = 0
        cpu.inc()
        assertEquals(0x01, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testInx() {
        cpu.x = 0xFE
        cpu.p = 0
        cpu.inx()
        assertEquals(0xFF, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0xFF
        cpu.p = 0
        cpu.inx()
        assertEquals(0x00, cpu.x)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0x00
        cpu.p = 0
        cpu.inx()
        assertEquals(0x01, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testIny() {
        cpu.y = 0xFE
        cpu.p = 0
        cpu.iny()
        assertEquals(0xFF, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.y = 0xFF
        cpu.p = 0
        cpu.iny()
        assertEquals(0x00, cpu.y)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.y = 0x00
        cpu.p = 0
        cpu.iny()
        assertEquals(0x01, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testJmp() {
        cpu.address = 0xABCD
        cpu.c = 0x1234
        cpu.jmp()
        assertEquals(0xABCD, cpu.c)
    }

    @Test def testJsr() {
        cpu.address = 0x0503
        cpu.c = 0x0204
        cpu.s = 0xFF
        cpu.jsr()
        assertEquals(0x0503, cpu.c)
        assertEquals(0xFD, cpu.s)
        assertEquals(0x03, mem.read(0x01FE))
        assertEquals(0x02, mem.read(0x01FF))
    }

    @Test def testLda() {
        cpu.operand = 0x00
        cpu.a = 0x00
        cpu.p = 0
        cpu.lda()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0x1A
        cpu.a = 0x00
        cpu.p = 0
        cpu.lda()
        assertEquals(0x1A, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0xAF
        cpu.a = 0x00
        cpu.p = 0
        cpu.lda()
        assertEquals(0xAF, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testLdx() {
        cpu.operand = 0x00
        cpu.x = 0x00
        cpu.p = 0
        cpu.ldx()
        assertEquals(0x00, cpu.x)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0x1A
        cpu.x = 0x00
        cpu.p = 0
        cpu.ldx()
        assertEquals(0x1A, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0xAF
        cpu.x = 0x00
        cpu.p = 0
        cpu.ldx()
        assertEquals(0xAF, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testLdy() {
        cpu.operand = 0x00
        cpu.y = 0x00
        cpu.p = 0
        cpu.ldy()
        assertEquals(0x00, cpu.y)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0x1A
        cpu.y = 0x00
        cpu.p = 0
        cpu.ldy()
        assertEquals(0x1A, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0xAF
        cpu.y = 0x00
        cpu.p = 0
        cpu.ldy()
        assertEquals(0xAF, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testLsr() {
        mem.write(0x1234, 0x00)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0x1234
        cpu.operand = 0x00
        cpu.a = 0x12
        cpu.p = 0
        cpu.lsr()
        assertEquals(0x00, mem.read(0x1234))
        assertEquals(0x12, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0x1234, 0x01)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0x1234
        cpu.operand = 0x01
        cpu.a = 0x12
        cpu.p = 0
        cpu.lsr()
        assertEquals(0x00, mem.read(0x1234))
        assertEquals(0x12, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0x1234, 0x02)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0x1234
        cpu.operand = 0x02
        cpu.a = 0x12
        cpu.p = 0
        cpu.lsr()
        assertEquals(0x01, mem.read(0x1234))
        assertEquals(0x12, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0x1234, 0x03)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0x1234
        cpu.operand = 0x03
        cpu.a = 0x12
        cpu.p = 0
        cpu.lsr()
        assertEquals(0x01, mem.read(0x1234))
        assertEquals(0x12, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0x1234, 0x12)
        cpu.mode = AddressingMode.Accumulator
        cpu.address = 0x1234
        cpu.operand = 0x01
        cpu.a = 0x01
        cpu.p = 0
        cpu.lsr()
        assertEquals(0x00, cpu.a)
        assertEquals(0x12, mem.read(0x1234))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testOra() {
        cpu.operand = 0x00
        cpu.a = 0x00
        cpu.p = 0
        cpu.ora()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0x21
        cpu.a = 0x38
        cpu.p = 0
        cpu.ora()
        assertEquals(0x39, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.operand = 0xA1
        cpu.a = 0x38
        cpu.p = 0
        cpu.ora()
        assertEquals(0xB9, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testPha() {
        cpu.a = 0x99
        cpu.s = 0xFA
        cpu.pha()
        assertEquals(0x99, mem.read(0x01FA))
        assertEquals(0xF9, cpu.s)
    }

    @Test def testPhp() {
        cpu.p = 0x81
        cpu.s = 0xEF
        cpu.php()
        assertEquals(0x81, mem.read(0x01EF))
        assertEquals(0xEE, cpu.s)
    }

    @Test def testPla() {
        mem.write(0x01FD, 0x00)
        cpu.a = 0x01
        cpu.s = 0xFC
        cpu.pla()
        assertEquals(0x00, cpu.a)
        assertEquals(0xFD, cpu.s)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0x01FD, 0xF1)
        cpu.a = 0x01
        cpu.s = 0xFC
        cpu.pla()
        assertEquals(0xF1, cpu.a)
        assertEquals(0xFD, cpu.s)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0x01FD, 0x71)
        cpu.a = 0x01
        cpu.s = 0xFC
        cpu.pla()
        assertEquals(0x71, cpu.a)
        assertEquals(0xFD, cpu.s)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testPlp() {
        mem.write(0x01CC, 0x34)
        cpu.p = 0x11
        cpu.s = 0xCB
        cpu.plp()
        assertEquals(0x34, cpu.p)
    }

    @Test def testRol() {
        mem.write(0xABCD, 0x00)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x00
        cpu.p = 0
        cpu.rol()
        assertEquals(0x00, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x80)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x80
        cpu.p = 0
        cpu.rol()
        assertEquals(0x00, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0xC0)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0xC0
        cpu.p = 0
        cpu.rol()
        assertEquals(0x80, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0xC0)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0xC0
        cpu.p = cpu.C_FLAG
        cpu.rol()
        assertEquals(0x81, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x81)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x81
        cpu.p = cpu.C_FLAG
        cpu.rol()
        assertEquals(0x03, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x81)
        cpu.a = 0x81
        cpu.mode = AddressingMode.Accumulator
        cpu.address = 0xABCD
        cpu.operand = 0x81
        cpu.p = cpu.C_FLAG
        cpu.rol()
        assertEquals(0x03, cpu.a)
        assertEquals(0x81, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testRor() {
        mem.write(0xABCD, 0x00)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x00
        cpu.p = 0
        cpu.ror()
        assertEquals(0x00, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x01)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x01
        cpu.p = 0
        cpu.ror()
        assertEquals(0x00, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x02)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x02
        cpu.p = 0
        cpu.ror()
        assertEquals(0x01, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x03)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x03
        cpu.p = 0
        cpu.ror()
        assertEquals(0x01, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x02)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x02
        cpu.p = cpu.C_FLAG
        cpu.ror()
        assertEquals(0x81, mem.read(0xABCD))
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x03)
        cpu.mode = AddressingMode.Absolute
        cpu.address = 0xABCD
        cpu.operand = 0x03
        cpu.p = cpu.C_FLAG
        cpu.ror()
        assertEquals(0x81, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        mem.write(0xABCD, 0x03)
        cpu.mode = AddressingMode.Accumulator
        cpu.address = 0xABCD
        cpu.operand = 0x03
        cpu.a = 0x03
        cpu.p = cpu.C_FLAG
        cpu.ror()
        assertEquals(0x81, cpu.a)
        assertEquals(0x03, mem.read(0xABCD))
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testRti() {
        cpu.c = 0xABCD
        cpu.s = 0xFC
        cpu.p = cpu.I_FLAG
        mem.write(0x01FD, 0x81)
        mem.write(0x01FE, 0x03)
        mem.write(0x01FF, 0x02)
        cpu.rti()
        assertEquals(0x0203, cpu.c)
        assertEquals(0xFF, cpu.s)
        assertEquals(0x81, cpu.p)
    }

    @Test def testRts() {
        cpu.c = 0xABCD
        cpu.s = 0xFD
        cpu.p = 0x11
        mem.write(0x01FE, 0x03)
        mem.write(0x01FF, 0x02)
        cpu.rts()
        assertEquals(0x0204, cpu.c)
        assertEquals(0xFF, cpu.s)
        assertEquals(0x11, cpu.p)
    }

    @Test def testSbc() {
        cpu.a = 0x00
        cpu.operand = 0x01
        cpu.p = cpu.C_FLAG
        cpu.sbc()
        assertEquals(0xFF, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x80
        cpu.operand = 0x01
        cpu.p = cpu.C_FLAG
        cpu.sbc()
        assertEquals(0x7F, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x7F
        cpu.operand = 0xFF
        cpu.p = cpu.C_FLAG
        cpu.sbc()
        assertEquals(0x80, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x00
        cpu.operand = 0x01
        cpu.p = 0
        cpu.sbc()
        assertEquals(0xFE, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x80
        cpu.operand = 0x01
        cpu.p = 0
        cpu.sbc()
        assertEquals(0x7E, cpu.a)
        assert(! cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x7F
        cpu.operand = 0xFF
        cpu.p = 0
        cpu.sbc()
        assertEquals(0x7F, cpu.a)
        assert(  cpu.isFlagSet(cpu.C_FLAG))
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.V_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testSec() {
        cpu.p = 0
        cpu.sec()
        assert(cpu.isFlagSet(cpu.C_FLAG))

        cpu.p = cpu.C_FLAG
        cpu.sec()
        assert(cpu.isFlagSet(cpu.C_FLAG))
    }

    @Test def testSed() {
        cpu.p = 0
        cpu.sed()
        assert(cpu.isFlagSet(cpu.D_FLAG))

        cpu.p = cpu.D_FLAG
        cpu.sed()
        assert(cpu.isFlagSet(cpu.D_FLAG))
    }

    @Test def testSei() {
        cpu.p = 0
        cpu.sei()
        assert(cpu.isFlagSet(cpu.I_FLAG))

        cpu.p = cpu.I_FLAG
        cpu.sei()
        assert(cpu.isFlagSet(cpu.I_FLAG))
    }

    @Test def testSta() {
        mem.write(0x00AB, 0x12)
        cpu.address = 0x00AB
        cpu.a = 0xFA
        cpu.sta()
        assertEquals(0xFA, mem.read(0x00AB))
    }

    @Test def testStx() {
        mem.write(0x00AB, 0x12)
        cpu.address = 0x00AB
        cpu.x = 0xFA
        cpu.stx()
        assertEquals(0xFA, mem.read(0x00AB))
    }

    @Test def testSty() {
        mem.write(0x00AB, 0x12)
        cpu.address = 0x00AB
        cpu.y = 0xFA
        cpu.sty()
        assertEquals(0xFA, mem.read(0x00AB))
    }

    @Test def testTax() {
        cpu.a = 0x00
        cpu.x = 0x12
        cpu.tax()
        assertEquals(0x00, cpu.x)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x01
        cpu.x = 0x00
        cpu.tax()
        assertEquals(0x01, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x91
        cpu.x = 0x00
        cpu.tax()
        assertEquals(0x91, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testTay() {
        cpu.a = 0x00
        cpu.y = 0x12
        cpu.tay()
        assertEquals(0x00, cpu.y)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x01
        cpu.y = 0x00
        cpu.tay()
        assertEquals(0x01, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.a = 0x91
        cpu.y = 0x00
        cpu.tay()
        assertEquals(0x91, cpu.y)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testTsx() {
        cpu.s = 0x00
        cpu.x = 0x12
        cpu.tsx()
        assertEquals(0x00, cpu.x)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.s = 0x01
        cpu.x = 0x00
        cpu.tsx()
        assertEquals(0x01, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.s = 0x91
        cpu.x = 0x00
        cpu.tsx()
        assertEquals(0x91, cpu.x)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testTxa() {
        cpu.x = 0x00
        cpu.a = 0x12
        cpu.txa()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0x01
        cpu.a = 0x00
        cpu.txa()
        assertEquals(0x01, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0x91
        cpu.a = 0x00
        cpu.txa()
        assertEquals(0x91, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testTxs() {
        cpu.x = 0x00
        cpu.s = 0x12
        cpu.txs()
        assertEquals(0x00, cpu.s)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0x01
        cpu.s = 0x00
        cpu.txs()
        assertEquals(0x01, cpu.s)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.x = 0x91
        cpu.s = 0x00
        cpu.txs()
        assertEquals(0x91, cpu.s)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

    @Test def testTya() {
        cpu.y = 0x00
        cpu.a = 0x12
        cpu.tya()
        assertEquals(0x00, cpu.a)
        assert(  cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.y = 0x01
        cpu.a = 0x00
        cpu.tya()
        assertEquals(0x01, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(! cpu.isFlagSet(cpu.N_FLAG))

        cpu.y = 0x91
        cpu.a = 0x00
        cpu.tya()
        assertEquals(0x91, cpu.a)
        assert(! cpu.isFlagSet(cpu.Z_FLAG))
        assert(  cpu.isFlagSet(cpu.N_FLAG))
    }

}
