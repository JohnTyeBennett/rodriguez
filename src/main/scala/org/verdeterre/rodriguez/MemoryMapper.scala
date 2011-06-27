package org.verdeterre.rodriguez

class MemoryMapper {

    val data = new Array[Int](0x10000)

    def clear(start: Int = 0, end: Int = 0xFFFF) {
        (start to end).foreach(data(_) = 0x00)
    }

    def read(address: Int): Int = data(address)

    def write(address: Int, byte: Int) {
        data(address) = byte
    }

    def write(startAt: Int, bytes: Array[Int]) {
        for (i <- 0 until bytes.size) write(startAt + i, bytes(i))
    }

    def write(bytes: Array[Int]) {
        write(0, bytes)
    }

}
