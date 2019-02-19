package com.github.gekomad.chessgengenerator.core

object Def {

  @inline final def BITScanForward(b: Long): Int = java.lang.Long.numberOfTrailingZeros(b)

  @inline final def BITScanReverse(b: Long): Int = 63 - java.lang.Long.numberOfLeadingZeros(b)

  @inline final def resetLSB(bits: Long): Long = bits & (bits - 1)

  @inline final def bitCount(bits: Long): Int = java.lang.Long.bitCount(bits)

}
