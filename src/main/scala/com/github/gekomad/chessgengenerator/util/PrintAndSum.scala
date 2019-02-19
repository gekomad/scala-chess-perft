package com.github.gekomad.chessgengenerator.util

import com.github.gekomad.chessgengenerator.core.ChessBoard.BitmapPosition

object PrintAndSum {

  def printAndSum(l: Option[Seq[(String, BitmapPosition)]], s: Long = 0): Long = {
    println
    l.map { x =>
      val o = (x zip (1 to 10000)).map { a =>
        println(s"${a._2})\t${a._1._1}\t${a._1._2}")
        a._1._2
      }
      val tot = o.sum
      print(s"Tot $tot nodes")
      if (s != 0)
        print(s" in $s seconds (${(tot / 1000) / s}k nodes per seconds)")
      println

      tot
    }
  }.getOrElse(-1)

}
