package com.github.gekomad.chessgengenerator.util

import com.github.gekomad.chessgengenerator.core.ChessBoard.BitmapPosition

object PrintAndSum {

  def sumAndPrint(l: Option[(Long, Seq[(String, BitmapPosition)])]): Option[Long] = {
    println()
    l.map { z =>
      val x         = z._2
      val time: Int = (z._1 / 1000).toInt
      val o = (x zip (1 to 10000)).map { a =>
        println(s"${a._2})\t${a._1._1}\t${a._1._2}")
        a._1._2
      }
      val tot = o.sum
      print(s"\nTot $tot nodes")
      if (time != 0)
        print(s" in $time seconds (${(tot / time) / 1000}k nodes per seconds)")
      println()
      tot
    }
  }

}
