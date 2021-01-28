package com.zhcg.info

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object tmp extends App{

  val list = List("x-UTF-16LE-BOM")
  for (char <- list) {
    var line = 0
    var isSuccess = true
    try {
      val charset = Charset.availableCharsets().get(char)
      println(charset)
      val b = Files.newBufferedReader(Paths.get("E:\\裤子\\qq----密码"), charset)
      while (b.ready()) {
        b.readLine()
        line += 1
      }

    } catch {
      case e: Exception => {
        isSuccess = false
        println(char + " failed on line " + line)
      }
    }
    if (isSuccess) println("*************************  Successs " + char)

  }
}
