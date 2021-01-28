package com.zhcg.info

import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

object CharSetTester extends App {
  def testCharset(fileName: String) {
    val charsetsKey = Charset.availableCharsets().keySet().toArray()

    for (char <- charsetsKey) {
      var line = 0
      var isSuccess = true
      try {
        val charset = Charset.availableCharsets().get(char)
        println(charset)
        val b = Files.newBufferedReader(Paths.get(fileName), charset)
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

  testCharset("E:\\裤子\\qq----密码")


}
