package com.zhcg.info

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

import scala.io.Source

object FileSpliter {
  def hashByphone(fileName: String) = {
    val inFile = Source.fromFile(fileName, "UTF-8")
    try inFile.getLines.foldLeft(Map.empty[String, PrintWriter]) {
      case (printers, line) =>
        val newLine = line.trim.replace("---", ";")
        val phone = try {
          newLine.split(',')(5).toLong
        } catch {
          case e: Exception =>
            0L
        }
        val id = Math.floorMod(phone, 9L) toString
        val printer = printers.getOrElse(id, new PrintWriter(new File(s"jingdong_$id.csv")))
        printer.println(newLine)
        printers.updated(id, printer)
    }.values.foreach(_.close)
    finally inFile.close

  }

  def splitByStr(fileName: String) = {
    val inFile = Source.fromFile(fileName, "UTF-8")
    try inFile.getLines.foldLeft(Map.empty[String, PrintWriter]) {
      case (printers, line) =>
        val num1 = line.count(x => x == '-') //18
        val num2 = line.split("---").length //7
        (num1, num2) match {
          case (18, 7) => {
            val phoneColumn = try{
              val phone = line.split("---")(5).toLong
              Right(phone)
            }catch{
              case e: Exception => Left("bad")
            }
            val id = phoneColumn match {
              case Right(good) => Math.floorMod(good, 9L)
              case Left(bad) => 9L
            }
            val printer = printers.getOrElse(id.toString, new PrintWriter(new File(s"jd_$id.csv")))
            printer.println(line)
            printers.updated(id.toString, printer)

          }
          case _ => {
            val printer = printers.getOrElse("有问题", new PrintWriter(new File(s"bad.csv")))
            printer.println(line)
            printers.updated("有问题", printer)

          }
        }

    }.values.foreach(_.close)
    finally inFile.close

  }

  def splitter2(stepTwoFileName: String): Unit = {
    val inFile = Source.fromFile(stepTwoFileName, "UTF-8")
    try inFile.getLines.foldLeft(Map.empty[String, PrintWriter]) {
      case (printers, line) =>
        val num2 = line.split("---").length //7
        if (num2 == 7){
          val phoneColumn = try{
            val phone = line.split("---")(5).toLong
            Right(phone)
          }catch{
            case e: Exception => Left("bad")
          }
          val id = phoneColumn match {
            case Right(good) => Math.floorMod(good, 9L)
            case Left(bad) => 9L
          }
          val printer = printers.getOrElse(id.toString, new PrintWriter(new File(s"bad_$id.csv")))
          printer.println(line)
          printers.updated(id.toString, printer)

        }else{
          val printer = printers.getOrElse("有问题", new PrintWriter(new File(s"bad_bad.csv")))
          printer.println(line)
          printers.updated("有问题", printer)

        }

    }.values.foreach(_.close)
    finally inFile.close

  }


  def splitter3(shunFileName: String): Unit = {
    val inFile = Source.fromFile(shunFileName, "UTF16")
    val printWriter = new PrintWriter("shun_result.csv")
    val rex = "(?<=N').*?(?=',)".r
    try {
      inFile.getLines.filter(x => x.startsWith("INSERT")).map(x => x.drop(92).dropRight(1) + ",").map(x => rex.findAllIn(x).map {
        x => if (x == "") "null" else x
      }
        .mkString("")).foreach(printWriter.println)
    }
    finally {
      printWriter.close()
      inFile.close

    }

  }

  def splitter4(wbFileName: String): Unit = {
    val inFile = Source.fromFile(wbFileName)
    try inFile.getLines.foldLeft(Map.empty[Long, PrintWriter]) {
      case (printers, line) =>
        val phone = try {
          val tmp = line.split("\\s")(0)
          tmp.toLong
        } catch {
          case e: Exception =>
            e.printStackTrace()
            0L
        }
        val newLine = line.replaceAll("\\s", ",")
        val id = Math.floorMod(phone, 9L)
        val printer = printers.getOrElse(id, new PrintWriter(new File(s"wb_$id.csv")))
        printer.println(newLine)
        printers.updated(id, printer)
    }.values.foreach(_.close)
    finally inFile.close
  }

  def splitter5(qqFile: String): Unit = {
    val inFile = Source.fromFile(qqFile)
    try inFile.getLines.foldLeft(Map.empty[Long, PrintWriter]) {
      case (printers, line) =>
        val qq = try {
          val tmp = line.split("----")(0)
          tmp.toLong
        } catch {
          case e: Exception =>
            e.printStackTrace()
            0L
        }
        val id = Math.floorMod(qq, 9L)
        val printer = printers.getOrElse(id, new PrintWriter(new File(s"qq_$id")))
        printer.println(line)
        printers.updated(id, printer)
    }.values.foreach(_.close)
    finally inFile.close
  }

  def splitter6(qqPwd: String): Unit = {
    val inFile = Source.fromFile(qqPwd,"x-UTF-16LE-BOM")

    def getHashOrBad(line: String) = {
      try {
        val tmp = line.split("----")(0).toLong
        Right(tmp)
      } catch {
        case e: Exception => Left(9L)
      }
    }

    try inFile.getLines.foldLeft(Map.empty[Long, PrintWriter]) {
      case (printers, line) =>
        val id = getHashOrBad(line) match {
          case Right(good) => Math.floorMod(good, 9L)
          case Left(bad) => bad
        }
        val printer = printers.getOrElse(id, new PrintWriter(new File(s"qqpwd_$id")))
        printer.println(line)
        printers.updated(id, printer)
    }.values.foreach(_.close)
    finally inFile.close

  }

  def main(args: Array[String]): Unit = {
//        val originFilename = "D:\\BaiduNetdiskDownload\\jd.txt"
//        splitByStr(originFilename)
//        val stepTwoFileName = "bad.csv"
//        splitter2(stepTwoFileName)

        val shunFileName = "D:\\BaiduNetdiskDownload\\script.sql"
        splitter3(shunFileName)

    //val wbFileName = "D:\\BaiduNetdiskDownload\\phone uid.txt"
    //splitter4(wbFileName)

    //    val qqFile = "E:\\裤子\\a.txt"
    //    splitter5(qqFile)

//    val qqPwd = "E:\\裤子\\qq----密码"
//    splitter6(qqPwd)


  }


}
