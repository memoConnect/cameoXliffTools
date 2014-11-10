package de.cameonet.xlifftools

import java.io.{File, FileInputStream}
import java.util.Properties

import com.fasterxml.jackson.core.JsonParseException
import com.typesafe.config.{ConfigFactory, Config}
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import play.api.libs.json.{JsObject, Json}

import scala.io.Source

/**
 * User: Björn Reimer
 * Date: 15.10.14
 * Time: 14:47
 */

object Main {

  def main(args: Array[String]) {

    // set charset to utf-8
    System.setProperty("file.encoding", "UTF-8")

    val parser = new ArgotParser("cameoXliffTools")

    val mode = parser.option[String](List("m", "mode"), "MODE", "[import|export|merge] required")

    val jsonSource = parser.option[File](List("s", "source"), "FILE", "source file for JSON import") {
      (s, opt) =>
        val file = new File(s)
        if (!file.exists) {
          parser.usage("Source file \"" + s + "\" does not exist.")
        }
        if (!file.isFile) {
          parser.usage("Source \"" + s + "\" is not a file.")
        }
        file
    }

    val xliffDefault = "xliff"
    val maybeXliffDir = parser.option[File]("xliff-dir", "DIRECTORY", "path to xliff files. Default: ./" + xliffDefault) {
      (s, opt) => new File(s)
    }

    val jsonDefault = "json"
    val maybeJsonDir = parser.option[File]("json-dir", "DIRECTORY", "path to json files. Default: ./" + jsonDefault) {
      (s, opt) => new File(s)
    }

    val propertiesDefault = "properties"
    val maybePropertiesDir = parser.option[File]("properties-dir", "DIRECTORY", "path to java property files. Default: ./" + propertiesDefault) {
      (s, opt) => new File(s)
    }

    val languages = parser.multiOption[String](List("l", "languages"), "LANG", "Will be created if it does not exist.")

    try {
      parser.parse(args)

      val xliffDir: File = maybeXliffDir.value.getOrElse(new File(xliffDefault))
      val jsonDir: File = maybeJsonDir.value.getOrElse(new File(jsonDefault))
      val propertiesDir: File = maybePropertiesDir.value.getOrElse(new File(propertiesDefault))

      // check directories
      if (!xliffDir.exists() && !xliffDir.mkdirs()) {
        throw new ConversionException("could not create xliff-dir: " + xliffDir.getAbsolutePath)
      }
      if (!xliffDir.isDirectory) {
        parser.usage("not a directory: " + xliffDir.getAbsolutePath)
      }

      if (!jsonDir.exists() && !jsonDir.mkdirs()) {
        throw new ConversionException("could not create json-dir: " + jsonDir.getAbsolutePath)
      }
      if (!jsonDir.isDirectory) {
        parser.usage("not a directory: " + jsonDir.getAbsolutePath)
      }

      if (!propertiesDir.exists() && !propertiesDir.mkdirs()) {
        throw new ConversionException("could not create properties-dir: " + propertiesDir.getAbsolutePath)
      }
      if (!propertiesDir.isDirectory) {
        parser.usage("not a directory: " + propertiesDir.getAbsolutePath)
      }

      mode.value match {
        case Some("import") =>
          // check if source file exists
          jsonSource.value match {
            case None => parser.usage("no source file defined")
            case Some(file) => importFile(file, xliffDir, languages.value)
          }
        case Some("export") => exportToJson(jsonDir, xliffDir)
        case Some("merge") =>
          // check if source file exists
          jsonSource.value match {
            case None => parser.usage("no source file defined")
            case Some(file) => mergeJson(file, xliffDir)
          }
        case _ => parser.usage("No mode selected.")
      }

      println("DONE")

    } catch {
      case e: ArgotUsageException => println(e.message)
      case e: ConversionException => println("ERROR: " + e.message)
      case e: JsonParseException => println("Error parsing json: " + e.getMessage)
    }

  }

  def importFile(sourceFile: File, xliffDir: File, languages: Seq[String]): Unit = {

    // get source language from name of jsonFile
    val sourceLang = getFileNameWithoutExtention(sourceFile)
    println("Using \"" + sourceLang + "\" as source language")

    // get existing xliff files
    val xliffFiles: Array[File] = xliffDir.listFiles.filter(_.getName.endsWith(".xlf"))

    // find languages that do not exist
    val newLanguages = languages.filter(lang => !xliffFiles.exists(getFileNameWithoutExtention(_).equals(lang)))

    // read existing xliff files and create empty ones for new languages
    val xliffs: Seq[Xliff] = xliffFiles.toSeq.map(XliffFactory(_)) ++ newLanguages.map(XliffFactory(_, sourceLang))

    // get file type of source from extension and update xliffs accordingly
    sourceFile.getName.split('.').last match {
      case "json" =>
        // parse json
        val json = Json.parse(Source.fromFile(sourceFile).mkString).as[JsObject]
        xliffs.foreach(_.updateFromJson(json).writeToFile(xliffDir))
      case "properties" =>
        // parse properties file
        val properties: Config = ConfigFactory.parseFile(sourceFile)
        xliffs.foreach(_.updateFromProperties(properties).writeToFile(xliffDir))
      case x => throw new ConversionException("unsupported file type: " + x)
    }
  }


  def exportToJson(jsonDir: File, xliffDir: File): Unit = {
    // read existing xliff files
    val xliffs: Seq[Xliff] = xliffDir.listFiles.filter(_.getName.endsWith(".xlf")).toSeq.map(XliffFactory(_))
    xliffs.foreach(_.writeToJsonFile(jsonDir))
  }

  def mergeJson(jsonFile: File, xliffDir: File): Unit = {

    // find language of json file
    val lang = getFileNameWithoutExtention(jsonFile)

    // parse  json
    val json = Json.parse(Source.fromFile(jsonFile).mkString).as[JsObject]

    // try to find corresponding xliff
    val xliffFiles = xliffDir.listFiles.filter(_.getName.endsWith(".xlf"))
    xliffFiles.find(file => getFileNameWithoutExtention(file).equals(lang)) match {
      case None => throw new ConversionException("no corresponding xliff found: " + jsonFile)
      case Some(file) => XliffFactory(file).setTargetsFromJson(json).writeToFile(xliffDir)
    }
  }

  def getFileNameWithoutExtention(file: File): String = file.getName.replaceFirst("[.][^.]+$", "")
}

class ConversionException(val message: String) extends RuntimeException(message)
