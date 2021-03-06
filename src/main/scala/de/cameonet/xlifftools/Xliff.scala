package de.cameonet.xlifftools

import java.io.{File, PrintWriter}

import com.typesafe.config.Config
import play.api.libs.json.{JsObject, JsString, Json}

import scala.collection.JavaConversions._
import scala.collection.Set
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node, NodeSeq, XML}

/**
 * User: Björn Reimer
 * Date: 16.10.14
 * Time: 14:04
 */
object XliffFactory {

  def apply(targetLang: String, sourceLang: String): Xliff = {
    val emptyXliff =
      <xliff xmlns="urn:oasis:names:tc:xliff:document:1.1" version="1.1">
        <file source-language={sourceLang} datatype="plaintext" original={sourceLang + ".json"}>
          <body>
          </body>
        </file>
      </xliff>

    new Xliff(emptyXliff, targetLang)
  }

  def apply(file: File): Xliff = {
    // todo: deal with xml parse errors
    val xml = XML.loadFile(file)
    val targetLang = Main.getFileNameWithoutExtention(file)

    new Xliff(xml, targetLang)
  }
}

class Xliff(doc: NodeSeq, targetLang: String) {

  override def toString = {
    this.doc.toString()
  }

  def getKeys: Set[String] = {
    val transUnits = doc \\ "trans-unit"
    transUnits.map(n => (n \ "@id").text).toSet
  }

  def getKeysAndTarget: Seq[(String, String)] = {
    val transUnits = doc \\ "trans-unit"
    transUnits.map {
      node =>
        val key = (node \ "@id").text
        val target = (node \\ "target").text
        (key, target)
    }
  }

  def flattenJson(j: JsObject): Map[String, String] = {
    j.keys
      .map {
      key =>
        j \ key match {
          case s: JsString => Map(key -> s.value)
          case o: JsObject => flattenJson(o).map { case (k, v) => (key + "." + k, v)}
          case e => throw new ConversionException("Json file contains non string value: " + e)
        }
    }
      .foldLeft[Map[String, String]](Map())(_ ++ _)
  }

  def createTransUnit(id: String, source: String): Node = {
    <trans-unit id={id}>
      <source>
        {source}
      </source>
      <target></target>
    </trans-unit>
  }

  /*
   * XML Transformer
   */
  private def updateSource(sourceText: String) = new RewriteRule {
    override def transform(subNode: Node): NodeSeq = subNode match {
      case e: Elem if e.label == "source" => <source>
        {sourceText}
      </source>
      case e => e
    }
  }

  private def deleteAndUpdate(delete: Set[String], source: Map[String, String]) = new RewriteRule {
    override def transform(node: Node): NodeSeq = node match {
      case e: Elem if delete.contains((e \ "@id").text) => NodeSeq.Empty
      case e: Elem if e.label == "trans-unit" =>
        val id = (e \ "@id").text
        val sourceText = source(id)
        new RuleTransformer(updateSource(sourceText)).transform(e)
      case e => e
    }
  }

  private def addNew(add: Set[String], source: Map[String, String]) = new RewriteRule {
    val newNodes = add.map {
      id =>
        val sourceText = source(id)
        createTransUnit(id, sourceText)
    }.toSeq

    override def transform(node: Node): NodeSeq = node match {
      case e: Elem if e.label == "body" =>
        val copy = e.copy(child = e.child ++ newNodes)
        copy
      case e => e
    }
  }

  // methods
  def update(source: Map[String, String]): Xliff = {
    // get keys from source
    val sourceKeys = source.keySet

    // determine keys that need to be deleted and created
    val targetKeys = this.getKeys
    val delete = targetKeys.diff(sourceKeys)
    val add = sourceKeys.diff(targetKeys)

    // delete and update existing keys
    val updated = new RuleTransformer(deleteAndUpdate(delete, source)).transform(doc)
    // create new keys
    val newAndUpdated = new RuleTransformer(addNew(add, source)).transform(updated)

    new Xliff(newAndUpdated, targetLang)
  }

  def updateFromJson(json: JsObject): Xliff = {
    update(flattenJson(json))
  }


  def updateFromProperties(properties: Config): Xliff = {
    val map = properties.entrySet().map { entry =>
      (entry.getKey, entry.getValue.render.replace("\"", ""))
    }
    update(map.toMap)
  }


  def setTargetsFromJson(json: JsObject): Xliff = {

    def updateTarget(targetText: String) = new RewriteRule {
      override def transform(subNode: Node): NodeSeq = subNode match {
        case e: Elem if e.label == "target" => <target>
          {targetText}
        </target>
        case e => e
      }
    }

    def updateTargets(targets: Map[String, String]) = new RewriteRule {
      override def transform(node: Node): NodeSeq = node match {
        case e: Elem if e.label == "trans-unit" && targets.keySet.contains((e \ "@id").text) =>
          val id = (e \ "@id").text
          val targetText = targets(id)
          new RuleTransformer(updateTarget(targetText)).transform(e)
        case e => e
      }
    }

    val targets = flattenJson(json)
    val updated = new RuleTransformer(updateTargets(targets)).transform(this.doc)

    new Xliff(updated, targetLang)
  }


  def toJson: JsObject = {
    def createJson(key: String, value: String): JsObject = {
      key.split('.').toList match {
        case Nil => Json.obj()
        case k :: Nil => Json.obj(k -> value)
        case k :: rest => Json.obj(k -> createJson(rest.mkString("."), value))
      }
    }

    getKeysAndTarget.foldLeft[JsObject](Json.obj()) {
      case (acc, (key, value)) => acc.deepMerge(createJson(key, value))
    }
  }

  def writeToFile(targetDir: File) = {
    //    val prettyPrinter = new scala.xml.PrettyPrinter(120, 2)
    val fileName = new File(targetDir, targetLang + ".xlf").getAbsolutePath
    XML.save(fileName, XML.loadString(this.doc.toString()), "UTF-8", xmlDecl = true)
  }

  def writeToJsonFile(targetDir: File) = {
    val file = new File(targetDir, targetLang + ".json").getAbsolutePath
    val writer = new PrintWriter(file)
    val json = this.toJson
    writer.write(Json.prettyPrint(json))
    writer.close()
  }
}
