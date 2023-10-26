package io

import scala.io.Source
import io.circe._
import io.circe.parser._

case class Scene(col: Int, row: Int, scene: Array[Array[Int]]) {

  val sceneHeight = this.row // 16
  val sceneWidth = this.col  // 24

  val blocksize = 30
  val screenHeight = sceneHeight * blocksize // 480
  val screenWidth = sceneWidth * blocksize + sceneHeight - 2 // 734

  // check if wall
  def isWall(col: Int, row: Int): Boolean = if (col > 0 && row > 0) scene(col)(row) > 0 else true

  //convert pixel position to grid index
  def toGridIndex(pos: Double, pixels: Int, columns: Int): Int = (pos.toInt / (pixels / columns)).floor.toInt
}

// ===== Reads scene from json file and converts to scala code =====
object SceneReader {

  private val sourceFromFile = Source.fromFile("src/main/scala/io/exampleScene2.json")

  // read lines and close file when done
  private val lines = try sourceFromFile.getLines().mkString finally sourceFromFile.close()

  private val sceneJson: Json = parse(lines).getOrElse(Json.Null)

  implicit val SceneDecoder: Decoder[Scene] = Decoder.forProduct3("row", "col", "scene")(Scene.apply) // circe decoder

  private val s = SceneDecoder(sceneJson.hcursor).toOption

  val scene = s.get

}