package engine

import scala.math._

/**
 * @param pos = player pos
 * @param dir = player direction vector (0.0 < dir < 1.0)
 * @param plane = player plane vector (0.0 < plane < 1.0)
 * ratio between dir and plane determines player/camera field of view (FOV)
 */

class Player(pos: Vector2D, dir: Vector2D, plane: Vector2D) {

  private val moveSpeed = 5.0
  private val rotSpeed = 10.0 * scala.math.Pi / 180

  var posX = pos.x
  var posY = pos.y

  var dirX = dir.x
  var dirY = dir.y

  var moveDirX = dirX * moveSpeed
  var moveDirY = dirY * moveSpeed

  var planeX = plane.x
  var planeY = plane.y

  var FOV = 2 * math.atan(plane.length/ dir.length)  // "how much" of the scene the player sees

  def moveForward() = {
    posX += dirX * moveSpeed
    posY += dirY * moveSpeed
  }

  def moveBackward() = {
    posX -= dirX * moveSpeed
    posY -= dirY * moveSpeed
  }

  // rotation matrix
  def rotateLeft() = {
    val oldDirX = dirX
    dirX = dirX * cos(-rotSpeed) - dirY * sin(-rotSpeed)
    dirY = oldDirX * sin(-rotSpeed) + dirY * cos(-rotSpeed)
    val oldPlaneX = planeX
    planeX = planeX * cos(-rotSpeed) - planeY * sin(-rotSpeed)
    planeY = oldPlaneX * sin(-rotSpeed) + planeY * cos(-rotSpeed)
  }

  def rotateRight() = {
    val oldDirX = dirX
    dirX = dirX * cos(rotSpeed) - dirY * sin(rotSpeed)
    dirY = oldDirX * sin(rotSpeed) + dirY * cos(rotSpeed)
    val oldPlaneX = planeX
    planeX = planeX * cos(rotSpeed) - planeY * sin(rotSpeed)
    planeY = oldPlaneX * sin(rotSpeed) + planeY * cos(rotSpeed)
  }

}
