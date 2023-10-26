import engine._
import io.SceneReader

import swing._
import java.awt.Color._
import java.awt.event.KeyEvent
import java.awt.{Color, Dimension, Graphics2D}
import scala.math._
import scala.swing.event.KeyPressed

object RayCaster extends SimpleSwingApplication {

  val p = new Player(
    new Vector2D(200.0, 100.0), // position
    new Vector2D(1.0,0.0),      // direction
    new Vector2D(0,0.66))       // plane
  val s = SceneReader.scene     // scene that was read from a json-file

  def top = new MainFrame {
    title    = "Raycaster"
    contents = new BoxPanel(Orientation.Horizontal)
    contents = new Raycasterpanel
    size = new Dimension(2 * s.screenHeight, s.screenWidth)
  }

  class Raycasterpanel extends Panel {
    override def paintComponent(g : Graphics2D) = {
      //map background
      g.setColor(DARK_GRAY)
      g.fillRect(0, 0, s.screenHeight, s.screenWidth)

      // draw walls and floors on map
      for ( i <- 0 until s.sceneHeight) {
        for ( j <- 0 until s.sceneWidth ) {
          if (s.isWall(i,j)) {
            s.scene(i)(j) match {
              case 1 => g.setColor(RED)
              case 2 => g.setColor(GREEN)
              case 3 => g.setColor(BLUE)
            }
          }
          else g.setColor(BLACK)
          g.fillRect( 1 + i * s.blocksize, 1 + j * s.blocksize, s.blocksize - 2, s.blocksize - 2)
        }
      }
      // draw player on map as triangle
      g.setColor(PINK)
      g.fillPolygon(
        Array(p.posX, p.posX - 20 * p.dirX - 10 * p.planeX, p.posX - 20 * p.dirX + 10 * p.planeX).map(_.toInt),
        Array(p.posY, p.posY - 20 * p.dirY - 10 * p.planeY, p.posY - 20 * p.dirY + 10 * p.planeY).map(_.toInt),
        3)

      // 3D scene background
      g.setColor(BLACK)
      g.fillRect(s.screenHeight, 0, 2 * s.screenHeight, s.screenWidth)

      // ===== RAY CASTER =====

      // start loop that continually calulates the rays from player out into scene. It goes through every x-position on the screen
      for (x <- 0 until s.screenHeight) {

        val cameraX = 2 * x / s.screenHeight.toDouble - 1 // CameraX is the x-coordinate in camera space (left side is -1, middle 0 and right side 1)
        val raydir = new Vector2D(p.dirX + p.planeX * cameraX, p.dirY + p.planeY * cameraX) // ray direction vector
        // map represents which tile of the map where in
        var mapX = s.toGridIndex(p.posX, s.screenWidth, s.sceneWidth)
        var mapY = s.toGridIndex(p.posY, s.screenHeight, s.sceneHeight)
        // length of ray form current position inside a tile to closest x or y-border (calculated below)
        var sideDist = new Vector2D(0.0, 0.0)
        // length of ray from one x or y-border to next x or y-border.
        // Based on the Pythagorean theorem of the ray direction vector but can be simplified since only the ratio between deltadistance for x and y matters in DDA later
        var deltaDist = {
          val x = if (raydir.x == 0 ) abs(1 / 1e30) else abs( 1 / raydir.x) // division by zero is avoided by setting to high number
          val y = if (raydir.y == 0 ) abs(1 / 1e30) else abs( 1 / raydir.y)
          new Vector2D(x * s.blocksize, y * s.blocksize) // scaled to grid size
        }
        // which direction to step in (+1 or -1) on map
        var stepX = 0
        var stepY = 0
        // was a wall hit? (yes = 1, no = 0)
        var hit = 0
        // was a x-side (Up-Down = 0) or y-side (Left-Right = 1) wall hit?
        var side = 0.5
        // calculate step and initial side distance to closest X- and Y-border depending on if ray direction is negative or positive
        if (raydir.x < 0) {
          stepX = -1
          sideDist.x = p.posX - (mapX * s.blocksize)
        } else {
          stepX = 1
          sideDist.x = (mapX + 1) * s.blocksize - p.posX
        }
        if (raydir.y < 0) {
          stepY = -1
          sideDist.y = p.posY - (mapY * s.blocksize)
        } else {
          stepY = 1
          sideDist.y = (mapY + 1) * s.blocksize - p.posY
        }

        // perform DDA: Go through every border on the tile in the ray direction until a wall is hit.
        while (hit == 0) {
          // jump to next map square, either in x or y-direction
          if (sideDist.x < sideDist.y) {
            sideDist.x += deltaDist.x
            mapX += stepX
            side = 0
          } else {
            sideDist.y += deltaDist.y
            mapY += stepY
            side = 1
          }
          // check if the ray hit a wall
          if (s.isWall(mapX, mapY)) hit = 1
        }

        // calculate distance to wall, that is, projected distance from wall to camera plane
        // since we are adding the deltadistance before checking if we hit a wall in the DDA, that distance has to be subtracted here.
        val perpWallDist = if (side == 0) sideDist.x - deltaDist.x else sideDist.y - deltaDist.y

        // calculate height of line to draw on screen
        var lineHeight = s.screenHeight * 10 / perpWallDist // pixelcoordinates
        // calculate lowest and highest pixel to fill in current stripe
        val wallColor: Color = {
          // normalize lineheight (0 to screenheight) to RGB value (45 to 255), since the image is a bit too dark if the whole range is used
          val normalized = if (lineHeight.isInfinity || lineHeight <= 0 || lineHeight > s.screenHeight) 255 else 45 + ((lineHeight) / s.screenHeight * 210).toInt
          if (mapX >= 0 && mapY >= 0 ) {
            s.scene(mapX)(mapY) match {
              case 1 => new Color(normalized,0,0)   // RED
              case 2 => new Color(0,normalized,0)   // GREEN
              case 3 => new Color(0,0,normalized)   // BLUE
              case _ => BLACK
            }
          } else BLACK
        }
        g.setColor(wallColor)
        // calculate lowest and highest pixel to fill in current stripe
        var drawStart = -lineHeight / 2 + s.screenHeight / 2
        if (drawStart < 0 ) drawStart = 0
        var drawEnd = lineHeight / 2 + s.screenHeight / 2
        if (drawEnd >= s.screenHeight ) drawEnd = s.screenHeight - 1

        // draw line that is part of the final image, rendered image is positioned to the right of the map
        g.drawLine(x + s.screenHeight, drawStart.toInt, x + s.screenHeight, drawEnd.toInt)

        // draw player FOV i.e what the player "sees" on the map
        val wall2D = new Vector2D(p.posX + perpWallDist * raydir.x, p.posY + perpWallDist * raydir.y)
        g.setColor(WHITE)
        g.drawLine(p.posX.toInt, p.posY.toInt, wall2D.x.toInt, wall2D.y.toInt)
      }
    }
    // key eventlistening
    focusable = true
    listenTo(this.keys)
    reactions +=  {
      case keyEvent: KeyPressed => {
        keyEvent.key.id match {
          case KeyEvent.VK_UP    => {
            // are we moving into a wall? If yes, close the program else move
            if (!s.isWall(s.toGridIndex(p.posX + p.moveDirX, s.screenWidth, s.sceneWidth), s.toGridIndex(p.posY + p.moveDirY, s.screenHeight, s.sceneHeight))) {
              p.moveForward()
            } else {
              println("You walked into a wall. Program closed.")
              quit()
            }
          }
          case KeyEvent.VK_DOWN  => {
            if (!s.isWall(s.toGridIndex(p.posX - p.moveDirX, s.screenWidth, s.sceneWidth), s.toGridIndex(p.posY - p.moveDirY, s.screenHeight, s.sceneHeight))) {
              p.moveBackward()
            } else {
              println("You walked into a wall. Program closed.")
              quit()
            }
          }
          case KeyEvent.VK_LEFT  =>  p.rotateLeft()
          case KeyEvent.VK_RIGHT =>  p.rotateRight()
          case _                 =>  // do nothing
        }
      }
        this.repaint() // repaint picture
    }
  }
}

