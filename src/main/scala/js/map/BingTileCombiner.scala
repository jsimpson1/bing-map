package js.map

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import js.map.BingMapUtility.WSG84Coordinate

object BingTileCombiner extends App {


  apply(WSG84Coordinate(39.812515, -77.276845), WSG84Coordinate(39.775253, -77.211525), 17)

  def apply(startPoint: WSG84Coordinate, endPoint: WSG84Coordinate, levelOfDetail: Int): Unit = {

    val tiles =
      BingMapUtility
        .getTilesAndHtmlValues(startPoint, endPoint, levelOfDetail)

    val width =
      tiles
        .map( t => t._2.x + t._2.size )
        .max

    val height =
      tiles
        .map( t => t._2.y + t._2.size )
        .max

    val compositeImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val gr =
      compositeImage
        .createGraphics()

    tiles.foreach { case (qk, tileInfo) =>
      val tileImage = ImageIO.read(new java.io.File(s"./tiles/${qk.value}.jpeg"))
      gr.drawImage(tileImage, tileInfo.x, tileInfo.y, null)
      println(s"${qk.value}-${tileInfo.x}-${tileInfo.y}")
    }

    gr.dispose()

    ImageIO.write(compositeImage, "jpeg", new java.io.File("composite.jpeg"))
  }



}
