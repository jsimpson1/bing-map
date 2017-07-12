package js.map

import scala.util.Random
import scala.xml.Elem

object BingMapUtility {

  lazy val minLatitude = -85.05112878
  lazy val maxLatitude = 85.05112878
  lazy val minLongitude = -180
  lazy val maxLongitude = 180

  case class PixelCoordinates(x: Int, y: Int)

  case class TileCoordinates(x: Int, y: Int)

  case class WSG84Coordinate(latitude: Double, longitude: Double)

  case class QuadKey(value: String) {
    def levelOfDetail: Int =
      value.length
  }

  def clip(n: Double, min: Double, max: Double): Double = {
    Math.min(Math.max(n, min), max)
  }

  def mapSize(levelOfDetail: Int): Int =
    256 << levelOfDetail

  def latLongToPixelXY(wSG84Coordinate: WSG84Coordinate, levelOfDetail: Int): PixelCoordinates = {

    val lat = clip(wSG84Coordinate.latitude, minLatitude, maxLatitude)
    val long = clip(wSG84Coordinate.longitude, minLongitude, maxLongitude)

    val x : Double = (long + 180)/360
    val sinLatitude: Double = Math.sin(lat * Math.PI / 180)

    val y = 0.5 - Math.log((1 + sinLatitude) / (1 - sinLatitude)) / (4 * Math.PI)
    val ms = mapSize(levelOfDetail)

    val pixelX = clip(x * ms + 0.5, 0, ms - 1).toInt
    val pixelY =  clip(y * ms + 0.5, 0, ms - 1).toInt
    PixelCoordinates(pixelX, pixelY)
  }

  def tileXYToQuadKey(tileCoordinates: TileCoordinates, levelOfDetail: Int): QuadKey = {
    val levels = (1 to levelOfDetail).toList.reverse
    val quadKeyComponents =
      levels.map { i =>
        var digit = 0
        val mask: Int = 1 << (i-1)
        val xBool = (tileCoordinates.x & mask) != 0
        val yBool = (tileCoordinates.y & mask) != 0
        if (xBool) digit += 1
        if (yBool) digit += 2
        digit
      }
    val value = quadKeyComponents.mkString("")
    QuadKey(value)
  }

  def pixelToTile(pixelCoordinates: PixelCoordinates): TileCoordinates = {
    TileCoordinates(pixelCoordinates.x/256, pixelCoordinates.y/256)
  }


  def getQuadKey(point: WSG84Coordinate, levelOfDetail: Int): QuadKey = {
    val pixelCoordinates = latLongToPixelXY(point, levelOfDetail)
    val tileCoordinates = pixelToTile(pixelCoordinates)
    tileXYToQuadKey(tileCoordinates, levelOfDetail)
  }

  def getQuadKeys(start: WSG84Coordinate, end: WSG84Coordinate, levelOfDetail: Int): IndexedSeq[QuadKey] = {
    val startPixelCoordinates = latLongToPixelXY(start, levelOfDetail)
    val endPixelCoordinates = latLongToPixelXY(end, levelOfDetail)
    val startTile = pixelToTile(startPixelCoordinates)
    val endTile = pixelToTile(endPixelCoordinates)
    (startTile.x to endTile.x).flatMap { x =>
      (startTile.y to endTile.y).map { y =>
        tileXYToQuadKey(TileCoordinates(x,y), levelOfDetail)
      }
    }.sortBy(_.value)
  }

  def getTilesAndHtmlValues(start: WSG84Coordinate, end: WSG84Coordinate, levelOfDetail: Int, mapPieceSize: Int = 256): IndexedSeq[(QuadKey, HtmlMapPiece)] = {
    val startPixelCoordinates = latLongToPixelXY(start, levelOfDetail)
    val endPixelCoordinates = latLongToPixelXY(end, levelOfDetail)
    val startTile = pixelToTile(startPixelCoordinates)
    val endTile = pixelToTile(endPixelCoordinates)
    var hx = 0
    var hy = 0

    (startTile.x to endTile.x).flatMap { x =>
      val rr = (startTile.y to endTile.y).map { y =>
        val r = (TileCoordinates(x,y), HtmlMapPiece(hy, hx, mapPieceSize))
        hx += mapPieceSize
        r
      }
      hy += mapPieceSize
      hx = 0
      rr
    }
      .map{ v =>
        (
          tileXYToQuadKey(v._1, levelOfDetail),
          v._2
        )
      }
      .sortBy(_._1.value)
  }

  case class HtmlMapPiece(x: Int, y: Int, size: Int)

  def getMapAsHtmlSvg(start: WSG84Coordinate, end: WSG84Coordinate, levelOfDetail: Int, mapPieceSize: Int, useGettyTileUrl: Boolean): Elem = {
    <html style="width:100%;height:100%;">
      <head>
        <script src="https://ariutta.github.io/svg-pan-zoom/dist/svg-pan-zoom.js"></script>
      </head>
      <body style="width:100%;height:100%;margin:0;">
        <svg id="svg" width="100%" height="100%">
          {
          getTilesAndHtmlValues(start, end, levelOfDetail, mapPieceSize).map { v =>
            val htmlValue = v._2
            getImageElem(v._1, htmlValue.x, htmlValue.y, htmlValue.size, useGettyTileUrl)
          }
          }
        </svg>
      </body>
      <script>
        {s""" // Don't use window.onLoad like this in production, because it can only listen to one function.
        window.onload = function() {
        // Expose to window namespase for testing purposes
        window.zoomTiger = svgPanZoom('#svg', {
      zoomEnabled: true,
      controlIconsEnabled: true,
      fit: true,
      center: true,
      zoomScaleSensitivity: 0.1,
      minZoom: 0.01,
      maxZoom: 30
      // viewportSelector: document.getElementById('demo-tiger').querySelector('#g4') // this option will make library to misbehave. Viewport should have no transform attribute
    });
    };"""}
      </script>
    </html>
  }



  def getImageElem(quadKey: QuadKey, x: Int, y: Int, mapPieceSize: Int = 256, useGettyTileUrl: Boolean): Elem = {
    val url =
      if (useGettyTileUrl)
        getGettyTileUrl(quadKey)
      else
        getGettyTileUrl(quadKey)

      <image xlink:href={url} x={s"$x"} y={s"$y"}  height={s"${mapPieceSize}px"} width={s"${mapPieceSize}px"}/>
  }

  val randomizer = new Random()

  def getBingTileUrl(quadKey: QuadKey): String =
    s"http://ecn.t${randomizer.nextInt(4)}.tiles.virtualearth.net/tiles/a${quadKey.value}.jpeg?g=5685"

  def getGettyTileUrl(quadKey: QuadKey): String = {
    s"https://getty.accur8.io/tiles/${quadKey.value}.jpeg"
  }

}