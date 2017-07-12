package js.map


import js.map.BingMapUtility.WSG84Coordinate
import java.io._
import java.util.concurrent.atomic.AtomicInteger

import org.apache.commons.io.IOUtils


object BingTileDownloader extends App {

  apply(WSG84Coordinate(39.812515, -77.276845), WSG84Coordinate(39.775253, -77.211525), 17)

  def apply(startPoint: WSG84Coordinate, endPoint: WSG84Coordinate, levelOfDetail: Int): Unit = {
    val quadkeyUrls =
      BingMapUtility
        .getTilesAndHtmlValues(startPoint, endPoint, levelOfDetail, 256)
        .map(_._1)
        .map(qk => qk -> BingMapUtility.getBingTileUrl(qk))
        .toIndexedSeq

    val counter = new AtomicInteger()

    quadkeyUrls
      .par
      .foreach { case (qk, url) =>
        val i = counter.incrementAndGet()
        import sys.process._
        import java.net.URL

        val dir = new File("tiles")
        dir.delete()
        dir.mkdirs()

        val file = new java.io.File(s"tiles/${qk.value}.jpeg")
        file.createNewFile()

        if (file.exists) {
          println(s"downloading ${i} of ${quadkeyUrls.size} ${url}")

          val is = new URL(url).openConnection().getInputStream
          val bytes: Array[Byte] = IOUtils.toByteArray(is)
          val fos = new FileOutputStream(file)

          fos.write(bytes)
          fos.flush()
          fos.close()

        }
      }
  }

}
