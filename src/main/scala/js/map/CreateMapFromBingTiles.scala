package js.map

import js.map.BingMapUtility.WSG84Coordinate

object CreateMapFromBingTiles extends App {

  /* Note to self about enlarging map
   * latitude = y axis
   * longitude = x axis
   */

  val sizeOfOriginalMap = 0.037262

  apply(WSG84Coordinate(39.812515 + sizeOfOriginalMap , -77.276845), WSG84Coordinate(39.775253, -77.211525), 17)
  //  apply(WSG84Coordinate(39.812515, -77.276845), WSG84Coordinate(39.775253, -77.211525), 17) // map until 7/1/2017

  def apply(startPoint: WSG84Coordinate, endPoint: WSG84Coordinate, levelOfDetail: Int) = {
    BingTileDownloader.apply(startPoint, endPoint, levelOfDetail)
    TileCombiner.apply(startPoint, endPoint, levelOfDetail)
    println("map is ./composite.jpeg")
  }

}
ls -alp