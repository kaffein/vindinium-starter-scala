package bot

import bot.Tile.Mine
import scala.math._

/**
 * Created by kaffein on 15/03/14.
 */
sealed trait GeoService {

  def getMinePositions(board: Board): Vector[Pos]

  def getClosestPositionsFrom(from: Pos, positions: List[Pos]): List[Pos]

}

object Gaya extends GeoService {

  def is[T](t: (Tile, Int)) = t._1.isInstanceOf[T]

  def getPositions[T](board: Board): Vector[Pos] = board.tiles.zipWithIndex filter (is[T](_)) map {
    t => Pos(getX(t, board.size), getY(t, board.size))
  }

  def getX(tile: (Tile, Int), boardSize: Int) = tile._2 % boardSize

  def getY(tile: (Tile, Int), boardSize: Int) = (tile._2 % boardSize) match {
    case x: Int if (x > 0) => (tile._2 / boardSize) + 1
    case _ => tile._2 / boardSize
  }

  override def getMinePositions(board: Board): Vector[Pos] = getPositions[Mine](board)

  override def getClosestPositionsFrom(from: Pos, positions: List[Pos]): List[Pos] = positions map (p => (p, sqrt((abs(p.x - from.x) + (abs(p.y) - from.y))))) sortWith ((u, v) => u._2 < v._2) map (p => p._1)
}