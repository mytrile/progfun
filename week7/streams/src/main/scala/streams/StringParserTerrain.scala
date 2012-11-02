package streams

import common._

trait StringParserTerrain extends GameDef {

  val level: String

  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean =
    p => if (p.x < 0 || p.y < 0) false
         else if (p.x > levelVector.length - 1) false
         else if (p.y > levelVector(0).length - 1) false
         else levelVector(p.x)(p.y) != '-'

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    def x = levelVector.indexWhere(_.indexWhere(_ == c) > -1)
    def y = levelVector(x).indexWhere(_ == c)
    Pos(x, y)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}