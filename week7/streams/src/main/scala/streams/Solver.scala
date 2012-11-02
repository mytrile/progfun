package streams

import common._

trait Solver extends GameDef {

  def done(b: Block): Boolean = b == Block(goal, goal)

  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] =
    b.legalNeighbors.map(x => x match {case (s,t) => (s, t :: history)}).toStream

  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] =
    neighbors filterNot (explored contains _._1)

  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = initial match {
    case (b,h) #:: xs => {
      val ys = newNeighborsOnly(neighborsWithHistory(b,h), explored)
      (b,h) #:: from(xs ++ ys, explored + b)
    }
    case _ => Stream.empty
  }

  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock, Nil)), Set.empty)

  lazy val pathsToGoal: Stream[(Block, List[Move])] = pathsFromStart.filter(b => done(b._1))

  lazy val solution: List[Move] = pathsToGoal match {
    case x #:: _ => x._2.reverse
    case _ => Nil
  }
}