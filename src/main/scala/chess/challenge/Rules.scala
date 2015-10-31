
package chess.challenge

/**
 * As container of any function about the logic of the game rules
 */
object Rules {

  def collision( p0: Position, p1: Position ): Boolean = p0._1 == p1._1 && p0._2 == p1._2
  def collision( position: Position, distribution: Distribution ): Boolean = distribution match {
    case Nil => false
    case _ => distribution.exists { collision( position, _ ) }
  }

  def isValid( distribution: Distribution ): Boolean = true // .: For the moment, everything is valid :. //

}
