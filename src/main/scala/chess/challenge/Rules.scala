
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

  /**
   * Does the first piece threat the other?
   *
   * @param p0
   * @param p1
   * @return
   */
  def threats( p0: Position, p1: Position ): Boolean = p0 match {
    case (_,_,Rook) => rookThreatens( p0, p1 )
    case (_,_,Bishop) => bishopThreatens( p0, p1 )
    case (_,_,King) => kingThreatens( p0, p1 )
    case (_,_,Knight) => knightThreatens( p0, p1 )
    case (_,_,Queen) => queenThreatens( p0, p1 )
  }

  def rookThreatens( r: Position, p: Position ): Boolean = r._1 == p._1 || r._2 == p._2
  def bishopThreatens( b: Position, p: Position ): Boolean = Math.abs( b._1 - p._1 ) == Math.abs( b._2 - p._2 )
  def kingThreatens( k: Position, p: Position ): Boolean = Math.abs( p._1 - k._1 ) <= 1 && Math.abs( p._2 - k._2 ) <= 1
  def knightThreatens( k: Position, p: Position ): Boolean = ( Math.abs( p._1 - k._1), Math.abs( p._2 - k._2 ) ) match {
    case (1,2) | (2,1) => true
    case _ => false
  }
  def queenThreatens( q: Position, p: Position ): Boolean = rookThreatens( q, p ) || bishopThreatens( q, p )

}
