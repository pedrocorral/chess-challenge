
package chess.challenge

/**
 * As container of any function about the logic of the game rules
 */
object Rules {

  /**
   * Does a position use the same square than other?
   *
   * @param p0
   * @param p1
   * @return
   */
  def collision( p0: Position, p1: Position ): Boolean = p0._1 == p1._1 && p0._2 == p1._2

  /**
   * Has been used this position (with any other piece) previously?
   *
   * @param position
   * @param distribution
   * @return
   */
  def collision( position: Position, distribution: Distribution ): Boolean = distribution match {
    case Nil => false
    case _ => distribution.exists { collision( position, _ ) }
  }

  /**
   * We want a distribution where all of the pieces do not threaten any other
   *
   * @param distribution
   * @return
   */
  def isValid( distribution: Distribution ): Boolean = distribution match {
    case Nil => true
    case x::xs => xs.forall( y => !collision(x,y) && !mutualThreaten(x,y) ) && isValid( xs )
  }

  def mutualThreaten( p: Position, distribution: Distribution ): Boolean = !distribution.forall( !mutualThreaten(p,_) )

  /**
   * Does any of the pieces threaten the other one?
   *
   * @param p0
   * @param p1
   * @return
   */
  def mutualThreaten( p0: Position, p1: Position ): Boolean = threatens( p0, p1 ) || threatens( p1, p0 )

  /**
   * Does the first piece threat the other?
   *
   * @param p0
   * @param p1
   * @return
   */
  def threatens( p0: Position, p1: Position ): Boolean = p0 match {
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
