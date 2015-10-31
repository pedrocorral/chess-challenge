
package chess.challenge

/**
 * The rows are called ranks, while the cols are called files
 * The files are usually denoted with letter, but we will use integers
 * because our grid could have different dimensions
 *
 * @param ranks
 * @param files
 */
case class Grid( ranks: Int, files : Int ) {

  /**
   * Should be the distribution builder, based on string like "KRRB" {King,Rook,Rook,Bishop}
   *
   * @param pieces
   * @return
   */
  def initialDistribution( pieces: String ): Distribution = initialDistribution(
    pieces.toList.map { _ match {
      case 'K' => King
      case 'Q' => Queen
      case 'B' => Bishop
      case 'R' => Rook
      case 'N' => Knight
      case x => throw new Error( s"ERRoR::Grid($ranks,$files)::initialDistribution::Invalid piece => '$x' while creating the initial distribution" )
    } }
  )

  def initialDistribution( pieces: List[Piece] ): Distribution = initialDistribution(
    pieces.groupBy( identity ).mapValues( _.length )
  )

  def initialDistribution( pieces: Map[Piece,Int] ): Distribution = {
    val distribution_candidate: Distribution = pieces.flatMap {
      case (piece, total) => List.fill(total)(piece)
    }.scanLeft( (0,-1,new Piece) )( {
      case ( (rank,file,_), piece ) => nextPosition( rank, file, piece ).getOrElse {
        throw new Error( s"ERRoR::Grid($ranks,$files)::initialDistribution::Unable to create piece => $piece after ($rank,$file)" )
      }
    } ).toList.tail

    if ( Rules.isValid( distribution_candidate ) ) distribution_candidate
    else nextValidDistribution( distribution_candidate ).getOrElse {
      throw new Error( s"ERRoR::Grid($ranks,$files)::initialDistribution::Unable to create one single valid distribution" )
    }
  }

  def nextValidDistribution( distribution: Distribution ): Option[Distribution] = {

    def resetPosition( position: Position ) = (0,-1,position._3)

    def nextAvailableDistribution( position: Position, rest_of_distribution: Distribution ): Option[Distribution] = nextPosition( position ) match {
      case None => nextValidDistribution( rest_of_distribution ) match {
        case None => None
        case Some( new_rest_of_distribution ) => nextAvailableDistribution( resetPosition(position), new_rest_of_distribution )
      }
      case Some( new_position ) if !Rules.collision( position, rest_of_distribution ) && Rules.isValid( new_position::rest_of_distribution ) => Some( new_position :: rest_of_distribution )
      case Some( new_position ) => nextAvailableDistribution( new_position, rest_of_distribution )
    }

    distribution match {
      case Nil => None
      case x::xs => nextAvailableDistribution( x, xs )
    }
  }

  def nextPosition( position: Position ): Option[Position] = nextPosition( position._1, position._2, position._3 )
  def nextPosition( rank: Int, file: Int, piece: Piece ): Option[Position] = ( ranks - rank, files - file ) match {
    case (1,1) => None
    case (_,1) => Some( (rank+1,0,piece) )
    case _ => Some( (rank,file+1,piece) )
  }

}

