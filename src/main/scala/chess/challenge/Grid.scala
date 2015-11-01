
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

  def apply[A]( pieces: String, first_value: A, foldResults: (A,Distribution) => A ): A = apply(
    pieces.toList.map { _ match {
      case 'K' => King
      case 'Q' => Queen
      case 'B' => Bishop
      case 'R' => Rook
      case 'N' => Knight
      case x => throw new Error( s"ERRoR::Grid($ranks,$files)::initialDistribution::Invalid piece => '$x' while creating the initial distribution" )
    } },
    first_value,
    foldResults
  )

  def apply[A]( pieces: List[Piece], first_value: A, foldResults: (A,Distribution) => A ): A = apply(
    pieces.groupBy( identity ).mapValues( _.length ),
    first_value,
    foldResults
  )

  def apply[A]( pieces: Map[Piece,Int], first_value: A, foldResults: (A,Distribution) => A): A = {
    val all_sorted_pieces = pieces.map {
      case (piece, total) => List.fill(total)(piece)
    }.flatten.toList

    distribute( None, Nil, all_sorted_pieces, first_value, foldResults )
  }

  def indexToPosition( index: Int, piece: Piece ): Position = ( index/ranks, index%ranks, piece )

  def distribute[A]( previous: Option[(Int,Piece)], valid_distribution: Distribution, pieces: List[Piece], previous_value: A, foldResults: (A,Distribution) => A ): A =
    pieces match {
      case x::xs => {
        def startFrom = previous match {
          case None => 0
          case Some( (_,piece) ) if x != piece => 0
          case Some( (index,piece) ) => index + 1
        }
        val previous_indices = valid_distribution.map { case( rank, file, _ ) => rank*ranks + file }
        ( startFrom until ranks*files ).filter( !previous_indices.contains(_) ).foldLeft( previous_value ) {
          case ( previous_value, index ) =>
            val position = indexToPosition( index, x )
            if ( Rules.mutualThreaten( position, valid_distribution ) ) previous_value
            else distribute( Some(index,x), position::valid_distribution, xs, previous_value, foldResults )
        }
      }
      case Nil => valid_distribution match {
        case Nil => throw new Error( "ERRoR::Grid($ranks,$files)::distribute::No more pieces but no valid distribution!" )
        case _ => foldResults( previous_value, valid_distribution )
      }
    }

}

