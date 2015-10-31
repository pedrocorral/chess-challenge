package chess.challenge

/**
 * Base trait for Kings, Queens, Knights, etc...
 */
trait Piece
case object King extends Piece
case object Queen extends Piece
case object Bishop extends Piece
case object Rook extends Piece
case object Knight extends Piece
