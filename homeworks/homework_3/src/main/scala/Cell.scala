trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(text: String) extends Cell {
  override def toString: String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = {
    table.getCell(ix, iy) match {
      case None => "outOfRange"
      case Some(cell) =>
        if (isCyclicReference(Set.empty)) "cyclic"
        else cell.toString
    }
  }

  private def isCyclicReference(visited: Set[ReferenceCell]): Boolean = {
    if (visited.contains(this)) true
    else {
      table.getCell(ix, iy) match {
        case Some(ref: ReferenceCell) => ref.isCyclicReference(visited + this)
        case _ => false
      }
    }
  }
}