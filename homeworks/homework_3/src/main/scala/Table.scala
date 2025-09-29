import scala.collection.mutable

class Table(width: Int, height: Int) {
  require(width > 0 && height > 0, "Width and height must be positive")

  private val cells: mutable.ArrayBuffer[Cell] =
    mutable.ArrayBuffer.fill(width * height)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      Some(cells(ix + iy * width))
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
      cells(ix + iy * width) = cell
    } else {
      throw new IndexOutOfBoundsException(s"Invalid coordinates: ($ix, $iy)")
    }
  }
}