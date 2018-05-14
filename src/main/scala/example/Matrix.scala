package example
/*
import scala.reflect.ClassTag
import scala.scalajs.js
import MatrixRange._
import Matrix._

trait MatrixRange {
  def content(maxSize: Int): Iterable[Int]
}

object MatrixRange {
  implicit def intToMatrixRange(x: Int): MatrixRange = new MatrixRange {
    override def content(maxSize: Int): Iterable[Int] = Array(x)
  }

  implicit def rangeToMatrixRange(x: Range): MatrixRange = new MatrixRange {
    override def content(maxSize: Int): Iterable[Int] = x
  }
}

// A star, indicating a range containing "all" elements
object * extends MatrixRange {
  override def content(maxSize: Int): Iterable[Int] = (0 until maxSize).map(println(_))
}


trait VectorLike[T] extends IndexedSeq[T] {
  def apply(entry: Int): T
  def apply(entries: MatrixRange): VectorLike[T]
  def update(entry: Int, value: T): Unit
  def update(entries: MatrixRange, values: VectorLike[T]): Unit
}

trait MatrixLike[T] {
  def apply(entry: Int): VectorLike[T]
  def apply(entries: MatrixRange): MatrixLike[T]
  def apply(entry1: Int, entry2: Int): T
  def apply(entry1: Int, entries2: MatrixRange): VectorLike[T]
  def apply(entries1: MatrixRange, entry2: Int): VectorLike[T]
  def apply(entries1: MatrixRange, entries2: MatrixRange): MatrixLike[T]

  def update(entry: Int, value: VectorLike[T]): Unit
  def update(entries: MatrixRange, value: MatrixLike[T]): Unit
  def update(entry1: Int, entry2: Int, value: T): Unit
  def update(entry1: Int, entries2: MatrixRange, value: VectorLike[T]): Unit
  def update(entries1: MatrixRange, entry2: Int, value: VectorLike[T]): Unit
  def update(entries1: MatrixRange, entries2: MatrixRange, value: MatrixLike[T]): Unit

  def size: (Int, Int)
  def data: Array[Array[T]]

  override def toString: String = {
    data.map(x => x.map(_.toString).mkString(" ")).mkString("\n")
  }
}

class MatrixCustomVector[T](matrix: Matrix[T], pos: Array[(Int, Int)]) extends VectorLike[T] {
  override def apply(entry: Int): T = matrix(pos(entry)._1, pos(entry)._2)
  override def apply(entries: MatrixRange): VectorLike[T] = {
    new MatrixCustomVector[T](matrix, entries.content(pos.length).map(pos.apply).toArray)
  }
  override def update(entry: Int, value: T): Unit = matrix(pos(entry)._1, pos(entry)._2) = value
  override def update(entries: MatrixRange, values: VectorLike[T]): Unit = {
    for(((x, y), v) <- entries.content(pos.length).map(pos.apply).zip(values)) {
      matrix(x, y) = v
    }
  }
  def length: Int = pos.length
}

class MatrixRow[T](matrix: Matrix[T], row: Int) extends VectorLike[T] {
  override def apply(entry: Int): T = matrix(row, entry)
  override def apply(entries: MatrixRange): VectorLike[T] = {
    new MatrixCustomVector[T](matrix, entries.content(matrix.size._2).map((row, _)).toArray)
  }
  override def update(entry: Int, value: T): Unit = matrix(row, entry) = value
  override def update(entries: MatrixRange, values: VectorLike[T]): Unit = {
    for((y, v) <- entries.content(matrix.size._2).zip(values)) {
      matrix(row, y) = v
    }
  }
  def length: Int = matrix.size._2
}

class MatrixCol[T](matrix: Matrix[T], col: Int) extends VectorLike[T] {
  override def apply(entry: Int): T = matrix(entry, col)
  override def apply(entries: MatrixRange): VectorLike[T] = {
    new MatrixCustomVector[T](matrix, entries.content(matrix.size._1).map((_, col)).toArray)
  }
  override def update(entry: Int, value: T): Unit = matrix(entry, col) = value
  override def update(entries: MatrixRange, values: VectorLike[T]): Unit = {
    for((x, v) <- entries.content(matrix.size._1).zip(values)) {
      matrix(x, col) = v
    }
  }
  def length: Int = matrix.size._1
}

class MatrixView[T:ClassTag](matrix: Matrix[T], pos: Array[Array[(Int, Int)]]) extends MatrixLike[T] {
  override def apply(entry: Int): VectorLike[T] = ???
  override def apply(entries: MatrixRange): MatrixLike[T] = ???
  override def apply(entry1: Int, entry2: Int): T = ???
  override def apply(entry1: Int, entries2: MatrixRange): VectorLike[T] = ???
  override def apply(entries1: MatrixRange, entry2: Int): VectorLike[T] = ???
  override def apply(entries1: MatrixRange, entries2: MatrixRange): MatrixLike[T] = ???
  override def update(entry: Int, value: VectorLike[T]): Unit = ???
  override def update(entries: MatrixRange, value: MatrixLike[T]): Unit = ???
  override def update(entry1: Int, entry2: Int, value: T): Unit = ???
  override def update(entry1: Int, entries2: MatrixRange, value: VectorLike[T]): Unit = ???
  override def update(entries1: MatrixRange, entry2: Int, value: VectorLike[T]): Unit = ???
  override def update(entries1: MatrixRange, entries2: MatrixRange, value: MatrixLike[T]): Unit = ???
  override def size: (Int, Int) = ???

  override def data: Array[Array[T]] = pos.map(x => x.map{case (x,y) => matrix.data(x)(y)})
}

class Matrix[T:ClassTag](val data: Array[Array[T]]) extends MatrixLike[T] {
  override def apply(entry: Int): VectorLike[T] = new MatrixRow[T](this, entry)
  override def apply(entries: MatrixRange): MatrixLike[T] = ???
  override def apply(entry1: Int, entry2: Int): T = data(entry1)(entry2)
  override def apply(entry1: Int, entries2: MatrixRange): VectorLike[T] = ???
  override def apply(entries1: MatrixRange, entry2: Int): VectorLike[T] = ???
  override def apply(entries1: MatrixRange, entries2: MatrixRange): MatrixLike[T] = {
    new MatrixView[T](this, entries1.content(size._1).map(i => entries2.content(size._2).map(j => (i,j)).toArray).toArray)
  }
  override def update(entry: Int, value: VectorLike[T]): Unit = ???
  override def update(entries: MatrixRange, value: MatrixLike[T]): Unit = ???
  override def update(entry1: Int, entry2: Int, value: T): Unit = data(entry1)(entry2) = value
  override def update(entry1: Int, entries2: MatrixRange, value: VectorLike[T]): Unit = ???
  override def update(entries1: MatrixRange, entry2: Int, value: VectorLike[T]): Unit = ???
  override def update(entries1: MatrixRange, entries2: MatrixRange, value: MatrixLike[T]): Unit = {
    for((i, x) <- entries1.content(size._1).zipWithIndex; (j, y) <- entries2.content(size._2).zipWithIndex) {
      data(i)(j) = value(x, y)
    }
  }
  override def size: (Int, Int) = (data.length, if(data.length > 0) data(0).length else 0)
}

object Matrix {
  implicit def iterableToRowMatrix[T:ClassTag](x: Iterable[T]): Matrix[T] = {
    new Matrix[T](Array(x.toArray))
  }
  implicit def iterableToRowMatrix[T:ClassTag](x: Array[T]): Matrix[T] = {
    new Matrix[T](Array(x))
  }
  implicit def iterableToRowMatrix[T:ClassTag](x: Array[Array[T]]): Matrix[T] = {
    new Matrix[T](x)
  }
}

object MyExample extends App {

  val matrix = new Matrix(Array(
    Array(1, 2, 0, 3),
    Array(1, 0, 1, 3),
    Array(1, 0, 2, 3),
    Array(3, 2, 1, 0)
  ))

  println(" -- selection *, 1 to 2 -- ")
  println(matrix(*, 1 to 2))
  println()

  println(" -- selection *, * -- ")
  println(matrix(*, *))
  println()

  println(" -- Assign 0 to first col -- ")
  matrix(*, 0) = Array(Array(0, 0, 0, 0)).transpose
  println(matrix)
  println()

  println(" -- Get third row -- ")
  val thirdRow = matrix(2)
  println(thirdRow)
  println()

  println(" -- Update third row -- ")
  thirdRow(2) = 1000
  println(thirdRow)
  println()

  println(" -- Matrix has changed -- ")
  println(matrix)
  println()
}
*/