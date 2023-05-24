import scala.util.Random
import scala.collection._
import scala.collection.parallel.CollectionConverters._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

package object Matrices {
  val random = new Random ()

  type Matriz = Vector[Vector[Int]]
  
  def matrizAlAzar(long:Int, vals:Int): Matriz = {
    val v = Vector.fill(long,long){random.nextInt(vals)}
    v
  }

  def transpuesta(m:Matriz):Matriz = {
    val l=m.length
    Vector.tabulate(l,l)((i,j)=>m(j)(i))
  }

  def prodPunto(v1:Vector[Int],v2:Vector[Int]):Int ={
    (v1 zip v2).map({case(i,j)=>(i*j)}).sum
  }

  // Ejercicio 1.1.1
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val m2Transpuesta = transpuesta(m2)
    val n = m1.length
    val result = Vector.tabulate(n, n) { (i, j) =>
      prodPunto(m1(i), m2Transpuesta(j))
    }
    result
  }
  // Ejercicio 1.1.2
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val m2Transpuesta = transpuesta(m2)
    val n = m1.length
    val result = Vector.tabulate(n, n) { (i, j) =>
      val fila = m1(i)
      val columna = m2Transpuesta(j)
      val par = fila.zip(columna).par // Convierte a paralelo
      par.map { case (x, y) => x * y }.sum
    }
    result
  }
  // Ejercicio 1.2.1
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
    Vector.tabulate(l, l) { (row, col) =>
      m(i + row)(j + col)
    }
  }
  // Ejercicio 1.2.2
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    Vector.tabulate(n, n) { (i, j) =>
      m1(i)(j) + m2(i)(j)
    }
  }
  // Ejercicio 1.2.3
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      val result1 = (c11, c12).zipped.map(_ ++ _)
      val result2 = (c21, c22).zipped.map(_ ++ _)

      result1 ++ result2
    }
  }
  // Ejercicio 1.2.4
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.length
    val threshold = 128 // Umbral para no paralelizar

    if (n <= threshold) {
      multMatrizRec(m1, m2) // Utilizar la implementación secuencial para matrices pequeñas
    } else {
      val l = n / 2
      val a11 = subMatriz(m1, 0, 0, l)
      val a12 = subMatriz(m1, 0, l, l)
      val a21 = subMatriz(m1, l, 0, l)
      val a22 = subMatriz(m1, l, l, l)
      val b11 = subMatriz(m2, 0, 0, l)
      val b12 = subMatriz(m2, 0, l, l)
      val b21 = subMatriz(m2, l, 0, l)
      val b22 = subMatriz(m2, l, l, l)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21)).par
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22)).par
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21)).par
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22)).par

      val result1 = (c11, c12).zipped.map(_ ++ _)
      val result2 = (c21, c22).zipped.map(_ ++ _)

      result1 ++ result2
    }
  }


  /*
  // Ejercicio 1.3.1
  def restaMatriz(m1:Matriz, m2:Matriz): Matriz = {
  . . . }
  // Ejercicio 1.3.2
  def multStrassen(m1:Matriz, m2:Matriz): Matriz = {
  . . . }
  // Ejercicio 1.3.3
  def multStrassenPar(m1:Matriz, m2:Matriz): Matriz = {
  . . . }
  */
  }