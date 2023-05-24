import common._
import scala.util.Random
package objectMatrices {
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
    (v1 zip v2).map({case(i,j)=>(i∗j)}).sum
  }
  // Ejercicio 1.1.1
  def multMatriz(m1:Matriz,m2:Matriz):Matriz = {
  . . . }
  // Ejercicio 1.1.2
  def multMatrizPar(m1:Matriz,m2:Matriz):Matriz = {
  . . . }
  // Ejercicio 1.2.1
  def subMatriz(m:Matriz, i:Int, j:Int, l:Int):Matriz = {
  . . .
  }
  // Ejercicio 1.2.2
  def sumMatriz(m1:Matriz, m2:Matriz): Matriz = {
  . . .
  }
  // Ejercicio 1.2.3
  def multMatrizRec(m1:Matriz, m2:Matriz): Matriz = {
  . . . }
  // Ejercicio 1.2.4
  def multMatrizRecPar(m1:Matriz, m2:Matriz): Matriz = {
  . . . }
  // Ejercicio 1.3.1
  def restaMatriz(m1:Matriz, m2:Matriz): Matriz = {
  . . . }
  // Ejercicio 1.3.2
  def multStrassen(m1:Matriz, m2:Matriz): Matriz = {
  . . . }
  // Ejercicio 1.3.3
  def multStrassenPar(m1:Matriz, m2:Matriz): Matriz = {
  . . . }