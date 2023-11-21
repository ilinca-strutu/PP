import util.{Pixel, Util}
import util.Util.{GrayscaleImage, getNeighbors, toLinear}

import java.util.GregorianCalendar
import scala.math.{abs, copySign, cos, cosh, signum}

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]):Image = {
    // transformam in Array de 3 Int-uri intr-un Pixel
    def toPixel(inturi:Array[Int]):Pixel ={
      Pixel.apply(inturi(0),inturi(1),inturi(2))
    }

    // separam List[Char]-ul intr-un Array[String] dupa '\n' si eliminam P3-ul de la inceput
    val lines:Array[String] =  image.mkString.split("\n").tail
    // separam Array[String]-ul dupa ' ' si apoi transformal in Array[Array[Int]]
    val ints:Array[Array[Int]] = lines.map(_.split(" ").map(_.toInt))
    // eliminam primele 2 linii din fisier si transformam fiecare linie in Pixel
    val listaPixels:Array[Pixel] = ints.drop(2).map(toPixel)

    // extragem dimensiunile matricei
    val m = ints.head.head
    val n = ints.head.tail.head

    // grupam pixelii pentru a crea matricea
    listaPixels.grouped(m).map(_.toList).toList
  }
  def toStringPPM(image: Image): List[Char] = {
    // transformam din List[List[Pixel]] in List[Pixel] si apoi transformam fiecare pixel intr-un string
    val pixel2 = image.flatten.flatMap(p => List(p.red +" "+ p.green+" "+p.blue+"\n"))

    // extragem dimensiunile matricei
    val n:Int = image.head.size
    val m:Int = image.size

    // aduagam header-ul si concatenam cu string-ul de pixeli
    (("P3\n"+n.toString+" "+m.toString+"\n255\n") :: pixel2).mkString("").toList
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    // daca image2 e goala, intoarcem Nil
    // altfel, adaugam primul element din image2, la finalul iamage1
    image2 match {
      case Nil => image1
      case _ => verticalConcat(image1.appended(image2.head),image2.tail)
    }
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    // cand nu mai avem linii de analizat, intoarcem comb
    // altfel, apelam pt cozile imaginilor 1 si 2 si appenduim concatenarea dintre 2 linii din cele 2 imagini
    def aux(mat1: Image, mat2: Image, comb: Image): Image = {
      if (mat1.isEmpty)
        comb
      else
        aux(mat1.tail, mat2.tail, comb.appended(mat1.head++ mat2.head))
    }

    aux(image1,image2, Nil)
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    // rotatie cu 90 de grade
    // transpunem matricea si facem reverse pe fiecare linie
    def auxRotate(img: Image):Image={
      img.transpose.reverse.map(_.take(image.head.size))
    }

    if(degrees%360 == 0) image
    else if(degrees%270 == 0) auxRotate(auxRotate(auxRotate(image)))
    else if(degrees%180 == 0) auxRotate(auxRotate(image))
    else auxRotate(image)
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  val black:Pixel = Pixel(0, 0, 0)
  val white:Pixel = Pixel(255,255,255)



  def edgeDetection(image: Image, threshold : Double): Image = {
    // transformam fiecare valoare in pixel-ul corespunzator
    def compareThreshold(value: Double): Pixel = {
      if(value <= threshold) black
      else white
    }

    // aplicam functia toGrayscale pe toata imaginea
    def transfortImageToGrayscale(img: Image): GrayscaleImage = {
      img.flatten.map(p => Util.toGrayScale(p)).grouped(img.head.size).toList
    }

    // pasul 2
    val blurConvolution = applyConvolution(transfortImageToGrayscale(image),gaussianBlurKernel)
    // pasul 3
    val Mx = applyConvolution(blurConvolution,Gx)
    val My = applyConvolution(blurConvolution,Gy)
    val flatMx = Mx.flatten
    val flatMy = My.flatten
    // pasul 4 + pasul 5
    // facem zip pe flatMx si flatMy pentru a crea perechile de elemente, facem suma modulelor
    // aplicam functia de threshold, grupam pixelii, creem lista
    (flatMx zip flatMy).map{case(x,y) => abs(x) + abs(y)}.map(p => compareThreshold(p)).grouped(Mx.head.size).toList

  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {
    // transformam cele 2 matrici in liste, facem produsul elementelor, facem suma lor
    def applyKernel(img: GrayscaleImage): Double = {
      val flatImg = img.flatten
      val flatKernel = kernel.flatten
      (flatImg zip flatKernel).map { case (x, y) => x * y }.sum
    }

    // extragem dimensiunile pentru a obtine raza
    val linesKernel:Int = kernel.size
    val radius:Int = linesKernel/2

    // aplicam functia de getNeighbours
    val neighbours:List[List[GrayscaleImage]] = Util.getNeighbors(image, radius)
    val neighboursCols = neighbours.head.size

    // aplicam algoritmul de convolutie pe lista de matrici
    neighbours.flatten.map(p => applyKernel(p)).grouped(neighboursCols).toList
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    // generam primul rand din matrice
    // punem 5 pe spatiile goale pentru a obtine rezultatul corect in functia de colorare
    def generateFirstRow(n: Int): List[Int] = {
      def addZero(count: Int): List[Int] = {
        count match {
          case 0 => Nil
          case _ => 5 :: addZero(count - 1)
        }
      }

      1 :: addZero(n - 1)
    }

    def computeRow(oldRow: List[Int], i: Int, j: Int): List[Int] = {
      j match {
        // cand e primul element de pe linie adaugam 1
        case 0 => 1 :: computeRow(oldRow, i, j + 1)
        case _ =>
          // cand e egal cu size, intoarce Nil pentru a incheia executia
          if(j == size) Nil

          // cand i si j sunt egele, adica suntem pe diagonala principala, adaugam 1
          else if(i == j ) 1 :: computeRow(oldRow, i, j + 1)

          // cand suntem sub diagonala principala aplicam formula L(i)(j) = (L(i-1)(j-1) + L(i-1)(j))%m
          // tot la acest pas facem si modulul pentru a salva timp
          else if (j < i) ((oldRow(j) + oldRow(j - 1))%m) :: computeRow(oldRow, i, j + 1)
          // cand suntem deasupra diagonalei principale, adaugam 5
          else 5 :: computeRow(oldRow, i, j + 1)
      }
    }

    def computePascalmatrix(count: Int, oldRow: List[Int]): List[Int] = {
      count match {
        // daca e primul rand, generam randul si oldRow devine primul rand
        case 0 => generateFirstRow(size) ++ computePascalmatrix(count + 1, generateFirstRow(size))
        case _ => {
          // cand e egal cu size, intoarce Nil pentru a incheia executia
          if(count == size) Nil
          else {
            // calculam noul rand in functie de oldRow
            // apelam recursiv, noul rand devenind oldRow
            val rez = computeRow(oldRow, count, 0)
            rez ++ computePascalmatrix(count + 1, rez)
          }
        }
      }
    }


    // calculam triunghiul PAscal, aplicam functia de colorare pe fiecare element si apoi le grupam
    computePascalmatrix(0,Nil).map(p => funct(p)).grouped(size).toList
  }
}
