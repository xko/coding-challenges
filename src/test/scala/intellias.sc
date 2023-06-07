
sealed trait Vielleicht[+T] {
    def flatMap[A](f: T => Vielleicht[A]):Vielleicht[A] = this match {
        case Etwa(v) => f(v)
        case Nichts => Nichts
    }
    def map[A](f: T => A):Vielleicht[A] = this match {
        case Etwa(v) => Etwa(f(v))
        case Nichts => Nichts
    }
}
case class Etwa[+T](v:T) extends Vielleicht[T]
case object Nichts extends Vielleicht[Nothing]

val a =Nichts
val b= Etwa("one")
for(aa<-b; bb<-a) yield (aa,bb)


import scala.annotation.tailrec
def reverse(xs: List[Int]): List[Int] = {
    @tailrec
    def tailrev(revSoFar:List[Int], remainOrig: List[Int]):List[Int] = remainOrig match {
        case Nil => revSoFar
        case h :: tail => tailrev(h :: revSoFar, tail )
    }

    tailrev(Nil, xs)
}

reverse(1::2::3::Nil)

trait Duck {
    def flapWings = "\\\\o//"
}
trait Plane {
    def runEngines = "<<==З"
}
trait Flyer[T] {
    def fly(fl:T):String
}



implicit val duckFlyer: Flyer[Duck] = new Flyer[Duck] {
    override def fly(d: Duck): String = d.flapWings
}

implicit val planeFlyer: Flyer[Plane] = new  Flyer[Plane] {
    override def fly(p: Plane): String = p.runEngines
}

def launch[T : Flyer](t:T): Unit = {
    println(implicitly[Flyer[T]].fly(t))
}

launch(new Duck {} )
launch(new Plane {} )

implicit class SelfLauncher[T : Flyer](fl: T) {
    def launch():Unit = println(implicitly[Flyer[T]].fly(fl))
}
new Duck {}.launch()