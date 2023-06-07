trait TPSqrt[T,R] {
    def sqrt(v: T):R
}

implicit val TPSqrtOfInt =  new TPSqrt[Int,Double] {
    override def sqrt(v: Int): Double = math.sqrt(v)
}

def tpSqrt[T,R](v: T)(implicit impl: TPSqrt[T,R]) = impl.sqrt(v)

val r1: Double = tpSqrt(2) // works, compiler infers Double

trait ATSqrt[T] {
    type R
    def sqrt(v: T):R
}


implicit object ATSqrtOfInt extends ATSqrt[Int] {
    override type R = Double
    override def sqrt(v: Int): Double = math.sqrt(v)
}

def atSqrt[T:ATSqrt](v: T) = implicitly[ATSqrt[T]].sqrt(v)

val r2:Double = atSqrt(2)// type mismatch; found : ATSqrt[Int] required: Double