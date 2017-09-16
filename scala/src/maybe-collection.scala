
// Sealed trait means that is not possible to add more
// implementation of this trait/interface out of this file.
// 
sealed trait Maybe[+A]{
  def get(): A

  // Haskell's forM_ - Apply a function that returns nothing to a
  // collection. 
  //
  def foreach(a: A => Unit): Unit

  // Haskell's fmap
  //
  def map[B](fn: A => B): Maybe[B]

  // Haskell's (>>=) bind 
  // 
  def flatMap[B](fn: A => Maybe[B]): Maybe[B]

  // Haskell's guard 
  // 
  def withFilter(fn: A => Boolean): Maybe[A]

  def filter(fn: A => Boolean) = withFilter(fn)

}


// Equivalent to Haskell's Nothing or Scala's None.
//
//
case object Empty extends Maybe[Nothing]{
  def get() = throw new RuntimeException("Error: Attemp to get data from Empty value")

  def foreach(fn: Nothing => Unit) = ()

  def map[B](fn: Nothing => B) = Empty

  def flatMap[B](fn: Nothing => Maybe[B]) = Empty

  def withFilter(fn: Nothing => Boolean) = Empty

}

// Similar to Haskell's Just or Scala's Some. 
//
//
case class  Just[+A](value: A) extends Maybe[A]{
  def get() = value

  def foreach(fn: A => Unit) = fn(value)

  def map[B](fn: A => B) = Just(fn(value))

  def flatMap[B](fn: A => Maybe[B]) = fn(value)

  def withFilter(fn: A => Boolean): Maybe[A] = {
    if(fn(value))
      Just(value)
    else
      Empty
  }
}

// Companion object
//
object Maybe{
  def apply[A](value: A) = {
    if (value != null)
      Just(value)
    else
      Empty
  }

  def makeMaybeFn[A, B](fn: A => B) = (x: A) => {
    apply(fn(x))
  }

  def mapM2[A, B, C](fn: (A, B) => C, ma: Maybe[A], mb: Maybe[B]) = {
    for {
      a <- ma
      b <- mb 
    } yield fn(a, b)
  }

}


def addSafe(sa: Maybe[Int], sb: Maybe[Int]) = {
  for {
    a <- sa 
    b <- sb 
  } yield a + b 
}

def readNumber(prompt: String) = {
  try  {
    print(prompt)
    val sc = new java.util.Scanner(System.in)
    val n  = Just(sc.nextInt)
    print("\n")
    n
  } catch {
    case ex: java.util.InputMismatchException
        => Empty
  }
}

def parseInt(str: String): Maybe[Int] =  {
  try Just(str.toInt)
  catch {
    case ex: java.lang.NumberFormatException
        => Empty
  }
}

