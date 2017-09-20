
class IO[A](action: => A){

  import scala.util.{Try, Success, Failure}

  def run() = action

  def map[B](fn: A => B): IO[B] = {
    new IO(fn(action))
  }

  def flatMap[B](fn: A => IO[B]): IO[B] = {
    new IO(fn(action).run())
  }

  // Retry until successful 
  def retry() = {
    def aux(): A = {
      Try{action} match {
        case Success(a)
            => a
        case _
            => aux()          
      }
    }
    new IO(aux())
  }


  def forever() = {
    def aux(){
      this.run()
      aux()
    }
    new IO(aux())
  }

  def doTimes(n: Int) = {
    val io = this 
    new IO(for (i <- 1 to n) this.run())
  }

} // ---- End of class IO ----- // 


object IO{
  def apply[A](action: => A) = new IO(action)

  val readLine   = new IO(scala.io.StdIn.readLine())
  val readDouble = new IO(scala.io.StdIn.readDouble())
  val readInt    = new IO(scala.io.StdIn.readInt())

  def printn[A](a: A) = new IO(Predef.println(a))
  def print[A](a: A)  = new IO(Predef.print(a))


  def prompt(msg: String): IO[String] = for {
    _    <- print(msg)
    line <- readLine   
  } yield line 
}


def promptParse[A](msg: String, fn: String => A): IO[A] = 
  for {
    _    <- IO.print(msg)
    line <- IO.readLine
    _    <- IO.printn("")
  } yield fn(line)


val getNumAndSum: IO[Int] =
  for {
    x <- promptParse("Enter x: ", _.toInt).retry
    y <- promptParse("Enter y: ", _.toInt).retry
  } yield x + y


val main =
  for {
    _ <- IO.printn("\nTesting IO action getNumAndSum")
    x <- getNumAndSum
    _ <- IO.print("The result is = " + x)
  } yield ()


// Uncomment the line below to run the program.
// main.forever().run()


