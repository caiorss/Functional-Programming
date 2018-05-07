
class SFile(path: String) extends java.io.File(path){
  /** Return new path with extension changed. */
  def changeExt(ext: String) = {
    val name = this.getName.split("\\.")(0) + "." + ext
    new SFile(new java.io.File(this.getParent, name).getPath())
  }
  /** Return new path with parent directory changed. */
  def changeParent(parent: String) = {
    new SFile(new java.io.File(parent, this.getName()).getPath())
  }
  def changeParentAndExt(parent: String, ext: String) = {
    val name = this.getName.split("\\.")(0) + "." + ext
    new SFile(new java.io.File(parent, name).getPath())
  }  
}

object AnsiTerm{

  /** Check whether terminal is Ansi */
  val isAnsi = { 
    val term = System.getenv("TERM")
    term != null && ( term.contains("xterm") || term.contains("ansi"))
  }

  val colors = Map(
    "Red"           -> "\u001B[31m",
    "Blue"          -> "\u001B[34m",
    "Cyan"          -> "\u001B[36m",
    "Yellow"        -> "\u001b[33m",
    "White"         -> "\u001b[37m",
    "Green"         -> "\u001b[32m",
    "Magenta"       -> "\u001b[35m",
    "DarkRed"       -> "\u001B[31m\u001B[1m",
    "BrightGreen"   -> "\u001b[32;1m",
    "BrigthYellow"  -> "\u001b[33;1m",
    "BrightMagenta" -> "\u001b[35;1m",
    "BrightCyan"    -> "\u001b[36;1m",
    "BrightWhite"   -> "\u001b[37;1m",
    "Reset"         -> "\u001b[0m"
  )

  def withColor(color: String)(action: => Unit) = 
    try {
      if(isAnsi) print(color)
      action
    } finally{
      if(isAnsi) print("\u001B[0m")
    }

  def addColor(color: String, text: String) = 
    if(isAnsi)
      color + text + "\u001B[0m"
    else
      text

  def println(text: String, color: String = null) = {
    if(color != null && isAnsi)
      System.out.println(color + text + "\u001B[0m")
    else
      System.out.println(text)
  }

  def progressIndicatorPercent(delayMs: Int) = 
    for (i <- 0 to 100) {
      Thread.sleep(delayMs)
      print("\u001b[1000D" + i + "%")
    }


  def testColors() = {
    if(!isAnsi)
      println("Warning: This terminal doesn't support ANSI colors")
    else
      for((k, v) <- colors){
        print(v)
        println(k)
        print("\u001B[0m")
      }
  }
} // ---- End of object AnsiTerm ----- //

object FileUtils{
  import java.io.File
  import java.nio.file.{Files, Paths, Path}
  import java.util.stream.{Stream => JStream}

  def file(path: String) =
    new java.io.File(path)

  def hasExtension(ext: String) =
    (file: File) => file.getPath.endsWith(ext)

  def fileInDirectory(dir: File) = 
    (file: File) => file.getPath().startsWith(dir.getPath())

  def walkDirectory(path: String, pred: Path => Boolean)(action: Path => Unit) =
    Files.walk(Paths.get(path))
      .filter(p => pred(p))
      .forEach(p => action(p))

  // def fileInDirs(file: File) =
  //   excludeDirs.exists(dir => file.getPath().startsWith(dir.getPath))

  /** Find files matching a predicate function in a directory and all its sub directories */
  def findFiles(path: String, pred: File => Boolean) : List[File] = {        
    val root = Paths.get(path)
    Files.walk(root)
      .toArray()    
      .toList.asInstanceOf[List[Path]]
      .map(_.toFile)
      .filter{f => pred(f) }
  }

}

import java.io.File
import FileUtils.hasExtension
val F = FileUtils
val A = AnsiTerm
val logColor     = A.colors("Blue")
val logColorOK   = A.colors("Green")
val logColorFail = A.colors("Red")

val emacsConfig = System.getProperty("user.home") + "/.emacs.d/init.el"

/** Execute sub-process with a given set of arguments and waits for its end. */
def execWait(program: String, args: List[String] = List()) = {
  import java.lang.ProcessBuilder
  val pb = new ProcessBuilder(program)
  args foreach pb.command.add
  pb.inheritIO().start().waitFor()
}


/** Compile org-mode file (*.org) generating .html file and returning 0 if successful. */
def compileOrgToHtml(file: java.io.File): Int = {
  // emacs -l ~/.emacs.d/init.el --visit clojure/README.org --batch -f org-html-export-to-html --kill
  val args = List(
    "-l", emacsConfig,
    "--visit", file.getPath,
    "--batch",
    "-f", "org-html-export-to-html",
    "--kill"
  )
  execWait("emacs", args)
}

/** Build target file if input file has changed or the target does not exist.
  * 
  * @param fileIn:  Input file needed for compilation
  * @param fileOut: Output file that will build
  * @param builder: Function that generates output file returning the process status code.
 */
def buildTarget(
  fileIn:  java.io.File,
  fileOut: java.io.File
 )(builder: java.io.File => Int) =
  if(!fileOut.isFile() || fileIn.lastModified > fileOut.lastModified){
    A.println(s"Building file: <$fileIn> waiting ...", logColor)
    val status = builder(fileIn) 
    // Check process status code
    if(status == 0) 
      A.println(s"SUCCESSFUL: file <$fileIn> compiled to <$fileOut. OK.", logColorOK)
    else
      A.println(s"ERROR: compilation of file <$fileIn> failed.", logColorFail)
  } else {
    A.println(s"SUCCESSFUL: File <$fileIn> already compiled to <$fileOut>. OK", logColorOK)
  }

def cleanHtmlFiles() =
  F.findFiles("dist", F.hasExtension("html")) foreach { file =>
    println("Removing file = " + file)
    file.delete()
  }


def compileFile(file: java.io.File, destDirectory: String) = {
  val input = new SFile(file.getPath())
  val out1 = input.changeExt("html")
  val out2: File = if(input.getName() != "README.org")
    out1.changeParent(destDirectory)
  else
    new java.io.File(destDirectory, "index.html")
  A.println("Target = " + out2, logColor)
  buildTarget(input, out2){ file =>
    // Build file out1 (.html)
    val status = compileOrgToHtml(file)
    if(status == 0){// Rename out1 to out2
      val flag = out1.renameTo(out2)
      if(flag)
        A.println(s"Moved file <$out1> to <$out2>", logColor)
    }
    status
  }
}

def compileDirectory(directory: String, destDirectory: String) = {
  val orgFiles = F.findFiles(directory, F.hasExtension(".org"))  
  orgFiles foreach { file => compileFile(file, destDirectory) }
}


compileDirectory("haskell", "dist/haskell")
compileDirectory("clojure", "dist/clojure")
compileDirectory("ocaml",   "dist/ocaml")
compileDirectory("scheme",  "dist/scheme")
compileDirectory("papers",  "dist/papers")
compileDirectory("scala",   "dist/scala")









