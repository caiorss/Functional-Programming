#!/bin/sh
exec scala -save "$0" "$@"
!#

// Display text in a GUI 
def displayText(text: String) = {
  import javax.swing.{JFrame, JTextArea, JScrollPane}
  val tarea = new JTextArea()
  val frame = new JFrame()
  frame.add(new JScrollPane(tarea))
  frame.setSize(400, 500)
  frame.setVisible(true)
  frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  tarea.setText(text)
}


def readFile(file: String) = {
  val src = scala.io.Source.fromFile(file)
  val txt = src.mkString
  src.close()
  txt 
}

println("Testing Scala script")

println("Arguments passed by user")

args.foldLeft(0){(acc, a) =>
  println(s"arg[${acc}] = ${a}")
  acc + 1
}


displayText(readFile(args(0)))
