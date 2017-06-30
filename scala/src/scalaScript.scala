
import javax.swing.{JFrame, JPanel, JTextArea}

println("Hello world Scala")

val frame = new JFrame("Sample scala script")
frame.setSize(300, 400)
frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE)

val tarea = new JTextArea()
val scroll = new javax.swing.JScrollPane(tarea)
frame.add(scroll)
frame.setVisible(true)

tarea.append("Hello world Scala Script")
tarea.append("\nHello world! (en)")
tarea.append("\nHola mundo!  (es)")
tarea.append("\nOla mundo!   (pt)")


print("\nob_scala_eol")
