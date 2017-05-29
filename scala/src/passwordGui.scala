
import javax.swing.{JFrame, JLabel, JButton, JPanel, JPasswordField}

def getPassword(passwd: javax.swing.JPasswordField) =
  new String(passwd.getPassword())

/// Register callback function
///
def onClick(button: JButton) (handler: => Unit) = {
  button.addActionListener(
    new java.awt.event.ActionListener(){
      def actionPerformed(evt: java.awt.event.ActionEvent) = {
        handler
      }
    }
  )
}


def onWindowExit(frame: javax.swing.JFrame) (handler: => Unit) = {
  frame.addWindowListener(
    new java.awt.event.WindowAdapter(){
      override def windowClosing(evt: java.awt.event.WindowEvent) = {
        handler
      }
  })
}

val frame = new JFrame("Scala password entry")
frame.setSize(400, 200)
frame.setLayout(new java.awt.GridLayout(2, 1))

// frame.setLayout()

val panel  = new JPanel(new java.awt.FlowLayout())
val label  = new JLabel("Password")
val passwd = new JPasswordField(10)
val btn    = new JButton("Login")
val status = new JLabel("Safe closed")
passwd.setEchoChar('*')


panel.add(label)
panel.add(passwd)
panel.add(btn)


frame.add(panel)
frame.add(status)

frame.setVisible(true)

//--------- Event Handling ----------

def checkPassword(
  passwd: String,
  input: String,
  okHanlder: () => Unit,
  errHandler: () => Unit ) = {

  if (input == passwd)
    okHanlder()
  else
    errHandler()
}

onClick(btn){ println("I was clicked")}

onClick(btn) {
  val pass = getPassword(passwd)
  if (pass == "thepassword")
    println("Safe opened")
  else
    println("Error: Wrong password")
}

onClick(btn){
  checkPassword(
    "thepassword"
   ,getPassword(passwd)
   ,() => status.setText("Safe opened. Ok")
   ,() => status.setText("Error: Wrong password")
  )}

onWindowExit(frame){ System.exit(0) }

print("\nob_scala_eol")
