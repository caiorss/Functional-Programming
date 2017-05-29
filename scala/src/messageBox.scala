
def messageBox (title: String, content: String) {
  javax.swing.JOptionPane.showMessageDialog (
    null,
    content,
    title,
    javax.swing.JOptionPane.PLAIN_MESSAGE
  )
 }

messageBox("Information", "Download of file animation.jar completed")
print("\nob_scala_eol")
