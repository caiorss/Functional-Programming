
import javax.swing.{JFrame, JButton, JMenuItem, JMenuBar, JMenu}
import java.awt.event.{ActionListener, ActionEvent}

class Application{
  def saveDocument(file: String, data: String) =
    println(s"Save document to file $file")

  def openFile(file: String) = 
    println(s"Open file = $file")

  def printDocument() =
    println("Printing document ...")

  def exit() = {
    println("Shutdown systems ...")
    System.exit(0)
  }
}

trait ICommand extends ActionListener{
  // Abstract method 
  def execute(): Unit

  // Concrete method 
  def actionPerformed(evt: ActionEvent) =
    execute()
}

class MainGUI extends JFrame{
  private val btSave    = new JButton("Save")
  private val btOpen    = new JButton("Open")
  private val btClose   = new JButton("Close")
  private val menuSave  = new JMenuItem("Save")
  private val menuOpen  = new JMenuItem("Open")
  private val menuClose = new JMenuItem("Close")

  init()
  def init(){
    setLayout(new java.awt.FlowLayout())
    setTitle("FP - Command Design Pattern for GUIs")
    // Can be without this, but explicit is better than implicit!
    // this.settSize(300, 276)
    setSize(300, 276)   
    // Add buttons
    //==============
    this.add(btSave)
    this.add(btOpen)
    this.add(btClose)
    // Add menu bar
    //================
    val menu = new JMenu("Save")
    menu.add(menuOpen)        
    menu.add(menuSave)
    menu.add(menuClose)
    val menuBar = new JMenuBar()
    menuBar.add(menu)
    setJMenuBar(menuBar)
  }

  def setSaveCommand(cmd: ICommand) = {
    btSave.addActionListener(cmd)
    menuSave.addActionListener(cmd)
    this // return this for method chaining
  }

  def setOpenCommand(cmd: ICommand) = { 
    btOpen.addActionListener(cmd)
    menuOpen.addActionListener(cmd)
    this
  }

  def setExitCommand(cmd: ICommand) = { 
    btClose.addActionListener(cmd)
    menuClose.addActionListener(cmd)
    this
  }
} //--- End of class MainGUI ---- // 



// Approach 1: The function generates anonymous 
// classes implementing the interface.
//
def makeCommand(action: => Unit) =
  new ICommand{
    def execute() = action
  }

def makeOpenComand(app: Application, file: String) =
  new ICommand{
    def execute() = app.openFile(file)
  }

val app = new Application()
val gui = new MainGUI()
gui.setVisible(true)
val cmdSave = makeCommand{ app.saveDocument("file1.txt", "some data") }
val cmdOpen = makeOpenComand(app, "/data/fileTest.csv")
val cmdExit = makeCommand{ println("Fake command. DO NOT EXIT during test.")}
gui.setSaveCommand(cmdSave)
gui.setOpenCommand(cmdOpen)
gui.setExitCommand(cmdExit)
