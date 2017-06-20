import javax.swing.{JFrame, JPanel, JTextField, JButton, JLabel}


//-----------  Observer Pattern Interfaces ----------- //

// Observer interface 
trait Observer {
  def update(): Unit
}

// Subject or Observable interface 
trait Observable {

  private var observers: Set[Observer] = Set()

  // Subscribe observer to subject updates 
  def attach(obs: Observer) {
    observers += obs 
  }

  // Remove/ unsubscribe observer from subject updates 
  def detach(obs: Observer) {
    observers -= obs 
  }

  // Notify all observers 
  def notifyObservers() {
    for (obs <- observers) obs.update() 
  }
}


//----------- Helpers to subscribe to events ------------- //

/// Register callback function
///
def onClick(button: JButton) (handler: () => Unit) = {
  button.addActionListener(
    new java.awt.event.ActionListener(){
      def actionPerformed(evt: java.awt.event.ActionEvent) = {
        handler()
      }
    }
  )
}


def onWindowExit(frame: javax.swing.JFrame) (handler: () => Unit) = {
  frame.addWindowListener(
    new java.awt.event.WindowAdapter(){
      override def windowClosing(evt: java.awt.event.WindowEvent) = {
        handler()
      }
  })
}

def showFrameSize(frame: javax.swing.JFrame){
  println(frame.getSize())
}


//--------------- MVC Counter Demo -------------------- // 


class CounterModel(init: Int) extends Observable{
  private var counter = init

  def getValue() = counter

  def increment() = {
    counter = counter + 1   // Every time the model state is changed,
                            // the observers must be notified.

    this.notifyObservers()  // the 'this' prefix is optional.
  }

  def decrement() = {
    counter = counter - 1
    this.notifyObservers()  
  } 
}

/// This view doesn't need a controller as this doesn't need an input.
class ConsoleView(counter: CounterModel) extends Observer{
  init()

  def init(){
    // Register observer (this class)
    counter.attach(this)
  }

  // updates the view 
  def update() {
    println("Counter value is = " + counter.getValue())
  }
}


class GuiView1(counter: CounterModel) extends Observer{
  private val frame   = new JFrame("Counter MVC App")
  private val panel   = new JPanel(new java.awt.FlowLayout())
  private val label   = new JLabel("Counter")
  private val display = new JTextField(10)
  private val btnInc  = new JButton("Increment")
  private val btnDec  = new JButton("Decrement")
  private val btnExit = new JButton("Exit")

  init()

  def init(){
    // Register observer (this class)
    counter.attach(this)

    panel.add(label)
    panel.add(display)
    panel.add(btnInc)
    panel.add(btnDec)
    panel.add(btnExit)

    display.setEditable(false)
    frame.add(panel)
    frame.setSize(600, 100)
    frame.show()

    // Exit application if user closes window.
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    this.update()
  }

  // updates the view 
  def update() {
    display.setText(counter.getValue().toString())
  }

  def onIncrement(handler: () => Unit) {
    onClick(btnInc)(handler)
  }

  def onDecrement(handler: () => Unit) {
    onClick(btnDec)(handler)
  }

  def onExit(handler: () => Unit) = {
    onClick(btnExit)(handler)
  }

  def show(){
    frame.show()
  }
}

class GuiView1Controller(viewp: GuiView1, modelp: CounterModel){
  private var view  = viewp
  private var model = modelp
  init()

  def init(){
    
    //---> Model Manipulation 
    view.onIncrement(this.increment)
    view.onDecrement(this.decrement)

    //---> GUI manipulation 
    view.onExit(() => System.exit(0))
  }

  def increment(){
    model.increment()
  }

  def decrement(){
    model.decrement()
  }

}

// View without input that only displays the model.
// This view doesn't need a controller as it doesn't
// have any user input.
//
class GuiView2(counter: CounterModel) extends Observer{
  private val frame   = new JFrame("Counter App - Display only view")
  private val panel   = new JPanel()
  private val display = new JLabel()

  init()

  def init(){
    counter.attach(this)

    panel.add(display)
    frame.setSize(354, 54)
    frame.add(panel)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.show()
    update()
  }

  // Method required by Observer interface 
  def update(){
    display.setText(counter.getValue().toString())
    // showFrameSize(frame)
  }
}


val counterModel = new CounterModel(0)
val consoleView  = new ConsoleView(counterModel)

val guiView1           = new GuiView1(counterModel)
val guiView1Controller = new GuiView1Controller(guiView1, counterModel)

val guiView2           = new GuiView2(counterModel)
