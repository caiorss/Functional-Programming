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


//--------------- MVP Counter Demo -------------------- //



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



class CounterView extends {
  private val frame   = new JFrame("Counter MVC App")
  private val panel   = new JPanel(new java.awt.FlowLayout())
  private val label   = new JLabel("Counter")
  private val display = new JTextField(10)
  private val btnInc  = new JButton("Increment")
  private val btnDec  = new JButton("Decrement")
  private val btnExit = new JButton("Exit")

  init()

  def init(){
    panel.add(label)
    panel.add(display)
    panel.add(btnInc)
    panel.add(btnDec)
    panel.add(btnExit)

    display.setEditable(false)
    frame.add(panel)
    frame.setSize(600, 100)
    // Exit application if user closes window.
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)   
  }

  // updates the view 
  def setDisplay(value: String) {
    display.setText(value)
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



class CounterPresenter(model: CounterModel) extends Observer{
  private val view  = new CounterView()

  init()

  def init(){

    // Subscribe presenter to model updates 
    model.attach(this)

    //---> Model Manipulation 
    view.onIncrement(this.increment)
    view.onDecrement(this.decrement)

    //---> GUI manipulation 
    view.onExit(() => System.exit(0))

    // Initial model display
    this.update()
  }

  def increment(){
    model.increment()
  }

  def decrement(){
    model.decrement()
  }

  // Method required by Observer interface. It updates the view
  // when the model notifies the presenter that its state has
  // changed.
  def update(){
    view.setDisplay(model.getValue().toString())
  }

  def show() = view.show()

}


val counterModel = new CounterModel(0)
val presenter    = new CounterPresenter(counterModel)
presenter.show()
