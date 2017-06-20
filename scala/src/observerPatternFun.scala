
// Observer object is replace by a callback function 
type Observer[A] = A => Unit

// Record containing functions 
case class Subject[A](
   attach: Observer[A] => Unit
  ,detach: Observer[A] => Unit
  ,notifyObservers: A  => Unit
  ,getState: () => A
)

def createSubject[A](stateInit: A) = {
  var observers : Set[A => Unit] = Set()  
  var state = stateInit

  val attach = (obs: Observer[A]) => {
    observers += obs 
  }

  val detach = (obs: Observer[A]) => {
    observers -= obs 
  }

  val notifyObservers = (a: A) => {
    state = a 
    for (obs <- observers) obs(a)
  }

  val getState = () => state

  Subject(attach, detach, notifyObservers, getState)
}


// The observer becomes just a function or callback!!
def consoleCelsiusObserver(temp: Double) = {
  printf("Current temperature is %.3f in °C\n", temp)
}

def consoleKelvinObserver(temp: Double) = {
    printf("Current temperature is %.3f in °K\n", temp + 273.0)
}

def consoleFahrenheitObserver(temp: Double) = {
  val tempF = 5.0 / 9.0 * temp + 32.0
  printf("Current temperature is %.3f in F\n", tempF)
}

// Creates a function that updates the GUI display
// 
// Note: (Double => Unit) is optional.
// The type annotation was added to make reading easier.
//
def makeGuiObserver(): (Double => Unit) = {
  val frame = new javax.swing.JFrame()
  val display = new javax.swing.JLabel()
  frame.setSize(255, 71)
  frame.add(display)
  frame.show()
  frame.setTitle("Temperature View")
  (temp: Double) =>  display.setText("Temperature = %.3f C".format(temp))
}

val sensor = createSubject[Double](20.0)
sensor.attach(consoleCelsiusObserver)
sensor.attach(consoleKelvinObserver)
sensor.attach(consoleFahrenheitObserver)

val guiObserver = makeGuiObserver()
sensor.attach(guiObserver)
print("\nob_scala_eol")
