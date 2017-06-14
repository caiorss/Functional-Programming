
// Observer interface 
trait Observer[A] {
  def update(a: A)
}

// Subject interface 
trait Subject[A] {
  // Subscribe observer to subject updates 
  def attach(obs: Observer[A]): Unit

  // Remove/ unsubscribe observer from subject updates 
  def detach(obs: Observer[A]): Unit

  // Notify all observers 
  def notifyObservers(a: A): Unit 
}

// Concrete: Subject
//----------------------------------
//
// Publishes temperature measurements. 
class TempSensor extends Subject[Double]{
  private var temp: Double = 0.0
  private var observers: Set[Observer[Double]] = Set()

  def attach(obs: Observer[Double]) =
    observers += obs

  def detach(obs: Observer[Double]) =
    observers -= obs 

  def notifyObservers(tempNew: Double) = { 
    temp = tempNew
    observers.foreach(_.update(tempNew))
    // for (obs <- observers) obs.update(tempNew)
  }

  def getTemp() = temp

}


// ---------- Concrete Observers ----------------- // 

class ConsoleCelsiusObserver extends Observer[Double]{
  def update(temp: Double) = {
    printf("Current temperature is %.3f in °C\n", temp)
  }
} // End of class ConsoleCelsiusObserver

class ConsoleKelvinObserver extends Observer[Double]{  
  def update(temp: Double) = {
    printf("Current temperature is %.3f in °K\n", temp + 273.0)
  }
} // End of class ConsoleKelvinObserver

class ConsoleFahrenheitObserver(subject: Subject[Double]) extends Observer[Double]{

  init()

  def init() {
    subject.attach(this)
  }

  def update(temp: Double) = {
    val tempF = 5.0 / 9.0 * temp + 32.0 
    printf("Current temperature is %.3f in F\n", tempF)
  }
} // End of class ConsoleFahrenheitObserver


/// Java Swing Observer 
class GuiCelsiusObserver(subject: Subject[Double]) extends Observer[Double] {
  private var frame   = new javax.swing.JFrame()
  private var display = new javax.swing.JLabel()

  init()

  def init(){
    frame.setSize(255, 71)
    frame.add(display)
    frame.show()
    frame.setTitle("Temperature View")
    subject.attach(this)
  }

  def update(temp: Double) = {
    display.setText("Temperature = %.3f C".format(temp))
  }
}

/// ----------- Classes Instantiation ---------- ///

val sensor = new TempSensor()

val consoleC = new ConsoleCelsiusObserver()
sensor.attach(consoleC)

val consoleK = new ConsoleKelvinObserver()
sensor.attach(consoleK)

val consoleF = new ConsoleFahrenheitObserver(sensor)

val guiObs = new GuiCelsiusObserver(sensor)

print("\nob_scala_eol")
