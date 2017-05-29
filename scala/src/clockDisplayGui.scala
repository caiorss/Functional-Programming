
def runTimer(interval: Int, taskFn: () => Unit) = {
  val task = new java.util.TimerTask() {
    def run() {
      taskFn()
    }
  }

  val timer = new java.util.Timer()

  // Run the task every 1 second interval (or 1000 milli seconds)
  timer.schedule(task, 1, interval)
  timer
}

def currentTime() = {
  java.time.LocalDateTime.now.toString
}


val frame = new javax.swing.JFrame("Java Clock App")
val label = new javax.swing.JLabel("")
frame.add(label)
frame.setSize(375, 76)
frame.setVisible(true)


runTimer(1000, () => label.setText(currentTime()))

print("\nob_scala_eol")
