package values

class Notification(val message: String) extends Value {
  override def toString = message
}

object Notification {
  def apply(msg: String) = new Notification(msg)
  val DONE = Notification("DONE")
  val OK = Notification("OK")
  val UNDEFINED = Notification("UNDEFINED")
  val UNKNOWN = Notification("UNKNOWN")
}