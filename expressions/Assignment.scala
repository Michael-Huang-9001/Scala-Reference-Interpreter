package expressions
import ui._
import values._

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (!vbl.execute(env).isInstanceOf[Variable])
      throw new TypeException("the identifier is not a variable")
    vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE
  }
}