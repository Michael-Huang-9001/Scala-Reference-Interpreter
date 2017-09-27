package expressions
import ui._
import values._

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    var boole = condition.execute(env)
    if(!boole.isInstanceOf[Boole])
      throw new TypeException("the condition is not a boole")
    var result: Value = Notification.OK
    while(condition.execute(env).asInstanceOf[Boole].value)
      result = body.execute(env)
    result
  }
}