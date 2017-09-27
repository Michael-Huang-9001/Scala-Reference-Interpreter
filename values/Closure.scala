package values
import ui._
import expressions._

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value], callEnv: Environment = null): Value = {
    if (params.length != args.length)
      throw new TypeException("number of parameters and arguments do not match")
    var tempEnv = new Environment(defEnv)
    tempEnv.put(params, args)
    body.execute(tempEnv)
    /*
    val callframe = if (callEnv == null) new Environment(defEnv) else new Environment(callEnv)
    callframe.put(params, args)
    body.execute(callframe)
    */
  }
}

object Closure {
  def apply(args: List[Value], callEnv: Environment = null) {
    this(args, callEnv)
  }
}