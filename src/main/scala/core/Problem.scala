package ninetyNineScalaProblems.core

trait Problem {
  import io.AnsiColor.*
  
  protected val number: Int
  protected val description: String
  protected def exec(): Unit

  protected def logApply(name: String, args: Any*): Unit =
    println(s"$name(${args.mkString(", ")})")

  protected def logResult(result: Any*): Unit =
    println(s"result = ${GREEN}${result.mkString(", ")}${RESET}\n")

  def run(): Unit = {
    println(s"\n${BOLD}${number}${RESET}. ${CYAN}${description}.${RESET}\n")
    exec()
  }
}

