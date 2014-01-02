package pl.luckboy.purfuncor
import scala.io.Source
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._
import scala.tools.jline.console.ConsoleReader
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.parser
import pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.frontend.kinder
import pl.luckboy.purfuncor.frontend.instant
import pl.luckboy.purfuncor.backend.interp

object Main
{
  type Environment = interp.SymbolEnvironment[instant.LambdaInfo[parser.LambdaInfo, resolver.LocalSymbol, resolver.GlobalSymbol, resolver.GlobalSymbol], frontend.TypeSimpleTerm[resolver.Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]], kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]]
  
  type NoValue = interp.NoValue[resolver.Symbol, instant.LambdaInfo[parser.LambdaInfo, resolver.LocalSymbol, resolver.GlobalSymbol, resolver.GlobalSymbol], frontend.TypeSimpleTerm[resolver.Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]], interp.SymbolClosure[instant.LambdaInfo[parser.LambdaInfo, resolver.LocalSymbol, resolver.GlobalSymbol, resolver.GlobalSymbol], frontend.TypeSimpleTerm[resolver.Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]]]]

  object ExitFlag extends Enumeration
  {
    val Exit, NoExit = Value
  }
  
  val commands = Map[String, List[String] => State[Environment, ExitFlag.Value]](
      "help" -> {
        _ => State({ env =>
          consoleReader.println("Commands:")
          consoleReader.println()
          consoleReader.println(":help                   display this text")
          consoleReader.println(":load <path> ...        load files")
          consoleReader.println(":paste                  enable the paste mode (exit from this mode is ctrl-D)")
          consoleReader.println(":quit                   exit this interpreter")
          consoleReader.println()
          (env, ExitFlag.NoExit)
        })
      },
      "load" -> {
        args => State({ env =>
          val (env2, res) = interpretTreeFiles(args.map { s => new java.io.File(s)}).run(env)
          printResult(res)
          (env2, ExitFlag.NoExit)
        })
      },
      "paste" -> {
        _ => State({ env =>
          val (env2, res) = interpretTreeString(readString()).run(env)
          printResult(res)
          (env2, ExitFlag.NoExit)
        })
      },
      "quit" -> {
        _ => State({ env => 
          (env, ExitFlag.Exit) 
        })
      })
  
  @tailrec
  def readStringLoop(s: String): String = {
    val line = consoleReader.readLine()
    if(line =/= null) readStringLoop(s + line + "\n") else s
  }

  def interpretTreeString(s: String) = interp.Interpreter.interpretTreeString(s)(interp.Interpreter.statefullyTransformToSymbolTree)
  
  def interpretTreeFiles(files: List[java.io.File]) = interp.Interpreter.interpretTreeFiles(files)(interp.Interpreter.statefullyTransformToSymbolTree)

  def interpretTermString(s: String) = interp.Interpreter.interpretTermString(s)(interp.Interpreter.transformToSymbolTerm3)

  lazy val consoleReader = new ConsoleReader
  
  def readString() = {
    val savedPrompt = consoleReader.getPrompt()
    consoleReader.setPrompt("")
    val s = readStringLoop("")
    consoleReader.setPrompt(savedPrompt)
    s
  }

  def printResult(res: ValidationNel[AbstractError, Validation[NoValue, Unit]]) =
    res match {
      case Success(Success(()))      => ()
      case Success(Failure(noValue)) => consoleReader.println(noValue.toString)
      case Failure(errs)             => consoleReader.println(errs.list.mkString("\n"))
    }
  
  def parseCommandLine(line: String) =
    line.split("\\s+").toList match {
      case cmd :: args =>
        if(cmd.headOption.map { _ === ':' }.getOrElse(false)) some((cmd.tail, args)) else none
      case _               =>
        none
    }
  
  @tailrec
  def mainLoop(env: Environment): Unit = {
    consoleReader.setPrompt("purfuncor> ")
    val line = consoleReader.readLine()
    if(line =/= null) {
      val (env2, exitFlag) = parseCommandLine(line) match {
        case Some((cmdName, args)) =>
          commands.filter { _._1.startsWith(cmdName) }.map { _._2 }.toList match {
            case List(cmd) => cmd(args).run(env)
            case Nil       => consoleReader.println("unknown command"); (env, ExitFlag.NoExit)
            case _         => consoleReader.println("ambiguous command"); (env, ExitFlag.NoExit)
          }
        case None                  =>
          val (newEnv, res) = interpretTermString(line).run(env)
          res match {
            case Success(value) => consoleReader.println(value.toString)
            case Failure(errs)  => consoleReader.println(errs.list.mkString("\n"))
          }
          (newEnv, ExitFlag.NoExit)
      }
      exitFlag match{
        case ExitFlag.Exit   => ()
        case ExitFlag.NoExit => mainLoop(env2)
      } 
    }
  }
  
  def main(args: Array[String]): Unit = {
    mainLoop(interp.SymbolEnvironment.empty)
  }
}