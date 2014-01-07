/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
import pl.luckboy.purfuncor.frontend.typer
import pl.luckboy.purfuncor.frontend.instant
import pl.luckboy.purfuncor.backend.interp

object Main
{
  type Environment = interp.SymbolEnvironment[instant.LambdaInfo[parser.LambdaInfo, resolver.LocalSymbol, resolver.GlobalSymbol, resolver.GlobalSymbol], frontend.TypeSimpleTerm[resolver.Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]], kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]]
  
  type TypeEnvironment = typer.SymbolTypeEnvironment[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]]
  
  type NoValue = interp.NoValue[resolver.Symbol, instant.LambdaInfo[parser.LambdaInfo, resolver.LocalSymbol, resolver.GlobalSymbol, resolver.GlobalSymbol], frontend.TypeSimpleTerm[resolver.Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]], interp.SymbolClosure[instant.LambdaInfo[parser.LambdaInfo, resolver.LocalSymbol, resolver.GlobalSymbol, resolver.GlobalSymbol], frontend.TypeSimpleTerm[resolver.Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, resolver.LocalSymbol]]]]

  type NoType = typer.NoType[pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol]
  
  object ExitFlag extends Enumeration
  {
    val Exit, NoExit = Value
  }
  
  val commands = Map[String, String => State[Environment, ExitFlag.Value]](
      "evaltype" -> {
        arg => State({ env =>
          val (env2, res) = interpretTypeTerm(arg)(env)
          res match {
            case Success(typeValue) => consoleReader.println(typeValue.toString)
            case Failure(errs)      => consoleReader.println(errs.list.mkString("\n"))
          }
          (env2, ExitFlag.NoExit)
        })
      },
      "help" -> {
        _ => State({ env =>
          consoleReader.println("Commands:")
          consoleReader.println()
          consoleReader.println(":evaltype <type expr>   evaluate the type expression")
          consoleReader.println(":help                   display this text")
          consoleReader.println(":kind <type expr>       display the kind of the type expression")
          consoleReader.println(":load <path> ...        load files")
          consoleReader.println(":paste                  enable the paste mode (exit from this mode is ctrl-D)")
          consoleReader.println(":quit                   exit this interpreter")
          consoleReader.println(":type <expr>            display the type of the expression")
          consoleReader.println()
          (env, ExitFlag.NoExit)
        })
      },
      "load" -> {
        arg => State({ env =>
          if(!arg.isEmpty) {
            val (env2, res) = interpretTreeFiles(arg.split("\\s+").map { s => new java.io.File(s) }.toList).run(env)
            printResult(res)
            (env2, ExitFlag.NoExit)
          } else {
            consoleReader.println("no file")
            (env, ExitFlag.NoExit)
          }
        })
      },
      "kind" -> {
        arg => State({ env =>
          val (env2, res) = transformTypeTermStringWithKindInference(arg).run(env)
          res match {
            case Success((_, kind)) => consoleReader.println(kind.toString)
            case Failure(errs)      => consoleReader.println(errs.list.mkString("\n"))
          }
          (env, ExitFlag.NoExit)
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
      },
      "type" -> {
        arg => State({ env =>
          val (env2, res) = transformTermStringWithTypeInference(arg).run(env)
          res match {
            case Success((_, typ)) => consoleReader.println(typ.toString)
            case Failure(errs)     => consoleReader.println(errs.list.mkString("\n"))
          }
          (env2, ExitFlag.NoExit)
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
  
  def transformTermStringWithTypeInference(s: String) =
    State({
      (env: Environment) =>
        val (env2, nameTree) = interp.symbolEnvironmentState.nameTreeFromEnvironmentS(env)
        val (typeEnv, typeInferenceEnv) = typer.Typer.statefullyMakeSymbolTypeInferenceEnvironment3(env2.kindTable, env2.typeTable).run(env2.typeEnv)
        (env2.copy(typeEnv = typeEnv), typer.Typer.transformTermStringWithTypeInference(s)(nameTree, typeInferenceEnv)(typer.Typer.transformToSymbolTerm2(env2.kindTable)))
    })
  
  def interpretTypeTerm(s: String) =
    State({
      (env: Environment) =>
        val (env2, nameTree) = interp.symbolEnvironmentState.nameTreeFromEnvironmentS(env)
        val (typeEnv, res) = typer.symbolTypeEnvironmentState.withClearS {
          typer.Typer.interpretTypeTermString(s)(nameTree)(typer.Typer.transformToSymbolTypeTerm2(env.kindTable)).run(_: TypeEnvironment)
        } (env.typeEnv)
        (env2.copy(typeEnv = typeEnv), res)
    })
    
  def transformTypeTermStringWithKindInference(s: String) =
    State({
      (env: Environment) =>
        val (env2, nameTree) = interp.symbolEnvironmentState.nameTreeFromEnvironmentS(env)
        (env2, kinder.Kinder.transformTypeTermStringWithKindInference(s)(nameTree, kinder.SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](env2.kindTable))(kinder.Kinder.transformToSymbolTypeTerm))
    })
  
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
    line.split("\\s+", 2).toList match {
      case cmd :: s =>
        if(cmd.headOption.map { _ === ':' }.getOrElse(false)) some((cmd.tail, s.headOption.getOrElse(""))) else none
      case _               =>
        none
    }
  
  @tailrec
  def mainLoop(env: Environment): Unit = {
    consoleReader.setPrompt("purfuncor> ")
    val line = consoleReader.readLine()
    if(line =/= null) {
      val (env2, exitFlag) = parseCommandLine(line) match {
        case Some((cmdName, arg)) =>
          commands.filter { _._1.startsWith(cmdName) }.map { _._2 }.toList match {
            case List(cmd) => cmd(arg).run(env)
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
      exitFlag match {
        case ExitFlag.Exit   => ()
        case ExitFlag.NoExit => mainLoop(env2)
      } 
    }
  }
  
  def main(args: Array[String]): Unit = {
    mainLoop(interp.SymbolEnvironment.empty)
  }
}
