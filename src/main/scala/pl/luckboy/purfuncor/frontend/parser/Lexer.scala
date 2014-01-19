/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh
import scalaz._
import scalaz.Scalaz._

case class Lexer() extends StdLexical
{
  delimiters ++= List("(", ")", "{", "}", "[", "]", "#", "##", ".", "\\", ";", "\n", "###")
  reserved ++= List("_", "false", "true", "tuple", "let", "in", "module", "import", "type", "unittype", "construct", 
      "select", "extract", "poly", "instance", "makearray", "makelist", "fieldset", "=", "=>", ":", "*", "!")
  
  case class CharLit(chars: String) extends Token
  case class ByteLit(chars: String) extends Token
  case class ShortLit(chars: String) extends Token
  case class IntLit(chars: String) extends Token
  case class LongLit(chars: String) extends Token
  case class FloatLit(chars: String) extends Token
  case class DoubleLit(chars: String) extends Token
  case class ConstrIdentifier(chars: String) extends Token
  case class VarIdentifier(chars: String) extends Token
  case class OpIdentifier(chars: String) extends Token
  
  override def token = (
      floatLit
      | doubleLit
      | keywordOrVarIdent
      | constrIdent
      | varIdent
      | charLit
      | byteLit
      | shortLit
      | longLit
      | intLit
      | stringLit
      | delim
      | keywordOrOpIdent)

  override def identChar = constrIdentChar | varIdentChar
      
  override def whitespaceChar = elem("space char",  c => c <= ' ' && c =/= '\n' && c =/= EofCh)

  def constrIdentChar = elem("upper", _.isUpper)  
  def varIdentChar = elem("lower", _.isLower) | elem('_')
  def opIdentChar = elem("op ident char", c => c.getType === Character.OTHER_PUNCTUATION && !"#;.\"'".contains(c))
  
  def esc = (
      elem('\\') ~ 'b'												^^^ '\b'
      | elem('\\') ~ 't'											^^^ '\t'
      | elem('\\') ~ 'n'											^^^ '\n'
      | elem('\\') ~ 'f'											^^^ '\f'
      | elem('\\') ~ 'r'											^^^ '\r'
      | elem('\\') ~ '\\'											^^^ '\\'
      | elem('\\') ~ '\''											^^^ '\''
      | elem('\\') ~ '"'											^^^ '"'
      | elem('\\') ~> repN(3, octDigit)								^^ { cs => Integer.parseInt(cs.mkString(""), 8).toChar }
      | elem('\\') ~> repN(2, octDigit)								^^ { cs => Integer.parseInt(cs.mkString(""), 8).toChar }
      | elem('\\') ~> octDigit										^^ { c => Integer.parseInt(c.toString, 8).toChar }
      | elem('\\') ~ 'u' ~> repN(4, hexDigit)						^^ { cs => Integer.parseInt(cs.mkString(""), 16).toChar }
      )
  
  def keywordOrVarIdent = varIdentChar ~ ((identChar | digit) *)	^^ { 
    case c ~ cs => 
      val s = (c :: cs).mkString("")
      if(reserved.contains(s)) Keyword(s) else VarIdentifier(s)
  }  

  def constrIdent = constrIdentChar ~ ((identChar | digit) *)		^^ { case c ~ cs => ConstrIdentifier((c :: cs).mkString("")) }  
  def varIdent = elem('`') ~> (chrExcept('`', EofCh) +) <~ elem('`') ^^ { cs => VarIdentifier(cs.mkString("")) }
  def keywordOrOpIdent = (opIdentChar +)							^^ { 
    cs =>
      val s = cs.mkString("")
      if(reserved.contains(s)) Keyword(s) else OpIdentifier(s)
  }
  
  def charLit = elem('\'') ~> (esc | chrExcept('\'', '\n', EofCh)) <~ elem('\'') ^^ { c => CharLit(c.toString) }
     
  def hexDigit = elem("hex digit", c => (c >= '0' && c <= '9') || (c.toUpper >= 'A' && c.toUpper <= 'F'))
  def octDigit = elem("oct digit", c => c >= '0' && c <= '7')
  
  def byteLit = integer <~ 'b'										^^ ByteLit
  def shortLit = integer <~ 's'										^^ ShortLit
  def intLit = integer 												^^ IntLit
  def longLit = integer <~ 'L'										^^ LongLit
  
  def integer = hexInteger | octInteger | decInteger
  def hexInteger = '0' ~ (elem('X') | elem('x')) ~ (hexDigit +)		^^ { case c1 ~ c2 ~ cs => (List(c1, c2) ++ cs).mkString("") }
  def octInteger = '0' ~ (octDigit *)								^^ { case c ~ cs => (c :: cs).mkString("") }
  def decInteger = (digit +)										^^ { _.mkString("") }

  def floatLit = (float | decInteger) <~ 'f'						^^ FloatLit
  def doubleLit = float												^^ DoubleLit
  
  def float = float1 | float2
  def float1 = (digit +) ~ '.' ~ (digit *) ~ (exp ?)				^^ { 
    case cs1 ~ c ~ cs2 ~ optS => ((cs1 :+ c) ++ cs2).mkString("") + optS.getOrElse("")
  }
  def float2 = (digit +) ~ exp										^^ { case cs ~ s => cs.mkString ++ s }
  def exp = (elem('e') | elem('E')) ~ ((elem('+') | elem('-')) ?) ~ (digit +) ^^ {
    case c1 ~ optC2 ~ cs => (List(c1) ++ optC2 ++ cs).mkString("")
  }
  
  def stringLit = elem('"') ~> ((esc | chrExcept('"', '\n', EofCh)) *) <~ elem('"') ^^ { cs => StringLit(cs.mkString("")) }
}
