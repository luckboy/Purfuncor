package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._

trait GlobalSymbolTabular[T, U]
{
  def getGlobalLocationFromTable(table: T)(sym: GlobalSymbol): Option[U]
  
  def getGlobalSymbolFromTable(table: T)(loc: U): Option[GlobalSymbol]
}