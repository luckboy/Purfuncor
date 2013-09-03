package pl.luckboy.purfuncor.frontend.resolver

trait LocalSymbolTabular[T, U]
{
  def getLocalLocationFromTable(table: T)(sym: LocalSymbol): Option[U]
  
  def getLocalSymbolFromTable(table: T)(loc: U): Option[LocalSymbol]
}