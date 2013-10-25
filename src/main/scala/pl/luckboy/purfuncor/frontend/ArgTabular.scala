package pl.luckboy.purfuncor.frontend

trait ArgTabular[T, U]
{
  def getArgLocationsFromTable(table: T): Seq[Option[U]]
}