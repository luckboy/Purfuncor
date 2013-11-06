package pl.luckboy.purfuncor.frontend.typer.spec
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable

trait TyperSpecUtils
{
  val makeInferredKindTable = {
    (s: String) =>
      resolver.Resolver.transformString(s)(NameTree.empty).flatMap {
        res =>
          val (_, res2) = Typer.statefullyTransformToSymbolTree2(InferredKindTable.empty)(res).run(SymbolTypeEnvironment.empty[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]])
          res2.map { _.treeInfo.typeTree.treeInfo.kindTable }
    }
  }
}