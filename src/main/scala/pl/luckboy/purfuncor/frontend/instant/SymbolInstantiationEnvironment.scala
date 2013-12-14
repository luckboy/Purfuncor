package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeInferenceEnvironment
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.RecursiveInitializer._

case class SymbolInstantiationEnvironment[T, U](
    typeInferenceEnv: SymbolTypeInferenceEnvironment[T, U],
    globalInstTree: InstanceTree[GlobalSymbol, AbstractPolyFunction[GlobalSymbol], GlobalInstance[GlobalSymbol]],
    lambdaInfos: Map[GlobalSymbol, Map[Int, InstantiationLambdaInfo[GlobalSymbol, GlobalSymbol]]],
    combNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])