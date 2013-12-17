package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeInferenceEnvironment
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.RecursiveInitializer._
import pl.luckboy.purfuncor.common.Result._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._
import pl.luckboy.purfuncor.frontend.typer.TypeResult._
import pl.luckboy.purfuncor.frontend.instant.PolyFunInstantiator._
import pl.luckboy.purfuncor.frontend.instant.TermUtils._

package object instant
{
  implicit def symbolTypeInferenceEnvironmentState[T, U]: TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol] = new TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol] {
    override def globalVarTypeFromEnvironmentS(loc: GlobalSymbol)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.varType(loc))
  
    override def notFoundInstanceNoTypeS(instArg: InstanceArg[GlobalSymbol, GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, NoType.fromError[GlobalSymbol](Error("couldn't find instance for " + instArg.polyFun + " with type " + instArg.typ, none, NoPosition)))
  
    override def ambiguousInstanceNoTypeS(instArg: InstanceArg[GlobalSymbol, GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, NoType.fromError[GlobalSymbol](Error("ambiguous instance for " + instArg.polyFun + " with type " + instArg.typ, none, NoPosition)))
  }
  
  implicit def symbolPolyFunInstantiator[T, U](implicit envSt: TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol]): PolyFunInstantiator[GlobalSymbol, GlobalSymbol, SymbolInstantiationEnvironment[T, U]] = new PolyFunInstantiator[GlobalSymbol, GlobalSymbol, SymbolInstantiationEnvironment[T, U]] {
    override def instantiatePolyFunctionS(lambdaInfo: PreinstantiationLambdaInfo[GlobalSymbol, GlobalSymbol], instArgs: Seq[InstanceArg[GlobalSymbol, GlobalSymbol]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, LocalInstance[GlobalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]) = {
      val (typeInferenceEnv, res) = PolyFunInstantiator.instantiatePolyFunctionS(lambdaInfo, instArgs, env.globalInstTree)(localInstTree)(env.typeInferenceEnv)
      (env.withTypeInferenceEnv(typeInferenceEnv), resultFromTypeResult(res))
    }
    
    override def getLambdaInfosFromEnvironmentS(loc: Option[GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]) =
      (env, env.lambdaInfos.get(loc))
  }
  
  implicit def symbolCombinatorInstanceRecursiveInitialzer[T, U]: RecursiveInitializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolInstantiationEnvironment[T, U]] = new RecursiveInitializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolInstantiationEnvironment[T, U]] {
    override def combinatorFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.comb
    
    override def recursiveCombinatorsFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.recursiveCombSyms
    
    override def markedRecursiveCombinatorsFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.markedRecCombSyms
    
    override def createNode(comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], recursiveCombLocs: Set[GlobalSymbol], markedRecCombLocs: Set[GlobalSymbol]) =
      CombinatorNode(comb, recursiveCombLocs, markedRecCombLocs)
    
    override def addNodeS(loc: GlobalSymbol, node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Unit) =
      (env.withComb(loc, node), ())
    
    override def isRecursiveFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = (env, env.isRecursive)
    
    override def isUninitializedGlobalVarS(loc: GlobalSymbol)(env: SymbolInstantiationEnvironment[T, U]) = (env, !env.lambdaInfos.contains(some(loc)))
    
    override def nonRecursivelyInitializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]) =
      if(!env.isRecursive)
        comb match {
          case Combinator(_, _, body, lambdaInfo, file) =>
            val lambdaInfos = Map(some(loc) -> (preinstantiationLambdaInfosFromTerm(body).mapValues { _.copy(file = file) } + (0 -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo))))
            val (env2, res) = instantiatePolyFunctionsForCombinatorsS(lambdaInfos)(env)
            res.map { lis => (env2.withLambdaInfos(env2.lambdaInfos ++ lis), ().successNel) }.valueOr { es => (env2, es.failure) }
          case PolyCombinator(_, _)         =>
            env.typeInferenceEnv.varType(loc) match {
              case typ: InferredType[GlobalSymbol] =>
                val instArgs = Seq(InstanceArg(PolyFunction(loc), typ))
                (env.withLambdaInfos(env.lambdaInfos + (some(loc) -> Map(0 -> InstantiationLambdaInfo(Seq(), instArgs)))), ().successNel)
              case noType: NoType[GlobalSymbol]    =>
                (env, resultFromTypeResult(noType.failure))
              case _                               =>
                (env, FatalError("uninferred type", none, NoPosition).failureNel)
            }
        }
      else
        (env, ().successNel)
    
    override def checkInitializationS(res: ValidationNel[AbstractError, Unit], combLocs: Set[GlobalSymbol], oldNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(env: SymbolInstantiationEnvironment[T, U]) = {
      val lambdaInfos = oldNodes.map {
        case (loc, oldNode) =>
          oldNode.comb match {
            case Combinator(_, _, body, _, file) => (some(loc), preinstantiationLambdaInfosFromTerm(body).mapValues { _.copy(file = file) })
            case PolyCombinator(_, _)            => (some(loc), Map[Int, PreinstantiationLambdaInfo[GlobalSymbol, GlobalSymbol]]())
          }
      }
      val (env2, res2) = instantiatePolyFunctionsForCombinatorsS(lambdaInfos)(env)
      val (env3, res3) = res2.map { lis => (env2.withLambdaInfos(env2.lambdaInfos ++ lis), ().successNel) }.valueOr { es => (env2, es.failure) }
      val (env4, res4) = (res |@| res3) { (_, _) => (env3, ().successNel) }.valueOr { es => (env3, es.failure) }
      (res4.map { _ => env4 }.getOrElse(env), res4)
    }
    
    override def nodesFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = (env, env.combNodes)
    
    override def withRecursiveS[V](combLocs: Set[GlobalSymbol], newNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], V))(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], V) = {
      val (env2, res) = f(env.withRecursive(true).withLambdaInfos(env.lambdaInfos -- combLocs.map(some)).withRecursiveCombSyms(combLocs))
      (env2.withRecursive(false).withCombNodes(newNodes).withRecursiveCombSyms(combLocs), res)
    }
    
    override def withClearS[V](f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], V))(env: SymbolInstantiationEnvironment[T, U]) =
      f(env)
  }
  
  implicit def symbolCombinatorInstanceInitializer[T, U]: Initializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolInstantiationEnvironment[T, U]] = new Initializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolInstantiationEnvironment[T, U]] {
    override def globalVarsFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = (env, env.lambdaInfos.keySet.flatten)
    
    override def usedGlobalVarsFromCombinator(comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]]) =
      comb match {
        case Combinator(_, _, body, _, _) => usedGlobalVarsFromTerm(body)
        case PolyCombinator(_, _)         => Set()
      }
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolInstantiationEnvironment[T, U]) = (env, ())
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]) = {
      val (env2, res) = recursivelyInitializeGlobalVarS(loc, comb)(resolver.TreeInfo(Tree(Map[GlobalSymbol, AbstractTypeCombinator[Symbol, TypeLambdaInfo[U, LocalSymbol]]](), resolver.TypeTreeInfo), Map(), Nil))(env)
      (env2, resultForFile(res, comb.file))
    }
    
    override def checkEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = (env, env.errs.toNel.toFailure(()))
    
    override def undefinedGlobalVarError = NonEmptyList(FatalError("undefined global variable", none, NoPosition))
    
    override def withSaveS[V, W](f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], Validation[V, W]))(env: SymbolInstantiationEnvironment[T, U]) =  {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }        
    }
  }
}