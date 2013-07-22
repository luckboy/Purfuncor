package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.lmbdindexer.TypeLambdaInfo
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import pl.luckboy.purfuncor.frontend.kinder.KindTermUnifier._
import pl.luckboy.purfuncor.frontend.kinder.KindInferrer._

package object kinder
{
  implicit val noKindSemigroup: Semigroup[NoKind] = new Semigroup[NoKind] {
    override def append(f1: NoKind, f2: => NoKind): NoKind = throw new UnsupportedOperationException
  }
  
  implicit val symbolKindTermUnifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], SymbolKindInferenceEnvironment, Int] = new Unifier[NoKind, KindTerm[StarKindTerm[Int]], SymbolKindInferenceEnvironment, Int] {
    override def matchesTermsS[U](term1: KindTerm[StarKindTerm[Int]], term2: KindTerm[StarKindTerm[Int]])(z: U)(f: (Int, Either[Int, KindTerm[StarKindTerm[Int]]], U, SymbolKindInferenceEnvironment) => (SymbolKindInferenceEnvironment, Validation[NoKind, U]))(env: SymbolKindInferenceEnvironment) =
      matchesKindTermsS(term1, term2)(z)(f)(env)
    
    override def getParamTermS(param: Int)(env: SymbolKindInferenceEnvironment) =
      (env, env.kindParamForest.getTerm(param))

    override def findRootParamS(param: Int)(env: SymbolKindInferenceEnvironment) =
      (env, env.kindParamForest.findRootParam(param).toSuccess(NoKind.fromError(FatalError("not found kind parameter", none, NoPosition))))
    
    override def replaceParamS(param: Int, term: KindTerm[StarKindTerm[Int]])(env: SymbolKindInferenceEnvironment) =
      if(!env.kindParamForest.containsTerm(param)) 
        env.kindParamForest.replaceParam(param, term).map {
          kpf => (env.withKindParamForest(kpf), ().success)
        }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter", none, NoPosition)).failure))
      else
        (env, NoKind.fromError(FatalError("kind parameter is already replaced", none, NoPosition)).failure)          

    override def unionParamsS(param1: Int, param2: Int)(env: SymbolKindInferenceEnvironment) =
      if(!env.kindParamForest.containsTerm(param1) && !env.kindParamForest.containsTerm(param2))
        env.kindParamForest.unionParams(param1, param2).map {
          kpf => (env.withKindParamForest(kpf), ().success)
        }.getOrElse((env, NoKind.fromError(FatalError("not found one kind parameter or two kind parameters", none, NoPosition)).failure))
      else
        (env, NoKind.fromError(FatalError("one kind parameter or two kind parameters are already replaced", none, NoPosition)).failure)
      
    override def allocateParamS(env: SymbolKindInferenceEnvironment) =
      env.kindParamForest.allocateParam.map { 
        case (kpf, p) => (env.withKindParamForest(kpf), p.success)
      }.getOrElse((env, NoKind.fromError(FatalError("one kind parameter or two kind parameters are already replaced", none, NoPosition)).failure))

    override def replaceTermParamsS(term: KindTerm[StarKindTerm[Int]])(f: (Int, SymbolKindInferenceEnvironment) => (SymbolKindInferenceEnvironment, Validation[NoKind, Either[Int, KindTerm[StarKindTerm[Int]]]]))(env: SymbolKindInferenceEnvironment) =
      replaceKindTermParamsS(term)(f)(env)

    override def mismatchedTermErrorS(env: SymbolKindInferenceEnvironment) = {
      val (s1, s2) = env.currentKindTermPair.map {
        case (t1, t2) => (intKindTermShowing.stringFrom(t1), intKindTermShowing.stringFrom(t2))
      }.getOrElse(("<unknown kind>", "<unknown kind>"))
      (env, NoKind.fromError(Error("couldn't match kind " + s1 + " with kind " + s2, none, NoPosition)))
    }
      
    override def withSaveS[U, V](f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, Validation[U, V]))(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Validation[U, V]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }
    }
  }
  
  implicit val symbolTypeSimpleTermKindInferrer: Inferrer[TypeSimpleTerm[Symbol, TypeLambdaInfo], SymbolKindInferenceEnvironment, Kind] = new Inferrer[TypeSimpleTerm[Symbol, TypeLambdaInfo], SymbolKindInferenceEnvironment, Kind] {
    override def inferSimpleTermInfoS(simpleTerm: TypeSimpleTerm[Symbol, TypeLambdaInfo])(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Kind) =
      simpleTerm match {
        case TypeLambda(args, body, TypeLambdaInfo(lambdaIdx)) =>
          throw new UnsupportedOperationException
        case TypeVar(loc) =>
          (env, env.typeVarKind(loc))
        case TypeLiteral(value) =>
          throw new UnsupportedOperationException
        case KindedTypeTerm(term, kind) =>
          throw new UnsupportedOperationException          
      }
    
    override def unifyInfosS(info1: Kind, info2: Kind)(env: SymbolKindInferenceEnvironment) = {
      val (env2, res1) = info1.instantiatedKindTermS(env)
      val (env3, res2) = info2.instantiatedKindTermS(env2)
      (res1 |@| res2) { (kt1, kt2) => env3.withKindTermPair(some((kt1, kt2)))(unifyKindsS(info1, info2)) }.valueOr { nk => (env3, nk) }
    }
      
    override def argInfosFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment) =
      argKindsFromKindS(info, argCount)(env)
      
    override def returnInfoFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment) =
      returnKindFromKindS(info, argCount)(env)
    
    override def isNoInfo(info: Kind) =
      info.isNoKind

    override def functionInfo(argCount: Int): Kind =
      throw new UnsupportedOperationException
      
    override def concatErrors(info1: Kind, info2: Kind): Kind =
      (info1, info2) match {
        case (noKind1: NoKind, noKind2: NoKind) => noKind1 |+| noKind2
        case _                                  => NoKind.fromError(FatalError("can't concat errors", none, NoPosition))
      } 

    override def unequalListLengthNoInfo: Kind =
      throw new UnsupportedOperationException

    override def withPos(res: (SymbolKindInferenceEnvironment, Kind))(pos: Position): (SymbolKindInferenceEnvironment, Kind) =
      throw new UnsupportedOperationException
  }
}