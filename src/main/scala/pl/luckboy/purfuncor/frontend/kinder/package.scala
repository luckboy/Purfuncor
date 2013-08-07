package pl.luckboy.purfuncor.frontend
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.lmbdindexer.TypeLambdaInfo
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import pl.luckboy.purfuncor.frontend.kinder.KindTermUnifier._
import pl.luckboy.purfuncor.frontend.kinder.KindInferrer._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._

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
        env.kindParamForest.findRootParam(param).map {
          rootParam =>
            env.irreplaceableKindParams.get(rootParam).map {
              kts => (env, NoKind.fromErrors(kts.map { kt => Error("couldn't instantiate parameter at defined kind " + intKindTermFromKindTerm(kt), none, NoPosition) }).failure)
            }.getOrElse {
              env.kindParamForest.replaceParam(rootParam, term).map {
                kpf => (env.withKindParamForest(kpf), ().success)
              }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter", none, NoPosition)).failure))
            }
        }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter", none, NoPosition)).failure))
      else
        (env, NoKind.fromError(FatalError("kind parameter is already replaced", none, NoPosition)).failure)          

    override def unionParamsS(param1: Int, param2: Int)(env: SymbolKindInferenceEnvironment) =
      if(!env.kindParamForest.containsTerm(param1) && !env.kindParamForest.containsTerm(param2))
        env.kindParamForest.unionParams(param1, param2).map {
          kpf =>
            kpf.findRootParam(param1).map {
              rp =>
                val definedKindTerms = env.irreplaceableKindParams.get(param1).map { _.list }.getOrElse(Nil) ++ env.irreplaceableKindParams.get(param1).map { _.list }.getOrElse(Nil)
                val newIrreplaceableKindParams = IntMap() ++ (env.irreplaceableKindParams ++ definedKindTerms.toNel.map { rp -> _ })
                (env.withKindParamForest(kpf).copy(irreplaceableKindParams = newIrreplaceableKindParams), ().success)
            }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter", none, NoPosition)).failure))
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
    override def inferSimpleTermInfoS(simpleTerm: TypeSimpleTerm[Symbol, TypeLambdaInfo])(env: SymbolKindInferenceEnvironment) =
      simpleTerm match {
        case TypeLambda(args, body, TypeLambdaInfo(lambdaIdx)) =>
          env.withTypeLambdaIdx(lambdaIdx) {
            _.withLocalTypeVarKinds(args.list.flatMap { a => a.name.map { s => (LocalSymbol(s), a.kind) } }.toMap) {
              newEnv =>
                val (newEnv2, retInfo) = inferS(body)(newEnv)
                val argInfos = args.map { a => a.name.map { s => newEnv2.typeVarKind(LocalSymbol(s)) }.getOrElse(InferredKind(Star(KindParam(0), NoPosition))) }.list
                functionKindFromKindsS(argInfos, retInfo)(newEnv2)
            }
          }
        case TypeVar(loc) =>
          (env, env.typeVarKind(loc))
        case TypeLiteral(value) =>
          value match {
            case TupleTypeFunValue(n)    => (env, InferredKind.tupleTypeFunKind(n))
            case TypeBuiltinFunValue(bf) => (env, InferredKind.fromTypeBuiltinFunction(bf))
          }
        case KindedTypeTerm(term, kind) =>
          val (env2, info) = inferS(term)(env)
          val (env3, res) = allocateKindTermParamsS(kind)(Map())(env2)
          res.map { p => unifyInfosS(info, InferringKind(p._2))(env3.withDefinedKind(p._2)) }.valueOr { (env3, _) }
      }
    
    override def unifyInfosS(info1: Kind, info2: Kind)(env: SymbolKindInferenceEnvironment) = {
      val (env2, res1) = info1.instantiatedKindTermS(env)
      val (env3, res2) = info2.instantiatedKindTermS(env2)
      (res1 |@| res2) { (kt1, kt2) => env3.withKindTermPair(some((kt1, kt2)))(unifyKindsS(info1, info2)) }.valueOr { (env3, _) }
    }
      
    override def argInfosFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment) =
      argKindsFromKindS(info, argCount)(env)
      
    override def returnInfoFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment) =
      returnKindFromKindS(info, argCount)(env)
    
    override def isNoInfo(info: Kind) =
      info.isNoKind

    override def functionInfo(argCount: Int) =
      functionKind(argCount)
      
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
  
  implicit val symbolTypeCombinatorKindInitializer = new Initializer[NoKind, GlobalSymbol, AbstractTypeCombinator[Symbol, TypeLambdaInfo], SymbolKindInferenceEnvironment] {
    override def globalVarsFromEnvironmentS(env: SymbolKindInferenceEnvironment) = (env, env.globalTypeVarKinds.keySet)
    
    override def usedGlobalVarsFromCombinator(comb: AbstractTypeCombinator[Symbol, TypeLambdaInfo]) =
      comb match {
        case TypeCombinator(_, _, body, _, _) => usedGlobalTypeVarsFromTypeTerm(body)
        case UnittypeCombinator(_, _, _)      => Set()
      }
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolKindInferenceEnvironment) =
      (env.withGlobalTypeVarKind(loc, UninferredKind), ())
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractTypeCombinator[Symbol, TypeLambdaInfo])(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Validation[NoKind, Unit]) =
      comb match {
        case TypeCombinator(kind, args, body, TypeLambdaInfo(lambdaIdx), file) =>
          throw new UnsupportedOperationException
        case UnittypeCombinator(n, kind, file)                  =>
          throw new UnsupportedOperationException
      }
    
    override def undefinedGlobalVarError: NoKind = throw new UnsupportedOperationException

    override def withSaveS[T, U](f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, Validation[T, U]))(env: SymbolKindInferenceEnvironment) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }
    }
  }
}