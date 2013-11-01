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
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.common.Initializer._
import pl.luckboy.purfuncor.common.RecursiveInitializer._
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import pl.luckboy.purfuncor.frontend.kinder.KindTermUnifier._
import pl.luckboy.purfuncor.frontend.kinder.KindInferrer._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._

package object kinder
{
  implicit val noKindSemigroup: Semigroup[NoKind] = new Semigroup[NoKind] {
    override def append(f1: NoKind, f2: => NoKind) =
      (f1, f2) match {
        case (NoKind(prevErrs1, currentErrs1), NoKind(prevErrs2, currentErrs2)) =>
          NoKind(prevErrs = prevErrs1 ++ prevErrs2, currentErrs = currentErrs1 ++ currentErrs2)
      }
  }
  
  implicit def symbolKindTermUnifier[T]: Unifier[NoKind, KindTerm[StarKindTerm[Int]], SymbolKindInferenceEnvironment[T], Int] = new Unifier[NoKind, KindTerm[StarKindTerm[Int]], SymbolKindInferenceEnvironment[T], Int] {
    override def matchesTermsS[U](term1: KindTerm[StarKindTerm[Int]], term2: KindTerm[StarKindTerm[Int]])(z: U)(f: (Int, Either[Int, KindTerm[StarKindTerm[Int]]], U, SymbolKindInferenceEnvironment[T]) => (SymbolKindInferenceEnvironment[T], Validation[NoKind, U]))(env: SymbolKindInferenceEnvironment[T]) =
      matchesKindTermsS(term1, term2)(z)(f)(env)
    
    override def getParamTermS(param: Int)(env: SymbolKindInferenceEnvironment[T]) =
      (env, env.kindParamForest.getTerm(param))

    override def findRootParamS(param: Int)(env: SymbolKindInferenceEnvironment[T]) =
      (env, env.kindParamForest.findRootParam(param).toSuccess(NoKind.fromError(FatalError("not found kind parameter", none, NoPosition))))
    
    override def replaceParamS(param: Int, term: KindTerm[StarKindTerm[Int]])(env: SymbolKindInferenceEnvironment[T]) =
      if(!env.kindParamForest.containsTerm(param)) 
        env.kindParamForest.findRootParam(param).map {
          rp =>
            env.irreplaceableKindParams.get(rp).map {
              kts => (env, NoKind.fromErrors(kts.map { kt => Error("couldn't instantiate parameter at defined kind " + intKindTermShowing.stringFrom(intKindTermFromKindTerm(kt)), none, NoPosition) }).failure)
            }.getOrElse {
              env.kindParamForest.replaceParam(rp, term).map {
                kpf => (env.withKindParamForest(kpf), ().success)
              }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter", none, NoPosition)).failure))
            }
        }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter", none, NoPosition)).failure))
      else
        (env, NoKind.fromError(FatalError("kind parameter is already replaced", none, NoPosition)).failure)          

    override def unionParamsS(param1: Int, param2: Int)(env: SymbolKindInferenceEnvironment[T]) =
      if(!env.kindParamForest.containsTerm(param1) && !env.kindParamForest.containsTerm(param2))
        env.kindParamForest.unionParams(param1, param2).map {
          case (kpf, isChanged) =>
            kpf.findRootParam(param1).map {
              rp =>
                val newIrreplaceableKindParams = if(param1 =/= param2) {
                  val definedKindTerms = env.irreplaceableKindParams.get(param1).map { _.list }.getOrElse(Nil) ++ env.irreplaceableKindParams.get(param2).map { _.list }.getOrElse(Nil)
                  IntMap() ++ (env.irreplaceableKindParams ++ definedKindTerms.toNel.map { rp -> _ })
                } else
                  env.irreplaceableKindParams
                (env.withKindParamForest(kpf).copy(irreplaceableKindParams = newIrreplaceableKindParams), isChanged.success)
            }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter", none, NoPosition)).failure))
        }.getOrElse((env, NoKind.fromError(FatalError("not found one kind parameter or two kind parameters", none, NoPosition)).failure))
      else
        (env, NoKind.fromError(FatalError("one kind parameter or two kind parameters are already replaced", none, NoPosition)).failure)
      
    override def allocateParamS(env: SymbolKindInferenceEnvironment[T]) =
      env.kindParamForest.allocateParam.map { 
        case (kpf, p) => (env.withKindParamForest(kpf), p.success)
      }.getOrElse((env, NoKind.fromError(FatalError("can't allocate kind parameter", none, NoPosition)).failure))

    override def replaceTermParamsS(term: KindTerm[StarKindTerm[Int]])(f: (Int, SymbolKindInferenceEnvironment[T]) => (SymbolKindInferenceEnvironment[T], Validation[NoKind, Either[Int, KindTerm[StarKindTerm[Int]]]]))(env: SymbolKindInferenceEnvironment[T]) =
      replaceKindTermParamsS(term)(f)(env)

    override def mismatchedTermErrorS(env: SymbolKindInferenceEnvironment[T]) = {
      val (s1, s2) = env.currentKindTermPair.map {
        case (t1, t2) => (intKindTermShowing.stringFrom(t1), intKindTermShowing.stringFrom(t2))
      }.getOrElse(("<unknown kind>", "<unknown kind>"))
      (env, NoKind.fromError(Error("couldn't match kind " + s1 + " with kind " + s2, none, NoPosition)))
    }
      
    override def prepareToUnificationS(env: SymbolKindInferenceEnvironment[T]) = (env, ())
    
    override def checkMatchingS(env: SymbolKindInferenceEnvironment[T]) = (env, false.success)
    
    override def withSaveS[U, V](f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], Validation[U, V]))(env: SymbolKindInferenceEnvironment[T]): (SymbolKindInferenceEnvironment[T], Validation[U, V]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }
    }
  }
  
  implicit def symbolTypeSimpleTermKindInferrer[T]: Inferrer[TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[T]], SymbolKindInferenceEnvironment[T], Kind] = new Inferrer[TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[T]], SymbolKindInferenceEnvironment[T], Kind] {
    override def inferSimpleTermInfoS(simpleTerm: TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[T]])(env: SymbolKindInferenceEnvironment[T]) =
      simpleTerm match {
        case TypeLambda(args, body, lmbdindexer.TypeLambdaInfo(_, lambdaIdx)) =>
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
          res.map { p => unifyInfosS(InferringKind(p._2), info)(env3.withDefinedKind(p._2)) }.valueOr { (env3, _) }
      }
    
    override def unifyInfosS(info1: Kind, info2: Kind)(env: SymbolKindInferenceEnvironment[T]) = {
      val (env2, res1) = info1.instantiatedKindTermS(env)
      val (env3, res2) = info2.instantiatedKindTermS(env2)
      (res1 |@| res2) { (kt1, kt2) => env3.withKindTermPair(some((kt1, kt2)))(unifyKindsS(info1, info2)) }.valueOr { (env3, _) }
    }
    
    override def unifyArgInfosS(funArgInfo: Kind, argInfo: Kind)(env: SymbolKindInferenceEnvironment[T]) =
      unifyInfosS(funArgInfo, argInfo)(env)
      
    override def argInfosFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment[T]) =
      argKindsFromKindS(info, argCount)(env)
      
    override def returnInfoFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment[T]) =
      returnKindFromKindS(info, argCount)(env)
    
    override def isNoInfo(info: Kind) =
      info.isNoKind

    override def functionInfo(argCount: Int) =
      functionKind(argCount)
      
    override def concatErrors(info1: Kind, info2: Kind): Kind =
      (info1, info2) match {
        case (noKind1: NoKind, noKind2: NoKind) => noKind1 |+| noKind2
        case (noKind: NoKind, _)                => noKind
        case (_, noKind: NoKind)                => noKind
        case _                                  => NoKind.fromError(FatalError("can't concat errors", none, NoPosition))
      } 

    override def unequalListLengthNoInfo =
      NoKind.fromError(FatalError("unequal list lengths", none, NoPosition))

    override def withPos(res: (SymbolKindInferenceEnvironment[T], Kind))(pos: Position) =
      (res._1, res._2.withPos(pos))
  }
  
  implicit def symbolTypeCombinatorKindRecursiveInitializer[T](implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], SymbolKindInferenceEnvironment[T], Int]): RecursiveInitializer[NoKind, GlobalSymbol, AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]], TypeCombinatorNode[Symbol, T, GlobalSymbol], SymbolKindInferenceEnvironment[T]] = new RecursiveInitializer[NoKind, GlobalSymbol, AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]], TypeCombinatorNode[Symbol, T, GlobalSymbol], SymbolKindInferenceEnvironment[T]] {
    override def combinatorFromNode(node: TypeCombinatorNode[Symbol, T, GlobalSymbol]) = node.comb
    
    override def recursiveCombinatorsFromNode(node: TypeCombinatorNode[Symbol, T, GlobalSymbol]) = node.recursiveCombSyms
    
    override def markedRecursiveCombinatorsFromNode(node: TypeCombinatorNode[Symbol, T, GlobalSymbol]) = node.markedRecCombSyms
    
    override def createNode(comb: AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]], recursiveCombLocs: Set[GlobalSymbol], markedRecCombLocs: Set[GlobalSymbol]) =
      TypeCombinatorNode(comb, recursiveCombLocs, markedRecCombLocs)
    
    override def addNodeS(loc: GlobalSymbol, node: TypeCombinatorNode[Symbol, T, GlobalSymbol])(env: SymbolKindInferenceEnvironment[T]) =
      (env.withTypeComb(loc, node), ())
      
    override def isRecursiveFromEnvironmentS(env: SymbolKindInferenceEnvironment[T]) = (env, env.isRecursive)
    
    override def isUninitializedGlobalVarS(loc: GlobalSymbol)(env: SymbolKindInferenceEnvironment[T]) = (env, env.typeVarKind(loc).isUninferredKind)
    
    private def instantiateKindsFromGlobalVarsS(syms: Set[GlobalSymbol])(env: SymbolKindInferenceEnvironment[T]) = {
      val (env2, res) = instantiateKindMapS(syms.map { s => (s, env.typeVarKind(s)) }.toMap)(env)
      res.map {
        ks =>
          val (env3, res2) = syms.flatMap { s => env2.localKindTables.get(some(s)).map { (s, _) } }.foldLeft((env2, Map[Option[GlobalSymbol], Map[Int, KindTable[LocalSymbol]]]().success[NoKind])) {
            case ((newEnv, Success(ktMaps)), (s, kts)) =>
              kts.foldLeft((newEnv, Map[Int, KindTable[LocalSymbol]]().success[NoKind])) {
                case ((newEnv2, Success(kts)), (i, kt)) =>
                  instantiateKindMapS(kt.kinds)(newEnv2).mapElements(identity, _.map { ks => kts + (i -> KindTable(ks)) })
                case ((newEnv2, Failure(nk)), _)        =>
                  (newEnv2, nk.failure)
              }.mapElements(identity, _.map { kts2 => ktMaps + (some(s) -> kts2) })
            case ((newEnv, Failure(nk)), _)            =>
              (newEnv, nk.failure)
          }
          res2.map {
            kts => (env3.withGlobalTypeVarKinds(ks).withLocalKindTables(env3.localKindTables ++ kts), ().success)
          }.valueOr { nk => (env3, nk.failure) }
      }.valueOr { nk => (env2, nk.failure) }
    }

    private def failInitializationS(noKind: NoKind, syms: Set[GlobalSymbol])(env: SymbolKindInferenceEnvironment[T]) =
      if(noKind.errs.forall { _.isInstanceOf[Error] })
        (env.withErrs(noKind).withGlobalTypeVarKinds(syms.map { s => (s, NoKind.fromError(Error("uninferred kind of global type variable " + s, none, NoPosition))) }.toMap), ().success[NoKind])
      else
        (env, noKind.failure)
    
    override def nonRecursivelyInitializeGlobalVarS(loc: GlobalSymbol, comb: AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]])(env: SymbolKindInferenceEnvironment[T]) =
      comb match {
        case typeComb @ TypeCombinator(kind, args, body, lmbdindexer.TypeLambdaInfo(_, lambdaIdx), file) =>
          (for {
            // Infers the kind.
            tmpTypeCombKind <- State((_: SymbolKindInferenceEnvironment[T]).withTypeCombSym(some(loc)) {
              _.withTypeLambdaIdx(lambdaIdx) {
                _.withLocalTypeVarKinds(args.flatMap { a => a.name.map { s => (LocalSymbol(s), a.kind) } }.toMap) {
                  newEnv =>
                    val (newEnv2, retKind) = inferS(body)(newEnv)
                    val argKinds = args.map { a => a.name.map { s => newEnv2.typeVarKind(LocalSymbol(s)) }.getOrElse(InferredKind(Star(KindParam(0), NoPosition))) }
                    functionKindFromKindsS(argKinds, retKind)(newEnv2)
                  }
               }
            })
            // Unifies the inferred kind with the defined kind.
            tmpTypeCombKind2 <- kind.map {
              kt =>
                for {
                  res <- allocateKindTermParams(kt)(Map())
                  tmpKind2 <- res.map {
                    case (_, kt2) => 
                      for {
                        _ <- State((env2: SymbolKindInferenceEnvironment[T]) => (env2.withDefinedKind(kt2), ()))
                        tmpKind <- State(symbolTypeSimpleTermKindInferrer.unifyInfosS(tmpTypeCombKind, InferringKind(kt2))(_: SymbolKindInferenceEnvironment[T]))
                      } yield tmpKind
                  }.valueOr { nk => State((_: SymbolKindInferenceEnvironment[T], nk)) }
                } yield tmpKind2
            }.getOrElse(State((_: SymbolKindInferenceEnvironment[T], tmpTypeCombKind)))
            // Checks the defined kinds.
            isRecursive <- State(isRecursiveFromEnvironmentS)
            res2 <- if(!isRecursive)
              for {
                definedKindTerms <- State((env2: SymbolKindInferenceEnvironment[T]) => (env2, env2.definedKindTerms))
                res <- checkDefinedKindTerms(definedKindTerms)
              } yield res
            else
              State((_: SymbolKindInferenceEnvironment[T], ().success))
            // Instantiates the inferred kinds.
            res4 <- res2.map {
              _ =>
                tmpTypeCombKind2 match {
                  case noKind: NoKind =>
                    State(failInitializationS(noKind, Set(loc)))
                  case _              =>
                    for {
                      _ <- State((env2: SymbolKindInferenceEnvironment[T]) => (env2.withGlobalTypeVarKind(loc, tmpTypeCombKind2), ()))
                      res3 <- if(!isRecursive)
                        State(instantiateKindsFromGlobalVarsS(Set(loc)))
                      else
                        State((_: SymbolKindInferenceEnvironment[T], ().success))
                    } yield res3
                }
            }.valueOr { nk => State(failInitializationS(nk, Set(loc))) }
          } yield res4).run(env)
        case UnittypeCombinator(n, kind, file) =>
          val tmpUnittypeCombKind = InferredKind.unittypeCombinatorKind(n)
          (for {
            tmpUnittypeCombKind2 <- kind.map {
              case kt =>
                for {
                  res <- allocateKindTermParams(kt)(Map())
                  tmpKind2 <- res.map {
                    case (_, kt2) =>
                      for {
                        _ <- State((env2: SymbolKindInferenceEnvironment[T]) => (env2.withDefinedKind(kt2), ()))
                        tmpKind <- State(symbolTypeSimpleTermKindInferrer.unifyInfosS(tmpUnittypeCombKind, InferringKind(kt2))(_: SymbolKindInferenceEnvironment[T]))
                      } yield tmpKind
                  }.valueOr { nk => State((_: SymbolKindInferenceEnvironment[T], nk)) }
                } yield tmpKind2
            }.getOrElse(State((_: SymbolKindInferenceEnvironment[T], tmpUnittypeCombKind)))
            definedKindTerms <- State((env2: SymbolKindInferenceEnvironment[T]) => (env2, env2.definedKindTerms))
            res <- checkDefinedKindTerms(definedKindTerms)
            res3 <- res.map {
              _ =>
                for {
                  unittypeCombKind <- State(tmpUnittypeCombKind2.instantiatedKindS(_: SymbolKindInferenceEnvironment[T]))
                  res2 <- unittypeCombKind match {
                    case noKind: NoKind =>
                      State((_: SymbolKindInferenceEnvironment[T], noKind.failure))
                    case _              =>
                      State((env2: SymbolKindInferenceEnvironment[T]) => ((env2.withGlobalTypeVarKind(loc, unittypeCombKind), ().success)))
                  }
                } yield res2
            }.valueOr { nk => State(failInitializationS(nk, Set(loc))) }
          } yield res3).run(env)
      }
    
    override def checkInitializationS(res: Validation[NoKind, Unit], combLocs: Set[GlobalSymbol], oldNodes: Map[GlobalSymbol, TypeCombinatorNode[Symbol, T, GlobalSymbol]])(env: SymbolKindInferenceEnvironment[T]) = {
      val (env2, res2) = checkDefinedKindTermsS(env.definedKindTerms)(env)
      (res |@| res2) {
        (_, _) => instantiateKindsFromGlobalVarsS(oldNodes.keySet)(env2)
      }.valueOr { failInitializationS(_, combLocs)(env2) }
    }
    
    override def nodesFromEnvironmentS(env: SymbolKindInferenceEnvironment[T]) = (env, env.typeCombNodes)
    
    override def withRecursiveS[U](combLocs: Set[GlobalSymbol], newNodes: Map[GlobalSymbol, TypeCombinatorNode[Symbol, T, GlobalSymbol]])(f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], U))(env: SymbolKindInferenceEnvironment[T]) = {
      val (env2, res) = f(env.withRecursive(true).withoutGlobalTypeVarKinds(combLocs))
      (env2.withRecursive(false).withTypeCombNodes(newNodes), res)
    }
    
    override def withClearS[U](f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], U))(env: SymbolKindInferenceEnvironment[T]) =
      env.withClear(f)
  }
  
  implicit def symbolTypeCombinatorKindInitializer[T]: Initializer[NoKind, GlobalSymbol, AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]], SymbolKindInferenceEnvironment[T]] = new Initializer[NoKind, GlobalSymbol, AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]], SymbolKindInferenceEnvironment[T]] {
    override def globalVarsFromEnvironmentS(env: SymbolKindInferenceEnvironment[T]) = (env, env.globalTypeVarKinds.keySet)
    
    override def usedGlobalVarsFromCombinator(comb: AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]]) =
      comb match {
        case TypeCombinator(_, _, body, _, _) => usedGlobalTypeVarsFromTypeTerm(body)
        case UnittypeCombinator(_, _, _)      => Set()
      }
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolKindInferenceEnvironment[T]) =
      if(!env.isRecursive) {
        (env.withGlobalTypeVarKind(loc, UninferredKind), ())
      } else {
        val (env2, res) = allocateKindTermParamsS(Star(KindParam(0), NoPosition))(Map())(env)
        res.map { p => (env2.withGlobalTypeVarKind(loc, InferringKind(p._2)), ()) }.valueOr { nk => (env2.withGlobalTypeVarKind(loc, nk), ()) }
      }
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo[T]])(env: SymbolKindInferenceEnvironment[T]) = {
      val (env2, res) = recursivelyInitializeGlobalVarS(loc, comb)(resolver.TypeTreeInfo)(env)
      (env2, res.swap.map { _.forFile(comb.file) }.swap)
    }
    
    override def checkEnvironmentS(env: SymbolKindInferenceEnvironment[T]) =
      (env, env.errNoKind.map { _.failure }.getOrElse(().success))

    override def undefinedGlobalVarError =
      NoKind.fromError(FatalError("undefined global type variable", none, NoPosition))

    override def withSaveS[U, V](f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], Validation[U, V]))(env: SymbolKindInferenceEnvironment[T]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }
    }
  }
  
  implicit def symbolKindInferenceEnvironmental[T]: KindInferenceEnvironmental[SymbolKindInferenceEnvironment[T], GlobalSymbol, LocalSymbol] = new KindInferenceEnvironmental[SymbolKindInferenceEnvironment[T], GlobalSymbol, LocalSymbol] {
    override def copyEnvironment(env: SymbolKindInferenceEnvironment[T]) = env
  
    override def globalTypeVarKindFromEnvironment(env: SymbolKindInferenceEnvironment[T])(sym: GlobalSymbol) =
      env.typeVarKind(sym)
 
    override def localKindTablesFromEnvironment(env: SymbolKindInferenceEnvironment[T])(sym: Option[GlobalSymbol]): Map[Int, KindTable[LocalSymbol]] =
      env.localKindTables.getOrElse(sym, Map())
      
    override def globalKindTableFromEnvironment(env: SymbolKindInferenceEnvironment[T]) =
      KindTable(env.globalTypeVarKinds)
      
    override def withCurrentTypeCombinatorLocation(env: SymbolKindInferenceEnvironment[T])(loc: Option[GlobalSymbol]) =
      env.withCurrentTypeCombSym(loc)
  
    override def getLocalKindTableFromEnvironment(env: SymbolKindInferenceEnvironment[T])(lambdaIdx: Int) =
      env.localKindTables.getOrElse(env.currentTypeCombSym, Map()).get(lambdaIdx)
  }
  
  implicit def symbolKindInferenceEnvironmentState[T]: KindInferenceEnvironmentState[SymbolKindInferenceEnvironment[T], GlobalSymbol] = new KindInferenceEnvironmentState[SymbolKindInferenceEnvironment[T], GlobalSymbol] {
    override def instantiateLocalKindTablesS(env: SymbolKindInferenceEnvironment[T]) = {
      val (env2, res) = env.localKindTables.getOrElse(env.currentTypeCombSym, Map()).foldLeft((env, Map[Int, KindTable[LocalSymbol]]().success[NoKind])) {
        case ((newEnv, Success(kts)), (i, kt)) =>
          instantiateKindMapS(kt.kinds)(newEnv).mapElements(identity, _.map { ks => kts + (i -> KindTable(ks)) })
        case ((newEnv, Failure(nk)), _)        =>
          (newEnv, nk.failure)
      }
      res.map {
        kts => (env2.copy(localKindTables = env2.localKindTables + (env2.currentTypeCombSym -> kts)), ().success)
      }.valueOr { nk => (env2, nk.failure) }
    }
  
    override def instantiateKindS(kind: Kind)(env: SymbolKindInferenceEnvironment[T]) = kind.instantiatedKindS(env)
  
    override def withTypeCombinatorLocationS[U](loc: Option[GlobalSymbol])(f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], U))(env: SymbolKindInferenceEnvironment[T]): (SymbolKindInferenceEnvironment[T], U) = {
      val oldLoc = env.currentTypeCombSym
      val (env2, res) = f(env.withCurrentTypeCombSym(loc))
      (env2.withCurrentTypeCombSym(oldLoc), res)
    }
  
    override def withClearS[U](f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], U))(env: SymbolKindInferenceEnvironment[T]): (SymbolKindInferenceEnvironment[T], U) =
      env.withClear(f)
  }
  
  implicit val resolverTreeInfoTransformer: TreeInfoTransformer[resolver.TreeInfo, GlobalSymbol, LocalSymbol] = new TreeInfoTransformer[resolver.TreeInfo, GlobalSymbol, LocalSymbol] {
    override def transformTreeInfo[T, U, E](treeInfo: resolver.TreeInfo[lmbdindexer.TypeLambdaInfo[T], U])(env: E)(implicit enval: KindInferenceEnvironmental[E, GlobalSymbol, LocalSymbol]): ValidationNel[AbstractError, resolver.TreeInfo[TypeLambdaInfo[T, LocalSymbol], TypeTreeInfo[U, GlobalSymbol]]] =
      Kinder.transformTypeTree(treeInfo.typeTree)(env)(enval).map { resolver.TreeInfo(_) }
  }  
}