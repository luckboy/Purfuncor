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
  
  implicit val symbolTypeSimpleTermKindInferrer: Inferrer[TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo], SymbolKindInferenceEnvironment, Kind] = new Inferrer[TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo], SymbolKindInferenceEnvironment, Kind] {
    override def inferSimpleTermInfoS(simpleTerm: TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo])(env: SymbolKindInferenceEnvironment) =
      simpleTerm match {
        case TypeLambda(args, body, lmbdindexer.TypeLambdaInfo(lambdaIdx)) =>
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

    override def unequalListLengthNoInfo =
      NoKind.fromError(FatalError("unequal list lengths", none, NoPosition))

    override def withPos(res: (SymbolKindInferenceEnvironment, Kind))(pos: Position) =
      (res._1, res._2.withPos(pos))
  }
  
  implicit val symbolTypeCombinatorKindInitializer: Initializer[NoKind, GlobalSymbol, AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo], SymbolKindInferenceEnvironment] = new Initializer[NoKind, GlobalSymbol, AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo], SymbolKindInferenceEnvironment] {
    override def globalVarsFromEnvironmentS(env: SymbolKindInferenceEnvironment) = (env, env.globalTypeVarKinds.keySet)
    
    override def usedGlobalVarsFromCombinator(comb: AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo]) =
      comb match {
        case TypeCombinator(_, _, body, _, _) => usedGlobalTypeVarsFromTypeTerm(body)
        case UnittypeCombinator(_, _, _)      => Set()
      }
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolKindInferenceEnvironment) =
      if(!env.isRecursive) {
        (env.withGlobalTypeVarKind(loc, UninferredKind), ())
      } else {
        val (env2, res) = allocateKindTermParamsS(Star(KindType, NoPosition))(Map())(env)
        res.map { p => (env2.withGlobalTypeVarKind(loc, InferringKind(p._2)), ()) }.valueOr { nk => (env2.withGlobalTypeVarKind(loc, nk), ()) }
      }
    
    private def instantiateKindsFromGlobalVarsS(kinds: Map[GlobalSymbol, Kind])(env: SymbolKindInferenceEnvironment) = {
      val (env2, res) = instantiateKindMapS(kinds.keys.map { s => (s, env.typeVarKind(s)) }.toMap)(env)
      res.map {
        ks =>
         val (env3, res2) = kinds.keys.flatMap { s => env2.localKindTables.get(some(s)).map { (s, _) } }.foldLeft((env2, Map[Option[GlobalSymbol], Map[Int, KindTable[LocalSymbol]]]().success[NoKind])) {
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
    
    private def failInitializationS(noKind: NoKind, syms: Set[GlobalSymbol])(env: SymbolKindInferenceEnvironment) =
      if(noKind.errs.forall { _.isInstanceOf[Error] })
        (env.withErrs(noKind).withGlobalTypeVarKinds(syms.map { s => (s, NoKind.fromError(Error("uninferred kind of global type variable " + s, none, NoPosition))) }.toMap), ().success[NoKind])
      else
        (env, noKind.failure)
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractTypeCombinator[Symbol, lmbdindexer.TypeLambdaInfo])(env: SymbolKindInferenceEnvironment) =
      env.withClear {
        env2 =>
          val (env10, res) = comb match {
            case typeComb @ TypeCombinator(kind, args, body, lmbdindexer.TypeLambdaInfo(lambdaIdx), file) =>
              val depSyms = usedGlobalTypeVarsFromTypeTerm(body).filter { env2.typeVarKind(_).isUninferredKind }
              if(depSyms.isEmpty) {
                // Infers the kind. 
                val (env3, tmpTypeCombKind) = env2.withTypeLambdaIdx(lambdaIdx) {
                  _.withLocalTypeVarKinds(args.flatMap { a => a.name.map { s => (LocalSymbol(s), a.kind) } }.toMap) {
                    newEnv =>
                      val (newEnv2, retKind) = inferS(body)(newEnv)
                      val argKinds = args.map { a => a.name.map { s => newEnv2.typeVarKind(LocalSymbol(s)) }.getOrElse(InferredKind(Star(KindParam(0), NoPosition))) }
                      functionKindFromKindsS(argKinds, retKind)(newEnv2)
                  }
                }
                // Unifies the inferred kind with the defined kind.
                val (env5, tmpTypeCombKind2) = kind.map {
                  kt =>
                    val (env4, res) = allocateKindTermParamsS(kt)(Map())(env3)
                    res.map { 
                      case (_, kt2) => symbolTypeSimpleTermKindInferrer.unifyInfosS(tmpTypeCombKind, InferringKind(kt2))(env4.withDefinedKind(kt2))
                    }.valueOr { (env4, _) }
                }.getOrElse((env3, tmpTypeCombKind))
                // Checks the defined kinds.
                val (env6, res) = if(env5.isRecursive) checkDefinedKindTermsS(env5.definedKindTerms)(env5) else (env5, ().success)
                // Instantiates the inferred kinds.
                res.map {
                  _ =>
                    tmpTypeCombKind2 match {
                      case noKind: NoKind =>
                        failInitializationS(noKind, Set(loc))(env6)
                      case _              =>
                        if(!env6.isRecursive)
                          instantiateKindsFromGlobalVarsS(Map(loc -> tmpTypeCombKind2))(env6)
                        else
                          (env6.withGlobalTypeVarKind(loc, tmpTypeCombKind2), ().success)
                    }
                }.valueOr { failInitializationS(_, Set(loc))(env6) }
              } else {
                val (nonRecursiveDepSyms, recusiveDepSyms) = depSyms.partition(env.typeCombNodes.contains)
                val recursiveTypeCombSyms = recusiveDepSyms ++ nonRecursiveDepSyms.flatMap { env.typeCombNodes.get(_).toSet.flatMap { _.recursiveCombSyms } }
                val markedRecTypeCombSyms = (recursiveTypeCombSyms & Set(loc)) ++ nonRecursiveDepSyms.flatMap { env.typeCombNodes.get(_).toSet.flatMap { _.markedRecCombSyms } }
                if((recursiveTypeCombSyms &~ markedRecTypeCombSyms).isEmpty) {
                  val combs = Map(loc -> comb) ++ env.typeCombNodes.flatMap {
                    case (s, n) => if(!(n.recursiveCombSyms & recursiveTypeCombSyms).isEmpty) some(s -> n.comb) else Map()
                  }
                  val (newTypeCombNodes, oldTypeCombNodes) = env.typeCombNodes.partition { case (_, n) => recursiveTypeCombSyms.subsetOf(n.recursiveCombSyms) }
                  // Infers the kinds of the type combinators of the recursive types.
                  val (env3, res) = initializeS(Tree(combs, resolver.TypeTreeInfo))(env.withRecursive(true)).mapElements(_.withRecursive(false).withTypeCombNodes(newTypeCombNodes), identity)
                  // Checks the defined kinds.
                  val (env4, res2) = checkDefinedKindTermsS(env3.definedKindTerms)(env3)
                  // Instantiates the inferred kinds.
                  (res |@| res2) {
                    (_, _) => instantiateKindsFromGlobalVarsS(oldTypeCombNodes.keys.map { s => (s, env4.typeVarKind(s)) }.toMap)(env4)
                  }.valueOr { failInitializationS(_, combs.keySet)(env4) }
                } else
                  (env.withTypeComb(loc, TypeCombinatorNode(typeComb, recursiveTypeCombSyms, markedRecTypeCombSyms)), ().success)
              }
            case UnittypeCombinator(n, kind, file) =>
              val tmpUnittypeCombKind = InferredKind.unittypeCombinatorKind(n)
              val (env4, tmpUnittypeCombKind2) = kind.map {
                kt =>
                  val (env3, res) = allocateKindTermParamsS(kt)(Map())(env2)
                  res.map { 
                    case (_, kt2) => symbolTypeSimpleTermKindInferrer.unifyInfosS(tmpUnittypeCombKind, InferringKind(kt2))(env3.withDefinedKind(kt2))
                  }.valueOr { (env3, _) }
              }.getOrElse((env, tmpUnittypeCombKind))
              val (env5, res) = checkDefinedKindTermsS(env4.definedKindTerms)(env4)
              res.map {
                _ =>
                  val (env6, unittypeCombKind) = tmpUnittypeCombKind2.instantiatedKindS(env5)
                  unittypeCombKind match {
                    case noKind: NoKind => (env6, noKind.failure)
                    case _              => (env6.withGlobalTypeVarKind(loc, unittypeCombKind), ().success)
                  }
              }.valueOr { failInitializationS(_, Set(loc))(env5) }
          }
          (env10, res.swap.map { _.forFile(comb.file) }.swap)
      }
    
    override def checkEnvironmentS(env: SymbolKindInferenceEnvironment) =
      (env, env.errNoKind.map { _.failure }.getOrElse(().success))

    override def undefinedGlobalVarError =
      NoKind.fromError(FatalError("undefined global type variable", none, NoPosition))

    override def withSaveS[T, U](f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, Validation[T, U]))(env: SymbolKindInferenceEnvironment) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }
    }
  }
  
  implicit val symbolKindInferenceEnvironmental: KindInferenceEnvironmental[SymbolKindInferenceEnvironment, GlobalSymbol, LocalSymbol] = new KindInferenceEnvironmental[SymbolKindInferenceEnvironment, GlobalSymbol, LocalSymbol] {
    override def copyEnvironment(env: SymbolKindInferenceEnvironment) = env
  
    override def globalKindTableFromEnvironment(env: SymbolKindInferenceEnvironment) =
      KindTable(env.globalTypeVarKinds)
  
    override def withCurrentTypeCombinatorLocation(env: SymbolKindInferenceEnvironment)(loc: Option[GlobalSymbol]) =
      env.withCurrentTypeCombSym(loc)
  
    override def getLocalKindTableFromEnvironment(env: SymbolKindInferenceEnvironment)(lambdaIdx: Int) =
      env.localKindTables.getOrElse(env.currentTypeCombSym, Map()).get(lambdaIdx)
  }
  
  implicit val symbolKindInferenceEnvironmentState: KindInferenceEnvironmentState[SymbolKindInferenceEnvironment, GlobalSymbol] = new KindInferenceEnvironmentState[SymbolKindInferenceEnvironment, GlobalSymbol] {
    override def instantiateLocalKindTablesS(env: SymbolKindInferenceEnvironment) = {
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
  
    override def instantiateKindS(kind: Kind)(env: SymbolKindInferenceEnvironment) = kind.instantiatedKindS(env)
  
    override def withTypeCombinatorLocationS[T](loc: Option[GlobalSymbol])(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, T))(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, T) = {
      val oldLoc = env.currentTypeCombSym
      val (env2, res) = f(env.withCurrentTypeCombSym(loc))
      (env2.withCurrentTypeCombSym(oldLoc), res)
    }
  
    override def withClearS[T](f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, T))(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, T) = {
      val oldLocalKindTables = env.localKindTables.getOrElse(none, Map())
      val (env2, res) = f(env.copy(localKindTables = env.localKindTables + (none -> Map())))
      (env2.copy(localKindTables = env.localKindTables + (none -> oldLocalKindTables)), res)
    }
  }
  
  implicit val resolverTreeInfoTransformer: TreeInfoTransformer[resolver.TreeInfo, GlobalSymbol, LocalSymbol] = new TreeInfoTransformer[resolver.TreeInfo, GlobalSymbol, LocalSymbol] {
    override def transformTreeInfo[E](treeInfo: resolver.TreeInfo[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo])(env: E)(implicit enval: KindInferenceEnvironmental[E, GlobalSymbol, LocalSymbol]): ValidationNel[AbstractError, resolver.TreeInfo[TypeLambdaInfo[LocalSymbol], TypeTreeInfo[GlobalSymbol]]] =
      Kinder.transformTypeTree(treeInfo.typeTree)(env)(enval).map { resolver.TreeInfo(_) }
  }
  
  implicit val resolverTreeInfoExtractor: TreeInfoExtractor[resolver.TreeInfo[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo], Symbol, GlobalSymbol] = new TreeInfoExtractor[resolver.TreeInfo[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo], Symbol, GlobalSymbol] {
    override def typeTreeFromTreeInfo(treeInfo: resolver.TreeInfo[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo]) = treeInfo.typeTree
  }
}