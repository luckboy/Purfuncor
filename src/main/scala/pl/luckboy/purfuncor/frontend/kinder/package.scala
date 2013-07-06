package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol

package object kinder
{
  implicit def symbolKindInferrer[T] = new Inferrer[TypeSimpleTerm[Symbol, T], SymbolKindInferenceEnvironment, Kind] {
    override def inferSimpleTermInfoS(simpleTerm: TypeSimpleTerm[Symbol, T])(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Kind) =
      throw new UnsupportedOperationException
    
    override def unifyInfosS(info1: Kind, info2: Kind)(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Kind) =
      throw new UnsupportedOperationException
      
    override def argInfosFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Validation[Kind, Seq[Kind]]) =
      throw new UnsupportedOperationException
      
    override def returnInfoFromInfoS(info: Kind, argCount: Int)(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Kind) =
      throw new UnsupportedOperationException
    
    override def isNoInfo(info: Kind): Boolean =
      throw new UnsupportedOperationException

    override def functionInfo(argCount: Int): Kind =
      throw new UnsupportedOperationException
      
    override def concatErrors(info1: Kind, info2: Kind): Kind =
      throw new UnsupportedOperationException

    override def unequalListLengthNoInfo: Kind =
      throw new UnsupportedOperationException

    override def withPos(res: (SymbolKindInferenceEnvironment, Kind))(pos: Position): (SymbolKindInferenceEnvironment, Kind) =
      throw new UnsupportedOperationException
  }
  
  implicit val symbolKindUnifier = new Unifier[NoKind, KindTerm[StarKindTerm[Int]], SymbolKindInferenceEnvironment, Int] {
    override def matchesTermsS[U](term1: KindTerm[StarKindTerm[Int]], term2: KindTerm[StarKindTerm[Int]])(f: (Int, Either[Int, KindTerm[StarKindTerm[Int]]], U, SymbolKindInferenceEnvironment) => (SymbolKindInferenceEnvironment, Validation[NoKind, U]))(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Validation[NoKind, U]) =
      throw new UnsupportedOperationException
    
    override def getParamTermS(param: Int)(env: SymbolKindInferenceEnvironment) =
      (env, env.kindParamForest.getTerm(param).map { _.kindTerm })

    override def findRootParamS(param: Int)(env: SymbolKindInferenceEnvironment) =
      (env, env.kindParamForest.findRootParam(param).toSuccess(NoKind.fromError(FatalError("not found kind parameter", none, NoPosition))))
    
    override def replaceParamS(param: Int, term: KindTerm[StarKindTerm[Int]])(env: SymbolKindInferenceEnvironment) =
      if(!env.kindParamForest.containsTerm(param)) 
        env.kindParamForest.replaceParam(param, InferredParamTerm(term)).map {
          kpf => (env.withKindParamForest(kpf), ().success)
        }.getOrElse((env, NoKind.fromError(FatalError("not found kind parameter or kind parameter is replaced", none, NoPosition)).failure))
      else
        (env, NoKind.fromError(FatalError("kind parameter is already replaced", none, NoPosition)).failure)          

    override def unionParamsS(param1: Int, param2: Int)(env: SymbolKindInferenceEnvironment): (SymbolKindInferenceEnvironment, Validation[NoKind, Unit]) =
      if(!env.kindParamForest.containsTerm(param1) && !env.kindParamForest.containsTerm(param2))
        env.kindParamForest.unionParams(param1, param2).map {
          kpf => (env.withKindParamForest(kpf), ().success)
        }.getOrElse((env, NoKind.fromError(FatalError("not found one kind parameter or two kind parameters", none, NoPosition)).failure))
      else
        (env, NoKind.fromError(FatalError("one kind parameter or two kind parameters are already replaced", none, NoPosition)).failure)
      
    override def mismatchedTermError(env: SymbolKindInferenceEnvironment): NoKind =
      throw new UnsupportedOperationException
  }
}