package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

trait Inferrer[-T, E, I]
{
  def inferSimpleTermInfoS(simpleTerm: T)(env: E): (E, I)
  
  def unifyInfosS(info1: I, info2: I)(env: E): (E, I)
  
  def argInfosFromInfoS(info: I, argCount: Int)(env: E): (E, Validation[I, Seq[I]])
  
  def returnInfoFromInfoS(info: I, argCount: Int)(env: E): (E, I)
  
  def isNoInfo(info: I): Boolean
  
  def functionInfo(argCount: Int): I
  
  def concatErrors(info1: I, info2: I): I
  
  def unequalListLengthNoInfo: I
  
  def withPos(res: (E, I))(pos: Position): (E, I)
}

object Inferrer
{
  def inferS[T, E, I](term: Term[T])(env: E)(implicit inferrer: Inferrer[T, E, I]): (E, I) = {
    val res = term match {
      case App(fun, args, _)     =>
        val (env2, funInfo) = inferS(fun)(env)
        inferTermInfosS(args.list)(env2) match {
          case (env3, Success(argInfos))  => appInfoS(funInfo, argInfos)(env3)
          case (env3, Failure(noInfo))    => (env3, noInfo)
        }
      case Simple(simpleTerm, _) =>
        inferrer.inferSimpleTermInfoS(simpleTerm)(env)
    }
    inferrer.withPos(res)(term.pos)
  }
    
  def infer[T, E, I](term: Term[T])(implicit inferrer: Inferrer[T, E, I]) =
    State(inferS[T, E, I](term))
    
  def appInfoS[T, E, I](funInfo: I, argInfos: Seq[I])(env: E)(implicit inferrer: Inferrer[T, E, I]) = {
    val (env2, funInfo2) = inferrer.unifyInfosS(funInfo, inferrer.functionInfo(argInfos.size))(env)
    val (env3, res) = inferrer.argInfosFromInfoS(funInfo2, argInfos.size)(env2)
    res match {
      case Success(funArgInfos) =>
        unifyInfoListsS(funArgInfos.toList, argInfos.toList)(env3) match {
          case (env4, Success(_))      => inferrer.returnInfoFromInfoS(funInfo, argInfos.size)(env4)
          case (env4, Failure(noInfo)) => (env4, noInfo)
        }
      case Failure(noInfo)      =>
        (env3, noInfo)
    }
  }
  
  def appInfo[T, E, I](funInfo: I, argInfos: Seq[I])(implicit inferrer: Inferrer[T, E, I]) =
    State(appInfoS[T, E, I](funInfo, argInfos))
  
  def unifyInfoListsS[T, E, I](infos1: List[I], infos2: List[I])(env: E)(implicit inferrer: Inferrer[T, E, I]) =
    if(infos1.size === infos2.size)
      infos1.zip(infos2).foldLeft((env, Seq[I]().success[I])) {
        case ((newEnv, Success(unifiedInfos)), (info1, info2)) =>
          val (newEnv2, unifiedInfo) = inferrer.unifyInfosS(info1, info2)(newEnv)
          (newEnv2, if(!inferrer.isNoInfo(unifiedInfo)) (unifiedInfos :+ unifiedInfo).success else unifiedInfo.failure)
        case ((newEnv, Failure(noInfo)), (info1, info2))       =>
          val (newEnv2, unifiedInfo) = inferrer.unifyInfosS(info1, info2)(newEnv)
          (newEnv2, inferrer.concatErrors(noInfo, unifiedInfo).failure)
      }
    else
      (env, inferrer.unequalListLengthNoInfo.failure)

  def unifyInfoLists[T, E, I](infos1: List[I], infos2: List[I])(implicit inferrer: Inferrer[T, E, I]) =
    State(unifyInfoListsS[T, E, I](infos1, infos2))
    
  def inferTermInfosS[T, E, I](terms: List[Term[T]])(env: E)(implicit inferrer: Inferrer[T, E, I]): (E, Validation[I, Seq[I]]) =
    terms.foldLeft((env, Seq[I]().success[I])) {
      case ((newEnv, Success(infos)), term)  =>
        val (newEnv2, info) = inferS(term)(newEnv)
        (newEnv2, if(!inferrer.isNoInfo(info)) (infos :+ info).success else info.failure)
      case ((newEnv, Failure(noInfo)), term) =>
        val (newEnv2, info) = inferS(term)(newEnv)
        (newEnv, inferrer.concatErrors(noInfo, info).failure)
    }

  def inferTtermInfos[T, E, I](terms: List[Term[T]])(implicit inferrer: Inferrer[T, E, I]) =
    State(inferTermInfosS[T, E, I](terms))
}