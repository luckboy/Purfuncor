package pl.luckboy.purfuncor.common
import scalaz._
import scalaz.Scalaz._

trait Inferrer[-T, E, I]
{
  def inferSimpleTermInfoS(simpleTerm: T)(env: E): (E, I)
  
  def unifyTwoInfosS(info1: I, info2: I)(env: E): (E, I)
  
  def argInfosFromInfo(info: I, argCount: Int)(env: E): Validation[I, Seq[I]]
  
  def returnInfoFromInfo(info: I, argCount: Int)(env: E): I
  
  def isNoInfo(info: I): Boolean 
}

object Inferrer
{
  def inferS[T, E, I](term: Term[T])(env: E)(implicit inferrer: Inferrer[T, E, I]): (E, I) =
    term match {
      case App(fun, args, _)     =>
        val (env2, funInfo) = inferS(fun)(env)
        inferTermInfosS(args.list)(env2) match {
          case (env3, Success(argInfos))  => appInfoS(funInfo, argInfos)(env3)
          case (env3, Failure(noInfo))    => (env3, noInfo)
        }
        throw new UnsupportedOperationException
      case Simple(simpleTerm, _) =>
        inferrer.inferSimpleTermInfoS(simpleTerm)(env)
    }
    
  def infer[T, E, I](term: Term[T])(implicit inferrer: Inferrer[T, E, I]) =
    State(inferS[T, E, I](term))
    
  def appInfoS[T, E, I](funInfo: I, argInfos: Seq[I])(env: E)(implicit inferrer: Inferrer[T, E, I]) =
    inferrer.argInfosFromInfo(funInfo, argInfos.size)(env) match {
      case Success(funArgInfos) =>
        unifyTwoInfoListsS(funArgInfos.toList, argInfos.toList)(env) match {
          case (env2, Success(_))      => (env2, inferrer.returnInfoFromInfo(funInfo, argInfos.size)(env2))
          case (env2, Failure(noInfo)) => (env2, noInfo)
        }
      case Failure(noInfo)      =>
        (env, noInfo)
    }
  
  def appInfo[T, E, I](funInfo: I, argInfos: Seq[I])(implicit inferrer: Inferrer[T, E, I]) =
    State(appInfoS[T, E, I](funInfo, argInfos))
  
  def unifyTwoInfoListsS[T, E, I](infos1: List[I], infos2: List[I])(env: E)(implicit inferrer: Inferrer[T, E, I]) =
    infos1.zip(infos2).foldLeft((env, Seq[I]().success[I])) {
      case ((newEnv, Success(unifiedInfos)), (info1, info2)) =>
        val (newEnv2, unifiedInfo) = inferrer.unifyTwoInfosS(info1, info2)(newEnv)
        (newEnv2, if(!inferrer.isNoInfo(unifiedInfo)) (unifiedInfos :+ unifiedInfo).success else unifiedInfo.failure)
      case ((newEnv, Failure(noInfo)), _)                    =>
        (newEnv, noInfo.failure)
    }

  def unifyTwoInfoLists[T, E, I](infos1: List[I], infos2: List[I])(implicit inferrer: Inferrer[T, E, I]) =
    State(unifyTwoInfoListsS[T, E, I](infos1, infos2))
    
  def inferTermInfosS[T, E, I](terms: List[Term[T]])(env: E)(implicit inferrer: Inferrer[T, E, I]): (E, Validation[I, Seq[I]]) =
    terms.foldLeft((env, Seq[I]().success[I])) {
      case ((newEnv, Success(infos)), term) =>
        val (newEnv2, info) = inferS(term)(newEnv)
        (newEnv2, if(!inferrer.isNoInfo(info)) (infos :+ info).success else info.failure)
      case ((newEnv, Failure(noInfo)), _)   =>
        (newEnv, noInfo.failure)
    }

  def inferTtermInfos[T, E, I](terms: List[Term[T]])(implicit inferrer: Inferrer[T, E, I]) =
    State(inferTermInfosS[T, E, I](terms))
}