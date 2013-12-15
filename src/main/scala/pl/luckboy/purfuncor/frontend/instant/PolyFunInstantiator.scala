package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait PolyFunInstantiator[L, M, E]
{
  def instantiatePolyFunctionS(lambdaInfo: PreinstantiationLambdaInfo[L, M], instArgs: Seq[InstanceArg[L, M]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(env: E): (E, ValidationNel[AbstractError, (InstantiationLambdaInfo[L, M], Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])])
  
  def getLambdaInfosFromEnvironmentS(loc: Option[L])(env: E): (E, Option[Map[Int, InstantiationLambdaInfo[L, M]]])
  
  def addLambdaInfosS(loc: Option[L], lambdaInfos: Map[Int, InstantiationLambdaInfo[L, M]])(env: E): (E, Unit)  
}

object PolyFunInstantiator
{
  def instantiatePolyFunctionsS[L, M, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, M]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, E]) = {
    val (env2, (res, localInstTree2)) = lambdaInfoMaps.foldLeft((env, (Map[Option[L], Map[Int, InstantiationLambdaInfo[L, M]]]().successNel[AbstractError], localInstTree))) {
      case ((newEnv, (newRes, newLocalInstTree)), (polyFun, lambdaInfos)) =>
        val (newEnv6, (newRes4, newLocalInstTree3)) = lambdaInfos.foldLeft((newEnv, (Map[Int, InstantiationLambdaInfo[L, M]]().successNel[AbstractError], newLocalInstTree))) {
          case ((newEnv2, (newRes2, newLocalInstTree2)), (i, lambdaInfo)) =>
            val (newEnv4, instArgs) = lambdaInfo.polyFun match {
              case Some(PolyFunction(polyFunLoc)) =>
                val (newEnv3, optLambdaInfos) = polyFunInstantiator.getLambdaInfosFromEnvironmentS(some(polyFunLoc))(newEnv2)
                (newEnv3, optLambdaInfos.flatMap { _.get(0).map { _.instArgs } }.getOrElse(Nil))
              case _                              =>
                (newEnv2, Nil)
            }
            val (newEnv5, newRes3) = polyFunInstantiator.instantiatePolyFunctionS(lambdaInfo, instArgs)(newLocalInstTree2)(newEnv4)
            (newEnv5, ((newRes2 |@| newRes3) { case (lis, (li, _)) => lis + (i -> li) }, newRes3.map { _._2 }.getOrElse(newLocalInstTree2))) 
        }
        (newEnv6, ((newRes |@| newRes4) { (liMaps, lis) => liMaps + (polyFun -> lis) }, newLocalInstTree3))
    }
    res.map { 
      _.foldLeft((env2, ())) {
        case ((newEnv, _), (l, lis)) => polyFunInstantiator.addLambdaInfosS(l, lis)(env2)
      }.mapElements(identity, _ => localInstTree2.success)
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  def instantiatePolyFunctions[L, M, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, M]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, E]) =
    State(instantiatePolyFunctionsS[L, M, E](lambdaInfoMaps)(localInstTree))
  
  def instanceArgsFromLocalInstanceTree[L, M, E](localInstTree: InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]) = {
    val instArgs = localInstTree.instTables.flatMap {
      case (pf, it) => it.pairs.map { case (t, LocalInstance(i)) => (i, InstanceArg(pf, t.typ)) }
    }
    (0 until instArgs.size).foldLeft(some(Seq[InstanceArg[L, M]]())) {
      (optIas, i) => optIas.flatMap { ias => instArgs.get(i).map { ias :+ _ } }
    }.toSuccess(NonEmptyList(FatalError("index of out bounds", none, NoPosition)))
  }
}