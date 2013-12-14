package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object CombinatorInstanceRecursiveInitializer
{
  def preinstantiationLambdasInfoFromTerm[T, U, V, W, X, Y](term: Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], X]])(implicit locational: Locational[T, Y, V]): Map[Int, PreinstantiationLambdaInfo[Y, W]] =
    term match {
      case App(fun, args, _)                             =>
        args.foldLeft(preinstantiationLambdasInfoFromTerm(fun)) { _ ++ preinstantiationLambdasInfoFromTerm(_) }
      case Simple(Let(binds, body, lambdaInfo), _)       =>
        binds.foldLeft(Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo[U, V, W, Y](lambdaInfo))) {
          (lis, b) => lis ++ preinstantiationLambdasInfoFromTerm(b.body)
        } ++ preinstantiationLambdasInfoFromTerm(body)
      case Simple(Lambda(_, body, lambdaInfo), _)        =>
        preinstantiationLambdasInfoFromTerm(body) + (lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo))
      case Simple(Var(loc, lambdaInfo), _)               =>
        Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo).copy(polyFun = locational.getGlobalLocationFromLocation(loc).map { PolyFunction(_) }))
      case Simple(Literal(_), _)                         =>
        Map()
      case Simple(TypedTerm(term, _), _)                 =>
        preinstantiationLambdasInfoFromTerm(term)
      case Simple(Construct(n, lambdaInfo), _)           =>
        Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo).copy(polyFun = some(ConstructFunction)))
      case Simple(Select(term, cases, lambdaInfo), _)    =>
        cases.foldLeft(Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo).copy[Y, W](polyFun = some(ConstructFunction))) ++ preinstantiationLambdasInfoFromTerm(term)) {
          (lis, c) => lis ++ preinstantiationLambdasInfoFromTerm(c.body)
        }
      case Simple(Extract(term, _, body, lambdaInfo), _) =>
        Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo[U, V, W, Y](lambdaInfo)) ++ preinstantiationLambdasInfoFromTerm(term) ++ preinstantiationLambdasInfoFromTerm(body)
    }
}