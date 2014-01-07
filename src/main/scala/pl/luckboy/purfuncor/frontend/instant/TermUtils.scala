/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TermUtils
{
  def preinstantiationLambdaInfosFromTerm[T, U, V, W, X, Y](term: Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], X]])(implicit locational: Locational[T, Y, V]): Map[Int, PreinstantiationLambdaInfo[Y, W]] =
    term match {
      case App(fun, args, _)                               =>
        args.foldLeft(preinstantiationLambdaInfosFromTerm(fun)) { _ ++ preinstantiationLambdaInfosFromTerm(_) }
      case Simple(Let(binds, body, lambdaInfo), pos)       =>
        binds.foldLeft(Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo[U, V, W, Y](lambdaInfo).copy(pos = pos))) {
          (lis, b) => lis ++ preinstantiationLambdaInfosFromTerm(b.body)
        } ++ preinstantiationLambdaInfosFromTerm(body)
      case Simple(Lambda(_, body, lambdaInfo), pos)        =>
        preinstantiationLambdaInfosFromTerm(body) + (lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo).copy(pos = pos))
      case Simple(Var(loc, lambdaInfo), pos)               =>
        Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo).copy(polyFun = locational.getGlobalLocationFromLocation(loc).map { l => PolyFunction[Y](l) }, pos = pos))
      case Simple(Literal(_), _)                           =>
        Map()
      case Simple(TypedTerm(term, _), _)                   =>
        preinstantiationLambdaInfosFromTerm(term)
      case Simple(Construct(n, lambdaInfo), pos)           =>
        Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo).copy(polyFun = some(ConstructFunction), pos = pos))
      case Simple(Select(term, cases, lambdaInfo), pos)    =>
        cases.foldLeft(Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo).copy[Y, W](polyFun = some(SelectFunction), pos = pos)) ++ preinstantiationLambdaInfosFromTerm(term)) {
          (lis, c) => lis ++ preinstantiationLambdaInfosFromTerm(c.body) + (c.lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo[U, V, W, Y](c.lambdaInfo).copy(polyFun = some(ConstructFunction), isCase = true, pos = c.typ.pos))
        }
      case Simple(Extract(term, _, body, lambdaInfo), pos) =>
        Map(lambdaInfo.idx -> PreinstantiationLambdaInfo.fromLambdaInfo[U, V, W, Y](lambdaInfo).copy(pos = pos)) ++ preinstantiationLambdaInfosFromTerm(term) ++ preinstantiationLambdaInfosFromTerm(body)
    }
}
