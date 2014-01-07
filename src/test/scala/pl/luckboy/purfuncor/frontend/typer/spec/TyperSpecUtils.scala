/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.spec
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable

trait TyperSpecUtils
{
  val makeInferredKindTable = {
    (s: String) =>
      resolver.Resolver.transformString(s)(NameTree.empty).flatMap {
        res =>
          val (_, res2) = Typer.statefullyTransformToSymbolTree2(InferredKindTable.empty)(res).run(SymbolTypeEnvironment.empty[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]])
          res2.map { _.treeInfo.typeTree.treeInfo.kindTable }
    }
  }
}
