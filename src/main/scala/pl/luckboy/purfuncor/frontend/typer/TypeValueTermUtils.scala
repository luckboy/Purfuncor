package pl.luckboy.purfuncor.frontend.typer
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TypeValueTermUtils
{
  def typeParamsFromTypeValueTerm[T](term: TypeValueTerm[T]): Set[Int] =
    term match {
      case TupleType(args)              => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case BuiltinType(_, args)         => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case Unittype(_, args, _)         => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case GlobalTypeApp(_, args, _)    => args.flatMap { a => typeParamsFromTypeValueTerm(a.body) -- a.argParams }.toSet
      case TypeParamApp(param, args, _) => Set(param) | args.flatMap { a => typeParamsFromTypeValueTerm(a.body) -- a.argParams }.toSet
      case TypeConjunction(terms)       => terms.flatMap(typeParamsFromTypeValueTerm).toSet
      case TypeDisjunction(terms)       => terms.flatMap(typeParamsFromTypeValueTerm).toSet
    }
  
  def typeArgParamsFromTypeValueTerm[T](term: TypeValueTerm[T]): Set[Int] =
    term match {
      case TupleType(args)              => args.flatMap(typeArgParamsFromTypeValueTerm).toSet
      case BuiltinType(_, args)         => args.flatMap(typeArgParamsFromTypeValueTerm).toSet
      case Unittype(_, args, _)         => args.flatMap(typeArgParamsFromTypeValueTerm).toSet
      case GlobalTypeApp(_, args, _)    => args.flatMap { _.argParams }.toSet
      case TypeParamApp(param, args, _) => args.flatMap { _.argParams }.toSet
      case TypeConjunction(terms)       => terms.flatMap(typeArgParamsFromTypeValueTerm).toSet
      case TypeDisjunction(terms)       => terms.flatMap(typeArgParamsFromTypeValueTerm).toSet
    }
  
  private def substituteTypeValueLambdasInTypeValueTerms[T](terms: Seq[TypeValueTerm[T]], paramLambdas: Map[Int, TypeValueLambda[T]], nextArgParam: Int) =
    terms.foldLeft(some(Seq[TypeValueTerm[T]]())) {
      (o, t) => for(ts <- o; t2 <- substituteTypeValueLambdasInTypeValueTerm(t, paramLambdas, nextArgParam)) yield (ts :+ t2)
    }

  private def substituteTypeValueLambdasInTypeValueLambdas[T](lambdas: Seq[TypeValueLambda[T]], paramLambdas: Map[Int, TypeValueLambda[T]], nextArgParam: Int) =
    lambdas.foldLeft(some(Seq[TypeValueLambda[T]]())) {
      (o, l) => for(ls <- o; l2 <- substituteTypeValueLambdasInTypeValueLambda(l, paramLambdas -- l.argParams, nextArgParam)) yield (ls :+ l2)
    }

  private def substituteTypeValueLambdasInTypeValueLambda[T](lambda: TypeValueLambda[T], paramLambdas: Map[Int, TypeValueLambda[T]], nextArgParam: Int): Option[TypeValueLambda[T]] =
    lambda match {
      case TypeValueLambda(argParams, typeParamApp @ TypeParamApp(param, args, paramAppIdx)) =>
        substituteTypeValueLambdasInTypeValueLambdas(args, paramLambdas, nextArgParam).flatMap {
          args2 =>
            paramLambdas.get(param).map {
              case TypeValueLambda(argParams2, body2) =>
                val (argParams3, paramLambdas3, body3) = if((argParams.toSet & argParams2.drop(args2.size).toSet).isEmpty) {
                  (argParams ++ argParams2.drop(args2.size), argParams2.take(args2.size).zip(args2).toMap, body2)
                } else {
                  val bodyParams2 = typeParamsFromTypeValueTerm(body2)
                  val nextArgParam2 = (
                      typeParamsFromTypeValueTerm(typeParamApp) |
                      typeArgParamsFromTypeValueTerm(typeParamApp) |
                      bodyParams2 |
                      typeArgParamsFromTypeValueTerm(body2)).maximum.map(+ _).getOrElse(nextArgParam)
                  val (_, tmpLambda) = normalizeTypeParamsInTypeValueLambdaForParamsS(TypeValueLambda(argParams2, body2), nextArgParam2)(IntMap())(bodyParams2.map { p => p -> p }.toMap)
                  (argParams ++ tmpLambda.argParams.drop(args2.size), tmpLambda.argParams.take(args2.size).zip(args2).toMap, tmpLambda.body)
                }
                val lambda3 = TypeValueLambda(Nil, body3)
                substituteTypeValueLambdasInTypeValueLambda(lambda3, paramLambdas3, nextArgParam).map {
                  case TypeValueLambda(_, body4) =>
                    TypeValueLambda(argParams3, body4)
                }
            }.getOrElse(some(TypeValueLambda(argParams, TypeParamApp(param, args2, paramAppIdx))))
        }
      case TypeValueLambda(argParams, body) =>
        substituteTypeValueLambdasInTypeValueTerm(body, paramLambdas -- argParams, nextArgParam).map { TypeValueLambda(argParams, _) }
    }
  
  private def substituteTypeValueLambdasInTypeValueTerm[T](term: TypeValueTerm[T], paramLambdas: Map[Int, TypeValueLambda[T]], nextArgParam: Int): Option[TypeValueTerm[T]] =
    term match {
      case TupleType(args)               => 
        substituteTypeValueLambdasInTypeValueTerms(args, paramLambdas, nextArgParam).map { TupleType(_) }
      case BuiltinType(bf, args)         =>
        substituteTypeValueLambdasInTypeValueTerms(args, paramLambdas, nextArgParam).map { BuiltinType(bf, _) }
      case Unittype(loc, args, sym)      =>
        substituteTypeValueLambdasInTypeValueTerms(args, paramLambdas, nextArgParam).map { Unittype(loc, _, sym) }
      case GlobalTypeApp(loc, args, sym) =>
        substituteTypeValueLambdasInTypeValueLambdas(args, paramLambdas, nextArgParam).map { GlobalTypeApp(loc, _, sym) }
      case typeParamApp: TypeParamApp[T] =>
        substituteTypeValueLambdasInTypeValueLambda(TypeValueLambda(Nil, typeParamApp), paramLambdas, nextArgParam).flatMap {
          case TypeValueLambda(Seq(), body) => some(body)
          case _                            => none
        }
      case TypeConjunction(terms)        =>
        substituteTypeValueLambdasInTypeValueTerms(terms.toSeq, paramLambdas, nextArgParam).map { ts => TypeConjunction(ts.toSet) }
      case TypeDisjunction(terms)        =>
        substituteTypeValueLambdasInTypeValueTerms(terms.toSeq, paramLambdas, nextArgParam).map { ts => TypeDisjunction(ts.toSet) }
    }
  
  def substituteTypeValueLambdas[T](term: TypeValueTerm[T], paramLambdas: Map[Int, TypeValueLambda[T]], nextArgParam: Int) = {
    substituteTypeValueLambdasInTypeValueTerm(term, paramLambdas, nextArgParam).map {
      term2 =>
        normalizeTypeParamsForParams(term2, nextArgParam)(typeParamsFromTypeValueTerm(term2).map { p => p -> p }.toMap)
    }
  }
    
  private def normalizeTypeParamsInTypeValueTermsForParamsS[T](terms: Seq[TypeValueTerm[T]], nextArgParam: Int)(lambdaParams: Map[Int, Int])(termParams: Map[Int, Int]) =
    terms.foldLeft((termParams, Seq[TypeValueTerm[T]]())) {
      case ((ps, ts), t) => normalizeTypeParamsInTypeValyeTermForParamsS(t, nextArgParam)(lambdaParams)(ps).mapElements(identity, ts :+ _)
    }
  
  private def normalizeTypeParamsInTypeValueLambdasForParamsS[T](lambdas: Seq[TypeValueLambda[T]], nextArgParam: Int)(lambdaParams: Map[Int, Int])(termParams: Map[Int, Int]) =
    lambdas.foldLeft((termParams, Seq[TypeValueLambda[T]]())) {
      case ((ps, ls), l) => normalizeTypeParamsInTypeValueLambdaForParamsS(l, nextArgParam)(lambdaParams)(ps).mapElements(identity, ls :+ _)
    }
  
  private def normalizeTypeParamsInTypeValueLambdaForParamsS[T](lambda: TypeValueLambda[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(termParams: Map[Int, Int]) =
    lambda match {
      case TypeValueLambda(argParams, body) =>
        val argParams2 = nextArgParam until (nextArgParam + argParams.size)
        val lambdaParams2 = IntMap() ++ (lambdaParams ++ argParams.zipWithIndex.map { p => (p._1, nextArgParam + p._2) })
        val (termParams2, body2) = normalizeTypeParamsInTypeValyeTermForParamsS(body, nextArgParam + argParams.size)(lambdaParams2)(termParams)
        (termParams2, TypeValueLambda(argParams2, body2))
    }
  
  private def normalizeTypeParamsInTypeValyeTermForParamsS[T](term: TypeValueTerm[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(termParams: Map[Int, Int]): (Map[Int, Int], TypeValueTerm[T]) =
    term match {
      case TupleType(args)                        => 
        val (termParams2, args2) = normalizeTypeParamsInTypeValueTermsForParamsS(args, nextArgParam)(lambdaParams)(termParams)
        (termParams2, TupleType(args2))
      case BuiltinType(bf, args)                  =>
        val (termParams2, args2) = normalizeTypeParamsInTypeValueTermsForParamsS(args, nextArgParam)(lambdaParams)(termParams)
        (termParams2, BuiltinType(bf, args2))
      case Unittype(loc, args, sym)               =>
        val (termParams2, args2) = normalizeTypeParamsInTypeValueTermsForParamsS(args, nextArgParam)(lambdaParams)(termParams)
        (termParams2, Unittype(loc, args2, sym))
      case GlobalTypeApp(loc, args, sym)          =>
        val (termParams2, args2) = normalizeTypeParamsInTypeValueLambdasForParamsS(args, nextArgParam)(lambdaParams)(termParams)
        (termParams2, GlobalTypeApp(loc, args2, sym))
      case TypeParamApp(param, args, _) =>
        val termParams2 = if(termParams.contains(param) || lambdaParams.contains(param)) 
          termParams
        else
          termParams + (param -> termParams.size)
        val param2 = lambdaParams.getOrElse(param, termParams.getOrElse(param, termParams.size))
        val (termParams3, args2) = normalizeTypeParamsInTypeValueLambdasForParamsS(args, nextArgParam)(lambdaParams)(termParams2)
        (termParams3, TypeParamApp(param2, args2, 0))
      case TypeConjunction(terms)                 =>
        val (termParams2, terms2) = normalizeTypeParamsInTypeValueTermsForParamsS(terms.toSeq, nextArgParam)(lambdaParams)(termParams)
        (termParams2, TypeConjunction(terms2.toSet))
      case TypeDisjunction(terms)                 =>
        val (termParams2, terms2) = normalizeTypeParamsInTypeValueTermsForParamsS(terms.toSeq, nextArgParam)(lambdaParams)(termParams)
        (termParams2, TypeDisjunction(terms2.toSet))
    }
  
  def normalizeTypeParams[T](term: TypeValueTerm[T], nextArgParam: Int) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(IntMap())(IntMap())._2

  def normalizeTypeParamsWithTypeParams[T](term: TypeValueTerm[T], nextArgParam: Int) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(IntMap())(IntMap()).swap
    
  def normalizeTypeParamsForParams[T](term: TypeValueTerm[T], nextArgParam: Int)(params: Map[Int, Int]) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(IntMap())(params)._2
}