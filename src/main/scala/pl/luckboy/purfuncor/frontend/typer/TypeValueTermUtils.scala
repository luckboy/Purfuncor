package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TypeValueTermUtils
{
  def typeParamsFromTypeValueTerm[T](term: TypeValueTerm[T]): Set[Int] =
    term match {
      case TupleType(args)           => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case BuiltinType(_, args)      => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case Unittype(_, args, _)      => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case GlobalTypeApp(_, args, _) => args.flatMap { a => typeParamsFromTypeValueTerm(a.body) -- a.argParams }.toSet
      case TypeParamApp(_, args, _)  => args.flatMap { a => typeParamsFromTypeValueTerm(a.body) -- a.argParams }.toSet
      case TypeConjunction(terms)    => terms.flatMap(typeParamsFromTypeValueTerm).toSet
      case TypeDisjunction(terms)    => terms.flatMap(typeParamsFromTypeValueTerm).toSet
    }
  
  def substituteTypeValueLambdasInTypeValueTerms[T](terms: Seq[TypeValueTerm[T]], lambdas: Map[Int, TypeValueLambda[T]]) =
    terms.foldLeft(some(Seq[TypeValueTerm[T]]())) {
      (o, t) => for(ts <- o; t2 <- substituteTypeValueLambdas(t, lambdas)) yield (ts :+ t2)
    }

  def substituteTypeValueLambdasInTypeValueLambdas[T](lambdas: Seq[TypeValueLambda[T]], lambdas2: Map[Int, TypeValueLambda[T]]) =
    lambdas.foldLeft(some(Seq[TypeValueLambda[T]]())) {
      (o, l) => for(ls <- o; l2 <- substituteTypeValueLambdasInTypeValueLambda(l, lambdas2 -- l.argParams)) yield (ls :+ l2)
    }

  def substituteTypeValueLambdasInTypeValueLambda[T](lambda: TypeValueLambda[T], lambdas: Map[Int, TypeValueLambda[T]]): Option[TypeValueLambda[T]] =
    lambda match {
      case TypeValueLambda(argParams, TypeParamApp(param, args, paramAppIdx)) =>
        substituteTypeValueLambdasInTypeValueLambdas(args, lambdas).flatMap {
          args2 =>
            lambdas.get(param).map {
              case TypeValueLambda(argParams2, body2) =>
                val argParams3 = argParams2.drop(args2.size) ++ argParams
                val lambda3 = TypeValueLambda(argParams3, body2)
                substituteTypeValueLambdasInTypeValueLambda(lambda3, argParams2.take(args2.size).zip(args2).toMap)
            }.getOrElse(some(TypeValueLambda(argParams, TypeParamApp(param, args2, paramAppIdx))))
        }
      case TypeValueLambda(argParams, body) =>
        substituteTypeValueLambdas(body, lambdas -- argParams).map { TypeValueLambda(argParams, _) }
    }
  
  def substituteTypeValueLambdas[T](term: TypeValueTerm[T], lambdas: Map[Int, TypeValueLambda[T]]): Option[TypeValueTerm[T]] =
    term match {
      case TupleType(args)               => 
        substituteTypeValueLambdasInTypeValueTerms(args, lambdas).map { TupleType(_) }
      case BuiltinType(bf, args)         =>
        substituteTypeValueLambdasInTypeValueTerms(args, lambdas).map { BuiltinType(bf, _) }
      case Unittype(loc, args, sym)      =>
        substituteTypeValueLambdasInTypeValueTerms(args, lambdas).map { Unittype(loc, _, sym) }
      case GlobalTypeApp(loc, args, sym) =>
        substituteTypeValueLambdasInTypeValueLambdas(args, lambdas).map { GlobalTypeApp(loc, _, sym) }
      case typeParamApp: TypeParamApp[T] =>
        substituteTypeValueLambdasInTypeValueLambda(TypeValueLambda(Nil, typeParamApp), lambdas).flatMap {
          case TypeValueLambda(Seq(), body) => some(body)
          case _                            => none
        }
      case TypeConjunction(terms)        =>
        substituteTypeValueLambdasInTypeValueTerms(terms.toSeq, lambdas).map { ts => TypeConjunction(ts.toSet) }
      case TypeDisjunction(terms)        =>
        substituteTypeValueLambdasInTypeValueTerms(terms.toSeq, lambdas).map { ts => TypeDisjunction(ts.toSet) }
    }
}