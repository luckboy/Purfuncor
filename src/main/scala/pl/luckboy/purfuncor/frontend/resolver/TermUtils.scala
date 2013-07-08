package pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TermUtils
{
  def usedGlobalVarsFromTerm[T, U](term: Term[SimpleTerm[Symbol, T, U]]): Set[GlobalSymbol] =
    term match {
      case App(fun, args, _)                 => usedGlobalVarsFromTerm(fun) | args.list.flatMap(usedGlobalVarsFromTerm).toSet
      case Simple(Var(sym: GlobalSymbol), _) => Set(sym)
      case Simple(_, _)                      => Set()
    }
  
  def usedGlobalTypeVarsFromTypeTerm[T](term: Term[TypeSimpleTerm[Symbol, T]]): Set[GlobalSymbol] =
    term match {
      case App(fun, args, _)                     => usedGlobalTypeVarsFromTypeTerm(fun) | args.list.flatMap(usedGlobalTypeVarsFromTypeTerm).toSet
      case Simple(TypeVar(sym: GlobalSymbol), _) => Set(sym)
      case Simple(_, _)                          => Set()
    }
}