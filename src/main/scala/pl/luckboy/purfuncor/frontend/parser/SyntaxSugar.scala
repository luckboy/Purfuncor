/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Terms._
import pl.luckboy.purfuncor.frontend.Terms._
import Terms._

object SyntaxSugar
{
  def makeString(s: String) = {
    (pos: Position) => makeArray(s.map { c => Simple(Literal(CharValue(c)), pos) }.toList)(pos)
  }
  
  def makeTuple(terms: List[Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]]) = {
    (pos: Position) => app(tupleFun(terms.size, pos), terms, pos)
  }
  
  def makeArray(terms: List[Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]]) = {
    (pos: Position) => app(makearrayFun(terms.size, pos), terms, pos)
  }
  
  def makeList(terms: List[Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]]) = {
    (pos: Position) =>
      val consSym = GlobalSymbol(NonEmptyList("predef", "::"), pos)
      val nilSym = GlobalSymbol(NonEmptyList("predef", "Nil"), pos)
      val consTerm = Simple(Var(consSym, LambdaInfo), pos)
      val nilTerm = Simple(Var(nilSym, LambdaInfo), pos)
      App(makelistFun(terms.size, pos), consTerm :: terms <::: NonEmptyList(nilTerm), pos)
  }
    
  def makeIfElse(condTerm: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]], firstTerm: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]], secondTerm: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) = {
    (pos: Position) =>
      App(builtinFun(BuiltinFunction.Cond, pos), NonEmptyList(
          Simple(Lambda(NonEmptyList(Arg(none, none, firstTerm.pos)), firstTerm, LambdaInfo), firstTerm.pos),
          Simple(Lambda(NonEmptyList(Arg(none, none, secondTerm.pos)), secondTerm, LambdaInfo), secondTerm.pos),
          condTerm), pos)
  }
  
  def makeTypeTuple(terms: List[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) = {
    (pos: Position) => app(tupleTypeFun(terms.size, pos), terms, pos)
  }
  
  def makeNamedFieldApp(sym: Symbol, moduleName: Option[String], fieldValues: List[NamedFieldValue]) = {
    (pos: Position) =>
      // C.apply.fields (C.default.fieldsWith (fieldset N (C.field.f1 x1) ... (C.field.fN xN)))
      App(variable(sym ++ List("apply", "fields"), LambdaInfo, pos),
          NonEmptyList(
              variable(moduleName.map(sym.withModule).getOrElse(sym), LambdaInfo, pos),
              App(variable(sym ++ List("default", "fieldsWith"), LambdaInfo, pos),
                  NonEmptyList(app(fieldsetFun(fieldValues.size, pos), fieldValues.map { 
                    case NamedFieldValue(name, value, fieldValuePos) =>
                      App(variable(sym ++ List("field", name), LambdaInfo, fieldValuePos), NonEmptyList(value), fieldValuePos)
                  }, pos)),
                  pos)),
          pos)
  }
  
  private def makeDatatypeDefPart(sym: Symbol, kind: Option[KindTerm[StarKindTerm[String]]], args: List[TypeArg], supertype: Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]], constrs: NonEmptyList[Constructor]) =
    List(
        // type (T: k) = tN = C1.Type t1 ... tN #| ... #| CM t1 ... tN
        TypeCombinatorDef(
            sym, kind,
            renamedTypeArgsFromTypeArgs(args, "t"),
            constrs.tail.foldLeft(app(typeVar(constrs.head.sym ++ List("Type"), constrs.head.sym.pos), renamedTypeVarsFromTypeArgs(args, "t"), constrs.head.sym.pos): Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) {
              (tt, c) => App(typeDisjFun(sym.pos), NonEmptyList(tt, app(typeVar(c.sym ++ List("Type"), c.sym.pos), renamedTypeVarsFromTypeArgs(args, "t"), c.sym.pos)), sym.pos)
            }),
        // instance select T construct {
        //   C1.Type
        //     .
        //     .
        //     .
        //   CM.Type
        // }
        SelectConstructInstanceDef(
            typeVar(sym, sym.pos),
            constrs.map { c => typeVar(c.sym ++ List("Type"), c.sym.pos) }),
        // grouptype N T.BaseGrouptype: k
        UnittypeCombinatorDef(
            args.size,
            sym ++ List("BaseGrouptype"),
            kind),
        // type (T.BaseType: k) t1 ... tN = T.BaseGrouptype t1 ... tN #& T.BaseSupertype t1 ... tN
        TypeCombinatorDef(
            sym ++ List("BaseType"), 
            kind,
            renamedTypeArgsFromTypeArgs(args, "t"),
            App(typeConjFun(sym.pos),
                NonEmptyList(
                    app(typeVar(sym ++ List("BaseGrouptype"), sym.pos), renamedTypeVarsFromTypeArgs(args, "t"), sym.pos),
                    app(typeVar(sym ++ List("BaseSupertype"), sym.pos), renamedTypeVarsFromTypeArgs(args, "t"), sym.pos)),
                sym.pos)),
        // type (T.BaseSupertype: k) t1 ... tN = V
        TypeCombinatorDef(
            sym ++ List("BaseSupertype"),
            kind, args,
            supertype.getOrElse(anyType(sym.pos))))
  
  def makeDatatypeDef(datatypeDef: DatatypeDef) = {
    //
    // datatype (T: k) t1 ... tN [extends V] = C1 U11 ... U1L1 extends V1 | ... | CM UM1 ... UMLM extends VM
    //
    // will be translated to:
    //
    // type (T: k) t1 ... tN = C1.Type t1 ... tN #| ... #| CM t1 ... tN
    // instance select T construct {
    //   C1.Type
    //     .
    //     .
    //     .
    //   CM.Type
    // }
    //
    // grouptype N T.BaseGrouptype: k
    // type (T.BaseType: k) t1 ... tN = T.BaseGrouptype t1 ... tN #& T.BaseSupertype t1 ... tN
    // type (T.BaseSupertype: k) t1 ... tN = V
    //
    // C1 x1 ... xL1 = _construct.C1 x1 ... xL1: T
    // _construct.C1 = ...
    // module C1 { ... }
    //
    //   .
    //   .
    //   .
    //
    // CM x1 ... xLM = _construct.CM x1 ... xLM: T
    // _construct.CM = ...
    // module CM { ... }
    //
    datatypeDef match {
      case DatatypeDef(sym, kind, args, supertype, constrs) =>
        val defs1 = makeDatatypeDefPart(sym, kind, args, supertype, constrs)
        val defs2 = constrs.list.flatMap {
          constr =>
            List(
                // Cj x1 ... xLj = _construct.Cj x1 ... xLj: T
                CombinatorDef(
                    constr.sym, none,
                    constr.fieldPoses.zipWithIndex.map { case (pos, i) => Arg(some("x" + (i + 1)), none, pos) },
                    typedTerm(
                        app(variable(constr.sym.withModule("_construct"), LambdaInfo, constr.sym.pos),
                            constr.fieldPoses.zipWithIndex.map { case (pos, i) => variable(NormalSymbol(NonEmptyList("x" + (i + 1)), pos), LambdaInfo, pos) },
                            constr.sym.pos),
                        typeVar(sym, constr.sym.pos),
                        constr.sym.pos))) ++ makeConstructor(constr, sym, kind, args)
        }
        defs1 ++ defs2
    }
  }
    
  def makeConstructor(constr: Constructor, datatypeSym: Symbol, datatypeKind: Option[KindTerm[StarKindTerm[String]]], datatypeArgs: List[TypeArg]) = {
    //
    // Cj Uj1 ... UjLj extends Vj or Cj { fj1: Uj1, ..., fjk1: Ujk1 = xjk1, ..., fjkK: UjkK = xjkK, ..., fjLj: UjLj } extends Vj
    //
    // will be translated to:
    //
    // _construct.Cj = construct M: \t1 ... tN => Uj1 #-> ... #-> UjLj #-> Cj.Type t1 ... tN
    // unittype N Cj.Unittype: k
    // type (Cj.TypeWithoutTuple: k) t1 ... tN = T.BaseType t1 ... tN #& Cj.Unittype t1 ... tN #& Cj.Supertype t1 ... tN
    // type (Cj.Tuple: k) t1 ... tN = (Uj1, ..., UjLj)
    // type (Cj.Type: k) t1 ... tN = Cj.TypeWithoutTuple t1 ... tN #& (Uj1, ..., UjLj)
    // type (Cj.Supertype: k) t1 ... tN = Vj
    //
    // // for named fields
    //
    // Cj.fj1 (x: Cj.Type) = #M 1 x
    //   .
    //   .
    //   .
    // Cj.fjM (x: Cj.Type) = #M M x
    //
    // type Cj.field.fj1 t1 ... tN = ##1 Uj1
    // Cj.field.fj1 x = ##1 x: Cj.field.fj1
    //
    //   .
    //   .
    //   .
    //
    // type Cj.field.fjM t1 ... tN = ##M UjM
    // Cj.field.fjM x = ##M x: Cj.field.fjM
    //
    // type Cj.FieldSet t1 ... tN = ((#FieldSet1 () () #| #FieldSet1 ##1 () (Cj.field.fj1 t1 ... tN) #| ... #| #FieldSet1 ##M () (Cj.field.fjM t1 ... tN)) #& #FieldSet2 ##1 () (Cj.field.fj1 t1 ... tN) #& ... #& #FieldSet2 ##M () (Cj.field.fjM t1 ... tN))
    // Cj.apply.fields = ###M
    //
    // Cj.default._fieldsWith = (fieldswith Lj k1 ... kK) xjk1 ... xjkK
    // Cj.default.fieldsWith fs = Cj.default._fieldsWith fs: Cj.FieldSet
    //
    // Uj1 #-> ... #-> UjLj #-> Cj.Type t1 ... tN
    val constructTypeTerm = constr.fieldTypes.foldRight(app(typeVar(constr.sym ++ List("Type"), constr.sym.pos), typeVarsFromTypeArgs(datatypeArgs), constr.sym.pos): Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) {
      case (ftt, tt) => App(funTypeFun(constr.sym.pos), NonEmptyList(ftt, tt), constr.sym.pos)
    }
    val constructTupleType = app(tupleTypeFun(constr.fieldCount, constr.sym.pos), constr.fieldTypes, constr.sym.pos)
    val defs1 = List(
        // _construct.Cj = construct M: \t1 ... tN => Uj1 #-> ... #-> UjLj #-> Cj.Type t1 ... tN
        CombinatorDef(
            constr.sym.withModule("_construct"),
            none,
            Nil,
            typedTerm(
                construct(constr.fieldCount, LambdaInfo, constr.sym.pos),
                typeLambda(namedTypeArgsFromTypeArgs(datatypeArgs), constructTypeTerm, TypeLambdaInfo, constr.sym.pos),
                constr.sym.pos)
        ),
        // unittype N Cj.Unittype: k
        UnittypeCombinatorDef(
            datatypeArgs.size,
            constr.sym ++ List("Unittype"),
            datatypeKind),
        // type (Cj.TypeWithoutTuple: k) t1 ... tN = T.BaseType t1 ... tN #& Cj.Unittype t1 ... tN #& Cj.Supertype t1 ... tN
        TypeCombinatorDef(
            constr.sym ++ List("TypeWithoutTuple"),
            datatypeKind,
            renamedTypeArgsFromTypeArgs(datatypeArgs, "t"),
            App(typeConjFun(constr.sym.pos),
                NonEmptyList(
                    App(typeConjFun(constr.sym.pos),
                        NonEmptyList(
                            app(typeVar(datatypeSym ++ List("BaseType"), constr.sym.pos), renamedTypeVarsFromTypeArgs(datatypeArgs, "t"), constr.sym.pos),
                            app(typeVar(constr.sym ++ List("Unittype"), constr.sym.pos), renamedTypeVarsFromTypeArgs(datatypeArgs, "t"), constr.sym.pos)),
                        constr.sym.pos),
                     app(typeVar(constr.sym ++ List("Supertype"), constr.sym.pos), renamedTypeVarsFromTypeArgs(datatypeArgs, "t"), constr.sym.pos)),
                constr.sym.pos)),
        // type (Cj.Tuple: k) t1 ... tN = (Uj1, ..., UjLj)
        TypeCombinatorDef(
            constr.sym ++ List("Tuple"),
            datatypeKind,
            datatypeArgs,
            constructTupleType),
        // type (Cj.Type: k) t1 ... tN = Cj.TypeWithoutTuple t1 ... tN #& (Uj1, ..., UjLj)
        TypeCombinatorDef(
            constr.sym ++ List("Type"),
            datatypeKind,
            namedTypeArgsFromTypeArgs(datatypeArgs),
            App(typeConjFun(constr.sym.pos),
                NonEmptyList(
                    app(typeVar(constr.sym ++ List("TypeWithoutTuple"), constr.sym.pos), typeVarsFromTypeArgs(datatypeArgs), constr.sym.pos),
                    constructTupleType),
                constr.sym.pos)),
        // type (Cj.Supertype: k) t1 ... tN = Vj
        TypeCombinatorDef(
            constr.sym ++ List("Supertype"),
            datatypeKind, datatypeArgs,
            constr.supertype.getOrElse(anyType(constr.sym.pos))))
    val defs2 = constr match {
      case UnnamedFieldConstructor(_, _, _) =>
        Nil
      case NamedFieldConstructor(sym, fields, _) =>
        val fieldCombDefs = fields.zipWithIndex.map {
          case (NamedField(fieldName, fieldType, _, fieldPos), i) =>
            // Cj.fjk (x: Cj.Type) = #M k x
            CombinatorDef(
                constr.sym ++ List(fieldName),
                none, 
                List(Arg(some("x"), some(typeVar(constr.sym ++ List("Type"), fieldPos)), fieldType.pos)),
                App(tupleFieldFun(constr.fieldCount, i, fieldPos),
                    NonEmptyList(variable(NormalSymbol(NonEmptyList("x"), fieldPos), LambdaInfo, fieldPos)),
                    fieldPos))
        }
        val fieldDefs = fields.zipWithIndex.flatMap {
          case (NamedField(fieldName, fieldType, _, fieldPos), i) =>
            List(
                // type Cj.field.fjk t1 ... tN = ##1 Ujk
                TypeCombinatorDef(
                    constr.sym ++ List("field", fieldName),
                    none, datatypeArgs,
                    App(fieldTypeFun(i, fieldType.pos), NonEmptyList(fieldType), fieldType.pos)),
                // Cj.field.fjk x = ##1 x: Cj.field.fjk
                CombinatorDef(
                    constr.sym ++ List("field", fieldName),
                    none,
                    List(Arg(some("x"), none, fieldPos)),
                    typedTerm(
                        App(fieldFun(i, fieldPos), NonEmptyList(variable(NormalSymbol(NonEmptyList("x"), fieldPos), LambdaInfo, fieldPos)), fieldPos), 
                        typeVar(constr.sym ++ List("field", fieldName), fieldType.pos), fieldPos)))
        }
        // ((#FieldSet1 () () #| #FieldSet1 ##1 () (Cj.field.fj1 t1 ... tN) #| ... #| #FieldSet1 ##M () (Cj.field.fjM t1 ... tN)) #& #FieldSet2 ##1 () (Cj.field.fj1 t1 ... tN) #& ... #& #FieldSet2 ##M () (Cj.field.fjM t1 ... tN))
        val tmpFieldSetTypeTerm1 = fields.zip(0 until fields.size).foldLeft(App(fieldSet1TypeFun(sym.pos), NonEmptyList(tupleTypeFun(0, sym.pos), tupleTypeFun(0, sym.pos)), sym.pos): Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) {
          case (tt, (f, i)) => 
            App(typeDisjFun(sym.pos), NonEmptyList(
                tt, 
                App(fieldSet1TypeFun(sym.pos), NonEmptyList(
                    App(fieldTypeFun(i, f.pos), NonEmptyList(tupleTypeFun(0, f.pos)), f.pos),
                    app(typeVar(constr.sym ++ List("field", f.name), f.pos), renamedTypeVarsFromTypeArgs(datatypeArgs, "t"), f.pos)),
                    sym.pos)),
                sym.pos)
        }
        val tmpFieldSetTypeTerm2 = fields.zip(0 until fields.size).foldLeft(tmpFieldSetTypeTerm1) {
          case (tt, (f, i)) => 
            App(typeConjFun(sym.pos), NonEmptyList(
                tt, 
                App(fieldSet2TypeFun(sym.pos), NonEmptyList(
                    App(fieldTypeFun(i, f.pos), NonEmptyList(tupleTypeFun(0, f.pos)), f.pos),
                    app(typeVar(constr.sym ++ List("field", f.name), f.pos), renamedTypeVarsFromTypeArgs(datatypeArgs, "t"), f.pos)),
                    sym.pos)),
                sym.pos)
        }
        val otherDefs = List(
            // type Cj.FieldSet t1 ... tN = ((#FieldSet1 () () #| #FieldSet1 ##1 () (Cj.field.fj1 t1 ... tN) #| ... #| #FieldSet1 ##M () (Cj.field.fjM t1 ... tN)) #& #FieldSet2 ##1 () (Cj.field.fj1 t1 ... tN) #& ... #& #FieldSet2 ##M () (Cj.field.fjM t1 ... tN))
            TypeCombinatorDef(
                constr.sym ++ List("FieldSet"),
                none,
                renamedTypeArgsFromTypeArgs(datatypeArgs, "t"),
                tmpFieldSetTypeTerm2),
            // Cj.apply.fields = ###M
            CombinatorDef(
                constr.sym ++ List("apply", "fields"),
                none, Nil,
                fieldSetAppFun(fields.size, constr.sym.pos)),
            // Cj.default._fieldsWith = (fieldswith Lj k1 ... kK) xjk1 ... xjkK
            CombinatorDef(
                constr.sym ++ List("default", "_fieldsWith"),
                none, Nil,
                app(
                    fieldswithFun(fields.size, fields.zipWithIndex.flatMap { case (f, i) => f.default.map { _ => i } }, sym.pos),
                    fields.flatMap { _.default },
                    sym.pos)),
            // Cj.default.fieldsWith fs = Cj.default._fieldsWith fs: Cj.FieldSet
            CombinatorDef(
                constr.sym ++ List("default", "fieldsWith"),
                none,
                List(Arg(some("fs"), none, sym.pos)),
                typedTerm(
                    App(variable(constr.sym ++ List("default", "_fieldsWith"), LambdaInfo, sym.pos), 
                        NonEmptyList(variable(NormalSymbol(NonEmptyList("fs"), sym.pos), LambdaInfo, sym.pos)),
                        sym.pos),
                    typeVar(constr.sym ++ List("FieldSet"), sym.pos), sym.pos)))
        fieldCombDefs ++ fieldDefs ++ otherDefs
    }
    defs1 ++ defs2
  }
  
  def makeTypeclassDef(typeclassDef: TypeclassDef) = {
    //
    // typeclass (TC: k) t1 ... tN
    // {
    //   m1: U1
    //     .
    //     .
    //     .
    //   mk1: Uk1 = xk1
    //     .
    //     .
    //     .
    //   mkL: UkL = xkL
    //     .
    //     .
    //     .
    //   mM: UM
    // }
    //
    // will be translated to:
    //
    // type (TC: k) t1 ... tN = C1.Type t1 ... tN 
    // instance select TC construct {
    //   TC.Type
    // }
    //
    // unittype N _typeclass.TC.Unittype: k
    // type (_typeclass.TC.Supertype: k) t1 ... tN = #Any
    // type (_typeclass.TC.BaseType: k) t1 ... tN = _typeclass.TC.Unittype t1 ... tN #& _typeclass.TC.Supertype t1 ... tN
    //
    // poly _typeclass.TC: _typeclass.TC
    //
    // module _typeclass.TC { ... }
    //
    // m1 = _typeclass.TC.m1 _typeclass.TC
    //   .
    //   .
    //   .
    // mM = _typeclass.TC.mM _typeclass.TC
    //
    typeclassDef match {
      case TypeclassDef(sym, kind, args, members) =>
        val constr = NamedFieldConstructor(
            sym = sym.withModule("_typeclass"),
            fields = members.list,
            supertype = none)
        val defs1 = List(
            PolyCombinatorDef(sym.withModule("_typeclass"), none)) ++ makeDatatypeDefPart(sym.withModule("_typeclass"), kind, args, none, NonEmptyList(constr))
        val defs2 = makeConstructor(constr, sym.withModule("_typeclass"), kind, args)
        val defs3 = members.map {
          member =>
            // mj = _typeclass.TC.mj _typeclass.TC
            CombinatorDef(
                sym.withName(member.name),
                none, Nil,
                App(variable(sym.withModule("_typeclass") ++ List(member.name), LambdaInfo, member.pos),
                    NonEmptyList(variable(sym.withModule("_typeclass"), LambdaInfo, member.pos)), member.pos))
        }.list
        defs1 ++ defs2 ++ defs3
    }
  }
  
  def makeTypeClassInstanceDef(typeClassInstDef: TypeClassInstanceDef) = {
    //
    // instance \t1 ... tN => TC U1 ... UM
    // {
	//   m1 x1 ... xK1 = y1
    //     .
    //     .
    //     .
    //   mL x1 ... xKL = yL
    // }
    //
    // will be translated to:
    //
    // instance _typeclass.TC => _instance.TC.`U1 ... UM`
    //
    // _instance.TC.`U1 ... UM` =
    //   construct _typeclass.TC {
    //     m1 = \x1 ... xK1 => y1,
    //        .
    //        .
    //        .
    //     mL = \x1 ... xKL => yL
    //   }: \t1 ... tN => _.typeclass.TC U1 ... UM
    //
    typeClassInstDef match {
      case TypeClassInstanceDef(typeArgs, typeClassSym, typeClassArgs, memberValues) =>
        val instCombName = typeClassArgs.map { 
          case t @ Simple(TypeLiteral(_) | TypeVar(_), _) => typeTermShowing.stringFrom(t)
          case t                                          => "(" + typeTermShowing.stringFrom(t) + ")"
        }.mkString(" ")
        List(
            // instance _typeclass.TC => _instance.TC.`U1 ... UM`
            InstanceDef(typeClassSym.withModule("_typeclass"), typeClassSym.withModule("_instance") ++ List(instCombName)),
            // _instance.TC.`U1 ... UM` =
            //   construct _typeclass.TC {
            //     m1 = \x1 ... xK1 => y1,
            //        .
            //        .
            //        .
            //     mL = \x1 ... xKL => yL
            //   }: \t1 ... tN => _.typeclass.TC U1 ... UM
            CombinatorDef(
                typeClassSym.withModule("_instance") ++ List(instCombName),
                none, Nil,
                typedTerm(
                    makeNamedFieldApp(
                        typeClassSym.withModule("_typeclass"),
                        some("_construct"),
                        memberValues.map { 
                          case MemberValue(name, args, body, pos) =>
                            NamedFieldValue(name, lambda(args, body, LambdaInfo, pos), pos)
                        }.list)(typeClassSym.pos),
                    typeLambda(
                        namedTypeArgsFromTypeArgs(typeArgs),
                        app(typeVar(typeClassSym.withModule("_typeclass"), typeClassSym.pos), typeClassArgs, typeClassSym.pos),
                        TypeLambdaInfo,
                        typeClassSym.pos), typeClassSym.pos)))
    }
  }
}
