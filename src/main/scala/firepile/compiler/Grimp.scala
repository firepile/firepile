package firepile.compiler

object GrimpUnapply {
  import soot.Value
  import soot.ValueBox
  import soot.Local
  import soot.grimp._
  import soot.jimple._
  import scala.collection.JavaConversions._
  import soot.{Unit => SootUnit}
  import soot.SootFieldRef
  import soot.SootMethodRef
  import soot.SootMethod
  import soot.SootClass
  import soot.Type

  object SClassName {
    def unapply(v: AnyRef) = v match {
      case x: SootClass => Some(x.getName)
      case _ => None
    }
  }

  object SMethodRef {
    def unapply(v: AnyRef) = v match {
      case x: SootMethodRef => Some((x.declaringClass, x.name, x.parameterTypes.asInstanceOf[java.util.List[Type]].toList, x.returnType, x.getSignature))
      case x: SootMethod => Some((x.getDeclaringClass, x.getName, x.getParameterTypes.asInstanceOf[java.util.List[Type]].toList, x.getReturnType, x.getSignature))
      case _ => None
    }
  }

  object SFieldRef {
    def unapply(v: AnyRef) = v match {
      case x: SootFieldRef => Some((x.declaringClass, x.name, x.`type`, x.getSignature))
      case _ => None
    }
  }

  object SInstanceFieldRef {
    def unapply(v: AnyRef) = v match {
      case x: SootFieldRef if ! x.isStatic => Some((x.declaringClass, x.name, x.`type`, x.getSignature))
      case _ => None
    }
  }

  object SStaticFieldRef {
    def unapply(v: AnyRef) = v match {
      case x: SootFieldRef if x.isStatic => Some((x.declaringClass, x.name, x.`type`, x.getSignature))
      case _ => None
    }
  }
  
 object SType {
  def unapply(v:AnyRef) = v match {
     case x: Type => Some(x.toString)
     case _ => None
     }
   }  
   
  object GNullConstant {
    def unapply(v: Value) = v match {
      case x: NullConstant => true
      case _ => false
    }
  }

  object GIntConstant {
    def unapply(v: Value) = v match {
      case x: IntConstant => Some(x.value)
      case _ => None
    }
  }

  object GLongConstant {
    def unapply(v: Value) = v match {
      case x: LongConstant => Some(x.value)
      case _ => None
    }
  }

  object GFloatConstant {
    def unapply(v: Value) = v match {
      case x: FloatConstant => Some(x.value)
      case _ => None
    }
  }

  object GDoubleConstant {
    def unapply(v: Value) = v match {
      case x: DoubleConstant => Some(x.value)
      case _ => None
    }
  }

  object GXor {
    def unapply(v: Value) = v match {
      case x: XorExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GUshr {
    def unapply(v: Value) = v match {
      case x: UshrExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GSub {
    def unapply(v: Value) = v match {
      case x: SubExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GShr {
    def unapply(v: Value) = v match {
      case x: ShrExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GShl {
    def unapply(v: Value) = v match {
      case x: ShlExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GRem {
    def unapply(v: Value) = v match {
      case x: RemExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GOr {
    def unapply(v: Value) = v match {
      case x: OrExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GNe {
    def unapply(v: Value) = v match {
      case x: NeExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GMul {
    def unapply(v: Value) = v match {
      case x: MulExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GLe {
    def unapply(v: Value) = v match {
      case x: LeExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GGe {
    def unapply(v: Value) = v match {
      case x: GeExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GEq {
    def unapply(v: Value) = v match {
      case x: EqExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GDiv {
    def unapply(v: Value) = v match {
      case x: DivExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GCmpl {
    def unapply(v: Value) = v match {
      case x: CmplExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GCmpg {
    def unapply(v: Value) = v match {
      case x: CmpgExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GCmp {
    def unapply(v: Value) = v match {
      case x: CmpExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GGt {
    def unapply(v: Value) = v match {
      case x: GtExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GLt {
    def unapply(v: Value) = v match {
      case x: LtExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GAdd {
    def unapply(v: Value) = v match {
      case x: AddExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }

  object GAnd {
    def unapply(v: Value) = v match {
      case x: AndExpr => Some((x.getOp1, x.getOp2))
      case _ => None
    }
  }


  object GNeg {
    def unapply(v: Value) = v match {
      case x: NegExpr => Some(x.getOp)
      case _ => None
    }
  }

  object GArrayLength {
    def unapply(v: Value) = v match {
      case x: LengthExpr => Some(x.getOp)
      case _ => None
    }
  }


  object GCast {
    def unapply(v: Value) = v match {
      case x: CastExpr => Some((x.getOp, x.getType))
      case _ => None
    }
  }

  object GInstanceof {
    def unapply(v: Value) = v match {
      case x: InstanceOfExpr => Some((x.getOp, x.getType))
      case _ => None
    }
  }

  object GNew {
    def unapply(v: Value) = v match {
      case x: NewExpr => Some(x.getType)
      case _ => None
    }
  }

  object GNewArray {
    def unapply(v: Value) = v match {
      case x: NewArrayExpr => Some((x.getType, x.getSize))
      case _ => None
    }
  }

  object GNewMultiArray {
    def unapply(v: Value) = v match {
      case x: NewMultiArrayExpr => Some((x.getType, x.getSizes.asInstanceOf[java.util.List[Value]].toList))
      case _ => None
    }
  }

  object GNewInvoke {
    def unapply(v: Value) = v match {
      case x: NewInvokeExpr => Some((x.getBaseType, x.getMethodRef, x.getArgs.asInstanceOf[java.util.List[Value]].toList))
      case _ => None
    }
  }

  object GStaticInvoke {
    def unapply(v: Value) = v match {
      case x: StaticInvokeExpr => Some((x.getMethodRef, x.getArgs.asInstanceOf[java.util.List[Value]].toList))
      case _ => None
    }
  }
  
  object GSpecialInvoke {
    def unapply(v: Value) = v match {
      case x: SpecialInvokeExpr => Some((x.getBase, x.getMethodRef, x.getArgs.asInstanceOf[java.util.List[Value]].toList))
      case _ => None
    }
  }

  object GVirtualInvoke {
    def unapply(v: Value) = v match {
      case x: VirtualInvokeExpr => Some((x.getBase, x.getMethodRef, x.getArgs.asInstanceOf[java.util.List[Value]].toList))
      case _ => None
    }
  }

  object GInterfaceInvoke {
    def unapply(v: Value) = v match {
      case x: soot.grimp.internal.GInterfaceInvokeExpr => Some((x.getBase, x.getMethodRef, x.getArgs.asInstanceOf[java.util.List[Value]].toList))
      case x: InterfaceInvokeExpr => Some((x.getBase, x.getMethodRef, x.getArgs.asInstanceOf[java.util.List[Value]].toList))
      case _ => None
    }
  }
 
  /*
  object GInterfaceInvokeExpr {
    def unapply(v: Value) = v match {
      case x: soot.grimp.internal.GInterfaceInvokeExpr => Some((x.getBase, x.getMethodRef, x.getArgs.asInstanceOf[java.util.List[Value]].toList))
      case _ => None
    }
  }
  */
 
  object GThrow {
    def unapply(v: SootUnit) = v match {
      case x: ThrowStmt => Some(x.getOp)
      case _ => None
    }
  }

  object GExitMonitor {
    def unapply(v: SootUnit) = v match {
      case x: ExitMonitorStmt => Some(x.getOp)
      case _ => None
    }
  }

  object GEnterMonitor {
    def unapply(v: SootUnit) = v match {
      case x: EnterMonitorStmt => Some(x.getOp)
      case _ => None
    }
  }

  object GGoto {
    def unapply(v: SootUnit) = v match {
      case x: GotoStmt => Some(x.getTarget)
      case _ => None
    }
  }

  object GNop {
    def unapply(v: SootUnit) = v match {
      case x: NopStmt => true
      case _ => false
    }
  }

  object GReturnVoid {
    def unapply(v: SootUnit) = v match {
      case x: ReturnVoidStmt => true
      case _ => false
    }
  }

  object GReturn {
    def unapply(v: SootUnit) = v match {
      case x: ReturnStmt => Some(x.getOp)
      case _ => None
    }
  }

  object GIf {
    def unapply(v: SootUnit) = v match {
      case x: IfStmt => Some((x.getCondition, x.getTarget))
      case _ => None
    }
  }

  object GIdentity {
    def unapply(v: SootUnit) = v match {
      case x: IdentityStmt => Some((x.getLeftOp, x.getRightOp))
      case _ => None
    }
  }

  object GAssignStmt {
    def unapply(v: SootUnit) = v match {
      case x: AssignStmt => Some((x.getLeftOp, x.getRightOp))
      case _ => None
    }
  }

  object GInvokeStmt {
    def unapply(v: SootUnit) = v match {
      case x: InvokeStmt => Some(x.getInvokeExpr)
      case _ => None
    }
  }
  
  object GTableSwitchStmt {
    def unapply(v: SootUnit) = v match {
      case x: TableSwitchStmt => Some((x.getKey, x.getLowIndex, x.getHighIndex,
        x.getTargets.asInstanceOf[java.util.List[SootUnit]].toList, x.getDefaultTarget))
      case _ => None
    }
  }

  object GLookupSwitchStmt {
    def unapply(v: SootUnit) = v match {
      case x: LookupSwitchStmt => Some((x.getKey, x.getLookupValues.toList,
        x.getTargets.asInstanceOf[java.util.List[SootUnit]].toList, x.getDefaultTarget))
      case _ => None
    }
  }

  object GLocal {
    def unapply(v: Value) = v match {
      case x: Local => Some((x.getName, x.getType))
      case _ => None
    }
  }

  object GThisRef {
    def unapply(v: Value) = v match {
      case x: ThisRef => Some(x.getType)
      case _ => None
    }
  }

  object GParameterRef {
    def unapply(v: Value) = v match {
      case x: ParameterRef => Some((x.getType, x.getIndex))
      case _ => None
    }
  }

  object GStaticFieldRef {
    def unapply(v: Value) = v match {
      case x: StaticFieldRef => Some(x.getFieldRef)
      case _ => None
    }
  }

  object GInstanceFieldRef {
    def unapply(v: Value) = v match {
      case x: InstanceFieldRef => Some(x.getBase, x.getFieldRef)
      case _ => None
    }
  }

  object GArrayRef {
    def unapply(v: Value) = v match {
      case x: ArrayRef => Some(x.getBase, x.getIndex)
      case _ => None
    }
  }

  object GBox {
    def unapply(v: ValueBox) = v match {
      case x: ValueBox => Some(x.getValue)
      case _ => None
    }
  }
}

