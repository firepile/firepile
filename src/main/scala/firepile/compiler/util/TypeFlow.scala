package firepile.compiler.util


import soot.{Unit => SootUnit, Type => SootType}
import soot.toolkits.graph.UnitGraph
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.util._
import soot.jimple._
import soot.toolkits.scalar._
import soot.Body
import soot.Scene
import soot.Local
import soot.Value
import soot.ValueBox
import soot.SootClass
import soot.ArrayType
import soot.SootMethodRef
import soot.SootFieldRef
import soot.SootMethod
import soot.grimp.Grimp
import soot.grimp.GrimpBody
import soot.options.Options
import soot.tagkit.Tag
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import firepile.compiler.GrimpUnapply._
import firepile.compiler.util.ScalaTypeGen._
import firepile.compiler.util.ClassDefs._
import firepile.compiler.util.ClassDefs.TYPEVARIANT._

object TypeFlow {
  val superTypeCache = new HashMap[List[ScalaClassDef],List[ScalaType]]()

  def main(args: Array[String]) = {
    if (args.length != 2) {
      println("usage: TypeFlow className methodSig")
      exit(1)
    }

    var className = args(0)
    val methodSig = args(1)
    var b: Body = null

    if(System.getProperty("os.name").toLowerCase().startsWith("win"))
    Scene.v.setSootClassPath(Scene.v.defaultClassPath
                  + ";."+";C:/ScalaWorld/Type-Specific-Compiler/lib/firepiletest.jar"
                  + ";C:/ScalaWorld/Type-Specific-Compiler/lib/firepiletypespecific.jar"
                  + ";C:/ScalaWorld/Type-Specific-Compiler/lib/soot-2.4.0.jar"
                  + ";C:/ScalaWorld/Type-Specific-Compiler/lib/scalap.jar"
                  + ";C:/ScalaWorld/Type-Specific-Compiler/lib/rt.jar"
                  + ";C:/ScalaWorld/Type-Specific-Compiler/lib/jce.jar"
                  + ";C:/ScalaWorld/Type-Specific-Compiler/lib/scala-library.jar")
    else
    Scene.v.setSootClassPath(Scene.v.defaultClassPath
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0-local/classes"
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0-local/test-classes"
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/classes"
      + ":/Users/nystrom/uta/funicular/funicular/firepile/target/scala_2.8.0.RC3/test-classes"
      + ":/Users/nystrom/firepile/target/scala_2.8.0.RC3/classes"
      + ":/Users/nystrom/firepile/target/scala_2.8.0.RC3/test-classes"
      + ":/Users/dwhite/svn/firepile/target/scala_2.8.0.RC3/classes"
      + ":/Users/dwhite/svn/firepile/target/scala_2.8.0.RC3/test-classes"
      + ":.:tests:bin:lib/soot-2.4.0.jar:/opt/local/share/scala-2.8/lib/scala-library.jar")

    Options.v.set_keep_line_number(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.setPhaseOption("cg", "verbose:true")
    Options.v.set_allow_phantom_refs(true)

    val c = Scene.v.loadClassAndSupport(className)
    Scene.v.loadNecessaryClasses
    c.setApplicationClass

    for (m <- c.methodIterator) {
      println("Checking: " + m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef) + " for " + methodSig)
      if ((m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)).equals(methodSig)) {
        if (! m.isConcrete)
          throw new RuntimeException("Can only run TypeFlow on concrete methods")
        b = m.retrieveActiveBody
        className = m.getDeclaringClass.getName
      }
    }

    if (b == null)
      println("Method " + methodSig + " not found in class " + className + ". Checking for raw name.")

    for (m <- c.methodIterator) {
      if (m.getName.equals(methodSig)) {
        if (! m.isConcrete)
          throw new RuntimeException("Can only run TypeFlow on concrete methods")
        b = m.retrieveActiveBody
        className = m.getDeclaringClass.getName
      }
    }

    if (b == null)
      throw new RuntimeException("Method " + methodSig + " not found in class " + className)

    println("declaring class is " + className) 
    val gb = Grimp.v().newBody(b, "gb")
    println("GRIMP\n" + gb)
    val g = new ExceptionalUnitGraph(gb)
    var classDefs: List[ScalaClassDef] = null

    val inners = className.split("\\$").toList
    var innerClass: ScalaClassDef = null
    if (inners.length > 1) {
      val (outerName :: innerNames) = inners
      val outer = getScalaSignature(outerName)
      innerClass = getInnerClassDef(outer.head, innerNames) 

      classDefs = List(innerClass)
    }
    else 
      classDefs = getScalaSignature(className.replaceAll("\\$",""))

    println("SUPERCLASSES: " + getSupertypes(classDefs))

    println("Got ClassDef for " + classDefs.head.name)
    val tfa = new TypeFlowAnalysis(g, classDefs.head)

    for (u <- gb.getUnits) {
      decorateWithTags(u, tfa)
      for (tgs <- u.getTags) {
        tgs match {
          case t: TypeFlowTag => println("Tag: " + t.getLocalName + ":" + t.getScalaType)
          case _ => 
        }
      }
    }
  }

  def getInnerClassDef(topClass: ScalaClassDef, nameChain: List[String]): ScalaClassDef = {
    nameChain match {
      case n :: ns => {
        topClass.innerClasses.find( ic => ic.name.endsWith(n)) match {
          case Some(icc: ScalaClassDef) => getInnerClassDef(icc, ns)
          case None => { println("found none" ); null }
        }
      }
      case Nil => { println("returning " + topClass.name); topClass }
    }
  }

  class TypeFlowAnalysis(graph: UnitGraph, cls: ScalaClassDef) extends ForwardFlowAnalysis[SootUnit,Map[String,ScalaType]](graph) {
    val emptySet: Map[String,ScalaType] = new HashMap[String,ScalaType]()

    doAnalysis

    protected def newInitialFlow(): Map[String,ScalaType] = {
        emptySet.clone
    }

    protected def entryInitialFlow(): Map[String,ScalaType] = {
      val paramMap = new HashMap[String,ScalaType]()
      val methodName = graph.getBody.getMethod.getName
      val methodSig = methodName + soot.AbstractJasminClass.jasminDescriptorOf(graph.getBody.getMethod.makeRef)
      val methodDef = getMethodDefBySig(methodSig)
      val thisMethod = getThisMethod(cls)


      println("entryInitialFlow for: " + methodSig)
      println("methodSig from ScalaType: " + jasminSigFromMethodDef(methodDef))

      assert(methodSig == jasminSigFromMethodDef(methodDef))

      // add class fields
      if (cls.fields != null)
        for (vd <- cls.fields)
           paramMap += vd.name -> vd.fieldScalaType


      if (methodDef == null)
        println("methodDef is null")

      // add params
      if( methodDef.params == null)
        println("params is null!!!")
      for (p <- methodDef.params) {
        println(" Adding param " + p.name + " = " + p.fieldScalaType)
        paramMap += (p.name -> p.fieldScalaType)
      }

      if(thisMethod.params != null)
        for (f <- thisMethod.params)
          paramMap += (f.name -> f.fieldScalaType)
      
      println("entryInitialFlow generated: " + paramMap)
      paramMap
    }

    protected def flowThrough(inValue: Map[String,ScalaType], unit: SootUnit, outValue: Map[String,ScalaType]): Unit = {
      // Compute gen from getDefs/kill set
      outValue ++= inValue

      // kill
      for (box <- unit.getDefBoxes) {
        box.getValue match {
          case x: Local => outValue -= getName(x)
          case x => println("wtf " + x + ": " + x.getClass.getName)
        }
      }
      

      // gen
      unit match {
        case GIdentity(x: Local, y: Local) => 
          outValue += getName(x) -> inValue(getName(y))
        case GAssignStmt(x: Local, y: Local) => 
          outValue += getName(x) -> inValue(getName(y))

        // Need to special case: field access, call, maybe others
        // ex:
        /*
        case GAssignStmt(x: Local, GVirtualInvokeExpr(base, method, args)) => 
          outValue += getName(x) -> lookup return type (ScalaType) of base.method
        ...
          */

        case GAssignStmt(x: Local, GInstanceFieldRef(base, field)) => { // not really used? (getters)
          outValue += getName(x) -> getFieldType(base, field)
          println("Assigning " + base.getType.toString + "::" + field.name + " to " + getName(x))
        }
        
        case GAssignStmt(x: Local, GVirtualInvoke(base, method, _)) => {
            outValue += getName(x) -> getMethodRetType(base, method) 
        }

        case GAssignStmt(x: Local, GArrayRef(base, _)) => { 
          // bytecodeTypeToScala(base.getType.toString) doesn't work here
          outValue += getName(x) -> bytecodeTypeToScala(base.getType.asInstanceOf[ArrayType].getArrayElementType.toString)
        }

        case GAssignStmt(x: Local, GInterfaceInvoke(base, method, _)) => {
          println("Assignment from " + base.asInstanceOf[Object].toString)
          outValue += getName(x) -> getMethodRetType(base, method)
        }

        case GAssignStmt(x: Local, GStaticInvoke(method, _)) =>
          outValue += getName(x) -> bytecodeTypeToScala(method.returnType.toString)

        case GAssignStmt(x: Local, GStaticFieldRef(field)) =>
          outValue += getName(x) -> bytecodeTypeToScala(field.`type`.toString)

        case GIdentity(x: Local, GNew(typ)) => { println("IDENTITY FROM NEW"); outValue += getName(x) -> getScalaSignature(typ.toString).head.scalatype }
        
        case GAssignStmt(x: Local, GNew(typ)) => { println("ASSIGNMENT FROM NEW"); outValue += getName(x) -> getScalaSignature(typ.toString).head.scalatype }

        case GIdentity(x: Local, GNewInvoke(base, method, args)) => { println("IDENTITY FROM NEW INVOKE"); }
        case GAssignStmt(x: Local, GNewInvoke(base, method, args)) => { println("ASSIGNMENT FROM NEW INVOKE"); }
        case GIdentity(x: Local, GSpecialInvoke(base, method, args)) => { println("IDENTITY FROM SPECIAL INVOKE"); }
        case GAssignStmt(x: Local, GSpecialInvoke(base, method, args)) => { println("ASSIGNMENT FROM SPECIAL INVOKE"); }
        
        case GIdentity(x: Local, GParameterRef(_, index)) => {
          // outValue += getName(x) -> inValue(getName(x))
          // NOTE: The following code will also need to change to match full signatures
          val param = getMethodDefBySig(getMethodSig(graph.getBody.getMethod)).params(index)
          outValue += getName(x) -> param.fieldScalaType
        }

        // Fall through cases: use the Java type provided by Soot.
        case GIdentity(x: Local, y) => 
          outValue += getName(x) -> bytecodeTypeToScala(y.getType.toString)
        case GAssignStmt(x: Local, y) => 
          outValue += getName(x) -> bytecodeTypeToScala(y.getType.toString)
        case x =>  println("wtf " + x + ": " + x.getClass.getName)
      }

/*
      */
      println("flowThrough: " + outValue)
    }

    protected def merge(in1: Map[String,ScalaType], in2: Map[String,ScalaType], out: Map[String,ScalaType]) = {
      // Compute least upper bound between matching vairables in in1 and in2, store in out
      println("MERGE")
      println("in1: " + in1)
      println("in2: " + in2)
      for (varName <- in1.keys)
        if (in2.contains(varName) && in1(varName) != in2(varName)) 
          out(varName) = lub(in1(varName), in2(varName))
        else
          out += (varName -> in1(varName))

      for (varName <- in2.keys)
        if (!out.contains(varName))
          out(varName) = in2(varName)
    }


    protected def copy(source: Map[String,ScalaType], dest: Map[String,ScalaType]) = {
      dest ++= source
    }

    private def getMethodDefBySig(searchClass: ScalaClassDef, sig: String): ScalaMethodDef = {
      var mdef: ScalaMethodDef = null

      mdef = searchThisMethodDefs(searchClass, sig)
      if ( mdef == null )
        mdef = searchSuperMethodDefs(searchClass, sig)

      mdef
    }

    private def searchThisMethodDefs(searchClass: ScalaClassDef, sig: String): ScalaMethodDef = {
      var mdef: ScalaMethodDef = null

      println("searchThisMethodDefs: Searching class " + searchClass.name)

      if ( searchClass.methods != null)
        for ( m <- searchClass.methods ) {
          if ( jasminSigFromMethodDef(m).equals(sig) ) {
             mdef = m
          }
        }

      mdef 
    }

    def getMethodSig(method: SootMethod): String = method.getName + soot.AbstractJasminClass.jasminDescriptorOf(method.makeRef)

    private def getThisMethod(searchClass: ScalaClassDef): ScalaMethodDef = {
      var mdef: ScalaMethodDef = null

      if (searchClass.methods != null)
        for (m <- searchClass.methods) {
          if (m.name.equals("this"))
            mdef = m
        }

      mdef
    }

    private def searchSuperMethodDefs(searchClass: ScalaClassDef, sig: String): ScalaMethodDef = {
      var mdef: ScalaMethodDef = null
      if (searchClass.superclass == null)
        println("superclasses are NULL!!!!")
      for ( sc <- searchClass.superclass ) {
        println("searchSuperMethodDefs: " + sc)
          val scAsClassDef = sc match {
            case x: NamedTyp if !x.name.startsWith("java.") => getScalaSignature(x.name).head
            case InstTyp(base: NamedTyp, _) => getScalaSignature(base.name).head
            case _ => null
          }

          if ( scAsClassDef != null) {
            println("searchSuperMethodDefs: searching " + scAsClassDef.name)
            mdef = searchThisMethodDefs(scAsClassDef, sig)
            if ( mdef == null && scAsClassDef.superclass != null)
              for( ssc <- scAsClassDef.superclass) { 
                val sscAsClassDef = sc match {
                  case x: NamedTyp if !x.name.startsWith("java.") => getScalaSignature(x.name).head
                  case InstTyp(base: NamedTyp, _) => getScalaSignature(base.name).head
                  case _ => null
                }
                mdef = searchSuperMethodDefs(sscAsClassDef, sig)
                if (mdef != null) return mdef
              }
          }
          if (mdef != null)
            return mdef
        }
      mdef
    }


    private def getMethodDefBySig(sig: String): ScalaMethodDef = getMethodDefBySig(cls, sig)

    private def jasminSigFromMethodDef(method: ScalaMethodDef): String = {
      method.name + "(" + method.params.map(p => p.fieldScalaType match {
        case NamedTyp(n: String) => matchBasicType(n)
        case InstTyp(base: NamedTyp, args: List[NamedTyp]) if base.name == "scala.Array" => "[" + matchBasicType(args.head.name)
        case InstTyp(base: NamedTyp, _) => matchBasicType(base.name)
      }).mkString + ")" + matchBasicType(method.returnScalaType match {
                                  case NamedTyp(name) => name
                                  case InstTyp(NamedTyp(name), _) => name })

    }

    def matchBasicType(typ: String) = typ match {
      case "scala.Unit" => "V"
      case "scala.Boolean" => "Z"
      case "scala.Byte" => "B"
      case "scala.Short" => "S"
      case "scala.Int" => "I"
      case "scala.Long" => "J"
      case "scala.Float" => "F"
      case "scala.Double" => "D"
      case "java.lang.Object" => "Ljava/lang/Object;"
      case "scala.Predef.String" => "Ljava/lang/String;"
      case x => "L" + x.replaceAll("\\.", "/") + ";"
    }

    private def bytecodeTypeToScala(bctype: String): ScalaType = {
        bctype match {
          case "boolean" => NamedTyp("scala.Boolean")
          case "byte" => NamedTyp("scala.Byte")
          case "char" => NamedTyp("scala.Char")
          case "short" => NamedTyp("scala.Short")
          case "int" => NamedTyp("scala.Int")
          case "long" => NamedTyp("scala.Long")
          case "float" => NamedTyp("scala.Float")
          case "double" => NamedTyp("scala.Double")
          case x if x.endsWith("[]") => InstTyp(NamedTyp("scala.Array"), List(bytecodeTypeToScala(x.substring(0, x.length-2))))
          case _ => NamedTyp(bctype)
        }
    }

    private def getName(local: Local): String =
      if (local.getName == "this") "this" else local.getName // + local.getNumber

    private def getMethodRetType(base: Value, method: SootMethodRef): ScalaType = {
      // val cdef = getScalaSignature(method.declaringClass.toString).head
      println("getMethodRetType: " + base.getType.toString + "::" + method.name)
      val cdef = getScalaSignature(base.getType.toString.replaceAll("\\$","")).head
      //val cdef = getScalaSignature(base.getType.toString).head
      println("getting scalasig for " + base.getType)
      if (cdef != null)
        getMethodDefBySig(cdef, getMethodSig(method.resolve)).returnScalaType
      else 
        bytecodeTypeToScala(method.returnType.toString)
    }

    private def getFieldType(base: Value, field: SootFieldRef): ScalaType = {
      val cdef = getScalaSignature(base.getType.toString).head
      var ftype: ScalaType = null
      if (cdef != null)
        for (vd <- cdef.fields)
          if (vd.name.equals(field.name))
             ftype = vd.fieldScalaType
      else
        ftype = bytecodeTypeToScala(field.`type`.toString)

      ftype
    }

  }


    def getSupertypes(bottom: String): List[ScalaType] = getSupertypes(getScalaSignature(bottom))

    def getSupertypes(bottom: ScalaType): List[ScalaType] = bottom match {
      case NamedTyp(name) => getSupertypes(getScalaSignature(name))
      case InstTyp(base, _) => getSupertypes(base)
      case ParamTyp(name, vr) => List(new ParamTyp(name, vr), new ParamTyp("scala.Any", vr))
    }

    def getSupertypes(bottom: List[ScalaClassDef]): List[ScalaType] = {
      if (superTypeCache.contains(bottom)) superTypeCache(bottom)
      else {
        val actualsLB = new ListBuffer[ScalaType]()

        for (cd <- bottom)
          if (cd.scalatype != null && cd.scalatype.isInstanceOf[InstTyp])
            actualsLB.appendAll(cd.scalatype.asInstanceOf[InstTyp].args)

        val actuals = actualsLB.toList

        val lb = new ListBuffer[ScalaType]()

        def collectSuperDefs(cl: ScalaClassDef): List[ScalaClassDef] = {
          val ab = new ListBuffer[ScalaClassDef]()
          if(cl.superclass != null) {
            for (sc <- cl.superclass) {
              val classDefs = sc match {
                case x: NamedTyp if !x.name.startsWith("java.") => getScalaSignature(x.name)
                case x: NamedTyp if x.name.startsWith("java.") => Nil 
                case InstTyp(base: NamedTyp, args: List[ScalaType]) => {
                  getScalaSignature(base.name)
                }
                case null => null
                case x  => throw new RuntimeException("ScalaType " + x + " not matched in getSupertypes")
              }

              // for generic names (A, B, etc) that do not have classes use scala.Any
              if (classDefs == null) ab.appendAll(List(new ScalaClassDef("scala.Any", "class", null, null, null, 0L, null, new NamedTyp("scala.Any"))))
              else {
                ab.appendAll(classDefs)
                ab.appendAll(classDefs.flatMap(s => collectSuperDefs(s)))
              }
            }
          }

          // ab.toList.distinct
          distinct(ab.toList)
        }

        def subst(t: ScalaType, formals: List[ScalaType], actuals: List[ScalaType]): ScalaType = 
          t match {
            case s: NamedTyp => s
            // case s @ InstTyp(base, args: List[NamedTyp]) => s
            case InstTyp(base, args) => new InstTyp(base, args.map(a => subst(a, formals, actuals))) 
            // case s @ ParamTyp(name, _) => { println("ParamTyp: " + s + " formals " + formals + " actuals " + actuals); (formals zip actuals).find(_._1.name == name) match {
            // There are some cases where the formals contain NamedTyp as well as ParamTyp
            case s @ ParamTyp(name, _) => (formals zip actuals).find(_._1 match { case x: NamedTyp => x.name == name
                                                    case x: ParamTyp => x.name == name
                                                    case _ => false }) match {
                                                      case Some(t) => t._2
                                                      case None => s 
                                                    } 
          }

        def matchFormalToActual(t: ScalaType) = t match {
          case x: NamedTyp => x
          case x @ InstTyp(base, args: List[ParamTyp]) => subst(x, args, actuals)
          case _ => null
        }
     
        for (cd <- bottom) { 
          if( cd.superclass != null) lb ++= cd.superclass.map(sc => matchFormalToActual(sc))
          lb ++= collectSuperDefs(cd).flatMap(d => d.superclass.map(sc => matchFormalToActual(sc)))
        }

        superTypeCache += bottom -> distinct(lb.toList) // lb.toList.distinct
        //lb.toList.distinct
        distinct(lb.toList)
      }

  }

  def lub(typ1: ScalaType, typ2: ScalaType): ScalaType = { 
    if (typ1.equals(typ2)) typ1 else {
        typ1 match {
          case x: NamedTyp => typ2 match {
            case y: NamedTyp => commonAncestor(typ1 :: getSupertypes(x), typ2 :: getSupertypes(y.name))
            case InstTyp(base: NamedTyp, args: List[ScalaType]) => lub(x, base)
          }
          case InstTyp(base1: NamedTyp, args1: List[ScalaType]) => typ2 match {
            case y: NamedTyp => lub(base1, y)
            case InstTyp(base2: NamedTyp, args2: List[ScalaType]) => { 
              val baseLUBs = getScalaSignature(lub(base1, base2).asInstanceOf[NamedTyp].name).head
              val paramTypsWithActuals = baseLUBs.scalatype.asInstanceOf[InstTyp].args.zip(args1 zip args2)

              val newArgs = paramTypsWithActuals.map(t => t._1 match {
                case ParamTyp(name, vr) => vr match {
                  case COVARIANT => Some((t._1, lub(t._2._1, t._2._2)))
                  case CONTRAVARIANT => Some((t._1, commonAncestor(List(t._2._1, new NamedTyp("scala.Nothing")), List(t._2._2, new NamedTyp("scala.Nothing")))))
                }
                // INVARIANT
                case x: NamedTyp => { println("x = " + x + "\nt._2 = " + t._2); if (!t._2._1.equals(t._2._2)) None else Some((t._1, t._2._1)) }
                case _ => new NamedTyp("scala.Any")
              })

              println("lub matched typ1 = " + typ1 + " typ2 = " + typ2)
              // new InstTyp(lub(base1, base2), args1.zip(args2).map(i => lub(i._1, i._2)))

              println("NEW ARGS = " + newArgs.filter(n => n != None).map(a => a match { case Some(x: (NamedTyp, ScalaType)) => x._1 }))
              val argPairs = newArgs.filter(n => n != None).map(a => a match { case Some(x: (NamedTyp, ScalaType)) => x })
              println("Supertypes of lub(base1, base2) = " + getSupertypes(lub(base1, base2)))
              val superTypeMatchingArgs = getSupertypeWithMatchingArgs(lub(base1, base2), argPairs.map(a => a._1 ))
              superTypeMatchingArgs match {
                case InstTyp(base, args) => new InstTyp(base, argPairs.map(na => na._2))
                case x: NamedTyp => x
              }
            }
          }
          case ParamTyp(name: String, typVar: TYPEVARIANT) => typ2 match {
            case y: NamedTyp => lub(y, typ1)
            case y: InstTyp => lub(y, typ1)
            case y: ParamTyp => if(name.equals(y.name)) return y else commonAncestor(getSupertypes(typ1), getSupertypes(y))
          }
          case _ => typ1
        }
    }

      
      /*
        If C has an invariant type parameter X:
        C[..., A, ...] <: C[..., B, ...] iff A=B

        If C has an covariant type parameter X:
        C[..., A, ...] <: C[..., B, ...] if A <: B

        If C has an contravariant type parameter X:
        C[..., A, ...] <: C[..., B, ...] if B <: A 
      */
    }

  private def commonAncestor(lst1: List[ScalaType], lst2: List[ScalaType]): ScalaType = {
    println("commonAncestor: lst1 = " + stripToBaseTypeName(lst1))
    println("lst2 = " + stripToBaseTypeName(lst2))
    println("intersection = " + stripToBaseTypeName(lst1).intersect(stripToBaseTypeName(lst2)))

    // Need to create our own version of intersect to better maintain order
    stripToBaseTypeName(lst1).intersect(stripToBaseTypeName(lst2)).head
  }

  private def getSupertypeWithMatchingArgs(base: ScalaType, args: List[ScalaType]): ScalaType = {
    val matching = (getScalaSignature(base.asInstanceOf[NamedTyp].name).head.scalatype :: getSupertypes(base)).find(st => st match {
      case InstTyp(base, scArgs) => { println("scArgs = " + scArgs); (scArgs zip args).filter(s => s._1 == s._2).length == scArgs.length }
      case x: NamedTyp => true
    })
   
    println("getSupertypeWithMatchingArgs(" + base + ", " + args + ") = " + matching)
    matching match { case Some(x: ScalaType) => x }
  }

  private def distinct[A](list: List[A]): List[A] = list.reverse.distinct.reverse 

  private def stripToBaseTypeName(st: ScalaType): ScalaType = st match {
    case x: NamedTyp => x
    case x: InstTyp => x.base
    case x: ParamTyp => new NamedTyp(x.name)
    case _ => st
  }

  private def stripToBaseTypeName(st: List[ScalaType]): List[ScalaType] = st.map(s => stripToBaseTypeName(s))


  def decorateWithTags(us: SootUnit, typeFlow: TypeFlowAnalysis) = {
    val out = typeFlow.getFlowAfter(us)
    for (u <- us.getDefBoxes)
      u.getValue match {
        case l: Local => us.addTag(new TypeFlowTag(l.getName, out(l.getName))) 
        case x =>  { }
      }
  }

  class TypeFlowTag(name: String, stype: ScalaType) extends Tag {
    def getName = "TypeFlowTag"
    def getValue: Array[Byte] = throw new RuntimeException("TypeFlowTag has no value for bytecode")
    def getLocalName = name
    def getScalaType = stype
  }

}

