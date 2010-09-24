import sbt._
import java.io.File

class FirepileProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  override def localScala = defineScala("2.8.0-local", new File("/opt/local/share/scala-2.8")) :: Nil

  //override def compileOptions = super.compileOptions ++ compileOptions("-Xlog-implicits", "-Xprint:namer", "-Xprint-types", "-Ytyper-debug", "-Ysqueeze:on", "-Yclosure-elim", "-Ydead-code", "-Yinline")
  // override def compileOptions = super.compileOptions ++ compileOptions("-Ysqueeze:on", "-Yclosure-elim", "-Ydead-code", "-Yinline")
  // override def compileOptions = super.compileOptions ++ compileOptions("-Xlog-implicits")
}

