import sbt._

class FirepileProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  //override def compileOptions = super.compileOptions ++ compileOptions("-Xlog-implicits", "-Xprint:namer", "-Xprint-types", "-Ytyper-debug", "-Ysqueeze:on", "-Yclosure-elim", "-Ydead-code", "-Yinline")
  // override def compileOptions = super.compileOptions ++ compileOptions("-Ysqueeze:on", "-Yclosure-elim", "-Ydead-code", "-Yinline")
  // override def compileOptions = super.compileOptions ++ compileOptions("-Xlog-implicits")
}

