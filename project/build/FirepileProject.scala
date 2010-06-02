import sbt._

class FirepileProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
  override def compileOptions = super.compileOptions ++ compileOptions("-Xprint:typer -Xprint:cleanup -Xprint:specialize -Yoptimise -Ysqueeze:on -Ydead-code -Yinline")
  // override def compileOptions = super.compileOptions ++ compileOptions("-Xlog-implicits")
}

