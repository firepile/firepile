all:
	fsc -cp lib/javacl-1.0-beta-4-shaded.jar:lib/jna.jar -sourcepath src -d bin src/simplecl/*.scala src/simplecl/util/*.scala src/simplecl/tree/*.scala src/simplecl/tests/*.scala

run:
	scala -cp lib/javacl-1.0-beta-4-shaded.jar:lib/jna.jar:bin simplecl.tests.OpenCLScalaTest
run2:
	scala -cp lib/javacl-1.0-beta-4-shaded.jar:lib/jna.jar:bin simplecl.tests.OpenCLScalaTest2
run3:
	scala -cp lib/javacl-1.0-beta-4-shaded.jar:lib/jna.jar:bin simplecl.tests.OpenCLScalaTest3

treetest:
	scala -cp bin simplecl.tree.Trees

