all:
	scalac -cp lib/javacl-1.0-beta-4-shaded.jar:lib/jna.jar -sourcepath src -d bin src/simplecl/*.scala src/simplecl/util/*.scala
