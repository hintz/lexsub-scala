##-Xrunhprof:cpu=samples,file=lexsub.hprof
java -Xmx10g -classpath ./target/lexsub-scala-0.0.1-SNAPSHOT.jar de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.FilterGoldMWE
