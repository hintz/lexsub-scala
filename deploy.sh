export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$PATH
mvn package
echo Starting upload..
scp target/lexsub-scala-0.0.1-SNAPSHOT.jar gerold@farnsworth.lt.informatik.tu-darmstadt.de:~/lexsub-scala/target/lexsub-scala-0.0.1-SNAPSHOT.jar
rm dependency-reduced-pom.xml
echo Upload complete.