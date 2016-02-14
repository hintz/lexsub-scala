#!/bin/sh

# multiwordnet
mvn deploy:deploy-file -Durl=file://$(dirname $0) -Dfile=jmwn-1.2.jar -DgroupId=org.itc -DartifactId=org.itc.mwn -Dpackaging=jar -Dversion=1.0.0

# ranklib
mvn deploy:deploy-file -Durl=file://$(dirname $0) -Dfile=RankLib-2.1.jar -DgroupId=ciir.umass.edu -DartifactId=ciir.umass.edu.RankLib -Dpackaging=jar -Dversion=2.1.0

