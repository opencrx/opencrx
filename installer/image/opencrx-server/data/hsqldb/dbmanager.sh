#!/bin/sh
export JAVA_HOME=$JDKPath
export PATH=$JAVA_HOME/bin:$PATH
cd $INSTALL_PATH/data/hsqldb
java -cp ./hsqldb-2.7.1.jar org.hsqldb.util.DatabaseManagerSwing --url jdbc:hsqldb:hsql://127.0.0.1:$HSQLDB_PORT/CRX --user sa --password manager99
