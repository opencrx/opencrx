#!/bin/sh

#  Run/Stop database
# ------------------

if [ "$1" = "run" ] ; then

  export JAVA_HOME=$JDKPath
  export PATH=$JDKPath/bin:$PATH
  cd $INSTALL_PATH/data/hsqldb
  java -Xmx800M -Dhsqldb.port=$HSQLDB_PORT -cp ./hsqldb-2.7.1.jar org.hsqldb.server.Server --port $HSQLDB_PORT --acl acl.txt --database.0 file:crx --dbname.0 CRX

fi

if [ "$1" = "stop" ] ; then

  export JAVA_HOME=$JDKPath
  export PATH=$JDKPath/bin:$PATH
  cd $INSTALL_PATH/data/hsqldb
  java -jar ./sqltool.jar --sql 'shutdown;' --rcFile=sqltool.rc CRX
  
fi
