#!/bin/sh

# Run/Stop openCRX Server
# -----------------------

if [ "$1" = "run" ] ; then

  # Start HSQLDB
  if [ -e $INSTALL_PATH/data/hsqldb/db.sh ] ; then 
    $INSTALL_PATH/data/hsqldb/db.sh run &
    sleep 3
  fi

  # Start TomEE
  export JAVA_HOME=$JDKPath
  cd ..
  rm -Rf temp/*
  rm -Rf work/*
  ./bin/catalina.sh run

fi

if [ "$1" = "stop" ] ; then

  # Stop HSQLDB
  if [ -e $INSTALL_PATH/data/hsqldb/db.sh ] ; then 
    $INSTALL_PATH/data/hsqldb/db.sh stop
  fi
  
  # Stop TomEE
  cd ..
  ./bin/catalina.sh stop

fi
