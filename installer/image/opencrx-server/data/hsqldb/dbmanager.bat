set JAVA_HOME=$JDKPath
set JAVA_HOME=%JAVA_HOME:/=\%
set PATH=%JAVA_HOME%\bin;%PATH%
cd $INSTALL_PATH\data\hsqldb
java -cp .\hsqldb.jar org.hsqldb.util.DatabaseManagerSwing --url jdbc:hsqldb:hsql://127.0.0.1:$HSQLDB_PORT/CRX --user sa --password manager99
