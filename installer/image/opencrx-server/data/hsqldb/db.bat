rem Run/Stop database
rem -----------------

if ""%1"" == ""run"" goto run
if ""%1"" == ""run-hsqldb"" goto run-hsqldb
if ""%1"" == ""stop"" goto stop
goto end

:run
start "HSQLDB ($HSQLDB_PORT)" "$INSTALL_PATH\data\hsqldb\db.bat" run-hsqldb
goto end

:run-hsqldb
set JAVA_HOME=$JDKPath
set JAVA_HOME=%JAVA_HOME:/=\%
set PATH=%JAVA_HOME%\bin;%PATH%
cd $INSTALL_PATH\data\hsqldb
java -Xmx800M -cp .\hsqldb.jar org.hsqldb.server.Server --port $HSQLDB_PORT --database.0 file:crx --dbname.0 CRX
goto end

:stop
set JAVA_HOME=$JDKPath
set JAVA_HOME=%JAVA_HOME:/=\%
set PATH=%JAVA_HOME%\bin;%PATH%
cd $INSTALL_PATH\data\hsqldb
java -jar ./sqltool.jar --sql "shutdown;" --rcFile=sqltool.rc CRX
goto end

:end

exit
