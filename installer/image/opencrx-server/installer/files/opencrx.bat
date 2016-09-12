@echo off

rem Run/Stop openCRX Server
rem -----------------------

if ""%1"" == ""run"" goto run
if ""%1"" == ""stop"" goto stop
if ""%1"" == ""run-tomcat"" goto run-opencrx
goto end

:run
start "openCRX Server $PROJECT_VERSION ($TOMCAT_HTTP_PORT)" "$INSTALL_PATH\$TOMEE_BASEDIR\bin\opencrx.bat" run-tomcat
goto end

:run-opencrx
set JAVA_HOME=$JDKPath
set JAVA_HOME=%JAVA_HOME:/=\%

rem Start HSQLDB
if exist "$INSTALL_PATH\data\hsqldb\db.bat" (
  start "HSQLDB" "$INSTALL_PATH\data\hsqldb\db.bat" run
  ping 127.0.0.1 > nul  
)

rem Start TomEE
cd "$INSTALL_PATH\$TOMEE_BASEDIR"
rmdir /s /q temp
rmdir /s /q work
mkdir temp
.\bin\catalina.bat run
goto end

:stop
set JAVA_HOME=$JDKPath
set JAVA_HOME=%JAVA_HOME:/=\%

rem Stop HSQLDB
if exist "$INSTALL_PATH\data\hsqldb\db.bat" (
	start "HSQLDB" "$INSTALL_PATH\data\hsqldb\db.bat" stop
  	ping 127.0.0.1 > nul
)

rem Stop TomEE
cd "$INSTALL_PATH\$TOMEE_BASEDIR"
.\bin\catalina.bat stop
goto end

:end

exit
