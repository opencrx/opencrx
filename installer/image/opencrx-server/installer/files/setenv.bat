REM BEGIN openCRX/CRX
set JAVA_OPTS=%JAVA_OPTS% -Xmx800M
set JAVA_OPTS=%JAVA_OPTS% -Dfile.encoding=UTF-8
set JAVA_OPTS=%JAVA_OPTS% -Dorg.openmdx.kernel.collection.InternalizedKeyMap.TrustInternalization=true
set JAVA_OPTS=%JAVA_OPTS% -Dorg.opencrx.maildir="%CATALINA_BASE%\maildir"
REM set JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote
REM set JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.port=8001 
REM set JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.authenticate=false
REM set JAVA_OPTS=%JAVA_OPTS% -Dcom.sun.management.jmxremote.ssl=false
REM JAVA_OPTS=%JAVA_OPTS% -Dorg.opencrx.usesendmail.CRX=false
REM JAVA_OPTS=%JAVA_OPTS% -Dorg.opencrx.mediadir="%CATALINA_BASE%\mediadir"
REM JAVA_OPTS=%JAVA_OPTS% -Dorg.openmdx.persistence.jdbc.useLikeForOidMatching=false
REM JAVA_OPTS=%JAVA_OPTS% -Djavax.jdo.option.TransactionIsolationLevel=read-committed
REM END openCRX/CRX

echo Using JAVA_OPTS:       "%JAVA_OPTS%"
