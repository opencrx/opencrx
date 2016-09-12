@echo off
set ANT_HOME=$ANTPath
set JAVA_HOME=$JDKPath
set PATH=%JAVA_HOME%\bin;%ANT_HOME%\bin;%PATH%
ant -f "$INSTALL_PATH\installer\bin\postinstaller.xml" -logfile "$INSTALL_PATH\install.log" postinstall -Dexpand.target=expand-ant
