#!/bin/sh
cd "$INSTALL_PATH"
export ANT_HOME=$ANTPath
export JAVA_HOME=$JDKPath
export PATH=$JAVA_HOME/bin:$ANT_HOME/bin:$PATH
ant -f "$INSTALL_PATH/installer/bin/postinstaller.xml" -logfile "$INSTALL_PATH/install.log" postinstall -Dexpand.target=expand-native
