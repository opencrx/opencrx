#!/bin/sh
java -Xmx800M -cp $HSQLDB_HOME/lib/hsqldb.jar org.hsqldb.server.Server --database.0 file:crx --acl acl.txt --dbname.0 CRX
