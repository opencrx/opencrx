#!/bin/sh
java -Xmx800M -cp ../../../../../../../../opt/hsqldb/jre-1.8/hsqldb-2/lib/hsqldb.jar org.hsqldb.server.Server --database.0 file:crx --dbname.0 CRX
