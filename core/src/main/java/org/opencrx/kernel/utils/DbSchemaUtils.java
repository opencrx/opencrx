/*
 * ====================================================================
v * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DbSchemaUtils
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011, CRIXP Corp., Switzerland
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of CRIXP Corp. nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.kernel.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.document1.jmi1.Media;
import org.opencrx.kernel.layer.persistence.Media_2;
import org.opencrx.kernel.tools.FastResultSet;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.Database_2;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.loading.Classes;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * DbSchemaUtils
 */
public class DbSchemaUtils {

	public static String getJdbcDriverName(
		String connectionUrl
	) {
		if(connectionUrl.startsWith("jdbc:postgresql:")) {
			return "org.postgresql.Driver";						
		} else if(connectionUrl.startsWith("jdbc:mysql:")) {
			return "com.mysql.jdbc.Driver";						
		} else if(connectionUrl.startsWith("jdbc:hsqldb:")) {
			return "org.hsqldb.jdbc.JDBCDriver";					
		} else if(connectionUrl.startsWith("jdbc:db2:")) {
			return "com.ibm.db2.jcc.DB2Driver";
		} else if(connectionUrl.startsWith("jdbc:as400:")) {
			return "com.ibm.as400.access.AS400JDBCDriver";
		} else if(connectionUrl.startsWith("jdbc:oracle:")) {
			return "oracle.jdbc.driver.OracleDriver";			
		} else if(connectionUrl.startsWith("jdbc:sqlserver:")) {
			return "com.microsoft.sqlserver.jdbc.SQLServerDriver";
		} else {
			return null;
		}
	}

	/**
	 * MigrationDefinition
	 *
	 */
	static class MigrationDefinition {

		public MigrationDefinition(
			String name,
			String testForReleaseStatement,
			List<String> migrateStatements
		) {
			this.name = name;
			this.testForVersionStatement = testForReleaseStatement;
			this.migrationStatements = migrateStatements;
		}
		
		public String getName() {
			return name;
		}
		
		public String getTestForVersionStatement() {
			return testForVersionStatement;
		}
		
		public List<String> getMigrationStatements() {
			return migrationStatements;
		}

		private final String name;
		private final String testForVersionStatement;
		private final List<String> migrationStatements;
	}

	/**
	 * Execute updates on given connection.
	 * 
	 * @param conn
	 * @param commands
	 * @throws ServiceException
	 */
	private static void executeUpdate(
		Connection conn,
		List<String> commands,
		boolean logExceptions
	) throws ServiceException {
		try {
			for(String command: commands) {
				PreparedStatement ps = null;
				try {
					ps = conn.prepareStatement(command);
					ps.executeUpdate();
					ps.close();
				} catch(Exception e) {
					if(logExceptions) {
						new ServiceException(e).log();
					}
				} finally {
					if(ps != null) {
						ps.close();
					}
				}
			}
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get connection to database holding the schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Connection getSchemaConnection(
	) throws ServiceException {
		try {
			// Schema database as mem: database
			Connection connSchema = DriverManager.getConnection("jdbc:hsqldb:mem:dbschema", "sa", "");
			if(!schemaPrepared) {
				// Clone res:org/opencrx/kernel/tools/resource/crx
				try {
					Properties properties = new Properties();
					properties.put("database", "org/opencrx/kernel/tools/resource/crx");
					properties.put("user", "SA");
					properties.put("password", "manager99");
					Connection conn = DriverManager.getConnection("jdbc:hsqldb:res:", properties);
					// Get schema
					PreparedStatement ps = conn.prepareStatement("script");
					ResultSet rs = ps.executeQuery();
					List<String> commands = new ArrayList<String>();
					while(rs.next()) {
						commands.add(rs.getString("Command"));
					}
					rs.close();
					ps.close();
					// Clone to target
					executeUpdate(connSchema, commands, false);
					conn.close();
				} catch(Exception e) {
					throw new ServiceException(e);
				}
				// Apply custom-extensions
				Enumeration<URL> schemaResources = Classes.getResources("META-INF/dbschema-add.sql");
				while(schemaResources.hasMoreElements()) {
					InputStream is = schemaResources.nextElement().openStream();
					List<String> commands = new ArrayList<String>();
					BufferedReader in = new BufferedReader(new InputStreamReader(is, "UTF-8"));
					String command = null;
					while((command = in.readLine()) != null) {
						commands.add(command);
					}
					in.close();
					executeUpdate(connSchema, commands, true);
				}
				schemaPrepared = true;
			}
			return connSchema;
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getSchema(
		Connection conn
	) throws ServiceException {
		List<String> schema = new ArrayList<String>();
		try {
			{
				PreparedStatement ps = conn.prepareStatement("script");
				ResultSet rs = ps.executeQuery();
				while(rs.next()) {
					schema.add(
						rs.getString("Command")
					);
				}
				rs.close();
				ps.close();
			}
		} catch(Exception e) {
			throw new ServiceException(e);
		} finally {
		}
		return schema;
	}

	/**
	 * Get definition for given db object from reference schema.
	 * 
	 * @param type
	 * @param object
	 * @param schema
	 * @param targetDatabaseName
	 * @param replaceObject
	 * @return
	 */
	public static String getObjectDefinition(
		String type,
		String object,
		List<String> schema,
		String targetDatabaseName,
		boolean replaceObject
	) {
		for(String command: schema) {
			if(
				command.indexOf(type) >= 0 && 
				(command.indexOf(object + "(") > 0 || command.indexOf(object + " ") > 0)
			) {
				command = command.replace(CREATE_TABLE_PREFIX, "CREATE TABLE ");
				command = command.replace(CREATE_VIEW_PREFIX, "CREATE VIEW ");
				command = command.replace(" PUBLIC.", " ");
				command = command.replace(",PUBLIC.", ", ");
				// PostgreSQL
				if(targetDatabaseName.indexOf("PostgreSQL") >=0) {
					if(type.equals(CREATE_SEQUENCE_PREFIX)) {
						command = command.replace("AS INTEGER", "");
					} else {
						if(replaceObject) {
							command = command.replace("CREATE VIEW ", "CREATE OR REPLACE VIEW ");
						}
						command = mapColumnDefinition(targetDatabaseName, command);
					}
				}
				// HSQLDB
				else if(targetDatabaseName.indexOf("HSQL") >=0) {
					if(replaceObject) {
						command = command.replace("CREATE VIEW ", "ALTER VIEW ");
						// View specification for ALTER VIEW does not work since 2.4.0
						int pos1 = command.indexOf("(");
						if(pos1 > 0) {
							int pos2 = command.indexOf(")", pos1);
							command = command.substring(0,  pos1) + command.substring(pos2 + 1);
						}
					}
				}
				// MySQL
				else if(targetDatabaseName.indexOf("MySQL") >=0) {
					if(replaceObject) {
						command = command.replace("CREATE VIEW ", "CREATE OR REPLACE VIEW ");
					}
					command = mapColumnDefinition(targetDatabaseName, command);
				}
				// DB2
				else if(targetDatabaseName.indexOf("DB2") >=0) {
					if(replaceObject) {
						// REPLACE not supported for UDB versions
						if(targetDatabaseName.indexOf("UDB") < 0) {
							command = command.replace("CREATE VIEW ", "CREATE OR REPLACE VIEW ");
						}
					}
					command = mapColumnDefinition(targetDatabaseName, command);
				}
				// Oracle
				else if(targetDatabaseName.indexOf("Oracle") >=0) {
					if(type.equals(CREATE_SEQUENCE_PREFIX)) {
						command = command.replace("AS INTEGER", "");
					} else {					
						if(replaceObject) {
							command = command.replace("CREATE VIEW ", "CREATE OR REPLACE VIEW ");
						}
						command = mapColumnDefinition(targetDatabaseName, command);
					}
				}
				// Microsoft
				else if(targetDatabaseName.indexOf("Microsoft") >=0) {
					if(replaceObject) {
						command = command.replace("CREATE VIEW ", "ALTER VIEW ");
					}
					command = mapColumnDefinition(targetDatabaseName, command);
				}
				return command;
			}
		}
		return null;
	}

	/**
	 * Map HSQLDB column definition to target database specific column definition.
	 * 
	 * @param targetDatabaseName
	 * @param command
	 * @return
	 */
	public static String mapColumnDefinition(
		String targetDatabaseName,
		String command
	) {
		if(targetDatabaseName.indexOf("HSQL") < 0) {
			command = command.replace("CLOB(10000000)", "CLOB");
			command = command.replace("VARBINARY(10000000)", "VARBINARY");
			command = command.replace("\"P$$PARENT\"", "P$$PARENT");
			command = command.replace("\"NAME\"", "NAME");
			command = command.replace("\"TYPE\"", "TYPE");
			command = command.replace("\"SCOPE\"", "SCOPE");
			command = command.replace("\"LANGUAGE\"", "LANGUAGE");
			command = command.replace("\"POSITION\"", "POSITION");
			command = command.replace("\"STATE\"", "STATE");
			command = command.replace("\"EXCEPTION\"", "EXCEPTION");
			command = command.replace("\"DOMAIN\"", "DOMAIN");
			command = command.replace("\"NUMBER\"", "NUMBER");
			command = command.replace("\"ACTION\"", "ACTION");
			command = command.replace("\"TEXT\"", "TEXT");
		}
		if(targetDatabaseName.indexOf("PostgreSQL") >=0) {
			// PostgreSQL
			command = command.replace(" TIMESTAMP", " TIMESTAMP WITH TIME ZONE");
			command = command.replace(" CLOB,", " TEXT,");
			command = command.replace(" VARBINARY,", " BYTEA,");
			command = command.replace(" CLOB)", " TEXT)");
			command = command.replace(" VARBINARY)", " BYTEA)");
			command = command.replace("'\\'", "E'\\\\'");
			command = command.replace("'AS DTYPE", "'::text AS DTYPE");
			command = command.replace(" CHAR(", " CHR(");
			while(command.indexOf("ROW_NUMBER()OVER()") > 0) {
				int pos = command.indexOf("ROW_NUMBER()OVER()");
				// Get next ORDER BY and use it as OVER() argument
				int posOrderBy1 = command.indexOf("ORDER BY", pos);
				int posOrderBy2 = command.indexOf(")", posOrderBy1);
				String orderBy = command.substring(posOrderBy1, posOrderBy2);
				command =
					command.substring(0, pos) +
					"ROW_NUMBER()OVER(" + orderBy + ")" +
					command.substring(pos + 18, posOrderBy1) +
					command.substring(posOrderBy2);
			}
		} else if(targetDatabaseName.indexOf("HSQL") >=0) {
			// HSQLDB
		} else if(targetDatabaseName.indexOf("MySQL") >=0) {
			// MySQL
			command = command.replace(" TIMESTAMP", " DATETIME");					
			command = command.replace(" BOOLEAN,", " BIT,");
			command = command.replace(" CLOB,", " LONGTEXT,");
			command = command.replace(" VARBINARY,", " BLOB,");
			command = command.replace(" BOOLEAN)", " BIT)");
			command = command.replace(" CLOB)", " LONGTEXT)");
			command = command.replace(" VARBINARY)", " BLOB)");
			command = command.replace("'\\'", "'\\\\'");
		} else if(targetDatabaseName.indexOf("DB2") >=0) {
			// DB2		
			command = command.replace(" BOOLEAN,", " SMALLINT,");
			command = command.replace(" VARBINARY,", " BLOB,");
			command = command.replace(" BOOLEAN)", " SMALLINT)");
			command = command.replace(" VARBINARY)", " BLOB)");
			command = command.replace("SUBSTRING(", "SUBSTR(");
			command = command.replace(" CHAR(", "CHR(");					
			command = command.replace(" WITH RECURSIVE", " WITH");
		} else if(targetDatabaseName.indexOf("Oracle") >=0) {
			// Oracle
			command = command.replace(" VARCHAR(", " VARCHAR2(");
			command = command.replace(" SMALLINT,", " NUMBER,");
			command = command.replace(" BOOLEAN,", " NUMBER,");					
			command = command.replace(" VARBINARY,", " BLOB,");
			command = command.replace(" BIGINT,", " INTEGER,");
			command = command.replace(" SMALLINT)", " NUMBER)");
			command = command.replace(" BOOLEAN)", " NUMBER)");					
			command = command.replace(" VARBINARY)", " BLOB)");
			command = command.replace(" BIGINT)", " INTEGER)");
			command = command.replace(",COMMENT ", ",\"comment\" ");
			command = command.replace(",NUMBER ", ",\"number\" ");
			command = command.replace("RESOURCE,", "\"resource\",");
			command = command.replace("RESOURCE)", "\"resource\")");
			command = command.replace(" RESOURCE ", " \"resource\" ");
			command = command.replace("SUBSTRING(", "SUBSTR(");
			command = command.replace(" CHAR(", "CHR(");						
			command = command.replace(" WITH RECURSIVE", " WITH");
		} else if(targetDatabaseName.indexOf("Microsoft") >=0) {
			// Microsoft
			command = command.replace("||", "+");
			command = command.replace(" VARCHAR(", " NVARCHAR(");
			command = command.replace(" DATE,", " DATETIME2,");
			command = command.replace(" TIMESTAMP", " DATETIME2");
			command = command.replace(" BOOLEAN,", " BIT,");
			command = command.replace(" CLOB,", " NTEXT,");
			command = command.replace(" VARBINARY,", " IMAGE,");					
			command = command.replace(" DATE)", " DATETIME2)");
			command = command.replace(" BOOLEAN)", " BIT)");
			command = command.replace(" CLOB)", " NTEXT)");
			command = command.replace(" VARBINARY)", " IMAGE)");
			command = command.replace(" WITH RECURSIVE", " WITH");
			command = command.replace("(ASS0.OBJECT_ID)+'*+1'", " CAST(ASS0.OBJECT_ID+'*+1' AS VARCHAR)");
			command = command.replace("(ASS1.OBJECT_ID)+'*+'+(TEMP.DISTANCE+1)", " CAST(ASS1.OBJECT_ID+'*+'+(TEMP.DISTANCE+1) AS VARCHAR)");
			while(command.indexOf("ROW_NUMBER()OVER()") > 0) {
				int pos = command.indexOf("ROW_NUMBER()OVER()");
				// Get next ORDER BY and use it as OVER() argument				
				int posOrderBy1 = command.indexOf("ORDER BY", pos);
				int posOrderBy2 = command.indexOf(")", posOrderBy1);
				String orderBy = command.substring(posOrderBy1, posOrderBy2);
				command =
					command.substring(0, pos) +
					"ROW_NUMBER()OVER(" + orderBy + ")" +
					command.substring(pos + 18, posOrderBy1) +
					command.substring(posOrderBy2);
			}
		}
		return command;
	}

	/**
	 * Get all table names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getTableNames(
	) throws ServiceException {
		Connection connS = getSchemaConnection();
		try {
			return getTableNames(connS);
		} finally {
			try {
				connS.close();
			} catch(Exception ignore) {}
		}
	}

	/**
	 * Get all table names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getTableNames(
		Connection connS
	) throws ServiceException {
		Set<String> tableNames = new TreeSet<String>();
		List<String> schema = getSchema(connS);
		for(String command: schema) {
			if(command.startsWith(CREATE_TABLE_PREFIX)) {
				tableNames.add(
					command.substring(
						CREATE_TABLE_PREFIX.length(),
						command.indexOf("(", CREATE_TABLE_PREFIX.length())
					).trim()
				);
			}
		}
		return new ArrayList<String>(tableNames);
	}

	/**
	 * Get all view names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getViewNames(
		Connection connS
	) throws ServiceException {
		Set<String> viewNames = new TreeSet<String>();
		List<String> schema = getSchema(connS);
		for(String command: schema) {
			if(command.startsWith(CREATE_VIEW_PREFIX)) {
				viewNames.add(
					command.substring(
						CREATE_VIEW_PREFIX.length(),
						command.indexOf("(", CREATE_VIEW_PREFIX.length())
					).trim()
				);
			}
		}
		return new ArrayList<String>(viewNames);
	}

	/**
	 * Get all view names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getViewNames(
	) throws ServiceException {
		Connection connS = getSchemaConnection();
		try {
			return getViewNames(connS);
		} finally {
			try {
				connS.close();
			} catch(Exception ignore) {}
		}
	}

    /**
     * Get column names for given db object.
     * 
     * @param dbObject
     * @param includeColumnTypes
     * @param excludeColumnTypes
     * @return
     * @throws ServiceException
     */
    public static List<String> getColumnNames(
    	String dbObject,
    	List<Integer> includeColumnTypes,
    	List<Integer> excludeColumnTypes
    ) throws ServiceException {
    	List<String> columnNames = new ArrayList<String>();
    	Connection conn = null;
		try {
			conn = getSchemaConnection();
			String statement = "SELECT * FROM " + dbObject + " WHERE 1=0";
			PreparedStatement ps = conn.prepareStatement(statement);		
			ResultSet rs = null;
			try {
				rs = ps.executeQuery();					
				ResultSetMetaData rsmd = rs.getMetaData();
			    for(int i = 0; i < rsmd.getColumnCount(); i++) {
			    	boolean includeColumn = includeColumnTypes == null || includeColumnTypes.isEmpty() || includeColumnTypes.contains(rsmd.getColumnType(i + 1));
			    	boolean excludeColumn = excludeColumnTypes != null && !excludeColumnTypes.isEmpty() && excludeColumnTypes.contains(rsmd.getColumnType(i + 1));
			    	if(includeColumn && !excludeColumn) {
			    		columnNames.add(rsmd.getColumnName(i + 1).toLowerCase());
			    	}
			    }
			} catch(Exception e) {
				throw new ServiceException(e);
			} finally {
				try {
					rs.close();
				} catch(Exception e) {}
				try {
					ps.close();
				} catch(Exception e) {}		
			}
		} catch(SQLException e) {
			throw new ServiceException(e);
		} finally {
			try {
				conn.close();
			} catch(Exception e0) {}
		}
		return columnNames;
    }

	/**
	 * Get all index names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getIndexNames(
		Connection connS
	) throws ServiceException {
		Set<String> indexNames = new TreeSet<String>();
		List<String> schema = getSchema(connS);
		for(String command: schema) {
			if(command.startsWith(CREATE_INDEX_PREFIX)) {
				indexNames.add(
					command.substring(
						CREATE_INDEX_PREFIX.length(),
						command.indexOf(" ", CREATE_INDEX_PREFIX.length() + 1)
					).trim()
				);
			}
		}
		return new ArrayList<String>(indexNames);
	}

	/**
	 * Get all index names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getIndexNames(
	) throws ServiceException {
		Connection connS = getSchemaConnection();
		try {
			return getIndexNames(connS);
		} finally {
			try {
				connS.close();
			} catch(Exception ignore) {}
		}
	}

	/**
	 * Get all sequence names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getSequenceNames(
		Connection connS
	) throws ServiceException {
		Set<String> sequenceNames = new TreeSet<String>();
		List<String> schema = getSchema(connS);
		for(String command: schema) {
			if(command.startsWith(CREATE_SEQUENCE_PREFIX)) {
				sequenceNames.add(
					command.substring(
						CREATE_SEQUENCE_PREFIX.length(),
						command.indexOf(" ", CREATE_SEQUENCE_PREFIX.length() + 1)
					).trim()
				);
			}
		}
		return new ArrayList<String>(sequenceNames);
	}

	/**
	 * Get all sequence names from reference schema.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> getSequenceNames(
	) throws ServiceException {
		Connection connS = getSchemaConnection();
		try {
			return getSequenceNames(connS);
		} finally {
			try {
				connS.close();
			} catch(Exception ignore) {}
		} 
	}

	/**
	 * Compare tables of given database with reference schema.
	 * 
	 * @param connT
	 * @param fix
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> validateTables(
		Connection connT,
		boolean fix
	) throws ServiceException {
		Connection connS = null;
		List<String> report = new ArrayList<String>();
		try {			
			connS = getSchemaConnection();
			List<String> schema = getSchema(connS);
			List<String> tableNames = getTableNames(connS);
			for(String tableName: tableNames) {
				String statement = "SELECT * FROM " + tableName + " WHERE 1=0";
				PreparedStatement psT = null;
				FastResultSet rsT = null;
				boolean exists = false;
				try {
					psT = connT.prepareStatement(statement);
					rsT = new FastResultSet(
						psT.executeQuery()
					);
					exists = true;
				} catch(Exception e) {
					if(!fix) {
						report.add("ERROR: Missing table=" + tableName + " (message=" + e + ")");
					}
				} finally {
					try {
						rsT.close();
					} catch(Exception e) {}
					try {
						psT.close();
					} catch(Exception e) {}
				}
				if(!exists) {
					statement = getObjectDefinition(CREATE_TABLE_PREFIX, tableName, schema, connT.getMetaData().getDatabaseProductName(), false); 
					if(fix) {
						PreparedStatement psFix = null;
						try {
							report.add("SQL: " + statement);
							psFix = connT.prepareStatement(statement);
							psFix.executeUpdate();
						} catch(Exception e0) {
							report.add("ERROR: Creation of table " + tableName + " failed (message=" + e0 + ")");
						} finally {
							try {
								psFix.close();
							} catch(Exception e0) {}
						}
					} else {
						report.add("FIX: " + statement);						
					}
				}
			}
		} catch(Exception e) {
			throw new ServiceException(e);
		} finally {
			try {
				connS.close();
			} catch(Exception e0) {}
		}
		return report;
	}
	
	/**
	 * Compare columns of all tables with reference schema.
	 * @param connT
	 * @param fix
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> validateTableColumns(
		Connection connT,
		boolean fix
	) throws ServiceException {
		Connection connS = null;
		List<String> report = new ArrayList<String>();
		try {
			connS = getSchemaConnection();
			List<String> schema = getSchema(connS); 
			List<String> tableNames = getTableNames(connS);
			for(String tableName: tableNames) {
				String statement = "SELECT * FROM " + tableName + " WHERE 1=0";
				PreparedStatement psT = null;
				FastResultSet rsT = null;
				try {
					psT = connT.prepareStatement(statement);
					rsT = new FastResultSet(
						psT.executeQuery()
					);
				} catch(Exception e) {					
				} finally {
					try {
						rsT.close();
					} catch(Exception e) {}
					try {
						psT.close();
					} catch(Exception e) {}					
				}
				PreparedStatement psS = connS.prepareStatement(statement);		
				FastResultSet rsS = null;
				try {
					rsS = new FastResultSet(
						psS.executeQuery()
					);					
				} catch(Exception e) {					
				} finally {
					try {
						rsS.close();
					} catch(Exception e) {}
					try {
						psS.close();
					} catch(Exception e) {}					
				}
				if(rsT != null && rsS != null) {
					if(!rsT.getColumnNames().containsAll(rsS.getColumnNames())) {
						List<String> missingColumns = new ArrayList<String>(rsS.getColumnNames());
						missingColumns.removeAll(rsT.getColumnNames());
						statement = getObjectDefinition(CREATE_TABLE_PREFIX, tableName, schema, connT.getMetaData().getDatabaseProductName(), false); 							
						for(String columnName: missingColumns) {
							if(!fix) {
								report.add("ERROR: missing column in table=" + tableName + ", column=" + columnName);								
							}
							// pos1: beginning of column definition
							int pos1 = statement.indexOf("(" + columnName.toUpperCase() + " ");
							if(pos1 < 0) {
								pos1 = statement.indexOf("," + columnName.toUpperCase() + " ");
							}
							if(pos1 < 0) {
								pos1 = statement.indexOf("(\"" + columnName.toUpperCase() + "\" ");
							}
							if(pos1 < 0) {
								pos1 = statement.indexOf(",\"" + columnName.toUpperCase() + "\" ");
							}
							if(pos1 > 0) {
								// pos2: end of column definition
								int pos2 = statement.indexOf(",", pos1+1);
								if(pos2 < 0) {
									pos2 = statement.length() - 1;
								} else {
									// handle case DECIMAL(?,?)
									if(Character.isDigit(statement.charAt(pos2+1))) {
										pos2 = statement.indexOf(",", pos2+1);
										if(pos2 < 0) {
											pos2 = statement.length();
										}
									}
								}
								if(pos2 > 0) {
									PreparedStatement psFix = null;
									String addColumnStatement = "ALTER TABLE " + tableName + " ADD " + statement.substring(pos1+1, pos2);
									if(fix) {
										try {
											report.add("SQL: " + addColumnStatement);
											psFix = connT.prepareStatement(addColumnStatement);
											psFix.executeUpdate();
										} catch(Exception e0) {
											report.add("ERROR: Adding column failed (message=" + e0 + ")");
										} finally {
											if(psFix != null) {
												try {
													psFix.close();
												} catch(Exception e0) {}
											}
										}
									} else {
										report.add("FIX: " + addColumnStatement);
									}
								}
							}
						}
					} else {
						report.add("OK: Table=" + tableName);
					}
					if(!rsS.getColumnNames().containsAll(rsT.getColumnNames())) {
						List<String> extraColumns = new ArrayList<String>(rsT.getColumnNames());
						extraColumns.removeAll(rsS.getColumnNames());
						report.add("WARN: Extra or custom columns in table=" + tableName + ", columns=" + extraColumns);						
					}
				}
			}
		} catch(Exception e) {
			throw new ServiceException(e);
		} finally {
			try {
				connS.close();
			} catch(Exception e0) {}
		}
		return report;
	}

	/**
	 * Validate existence of views with reference schema.
	 * @param connT
	 * @param fix
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> validateViews(
		Connection connT,
		boolean fix
	) throws ServiceException {
		Connection connS = null;
		List<String> report = new ArrayList<String>();
		try {
			connS = getSchemaConnection();
			List<String> schema = getSchema(connS);			
			List<String> viewNames = getViewNames(connS);
			for(String viewName: viewNames) {
				String statement = "SELECT * FROM " + viewName + " WHERE 1=0";
				PreparedStatement psT = null;
				FastResultSet rsT = null;
				boolean exists = false;
				try {
					psT = connT.prepareStatement(statement);
					rsT = new FastResultSet(
						psT.executeQuery()
					);
					exists = true;
				} catch(Exception ignore) {					
				} finally {
					try {
						rsT.close();
					} catch(Exception e) {}
					try {
						psT.close();
					} catch(Exception e) {}					
				}
				statement = getObjectDefinition(CREATE_VIEW_PREFIX, viewName, schema, connT.getMetaData().getDatabaseProductName(), exists); 
				if(fix) {
					PreparedStatement psFix = null;
					try {
						report.add("SQL: " + statement);
						psFix = connT.prepareStatement(statement);
						psFix.executeUpdate();
					} catch(Exception e) {
						if(!OPTIONAL_DBOBJECTS.contains(viewName)) {
							report.add("ERROR: Create/Replace of view " + viewName + " failed (message=" + e + ")");
						}
					} finally {
						try {
							psFix.close();
						} catch(Exception e0) {}
					}		
				} else {
					if(exists) {
						report.add("OK: View " + viewName);
					} else {
						if(!OPTIONAL_DBOBJECTS.contains(viewName)) {
							report.add("FIX: " + statement);
						}
					}
				}
			}
		} catch(Exception e) {
			throw new ServiceException(e);
		} finally {
			try {
				connS.close();
			} catch(Exception ignore) {}
		}
		return report;
	}

	/**
	 * Get identity patterns for given class in case the class has references matching the 
	 * referenceName and the identity pattern matches the segment authority.
	 * 
	 * @param classDef
	 * @param segmentIdentity
	 * @param referenceName
	 * @return
	 * @throws ServiceException
	 */
	protected static List<Path> getIdentityPatterns(
		ModelElement_1_0 classDef,
		Path segmentIdentity,
		String referenceName
	) throws ServiceException {
		Model_1_0 model = classDef.getModel();
		List<Path> xriPatterns = new ArrayList<Path>();
		if(model.getFeatureDef(classDef, referenceName, false) != null) {
            Path identityPattern = model.getIdentityPattern(classDef);
            if(identityPattern == null) {
            	// Get <<root>> class in case of an abstract model pattern
            	ModelElement_1_0 compositeReference = model.getCompositeReference(classDef);
            	List<String> suffix = new ArrayList<String>();
            	while(compositeReference != null) {
            		suffix.add((String)compositeReference.getName());
            		suffix.add(":*");
            		classDef = model.getElement(model.getElement(compositeReference.getExposedEnd()).getType());
            		compositeReference = model.getCompositeReference(classDef);
            	}
            	// Include all sub-classes of the <<root>> class having an identity pattern
            	for(Object subtype: classDef.objGetList("subtype")) {
            		ModelElement_1_0 subClassDef = model.getElement(subtype);
            		identityPattern = model.getIdentityPattern(subClassDef);
            		if(identityPattern != null) {
            			for(String component: suffix) {
            				identityPattern = identityPattern.getChild(component);
            			}
        	            if(
        	            	segmentIdentity.isLike(identityPattern.getPrefix(5)) &&
        	            	identityPattern.size() < 15 // no persistence configuration of longer paths
        	            ) {
        	            	xriPatterns.add(
        	            		segmentIdentity.getDescendant(identityPattern.getSuffix(5)).getDescendant(referenceName, ":*")
        	            	);
        	            }
            		}    		
            	}
            } else if(identityPattern != null) {
	            if(segmentIdentity.isLike(identityPattern.getPrefix(5))) {
	            	xriPatterns.add(
	            		segmentIdentity.getDescendant(identityPattern.getSuffix(5)).getDescendant(referenceName, ":*")
	            	);
	            }
            }
		}		
		return xriPatterns;
	}

	/**
	 * Recursiviley list files of dir.
	 * 
	 * @param dir
	 * @return
	 */
	protected static Set<File> listFilesRecursively(
		File dir
	) {
		Set<File> files = new TreeSet<File>();
		if(dir.isDirectory()) {
			if(!HIDDEN_FILES.contains(dir.getName())) {
				for(File f: dir.listFiles()) {
					if(f.isDirectory()) {
						files.addAll(listFilesRecursively(f));
					} else {
						files.add(f);
					}
				}
			}
		}
		return files;
	}

	/**
	 * Migrate media to file system in case org.opencrx.mediadir.* is set.
	 * 
	 * @param segment
	 * @throws ServiceException
	 */
	public static List<String> migrateMediaToFS(
		String providerName,
		PersistenceManager pm,
		Connection connT,
		boolean validateOnly
	) throws ServiceException {
		List<String> report = new ArrayList<String>();
		if(System.getProperty("org.opencrx.mediadir." + providerName) != null) {
	        File mediadir = new File(System.getProperty("org.opencrx.mediadir." + providerName));
	        Set<File> existingMediaFiles = listFilesRecursively(mediadir);
			Database_2 db = DbSchemaUtils.getDatabasePlugIns()[0];
			try {
				PreparedStatement psT = connT.prepareStatement("SELECT object_id FROM OOCKE1_MEDIA ORDER BY object_id");
				ResultSet rsT = psT.executeQuery();
				int count = 0;
				int countEmptyMedia = 0;
				while(rsT.next()) {
					String objectId = rsT.getString(1);
					Path mediaIdentity = db.getReference(
						connT, 
						objectId
					).getChild(
						objectId.substring(objectId.lastIndexOf("/") + 1)
					);
					boolean unsupportedMediaIdentity = false;
					for(int i = 1; i < mediaIdentity.size(); i++) {
						if(mediaIdentity.get(i).indexOf(":") >= 0) {
							unsupportedMediaIdentity = true;
							break;
						}
					}
					if(unsupportedMediaIdentity) {
						report.add("ERROR: Unsupported media identity " + mediaIdentity.toXRI() + ". Qualifiers must not contain [:]");
						SysLog.error("ERROR: Unsupported media identity " + mediaIdentity.toXRI() + ". Qualifiers must not contain [:]");						
					} else {
						File contentDir = Media_2.toContentDir(mediadir, mediaIdentity);
						File contentFile = new File(contentDir, mediaIdentity.getLastSegment().toString());
						if(validateOnly) {
							if(!existingMediaFiles.contains(contentFile)) {
								Media media = (Media)pm.getObjectById(mediaIdentity);
								BinaryLargeObject mediaContent =  media.getContent();
								long length = 0L;
								try {
									length = mediaContent == null ? 0 : mediaContent.getLength();
								} catch(Exception e) {
									throw new ServiceException(e);
								}
								if(length > 0) {
									report.add("ERROR: Missing media file for " + media.refGetPath().toXRI() + ". Expected location " + contentFile);
									SysLog.error("ERROR: Missing media file for " + media.refGetPath().toXRI() + ". Expected location " + contentFile);
								} else {
									countEmptyMedia++;
								}
							}
						} else {
							if(!contentFile.exists()) {
								// Touching content if media file does not exist created content file
								Media media = (Media)pm.getObjectById(mediaIdentity);
								media.getContent();
							}
						}
					}
					count++;
					if(count % 100 == 0) {
						pm.evictAll();
						if(validateOnly) {
							System.out.println(new java.util.Date() + ": Validated " + count + " media objects");
						} else {
							System.out.println(new java.util.Date() + ": Migrated " + count + " media objects");							
						}
					}
				}
				rsT.close();
				psT.close();
				connT.close();
				if(validateOnly) {
					report.add("OK: Validated " + count + " media objects where " + countEmptyMedia + " are empty");
					System.out.println(new java.util.Date() + ": Validated " + count + " media objects where " + countEmptyMedia + " are empty");
				} else {
					report.add("OK: Migrated " + count + " media objects");					
					System.out.println(new java.util.Date() + ": Migrated " + count + " media objects");
				}
			} catch(Exception e) {
				new ServiceException(e).log();
				report.add("ERROR: Exception. Message is " + e.getMessage());
			}
		}
		return report;
	}

	/**
	 * Migrate media to file system in case org.opencrx.mediadir.* is set.
	 * 
	 * @param segment
	 * @throws ServiceException
	 */
	public static List<String> migrateMediaToDB(
		String providerName,
		PersistenceManager pm
	) throws ServiceException {
		List<String> report = new ArrayList<String>();
		if(System.getProperty("org.opencrx.mediadir." + providerName) != null) {
	        File mediadir = new File(System.getProperty("org.opencrx.mediadir." + providerName));
	        Set<File> mediaFiles = listFilesRecursively(mediadir);
	        int count = 0;
	        for(File mediaFile: mediaFiles) {	        	
	        	List<String> components = new ArrayList<String>();
	        	File contentFile = mediaFile;
	        	while(!contentFile.equals(mediadir)) {
	        		components.add(0, contentFile.getName());
	        		contentFile = contentFile.getParentFile();
	        	}
	        	if(components.size() > 0) {
	        		components.set(0, components.get(0).replace("_", ":"));
	        	}
	        	Path mediaIdentity = new Path(components.toArray(new String[components.size()]));
	        	Media media = null;
	        	try {
	        		media = (Media)pm.getObjectById(mediaIdentity);
	        	} catch(Exception e) {
	        		new ServiceException(e).log();
	        	}
	        	if(media == null) {
	        		report.add("ERROR: Missing media " + mediaIdentity.toXRI() + " for file " + mediaFile);	        		
	        	} else {
	        		try {
	        			if(
	        				media.getContent() == null || 
	        				mediaFile.lastModified() > media.getModifiedAt().getTime()
	        			) {
		        			pm.currentTransaction().begin();
			        		media.setContent(BinaryLargeObjects.valueOf(mediaFile));
			        		pm.currentTransaction().commit();
			        		report.add("OK: Migrated " + mediaIdentity.toXRI());
	        			} else {
							System.out.println(new java.util.Date() + ": Media up-to-date for file " + mediaFile + ".");	        				
	        			}
		        		count++;
		        		if(count % 100 == 0) {
							System.out.println(new java.util.Date() + ": Migrated " + count + " media files to database");
							pm.evictAll();
		        		}
	        		} catch(Exception e) {
	        			try {
	        				pm.currentTransaction().rollback();
	        			} catch(Exception ignore) {}
	        			new ServiceException(e).log();
		        		report.add("ERROR: Error migrating media. Message is " + e.getMessage());
	        		}
	        	}  		        	
	        }
		}
		return report;
	}

	/**
	 * Validate existence of indexes with reference schema.
	 * 
	 * @param connT
	 * @param fix
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> validateIndexes(
		Connection connT,
		boolean fix
	) throws ServiceException {
		Connection connS = null;
		List<String> report = new ArrayList<String>();
		try {
			connS = getSchemaConnection();
			List<String> schema = getSchema(connS);
			List<String> indexNames = getIndexNames(connS);
			String databaseProductName = connT.getMetaData().getDatabaseProductName();
			for(String indexName: indexNames) {
				String statement = getObjectDefinition(CREATE_INDEX_PREFIX, indexName, schema, connT.getMetaData().getDatabaseProductName(), false);
				PreparedStatement psT = null;
				try {
					if(databaseProductName.indexOf("Oracle") >= 0) {
						if(indexName.length() > 30) {
							statement = statement.replace(indexName, indexName.substring(0, 30));
						}
					}
					psT = connT.prepareStatement(statement);
					psT.executeUpdate();
					if(fix) {
						report.add("SQL: " + statement);
					} else {
						report.add("OK: Index " + indexName);						
					}
				} catch(SQLException e0) {
					String message = e0.getMessage();
					int errorCode = e0.getErrorCode();
					boolean alreadyExists = 
						message.indexOf("already exists") >= 0 || // PostgreSQL, HSQLDB, DB2
						message.indexOf("Duplicate key name") >= 0 || // MySQL
						message.indexOf("ORA-01408") >= 0 || // Oracle
						message.indexOf("ORA-00955") >= 0 || // Oracle
						errorCode == 0 || // PostgreSQL
						errorCode == 1913; // SQL Server
					if(alreadyExists) {
						if(!fix) {
							report.add("OK: Index " + indexName);
						}
					} else {
						report.add("ERROR: " + statement + " (message=" + e0.getMessage() + "; errorCode=" + errorCode + ")");
					}
				} finally {
					try {
						psT.close();
					} catch(Exception e0) {}
				}
			}
		} catch(Exception e) {
			throw new ServiceException(e);
		} finally {
			try {
				connS.close();
			} catch(Exception ignore) {}
		}
		return report;		
	}

	/**
	 * Validate existence of sequences with reference schema.
	 * 
	 * @param connT
	 * @param fix
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> validateSequences(
		Connection connT,
		boolean fix
	) throws ServiceException {
		Connection connS = null;
		List<String> report = new ArrayList<String>();
		try {
			connS = getSchemaConnection();
			List<String> schema = getSchema(connS);
			List<String> sequenceNames = getSequenceNames(connS);
			String databaseProductName = connT.getMetaData().getDatabaseProductName();
			for(String sequenceName: sequenceNames) {
				String statement = getObjectDefinition(CREATE_SEQUENCE_PREFIX, sequenceName, schema, databaseProductName, false);
				PreparedStatement psT = null;
				if(databaseProductName.indexOf("MySQL") >= 0) {
					statement = "SELECT * FROM " + sequenceName + " WHERE 1=0";
					try {
						psT = connT.prepareStatement(statement);
						psT.executeQuery();
						psT.close();
						report.add("OK: Sequence " + sequenceName);
					} catch(Exception e) {
						psT.close();
						statement = "CREATE TABLE " + sequenceName + "(nextval int)";
						report.add("SQL: " + statement);
						psT = connT.prepareStatement(statement);
						psT.executeUpdate();
						psT.close();
						statement = "INSERT INTO " + sequenceName + " (nextval) VALUES (1000000)";
						report.add("SQL: " + statement);
						psT = connT.prepareStatement(statement);
						psT.executeUpdate();
						psT.close();
					} finally {
						try {
							psT.close();
						} catch(Exception e0) {}
					}
				} else {
					try {
						if(databaseProductName.indexOf("Oracle") >= 0) {
							statement = statement + " NOCYCLE CACHE 100 NOORDER";
						}
						psT = connT.prepareStatement(statement);
						psT.executeUpdate();
						if(fix) {
							report.add("SQL: " + statement);
						} else {
							report.add("OK: Sequence " + sequenceName);						
						}
					} catch(SQLException e0) {
						String message = e0.getMessage();
						int errorCode = e0.getErrorCode();
						boolean alreadyExists = 
							message.indexOf("already exists") >= 0 ||
							message.indexOf("Duplicate key name") >= 0 ||
							message.indexOf("ORA-01408") >= 0 ||
							message.indexOf("ORA-00955") >= 0 ||
							message.indexOf("SQLSTATE=42710") >= 0 ||
							message.indexOf("errorCode=2714") >= 0 ||
							errorCode == 99999 ||
							errorCode == 2714;
						if(alreadyExists) {
							if(!fix) {
								report.add("OK: Sequence " + sequenceName);
							}
						} else {
							report.add("ERROR: " + statement + " (message=" + e0.getMessage() + "; errorCode=" + errorCode + ")");
						}
					} finally {
						try {
							psT.close();
						} catch(Exception e0) {}
					}
				}
			}
		} catch(Exception e) {
			throw new ServiceException(e);
		} finally {
			try {
				connS.close();
			} catch(Exception ignore) {}
		}
		return report;
	}
	
	/**
	 * Get database plug-ins configuration.
	 * 
	 * @return
	 */
	protected static Database_2[] getDatabasePlugIns(
	) throws ServiceException {
		if(DbSchemaUtils.databasePlugIns == null) {
			DbSchemaUtils.databasePlugIns = Utils.getDatabasePlugIns();
		}
		return DbSchemaUtils.databasePlugIns;
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	protected static final int FETCH_SIZE = 200;
	protected static final Set<String> HIDDEN_FILES = new TreeSet<String>(
		Arrays.asList(".git", ".cvs", ".svn")
	);
	public static final String CREATE_TABLE_PREFIX = "CREATE MEMORY TABLE PUBLIC.";
	public static final String CREATE_VIEW_PREFIX = "CREATE VIEW PUBLIC.";
	public static final String CREATE_SEQUENCE_PREFIX = "CREATE SEQUENCE PUBLIC.";
	public static final String CREATE_INDEX_PREFIX = "CREATE INDEX";
	public static final String ALTER_TABLE_PREFIX = "ALTER TABLE";
	public static final Set<String> OPTIONAL_DBOBJECTS = new HashSet<String>(
		Arrays.asList(
			"OOCKE1_TOBJ_ACCTMEMBERSHIP_ALT", 
			"OOCKE1_TOBJ_ACCTMEMBERSHIP_FR", 
			"OOCKE1_TOBJ_ACCTMEMBERSHIP_TO"
		)
	);
	
	protected static boolean schemaPrepared = false;
	protected static Database_2[] databasePlugIns;

}
