/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
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
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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
import org.openmdx.application.dataprovider.layer.persistence.jdbc.Database_1;
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
	protected static Connection getSchemaConnection(
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
	protected static List<String> getSchema(
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
	protected static String getObjectDefinition(
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
		// PostgreSQL
		if(targetDatabaseName.indexOf("PostgreSQL") >=0) {
			command = command.replace(" TIMESTAMP", " TIMESTAMP WITH TIME ZONE");
			command = command.replace(" CLOB,", " TEXT,");
			command = command.replace(" VARBINARY,", " BYTEA,");
			command = command.replace(" CLOB)", " TEXT)");
			command = command.replace(" VARBINARY)", " BYTEA)");
			command = command.replace("'\\'", "E'\\\\'");
			command = command.replace("'AS DTYPE", "'::text AS DTYPE");
			command = command.replace(" CHAR(", " CHR(");
		}
		// HSQLDB
		else if(targetDatabaseName.indexOf("HSQL") >=0) {
		}		
		// MySQL
		else if(targetDatabaseName.indexOf("MySQL") >=0) {
			command = command.replace(" TIMESTAMP", " DATETIME");					
			command = command.replace(" BOOLEAN,", " BIT,");
			command = command.replace(" CLOB,", " LONGTEXT,");
			command = command.replace(" VARBINARY,", " BLOB,");
			command = command.replace(" BOOLEAN)", " BIT)");
			command = command.replace(" CLOB)", " LONGTEXT)");
			command = command.replace(" VARBINARY)", " BLOB)");
			command = command.replace("'\\'", "'\\\\'");
		}
		// DB2		
		else if(targetDatabaseName.indexOf("DB2") >=0) {
			command = command.replace(" BOOLEAN,", " SMALLINT,");
			command = command.replace(" VARBINARY,", " BLOB,");
			command = command.replace(" BOOLEAN)", " SMALLINT)");
			command = command.replace(" VARBINARY)", " BLOB)");
			command = command.replace("SUBSTRING(", "SUBSTR(");
			command = command.replace(" CHAR(", "CHR(");					
			command = command.replace(" WITH RECURSIVE", " WITH");
		}
		// Oracle
		else if(targetDatabaseName.indexOf("Oracle") >=0) {
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
		}		
		// Microsoft
		else if(targetDatabaseName.indexOf("Microsoft") >=0) {
			command = command.replace("||", "+");
			command = command.replace(" DATE,", " DATETIME,");					
			command = command.replace(" TIMESTAMP", " DATETIME");					
			command = command.replace(" BOOLEAN,", " BIT,");
			command = command.replace(" CLOB,", " NTEXT,");
			command = command.replace(" VARBINARY,", " IMAGE,");					
			command = command.replace(" DATE)", " DATETIME)");					
			command = command.replace(" BOOLEAN)", " BIT)");
			command = command.replace(" CLOB)", " NTEXT)");
			command = command.replace(" VARBINARY)", " IMAGE)");
			command = command.replace(" WITH RECURSIVE", " WITH");
			command = command.replace("(ASS0.OBJECT_ID)+'*+1'", " CAST(ASS0.OBJECT_ID+'*+1' AS VARCHAR)");
			command = command.replace("(ASS1.OBJECT_ID)+'*+'+(TEMP.DISTANCE+1)", " CAST(ASS1.OBJECT_ID+'*+'+(TEMP.DISTANCE+1) AS VARCHAR)");
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
	 * Migrate data from an older schema version to latest version.
	 * 
	 * @param connT
	 * @param fix
	 * @return
	 * @throws ServiceException
	 */
	public static List<String> migrateData(
		Connection connT,
		boolean fix
	) throws ServiceException {
		MigrationDefinition[] migrationDefinitions = {
			new MigrationDefinition(
				"2.2 -> 2.3",
				"SELECT member_role FROM OOCKE1_ACCOUNTASSIGNMENT_ WHERE 1=0",
				Arrays.asList(
					"UPDATE OOCKE1_ACCOUNTASSIGNMENT SET MEMBER_ROLE_0 = (SELECT MEMBER_ROLE FROM OOCKE1_ACCOUNTASSIGNMENT_ ass_ WHERE ass_.OBJECT_ID = OOCKE1_ACCOUNTASSIGNMENT.OBJECT_ID AND ass_.IDX = 0) WHERE MEMBER_ROLE_0 IS NULL",
					"UPDATE OOCKE1_ACCOUNTASSIGNMENT SET MEMBER_ROLE_1 = (SELECT MEMBER_ROLE FROM OOCKE1_ACCOUNTASSIGNMENT_ ass_ WHERE ass_.OBJECT_ID = OOCKE1_ACCOUNTASSIGNMENT.OBJECT_ID AND ass_.IDX = 1) WHERE MEMBER_ROLE_1 IS NULL",
					"UPDATE OOCKE1_ACCOUNTASSIGNMENT SET MEMBER_ROLE_2 = (SELECT MEMBER_ROLE FROM OOCKE1_ACCOUNTASSIGNMENT_ ass_ WHERE ass_.OBJECT_ID = OOCKE1_ACCOUNTASSIGNMENT.OBJECT_ID AND ass_.IDX = 2) WHERE MEMBER_ROLE_2 IS NULL",
					"UPDATE OOCKE1_ACCOUNTASSIGNMENT SET MEMBER_ROLE_3 = (SELECT MEMBER_ROLE FROM OOCKE1_ACCOUNTASSIGNMENT_ ass_ WHERE ass_.OBJECT_ID = OOCKE1_ACCOUNTASSIGNMENT.OBJECT_ID AND ass_.IDX = 3) WHERE MEMBER_ROLE_3 IS NULL",
					"UPDATE OOCKE1_ACCOUNTASSIGNMENT SET MEMBER_ROLE_4 = (SELECT MEMBER_ROLE FROM OOCKE1_ACCOUNTASSIGNMENT_ ass_ WHERE ass_.OBJECT_ID = OOCKE1_ACCOUNTASSIGNMENT.OBJECT_ID AND ass_.IDX = 4) WHERE MEMBER_ROLE_4 IS NULL"
				)
			),
			new MigrationDefinition(
				"2.3 -> 2.4",
				null,
				Collections.<String>emptyList()
			),
			new MigrationDefinition(
				"2.4 -> 2.5",
				"SELECT rate_type FROM OOCKE1_WORKRECORD WHERE 1=0",
				Arrays.asList(
					"UPDATE OOCKE1_WORKRECORD SET PAYMENT_TYPE = 0 WHERE PAYMENT_TYPE IS NULL",
					"UPDATE OOCKE1_WORKRECORD SET RECORD_TYPE = RATE_TYPE WHERE RECORD_TYPE IS NULL",
					"UPDATE OOCKE1_WORKRECORD SET QUANTITY = (1.0 * DURATION_HOURS) + (DURATION_MINUTES / 60.0) WHERE QUANTITY IS NULL",
					"UPDATE OOCKE1_WORKRECORD SET QUANTITY_UOM = N'uom/CRX/Root/hour' WHERE QUANTITY_UOM IS NULL"
				)				
			),
			new MigrationDefinition(
				"2.5 -> 2.6",
				null,
				Arrays.asList(
					"DELETE FROM OOCKE1_CALENDARFEED_ WHERE OBJECT_ID IN (SELECT OBJECT_ID FROM OOCKE1_CALENDARFEED WHERE DTYPE = 'org:opencrx:kernel:home1:IcalFeed')",
					"DELETE FROM OOCKE1_CALENDARFEED WHERE DTYPE = 'org:opencrx:kernel:home1:IcalFeed'"
				)								
			),
			new MigrationDefinition(
				"2.6 -> 2.7",
				null,
				Collections.<String>emptyList()
			),
			new MigrationDefinition(
				"2.7 -> 2.8",
				"SELECT e_mail_address FROM OOCKE1_EMAILACCOUNT WHERE 1=0",
				Arrays.asList(
					"UPDATE OOCKE1_ACTIVITYPARTY SET PARTY_STATUS = 0 WHERE PARTY_STATUS IS NULL",
					"UPDATE OOCKE1_DOCUMENTFOLDERASS SET ASSIGNMENT_ROLE = 0 WHERE ASSIGNMENT_ROLE IS NULL",
					"UPDATE OOCKE1_SEGMENT SET ACCESS_LEVEL_UPDATE = 3 WHERE ACCESS_LEVEL_UPDATE IS NULL",
					"UPDATE OOCKE1_EMAILACCOUNT SET NAME = E_MAIL_ADDRESS, IS_ACTIVE = true WHERE NAME IS NULL"
				)				
			),
			new MigrationDefinition(
				"2.8 -> 2.9",
				"SELECT reference_filter FROM OOCKE1_EXPORTPROFILE WHERE 1=0",
				Arrays.asList(
					"UPDATE OOMSE2_PRIVILEGE SET DTYPE = 'org:openmdx:security:realm1:Privilege' WHERE DTYPE LIKE 'org:openmdx:security:authorization1:%'", 
					"UPDATE OOMSE2_PRIVILEGE_ SET DTYPE = 'org:openmdx:security:realm1:Privilege' WHERE DTYPE LIKE 'org:openmdx:security:authorization1:%'",
					"UPDATE OOCKE1_EXPORTPROFILE SET EXPORT_PARAMS = REFERENCE_FILTER",
					"UPDATE OOCKE1_EXPORTPROFILE SET LOCALE = 0 WHERE LOCALE IS NULL",
					"UPDATE OOCKE1_ACTIVITYCREATOR SET ICAL_TYPE = 1 WHERE ICAL_TYPE = 0 AND EXISTS (SELECT 0 FROM OOCKE1_ACTIVITYTYPE T WHERE OOCKE1_ACTIVITYCREATOR.ACTIVITY_TYPE = T.OBJECT_ID AND ACTIVITY_CLASS <> 8)",
					"UPDATE OOCKE1_ACTIVITYCREATOR SET ICAL_TYPE = 2 WHERE ICAL_TYPE = 0 AND EXISTS (SELECT 0 FROM OOCKE1_ACTIVITYTYPE T WHERE OOCKE1_ACTIVITYCREATOR.ACTIVITY_TYPE = T.OBJECT_ID AND ACTIVITY_CLASS = 8)",
					"UPDATE OOCKE1_ACTIVITY SET ICAL_TYPE = 1 WHERE ICAL_TYPE = 0 AND DTYPE <> 'org:opencrx:kernel:activity1:Task'",
					"UPDATE OOCKE1_ACTIVITY SET ICAL_TYPE = 2 WHERE ICAL_TYPE = 0 AND DTYPE = 'org:opencrx:kernel:activity1:Task'"		
				)								 
			),
			new MigrationDefinition(
				"2.9 -> 2.10",
				"SELECT CLOSING_CODE FROM OOCKE1_ACCOUNT WHERE 1=0",
				Arrays.asList(
					"UPDATE OOCKE1_ACCOUNT SET CLOSING_CODE = 0 WHERE CLOSING_CODE IS NULL",
					"DELETE FROM OOCKE1_ALERT WHERE REFERENCE LIKE 'reminder/%'",
					"DELETE FROM OOCKE1_ALERT_ WHERE NOT EXISTS (SELECT 0 FROM OOCKE1_ALERT a WHERE a.OBJECT_ID = OOCKE1_ALERT_.OBJECT_ID)"			
				)								 				
			),
			new MigrationDefinition(
				"2.10 -> 2.11",
				"SELECT ICAL_CLASS FROM OOCKE1_ACTIVITYCREATOR WHERE 1=0",
				Arrays.asList(
					"UPDATE OOCKE1_ACTIVITYCREATOR SET ICAL_CLASS = 3 WHERE ICAL_CLASS IS NULL",
					"UPDATE OOCKE1_ACTIVITY SET ICAL_CLASS = 1 WHERE ICAL_CLASS IS NULL AND ICAL LIKE '%CLASS:PRIVATE%'",
					"UPDATE OOCKE1_ACTIVITY SET ICAL_CLASS = 2 WHERE ICAL_CLASS IS NULL AND ICAL LIKE '%CLASS:CONFIDENTIAL%'",
					"UPDATE OOCKE1_ACTIVITY SET ICAL_CLASS = 3 WHERE ICAL_CLASS IS NULL AND ICAL LIKE '%CLASS:PUBLIC%'",
					"UPDATE OOCKE1_ACTIVITY SET ICAL_CLASS = 2 WHERE ICAL_CLASS IS NULL",
					"ALTER TABLE OOCKE1_WFPROCESS ALTER COLUMN DESCRIPTION TYPE VARCHAR(2000)",
					"ALTER TABLE OOCKE1_CODEVALUEENTRY_ ALTER COLUMN LONG_TEXT TYPE VARCHAR(512)"
				)
			),
			new MigrationDefinition(
				"2.11 -> 2.12",
				"SELECT CONTENT_LANGUAGE FROM OOCKE1_DOCUMENT_ WHERE 1=0",
				Arrays.asList(
					"UPDATE OOCKE1_DOCUMENT_ SET CONTENT_LANGUAGE = (SELECT CONTENT_LANGUAGE FROM OOCKE1_DOCUMENT DOC WHERE DOC.OBJECT_ID = OOCKE1_DOCUMENT_.OBJECT_ID) WHERE IDX = 0",
					"UPDATE OOCKE1_DOCUMENT SET CONTENT_LANGUAGE_ = 0 WHERE CONTENT_LANGUAGE_ IS NULL",
					"UPDATE OOCKE1_ACTIVITYGROUP SET ACTIVITY_GROUP_TYPE = 0 WHERE ACTIVITY_GROUP_TYPE IS NULL",
					"{HSQL,PostgreSQL,MySQL,DB2,Microsoft}UPDATE OOCKE1_INVOLVEDOBJECT SET OBJECT_ID = SUBSTRING(OBJECT_ID, 1, POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + 1__SUBSTR_VOID_ENDPOS__))) || 'activity' || SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + POSITION('/' IN SUBSTRING(OBJECT_ID,POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + 1__SUBSTR_VOID_ENDPOS__))__SUBSTR_VOID_ENDPOS__) WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'",
					"{HSQL,PostgreSQL,MySQL,DB2,Microsoft}UPDATE OOCKE1_INVOLVEDOBJECT_ SET OBJECT_ID = SUBSTRING(OBJECT_ID, 1, POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + 1__SUBSTR_VOID_ENDPOS__))) || 'activity' || SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + POSITION('/' IN SUBSTRING(OBJECT_ID,POSITION('/' IN OBJECT_ID) + POSITION('/' IN SUBSTRING(OBJECT_ID, POSITION('/' IN OBJECT_ID) + 1__SUBSTR_VOID_ENDPOS__)) + 1__SUBSTR_VOID_ENDPOS__))__SUBSTR_VOID_ENDPOS__) WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'",
					"{Oracle}UPDATE OOCKE1_INVOLVEDOBJECT SET OBJECT_ID = SUBSTR(OBJECT_ID, 1, INSTR(OBJECT_ID, '/', 1, 3)) || 'activity' || SUBSTR(OBJECT_ID, INSTR(OBJECT_ID, '/', 1, 3)) WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'",
					"{Oracle}UPDATE OOCKE1_INVOLVEDOBJECT_ SET OBJECT_ID = SUBSTR(OBJECT_ID, 1, INSTR(OBJECT_ID, '/', 1, 3)) || 'activity' || SUBSTR(OBJECT_ID, INSTR(OBJECT_ID, '/', 1, 3)) WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'",
					"UPDATE OOCKE1_INVOLVEDOBJECT SET OBJECT_ID = REPLACE(OBJECT_ID, 'involvedObject/', 'involvedObject1/org:opencrx:kernel:activity1/') WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'",
					"UPDATE OOCKE1_INVOLVEDOBJECT_ SET OBJECT_ID = REPLACE(OBJECT_ID, 'involvedObject/', 'involvedObject1/org:opencrx:kernel:activity1/') WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'",
					"UPDATE OOCKE1_INVOLVEDOBJECT SET DTYPE = 'org:opencrx:kernel:generic:InvolvedObject' WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'",
					"UPDATE OOCKE1_INVOLVEDOBJECT_ SET DTYPE = 'org:opencrx:kernel:generic:InvolvedObject' WHERE DTYPE = 'org:opencrx:kernel:activity1:InvolvedObject'"
				)
			),
			new MigrationDefinition(
				"2.12 -> 2.13",
				"SELECT * FROM OOCKE1_RESOURCE r INNER JOIN OOCKE1_RESOURCERATE rr ON r.OBJECT_ID = rr.P$$PARENT WHERE r.STANDARD_RATE IS NOT NULL",
				Arrays.asList(
					// Migrate FX -> FR
					"UPDATE OOCKE1_ADDRESS SET POSTAL_COUNTRY = 250 WHERE POSTAL_COUNTRY = 249",
					// Migrate Resource::standardRate -> OOCKE1_RESOURCERATE 
					"INSERT INTO OOCKE1_RESOURCERATE (" +
					"  OBJECT_ID," + 
					"  ACCESS_LEVEL_BROWSE," + 
					"  ACCESS_LEVEL_DELETE," + 
					"  ACCESS_LEVEL_UPDATE," + 
					"  CATEGORY_," + 
					"  CREATED_AT," + 
					"  CREATED_BY_," + 
					"  DESCRIPTION," + 
					"  DISABLED," + 
					"  DISABLED_REASON," + 
					"  EXTERNAL_LINK_," + 
					"  MODIFIED_BY_," + 
					"  NAME," + 
					"  OWNER_," + 
					"  RATE," + 
					"  RATE_CURRENCY," + 
					"  RATE_TYPE," + 
					"  P$$PARENT," + 
					"  USER_BOOLEAN4_," + 
					"  USER_CODE4_," + 
					"  USER_DATE4_," + 
					"  USER_DATE_TIME4_," + 
					"  USER_NUMBER4_," + 
					"  USER_STRING4_," + 
					"  DTYPE," + 
					"  MODIFIED_AT" + 
					") " + 
					"SELECT " + 
					"  REPLACE(" + "r.OBJECT_ID, 'resource', 'resourceRate') || '/' || 'StandardRate'," + 
					"  r.ACCESS_LEVEL_BROWSE," + 
					"  r.ACCESS_LEVEL_DELETE," + 
					"  r.ACCESS_LEVEL_UPDATE," + 
					"  0," + 
					"  r.CREATED_AT," + 
					"  0," + 
					"  NULL," + 
					"  NULL," + 
					"  NULL," + 
					"  0," + 
					"  0," + 
					"  'Standard rate'," + 
					"  0," + 
					"  r.STANDARD_RATE," + 
					"  r.RATE_CURRENCY," + 
					"  1," + // 1=Work (Standard Rate)
					"  r.OBJECT_ID," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  'org:opencrx:kernel:activity1:ResourceRate'," + 
					"  r.MODIFIED_AT " + 
					"FROM " + 
					"  OOCKE1_RESOURCE r " + 
					"WHERE " + 
					"  r.STANDARD_RATE IS NOT NULL",
					// Migrate Resource::standardRate -> OOCKE1_RESOURCERATE_					
					"INSERT INTO OOCKE1_RESOURCERATE_ (" + 
					"  OBJECT_ID," + 
					"  IDX," + 
					"  CREATED_BY," + 
					"  MODIFIED_BY," + 
					"  OWNER," + 
					"  DTYPE" + 
					") " + 
					"SELECT" + 
					"  REPLACE(r.OBJECT_ID, 'resource', 'resourceRate') || '/' || 'StandardRate'," + 
					"  R_.IDX," + 
					"  R_.CREATED_BY," + 
					"  R_.MODIFIED_BY," + 
					"  R_.OWNER," + 
					"  'org:opencrx:kernel:activity1:ResourceRate'" + 
					"FROM " + 
					"  OOCKE1_RESOURCE_ r_ " + 
					"INNER JOIN " + 
					"  OOCKE1_RESOURCE r " + 
					"ON " + 
					"  r.OBJECT_ID = r_.OBJECT_ID " + 
					"WHERE " + 
					"  r.STANDARD_RATE IS NOT NULL",
					// Migrate Resource::overtimeRate -> OOCKE1_RESOURCERATE
					"INSERT INTO OOCKE1_RESOURCERATE (" +
					"  OBJECT_ID," + 
					"  ACCESS_LEVEL_BROWSE," + 
					"  ACCESS_LEVEL_DELETE," + 
					"  ACCESS_LEVEL_UPDATE," + 
					"  CATEGORY_," + 
					"  CREATED_AT," + 
					"  CREATED_BY_," + 
					"  DESCRIPTION," + 
					"  DISABLED," + 
					"  DISABLED_REASON," + 
					"  EXTERNAL_LINK_," + 
					"  MODIFIED_BY_," + 
					"  NAME," + 
					"  OWNER_," + 
					"  RATE," + 
					"  RATE_CURRENCY," + 
					"  RATE_TYPE," + 
					"  P$$PARENT," + 
					"  USER_BOOLEAN4_," + 
					"  USER_CODE4_," + 
					"  USER_DATE4_," + 
					"  USER_DATE_TIME4_," + 
					"  USER_NUMBER4_," + 
					"  USER_STRING4_," + 
					"  DTYPE," + 
					"  MODIFIED_AT" + 
					") " + 
					"SELECT " + 
					"  REPLACE(" + "r.OBJECT_ID, 'resource', 'resourceRate') || '/' || 'OvertimeRate'," + 
					"  r.ACCESS_LEVEL_BROWSE," + 
					"  r.ACCESS_LEVEL_DELETE," + 
					"  r.ACCESS_LEVEL_UPDATE," + 
					"  0," + 
					"  r.CREATED_AT," + 
					"  0," + 
					"  NULL," + 
					"  NULL," + 
					"  NULL," + 
					"  0," + 
					"  0," + 
					"  'Overtime rate'," + 
					"  0," + 
					"  r.OVERTIME_RATE," + 
					"  r.RATE_CURRENCY," + 
					"  2," + // 2=Work (Overtime Rate)
					"  r.OBJECT_ID," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  0," + 
					"  'org:opencrx:kernel:activity1:ResourceRate'," + 
					"  r.MODIFIED_AT " + 
					"FROM " + 
					"  OOCKE1_RESOURCE r " + 
					"WHERE " + 
					"  r.OVERTIME_RATE IS NOT NULL",
					// Migrate Resource::overtimeRate -> OOCKE1_RESOURCERATE_					
					"INSERT INTO OOCKE1_RESOURCERATE_ (" + 
					"  OBJECT_ID," + 
					"  IDX," + 
					"  CREATED_BY," + 
					"  MODIFIED_BY," + 
					"  OWNER," + 
					"  DTYPE" + 
					") " + 
					"SELECT" + 
					"  REPLACE(r.OBJECT_ID, 'resource', 'resourceRate') || '/' || 'OvertimeRate'," + 
					"  R_.IDX," + 
					"  R_.CREATED_BY," + 
					"  R_.MODIFIED_BY," + 
					"  R_.OWNER," + 
					"  'org:opencrx:kernel:activity1:ResourceRate'" + 
					"FROM " + 
					"  OOCKE1_RESOURCE_ r_ " + 
					"INNER JOIN " + 
					"  OOCKE1_RESOURCE r " + 
					"ON " + 
					"  r.OBJECT_ID = r_.OBJECT_ID " + 
					"WHERE " + 
					"  r.OVERTIME_RATE IS NOT NULL"
				)
			),
		};
		String targetDatabaseName = "";
		try {
			targetDatabaseName = connT.getMetaData().getDatabaseProductName();
		} catch(Exception e) {}
		List<String> report = new ArrayList<String>();
		PreparedStatement psT = null;
		for(MigrationDefinition migrationDefinition: migrationDefinitions) {
			try {
				if(migrationDefinition.getTestForVersionStatement() != null) {
					psT = connT.prepareStatement(migrationDefinition.getTestForVersionStatement());
					psT.executeQuery();
					ResultSet rsT = psT.executeQuery();
					rsT.next();
					rsT.close();			
					psT.close();
				}
				for(String statement: migrationDefinition.getMigrationStatements()) {
					boolean includeStatement = false;
					if(statement.startsWith("{")) {
						int pos = statement.indexOf("}");
						for(String name: statement.substring(1, pos).split(",")) {
							if(targetDatabaseName.indexOf(name) >= 0) {
								includeStatement = true;
								break;
							}
						}
						statement = statement.substring(pos + 1);
					} else {
						includeStatement = true;
					}
					if(includeStatement) {
						if(targetDatabaseName.indexOf("DB2") >=0) {
							statement = statement.replace("true", "1");
							statement = statement.replace("false", "0");
							statement = statement.replace("POSITION('/' IN ", "LOCATE('/', ");
							statement = statement.replace("SUBSTRING(", "SUBSTR(");
							statement = statement.replace("__SUBSTR_VOID_ENDPOS__", "");
						} else if(targetDatabaseName.indexOf("Oracle") >=0) {
							statement = statement.replace("true", "1");
							statement = statement.replace("false", "0");
							statement = statement.replace("__SUBSTR_VOID_ENDPOS__", "");						
						} else if(targetDatabaseName.indexOf("Microsoft") >=0) {
							statement = statement.replace(" || ", " + ");
							statement = statement.replace("POSITION('/' IN ", "CHARINDEX('/', ");
							statement = statement.replace("__SUBSTR_VOID_ENDPOS__", ",1000");
						} else {
							statement = statement.replace("__SUBSTR_VOID_ENDPOS__", "");						
						}
						if(statement.startsWith(ALTER_TABLE_PREFIX)) {
							statement = mapColumnDefinition(targetDatabaseName, statement);
							if(targetDatabaseName.indexOf("PostgreSQL") >=0) {
								// nothing to do
							} else if(targetDatabaseName.indexOf("HSQL") >=0) {
								statement = statement.replace("TYPE ", "");							
							} else if(targetDatabaseName.indexOf("Oracle") >=0) {
								statement = statement.replace("ALTER COLUMN", "MODIFY");
								statement = statement.replace("TYPE ", "");
							} else if(targetDatabaseName.indexOf("MySQL") >=0) {
								statement = statement.replace("ALTER COLUMN", "MODIFY");						
								statement = statement.replace("TYPE ", "");
							} else if(targetDatabaseName.indexOf("DB2") >=0) {
								statement = statement.replace("TYPE ", "SET DATA TYPE ");
							} else if(targetDatabaseName.indexOf("Microsoft") >=0) {
								statement = statement.replace("TYPE ", "");
							}
						}
						if(fix) {
							try {					
								report.add("SQL (" + migrationDefinition.getName() + "): " + statement);
								psT = connT.prepareStatement(statement);
								psT.executeUpdate();
							} catch(Exception e) {
								report.add("ERROR: Migration failed (message=" + e + ")");									
							} finally {
								try {
									psT.close();
								} catch(Exception e0) {}
							}
						} else {								
							report.add("FIX (" + migrationDefinition.getName() + "): " + statement);								
						}
					}
				}
			} catch(Exception e) {
				try {
					psT.close();
				} catch(Exception e0) {}
				// Schema is not valid for migration
			}
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
			Database_1 databasePlugIn = QueryBuilderUtil.getDatabasePlugIns()[0];
			try {
				PreparedStatement psT = connT.prepareStatement("SELECT object_id FROM OOCKE1_MEDIA ORDER BY object_id");
				ResultSet rsT = psT.executeQuery();
				int count = 0;
				int countEmptyMedia = 0;
				while(rsT.next()) {
					String objectId = rsT.getString(1);
					Path mediaIdentity = databasePlugIn.getDelegate().getReference(
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
						File contentFile = new File(contentDir, mediaIdentity.getLastSegment().toClassicRepresentation());
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
				if(
					databaseProductName.indexOf("Microsoft") >= 0 ||
					databaseProductName.indexOf("MySQL") >= 0
				) {
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
							errorCode == 99999;
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
	
}
