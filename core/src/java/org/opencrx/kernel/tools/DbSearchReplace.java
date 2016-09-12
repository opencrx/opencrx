/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CopyDb tool
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2011, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.tools;

import java.io.IOException;
import java.io.PrintStream;
import java.io.Reader;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.opencrx.kernel.utils.DbSchemaUtils;
import org.openmdx.application.configuration.Configuration;
import org.openmdx.application.dataprovider.layer.persistence.jdbc.Database_1;
import org.openmdx.application.dataprovider.layer.persistence.jdbc.LayerConfigurationEntries;
import org.openmdx.base.exception.ServiceException;

public class DbSearchReplace {

	/**
	 * Map CLOB to String.
	 * 
	 * @param clob
	 * @return
	 * @throws IOException
	 * @throws SQLException
	 */
	private static String getStringFromClob(
		java.sql.Clob clob
	) throws IOException, SQLException {
		Reader reader = clob.getCharacterStream();
		StringBuilder s = new StringBuilder();
		int c;
		while ((c = reader.read()) != -1) {
			s.append((char) c);
		}
		return s.toString();
	}

	/**
	 * Db-specific column name mapping.
	 * 
	 * @param conn
	 * @param dbObject
	 * @param columnName
	 * @return
	 * @throws SQLException
	 */
	private static String mapColumnName(
		Connection conn, 
		String dbObject, 
		String columnName
	) throws SQLException {
		String databaseProductName = conn.getMetaData().getDatabaseProductName();
		if("HSQL Database Engine".equals(databaseProductName)) {
			String mappedColumnName = columnName.toUpperCase();
			if("POSITION".equals(mappedColumnName) || mappedColumnName.indexOf("$") > 0) {
				return "\"" + mappedColumnName + "\"";
			}
			else {
				return mappedColumnName;
			}
		}
		else {
			return columnName.toUpperCase();
		}
	}

	/**
	 * Perform search/replace for given dbObject and patterns.
	 * @param dbObject
	 * @param useSuffix
	 * @param conn
	 * @param columnNameIncludes
	 * @param columnNameExcludes
	 * @param searchPattern
	 * @param replacement
	 * @param out
	 * @throws SQLException
	 */
	private static void searchReplaceDbObject(
		String dbObject, 
		boolean useSuffix, 
		Connection conn,
		String columnNameIncludes,
		String columnNameExcludes,
		String searchPattern,
		String replacement,
		boolean validateOnly,
		PrintStream out
	) throws SQLException {
		String currentStatement = null;
		Database_1 db = new Database_1();
		try {
			Configuration configuration = new Configuration();
			configuration.values(LayerConfigurationEntries.BOOLEAN_TYPE).put(0, LayerConfigurationEntries.BOOLEAN_TYPE_STANDARD);
			configuration.values(LayerConfigurationEntries.DATABASE_CONNECTION_FACTORY);
			db.activate((short) 0, configuration, null);
		} catch (Exception e) {
			out.println("Can not activate database plugin: " + e.getMessage());
		}
		try {
			PreparedStatement s = conn.prepareStatement(currentStatement = "SELECT * FROM " + dbObject + (useSuffix ? "_" : ""));
			ResultSet rs = s.executeQuery();
			if(rs != null) {
				ResultSetMetaData rsm = rs.getMetaData();
				FastResultSet frs = new FastResultSet(rs);
				int nRows = 0;
				// Read all rows
				while(frs.next()) {
					// Prepare UPDATE statement
					String statement = "UPDATE " + dbObject + (useSuffix ? "_" : "") + " SET ";
					List<Object> statementParameters = new ArrayList<Object>();
					for(int j = 0; j < rsm.getColumnCount(); j++) {
						String columnName = rsm.getColumnName(j + 1);
						boolean includeColumn = true;
						if(columnNameIncludes != null) {
							includeColumn = Pattern.matches(columnNameIncludes, columnName.toUpperCase());
						}
						if(columnNameExcludes != null) {
							includeColumn &= !Pattern.matches(columnNameExcludes, columnName.toUpperCase());
						}
						if(includeColumn && frs.getObject(columnName) != null) {
							String value = null;
							if(frs.getObject(columnName) instanceof java.sql.Clob) {
								try {
									value = DbSearchReplace.getStringFromClob((java.sql.Clob) frs.getObject(columnName));
								} catch (Exception e) {
									out.println("Reading Clob failed. Reason: " + e.getMessage());
									out.println("Statement=" + statement);
									out.println("Parameters=" + statementParameters);
								}
							} else if(frs.getObject(columnName) instanceof String) {
								value = (String)frs.getObject(columnName);
							}
							if(value != null) {
								String replacedValue = value.replaceAll(searchPattern, replacement);
								if(!value.equals(replacedValue)) {
									String mappedColumnName = mapColumnName(conn, dbObject, columnName);
									statement += (statementParameters.isEmpty() ? "" : ",") + mappedColumnName + " = ?";								
									statementParameters.add(replacedValue);
								}
							}
						}
					}
					if(!statementParameters.isEmpty()) {
						statement += " WHERE OBJECT_ID = ?";
						statementParameters.add(frs.getObject("OBJECT_ID"));
						if(useSuffix) {
							statement += " AND IDX = ?";
							statementParameters.add(frs.getObject("IDX"));
						}
						// Execute UPDATE
						try {
							out.println("Statement=" + statement);
							out.println("Parameters=" + statementParameters);
							out.println();
							if(!validateOnly) {
								PreparedStatement t = conn.prepareStatement(currentStatement = statement);
								for(int j = 0; j < statementParameters.size(); j++) {
									Object parameter = statementParameters.get(j);
									db.getDelegate().setPreparedStatementValue(conn, t, j + 1, parameter);
								}
								t.executeUpdate();
								t.close();
							}
						} catch (Exception e) {
							new ServiceException(e).log();
							out.println("Update failed. Reason: " + e.getMessage());
						}
					}
					nRows++;
					if(nRows % 1000 == 0) {
						out.println(nRows + " rows processed");
					}
				}
				rs.close();
			} else {
				out.println("Unable to process table (result set is null). Statement: " + currentStatement);
			}
			s.close();
		} catch (Exception e) {
			new ServiceException(e).log();
			out.println("Unable to process table (see log for more info). Statement: " + currentStatement);
		}
	}

	/**
	 * Perform search/replace on all tables of the given namespace.
	 *  
	 * @param conn
	 * @param dbObjects
	 * @param columnNameIncludes
	 * @param columnNameExcludes
	 * @param searchPattern
	 * @param replacement
	 * @param validateOnly
	 * @param out
	 */
	private static void searchReplaceNamespace(
	    Connection conn,
	    List<String> dbObjects,
	    String columnNameIncludes,
	    String columnNameExcludes,
	    String searchPattern,
	    String replacement,
	    boolean validateOnly,
	    PrintStream out
	) {
		String currentStatement = null;
		try {
			out.println("Processing tables:");
			int ii = 0;
			for(String dbObject: dbObjects) {
				out.println(ii + ": " + dbObject);
				ii++;
			}
			Set<String> processedDbObjects = new HashSet<String>();
			for (int i = 0; i < dbObjects.size(); i++) {
				String dbObject = dbObjects.get(i);
				if((dbObject != null) && !dbObject.isEmpty() && !processedDbObjects.contains(dbObject)) {
					out.println("Processing table " + i + ": " + dbObject);
					DbSearchReplace.searchReplaceDbObject(
						dbObject, 
						false, // useSuffix
						conn,
						columnNameIncludes,
						columnNameExcludes,
						searchPattern,
						replacement,
						validateOnly,
						out
					);
					out.println("Processing table " + i + ": " + dbObject + "_");
					DbSearchReplace.searchReplaceDbObject(
						dbObject, 
						true, // useSuffix
						conn,
						columnNameIncludes,
						columnNameExcludes,
						searchPattern,
						replacement,
						validateOnly,
						out
					);
					processedDbObjects.add(dbObject);
				}
			}
		} catch (SQLException e) {
			ServiceException e0 = new ServiceException(e);
			e0.log();
			out.println("Statement: " + currentStatement + " (message=" + e0.getMessage());
		}
	}

	/**
	 * Database search/replace utility.
	 *  
	 * @param conn
	 * @param tableNameIncludes
	 * @param tableNameExcludes
	 * @param columnNameIncludes
	 * @param columnNameExcludes
	 * @param searchPattern
	 * @param replacement
	 * @param validateOnly
	 * @param out
	 * @throws ServiceException
	 */
	public static void dbSearchReplace(
		Connection conn,
		String tableNameIncludes,
		String tableNameExcludes,
	    String columnNameIncludes,
	    String columnNameExcludes,
	    String searchPattern,
	    String replacement,
	    boolean validateOnly,
	    PrintStream out
	) throws ServiceException {
		// Prepare table names
		{
			DBOBJECTS_KERNEL.clear();
			DBOBJECTS_SECURITY.clear();
			List<String> tableNames = new ArrayList<String>();
			try {
				tableNames = DbSchemaUtils.getTableNames();
			} catch (Exception e) {
				new ServiceException(e).log();
			} 
			for (String tableName : tableNames) {
				if(tableName.indexOf("_TOBJ_") < 0 && tableName.indexOf("_JOIN_") < 0 && !tableName.endsWith("_")) {
					if(tableName.startsWith("OOCKE1_")) {
						DBOBJECTS_KERNEL.add(tableName);
					}
					else if(tableName.startsWith("OOMSE2_")) {
						DBOBJECTS_SECURITY.add(tableName);
					}
				}
			}			
		}
		// Apply table name patterns
		{
			for(Iterator<String> i = DBOBJECTS_KERNEL.iterator(); i.hasNext(); ) {
				String tableName = i.next();
				boolean includeTable = true;
				if(tableNameIncludes != null) {
					includeTable = Pattern.matches(tableNameIncludes, tableName.toUpperCase());
				}
				if(tableNameExcludes != null) {
					includeTable &= !Pattern.matches(tableNameExcludes, tableName.toUpperCase());
				}
				if(!includeTable) {
					i.remove();
				}				
			}
			for(Iterator<String> i = DBOBJECTS_SECURITY.iterator(); i.hasNext(); ) {
				String tableName = i.next();
				boolean includeTable = true;
				if(tableNameIncludes != null) {
					includeTable = Pattern.matches(tableNameIncludes, tableName.toUpperCase());
				}
				if(tableNameExcludes != null) {
					includeTable &= !Pattern.matches(tableNameExcludes, tableName.toUpperCase());
				}
				if(!includeTable) {
					i.remove();
				}
			}
		}
		try {
			// Namespace kernel
			DbSearchReplace.searchReplaceNamespace(
				conn, 
				DBOBJECTS_KERNEL, 
				columnNameIncludes,
				columnNameExcludes,
				searchPattern,
				replacement,
				validateOnly,
			    out
			);
			// Namespace security
			DbSearchReplace.searchReplaceNamespace(
				conn, 
				DBOBJECTS_SECURITY, 
				columnNameIncludes,
				columnNameExcludes,
				searchPattern,
				replacement,
				validateOnly,
			   	out
			);
		}
		catch (Exception e) {
			throw new ServiceException(e);
		}
		out.println();
		out.println("!!! DONE !!!");
	}

	// -----------------------------------------------------------------------
	// Members
	// -----------------------------------------------------------------------
	static final List<String> DBOBJECTS_KERNEL = new ArrayList<String>();

	static final List<String> DBOBJECTS_SECURITY = new ArrayList<String>();

}
