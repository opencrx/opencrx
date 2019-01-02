/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: QueryTool
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2018, CRIXP Corp., Switzerland
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

import java.io.FileInputStream;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;

import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.log.SysLog;

/**
 * QueryTool
 *
 */
public class QueryTool {

	/**
	 * Execute query.
	 * 
	 * @param connection
	 * @param statement
	 * @param statementParameters
	 * @param fetchSize
	 * @param fetchDirection
	 * @param maxRows
	 * @throws ServiceException
	 */
	public static void executeQuery(
		Connection connection,
		String statement,
		List<String> statementParameters,
		int fetchSize,
		int fetchDirection,
		int maxRows
	) throws ServiceException {
		try {
	        SysLog.log(Level.INFO, "> executeQuery");
			PreparedStatement ps = null;
			// Prepare
			{
		        long startTime = System.currentTimeMillis();
				ps = connection.prepareStatement(statement);
				if(statementParameters.size() == 1 && statementParameters.get(0).isEmpty()) {
					statementParameters.clear();
				}
				for(int i = 0; i < statementParameters.size(); i++) {
					String value = statementParameters.get(i);
					if(value.startsWith(":integer:")) {
						ps.setInt(i + 1, Integer.parseInt(value.substring(9)));
					} else if(value.startsWith(":decimal:")) {
						ps.setBigDecimal(i + 1, new BigDecimal(value.substring(9)));
					} else if(value.startsWith(":boolean:")) {
						ps.setBoolean(i + 1, Boolean.parseBoolean(value.substring(9)));
					} else {
						ps.setString(i + 1, value);
					}
				}
		        long duration = System.currentTimeMillis() - startTime;
		        SysLog.log(Level.INFO, "prepare time: {0}", Long.valueOf(duration));
			}
			ps.setFetchSize(fetchSize);
			ps.setFetchDirection(fetchDirection);
			ps.setMaxRows(maxRows);
	        SysLog.log(Level.INFO, "statement: {0}", statement);
	        SysLog.log(Level.INFO, "parameters: {0}", statementParameters);
	        SysLog.log(Level.INFO, "fetchSize: {0}", fetchSize);
	        SysLog.log(Level.INFO, "fetchDirection: {0}", fetchDirection);
	        SysLog.log(Level.INFO, "maxRows: {0}", maxRows);
	        // Execute
	        ResultSet rs = null;
	        {
		        long startTime = System.currentTimeMillis();
		        rs = ps.executeQuery();
		        long duration = System.currentTimeMillis() - startTime;
		        SysLog.log(Level.INFO, "execution time: {0}", Long.valueOf(duration));
	        }
	        // Fetch
	        {
		        long startTime = System.currentTimeMillis();
				ResultSetMetaData rsm = rs.getMetaData();	        	
		        int count = 0;
		        while(rs.next()) {
					for(int j = 0; j < rsm.getColumnCount(); j++) {
						String columnName = rsm.getColumnName(j + 1);
						@SuppressWarnings("unused")
						Object value = rs.getObject(columnName);
					}
		        	count++;
		        	if(count > maxRows) {
		        		break;
		        	}
		        }
		        long duration = System.currentTimeMillis() - startTime;
		        SysLog.log(Level.INFO, "fetch time for {0} rows: {1}", count, Long.valueOf(duration));
	        }
	        rs.close();
	        ps.close();
	        SysLog.log(Level.INFO, "< executeQuery");
		} catch(SQLException e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Execute query.
	 * 
	 * @param query
	 * @throws ServiceException
	 */
	public static void executeQuery(
		Properties query
	) throws ServiceException {
		try {
			Class.forName(query.getProperty("jdbcDriver"));
			Properties props = new Properties();
			props.put("user", query.get("username"));
			props.put("password", query.get("password"));
			Connection connection = DriverManager.getConnection(query.getProperty("jdbcUrl"), props);
			connection.setAutoCommit(Boolean.parseBoolean(query.getProperty("autoCommit")));
			QueryTool.executeQuery(
				connection,
				query.getProperty("statement"),
				new ArrayList<String>(Arrays.asList(query.getProperty("statementParameters").split(","))),
				Integer.valueOf(query.getProperty("fetchSize")),
				Integer.valueOf(query.getProperty("fetchDirection")),
				Integer.valueOf(query.getProperty("maxRows"))
			);
		} catch (Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * QueryTool.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			String queryFile = null;
			for(int i = 0; i < args.length; i++) {
				if("--query".equals(args[i])) {
					queryFile = args[i+1];
				}
			}
			Properties query = new Properties();
			query.load(new FileInputStream(queryFile));
			QueryTool.executeQuery(query);
		} catch (Exception e) {
			new ServiceException(e).log();
		}
	}

	// -----------------------------------------------------------------------
	// Members
	// -----------------------------------------------------------------------

}
