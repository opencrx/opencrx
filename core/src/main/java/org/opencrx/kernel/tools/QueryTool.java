/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: QueryTool
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
import java.io.IOException;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Properties;

/**
 * QueryTool
 *
 */
public class QueryTool {

	public static class QueryExecutor implements Runnable {

		public QueryExecutor(
			Properties query,
			String conn,
			int loops
		) {
			this.query = query;
			this.conn = conn;
			this.loops = loops;
		}
		
		/**
		 * Execute query.
		 * 
		 * @param connection
		 * @param statement
		 * @param statementParameters
		 * @param fetchSize
		 * @param fetchDirection
		 * @param maxRows
		 * @param loopIndex
		 * @throws SQLException
		 */
		public void executeQuery(
			Connection connection,
			int statementIndex,
			String statement,
			List<String> statementParameters,
			int fetchSize,
			int fetchDirection,
			int maxRows,
			int loopIndex
		) throws SQLException {
			String prefix = new Date() + "   [" + loopIndex + "," + Thread.currentThread().getId() + "] ";		
			{
		        System.out.println(prefix + "query: " + statementIndex);
				PreparedStatement ps = null;
		        System.out.println(prefix + "statement[" + statementIndex + "]: " + statement);
		        System.out.println(prefix + "parameters[" + statementIndex + "]: " + statementParameters);
		        System.out.println(prefix + "fetchSize[" + statementIndex + "]: " + fetchSize);
		        System.out.println(prefix + "fetchDirection[" + statementIndex + "]: " + fetchDirection);
		        System.out.println(prefix + "maxRows[" + statementIndex + "]: " + maxRows);
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
			        System.out.println(prefix + "prepare time[" + statementIndex + "]: " + Long.valueOf(duration));
				}
				ps.setFetchSize(fetchSize);
				ps.setFetchDirection(fetchDirection);
				ps.setMaxRows(maxRows);
		        // Execute
		        ResultSet rs = null;
		        {
			        long startTime = System.currentTimeMillis();
			        rs = ps.executeQuery();
			        long duration = System.currentTimeMillis() - startTime;
			        System.out.println(prefix + "executeQuery[" + statementIndex + "]: " + Long.valueOf(duration) + "");
		        }
		        // Fetch
		        {
			        long startTime = System.currentTimeMillis();
					ResultSetMetaData rsm = rs.getMetaData();	        	
					for(int j = 0; j < rsm.getColumnCount(); j++) {
						@SuppressWarnings("unused")
						String columnName = rsm.getColumnName(j + 1);
					}
			        int count = 0;
			        while(rs.next()) {
			        	if(count == 0) {
					        long duration = System.currentTimeMillis() - startTime;
					        System.out.println(prefix + "rs.next() for first row[" + statementIndex + "]: " + Long.valueOf(duration) + "");
			        		startTime = System.currentTimeMillis();
			        	}
						for(int j = 0; j < rsm.getColumnCount(); j++) {
							@SuppressWarnings("unused")
							Object value = rs.getObject(j + 1);
						}
			        	count++;
			        	if(count > maxRows) {
			        		break;
			        	}
			        }
			        long duration = System.currentTimeMillis() - startTime;
			        System.out.println(prefix + "total fetch time for {" + count + "} rows[" + statementIndex + "]: " + Long.valueOf(duration) + "");
		        }
		        rs.close();
		        ps.close();
			}
		}

		/**
		 * Execute query.
		 * 
		 * @param connection
		 * @param statement
		 * @param fetchSize
		 * @param fetchDirection
		 * @param maxRows
		 * @param loopIndex
		 * @throws SQLException
		 */
		public void executeQuery(
			Connection connection,
			int statementIndex,
			String statement,
			int fetchSize,
			int fetchDirection,
			int maxRows,
			int loopIndex
		) {
			String prefix = new Date() + "   [" + loopIndex + "," + Thread.currentThread().getId() + "] ";
			try {
				
		        System.out.println(prefix + "query: " + statementIndex);
				Statement s = null;
		        System.out.println(prefix + "statement[" + statementIndex + "]: " + statement);
		        System.out.println(prefix + "fetchSize[" + statementIndex + "]: " + fetchSize);
		        System.out.println(prefix + "fetchDirection[" + statementIndex + "]: " + fetchDirection);
		        System.out.println(prefix + "maxRows[" + statementIndex + "]: " + maxRows);
				// Prepare
				{
			        long startTime = System.currentTimeMillis();
					s = connection.createStatement();
			        long duration = System.currentTimeMillis() - startTime;
			        System.out.println(prefix + "prepare time[" + statementIndex + "]: " + Long.valueOf(duration));
				}
				s.setFetchSize(fetchSize);
				s.setFetchDirection(fetchDirection);
				s.setMaxRows(maxRows);
		        // Execute
		        ResultSet rs = null;
		        {
			        long startTime = System.currentTimeMillis();
			        rs = s.executeQuery(statement);
			        long duration = System.currentTimeMillis() - startTime;
			        System.out.println(prefix + "executeQuery[" + statementIndex + "]: " + Long.valueOf(duration) + "");
		        }
		        // Fetch
		        {
			        long startTime = System.currentTimeMillis();
					ResultSetMetaData rsm = rs.getMetaData();	        	
					for(int j = 0; j < rsm.getColumnCount(); j++) {
						@SuppressWarnings("unused")
						String columnName = rsm.getColumnName(j + 1);
					}
			        int count = 0;
			        while(rs.next()) {
			        	if(count == 0) {
					        long duration = System.currentTimeMillis() - startTime;
					        System.out.println(prefix + "rs.next() for first row[" + statementIndex + "]: " + Long.valueOf(duration) + "");
			        		startTime = System.currentTimeMillis();
			        	}
						for(int j = 0; j < rsm.getColumnCount(); j++) {
							@SuppressWarnings("unused")
							Object value = rs.getObject(j + 1);
						}
			        	count++;
			        	if(count > maxRows) {
			        		break;
			        	}
			        }
			        long duration = System.currentTimeMillis() - startTime;
			        System.out.println(prefix + "total fetch time for {" + count + "} rows[" + statementIndex + "]: " + Long.valueOf(duration) + "");
		        }
		        rs.close();
		        s.close();
			} catch(Exception e) {
		        System.out.println(prefix + "error: " + e.getMessage());
			}
		}

		/**
		 * Execute query.
		 * 
		 * @param query
		 * @throws SQLException, ClassNotFoundException
		 */
		@Override
		public void run(
		) {
			for(int loopIndex = 0; loopIndex < this.loops; loopIndex++) {
				String prefix = new Date() + "   [" + loopIndex + "," + Thread.currentThread().getId() + "] ";		
				try {
					Connection connection = null;
					if(this.conn == null) {
						Properties props = new Properties();
						props.put("user", this.query.get("username"));
						props.put("password", this.query.get("password"));
						Class.forName(this.query.getProperty("jdbcDriver"));
						connection = DriverManager.getConnection(this.query.getProperty("jdbcUrl"), props);
					} else {
						Properties props = new Properties();
						props.put("user", this.query.get("username[" + this.conn + "]"));
						props.put("password", this.query.get("password[" + this.conn + "]"));
						Class.forName(query.getProperty("jdbcDriver[" + this.conn + "]"));
						connection = DriverManager.getConnection(this.query.getProperty("jdbcUrl[" + this.conn + "]"), props);				
					}
					connection.setAutoCommit(Boolean.parseBoolean(this.query.getProperty("autoCommit")));
					if(this.query.containsKey("statement")) {
						String statementParameters = this.query.getProperty("statementParameters");
						if(statementParameters == null || statementParameters.isEmpty()) {
							this.executeQuery(
								connection,
								0, // statementIndex
								this.query.getProperty("statement"),
								Integer.valueOf(this.query.getProperty("fetchSize")),
								Integer.valueOf(this.query.getProperty("fetchDirection")),
								Integer.valueOf(this.query.getProperty("maxRows")),
								loopIndex
							);
						} else {
							this.executeQuery(
								connection,
								0, // statementIndex
								this.query.getProperty("statement"),
								new ArrayList<String>(Arrays.asList(statementParameters.split(","))),
								Integer.valueOf(this.query.getProperty("fetchSize")),
								Integer.valueOf(this.query.getProperty("fetchDirection")),
								Integer.valueOf(this.query.getProperty("maxRows")),
								loopIndex
							);
						}
					} else {
						int statementIndex = 0;
				        long startTime = System.currentTimeMillis();				
						while(this.query.containsKey("statement[" + statementIndex + "]")) {
							try {
								System.out.println(prefix + "#");
								String statementParameters = this.query.getProperty("statementParameters[" + statementIndex + "]"); 
								if(statementParameters == null || statementParameters.isEmpty()) {
									this.executeQuery(
										connection,
										statementIndex,
										this.query.getProperty("statement[" + statementIndex + "]"),
										Integer.valueOf(this.query.getProperty("fetchSize")),
										Integer.valueOf(this.query.getProperty("fetchDirection")),
										Integer.valueOf(this.query.getProperty("maxRows")),
										loopIndex
									);						
								} else {
									this.executeQuery(
										connection,
										statementIndex,
										this.query.getProperty("statement[" + statementIndex + "]"),
										new ArrayList<String>(Arrays.asList(statementParameters.split(","))),
										Integer.valueOf(this.query.getProperty("fetchSize")),
										Integer.valueOf(this.query.getProperty("fetchDirection")),
										Integer.valueOf(this.query.getProperty("maxRows")),
										loopIndex
									);
								}
							} catch(Exception e) {
						        System.out.println(prefix + "error: " + e.getMessage());									
							}
							statementIndex++;
						}
				        long duration = System.currentTimeMillis() - startTime;
						System.out.println(prefix + "#");
				        System.out.println(prefix + "total execution time: " + Long.valueOf(duration));				
					}
				} catch(Exception e) {
			        System.out.println(prefix + "error: " + e.getMessage());									
				}
			}
		}
		
		private final String conn;
		private final Integer loops;
		private final Properties query;
	}
	
	/**
	 * QueryTool.
	 * 
	 * @param args
	 */
	public static void main(
		String[] args
	) throws SQLException, IOException, ClassNotFoundException {
		{
			String queryFile = null;
			String conn = null;
			Integer loops = 1;
			Integer threads = 1;
			for(int i = 0; i < args.length; i++) {
				if("--query".equals(args[i])) {
					queryFile = args[i+1];
				} else if("--conn".equals(args[i])) {
					conn = args[i+1];
				} else if("--loops".equals(args[i])) {
					loops = Integer.parseInt(args[i+1]);
				} else if("--threads".equals(args[i])) {
					threads = Integer.parseInt(args[i+1]);
				}
			}
			Properties query = new Properties();
			query.load(new FileInputStream(queryFile));
			List<Thread> queryExecutors = new ArrayList<Thread>();
			for(int i = 0; i < threads; i++) {
				Thread queryExecutor = new Thread(new QueryExecutor(query, conn, loops));
				queryExecutors.add(queryExecutor);
				queryExecutor.start();
			}
			for(Thread queryExecutor: queryExecutors) {
				try {
					queryExecutor.join();
				} catch(Exception ignore) {}
			}
		}
	}

	// -----------------------------------------------------------------------
	// Members
	// -----------------------------------------------------------------------

}
