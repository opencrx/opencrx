/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DbSchemaWizardController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.wizard;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import javax.jdo.PersistenceManager;
import javax.naming.Context;
import javax.naming.InitialContext;

import org.opencrx.kernel.utils.DbSchemaUtils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * DbSchemaWizardController
 *
 */
public class DbSchemaWizardController extends AbstractWizardController {

	/**
	 * FormFields
	 *
	 */
	public static class FormFields {
	
		/**
		 * @return the connectionUrl
		 */
		public String getConnectionUrl() {
			return connectionUrl;
		}
		/**
		 * @param connectionUrl the connectionUrl to set
		 */
		public void setConnectionUrl(String connectionUrl) {
			this.connectionUrl = connectionUrl;
		}
		/**
		 * @return the userName
		 */
		public String getUserName() {
			return userName;
		}
		/**
		 * @param userName the userName to set
		 */
		public void setUserName(String userName) {
			this.userName = userName;
		}
		/**
		 * @return the password
		 */
		public String getPassword() {
			return password;
		}
		/**
		 * @param password the password to set
		 */
		public void setPassword(String password) {
			this.password = password;
		}
		private String connectionUrl;
		private String userName;
		private String password;
		
	}

	/**
	 * Get database connection.
	 * 
	 * @param connectionUrl
	 * @param userName
	 * @param password
	 * @return
	 */
	protected Connection getConnection(
		String connectionUrl,
		String userName,
		String password
	) {
		Connection connT = null;
		try {
			if(connectionUrl != null && connectionUrl.startsWith("java:")) {
				Context initialContext = new InitialContext();
				javax.sql.DataSource dsT = (javax.sql.DataSource)initialContext.lookup(connectionUrl);
				connT = dsT.getConnection();
			} else {
				try {
					String driverName = org.opencrx.kernel.utils.DbSchemaUtils.getJdbcDriverName(connectionUrl);
					Class.forName(driverName);
				} catch (Exception e) {
					this.report.add("ERROR: Unable to load database driver (message=" + e.getMessage() + ")");					
				}
				connT = DriverManager.getConnection(connectionUrl, userName, password);				
			}
		} catch(Exception e) {
			this.report.add("ERROR: unable to get connection to database (message=" + e.getMessage() + ")");
		}
		return connT;
	}
	
	/**
	 * Validate and fix database schema.
	 * 
	 * @param connectionUrl
	 * @param userName
	 * @param password
	 * @param fix
	 * @throws ServiceException
	 * @throws SQLException
	 */
	protected void validateSchema(
		String connectionUrl,
		String userName,
		String password,
		boolean fix
	) throws ServiceException, SQLException {
		// Get connection to running db
		System.out.print(new java.util.Date() + ": DbSchemaWizard  Getting connection " + connectionUrl.toString() + " ... ");
		Connection connT = this.getConnection(connectionUrl, userName, password);
		if(connT != null) {
			System.out.println("done");
			// Validate
			System.out.print(new java.util.Date() + ": DbSchemaWizard  Validating " + (fix ? "and fixing " : "") + "tables ... ");
			this.report.addAll(
				org.opencrx.kernel.utils.DbSchemaUtils.validateTables(
					connT,
					fix
				)
			);
			System.out.println("done");
			System.out.print(new java.util.Date() + ": DbSchemaWizard  Validating " + (fix ? "and fixing " : "") + "columns ... ");
			this.report.addAll(
				org.opencrx.kernel.utils.DbSchemaUtils.validateTableColumns(
					connT,
					fix
				)
			);
			System.out.println("done");
			System.out.print(new java.util.Date() + ": DbSchemaWizard  Migrating data ... ");
			this.report.addAll(
				org.opencrx.kernel.utils.DbSchemaUtils.migrateData(
					connT,
					true // migrate by default
				)
			);
			System.out.println("done");
			System.out.print(new java.util.Date() + ": DbSchemaWizard  Validating " + (fix ? "and fixing " : "") + "views ... ");
			this.report.addAll(
				org.opencrx.kernel.utils.DbSchemaUtils.validateViews(
					connT,
					fix
				)
			);
			System.out.println("done");
			System.out.print(new java.util.Date() + ": DbSchemaWizard  Validating " + (fix ? "and fixing " : "") + "indexes ... ");
			this.report.addAll(
				org.opencrx.kernel.utils.DbSchemaUtils.validateIndexes(
					connT,
					fix
				)
			);
			System.out.println("done");
			System.out.print(new java.util.Date() + ": DbSchemaWizard  Validating " + (fix ? "and fixing " : "") + "sequences ... ");
			this.report.addAll(
				org.opencrx.kernel.utils.DbSchemaUtils.validateSequences(
					connT,
					fix
				)
			);
			System.out.println("done");
		}		
	}

	/**
	 * Refresh action.
	 * 
	 * @param formFields
	 */
	public void doRefresh(
		@RequestParameter(type = "Bean") FormFields formFields		
	) {
		this.formFields = formFields;
		this.report = new ArrayList<String>();
		// HSQLDB always required for reading reference schema
		try {
			Class.forName(
				org.opencrx.kernel.utils.DbSchemaUtils.getJdbcDriverName("jdbc:hsqldb:")
			);
		} catch(Exception ignore) {}
	}
	
	/**
	 * Validate action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 * @throws SQLException
	 */
	public void doValidate(
		@RequestParameter(type = "Bean") FormFields formFields		
	) throws ServiceException, SQLException {
		this.doRefresh(formFields);
		this.validateSchema(
			this.formFields.getConnectionUrl(), 
			this.formFields.getUserName(), 
			this.formFields.getPassword(), 
			false // fix
		);
	}

	/**
	 * ValidateAndFix action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 * @throws SQLException
	 */
	public void doValidateAndFix(
		@RequestParameter(type = "Bean") FormFields formFields		
	) throws ServiceException, SQLException {
		this.doRefresh(formFields);		
		this.validateSchema(
			this.formFields.getConnectionUrl(), 
			this.formFields.getUserName(), 
			this.formFields.getPassword(), 
			true // fix
		);
	}

	/**
	 * Migrate media to FS action.
	 * 
	 * @param formFields
	 */
	public void doMigrateMediaToFS(
		@RequestParameter(type = "Bean") FormFields formFields		
	) {
		this.doRefresh(formFields);		
		PersistenceManager pm = this.getPm();
		String providerName = this.getProviderName();
		Connection connT = this.getConnection(
			formFields.getConnectionUrl(), 
			formFields.getUserName(), 
			formFields.getPassword()
		);
		if(connT != null) {
			try  {
	 			this.report.addAll(
					DbSchemaUtils.migrateMediaToFS(providerName, pm, connT, false)
				);
			} catch(Exception e) {
				new ServiceException(e).log();
				this.report.add("ERROR: message is " + e.getMessage());
			}
		}
	}

	/**
	 * Migrate media to DB action.
	 * 
	 * @param formFields
	 */
	public void doMigrateMediaToDB(
		@RequestParameter(type = "Bean") FormFields formFields		
	) {
		this.doRefresh(formFields);		
		PersistenceManager pm = this.getPm();
		String providerName = this.getProviderName();
		try {
			this.report.addAll(
				DbSchemaUtils.migrateMediaToDB(providerName, pm)
			);
		} catch(Exception e) {
			new ServiceException(e).log();
			this.report.add("ERROR: message is " + e.getMessage());
		}
	}

	/**
	 * Validate media action.
	 * 
	 * @param formFields
	 */
	public void doValidateMedia(
		@RequestParameter(type = "Bean") FormFields formFields		
	) {
		this.doRefresh(formFields);		
		PersistenceManager pm = this.getPm();
		String providerName = this.getProviderName();
		Connection connT = this.getConnection(
			formFields.getConnectionUrl(), 
			formFields.getUserName(), 
			formFields.getPassword()
		);		
		try {
			this.report.addAll(
				DbSchemaUtils.migrateMediaToFS(providerName, pm, connT, true)
			);
		} catch(Exception e) {
			new ServiceException(e).log();
			this.report.add("ERROR: message is " + e.getMessage());
		}
	}

	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(
	) {
		this.setExitAction(
			 new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}
	
	/**
	 * @return the formFields
	 */
	public FormFields getFormFields() {
		return formFields;
	}
	
	/**
	 * @return the report
	 */
	public List<String> getReport() {
		return report;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	private FormFields formFields;
	private List<String> report;

}
