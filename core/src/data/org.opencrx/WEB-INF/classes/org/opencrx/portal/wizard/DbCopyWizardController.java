/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DbCopyWizardController
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
package org.opencrx.portal.wizard;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collections;

import org.opencrx.kernel.utils.DbSchemaUtils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.JspWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * DbCopyWizardController
 *
 */
public class DbCopyWizardController extends JspWizardController {
	
	/**
	 * Constructor.
	 */
	public DbCopyWizardController(
	) {
		super();
	}
	
	/**
	 * ProgressMeter
	 *
	 */
	public static class ProgressMeter {		
		
		/**
		 * @return the report
		 */
		public ByteArrayOutputStream getReport() {
			return report;
		}

		/**
		 * @param report the report to set
		 */
		public void setReport(ByteArrayOutputStream report) {
			this.report = report;
		}

		/**
		 * @return the done
		 */
		public boolean isDone() {
			return done;
		}

		/**
		 * @param done the done to set
		 */
		public void setDone(boolean done) {
			this.done = done;
		}

		private ByteArrayOutputStream report;
		private boolean done;

	}

	/**
	 * FormFields
	 *
	 */
	public static class FormFields {
	
		/**
		 * @return the jdbcUrlSource
		 */
		public String getJdbcUrlSource() {
			return jdbcUrlSource;
		}
		/**
		 * @param jdbcUrlSource the jdbcUrlSource to set
		 */
		public void setJdbcUrlSource(String jdbcUrlSource) {
			this.jdbcUrlSource = jdbcUrlSource;
		}
		/**
		 * @return the usernameSource
		 */
		public String getUsernameSource() {
			return usernameSource;
		}
		/**
		 * @param usernameSource the usernameSource to set
		 */
		public void setUsernameSource(String usernameSource) {
			this.usernameSource = usernameSource;
		}
		/**
		 * @return the passwordSource
		 */
		public String getPasswordSource() {
			return passwordSource;
		}
		/**
		 * @param passwordSource the passwordSource to set
		 */
		public void setPasswordSource(String passwordSource) {
			this.passwordSource = passwordSource;
		}
		/**
		 * @return the jdbcUrlTarget
		 */
		public String getJdbcUrlTarget() {
			return jdbcUrlTarget;
		}
		/**
		 * @param jdbcUrlTarget the jdbcUrlTarget to set
		 */
		public void setJdbcUrlTarget(String jdbcUrlTarget) {
			this.jdbcUrlTarget = jdbcUrlTarget;
		}
		/**
		 * @return the usernameTarget
		 */
		public String getUsernameTarget() {
			return usernameTarget;
		}
		/**
		 * @param usernameTarget the usernameTarget to set
		 */
		public void setUsernameTarget(String usernameTarget) {
			this.usernameTarget = usernameTarget;
		}
		/**
		 * @return the passwordTarget
		 */
		public String getPasswordTarget() {
			return passwordTarget;
		}
		/**
		 * @param passwordTarget the passwordTarget to set
		 */
		public void setPasswordTarget(String passwordTarget) {
			this.passwordTarget = passwordTarget;
		}
		/**
		 * @return the includeDbObjects
		 */
		public String getIncludeDbObjects() {
			return includeDbObjects;
		}
		/**
		 * @param includeDbObjects the includeDbObjects to set
		 */
		public void setIncludeDbObjects(String includeDbObjects) {
			this.includeDbObjects = includeDbObjects;
		}
		/**
		 * @return the excludeDbObjects
		 */
		public String getExcludeDbObjects() {
			return excludeDbObjects;
		}
		/**
		 * @param excludeDbObjects the excludeDbObjects to set
		 */
		public void setExcludeDbObjects(String excludeDbObjects) {
			this.excludeDbObjects = excludeDbObjects;
		}
		public String getValuePatterns() {
			return valuePatterns;
		}
		public void setValuePatterns(String valuePatterns) {
			this.valuePatterns = valuePatterns;
		}
		public String getValueReplacements() {
			return valueReplacements;
		}
		public void setValueReplacements(String valueReplacements) {
			this.valueReplacements = valueReplacements;
		}
		
		private String jdbcUrlSource;
		private String usernameSource;
		private String passwordSource;
		private String jdbcUrlTarget;
		private String usernameTarget;
		private String passwordTarget;
		private String includeDbObjects;
		private String excludeDbObjects;
		private String valuePatterns;
		private String valueReplacements;
		
	}
	
	/**
	 * Refresh action.
	 * 
	 * @param formFields
	 */
	public void doRefresh(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) {
		this.formFields = formFields;
	}

	/**
	 * Copy action.
	 * 
	 * @param formFields
	 */
	public void doCopy(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields		
	) {
		this.doRefresh(formFields);
		ProgressMeter progressMeter = new ProgressMeter();
		this.getSession().setAttribute(
			ProgressMeter.class.getName(),
			progressMeter
		);
		try {
			progressMeter.setReport(new ByteArrayOutputStream());
			progressMeter.setDone(false);
			String includeDbObjects = this.formFields.getIncludeDbObjects();
			String excludeDbObjects = this.formFields.getExcludeDbObjects();
			String valuePatterns = this.formFields.getValuePatterns();
			String valueReplacements = this.formFields.getValueReplacements();
			org.opencrx.kernel.tools.CopyDb.copyDb(
				DbSchemaUtils.getJdbcDriverName(this.formFields.getJdbcUrlSource()),
				this.formFields.getUsernameSource(),
				this.formFields.getPasswordSource(),
				this.formFields.getJdbcUrlSource(),
				DbSchemaUtils.getJdbcDriverName(this.formFields.getJdbcUrlTarget()),
				this.formFields.getUsernameTarget(),
				this.formFields.getPasswordTarget(),
				this.formFields.getJdbcUrlTarget(),
				includeDbObjects == null ? Collections.<String>emptyList() : Arrays.asList(includeDbObjects.split("[,\r\n]")),
				excludeDbObjects == null ? Collections.<String>emptyList() : Arrays.asList(excludeDbObjects.split("[,\r\n]")),
				valuePatterns == null ? Collections.<String>emptyList() : Arrays.asList(valuePatterns.split("[,\r\n]")),
				valueReplacements == null ? Collections.<String>emptyList() : Arrays.asList(valueReplacements.split("[,\r\n]")),
				new PrintStream(progressMeter.getReport())
			);
		} catch(Exception e) {
			ServiceException e0 = new ServiceException(e);
			e0.printStackTrace(new PrintStream(progressMeter.getReport()));
		}
		progressMeter.setDone(true);
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
	 * Clear action.
	 * 
	 * @param formFields
	 */
	public void doClear(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields		
	) {
		this.doRefresh(formFields);
		this.getSession().setAttribute(
			ProgressMeter.class.getName(),
			null
		);
	}

	/**
	 * Refresh report action.
	 * 
	 */
	public void doRefreshReport(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields		
	) {
		this.doRefresh(formFields);
	}

	/**
	 * Returns true if copy is running.
	 * 
	 * @return
	 */
	public Boolean isRunning(
	) {
		ProgressMeter progressMeter = this.getProgressMeter();
		return progressMeter == null
			? null
			: !progressMeter.isDone();
	}

	/**
	 * Get progress meter.
	 * 
	 * @return
	 */
	public ProgressMeter getProgressMeter(
	) {
		return (ProgressMeter)this.getSession().getAttribute(
			ProgressMeter.class.getName()
		);
	}

	/**
	 * @return the formFields
	 */
	public FormFields getFormFields() {
		return formFields;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private FormFields formFields;
}
