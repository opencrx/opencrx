/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: LogConfigurationController
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

import java.util.Collection;
import java.util.Enumeration;

import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * LogConfigurationController
 *
 */
public class LogConfigurationController extends AbstractWizardController {

	/**
	 * LogConfigurationController.
	 * 
	 */
	public LogConfigurationController(
	) {
		super();
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
	 * Refresh action.
	 * 
	 */
	public void doRefresh(
		@RequestParameter(name = "levelAllLoggers") Integer levelAllLoggers		
	) {
		this.levelAllLoggers = levelAllLoggers;
		if(this.levelAllLoggers == null) {
			this.levelAllLoggers = LOG_LEVEL_INDIVIDUAL;
		}		
	}

	/**
	 * OK action.
	 * 
	 */
	public void doOK(
		@RequestParameter(name = "levelAllLoggers") Integer levelAllLoggers		
	) {
		this.doRefresh(
			levelAllLoggers
		);
		java.util.logging.LogManager logManager = java.util.logging.LogManager.getLogManager();
		Enumeration<String> parameterNames = this.getRequest().getParameterNames();
		while(parameterNames.hasMoreElements()) {
			String parameterName = parameterNames.nextElement();
			if(parameterName.startsWith("Logger.")) {
				String loggerName = parameterName.substring(7);
				java.util.logging.Level logLevel = this.levelAllLoggers == LOG_LEVEL_INDIVIDUAL
					? java.util.logging.Level.parse(this.getRequest().getParameter(parameterName))
					: java.util.logging.Level.parse(Integer.toString(levelAllLoggers));
				java.util.logging.Logger logger = logManager.getLogger(loggerName);
				if(logger != null) {
					logger.setLevel(logLevel);
					for(java.util.logging.Handler handler: logger.getHandlers()) {
						handler.setLevel(logLevel);
					}
				}
				// Set level for all loggers registered by openMDX logger factory
				if(org.openmdx.kernel.log.LoggerFactory.STANDARD_LOGGER_NAME.equals(loggerName)) {
					Collection<java.util.logging.Logger> loggers = org.openmdx.kernel.log.LoggerFactory.getLoggers();
					for(java.util.logging.Logger l: loggers) {
						l.setLevel(logLevel);
						for(java.util.logging.Handler handler: l.getHandlers()) {
							handler.setLevel(logLevel);
						}
					}
				}
			}
		}
	}

	/**
	 * @return the levelAllLoggers
	 */
	public Integer getLevelAllLoggers() {
		return levelAllLoggers;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final int LOG_LEVEL_INDIVIDUAL = 9999;
	
	private Integer levelAllLoggers;

}
