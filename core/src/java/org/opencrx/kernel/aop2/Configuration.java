/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.aop2;

public class Configuration {

	public Configuration(		
	) {		
		try {
			// Register backend classes
			org.opencrx.kernel.backend.Accounts.register();
			org.opencrx.kernel.backend.Activities.register();
			org.opencrx.kernel.backend.Addresses.register();
			org.opencrx.kernel.backend.Buildings.register();
			org.opencrx.kernel.backend.Admin.register();
			org.opencrx.kernel.backend.Base.register();
			org.opencrx.kernel.backend.Cloneable.register();	
			org.opencrx.kernel.backend.Contracts.register();
			org.opencrx.kernel.backend.Depots.register();
			org.opencrx.kernel.backend.Documents.register();
			org.opencrx.kernel.backend.Exporter.register();
			org.opencrx.kernel.backend.Forecasts.register();
			org.opencrx.kernel.backend.ICalendar.register();
			org.opencrx.kernel.backend.XmlImporter.register();
			org.opencrx.kernel.backend.Models.register();
			org.opencrx.kernel.backend.Notifications.register();
			org.opencrx.kernel.backend.Products.register();
			org.opencrx.kernel.backend.SecureObject.register();
			org.opencrx.kernel.backend.UserHomes.register();
			org.opencrx.kernel.backend.VCard.register();
			org.opencrx.kernel.backend.Workflows.register();
		}
		catch(Exception e) {}
	}
	
	public void setPasswordEncodingAlgorithm(
		String value
	) {
		this.passwordEncodingAlgorithm = value;
	}
	
	public String getPasswordEncodingAlgorithm(
	) {
		return this.passwordEncodingAlgorithm;
	}
	
	public boolean isEMailAddressLookupCaseInsensitive(
	) {
		return this.isEMailAddressLookupCaseInsensitive;
	}
	
	public void setEMailAddressLookupCaseInsensitive(
		boolean value
	) {
		this.isEMailAddressLookupCaseInsensitive = value;
	}
	
	public boolean isEMailAddressLookupIgnoreDisabled(
	) {
		return this.isEMailAddressLookupIgnoreDisabled;
	}
	
	public void setEMailAddressLookupIgnoreDisabled(
		boolean value
	) {
		this.isEMailAddressLookupIgnoreDisabled = value;
	}
	
	private String passwordEncodingAlgorithm;
	private boolean isEMailAddressLookupCaseInsensitive = true;
	private boolean isEMailAddressLookupIgnoreDisabled = false;
	
}
