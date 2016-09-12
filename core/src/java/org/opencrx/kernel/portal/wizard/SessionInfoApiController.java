/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SessionInfoApiController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2016, CRIXP Corp., Switzerland
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

import java.io.Reader;

import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractApiController;
import org.openmdx.portal.servlet.ApplicationContext;

/**
 * SessionInfoApiController
 *
 */
public class SessionInfoApiController extends AbstractApiController {

	public static class DataBean {

		public String getLoginPrincipal(
		) {
			return this.loginPrincipal;
		}
		public void setLoginPrincipal(
			String loginPrincipal
		) {
			this.loginPrincipal = loginPrincipal;
		}
		public String getLocale(
		) {
			return this.locale;
		}
		public void setLocale(
			String locale
		) {
			this.locale = locale;
		}
		public void setUserRole(
			String userRole
		) {
			this.userRole = userRole;
		}
		public String getUserRole(
		) {
			return this.userRole;
		}
		public void setTimeZone(
			String timeZone
		) {
			this.timeZone = timeZone;
		}
		public String getTimeZone(
		) {
			return this.timeZone;
		}
		public void setSessionId(
			String sessionId
		) {
			this.sessionId = sessionId;
		}
		public String getSessionId(
		) {
			return this.sessionId;
		}
		private String loginPrincipal;
		private String locale;
		private String userRole;
		private String timeZone;
		private String sessionId;
	}

	public static class MetaInfBean {
		
		/**
		 * @return the labelProvider
		 */
		public String getLabelProvider() {
			return labelProvider;
		}
		/**
		 * @param labelProvider the labelProvider to set
		 */
		public void setLabelProvider(String labelProvider) {
			this.labelProvider = labelProvider;
		}
		/**
		 * @return the labelSegment
		 */
		public String getLabelSegment() {
			return labelSegment;
		}
		/**
		 * @param labelSegment the labelSegment to set
		 */
		public void setLabelSegment(String labelSegment) {
			this.labelSegment = labelSegment;
		}
		/**
		 * @return the labelLoginPrincipal
		 */
		public String getLabelLoginPrincipal() {
			return labelLoginPrincipal;
		}
		/**
		 * @param labelLoginPrincipal the labelLoginPrincipal to set
		 */
		public void setLabelLoginPrincipal(String labelLoginPrincipal) {
			this.labelLoginPrincipal = labelLoginPrincipal;
		}
		/**
		 * @return the labelLocale
		 */
		public String getLabelLocale() {
			return labelLocale;
		}
		/**
		 * @param labelLocale the labelLocale to set
		 */
		public void setLabelLocale(String labelLocale) {
			this.labelLocale = labelLocale;
		}
		/**
		 * @return the labelUserRole
		 */
		public String getLabelUserRole() {
			return labelUserRole;
		}
		/**
		 * @param labelUserRole the labelUserRole to set
		 */
		public void setLabelUserRole(String labelUserRole) {
			this.labelUserRole = labelUserRole;
		}
		/**
		 * @return the labelTimeZone
		 */
		public String getLabelTimeZone() {
			return labelTimeZone;
		}
		/**
		 * @param labelTimeZone the labelTimeZone to set
		 */
		public void setLabelTimeZone(String labelTimeZone) {
			this.labelTimeZone = labelTimeZone;
		}
		/**
		 * @return the labelSessionId
		 */
		public String getLabelSessionId() {
			return labelSessionId;
		}
		/**
		 * @param labelSessionId the labelSessionId to set
		 */
		public void setLabelSessionId(String labelSessionId) {
			this.labelSessionId = labelSessionId;
		}
		private String labelProvider;
		private String labelSegment;
		private String labelLoginPrincipal;
		private String labelLocale;
		private String labelUserRole;
		private String labelTimeZone;
		private String labelSessionId;
	}
	
	public SessionInfoApiController(
	) {
		com.google.gson.GsonBuilder gsonBuilder = new com.google.gson.GsonBuilder();
		gsonBuilder.setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
		this.gson = gsonBuilder.create();			
	}

	public Object fromJson(
		Reader reader,
		Class<?> clazz
	) throws ServiceException {
		return this.gson.fromJson(reader, clazz);
	}

	public String toJson(
		Object object
	) throws ServiceException {
		return this.gson.toJson(object);
	}
	
	public DataBean getData(
		Path path
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		DataBean dataBean = new DataBean();
		dataBean.setLoginPrincipal(app.getLoginPrincipal());
		dataBean.setLocale(app.getCurrentLocaleAsString());
		dataBean.setUserRole(app.getCurrentUserRole());
		dataBean.setTimeZone(app.getCurrentTimeZone());
		dataBean.setSessionId(app.getSessionId());
		return dataBean;
	}

	public MetaInfBean getMetaInf(
		Path path
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		MetaInfBean metaInfBean = new MetaInfBean();
		metaInfBean.setLabelProvider("Provider");
		metaInfBean.setLabelSegment("Segment");
		metaInfBean.setLabelLoginPrincipal(app.getLabel("org:openmdx:security:realm1:Principal"));
		metaInfBean.setLabelLocale(this.getLabel("org:opencrx:kernel:generic:LocalizedField:locale"));
		metaInfBean.setLabelUserRole(app.getLabel("org:openmdx:security:realm1:Role"));
		metaInfBean.setLabelTimeZone(this.getLabel("org:opencrx:kernel:address1:Addressable:tz"));
		metaInfBean.setLabelSessionId("Session Id");
		return metaInfBean;
	}

	private final com.google.gson.Gson gson;

}
