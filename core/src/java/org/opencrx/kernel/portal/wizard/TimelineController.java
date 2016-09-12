/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TimelineController
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

import java.net.URLEncoder;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;

/**
 * TimelineController
 *
 */
public class TimelineController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public TimelineController(
	) {
		super();
	}
	
	/**
	 * URL encode given string.
	 * 
	 * @param s
	 * @return
	 */
	public String encodeUrl(
		String s
	) {
		try {
			return URLEncoder.encode(s, "UTF-8");
		} catch(Exception ignore) {}
		return "";
	}

	/**
	 * Refresh action.
	 * 
	 */
	public void doRefresh(
	) {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		RefObject_1_0 obj = this.getObject();
		String groupComponent = "";
		String filterComponent = "";
		// activity filter
		if(obj instanceof ActivityFilterGroup) {
			ActivityFilterGroup activityFilterGroup = (ActivityFilterGroup)obj;
			if ((activityFilterGroup.getName() != null) && !activityFilterGroup.getName().isEmpty()) {
				filterComponent = "/filter/" + this.encodeUrl(activityFilterGroup.getName());
			}
			obj = (RefObject_1_0)pm.getObjectById(activityFilterGroup.refGetPath().getParent().getParent());
		} else if (obj instanceof ActivityFilterGlobal) {
			ActivityFilterGlobal activityFilterGlobal = (ActivityFilterGlobal)obj;
			if ((activityFilterGlobal.getName() != null) && !activityFilterGlobal.getName().isEmpty()) {
				filterComponent = "/globalfilter/" + this.encodeUrl(activityFilterGlobal.getName());
			}
		}
		// activity group
		if(obj instanceof ActivityTracker) {
			ActivityTracker activityTracker = (ActivityTracker)obj;
			if ((activityTracker.getName() != null) && !activityTracker.getName().isEmpty()) {
				groupComponent = "/tracker/" + this.encodeUrl(activityTracker.getName());
			}
		} else if(obj instanceof ActivityCategory) {
			ActivityCategory activityCategory =
				(ActivityCategory)obj;
			if ((activityCategory.getName() != null) && !activityCategory.getName().isEmpty()) {
				groupComponent = "/category/" + this.encodeUrl(activityCategory.getName());
			}
		} else if(obj instanceof ActivityMilestone) {
			ActivityMilestone activityMilestone = (ActivityMilestone)obj;
			if ((activityMilestone.getName() != null) && !activityMilestone.getName().isEmpty()) {
				groupComponent = "/milestone/" + this.encodeUrl(activityMilestone.getName());
			}
		} else if(obj instanceof Resource) {
			Resource resource = (Resource)obj;
			if ((resource.getName() != null) && !resource.getName().isEmpty()) {
				groupComponent = "/resource/" + this.encodeUrl(resource.getName());
			}
		} else if(obj instanceof UserHome) {
			groupComponent = "/userhome/" + this.encodeUrl(obj.refGetPath().getBase());
		} else if (obj instanceof AccountFilterGlobal) {
			AccountFilterGlobal accountFilterGlobal = (AccountFilterGlobal)obj;
			if ((accountFilterGlobal.getName() != null) && !accountFilterGlobal.getName().isEmpty()) {
				filterComponent = "/filter/" + this.encodeUrl(accountFilterGlobal.getName());
			}
		}
		if (!groupComponent.isEmpty() || !filterComponent.isEmpty()) {
			this.targetUrl =
				this.getRequest().getContextPath().replace("-core-", "-ical-") + "/" +
				(obj instanceof AccountFilterGlobal ? "bdays" : "ical") + "?id=" +
				this.getProviderName() + "/" + this.getSegmentName() +
				groupComponent + filterComponent +
				"&resource=activities.html&user.locale=" + this.encodeUrl(app.getCurrentLocaleAsString()) + "&user.tz=" + this.encodeUrl(app.getCurrentTimeZone());
		}
	}

	/**
	 * @return the targetUrl
	 */
	public String getTargetUrl() {
		return targetUrl;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private String targetUrl;

}
