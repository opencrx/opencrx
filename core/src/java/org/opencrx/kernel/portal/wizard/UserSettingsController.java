/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: UserSettingsController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.jdo.PersistenceManager;
import javax.servlet.http.HttpServletRequest;

import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.component.ShowObjectView;

/**
 * UserSettingsController
 *
 */
public class UserSettingsController extends org.openmdx.portal.servlet.AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public UserSettingsController(
   	) {
   		super();
   	}

	/**
	 * Return true if current user is owner of given user home.
	 * 
	 * @param userHome
	 * @return
	 */
	public boolean currentUserOwnsHome(
		UserHome userHome
	) {
		ApplicationContext app = this.getApp();
		String segmentName = this.getSegmentName();
		return app.getCurrentUserRole().equals(userHome.refGetPath().getLastSegment().toClassicRepresentation() + "@" + segmentName);		
	}

	/**
	 * Return true if current user is segment admin.
	 * 
	 * @return
	 */
	public boolean currentUserIsAdmin(
	) {
		ApplicationContext app = this.getApp();
		String segmentName = this.getSegmentName();
		return app.getCurrentUserRole().equals(org.opencrx.kernel.generic.SecurityKeys.ADMIN_PRINCIPAL + org.opencrx.kernel.generic.SecurityKeys.ID_SEPARATOR + segmentName + "@" + segmentName);		
	}
	
	/**
	 * Refresh action.
	 * 
	 * @throws ServiceException
	 */
	public void doRefresh(
	) throws ServiceException {
		RefObject_1_0 obj = this.getObject();
   		if(obj instanceof UserHome) {
			UserHome userHome = (UserHome)obj;
			this.userSettings =  new Properties();
			if(userHome.getSettings() != null) {
				try {
					this.userSettings.load(
						new ByteArrayInputStream(
							userHome.getSettings().getBytes("UTF-8")
						)
					);
				} catch(Exception ignore) {}
			}
   		}
		this.selectPerspectiveActions = ((ShowObjectView)this.getCurrentView()).getSelectPerspectiveAction();
	}

	/**
	 * OK action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
	) throws ServiceException {
	    this.doRefresh();
		ApplicationContext app = this.getApp();
		RefObject_1_0 obj = this.getObject();
		PersistenceManager pm = this.getPm();
		if(obj instanceof UserHome) {
			HttpServletRequest request = this.getRequest();
			org.opencrx.kernel.home1.jmi1.UserHome userHome = (UserHome)obj;
			boolean currentUserOwnsHome = this.currentUserOwnsHome(userHome);
			String fTimezone = request.getParameter("timezone");
			String fStoreSettingsOnLogoff = request.getParameter("storeSettingsOnLogoff");
			String fEmailAccount = request.getParameter("emailAccount");
			String fSendmailSubjectPrefix = request.getParameter("sendmailSubjectPrefix");
			String fWebAccessUrl = request.getParameter("webAccessUrl");
			String fTopNavigationShowMax = request.getParameter("topNavigationShowMax");
			String fHideWorkspaceDashboard = request.getParameter("hideWorkspaceDashboard");
			String fAnchorUserDialog = request.getParameter("anchorUserDialog");
			List<List<String>> perspectiveRootObjects = new ArrayList<List<String>>();
			for(int p = 0; p < selectPerspectiveActions.length; p++) {
				List<String> fRootObjects = new ArrayList<String>();
				fRootObjects.add("1");
				for(int i = 1; i < 20; i++) {
					String state = request.getParameter("rootObject" + p + "_" + i);
					if(i < app.getRootObject().length && app.getRootObject()[i] instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
						state = "1";
					}
					fRootObjects.add(
						state == null ? "0" : "1"
					);
				}
				perspectiveRootObjects.add(fRootObjects);
			}
			Map<String,String> fSubscriptions = new HashMap<String,String>();
			Enumeration<String> parameterNames = request.getParameterNames();
			while(parameterNames.hasMoreElements()) {
				String parameterName = parameterNames.nextElement();
				if(
					parameterName.startsWith("topicIsActive-") ||
					parameterName.startsWith("topicCreation-") ||
					parameterName.startsWith("topicReplacement-") ||
					parameterName.startsWith("topicRemoval-")
				) {
					fSubscriptions.put(
						parameterName,
						request.getParameter(parameterName)
					);
				}
			}
			for(int p = 0; p < selectPerspectiveActions.length; p++) {
				try {
					pm.currentTransaction().begin();
					Properties userSettings = UserHomes.getInstance().getDefaultUserSettings(userHome);
					// admin settings are prefixed with a * and override the default and user settings
					Properties adminSettings = new Properties();
					for(Iterator<Map.Entry<Object,Object>> i = userSettings.entrySet().iterator(); i.hasNext(); ) {
						Map.Entry<Object,Object> e = i.next();
						String key = (String)e.getKey();
						if(key.startsWith("*")) {
							adminSettings.put(
								key.substring(1),
								e.getValue()
							);
							i.remove();
						}
					}
					userSettings.putAll(this.getUserSettings());
					userSettings.putAll(adminSettings);
					org.opencrx.kernel.backend.UserHomes.getInstance().applyUserSettings(
						userHome,
						p,
						userSettings,
						!currentUserOwnsHome, // noInitUserHome
						true, // store settings
						userHome.getPrimaryGroup(),
						fTimezone,
						fStoreSettingsOnLogoff,
						fEmailAccount,
						fSendmailSubjectPrefix,
						fWebAccessUrl,
						fTopNavigationShowMax,
						"on".equals(fHideWorkspaceDashboard),
						"on".equals(fAnchorUserDialog),
						perspectiveRootObjects.get(p),
						fSubscriptions
					);
					pm.currentTransaction().commit();
					this.doRefresh(); // Refresh applied (and persistent) settings
					if(this.currentUserOwnsHome(userHome)) {
						app.getSettings().clear();
						app.getSettings().putAll(this.getUserSettings());
					}
				} catch(Exception e) {
					new ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e0) {}
				}
			}
		}
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

   	/**
   	 * Cancel action.
   	 * 
   	 * @throws ServiceException
   	 */
   	public void doCancel(
	) throws ServiceException {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

   	/**
   	 * Get settings of current user.
   	 * 
   	 * @return
   	 */
   	public Properties getUserSettings(
   	) {
   		return this.userSettings;
   	}

	/**
	 * @return the selectPerspectiveActions
	 */
	public Action[] getSelectPerspectiveActions() {
		return selectPerspectiveActions;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
   	private Properties userSettings;
   	private Action[] selectPerspectiveActions;
   	
}
