/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MergeChangesWizardController
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

import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams;
import org.opencrx.kernel.activity1.jmi1.ActivityProcess;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.opencrx.kernel.backend.Base;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Codes;
import org.openmdx.portal.servlet.ObjectReference;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * MergeChangesWizardController
 *
 */
public class ManageChangeRequestsWizardController extends AbstractWizardController {

	/**
	 * Format
	 *
	 */
	public enum Format {
		SUMMARY,
		CHANGE_REQUEST,
		EXISTING_OBJECT,
		MERGED_OBJECT
	}

	/**
	 * Constructor.
	 * 
	 */
	public ManageChangeRequestsWizardController(
	) {
		super();
	}


    /**
     * Get code mapper used for object title generation.
     * 
     * @param codes
     * @return
     */
    protected Base.CodeMapper getCodeMapper(
    	final Codes codes
    ) {
		return new Base.CodeMapper() {
			
			@Override
			public String getLocaleText(short locale) {
				return (String)codes.getShortText("locale", (short)0, true, true).get(locale);
			}
			@Override
			public String getCurrencyText(short currency, short locale) {
				return (String)codes.getLongText("currency", locale, true, true).get(currency);
			}
			@Override
			public String getCountryText(short country, short locale) {
				return (String)codes.getLongText("org:opencrx:kernel:address1:PostalAddressable:postalCountry", locale, true, true).get(country);
			}
		};	
    }

	/**
	 * Render change request in given format.
	 *
	 * @param activity
	 * @param format
	 * @param out
	 */
	public void renderChangeRequest(
		Activity activity,
		Format format,
		Writer out
	) throws ServiceException, IOException {

	}

	/**
	 * Get the pending change requests.
	 * 
	 * @return
	 */
	public List<Activity> getChangeRequests(
	) {
		return Collections.emptyList();
	}

	/**
	 * Apply changes to changes object and set activity status to complete.
	 * 
	 */
	protected void applyChanges(
	) throws ServiceException {
		
	}

	protected String getFollowUpTitle(
		Activity activity,
		ActivityProcessTransition processTransition
	) throws ServiceException {
		return this.getWizardName() + " [" + processTransition.getName() + "]";
	}

	/**
	 * Reject changes and set activity status to cancelled.
	 * 
	 */
	protected void performFollowUp(
		Activity activity,
		ActivityProcessTransition processTransition
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
		try {
			pm.currentTransaction().begin();
			ActivityDoFollowUpParams doFollowUpParams = Structures.create(
				ActivityDoFollowUpParams.class, 
				Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, null),
				Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, null),
				Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, this.getFollowUpTitle(activity, processTransition)),
				Datatypes.member(ActivityDoFollowUpParams.Member.transition, processTransition)
			);
			activity.doFollowUp(doFollowUpParams);
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception ignore) {}
		}
	}

	/**
	 * Refresh action.
	 * 
	 * @param currentChangeRequestXri
	 * @param selectedChangeRequestXri
	 */
	public void doRefresh(
		@RequestParameter(name = "currentChangeRequestXri") String currentChangeRequestXri,
		@RequestParameter(name = "selectedChangeRequestXri") String selectedChangeRequestXri
	) {
		PersistenceManager pm = this.getPm();
		Path currentChangeRequestIdentity = currentChangeRequestXri == null || currentChangeRequestXri.isEmpty()
			? null
			: new Path(currentChangeRequestXri);
		Path selectedChangeRequestIdentity = selectedChangeRequestXri == null || selectedChangeRequestXri.isEmpty()
			? null
			: new Path(selectedChangeRequestXri);
		if(selectedChangeRequestIdentity != null) {
			this.selectedChangeRequest = (Activity)pm.getObjectById(selectedChangeRequestIdentity);
		} else if(currentChangeRequestIdentity != null) {
			this.selectedChangeRequest = (Activity)pm.getObjectById(currentChangeRequestIdentity);
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
	 * Apply action.
	 * 
	 * @param currentActivityXri
	 * @param selectedActivityXri
	 */
	public void doApply(
		@RequestParameter(name = "currentChangeRequestXri") String currentChangeRequestXri,
		@RequestParameter(name = "selectedChangeRequestXri") String selectedChangeRequestXri
	) throws ServiceException {
		this.doRefresh(
			currentChangeRequestXri, 
			selectedChangeRequestXri
		);
		if(this.selectedChangeRequest != null) {
			this.applyChanges();
		}		
	}

	/**
	 * RejectChanges action.
	 * 
	 * @param currentChangeRequestXri
	 * @param selectedChangeRequestXri
	 * @param processTransitionXri
	 */
	public void doDoFollowUp(
		@RequestParameter(name = "currentChangeRequestXri") String currentChangeRequestXri,
		@RequestParameter(name = "selectedChangeRequestXri") String selectedChangeRequestXri,
		@RequestParameter(name = "processTransitionXri") String processTransitionXri
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			currentChangeRequestXri, 
			selectedChangeRequestXri
		);
		if(this.selectedChangeRequest != null && processTransitionXri != null) {
			ActivityProcessTransition processTransition = null;
			try {
				processTransition = (ActivityProcessTransition)pm.getObjectById(new Path(processTransitionXri));
			} catch(Exception ignore) {}
			if(processTransition != null) {
				this.performFollowUp(
					this.selectedChangeRequest,
					processTransition
				);
				Map<String,String[]> parameterMap = new HashMap<String,String[]>(this.getRequest().getParameterMap());
				parameterMap.remove(PARAMETER_CURRENT_CHANGE_REQUEST_XRI);
				parameterMap.remove(PARAMETER_SELECTED_CHANGE_REQUEST_XRI);
				this.selectedChangeRequest = null;
				this.forward(
					"Refresh", 
					parameterMap
				);
			}
		}
	}

	/**
	 * Get transition to forward process state.
	 * 
	 * @param activity
	 * @return
	 */
	public List<ActivityProcessTransition> getNextTransitions(
		Activity activity
	) {
		PersistenceManager pm = this.getPm();
		if(activity != null && activity.getProcessState() != null) {
			ActivityProcessState processState = activity.getProcessState();
			ActivityProcess activityProcess = (ActivityProcess)pm.getObjectById(processState.refGetPath().getParent().getParent());
			ActivityProcessTransitionQuery processTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
			processTransitionQuery.thereExistsPrevState().equalTo(processState);
			processTransitionQuery.thereExistsNextState().notEqualTo(processState);
			processTransitionQuery.orderByNewPercentComplete().descending();
			return activityProcess.getTransition(processTransitionQuery);
		}
		return Collections.emptyList();
	}

	/**
	 * @return the selectedActivity
	 */
	public Activity getSelectedChangeRequest() {
		return selectedChangeRequest;
	}

	/**
	 * Get color depending on format to be rendered.
	 * 
	 * @param format
	 * @return
	 */
	public String getColor(
		Format format
	) {
		switch(format) {
			case CHANGE_REQUEST: return "#FFCC00";
			case EXISTING_OBJECT: return "#CECEF6";
			case MERGED_OBJECT: return "#9FF781";
			default: return "none";
		}
	}

	/**
	 * @return the postRenderChangeRequestScript
	 */
	public StringBuilder getPostRenderChangeRequestScript() {
		return this.postRenderChangeRequestScript;
	}

	/**
	 * Reset postRenderChangeRequestScript.
	 * 
	 */
	public void resetPostRenderChangeRequestScript(
	) {
		this.postRenderChangeRequestScript.setLength(0);
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String PARAMETER_CURRENT_CHANGE_REQUEST_XRI = "currentChangeRequestXri";
	public static final String PARAMETER_SELECTED_CHANGE_REQUEST_XRI = "selectedChangeRequestXri";
	public static final String PARAMETER_PROCESS_TRANSITION_XRI = "processTransitionXri";

	protected Activity selectedChangeRequest;
	protected StringBuilder postRenderChangeRequestScript = new StringBuilder();

}
