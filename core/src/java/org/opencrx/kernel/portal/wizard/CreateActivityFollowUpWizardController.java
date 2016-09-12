/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateActivityFollowUpWizardController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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

import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery;
import org.opencrx.kernel.activity1.cci2.SubActivityTransitionQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.opencrx.kernel.activity1.jmi1.SubActivityTransition;
import org.opencrx.kernel.generic.OpenCrxException;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * CreateActivityFollowUpWizardController
 *
 */
public class CreateActivityFollowUpWizardController extends org.openmdx.portal.servlet.AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 * @param formName
	 */
	public CreateActivityFollowUpWizardController(
   	) {
   		super();
   	}

	/**
	 * Get next transitions of activity.
	 * 
	 * @param activity
	 * @param onlySubActivityTransitions
	 * @param orderPercentCompleteIncreasing
	 * @param pm
	 * @return
	 */
	public List<ActivityProcessTransition> getNextTransitionsOfActivity(
		org.opencrx.kernel.activity1.jmi1.Activity activity,
		boolean onlySubActivityTransitions,
		boolean orderPercentCompleteIncreasing,
		javax.jdo.PersistenceManager pm
	) {	
		List<ActivityProcessTransition> transitions = new ArrayList<ActivityProcessTransition>();
		if (activity == null) {
			return transitions;
		}
		org.opencrx.kernel.activity1.jmi1.ActivityProcess activityProcess = null;
		try {
			activityProcess = activity.getActivityType().getControlledBy();
		} catch (Exception e) {
			System.out.println("activity #" + activity.getActivityNumber() + " with control process error");
			new ServiceException(e).log();
		}
		org.opencrx.kernel.activity1.jmi1.ActivityProcessState processState = null;
		try {
			processState = activity.getProcessState();
		} catch (Exception e) {
			System.out.println("activity #" + activity.getActivityNumber() + " with process state error");
			new ServiceException(e).log();
		}
		if (processState != null) {
			if (onlySubActivityTransitions) {
				SubActivityTransitionQuery subActivityTransitionQuery = (SubActivityTransitionQuery)pm.newQuery(SubActivityTransition.class);
				subActivityTransitionQuery.thereExistsPrevState().equalTo(processState);
				if (orderPercentCompleteIncreasing) {
					subActivityTransitionQuery.orderByNewPercentComplete().ascending();
				} else {
					subActivityTransitionQuery.orderByNewPercentComplete().descending();
				}
				subActivityTransitionQuery.orderByName().ascending();
				for(ActivityProcessTransition t: activityProcess.getTransition(subActivityTransitionQuery)) {
					transitions.add(t);
				}
			} else {
				ActivityProcessTransitionQuery activityProcessTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
				activityProcessTransitionQuery.thereExistsPrevState().equalTo(processState);
				if (orderPercentCompleteIncreasing) {
					activityProcessTransitionQuery.orderByNewPercentComplete().ascending();
				} else {
					activityProcessTransitionQuery.orderByNewPercentComplete().descending();
				}
				activityProcessTransitionQuery.orderByName().ascending();
				for(ActivityProcessTransition t: activityProcess.getTransition(activityProcessTransitionQuery)) {
					transitions.add(t);
				}
			}
		}
		return transitions;
	}
	
	/**
	 * OK action.
	 * 
	 * @param isInitialized
	 * @param assignToXri
	 * @param reassignActivity
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
		@RequestParameter(name = "isInitialized") Boolean isInitialized,
		@RequestParameter(name = "assignToXri") String assignToXri,
		@RequestParameter(name = "reassignActivity") Boolean reassignActivity,
		@FormParameter(forms = "doFollowUpForm") Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = getPm();
		ApplicationContext app = getApp();
		RefObject_1_0 obj = getObject();
		this.doRefresh(
			isInitialized, 
			assignToXri,
			reassignActivity,
			formFields
		);
	    // doFollowUp
	    org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition transition =
        	(org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)pm.getObjectById(
        		formFields.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:transition")
        	);
	    String followUpTitle = (String)formFields.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:followUpTitle");
	    String followUpText = (String)formFields.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:followUpText");
		Contact assignTo = formFields.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:assignTo") != null ?
	    	(org.opencrx.kernel.account1.jmi1.Contact)pm.getObjectById(
	    		formFields.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:assignTo")
	    	) : null;
        // updateActivity
	    String description = (String)formFields.get("org:opencrx:kernel:activity1:Activity:description");
	    String location = (String)formFields.get("org:opencrx:kernel:activity1:Activity:location");
	    Short priority = (Short)formFields.get("org:opencrx:kernel:activity1:Activity:priority");
	    Date dueBy = (Date)formFields.get("org:opencrx:kernel:activity1:Activity:dueBy");
	    if(transition != null && this.getObject() instanceof Activity) {
	    	try {
		    	Activity activity = (Activity)this.getObject();
				ActivityDoFollowUpParams params = Structures.create(
					ActivityDoFollowUpParams.class,
					Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, assignTo),
					Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, followUpText),
					Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, followUpTitle),
					Datatypes.member(ActivityDoFollowUpParams.Member.transition, transition)
				);
				pm.refresh(activity);
				pm.currentTransaction().begin();
				activity.doFollowUp(params);
				activity.setAssignedTo(this.assignedTo);
				activity.setDescription(description);
				activity.setLocation(location);
				activity.setPriority(priority);
				activity.setDueBy(dueBy);
				pm.currentTransaction().commit();
				this.setExitAction(
					new ObjectReference(obj, app).getSelectObjectAction()
				);
	    	} catch(Exception e) {
	    		try {
	    			pm.currentTransaction().rollback();
	    		} catch(Exception ignore) {}
	    		ServiceException e0 = new ServiceException(e);
	    		e0.log();
	    		org.openmdx.kernel.exception.BasicException b = e0.getCause(OpenCrxException.DOMAIN);
	    		this.errorMessage = b == null ? e0.getMessage() : b.getDescription();
	    	}
	    }
	}

	/**
	 * Refresh action.
	 * 
	 * @param isInitialized
	 * @param assignToXri
	 * @param reassignActivity
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doRefresh(
		@RequestParameter(name = "isInitialized") Boolean isInitialized,
		@RequestParameter(name = "assignToXri") String assignToXri,
		@RequestParameter(name = "reassignActivity") Boolean reassignActivity,
		@FormParameter(forms = "doFollowUpForm") Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.formFields = formFields;
		if(!Boolean.TRUE.equals(isInitialized) && this.getObject() instanceof Activity) {
			Activity activity = (Activity)this.getObject();
			this.nextTransitions = this.getNextTransitionsOfActivity(activity, false, true, pm);
			this.formFields.put("org:opencrx:kernel:activity1:Activity:assignedTo", activity.getAssignedTo() == null ? null : activity.getAssignedTo().refGetPath());
			this.formFields.put("org:opencrx:kernel:activity1:Activity:description", activity.getDescription());
			this.formFields.put("org:opencrx:kernel:activity1:Activity:location", activity.getLocation());
			this.formFields.put("org:opencrx:kernel:activity1:Activity:priority", activity.getPriority());
			this.formFields.put("org:opencrx:kernel:activity1:Activity:dueBy", activity.getDueBy());			
		}
		if(assignToXri != null) {
			Contact resourceContact = null;
			try {
				resourceContact = (Contact)pm.getObjectById(new Path(assignToXri));
		 	} catch (Exception e) {}
		 	if(resourceContact != null && Boolean.TRUE.equals(reassignActivity)) {
				formFields.put("org:opencrx:kernel:activity1:Activity:assignedTo", resourceContact.refGetPath());
		 	}
		}
 	    this.assignedTo = null;
 	    try {
 	    	this.assignedTo = formFields.get("org:opencrx:kernel:activity1:Activity:assignedTo") != null ?
	 	    	(org.opencrx.kernel.account1.jmi1.Contact)pm.getObjectById(
	 	    		formFields.get("org:opencrx:kernel:activity1:Activity:assignedTo")
	 	    	) : null;
 	    } catch (Exception e) {}
	}

	/**
	 * Cancel action.
	 * 
	 * @throws ServiceException
	 */
	public void doCancel(
	) throws ServiceException {
		this.setExitAction(
			new ObjectReference(getObject(), getApp()).getSelectObjectAction()
		);
	}

	/**
	 * Get form values.
	 * 
	 * @return
	 */
	public Map<String,Object> getFormFields(
	) {
		return this.formFields;
	}

	/**
	 * @return the nextTransitions
	 */
	public List<ActivityProcessTransition> getNextTransitions(
	) {
		return this.nextTransitions;
	}

	/**
	 * @return the assignedTo
	 */
	public Contact getAssignedTo(
	) {
		return this.assignedTo;
	}

	/**
	 * Get view port.
	 * 
	 * @param out
	 * @return
	 */
	public ViewPort getViewPort(
		Writer out
	) {
		if(this.viewPort == null) {
			TransientObjectView view = new TransientObjectView(
				this.getFormFields(),
				this.getApp(),
				this.getObject(),
				this.getPm()
			);
			this.viewPort = ViewPortFactory.openPage(
				view,
				this.getRequest(),
				out
			);			
		}
		return this.viewPort;
	}

	/**
	 * @return the errorMessage
	 */
	public String getErrorMessage() {
		return errorMessage;
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#close()
	 */
    @Override
    public void close(
    ) throws ServiceException {
	    super.close();
	    if(this.viewPort != null) {
	    	this.viewPort.close(false);
	    }
    }

	//-------------------------------------------------------------------
	// Members
	//-------------------------------------------------------------------
	public static final String RESOURCE_CLASS = "org:opencrx:kernel:activity1:Resource";

	private Map<String,Object> formFields;
	private List<ActivityProcessTransition> nextTransitions;
	private Contact assignedTo;
	private String errorMessage = null;
	private ViewPort viewPort;

}
