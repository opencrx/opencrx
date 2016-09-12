/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateProjectWizardController
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
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.generic.SecurityKeys;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;

/**
 * CreateProjectWizardController
 *
 */
public class CreateProjectWizardController extends org.openmdx.portal.servlet.AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateProjectWizardController(
   	) {
   		super();
   	}
	
	/**
	 * Create project.
	 * 
	 * @param activitySegment
	 * @param name
	 * @param description
	 * @return
	 * @throws ServiceException
	 */
	public static ActivityTracker createProject(
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment,
    	String name,
    	String description
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(activitySegment);
		String providerName = activitySegment.refGetPath().get(2);
		String segmentName = activitySegment.refGetPath().get(4);
		org.opencrx.security.realm1.jmi1.PrincipalGroup usersPrincipalGroup =
			(org.opencrx.security.realm1.jmi1.PrincipalGroup)org.opencrx.kernel.backend.SecureObject.getInstance().findPrincipal(
				"Users",
				org.opencrx.kernel.backend.SecureObject.getInstance().getRealm(
					pm,
					providerName,
					segmentName
				)
			);
		org.opencrx.security.realm1.jmi1.PrincipalGroup administratorsPrincipalGroup =
			(org.opencrx.security.realm1.jmi1.PrincipalGroup)org.opencrx.kernel.backend.SecureObject.getInstance().findPrincipal(
				"Administrators",
				org.opencrx.kernel.backend.SecureObject.getInstance().getRealm(
					pm,
					providerName,
					segmentName
				)
			);
		List<org.opencrx.security.realm1.jmi1.PrincipalGroup> allUsers = new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
		allUsers.add(usersPrincipalGroup);
		allUsers.add(administratorsPrincipalGroup);
        org.opencrx.kernel.activity1.jmi1.ActivityTracker activityTracker = Activities.getInstance().initActivityTracker(
            name,
            allUsers,
            activitySegment
        );
    	// ActivityCreator Incident
    	org.opencrx.kernel.activity1.jmi1.ActivityCreator defaultCreator = Activities.getInstance().initActivityCreator(
    	    name + " - " + Activities.ACTIVITY_CREATOR_NAME_INCIDENTS,
	    	Activities.getInstance().initActivityType(
	    	    Activities.getInstance().findActivityProcess(
	    	        Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING,
	    	        activitySegment
	    	    ),
	    	    Activities.ACTIVITY_CREATOR_NAME_INCIDENTS,
	    	    Activities.ActivityClass.INCIDENT.getValue(),
	    	    null, // owningGroups
	    	    SecurityKeys.ACCESS_LEVEL_NA		    	    
	    	),
    	    Arrays.asList(new ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// ActivityCreator Meeting
    	Activities.getInstance().initActivityCreator(
    	    name + " - " + Activities.ACTIVITY_CREATOR_NAME_MEETINGS,
	    	Activities.getInstance().initActivityType(
	    	    Activities.getInstance().findActivityProcess(
	    	        Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING,
	    	        activitySegment
	    	    ),
	    	    Activities.ACTIVITY_TYPE_NAME_MEETINGS,
	    	    Activities.ActivityClass.MEETING.getValue(),
	    	    null, // owningGroups
	    	    SecurityKeys.ACCESS_LEVEL_NA		    	    
	    	),
    	    Arrays.asList(new ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// ActivityCreator Task
    	Activities.getInstance().initActivityCreator(
    	    name + " - " + Activities.ACTIVITY_CREATOR_NAME_TASKS,
	    	Activities.getInstance().initActivityType(
	    	    Activities.getInstance().findActivityProcess(
	    	        Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING,
	    	        activitySegment
	    	    ),
	    	    Activities.ACTIVITY_TYPE_NAME_TASKS,
	    	    Activities.ActivityClass.TASK.getValue(),
	    	    null, // owningGroups
	    	    SecurityKeys.ACCESS_LEVEL_NA		    	    		    	    
	    	),
    	    Arrays.asList(new ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// ActivityCreator E-Mail
    	Activities.getInstance().initActivityCreator(
    	    name + " - " + Activities.ACTIVITY_CREATOR_NAME_EMAILS,
	    	Activities.getInstance().initActivityType(
	    	    Activities.getInstance().findActivityProcess(
	    	        Activities.ACTIVITY_PROCESS_NAME_EMAILS,
	    	        activitySegment
	    	    ),
	    	    Activities.ACTIVITY_TYPE_NAME_EMAILS,
	    	    Activities.ActivityClass.EMAIL.getValue(),
	    	    null, // owningGroups
	    	    SecurityKeys.ACCESS_LEVEL_NA		    	    
	    	),
    	    Arrays.asList(new ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// Update tracker
        pm.currentTransaction().begin();
    	activityTracker.setDescription(description);
    	activityTracker.setDefaultCreator(defaultCreator);
    	activityTracker.setActivityGroupType(Activities.ActivityGroupType.PROJECT.getValue());    	
    	pm.currentTransaction().commit();
    	return activityTracker;
	}

	/**
	 * OK action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
		@FormParameter(forms = "CreateActivityTrackerForm") Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.formFields = formFields;
		String providerName = this.getProviderName();
		String segmentName = this.getSegmentName();
	    String name = (String)this.formFields.get("org:opencrx:kernel:activity1:ActivityGroup:name");
	    String description = (String)this.formFields.get("org:opencrx:kernel:activity1:ActivityGroup:description");
    	if(name == null || name.isEmpty()) {
    		this.errorMessage += "The field 'Name' is mandatory";
    	}
	    if(name != null && !name.isEmpty()) {
	    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, providerName, segmentName);
	    	ActivityTracker activityTracker = createProject(
	    		activitySegment, 
	    		name, 
	    		description
	    	);
	    	this.setExitAction(
	    		new ObjectReference(activityTracker, app).getSelectObjectAction()
	    	);
	    }		
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
   	 * Get form values.
   	 * 
   	 * @return
   	 */
   	public Map<String,Object> getFormFields(
   	) {
   		return this.formFields;
   	}
   	
	/**
	 * Get viewPort.
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

    //-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	private Map<String,Object> formFields;
	private ViewPort viewPort;

}
