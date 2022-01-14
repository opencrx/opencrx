/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateAgendaWizardController
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.generic.SecurityKeys;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JsfWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * CreateAgendaWizardController
 *
 */
@SuppressWarnings("deprecation")
@ManagedBean
@SessionScoped
public class CreateAgendaWizardController extends JsfWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateAgendaWizardController(
   	) {
   		super();
   	}
	
	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.JsfWizardController#newData()
	 */
	@Override
	public Map<String, Object> newData(
	) throws ServiceException {
		return new HashMap<String,Object>();
	}

	/**
	 * Create agenda.
	 * 
	 * @param activitySegment
	 * @param name
	 * @param description
	 * @return
	 * @throws ServiceException
	 */
	public static ActivityTracker createAgenda(
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
    	// ActivityCreator Meeting
    	org.opencrx.kernel.activity1.jmi1.ActivityCreator defaultCreator = Activities.getInstance().initActivityCreator(
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
    	    Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// ActivityCreator SalesVisit
    	Activities.getInstance().initActivityCreator(
    	    name + " - " + Activities.ACTIVITY_CREATOR_NAME_SALES_VISITS,
	    	Activities.getInstance().initActivityType(
	    	    Activities.getInstance().findActivityProcess(
	    	        Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING,
	    	        activitySegment
	    	    ),
	    	    Activities.ACTIVITY_TYPE_NAME_SALES_VISITS,
	    	    Activities.ActivityClass.SALES_VISIT.getValue(),
	    	    null, // owningGroups
	    	    SecurityKeys.ACCESS_LEVEL_NA
	    	),
    	    Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// ActivityCreator Absence
    	Activities.getInstance().initActivityCreator(
    	    name + " - " + Activities.ACTIVITY_CREATOR_NAME_ABSENCES,
	    	Activities.getInstance().initActivityType(
	    	    Activities.getInstance().findActivityProcess(
	    	        Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING,
	    	        activitySegment
	    	    ),
	    	    Activities.ACTIVITY_TYPE_NAME_ABSENCES,
	    	    Activities.ActivityClass.ABSENCE.getValue(),
	    	    null, // owningGroups
	    	    SecurityKeys.ACCESS_LEVEL_NA
	    	),
    	    Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{activityTracker}),
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
    	    Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// Update tracker
        pm.currentTransaction().begin();
    	activityTracker.setDescription(description);
    	activityTracker.setDefaultCreator(defaultCreator);
    	activityTracker.setActivityGroupType(Activities.ActivityGroupType.AGENDA.getValue());
    	pm.currentTransaction().commit();
    	return activityTracker;
	}
	
	/**
	 * doCreate action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCreate(
		javax.faces.event.AjaxBehaviorEvent event		
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		Map<String,Object> data = this.getData();
		String providerName = this.getProviderName();
		String segmentName = this.getSegmentName();
		List<String> errors = new ArrayList<String>();
		org.opencrx.kernel.activity1.jmi1.Segment activitySegment =
		    (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
		        new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName, "segment", segmentName)
			);		
	    String name = (String)data.get("name");
	    String description = (String)data.get("description");
    	if(name == null || name.isEmpty()) {
    		errors.add("The field 'Name' is mandatory");
    	}
	    if(name != null && !name.isEmpty()) {
	   		try {	    	
		    	ActivityTracker activityTracker = createAgenda(
		    		activitySegment,
		    		name,
		    		description
		    	);
		    	Action exitAction = new ObjectReference(
		    		activityTracker,
		    		app
		    	).getSelectObjectAction();
	   			ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
	   			externalContext.redirect(
	   				externalContext.getRequestContextPath() + "/" + exitAction.getEncodedHRef()
	   			);
	   		} catch(Exception e) {
	   			throw new ServiceException(e);
	   		}
	    }
	    data.put("errors", errors);
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	

}
