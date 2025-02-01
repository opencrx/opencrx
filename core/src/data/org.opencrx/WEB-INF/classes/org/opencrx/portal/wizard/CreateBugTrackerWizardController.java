/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateBugTrackerWizardController
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;

/**
 * CreateBugTrackerWizardController
 *
 */
@SessionScoped
@Named
public class CreateBugTrackerWizardController extends JsfWizardController implements Serializable {

	private static final long serialVersionUID = -1267444308570463904L;

	/**
	 * Constructor.
	 * 
	 */
	public CreateBugTrackerWizardController(
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
	 * Create bug tracker.
	 * 
	 * @param activitySegment
	 * @param name
	 * @param description
	 * @return
	 * @throws ServiceException
	 */
	public static ActivityTracker createBugTracker(
		org.opencrx.kernel.activity1.jmi1.Segment activitySegment,
		String name,
		String description
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(activitySegment);
		String providerName = activitySegment.refGetPath().getSegment(2).toString();
		String segmentName = activitySegment.refGetPath().getSegment(4).toString();
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
    	    Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{activityTracker}),
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
    	    Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{activityTracker}),
    	    allUsers
    	);
    	// Update tracker
        pm.currentTransaction().begin();
    	activityTracker.setDescription(description);
    	activityTracker.setDefaultCreator(defaultCreator);
    	activityTracker.setActivityGroupType(Activities.ActivityGroupType.BUG_TRACKER.getValue());    	
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
		jakarta.faces.event.AjaxBehaviorEvent event				
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		Map<String,Object> data = this.getData();
		List<String> errors = new ArrayList<String>();
		String providerName = this.getProviderName();
		String segmentName = this.getSegmentName();
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
		    	ActivityTracker activityTracker = createBugTracker(
		    		activitySegment,
		    		name,
		    		description
		    	);
		    	// Forward to tracker
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
