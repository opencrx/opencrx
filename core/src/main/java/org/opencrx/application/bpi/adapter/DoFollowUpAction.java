/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateActivityAction
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
package org.opencrx.application.bpi.adapter;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import javax.jdo.PersistenceManager;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.opencrx.application.bpi.datatype.BpiDoFollowUpParams;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult;
import org.opencrx.kernel.activity1.jmi1.ActivityProcess;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * DoFollowUpAction
 *
 */
public class DoFollowUpAction extends BpiAction {

	/* (non-Javadoc)
	 * @see org.opencrx.application.bpi.adapter.BpiAdapterServlet.Action#handle(org.openmdx.base.naming.Path, javax.jdo.PersistenceManager, jakarta.servlet.http.HttpServletRequest, jakarta.servlet.http.HttpServletResponse)
	 */
	@Override
    public void perform(
    	Path path, PersistenceManager pm,
    	BpiPlugIn plugIn,
    	HttpServletRequest req, 
    	HttpServletResponse resp
    ) throws IOException, ServiceException {
    	List<Activity> activities = plugIn.findActivities(path.getPrefix(7), pm);
    	if(activities == null || activities.isEmpty()) {
    		resp.setStatus(HttpServletResponse.SC_NOT_FOUND);    		
    	} else {
    		Activity activity = activities.iterator().next();
    		BpiDoFollowUpParams bpiDoFollowUpParams = plugIn.parseObject(
    			req.getReader(),
    			BpiDoFollowUpParams.class
    		);
    		ActivityProcessTransition processTransition = null;
    		if(bpiDoFollowUpParams.getTransition() == null) {
    			if(activity.getProcessState() != null && activity.getActivityType() != null) {
					ActivityProcess activityProcess = activity.getActivityType().getControlledBy();
					ActivityProcessTransitionQuery processTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
					processTransitionQuery.thereExistsPrevState().equalTo(activity.getProcessState());
					// Make progress
					if(activity.getPercentComplete() != null) {
						processTransitionQuery.thereExistsNewPercentComplete().greaterThan(activity.getPercentComplete());
					}
					processTransitionQuery.orderByNewPercentComplete().ascending();
					processTransitionQuery.orderByNewActivityState().ascending();
					List<ActivityProcessTransition> processTransitions = activityProcess.getTransition(processTransitionQuery);
					processTransition = processTransitions.isEmpty()
						? null
						: processTransitions.iterator().next();
    			}
    		} else {
    			if(activity.getActivityType() != null) {
					ActivityProcess activityProcess = activity.getActivityType().getControlledBy();
					ActivityProcessTransitionQuery processTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
					processTransitionQuery.name().equalTo(bpiDoFollowUpParams.getTransition());
					List<ActivityProcessTransition> processTransitions = activityProcess.getTransition(processTransitionQuery);
					processTransition = processTransitions.isEmpty()
						? null
						: processTransitions.iterator().next();
    			}
    		}
    		if(processTransition == null) {
        		resp.setStatus(HttpServletResponse.SC_NOT_FOUND);    			
    		} else {
	    		Contact assignTo = null;
	    		try {
	    			String contactId = bpiDoFollowUpParams.getAssignTo();
	    			if(contactId != null) {
	    				List<Contact> contacts = plugIn.findContacts(path.getPrefix(5).getDescendant("contact", contactId), pm);
	    				assignTo = contacts.isEmpty() ? null : contacts.iterator().next();
	    			}
	    		} catch(Exception ignore) {}
	    		try {
	    			String followUpText = bpiDoFollowUpParams.getText();
	    			// The activity's detailedDescription and assignedTo is updated in case updateActivity is true.
	    			// The existing detailedDescription is then merged with the followUpText.
	    			if(Boolean.TRUE.equals(bpiDoFollowUpParams.getUpdateActivity())) {
	    				pm.currentTransaction().begin();
	    				activity.setDetailedDescription(
	    					plugIn.mergeActivityDetailedDescription(
		    					activity.getDetailedDescription(),
		    					bpiDoFollowUpParams.getText()
		    				)
		    			);
	    				activity.setAssignedTo(assignTo);
	    				pm.currentTransaction().commit();
	    			}
		    		pm.currentTransaction().begin();
					ActivityDoFollowUpParams doFollowUpParams = Structures.create(
						ActivityDoFollowUpParams.class, 
						Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, assignTo),
						Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, followUpText),
						Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, bpiDoFollowUpParams.getTitle()),
						Datatypes.member(ActivityDoFollowUpParams.Member.transition, processTransition)
					);
					ActivityDoFollowUpResult result = activity.doFollowUp(doFollowUpParams);
		    		pm.currentTransaction().commit();
		    		if(result.getFollowUp() != null) {
		        		resp.setCharacterEncoding("UTF-8");
		        		resp.setContentType("application/json");
		        		PrintWriter pw = resp.getWriter();
		        		plugIn.printObject(
		        			pw, 
		        			plugIn.toBpiActivityFollowUp(
		        				result.getFollowUp(), 
		        				plugIn.newBpiActivityFollowUp(),
		        				this.getFetchGroup(req)
		        			)
		        		);
		        		resp.setStatus(HttpServletResponse.SC_OK);
		    		}
	    		} catch(Exception e) {
	        		resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);	    			
	    			new ServiceException(e).log();
	    			try {
	    				pm.currentTransaction().rollback();
	    			} catch(Exception ignore) {}
	    		}
    		}
    	}
    }

}
