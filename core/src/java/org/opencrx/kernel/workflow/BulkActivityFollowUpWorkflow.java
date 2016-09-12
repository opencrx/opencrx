/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BulkActivityFollowUpWorkflow
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.workflow;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.utils.WorkflowHelper;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.UserObjects;
import org.openmdx.kernel.log.SysLog;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * BulkActivityFollowUpWorkflow
 *
 */
public class BulkActivityFollowUpWorkflow extends Workflows.AsynchronousWorkflow {
    
	/**
	 * Create log entry.
	 * 
	 * @param name
	 * @param wfProcessInstance
	 * @param numberOfActivities
	 * @param countSuccess
	 * @param countFailed
	 * @throws ServiceException
	 */
	protected void createLogEntry(
		String name,
		WfProcessInstance wfProcessInstance,
		int numberOfActivities,
		int countSuccess,
		int countFailed
	) throws ServiceException {
	    Map<String,Object> report = new HashMap<String,Object>();
	    report.put(
	    	"Total",
	    	Integer.toString(numberOfActivities)
	    );
	    report.put(
	    	"Success",
	    	Integer.toString(countSuccess)
	    );
	    report.put(
	    	"Failed",
	    	Integer.toString(countFailed)
	    );
	    report.put(
	    	"Pending",
	    	Integer.toString(numberOfActivities - countSuccess - countFailed)
	    );
        WorkflowHelper.createLogEntry(
            wfProcessInstance,
            name,
            report.toString()
        );
	}

    /* (non-Javadoc)
     * @see org.opencrx.kernel.backend.Workflows.AsynchronousWorkflow#execute(org.opencrx.kernel.home1.jmi1.WfProcessInstance)
     */
    @Override
    public void execute(
        WfProcessInstance wfProcessInstance
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
        try {
    	    PersistenceManager pmUser = pm.getPersistenceManagerFactory().getPersistenceManager(
    	    	wfProcessInstance.refGetPath().get(6),
    	    	null
    	    );
    	    UserObjects.setBulkLoad(pmUser, true);
            Map<String,Object> params = WorkflowHelper.getWorkflowParameters(
            	(WfProcessInstance)pmUser.getObjectById(wfProcessInstance.refGetPath())
            );
            Activity activity = null;
            if(params.get(OPTION_ACTIVITY) instanceof Activity) {
           		activity = (Activity)pmUser.getObjectById(
           			((Activity)params.get(OPTION_ACTIVITY)).refGetPath()
           		);
            }
            if(activity == null) {
            	WorkflowHelper.createLogEntry(
            		wfProcessInstance, 
            		OPTION_ACTIVITY,
            		"Option >" + OPTION_ACTIVITY + "< must be a ReferenceProperty and reference an Activity"
            	);
            }
            ActivityGroup activityGroup = null;
            if(wfProcessInstance.getTargetObject() != null) {
            	try {
            		activityGroup = (ActivityGroup)pmUser.getObjectById(
            			new Path(wfProcessInstance.getTargetObject())
            		);
            	} catch(Exception e) {}
            }
            if(activityGroup == null) {
            	WorkflowHelper.createLogEntry(
            		wfProcessInstance, 
            		"Target", 
            		"Target must be of type ActivityGroup"
            	);
            }			
            List<ActivityProcessTransition> processTransitions = new ArrayList<ActivityProcessTransition>();
            List<String> followUpTitles = new ArrayList<String>();
    	    List<String> followUpTexts = new ArrayList<String>();
            if(params.get(OPTION_TRANSITION) instanceof ActivityProcessTransition) {
            	processTransitions.add(
            		(ActivityProcessTransition)pmUser.getObjectById(
            			((ActivityProcessTransition)params.get(OPTION_TRANSITION)).refGetPath()
            		)
           		);
            }
    	    if(params.get(OPTION_FOLLOWUP_TITLE) instanceof String) {
    	    	followUpTitles.add((String)params.get(OPTION_FOLLOWUP_TITLE));
    	    }
    	    if(params.get(OPTION_FOLLOWUP_TEXT) instanceof String) {
    	    	followUpTexts.add((String)params.get(OPTION_FOLLOWUP_TEXT));
    	    }
            int idx = 0;
            while(params.get(OPTION_TRANSITION + idx) instanceof ActivityProcessTransition) {
            	processTransitions.add(
            		(ActivityProcessTransition)pmUser.getObjectById(
            			((ActivityProcessTransition)params.get(OPTION_TRANSITION + idx)).refGetPath()
            		)
           		);
    	    	followUpTitles.add((String)params.get(OPTION_FOLLOWUP_TITLE + idx));
    	    	followUpTexts.add((String)params.get(OPTION_FOLLOWUP_TEXT + idx));
    	    	idx++;
            }
    	    Contact assignTo = null;
    	    if(params.get(OPTION_ASSIGN_TO) instanceof Contact) {
    	    	assignTo = (Contact)pmUser.getObjectById(
    	    		((Contact)params.get(OPTION_ASSIGN_TO)).refGetPath()
    	    	);
    	    }
    	    // Create follow ups
    		List<Path> activityIdentities = new ArrayList<Path>();
    	    int countSuccess = 0;
    	    int countFailed = 0;
    		ActivityProcessState processState = activity == null ? null : activity.getProcessState();
    	    if(activityGroup != null && processState != null && !processTransitions.isEmpty()) {
	    		// Get identity of activities to be updated
	    		ActivityQuery activityQuery = (ActivityQuery)pmUser.newQuery(Activity.class);
	    		activityQuery.thereExistsProcessState().equalTo(processState);
	    		((Query)activityQuery).getFetchPlan().setFetchSize(FETCH_SIZE);
    			Collection<Activity> activities = activityGroup.getFilteredActivity(activityQuery);
    			int count = 0;
    			for(Activity a: activities) {
    				activityIdentities.add(a.refGetPath());
    				count++;
    				if(count % 1000 == 0) {
    	    			this.createLogEntry(
    	    				"Report @" + new Date(),
    	    				wfProcessInstance, 
    	    				activityIdentities.size(), 
    	    				countSuccess, 
    	    				countFailed
    	    			);    					
    				}
    			}
    			this.createLogEntry(
    				"Report @" + new Date(),
    				wfProcessInstance, 
    				activityIdentities.size(), 
    				countSuccess, 
    				countFailed
    			);
    			WfProcessInstance parentProcessInstance = (WfProcessInstance)pmUser.getObjectById(wfProcessInstance.refGetPath());
    			for(Path activityIdentity: activityIdentities) {
    	    		try {
						Activity a = (Activity)pmUser.getObjectById(activityIdentity);
    	    			for(int i = 0; i < processTransitions.size();i++) {
							ActivityDoFollowUpParams doFollowUpParams = Structures.create(
								ActivityDoFollowUpParams.class, 
								Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, assignTo),
								Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, followUpTexts.get(i)),
								Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, followUpTitles.get(i)),
								Datatypes.member(ActivityDoFollowUpParams.Member.transition, processTransitions.get(i)),
								Datatypes.member(ActivityDoFollowUpParams.Member.parentProcessInstance, parentProcessInstance)
							);
							pmUser.currentTransaction().begin();
							a.doFollowUp(doFollowUpParams);
							pmUser.currentTransaction().commit();
    	    			}
						countSuccess++;
						if(assignTo != null) {
							pmUser.currentTransaction().begin();
    	          			a.setAssignedTo(assignTo);
    	          			pmUser.currentTransaction().commit();
						}
    	    		} catch(Exception e) {
    	    			countFailed++;
	    				ServiceException e0 = new ServiceException(e);
	    				SysLog.detail(e0.getMessage(), e0.getCause());
    	    			try {
    	    				pmUser.currentTransaction().rollback();
    	    			} catch(Exception e1) {}
    	    		}
    	    		if((countSuccess + countFailed) % 100 == 0) {
    	    			this.createLogEntry(
    	    				"Report @" + new Date(),
    	    				wfProcessInstance, 
    	    				activityIdentities.size(), 
    	    				countSuccess, 
    	    				countFailed
    	    			);
    	    		}
    	    	}
            }
    	    this.createLogEntry(
    	    	"Report - Complete",
    	    	wfProcessInstance,
    	    	activityIdentities.size(),
    	    	countSuccess,
    	    	countFailed    	
    	    );
    	    pmUser.close();
        } catch(Exception e) {
        	SysLog.warning("Can not perform BulkActivityFollowUp (reason=Exception)", e.getMessage());
            ServiceException e0 = new ServiceException(e);
            SysLog.detail(e0.getMessage(), e0.getCause());
            WorkflowHelper.createLogEntry(
                wfProcessInstance,
                "Can not perform BulkActivityFollowUp: Exception",
                e.getMessage()
            );
            throw e0;
        }
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final int FETCH_SIZE = 200;
    
    public static final String OPTION_ACTIVITY = "activity";
    public static final String OPTION_TRANSITION = "transition";
    public static final String OPTION_FOLLOWUP_TITLE = "followUpTitle";
    public static final String OPTION_FOLLOWUP_TEXT = "followUpText";
    public static final String OPTION_ASSIGN_TO = "assignTo";

}

//--- End of File -----------------------------------------------------------
