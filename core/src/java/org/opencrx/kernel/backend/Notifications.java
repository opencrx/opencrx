/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Notifications
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
package org.opencrx.kernel.backend;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.cci2.ActivityFollowUpQuery;
import org.opencrx.kernel.activity1.cci2.MeetingPartyQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.Incident;
import org.opencrx.kernel.activity1.jmi1.Meeting;
import org.opencrx.kernel.activity1.jmi1.MeetingParty;
import org.opencrx.kernel.backend.Activities.PartyStatus;
import org.opencrx.kernel.home1.jmi1.Alert;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.application.dataprovider.cci.DataproviderOperations;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.HtmlEncoder;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.CssClass;
import org.openmdx.portal.servlet.action.SelectObjectAction;

/**
 * Notifications
 *
 */
public class Notifications extends AbstractImpl {

	/**
	 * Register Notifications with backend.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new Notifications());
	}

	/**
	 * Retrieve registered Notifications.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Notifications getInstance(
	) throws ServiceException {
		return getInstance(Notifications.class);
	}

	/**
	 * Notifications backend.
	 * 
	 */
	protected Notifications(
	) {
		
	}

	/**
	 * Get access URL.
	 * 
	 * @param targetIdentity
	 * @param userHome
	 * @return
	 * @throws ServiceException
	 */
	public String getAccessUrl(
		Path targetIdentity,
		UserHome userHome
	) throws ServiceException {
        String webAccessUrl = UserHomes.getInstance().getWebAccessUrl(userHome);
        Action selectTargetAction = targetIdentity == null 
        	? null 
        	: new Action(
                SelectObjectAction.EVENT_ID, 
                new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, targetIdentity.toXRI())
                },
                "",
                true
            );
        return 
	        webAccessUrl + 
	        "?event=" + SelectObjectAction.EVENT_ID + 
	        "&parameter=" + selectTargetAction.getParameter();		
	}

    /**
     * Get notification text.
     * 
     * @param pm
     * @param target
     * @param wfProcessInstanceIdentity
     * @param userHome
     * @param params
     * @return
     * @throws ServiceException
     */
    public String getNotificationText(
        PersistenceManager pm,
        ContextCapable target,
        Path wfProcessInstanceIdentity,
        UserHome userHome,
        Map<String,Object> params
    ) throws ServiceException {
        String text = "#ERR";
        String webAccessUrl = UserHomes.getInstance().getWebAccessUrl(userHome);
        Path targetIdentity = target == null 
        	? null 
        	: target.refGetPath();
        Action selectTargetAction = targetIdentity == null 
        	? null 
        	: new Action(
                SelectObjectAction.EVENT_ID, 
                new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, targetIdentity.toXRI())
                },
                "",
                true
            );
        Path triggeredBy = params.get("triggeredBy") instanceof String 
        	? new Path((String)params.get("triggeredBy")) 
        	: params.get("triggeredBy") instanceof Path 
        		? (Path)params.get("triggeredBy") 
        		: null;
        String triggeredById = triggeredBy == null ? "N/A" : triggeredBy.getLastSegment().toClassicRepresentation(); 
        Action selectWfProcessInstanceAction = wfProcessInstanceIdentity == null 
        	? null 
        	: new Action(
                SelectObjectAction.EVENT_ID, 
                new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, wfProcessInstanceIdentity.toXRI())    
                },
                "",
                true
            );
        Action selectTriggeredByAction =  triggeredBy == null 
        	? null 
        	: new Action(
                SelectObjectAction.EVENT_ID,
                new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, triggeredBy.toXRI())
                },
                "",
                true
            );
        // Alert specific text
        if(target instanceof Alert) {
            Alert alert = (Alert)target;
            ContextCapable referencedObj = null;
            try {
            	referencedObj = alert.getReference();
            } catch(Exception ignore) {}
            if(
                (referencedObj != null) && 
                !(referencedObj instanceof UserHome)
            ) {
                text = this.getNotificationText(
                    pm,
                    referencedObj,
                    wfProcessInstanceIdentity,
                    userHome,
                    params
               );
            } else {
                text = "<!DOCTYPE html>";
    	        text += "<html lang=\"en\">";
    	        text += "<head>";
    	        text += "	<meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">";
    	        text += "	<link rel=\"stylesheet\" href=\"http://getbootstrap.com/dist/css/bootstrap.min.css\">";
    	        text += "</head>";
    	        text += "<body>";
    	        text += "<div class=\"container\">";
                if(selectTargetAction != null) {
                	text += "<div class=\"alert alert-warning\" style=\"margin-bottom:0px\">";
                	text += "<h4><a href=\""+ this.getAccessUrl(targetIdentity, userHome) + "\">" + (alert.getName() == null ? "Alert" : alert.getName()) + "</a></h4>";
                    text += "</div>";
        	        text += "<pre style=\"" + PRE_STYLE + "\">";
                } else {
        	        text += "<pre style=\"" + PRE_STYLE + "\">";
	                text += "Name:\n";
	                text += (alert.getName() == null ? "---" : alert.getName()) + "\n\n";
                }
                text += "Description:\n";
                text += (alert.getDescription() == null ? "---" : alert.getDescription()) + "\n\n"; 
                text += "</pre>";
                text += "</div>";
                text += "</body>";
                text += "</html>";                
            }
        } else if(
            (target instanceof Activity) ||
            (target instanceof ActivityFollowUp)
        ) {
            // Activity specific text
            Activity activity = target instanceof Activity 
            	? (Activity)target 
            	: (Activity)pm.getObjectById(new Path(target.refMofId()).getParent().getParent());
            Contact reportingContact = null;
            try {
            	reportingContact = activity.getReportingContact();
            } catch(Exception ignore) {}
            Account reportingAccount = null;
            try {
            	reportingAccount = activity.getReportingAccount();
            } catch(Exception ignore) {}
            Contact assignedTo = null;
            try {
            	assignedTo = activity.getAssignedTo();                
            } catch(Exception ignore) {}
            ActivityProcessState activityState = activity.getProcessState();                
            ActivityProcessTransition lastTransition = activity.getLastTransition();                
            String activityName = activity.getName();
            text = "<!DOCTYPE html>";
	        text += "<html lang=\"en\">";
	        text +=     "<head>";
	        text += "	<meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">";
	        text += "	<link rel=\"stylesheet\" href=\"http://getbootstrap.com/dist/css/bootstrap.min.css\">";
	        text += "</head>";
	        text += "<body>";
	        text += "<div class=\"container\">";
            // Activity details
            {
            	text += "<div class=\"" + CssClass.alert + " " + CssClass.alertWarning + "\" style=\"margin-bottom:0px\">";
            	text += "<h4><a href=\"" + webAccessUrl + "?event=" + SelectObjectAction.EVENT_ID + "&parameter=" + selectTargetAction.getParameter() + "\">#" + activity.getActivityNumber() + ": " +  HtmlEncoder.encode(activityName == null ? "---" : activityName, false) + "</a></h4>";
                text += "</div>";
            	text += "<pre style=\"" + PRE_STYLE + "\">";
                text += "=========================================================\n";
                text += "Reporting Contact:          " + (reportingContact == null ? "---" : reportingContact.getFullName()) + "\n";
                text += "Reporting Account:          " + (reportingAccount == null ? "---" : reportingAccount.getFullName()) + "\n";
                text += "Handler:                    " + (assignedTo == null ? "---" : assignedTo.getFullName()) + "\n";
                text += "=========================================================\n";
                int ii = 0;
                Collection<ActivityGroupAssignment> assignedGroups = activity.getAssignedGroup();
                for(ActivityGroupAssignment assignedGroup: assignedGroups) {
                    ActivityGroup group = assignedGroup.getActivityGroup();
                    if(group != null) {
                        if(ii == 0) {
                            text += "Activity Group:             " + group.getName() + "\n";
                        } else {
                            text += "                            " + group.getName() + "\n";                        
                        }
                    }
                }
                text += "Activity#:                  " + activity.getActivityNumber() + "\n";
                if(activity instanceof Incident) {
                    Incident incident = (org.opencrx.kernel.activity1.jmi1.Incident)activity;
                    try {
                        text += "Category:                   " + incident.getCategory() + "\n";
                    } catch(Exception ignore) {}
                    try {
                        text += "Reproducibility:            " + incident.getReproducibility() + "\n";
                    } catch(Exception e) {}
                    try {
                        text += "Severity:                   " + incident.getSeverity() + "\n";
                    } catch(Exception ignore) {}
                }
                text += "Priority:                   " + activity.getPriority() + "\n";
                text += "Status:                     " + (activityState == null ? "---" : activityState.getName()) + "\n";
                text += "Last transition:            " + (lastTransition == null ? "---" : lastTransition.getName()) + "\n";
                text += "=========================================================\n";
                text += "Scheduled start:            " + (activity.getScheduledStart() == null ? "---" : activity.getScheduledStart()) + "\n";
                text += "Scheduled end:              " + (activity.getScheduledEnd() == null ? "---" : activity.getScheduledEnd()) + "\n";
                text += "Due by:                     " + (activity.getDueBy() == null ? "---" : activity.getDueBy()) + "\n";
                text += "Actual start:               " + (activity.getActualStart() == null ? "---" : activity.getActualStart()) + "\n";
                text += "Actual end:                 " + (activity.getActualEnd() == null ? "---" : activity.getActualEnd()) + "\n";
                text += "=========================================================\n";
                text += "Date Submitted:             " + activity.getCreatedAt() + "\n";
                text += "Last Modified:              " + activity.getModifiedAt() + "\n";
                text += "=========================================================\n";
                // Meeting
                if(activity instanceof Meeting) {
                	text += "Organizer:\n";
                	text += "*  " + activity.getAssignedTo().getFullName() + "\n";
                	text += "Attendees:\n";
                	Meeting meeting = (Meeting)activity;
                	MeetingPartyQuery partyQuery = (MeetingPartyQuery)pm.newQuery(MeetingParty.class);
                	partyQuery.orderByPartyStatus().ascending();
                	List<MeetingParty> meetingParties = meeting.getMeetingParty(partyQuery);            	
                	for(MeetingParty meetingParty: meetingParties) {
                		String partyStatus =  meetingParty.getPartyStatus() == PartyStatus.ACCEPTED.getValue() 
                			? "+" 
                			: meetingParty.getPartyStatus() == PartyStatus.DECLINED.getValue() 
                				? "-" 
                				: "?";            		
                		text += partyStatus + "  " + (meetingParty.getEmailHint() == null 
                			? meetingParty.getParty().getFullName() 
                			: meetingParty.getEmailHint()) + "\n"; 
                	}
                    text += "=========================================================\n";            		
                }
                // Summary, Description, Message Body
                String activityDescription = activity.getDescription();
                String activityDetailedDescription = activity.getDetailedDescription();
                String messageBody = activity instanceof org.opencrx.kernel.activity1.jmi1.EMail ? 
                	((org.opencrx.kernel.activity1.jmi1.EMail)activity).getMessageBody() : 
                	null;
                text += "Summary:\n";
                text += (activityName == null ? "---" : activityName) + "\n\n";
                text += "Description:\n";
                text += (activityDescription == null ? "---" : activityDescription) + "\n\n"; 
                text += "Details:\n";
                text += (activityDetailedDescription == null ? "---" : activityDetailedDescription) + "\n\n";
                if(messageBody != null) {
                    text += "Message Body:\n";
                    text += messageBody + "\n\n";                     
                }
                text += "</pre>";
            }
            // Follow-Ups
            {
	            // Follow Ups
	            ActivityFollowUpQuery followUpQuery = (ActivityFollowUpQuery)pm.newQuery(ActivityFollowUp.class);
	            followUpQuery.orderByCreatedAt().descending();
	            Collection<ActivityFollowUp> followUps = activity.getFollowUp(followUpQuery);
	            for(ActivityFollowUp followUp: followUps) {
	                Contact followUpAssignedTo = null;
	                try {
	                	followUpAssignedTo = followUp.getAssignedTo();
	                } catch(Exception ignore) {}
	                ActivityProcessTransition followUpTransition = null;
	                try {
	                	followUpTransition = followUp.getTransition();
	                } catch(Exception ignore) {}
	                text += "<div class=\"" + CssClass.alert + " " + CssClass.alertInfo + "\" style=\"padding:10px;margin-bottom:0px\">";
	                if(followUp.getTitle() != null && !followUp.getTitle().isEmpty()) {
		                text += "<div class=\"" + CssClass.row + "\">";
		                text += "	<div class=\"" + CssClass.colXs12 + "\"><h4>" + HtmlEncoder.encode(followUp.getTitle(), false) + "</h4></div>";
		                text += "</div>";
	                }
	                text += "<div class=\"row\">";
	                text += "	<div class=\"" + CssClass.colXs3 + " text-left\">" + (followUpTransition == null ? "---" : followUpTransition.getName()) + "</div>";
	                text += "	<div class=\"" + CssClass.colXs4 + " text-right\">" + (followUpAssignedTo == null ? "---" : followUpAssignedTo.getFullName()) + "</div>";
	                text += "	<div class=\"" + CssClass.colXs5 + " text-right\">" + followUp.getCreatedAt() + "</div>";
	                text += "</div>";
	                text += "</div>";
	                String followUpText = followUp.getText();
	                if(
	                	followUpText != null &&
	                	!HtmlEncoder.containsWiki(followUpText) &&
	                	HtmlEncoder.containsHtml(followUpText)
	                ) {
	                	text += "<div class=\"" + CssClass.panel + " panel-default\">";
	                	text += "<div class=\"panel-body\">";
	                	text += followUpText;
	                	text += "</div>";
	                	text += "</div>";
	                } else {
                		text += "<pre style=\"" + PRE_STYLE + "\">";
                		text += followUpText == null || followUpText.isEmpty() 
                			? "---" 
                			: HtmlEncoder.encode(followUpText, false);
                		text += "</pre>";
                	}
	            }
            }
            text += "</div>";
            text += "</body>";
            text += "</html>";
        } else {
            // Generic text
            text = "<!DOCTYPE html>";
	        text += "<html lang=\"en\">";
	        text +=     "<head>";
	        text += "	<meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">";
	        text += "	<link rel=\"stylesheet\" href=\"http://getbootstrap.com/dist/css/bootstrap.min.css\">";
	        text += "</head>";
	        text += "<body>";
	        text += "<div class=\"container\">";
	        text += "<pre style=\"" + PRE_STYLE + "\">";
            text += "=========================================================\n";
            text += "Event:           " + (params.get("triggeredByEventType") == null ? "N/A" : DataproviderOperations.toString(((Number)params.get("triggeredByEventType")).intValue())) + "\n";
            text += "Trigger Id:      " + triggeredById + "\n";
            if(selectTargetAction != null) {
                text += "Object Invoked:  " + ("\"" + webAccessUrl + "?event=" + SelectObjectAction.EVENT_ID + "&parameter=" + selectTargetAction.getParameter() + "\"\n");
            }
            text += "Workflow:        " + (selectWfProcessInstanceAction == null ? "N/A" : "\"" + webAccessUrl + "?event=" +  + SelectObjectAction.EVENT_ID + "&parameter=" + selectWfProcessInstanceAction.getParameter()) + "\"\n";
            text += "Trigger URL:     " + (selectTriggeredByAction == null ? "N/A" : "\"" + webAccessUrl + "?event=" +  + SelectObjectAction.EVENT_ID + "&parameter=" + selectTriggeredByAction.getParameter()) + "\"\n";
            text += "=========================================================\n";
            text += "</pre>";
            text += "</div>";
            text += "</body>";
            text += "</html>";
        }
        return text;
    }

    /**
     * Get notification subject.
     * 
     * @param pm
     * @param target
     * @param userHome
     * @param params
     * @param useSendMailSubjectPrefix
     * @return
     * @throws ServiceException
     */
    public String getNotificationSubject(
        PersistenceManager pm,
        ContextCapable target,
        Path wfProcessInstanceIdentity,
        UserHome userHome,  
        Map<String,Object> params,
        boolean useSendMailSubjectPrefix
    ) throws ServiceException {
        Path userHomeIdentity = userHome.refGetPath();
        String providerName = userHomeIdentity.getSegment(2).toClassicRepresentation();
        String segmentName = userHomeIdentity.getSegment(4).toClassicRepresentation();
        String title = null;
        Path triggeredBy = params.get("triggeredBy") instanceof String 
        	? new Path((String)params.get("triggeredBy")) 
        	: params.get("triggeredBy") instanceof Path 
        		? (Path)params.get("triggeredBy") 
        		: null;
        String triggeredById = triggeredBy == null ? "N/A" : triggeredBy.getLastSegment().toClassicRepresentation();
        String sendMailSubjectPrefix = userHome.getSendMailSubjectPrefix() == null 
        	? "[" + providerName + ":" + segmentName + "]" 
        	: userHome.getSendMailSubjectPrefix();
        String webAccessUrl = UserHomes.getInstance().getWebAccessUrl(userHome);
        if(
            (target != null) && 
            !Boolean.TRUE.equals(params.get("confidential"))
        ) {
            try {
                if(target instanceof EMail) {
                    EMail emailActivity = (EMail)target;
                    title = (useSendMailSubjectPrefix ? sendMailSubjectPrefix + ": " : "") + emailActivity.getMessageSubject();
                } else if(target instanceof Alert) {
                    Alert alert = (Alert)target;
                    if(alert.getName() != null && !alert.getName().isEmpty()) {
                    	title = (useSendMailSubjectPrefix ? sendMailSubjectPrefix + ": " : "") + alert.getName();
                    } else if(
                        (alert.getReference() != null) &&
                        !(alert.getReference() instanceof UserHome)
                    ) {
                        title = this.getNotificationSubject(
                            pm, 
                            alert.getReference(),
                            wfProcessInstanceIdentity,
                            userHome, 
                            params,
                            useSendMailSubjectPrefix
                        );
                    } else {
                        title = alert.refGetPath().toXRI();
                    }
                } else {
                    if(title == null) {
                        try {
                            if(target.refGetValue("name") != null) {
                                title = sendMailSubjectPrefix + ": " + target.refGetValue("name");
                            }
                        } catch(Exception ignore) {}
                    }
                    if(title == null) {
                        try {
                            if(target.refGetValue("title") != null) {
                                title = sendMailSubjectPrefix + ": " + target.refGetValue("title");
                            }
                        } catch(Exception ignore) {}
                    }
                    if(title == null) {
                        try {
                            if(target.refGetValue("fullName") != null) {
                                title = sendMailSubjectPrefix + ": " + target.refGetValue("fullName");
                            }
                        } catch(Exception ignore) {}
                    }
                }
            } catch(Exception e) {}
        }
        if(title == null) {
            title = 
                sendMailSubjectPrefix + ": " + 
                "from=" + providerName + "/" + segmentName + "/" + userHomeIdentity.getSegment(6).toClassicRepresentation() + "; " + 
                "trigger=" + triggeredById + "; " +
                "access=" + webAccessUrl;                
        }
        return title;
    }

    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final short CHANGE_PASSWORD_OK = 0;
    public static final short MISSING_NEW_PASSWORD = 1;
    public static final short MISSING_NEW_PASSWORD_VERIFICATION = 2;
    public static final short PASSWORD_VERIFICATION_MISMATCH = 3;
    public static final short CAN_NOT_RETRIEVE_REQUESTED_PRINCIPAL = 4;
    public static final short CAN_NOT_CHANGE_PASSWORD = 5;
    public static final short MISSING_OLD_PASSWORD = 6;

    protected static final String PRE_STYLE = "white-space:pre-wrap; word-break:normal;";

}

//--- End of File -----------------------------------------------------------
