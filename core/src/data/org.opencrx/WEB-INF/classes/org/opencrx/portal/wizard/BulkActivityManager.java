/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BulkActivityManager
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

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.activity1.cci2.ActivityProcessStateQuery;
import org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityProcess;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.PropertySet;
import org.opencrx.kernel.home1.cci2.TimerQuery;
import org.opencrx.kernel.home1.cci2.WfActionLogEntryQuery;
import org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery;
import org.opencrx.kernel.home1.jmi1.Timer;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.home1.jmi1.WfActionLogEntry;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.portal.DateTimePropertyDataBinding;
import org.opencrx.kernel.portal.IntegerPropertyDataBinding;
import org.opencrx.kernel.portal.StringPropertyDataBinding;
import org.opencrx.kernel.utils.PropertiesHelper;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.kernel.utils.WorkflowHelper;
import org.opencrx.kernel.workflow.BulkActivityFollowUpWorkflow;
import org.opencrx.kernel.workflow.BulkCreateActivityWorkflow;
import org.opencrx.kernel.workflow.BulkCreateActivityWorkflow.CreationType;
import org.opencrx.kernel.workflow1.jmi1.WfProcess;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JsfWizardController;

import jakarta.annotation.ManagedBean;
import jakarta.enterprise.context.SessionScoped;

/**
 * BulkActivityManager
 *
 */
@SuppressWarnings("deprecation")
@ManagedBean
@SessionScoped
public class BulkActivityManager extends JsfWizardController {

	/**
	 * ActivityFollowUpBean
	 *
	 */
	public static class ActivityFollowUpBean {
		/**
		 * @return the title
		 */
		public String getTitle() {
			return title;
		}
		/**
		 * @param title the title to set
		 */
		public void setTitle(String title) {
			this.title = title;
		}
		/**
		 * @return the text
		 */
		public String getText() {
			return text;
		}
		/**
		 * @param text the text to set
		 */
		public void setText(String text) {
			this.text = text;
		}
		/**
		 * @return the transition
		 */
		public JsfWizardController.ObjectReferenceBean getTransition() {
			return transition;
		}
		/**
		 * @param transition the transition to set
		 */
		public void setTransition(JsfWizardController.ObjectReferenceBean transition) {
			this.transition = transition;
		}
		private JsfWizardController.ObjectReferenceBean transition = new JsfWizardController.ObjectReferenceBean();
		private String title;
		private String text;
	}

	/**
	 * TimerBean
	 *
	 */
	public static class TimerBean {
		/**
		 * @return the name
		 */
		public String getName() {
			return name;
		}
		/**
		 * @param name the name to set
		 */
		public void setName(String name) {
			this.name = name;
		}
		/**
		 * @return the triggerAt
		 */
		public Date getTriggerAt() {
			return triggerAt;
		}
		/**
		 * @param triggerAt the triggerAt to set
		 */
		public void setTriggerAt(Date triggerAt) {
			this.triggerAt = triggerAt;
		}
		/**
		 * @return the timer
		 */
		public JsfWizardController.ObjectReferenceBean getTimer() {
			return timer;
		}
		/**
		 * @param timer the timer to set
		 */
		public void setTimer(JsfWizardController.ObjectReferenceBean timer) {
			this.timer = timer;
		}
		private JsfWizardController.ObjectReferenceBean timer;
		private String name;
		private Date triggerAt;
	}

	/**
	 * WfProcessInstanceBean
	 *
	 */
	public static class WfProcessInstanceBean {
		/**
		 * @return the wfProcessInstance
		 */
		public JsfWizardController.ObjectReferenceBean getWfProcessInstance() {
			return wfProcessInstance;
		}
		/**
		 * @param wfProcessInstance the wfProcessInstance to set
		 */
		public void setWfProcessInstance(JsfWizardController.ObjectReferenceBean wfProcessInstance) {
			this.wfProcessInstance = wfProcessInstance;
		}
		/**
		 * @return the processState
		 */
		public String getProcessState() {
			return processState;
		}
		/**
		 * @param processState the processState to set
		 */
		public void setProcessState(String processState) {
			this.processState = processState;
		}
		/**
		 * @return the lastActivityOn
		 */
		public Date getLastActivityOn() {
			return lastActivityOn;
		}
		/**
		 * @param lastActivityOn the lastActivityOn to set
		 */
		public void setLastActivityOn(Date lastActivityOn) {
			this.lastActivityOn = lastActivityOn;
		}
		/**
		 * @return the startedOn
		 */
		public Date getStartedOn() {
			return startedOn;
		}
		/**
		 * @param startedOn the startedOn to set
		 */
		public void setStartedOn(Date startedOn) {
			this.startedOn = startedOn;
		}
		/**
		 * @return the isFailed
		 */
		public Boolean getIsFailed() {
			return isFailed;
		}
		/**
		 * @param isFailed the isFailed to set
		 */
		public void setIsFailed(Boolean isFailed) {
			this.isFailed = isFailed;
		}
		/**
		 * @return the logEntries
		 */
		public List<String> getLogEntries() {
			return logEntries;
		}
		/**
		 * @param logEntries the logEntries to set
		 */
		public void setLogEntries(List<String> logEntries) {
			this.logEntries = logEntries;
		}
		/**
		 * @return the isCompleted
		 */
		public Boolean getIsCompleted() {
			return isCompleted;
		}
		/**
		 * @param isCompleted the isCompleted to set
		 */
		public void setIsCompleted(Boolean isCompleted) {
			this.isCompleted = isCompleted;
		}
		/**
		 * @return the createdAt
		 */
		public Date getCreatedAt() {
			return createdAt;
		}
		/**
		 * @param createdAt the createdAt to set
		 */
		public void setCreatedAt(Date createdAt) {
			this.createdAt = createdAt;
		}
		private JsfWizardController.ObjectReferenceBean wfProcessInstance;
		private String processState;
		private Date lastActivityOn;
		private Date startedOn;
		private Date createdAt;
		private Boolean isFailed;
		private Boolean isCompleted;
		private List<String> logEntries;
	}

	/**
	 * ActivityTemplateBean
	 *
	 */
	public static class ActivityTemplateBean {
		/**
		 * @return the excludeNoBulkEMail
		 */
		public Boolean getExcludeNoBulkEMail() {
			return excludeNoBulkEMail;
		}
		/**
		 * @param excludeNoBulkEMail the excludeNoBulkEMail to set
		 */
		public void setExcludeNoBulkEMail(Boolean excludeNoBulkEMail) {
			this.excludeNoBulkEMail = excludeNoBulkEMail;
		}
		/**
		 * @return the isEMail
		 */
		public Boolean getIsEMail() {
			return isEMail;
		}
		/**
		 * @param isEMail the isEMail to set
		 */
		public void setIsEMail(Boolean isEMail) {
			this.isEMail = isEMail;
		}
		/**
		 * @return the locale
		 */
		public Short getLocale() {
			return locale;
		}
		/**
		 * @param locale the locale to set
		 */
		public void setLocale(Short locale) {
			this.locale = locale;
		}
		/**
		 * @return the name
		 */
		public String getName() {
			return name;
		}
		/**
		 * @param name the name to set
		 */
		public void setName(String name) {
			this.name = name;
		}
		/**
		 * @return the description
		 */
		public String getDescription() {
			return description;
		}
		/**
		 * @param description the description to set
		 */
		public void setDescription(String description) {
			this.description = description;
		}
		/**
		 * @return the detailedDescription
		 */
		public String getDetailedDescription() {
			return detailedDescription;
		}
		/**
		 * @param detailedDescription the detailedDescription to set
		 */
		public void setDetailedDescription(String detailedDescription) {
			this.detailedDescription = detailedDescription;
		}
		/**
		 * @return the scheduledStart
		 */
		public Date getScheduledStart() {
			return scheduledStart;
		}
		/**
		 * @param scheduledStart the scheduledStart to set
		 */
		public void setScheduledStart(Date scheduledStart) {
			this.scheduledStart = scheduledStart;
		}
		/**
		 * @return the scheduledEnd
		 */
		public Date getScheduledEnd() {
			return scheduledEnd;
		}
		/**
		 * @param scheduledEnd the scheduledEnd to set
		 */
		public void setScheduledEnd(Date scheduledEnd) {
			this.scheduledEnd = scheduledEnd;
		}
		/**
		 * @return the dueBy
		 */
		public Date getDueBy() {
			return dueBy;
		}
		/**
		 * @param dueBy the dueBy to set
		 */
		public void setDueBy(Date dueBy) {
			this.dueBy = dueBy;
		}
		/**
		 * @return the priority
		 */
		public Short getPriority() {
			return priority;
		}
		/**
		 * @param priority the priority to set
		 */
		public void setPriority(Short priority) {
			this.priority = priority;
		}
		/**
		 * @return the placeHolders
		 */
		public String getPlaceHolders() {
			return placeHolders;
		}
		/**
		 * @param placeHolders the placeHolders to set
		 */
		public void setPlaceHolders(String placeHolders) {
			this.placeHolders = placeHolders;
		}
		/**
		 * @return the messageSubject
		 */
		public String getMessageSubject() {
			return messageSubject;
		}
		/**
		 * @param messageSubject the messageSubject to set
		 */
		public void setMessageSubject(String messageSubject) {
			this.messageSubject = messageSubject;
		}
		/**
		 * @return the messageBody
		 */
		public String getMessageBody() {
			return messageBody;
		}
		/**
		 * @param messageBody the messageBody to set
		 */
		public void setMessageBody(String messageBody) {
			this.messageBody = messageBody;
		}
		/**
		 * @return the senderEMail
		 */
		public JsfWizardController.ObjectReferenceBean getSenderEMail() {
			return senderEMail;
		}
		/**
		 * @param senderEMail the senderEMail to set
		 */
		public void setSenderEMail(JsfWizardController.ObjectReferenceBean senderEMail) {
			this.senderEMail = senderEMail;
		}
		public Short getRecipientEMailAddressUsage1() {
			return recipientEMailAddressUsage1;
		}
		/**
		 * @param recipientEMailAddressUsage1 the recipientEMailAddressUsage1 to set
		 */
		public void setRecipientEMailAddressUsage1(Short recipientEMailAddressUsage1) {
			this.recipientEMailAddressUsage1 = recipientEMailAddressUsage1;
		}
		/**
		 * @return the recipientEMailAddressUsage2
		 */
		public Short getRecipientEMailAddressUsage2() {
			return recipientEMailAddressUsage2;
		}
		/**
		 * @param recipientEMailAddressUsage2 the recipientEMailAddressUsage2 to set
		 */
		public void setRecipientEMailAddressUsage2(Short recipientEMailAddressUsage2) {
			this.recipientEMailAddressUsage2 = recipientEMailAddressUsage2;
		}
		/**
		 * @return the recipientEMailAddressUsage3
		 */
		public Short getRecipientEMailAddressUsage3() {
			return recipientEMailAddressUsage3;
		}
		/**
		 * @param recipientEMailAddressUsage3 the recipientEMailAddressUsage3 to set
		 */
		public void setRecipientEMailAddressUsage3(Short recipientEMailAddressUsage3) {
			this.recipientEMailAddressUsage3 = recipientEMailAddressUsage3;
		}
		/**
		 * @return the activityCreator
		 */
		public JsfWizardController.ObjectReferenceBean getActivityCreator() {
			return activityCreator;
		}
		/**
		 * @param activityCreator the activityCreator to set
		 */
		public void setActivityCreator(JsfWizardController.ObjectReferenceBean activityCreator) {
			this.activityCreator = activityCreator;
		}
		private JsfWizardController.ObjectReferenceBean activityCreator;
		private Boolean excludeNoBulkEMail;
		private Boolean isEMail;
		private Short locale;
		private String name;
		private String description;
		private String detailedDescription;
		private Date scheduledStart;
		private Date scheduledEnd;
		private Date dueBy;
		private Short priority;
		private String messageSubject;
		private String messageBody;
		private JsfWizardController.ObjectReferenceBean senderEMail;
		private Short recipientEMailAddressUsage1;
		private Short recipientEMailAddressUsage2;
		private Short recipientEMailAddressUsage3;
		private String placeHolders;
	}

	/**
	 * BulkCreateActivityBean
	 *
	 */
	public class BulkCreateActivityBean {
		
		/**
		 * BulkActivityFollowUpBean
		 *
		 */
		public class BulkActivityFollowUpBean {
			/**
			 * @return the activityProcessState
			 */
			public JsfWizardController.ObjectReferenceBean getActivityProcessState() {
				return activityProcessState;
			}
			/**
			 * @param activityProcessState the activityProcessState to set
			 */
			public void setActivityProcessState(JsfWizardController.ObjectReferenceBean activityProcessState) {
				this.activityProcessState = activityProcessState;
			}
			/**
			 * @return the counter
			 */
			public Integer getCounter() {
				return counter;
			}
			/**
			 * @param counter the counter to set
			 */
			public void setCounter(Integer counter) {
				this.counter = counter;
			}
			/**
			 * @return the hasFollowUpTransitions
			 */
			public Boolean getHasFollowUpTransitions() {
				return hasFollowUpTransitions;
			}
			/**
			 * @param hasFollowUpTransitions the hasFollowUpTransitions to set
			 */
			public void setHasFollowUpTransitions(Boolean hasFollowUpTransitions) {
				this.hasFollowUpTransitions = hasFollowUpTransitions;
			}			
			/**
			 * @return the doBulkActivityFollowUpParams
			 */
			public DoBulkActivityFollowUpParams getDoBulkActivityFollowUpParams() {
				return doBulkActivityFollowUpParams;
			}
			/**
			 * @param doBulkActivityFollowUpParams the doBulkActivityFollowUpParams to set
			 */
			public void setDoBulkActivityFollowUpParams(DoBulkActivityFollowUpParams doBulkActivityFollowUpParams) {
				this.doBulkActivityFollowUpParams = doBulkActivityFollowUpParams;
			}	
			/**
			 * @return the findAssignToResult
			 */
			public List<JsfWizardController.ObjectReferenceBean> getFindAssignToResult() {
				return findAssignToResult;
			}
			/**
			 * @param findAssignToResult the findAssignToResult to set
			 */
			public void setFindAssignToResult(List<JsfWizardController.ObjectReferenceBean> findAssignToResult) {
				this.findAssignToResult = findAssignToResult;
			}			
			/**
			 * @return the findFollowUp1TransitionResult
			 */
			public List<JsfWizardController.ObjectReferenceBean> getFindFollowUp1TransitionResult() {
				return findFollowUp1TransitionResult;
			}
			/**
			 * @param findFollowUp1TransitionResult the findFollowUp1TransitionResult to set
			 */
			public void setFindFollowUp1TransitionResult(List<JsfWizardController.ObjectReferenceBean> findFollowUp1TransitionResult) {
				this.findFollowUp1TransitionResult = findFollowUp1TransitionResult;
			}
			/**
			 * @return the findFollowUp2TransitionResult
			 */
			public List<JsfWizardController.ObjectReferenceBean> getFindFollowUp2TransitionResult() {
				return findFollowUp2TransitionResult;
			}
			/**
			 * @param findFollowUp2TransitionResult the findFollowUp2TransitionResult to set
			 */
			public void setFindFollowUp2TransitionResult(List<JsfWizardController.ObjectReferenceBean> findFollowUp2TransitionResult) {
				this.findFollowUp2TransitionResult = findFollowUp2TransitionResult;
			}
			
		   	/**
		   	 * Find assignTo contact.
		   	 * 
		   	 * @param event
		   	 * @throws ServiceException
		   	 */
		   	public void findAssignTo(
		   		jakarta.faces.event.AjaxBehaviorEvent event
		   	) throws ServiceException {
		   		Path path = BulkActivityManager.this.getObjectIdentity();
		   		JsfWizardController.QueryBean queryBean = new JsfWizardController.QueryBean();
		   		queryBean.setPosition(0);
		   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
		   		queryBean.setQuery(this.getDoBulkActivityFollowUpParams().getAssignTo().getTitle());
		   		this.setFindAssignToResult(
		   			BulkActivityManager.this.findContacts(
			   			path,
			   			queryBean
			   		)
		   		);
		   	}
		   	
		   	/**
		   	 * Find follow-up 1 transition.
		   	 * 
		   	 * @param event
		   	 * @throws ServiceException
		   	 */
		   	public void findFollowUp1Transition(
		   		jakarta.faces.event.AjaxBehaviorEvent event
		   	) throws ServiceException {
		   		Path path = new Path(this.getActivityProcessState().getXri());
		   		JsfWizardController.QueryBean queryBean = new JsfWizardController.QueryBean();
		   		queryBean.setPosition(0);
		   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
		   		queryBean.setQuery(this.getDoBulkActivityFollowUpParams().getFollowUp1().getTransition().getTitle());
		   		this.setFindFollowUp1TransitionResult(
		   			BulkActivityManager.this.findActivityProcessTransitions(
			   			path,
			   			queryBean
			   		)
		   		);
		   	}

		   	/**
		   	 * Find follow-up 1 transition.
		   	 * 
		   	 * @param event
		   	 * @throws ServiceException
		   	 */
		   	public void findFollowUp2Transition(
		   		jakarta.faces.event.AjaxBehaviorEvent event
		   	) throws ServiceException {
		   		Path path = new Path(this.getActivityProcessState().getXri());
		   		JsfWizardController.QueryBean queryBean = new JsfWizardController.QueryBean();
		   		queryBean.setPosition(0);
		   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);
		   		queryBean.setQuery(this.getDoBulkActivityFollowUpParams().getFollowUp2().getTransition().getTitle());
		   		this.setFindFollowUp2TransitionResult(
		   			BulkActivityManager.this.findActivityProcessTransitions(
			   			path,
			   			queryBean
			   		)
		   		);
		   	}

			/**
			 * Do bulk activity follow up.
			 * 
			 * @param path
			 * @param params
			 * @return
			 * @throws ServiceException
			 */
			public void doBulkActivityFollowUp(
				jakarta.faces.event.AjaxBehaviorEvent event
			) throws ServiceException {
				PersistenceManager pm = BulkActivityManager.this.getPm();
				ApplicationContext app = BulkActivityManager.this.getApp();
				Map<String,Object> data = BulkActivityManager.this.getData();
				Path path = BulkActivityManager.this.getObjectIdentity();
				DoBulkActivityFollowUpParams params = this.getDoBulkActivityFollowUpParams(); 
				ActivityGroup activityGroup = (ActivityGroup)pm.getObjectById(new Path(BulkCreateActivityBean.this.getActivityGroup().getXri()));
				ActivityProcessState activityProcessState = (ActivityProcessState)pm.getObjectById(new Path(this.getActivityProcessState().getXri()));
				ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
				activityQuery.thereExistsProcessState().equalTo(activityProcessState);
				activityQuery.forAllDisabled().isFalse();
				List<Activity> activities = activityGroup.getFilteredActivity(activityQuery);
				if(!activities.isEmpty()) {
					Activity activity = activities.iterator().next();
					if(params.getFollowUp1() != null) {
		    	    	try {
							List<ActivityFollowUpBean> activityFollowUpBeans = new ArrayList<ActivityFollowUpBean>();
							if(params.getFollowUp1() != null && params.getFollowUp1().getTransition() != null && params.getFollowUp1().getTransition().getXri() != null && !params.getFollowUp1().getTransition().getXri().isEmpty()) {
								activityFollowUpBeans.add(params.getFollowUp1());
							}
							if(params.getFollowUp2() != null && params.getFollowUp2().getTransition() != null && params.getFollowUp2().getTransition().getXri() != null && !params.getFollowUp2().getTransition().getXri().isEmpty()) {
								activityFollowUpBeans.add(params.getFollowUp2());
							}
		    	    		List<String> transitionNames = new ArrayList<String>();
		    	    		for(ActivityFollowUpBean activityFollowUpBean: activityFollowUpBeans) {
		    	    			transitionNames.add(activityFollowUpBean.getTransition().getTitle());
							}
					    	UserHome userHome = (UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());		  
							org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = BulkActivityManager.this.getWorkflowSegment(path);
							WfProcess wfProcess = Workflows.getInstance().findWfProcess(org.opencrx.kernel.backend.Workflows.WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP, workflowSegment);
			  				pm.currentTransaction().begin();
			  				BasicObject executionTarget = null;
			  				TimerBean timerBean = params.getTimer();
			    	    	// Create a timer
							if(
								timerBean != null &&
								timerBean.getTriggerAt() != null
							) {
								Timer timer = pm.newInstance(Timer.class);
								timer.setName(
									(timerBean.getName() == null || timerBean.getName().isEmpty() ? "" : timerBean.getName() + ": ") +
									activityGroup.getName() + " / " + transitionNames + " / " + userHome.refGetPath().getLastSegment().toString() 
								);
								timer.setTimerStartAt(timerBean.getTriggerAt());
								try {
									if (wfProcess != null) {
										timer.getAction().add(wfProcess);
									}
								} catch (Exception e) {
									new ServiceException(e).log();
								}
								timer.setTriggerRepeat(1);
								timer.setTriggerIntervalMinutes(5); /* note that this value MUST be bigger than the ping interval of the subscription handler */
								timer.setDisabled(false);
								timer.setTimerState((short)10); // open
								timer.setTimerEndAt(new Date(timerBean.getTriggerAt().getTime() + 3600000L)); // + 60 minutes
								timer.setTarget(activityGroup);
								userHome.addTimer(
									Utils.getUidAsString(),
									timer
								);
								executionTarget = timer;
							} else {
								// Create workflow instance
								executionTarget = Workflows.getInstance().executeWorkflow(
									activityGroup.getName() + " / " + transitionNames + " / " + userHome.refGetPath().getLastSegment().toString(),
									userHome,
									wfProcess,
									activityGroup,
									null, // booleanParams
									null, // stringParams
									null, // integerParams 
									null, // decimalParams
									null, // dateTimeParams
									null, // uriParams
									null // parentProcessInstance
								);
							}
							// Set BulkActivityFollowUpWorkflow parameters
							if(executionTarget instanceof WfProcessInstance) {
								if(activity != null) {
									WorkflowHelper.addParameter(
										(WfProcessInstance)executionTarget, 
										BulkActivityFollowUpWorkflow.OPTION_ACTIVITY, 
										activity
									);
								}
								if(params.getAssignTo() != null && params.getAssignTo().getXri() != null && !params.getAssignTo().getXri().isEmpty()) {
									Contact assignTo = (Contact)pm.getObjectById(new Path(params.getAssignTo().getXri()));
									WorkflowHelper.addParameter(
										(WfProcessInstance)executionTarget, 
										BulkActivityFollowUpWorkflow.OPTION_ASSIGN_TO, 
										assignTo
									);
								}						
							} else if(executionTarget instanceof PropertySet) {
								if(activity != null) {
									PropertiesHelper.addProperty(
										(PropertySet)executionTarget, 
										BulkActivityFollowUpWorkflow.OPTION_ACTIVITY, 
										activity
									);	
								}
								if(params.getAssignTo() != null && params.getAssignTo().getXri() != null) {
									Contact assignTo = (Contact)pm.getObjectById(new Path(params.getAssignTo().getXri()));
									PropertiesHelper.addProperty(
										(PropertySet)executionTarget, 
										BulkActivityFollowUpWorkflow.OPTION_ASSIGN_TO, 
										assignTo
									);							
								}						
							}
							{
								int i = 0;
								for(ActivityFollowUpBean activityFollowUpBean: activityFollowUpBeans) {
									if(executionTarget instanceof WfProcessInstance) {
										ActivityProcessTransition activityProcessTransition = (ActivityProcessTransition)pm.getObjectById(new Path(activityFollowUpBean.getTransition().getXri()));
										WorkflowHelper.addParameter(
											(WfProcessInstance)executionTarget, 
											BulkActivityFollowUpWorkflow.OPTION_TRANSITION + i, 
											activityProcessTransition
										);
										WorkflowHelper.addParameter(
											(WfProcessInstance)executionTarget, 
											BulkActivityFollowUpWorkflow.OPTION_FOLLOWUP_TITLE + i, 
											activityFollowUpBean.getTitle()
										);
										WorkflowHelper.addParameter(
											(WfProcessInstance)executionTarget, 
											BulkActivityFollowUpWorkflow.OPTION_FOLLOWUP_TEXT + i, 
											activityFollowUpBean.getText()
										);
										i++;
									} else if(executionTarget instanceof PropertySet) {
										ActivityProcessTransition activityProcessTransition = (ActivityProcessTransition)pm.getObjectById(new Path(activityFollowUpBean.getTransition().getXri()));
										PropertiesHelper.addProperty(
											(PropertySet)executionTarget, 
											BulkActivityFollowUpWorkflow.OPTION_TRANSITION + i, 
											activityProcessTransition
										);
										PropertiesHelper.addProperty(
											(PropertySet)executionTarget, 
											BulkActivityFollowUpWorkflow.OPTION_FOLLOWUP_TITLE + i, 
											activityFollowUpBean.getTitle()
										);
										PropertiesHelper.addProperty(
											(PropertySet)executionTarget, 
											BulkActivityFollowUpWorkflow.OPTION_FOLLOWUP_TEXT + i, 
											activityFollowUpBean.getText()
										);
										i++;								
									}
								}
							}
							pm.currentTransaction().commit();
							if(executionTarget instanceof WfProcessInstance) {
								data.put(
									"wfProcessInstance",
									BulkActivityManager.this.toWfProcessInstanceBean((WfProcessInstance)executionTarget)
								);
							} else {
								data.put(
									"wfProcessInstance",
									new WfProcessInstanceBean()
								);
							}
						} catch(Exception e) {
							try {
								pm.currentTransaction().rollback();
							} catch(Exception e1) {}
							new ServiceException(e).log();
						}
		    	    }
				}
			}
		   	
			private JsfWizardController.ObjectReferenceBean activityProcessState;
			private Boolean hasFollowUpTransitions;
			private Integer counter;
			private DoBulkActivityFollowUpParams doBulkActivityFollowUpParams = new DoBulkActivityFollowUpParams();
			private List<JsfWizardController.ObjectReferenceBean> findAssignToResult;
			private List<JsfWizardController.ObjectReferenceBean> findFollowUp1TransitionResult;
			private List<JsfWizardController.ObjectReferenceBean> findFollowUp2TransitionResult;			
		}

		/**
		 * @return the activityCreator
		 */
		public JsfWizardController.ObjectReferenceBean getActivityCreator() {
			return activityCreator;
		}
		/**
		 * @param activityCreator the activityCreator to set
		 */
		public void setActivityCreator(JsfWizardController.ObjectReferenceBean activityCreator) {
			this.activityCreator = activityCreator;
		}
		/**
		 * @return the targetGroup
		 */
		public JsfWizardController.ObjectReferenceBean getTargetGroup() {
			return targetGroup;
		}
		/**
		 * @param targetGroup the targetGroup to set
		 */
		public void setTargetGroup(JsfWizardController.ObjectReferenceBean targetGroup) {
			this.targetGroup = targetGroup;
		}
		/**
		 * @return the activityTemplate
		 */
		public ActivityTemplateBean getActivityTemplate() {
			return activityTemplate;
		}
		/**
		 * @param activityTemplate the activityTemplate to set
		 */
		public void setActivityTemplate(ActivityTemplateBean activityTemplate) {
			this.activityTemplate = activityTemplate;
		}
		/**
		 * @return the activityGroup
		 */
		public JsfWizardController.ObjectReferenceBean getActivityGroup() {
			return activityGroup;
		}
		/**
		 * @param activityGroup the activityGroup to set
		 */
		public void setActivityGroup(JsfWizardController.ObjectReferenceBean activityGroup) {
			this.activityGroup = activityGroup;
		}
		/**
		 * @return the doBulkCreateActivityParams
		 */
		public DoBulkCreateActivityParams getDoBulkCreateActivityParams() {
			return doBulkCreateActivityParams;
		}
		/**
		 * @param doBulkCreateActivityParams the doBulkCreateActivityParams to set
		 */
		public void setDoBulkCreateActivityParams(DoBulkCreateActivityParams doBulkCreateActivityParams) {
			this.doBulkCreateActivityParams = doBulkCreateActivityParams;
		}
		/**
		 * @return the findTargetGroupEMail1Result
		 */
		public List<JsfWizardController.ObjectReferenceBean> getFindTargetGroupEMail1Result() {
			return findTargetGroupEMail1Result;
		}
		/**
		 * @param findTargetGroupEMail1Result the findTargetGroupEMail1Result to set
		 */
		public void setFindTargetGroupEMail1Result(List<JsfWizardController.ObjectReferenceBean> findTargetGroupEMail1Result) {
			this.findTargetGroupEMail1Result = findTargetGroupEMail1Result;
		}
		/**
		 * @return the findTargetGroupEMail2Result
		 */
		public List<JsfWizardController.ObjectReferenceBean> getFindTargetGroupEMail2Result() {
			return findTargetGroupEMail2Result;
		}
		/**
		 * @param findTargetGroupEMail2Result the findTargetGroupEMail2Result to set
		 */
		public void setFindTargetGroupEMail2Result(List<JsfWizardController.ObjectReferenceBean> findTargetGroupEMail2Result) {
			this.findTargetGroupEMail2Result = findTargetGroupEMail2Result;
		}
		/**
		 * @return the findTargetGroupEMail3Result
		 */
		public List<JsfWizardController.ObjectReferenceBean> getFindTargetGroupEMail3Result() {
			return findTargetGroupEMail3Result;
		}
		/**
		 * @param findTargetGroupEMail3Result the findTargetGroupEMail3Result to set
		 */
		public void setFindTargetGroupEMail3Result(List<JsfWizardController.ObjectReferenceBean> findTargetGroupEMail3Result) {
			this.findTargetGroupEMail3Result = findTargetGroupEMail3Result;
		}
		/**
		 * @return the findSenderEMailResult
		 */
		public List<JsfWizardController.ObjectReferenceBean> getFindSenderEMailResult() {
			return findSenderEMailResult;
		}
		/**
		 * @param findSenderEMailResult the findSenderEMailResult to set
		 */
		public void setFindSenderEMailResult(List<JsfWizardController.ObjectReferenceBean> findSenderEMailResult) {
			this.findSenderEMailResult = findSenderEMailResult;
		}
		/**
		 * @return the bulkActivityFollowUps
		 */
		public List<BulkActivityFollowUpBean> getBulkActivityFollowUps() {
			return bulkActivityFollowUps;
		}
		/**
		 * @param bulkActivityFollowUps the bulkActivityFollowUps to set
		 */
		public void setBulkActivityFollowUps(List<BulkActivityFollowUpBean> bulkActivityFollowUps) {
			this.bulkActivityFollowUps = bulkActivityFollowUps;
		}
		
		/**
		 * Get activity creation status.
		 * 
		 * @param activityGroup
		 * @param activityProcessState
		 * @return
		 * @throws ServiceException
		 */
		public BulkCreateActivityBean.BulkActivityFollowUpBean toBulkActivityFollowUpBean(
			ActivityGroup activityGroup,
			ActivityProcessState activityProcessState,
			ActivityCreator activityCreator
		) throws ServiceException {
			BulkActivityFollowUpBean dataBean = new BulkActivityFollowUpBean();
			dataBean.setActivityProcessState(BulkActivityManager.this.newObjectReferenceBean(activityProcessState));
			dataBean.setHasFollowUpTransitions(
				!BulkActivityManager.this.findActivityProcessTransitions(activityProcessState.refGetPath(), null).isEmpty()
			);
			dataBean.setCounter(
				BulkActivityManager.this.getCountActivities(
					activityGroup, 
					activityProcessState,
					activityCreator
				)
			);
			return dataBean;
		}

	   	/**
	   	 * Find target group 1 email.
	   	 * 
	   	 * @param event
	   	 * @throws ServiceException
	   	 */
	   	public void findTargetGroupEMail1(
	   		jakarta.faces.event.AjaxBehaviorEvent event
	   	) throws ServiceException {
	   		Path path = BulkActivityManager.this.getObjectIdentity();
	   		JsfWizardController.QueryBean queryBean = new JsfWizardController.QueryBean();
	   		queryBean.setPosition(0);
	   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);
	   		queryBean.setQuery(this.getDoBulkCreateActivityParams().getTargetGroupEMail1().getTitle());
	   		this.setFindTargetGroupEMail1Result(
	   			BulkActivityManager.this.findEMailAddresses(
		   			path,
		   			queryBean
		   		)
	   		);
	   	}

	   	/**
	   	 * Find target group 2 email.
	   	 * 
	   	 * @param event
	   	 * @throws ServiceException
	   	 */
	   	public void findTargetGroupEMail2(
	   		jakarta.faces.event.AjaxBehaviorEvent event
	   	) throws ServiceException {
	   		Path path = BulkActivityManager.this.getObjectIdentity();
	   		JsfWizardController.QueryBean queryBean = new JsfWizardController.QueryBean();
	   		queryBean.setPosition(0);
	   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);
	   		queryBean.setQuery(this.getDoBulkCreateActivityParams().getTargetGroupEMail2().getTitle());
	   		this.setFindTargetGroupEMail2Result(
	   			BulkActivityManager.this.findEMailAddresses(
		   			path,
		   			queryBean
		   		)
	   		);
	   	}

	   	/**
	   	 * Find target group 3 email.
	   	 * 
	   	 * @param event
	   	 * @throws ServiceException
	   	 */
	   	public void findTargetGroupEMail3(
	   		jakarta.faces.event.AjaxBehaviorEvent event
	   	) throws ServiceException {
	   		Path path = BulkActivityManager.this.getObjectIdentity();
	   		JsfWizardController.QueryBean queryBean = new JsfWizardController.QueryBean();
	   		queryBean.setPosition(0);
	   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);
	   		queryBean.setQuery(this.getDoBulkCreateActivityParams().getTargetGroupEMail3().getTitle());
	   		this.setFindTargetGroupEMail3Result(
	   			BulkActivityManager.this.findEMailAddresses(
		   			path,
		   			queryBean
		   		)
	   		);
	   	}

	   	/**
	   	 * Find sender email.
	   	 * 
	   	 * @param event
	   	 * @throws ServiceException
	   	 */
	   	public void findSenderEMail(
	   		jakarta.faces.event.AjaxBehaviorEvent event
	   	) throws ServiceException {
	   		Path path = BulkActivityManager.this.getObjectIdentity();
	   		JsfWizardController.QueryBean queryBean = new JsfWizardController.QueryBean();
	   		queryBean.setPosition(0);
	   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);
	   		queryBean.setQuery(this.getActivityTemplate().getSenderEMail().getTitle());
	   		this.setFindSenderEMailResult(
	   			BulkActivityManager.this.findEMailAddresses(
		   			path,
		   			queryBean
		   		)
	   		);
	   	}

		/**
		 * Do bulk create activity.
		 * 
		 * @param path
		 * @param params
		 * @return
		 * @throws ServiceException
		 */
		public void doBulkCreateActivity(
			jakarta.faces.event.AjaxBehaviorEvent event
		) throws ServiceException {
			PersistenceManager pm = BulkActivityManager.this.getPm();
			ApplicationContext app = BulkActivityManager.this.getApp();
			Map<String,Object> data = BulkActivityManager.this.getData();
			Path path = BulkActivityManager.this.getObjectIdentity();
			DoBulkCreateActivityParams params = this.getDoBulkCreateActivityParams();
			BulkCreateActivityWorkflow.CreationType creationType = null;
			ActivityGroup activityGroup = (ActivityGroup)pm.getObjectById(new Path(this.getActivityGroup().getXri()));
			ActivityCreator activityCreator = (ActivityCreator)pm.getObjectById(new Path(this.getActivityCreator().getXri()));
			ActivityTemplateBean activityTemplateBean = BulkCreateActivityBean.this.getActivityTemplate();
			if(Boolean.TRUE.equals(params.getRestrictTargetGroup())) {
				if(Boolean.TRUE.equals(params.getIsConfirmed())) {
					creationType = BulkCreateActivityWorkflow.CreationType.CREATE_TEST_CONFIRMED;
				} else {
					creationType = BulkCreateActivityWorkflow.CreationType.CREATE_TEST;
				}
			} else {
				if(Boolean.TRUE.equals(params.getIsConfirmed())) {
					creationType = BulkCreateActivityWorkflow.CreationType.CREATE_CONFIRMED;
				} else {
					creationType = BulkCreateActivityWorkflow.CreationType.CREATE;
				}
			}
	    	try {
				UserHome currentUserHome = (UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());		  
				org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = BulkActivityManager.this.getWorkflowSegment(path);
				WfProcess wfProcess = Workflows.getInstance().findWfProcess(org.opencrx.kernel.backend.Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY, workflowSegment);
				pm.currentTransaction().begin();
				WfProcessInstance wfProcessInstance = Workflows.getInstance().executeWorkflow(
					activityCreator.getName() + " / " + creationType.name() + " / " + currentUserHome.refGetPath().getLastSegment().toString(),
					currentUserHome, 
					wfProcess,
					activityCreator,
					null, // booleanParams
					null, // stringParams
					null, // integerParams 
					null, // decimalParams
					null, // dateTimeParams
					null, // uriParams
					null // parentProcessInstance
				);
				// Set BulkCreateActivityWorkflow parameters
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_LOCALE,
					activityTemplateBean.getLocale()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_DEFAULT_PLACEHOLDERS,
					activityTemplateBean.getPlaceHolders()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_CREATION_TYPE, 
					creationType.name()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_ACCOUNTS_SELECTOR, 
					(AccountFilterGlobal)activityGroup.getTargetGroupAccounts()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_ACTIVITY_NAME, 
					activityTemplateBean.getName()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_ACTIVITY_DESCRIPTION, 
					activityTemplateBean.getDescription()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_ACTIVITY_DETAILED_DESCRIPTION, 
					activityTemplateBean.getDetailedDescription()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance,
					BulkCreateActivityWorkflow.OPTION_ACTIVITY_SCHEDULED_START, 
					activityTemplateBean.getScheduledStart()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_ACTIVITY_SCHEDULED_END, 
					activityTemplateBean.getScheduledEnd()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_ACTIVITY_DUE_BY, 
					activityTemplateBean.getDueBy()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_EMAIL_SENDER, 
					activityTemplateBean.getSenderEMail() == null || activityTemplateBean.getSenderEMail().getXri() == null || activityTemplateBean.getSenderEMail().getXri().isEmpty()
						? null
						: (AccountAddress)pm.getObjectById(new Path(activityTemplateBean.getSenderEMail().getXri()))
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_EMAIL_MESSAGE_SUBJECT, 
					activityTemplateBean.getMessageSubject()
				);
				if(activityTemplateBean.getMessageBody() != null) {
					int idx = 0;
					for(String messageBodyPart: Utils.splitString(activityTemplateBean.getMessageBody(), 2048)) {
						WorkflowHelper.addParameter(
							wfProcessInstance, 
							BulkCreateActivityWorkflow.OPTION_EMAIL_MESSAGE_BODY + idx, 
							messageBodyPart
						);
						idx++;
					}
				}
				WorkflowHelper.addParameter(
					wfProcessInstance,
					BulkCreateActivityWorkflow.OPTION_EMAIL_ADDRESS_USAGE + Integer.toString(0),
					activityTemplateBean.getRecipientEMailAddressUsage1()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_EMAIL_ADDRESS_USAGE + Integer.toString(1),
					activityTemplateBean.getRecipientEMailAddressUsage2()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance,
					BulkCreateActivityWorkflow.OPTION_EMAIL_ADDRESS_USAGE + Integer.toString(2), 
					activityTemplateBean.getRecipientEMailAddressUsage3()
				);
				if(Boolean.TRUE.equals(params.getRestrictTargetGroup())) {
					WorkflowHelper.addParameter(
						wfProcessInstance,
						BulkCreateActivityWorkflow.OPTION_TEST_EMAIL + Integer.toString(0),
						params.getTargetGroupEMail1() == null ? null : (BasicObject)pm.getObjectById(new Path(params.getTargetGroupEMail1().getXri()))
					);
					WorkflowHelper.addParameter(
						wfProcessInstance,
						BulkCreateActivityWorkflow.OPTION_TEST_EMAIL + Integer.toString(1),
						params.getTargetGroupEMail2() == null ? null : (BasicObject)pm.getObjectById(new Path(params.getTargetGroupEMail2().getXri()))
					);
					WorkflowHelper.addParameter(
						wfProcessInstance,
						BulkCreateActivityWorkflow.OPTION_TEST_EMAIL + Integer.toString(2),
						params.getTargetGroupEMail3() == null ? null : (BasicObject)pm.getObjectById(new Path(params.getTargetGroupEMail3().getXri()))
					);
				}
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_EXCLUDE_NO_BULK_EMAIL, 
					activityTemplateBean.getExcludeNoBulkEMail()
				);
				WorkflowHelper.addParameter(
					wfProcessInstance, 
					BulkCreateActivityWorkflow.OPTION_IGNORE_EXECUTION_TIME_LIMIT, 
					params.getIgnoreExecutionTimeLimit()
				);
				pm.currentTransaction().commit();
				// In test mode execute BulkCreateActivityWorkflow immediately instead 
				// of waiting for WorkflowHandlerServlet
				if(
					creationType == CreationType.CREATE ||
					creationType == CreationType.CREATE_TEST ||
					creationType == CreationType.CREATE_TEST_CONFIRMED
				) {
					try {
						pm.currentTransaction().begin();
						wfProcessInstance.setStartedOn(new Date());
	                    pm.currentTransaction().commit();
						new BulkCreateActivityWorkflow().execute(wfProcessInstance);
						pm.currentTransaction().begin();
	                	pm.refresh(wfProcessInstance);
	                	wfProcessInstance.setFailed(Boolean.FALSE);
	                	wfProcessInstance.setLastActivityOn(new Date());
	                	wfProcessInstance.setStepCounter(
	                        Integer.valueOf(wfProcessInstance.getStepCounter().intValue() + 1)
	                    );
	                    pm.currentTransaction().commit();
					} catch(Exception e) {
						try {
							pm.currentTransaction().rollback();
						} catch(Exception ignore) {}
						if(wfProcessInstance != null) {
							pm.currentTransaction().begin();
							wfProcessInstance.setFailed(Boolean.TRUE);
							pm.currentTransaction().commit();
						}
					}
				} else {
					// Let workflow handler execute the process
				}
				data.put(
					"wfProcessInstance",
					BulkActivityManager.this.toWfProcessInstanceBean(wfProcessInstance)
				);
			} catch(Exception e) {
				try {
					pm.currentTransaction().rollback();
				} catch(Exception e1) {}
				new ServiceException(e).log();
			}
		}

		/**
		 * Update activity template.
		 * 
		 * @param dataBean
		 * @throws ServiceException
		 */
		public void saveActivityTemplate(
			jakarta.faces.event.AjaxBehaviorEvent event		
		) throws ServiceException {
			PersistenceManager pm = BulkActivityManager.this.getPm();
			ActivityTemplateBean activityTemplateBean = this.getActivityTemplate();
			ActivityCreator activityCreator = (ActivityCreator)pm.getObjectById(new Path(activityTemplateBean.getActivityCreator().getXri()));		
			String propertySetPrefix = ":" + BulkActivityManager.PROPERTY_SET_NAME_SETTINS + "." + activityTemplateBean.getLocale();		
			String templatePlaceHolders = activityTemplateBean.getPlaceHolders();
			// Amend place holders
			try {
				Properties placeHolders = new Properties();
				if(templatePlaceHolders != null) {
					placeHolders.load(new StringReader(templatePlaceHolders));
				}
				BulkCreateActivityWorkflow bulkCreateActivityWorkflow = new BulkCreateActivityWorkflow();
				bulkCreateActivityWorkflow.updatePlaceHolders(
					placeHolders,
					activityTemplateBean.getDescription()
				);
				bulkCreateActivityWorkflow.updatePlaceHolders(
					placeHolders,
					activityTemplateBean.getDetailedDescription()
				);
				bulkCreateActivityWorkflow.updatePlaceHolders(
					placeHolders,
					activityTemplateBean.getMessageSubject()
				);
				bulkCreateActivityWorkflow.updatePlaceHolders(
					placeHolders,
					activityTemplateBean.getMessageBody()
				);
				ByteArrayOutputStream bos = new ByteArrayOutputStream();			
				placeHolders.store(new OutputStreamWriter(bos, "UTF-8"), "");
				activityTemplateBean.setPlaceHolders(bos.toString("UTF-8"));
			} catch(Exception ignore) {}
			// Save template
			try {
				pm.currentTransaction().begin();
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!name", 
					activityTemplateBean.getName()
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!description", 
					activityTemplateBean.getDescription()
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!detailedDescription", 
					activityTemplateBean.getDetailedDescription()
				);
				BulkActivityManager.this.setDateTimeProperty(
					activityCreator, 
					propertySetPrefix,
					"!scheduledStart", 
					activityTemplateBean.getScheduledStart()
				);
				BulkActivityManager.this.setDateTimeProperty(
					activityCreator, 
					propertySetPrefix,
					"!scheduledEnd", 
					activityTemplateBean.getScheduledEnd()
				);
				BulkActivityManager.this.setDateTimeProperty(
					activityCreator, 
					propertySetPrefix,
					"!dueBy", 
					activityTemplateBean.getDueBy()
				);
				BulkActivityManager.this.setIntegerProperty(
					activityCreator, 
					propertySetPrefix,
					"!priority", 
					activityTemplateBean.getPriority() == null ? null : activityTemplateBean.getPriority().intValue()
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!messageSubject", 
					activityTemplateBean.getMessageSubject()
				);
				// Split messageBody into pieces of 2048 chars
				{
					List<String> messageBodyParts = Utils.splitString(activityTemplateBean.getMessageBody(), 2048);
					for(int i = 0; i < messageBodyParts.size(); i++) {
						try {
							BulkActivityManager.this.setStringProperty(
								activityCreator, 
								propertySetPrefix,
								"!messageBody" + i, 
								messageBodyParts.get(i)
							);
						} catch (Exception e) {
							new ServiceException(e).log();
						}
					}
					// reset unused messageBody properties if they exist
					try {
						int idx = messageBodyParts.size();
						while(BulkActivityManager.this.getStringProperty(activityCreator, propertySetPrefix, "!messageBody" + idx) != null) {
							org.opencrx.kernel.base.jmi1.Property property = BulkActivityManager.this.findProperty(
								activityCreator, 
								propertySetPrefix,
								"!messageBody" + idx
							);
							property.refDelete();
							idx++;
						}
					} catch (Exception e) {
						new ServiceException(e).log();
					}
				}
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!placeHolders", 
					activityTemplateBean.getPlaceHolders()
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!excludeNoBulkEMail", 
					Boolean.toString(Boolean.TRUE.equals(activityTemplateBean.getExcludeNoBulkEMail()))
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!sender", 
					activityTemplateBean.getSenderEMail() == null ? null : activityTemplateBean.getSenderEMail().getXri()
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!usage." + Integer.toString(0),
					activityTemplateBean.getRecipientEMailAddressUsage1() == null ? null : Short.toString(activityTemplateBean.getRecipientEMailAddressUsage1())
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!usage." + Integer.toString(1),
					activityTemplateBean.getRecipientEMailAddressUsage2() == null ? null : Short.toString(activityTemplateBean.getRecipientEMailAddressUsage2())
				);
				BulkActivityManager.this.setStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!usage." + Integer.toString(2),
					activityTemplateBean.getRecipientEMailAddressUsage3() == null ? null : Short.toString(activityTemplateBean.getRecipientEMailAddressUsage3())
				);
				pm.currentTransaction().commit();
			} catch(Exception e) {
				new ServiceException(e).log();
				try {
					pm.currentTransaction().rollback();
				} catch(Exception e0) {}
			}
		}
	
		private JsfWizardController.ObjectReferenceBean activityGroup;
		private JsfWizardController.ObjectReferenceBean activityCreator;
		private JsfWizardController.ObjectReferenceBean targetGroup;
		private ActivityTemplateBean activityTemplate;
		private List<BulkActivityFollowUpBean> bulkActivityFollowUps;
		private DoBulkCreateActivityParams doBulkCreateActivityParams;
		private List<JsfWizardController.ObjectReferenceBean> findTargetGroupEMail1Result;
		private List<JsfWizardController.ObjectReferenceBean> findTargetGroupEMail2Result;
		private List<JsfWizardController.ObjectReferenceBean> findTargetGroupEMail3Result;
		private List<JsfWizardController.ObjectReferenceBean> findSenderEMailResult;
	}

	/**
	 * DoBulkCreateActivityParams
	 *
	 */
	public static class DoBulkCreateActivityParams {
		/**
		 * @return the isConfirmed
		 */
		public Boolean getIsConfirmed() {
			return isConfirmed;
		}
		/**
		 * @param isConfirmed the isConfirmed to set
		 */
		public void setIsConfirmed(Boolean isConfirmed) {
			this.isConfirmed = isConfirmed;
		}
		/**
		 * @return the restrictTargetGroup
		 */
		public Boolean getRestrictTargetGroup() {
			return restrictTargetGroup;
		}
		/**
		 * @param restrictTargetGroup the restrictTargetGroup to set
		 */
		public void setRestrictTargetGroup(Boolean restrictTargetGroup) {
			this.restrictTargetGroup = restrictTargetGroup;
		}
		/**
		 * @return the targetGroupEMail1
		 */
		public JsfWizardController.ObjectReferenceBean getTargetGroupEMail1() {
			return targetGroupEMail1;
		}
		/**
		 * @param targetGroupEMail1 the targetGroupEMail1 to set
		 */
		public void setTargetGroupEMail1(JsfWizardController.ObjectReferenceBean targetGroupEMail1) {
			this.targetGroupEMail1 = targetGroupEMail1;
		}
		/**
		 * @return the targetGroupEMail2
		 */
		public JsfWizardController.ObjectReferenceBean getTargetGroupEMail2() {
			return targetGroupEMail2;
		}
		/**
		 * @param targetGroupEMail2 the targetGroupEMail2 to set
		 */
		public void setTargetGroupEMail2(JsfWizardController.ObjectReferenceBean targetGroupEMail2) {
			this.targetGroupEMail2 = targetGroupEMail2;
		}
		/**
		 * @return the targetGroupEMail3
		 */
		public JsfWizardController.ObjectReferenceBean getTargetGroupEMail3() {
			return targetGroupEMail3;
		}
		/**
		 * @param targetGroupEMail3 the targetGroupEMail3 to set
		 */
		public void setTargetGroupEMail3(JsfWizardController.ObjectReferenceBean targetGroupEMail3) {
			this.targetGroupEMail3 = targetGroupEMail3;
		}
		/**
		 * @return the ignoreExecutionTimeLimit
		 */
		public Boolean getIgnoreExecutionTimeLimit() {
			return ignoreExecutionTimeLimit;
		}
		/**
		 * @param ignoreExecutionTimeLimit the ignoreExecutionTimeLimit to set
		 */
		public void setIgnoreExecutionTimeLimit(Boolean ignoreExecutionTimeLimit) {
			this.ignoreExecutionTimeLimit = ignoreExecutionTimeLimit;
		}
		private Boolean restrictTargetGroup;
		private JsfWizardController.ObjectReferenceBean targetGroupEMail1 = new JsfWizardController.ObjectReferenceBean();
		private JsfWizardController.ObjectReferenceBean targetGroupEMail2 = new JsfWizardController.ObjectReferenceBean();
		private JsfWizardController.ObjectReferenceBean targetGroupEMail3 = new JsfWizardController.ObjectReferenceBean();		
		private Boolean isConfirmed;
		private Boolean ignoreExecutionTimeLimit;
	}

	/**
	 * DoBulkActivityFollowUpParams
	 *
	 */
	public static class DoBulkActivityFollowUpParams {
		/**
		 * @return the timer
		 */
		public TimerBean getTimer() {
			return timer;
		}
		/**
		 * @param timer the timer to set
		 */
		public void setTimer(TimerBean timer) {
			this.timer = timer;
		}
		/**
		 * @return the assignTo
		 */
		public JsfWizardController.ObjectReferenceBean getAssignTo() {
			return assignTo;
		}
		/**
		 * @param assignTo the assignTo to set
		 */
		public void setAssignTo(JsfWizardController.ObjectReferenceBean assignTo) {
			this.assignTo = assignTo;
		}
		/**
		 * @return the followUp1
		 */
		public ActivityFollowUpBean getFollowUp1() {
			return followUp1;
		}
		/**
		 * @param followUp1 the followUp1 to set
		 */
		public void setFollowUp1(ActivityFollowUpBean followUp1) {
			this.followUp1 = followUp1;
		}
		/**
		 * @return the followUp2
		 */
		public ActivityFollowUpBean getFollowUp2() {
			return followUp2;
		}
		/**
		 * @param followUp2 the followUp2 to set
		 */
		public void setFollowUp2(ActivityFollowUpBean followUp2) {
			this.followUp2 = followUp2;
		}
		private JsfWizardController.ObjectReferenceBean assignTo = new JsfWizardController.ObjectReferenceBean();
		private ActivityFollowUpBean followUp1 = new ActivityFollowUpBean();
		private ActivityFollowUpBean followUp2 = new ActivityFollowUpBean();
		private TimerBean timer = new TimerBean();
	}

	/**
	 * ProcessState
	 *
	 */
	public enum ProcessState {
	    NA,
		PENDING_NOTYETSTARTED,
		PENDING_STARTED,
		COMPLETED_SUCCESS,
		COMPLETED_FAILURE
	}

	/**
	 * Constructor.
	 * 
	 */
	public BulkActivityManager(
	) {
	}

	/**
	 * Get account segment.
	 * 
	 * @param path
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.account1.jmi1.Segment getAccountSegment(
		Path path
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		return (org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
			new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", path.getSegment(2).toString(), "segment", path.getSegment(4).toString())
		);
	}

	/**
	 * Get workflow segment.
	 * 
	 * @param path
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.workflow1.jmi1.Segment getWorkflowSegment(
		Path path
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		return (org.opencrx.kernel.workflow1.jmi1.Segment)pm.getObjectById(
			new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", path.getSegment(2).toString(), "segment", path.getSegment(4).toString())
		);
	}

	/**
	 * Init wizard settings for given activity creator and locale.
	 * 
	 * @param activityCreator
	 * @param locale
	 * @throws ServiceException
	 */
	public static void initSettings(
		ActivityCreator activityCreator, 
		short locale
	) throws ServiceException {
		StringPropertyDataBinding stringPropertyDataBinding = new StringPropertyDataBinding();				
    	stringPropertyDataBinding.setValue(
			activityCreator, 
			":" + PROPERTY_SET_NAME_SETTINS + "." + locale + "!name",
			activityCreator.getName() == null 
				? "@TODO"
				: activityCreator.getName().indexOf("-") > 0 
					? activityCreator.getName().substring(0, activityCreator.getName().indexOf("-"))
					: activityCreator.getName()
		);
	}
	
	/**
	 * Get template locales.
	 * 
	 * @return
	 */
	public List<Short> getTemplateLocales(
		ActivityCreator activityCreator
	) {
		PersistenceManager pm = this.getPm();
		List<Short> selectableLocales = new ArrayList<Short>();
		org.opencrx.kernel.generic.cci2.PropertySetQuery propertySetQuery = (org.opencrx.kernel.generic.cci2.PropertySetQuery)pm.newQuery(org.opencrx.kernel.generic.jmi1.PropertySet.class);
		propertySetQuery.name().like(BulkActivityManager.PROPERTY_SET_NAME_SETTINS + "\\..*");
		List<org.opencrx.kernel.generic.jmi1.PropertySet> settings = activityCreator.getPropertySet(propertySetQuery);
		for(org.opencrx.kernel.generic.jmi1.PropertySet setting: settings) {
			try {
				String settingName = setting.getName();
				selectableLocales.add(
					Short.parseShort(settingName.substring(settingName.lastIndexOf(".") + 1))
				);
			} catch(Exception ignore) {}
		}
		if(selectableLocales.isEmpty()) {
			selectableLocales.add((short)0);
		}
		return selectableLocales;
	}

	/**
	 * Get WfProcess state.
	 * 
	 * @param wfProcessInstance
	 * @return
	 */
	public BulkActivityManager.ProcessState getWfProcessState(
    	WfProcessInstance wfProcessInstance
    ) {
		BulkActivityManager.ProcessState pstate = BulkActivityManager.ProcessState.NA;
    	if (wfProcessInstance != null) {
    		if (wfProcessInstance.getLastActivityOn() == null) {
    			pstate = BulkActivityManager.ProcessState.PENDING_NOTYETSTARTED;
    		} else  if (wfProcessInstance.getStartedOn() == null){
    			pstate = BulkActivityManager.ProcessState.PENDING_STARTED;
    		} else if (wfProcessInstance.isFailed() != null && wfProcessInstance.isFailed().booleanValue()) {
    			pstate = BulkActivityManager.ProcessState.COMPLETED_FAILURE;
    		} else {
    			pstate = BulkActivityManager.ProcessState.COMPLETED_SUCCESS;
    		}
    	}
    	return pstate;
    }

    /**
     * Get number of failed child processes.
     * 
     * @param wfProcessInstance
     * @return
     */
    public int getCountChildrenFailed(
    	WfProcessInstance wfProcessInstance
    ) {
    	PersistenceManager pm = this.getPm();
		org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery query = (org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.WfProcessInstance.class);
		query.startedOn().isNonNull();
		query.forAllFailed().isTrue();
		QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);		
		return wfProcessInstance.getChildProcessInstance(query).size();    	
    }
    
    /**
     * Get number of pending child processes.
     * 
     * @param wfProcessInstance
     * @return
     */
    public int getCountChildrenPending(
    	WfProcessInstance wfProcessInstance
    ) {
    	PersistenceManager pm = this.getPm();
		org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery query = (org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.WfProcessInstance.class);
		query.startedOn().isNull();
		QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);		
		return wfProcessInstance.getChildProcessInstance(query).size();    	
    }

    /**
     * Get number of successful child processes.
     * 
     * @param wfProcessInstance
     * @return
     */
    public int getCountChildrenSuccess(
    	WfProcessInstance wfProcessInstance
    ) {
    	PersistenceManager pm = this.getPm();
		org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery query = (org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.WfProcessInstance.class);
		query.startedOn().isNonNull();
		query.forAllFailed().isFalse();
		QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);		
		return wfProcessInstance.getChildProcessInstance(query).size();    	
    }
	
    /**
     * Get number of activities having the given process state.
     * 
     * @return
     */
    public int getCountActivities(
    	ActivityGroup activityGroup,
    	ActivityProcessState activityProcessState,
    	ActivityCreator activityCreator
    ) {
    	PersistenceManager pm = this.getPm();
		ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
		query.thereExistsProcessState().equalTo(activityProcessState);
		query.thereExistsLastAppliedCreator().equalTo(activityCreator);
		QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);		
		return activityGroup.getFilteredActivity(query).size();
    }

	/**
	 * Map WfProcessInstance to bean.
	 * 
	 * @param wfProcessInstance
	 * @return
	 * @throws ServiceException
	 */
	public WfProcessInstanceBean toWfProcessInstanceBean(
		WfProcessInstance wfProcessInstance
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		SimpleDateFormat dtf = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss", this.getApp().getCurrentLocale());
		dtf.setTimeZone(TimeZone.getTimeZone(this.getApp().getCurrentTimeZone()));									
		WfProcessInstanceBean dataBean = new WfProcessInstanceBean();
		dataBean.setWfProcessInstance(this.newObjectReferenceBean(wfProcessInstance));
		BulkActivityManager.ProcessState processState = this.getWfProcessState(wfProcessInstance);
		dataBean.setLastActivityOn(wfProcessInstance.getLastActivityOn());
		dataBean.setStartedOn(wfProcessInstance.getStartedOn());
		dataBean.setCreatedAt(wfProcessInstance.getCreatedAt());
		dataBean.setIsFailed(processState == BulkActivityManager.ProcessState.COMPLETED_FAILURE);
		{
			List<String> logEntries = new ArrayList<String>();
			try {
				WfActionLogEntryQuery wfActionLogEntryQuery = (WfActionLogEntryQuery)pm.newQuery(WfActionLogEntry.class);
				wfActionLogEntryQuery.orderByModifiedAt().descending();								
				int counter = processState == BulkActivityManager.ProcessState.COMPLETED_SUCCESS ? 1 : 3;
				List<WfActionLogEntry> wfActionLogEntries = wfProcessInstance.getActionLog(wfActionLogEntryQuery);
				for(WfActionLogEntry entry: wfActionLogEntries) {
					logEntries.add(dtf.format(entry.getCreatedAt()) + "  " + entry.getName() + "  " + entry.getDescription());
					counter--;
					if(counter <= 0) break;
				}
			} catch(Exception ignore) {}
			try {
				if (!wfProcessInstance.getChildProcessInstance().isEmpty()) {
					int countPending = this.getCountChildrenPending(wfProcessInstance);
					if(countPending > 0) {
						// override process state in case there are pending children
						processState = BulkActivityManager.ProcessState.PENDING_STARTED;
					}
					int countSuccess = this.getCountChildrenSuccess(wfProcessInstance);
					int countFailed = this.getCountChildrenFailed(wfProcessInstance);
					logEntries.add("                      Children: " + (countPending == 0 ? "Complete" : "In progress") + " {Success: " + countSuccess + ", Failed: " + countFailed + ", Pending: " + countPending + ", Total: " + (countSuccess + countFailed + countPending) + "}");
				}
			} catch(Exception ignore) {}
			dataBean.setLogEntries(logEntries);
		}
		dataBean.setProcessState(processState.name());
		dataBean.setIsCompleted(
			processState == BulkActivityManager.ProcessState.COMPLETED_FAILURE ||
			processState == BulkActivityManager.ProcessState.COMPLETED_SUCCESS
		);
		return dataBean;
	}

	/**
	 * Get string property value.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @return
	 * @throws ServiceException
	 */
	public String getStringProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName
	) throws ServiceException {
		StringPropertyDataBinding stringPropertyDataBinding = new StringPropertyDataBinding();		
		return (String)stringPropertyDataBinding.getValue(
			propertySetHolder, 
			propertySetPrefix + propertyName
		);
	}

	/**
	 * Get integer property value.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @return
	 * @throws ServiceException
	 */
	public Integer getIntegerProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName
	) throws ServiceException {
		IntegerPropertyDataBinding integerPropertyDataBinding = new IntegerPropertyDataBinding();		
		return (Integer)integerPropertyDataBinding.getValue(
			propertySetHolder, 
			propertySetPrefix + propertyName
		);
	}

	/**
	 * Get string property value as object reference.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @return
	 * @throws ServiceException
	 */
	public JsfWizardController.ObjectReferenceBean getObjectReferenceProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(propertySetHolder);
		String xri = this.getStringProperty(
			propertySetHolder, 
			propertySetPrefix, 
			propertyName
		);
		return xri == null || xri.isEmpty()
			? new JsfWizardController.ObjectReferenceBean()
			: this.newObjectReferenceBean((RefObject_1_0)pm.getObjectById(new Path(xri)));
	}

	/**
	 * Get date property value.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @return
	 * @throws ServiceException
	 */
	public Date getDateTimeProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName
	) throws ServiceException {
		DateTimePropertyDataBinding dateTimePropertyDataBinding = new DateTimePropertyDataBinding();		
		return (Date)dateTimePropertyDataBinding.getValue(
			propertySetHolder, 
			propertySetPrefix + propertyName
		);
	}

	/**
	 * Set string property value.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @param value
	 * @throws ServiceException
	 */
	public void setStringProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName,
		String value
	) throws ServiceException {
		StringPropertyDataBinding stringPropertyDataBinding = new StringPropertyDataBinding();				
		stringPropertyDataBinding.setValue(
			propertySetHolder, 
			propertySetPrefix + propertyName, 
			value
		);		
	}

	/**
	 * Set date property value.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @param value
	 * @throws ServiceException
	 */
	public void setDateTimeProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName,
		Date value
	) throws ServiceException {
		DateTimePropertyDataBinding dateTimePropertyDataBinding = new DateTimePropertyDataBinding();				
		dateTimePropertyDataBinding.setValue(
			propertySetHolder, 
			propertySetPrefix + propertyName, 
			value
		);		
	}

	/**
	 * Set integer property value.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @param value
	 * @throws ServiceException
	 */
	public void setIntegerProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName,
		Integer value
	) throws ServiceException {
		IntegerPropertyDataBinding integerPropertyDataBinding = new IntegerPropertyDataBinding();				
		integerPropertyDataBinding.setValue(
			propertySetHolder, 
			propertySetPrefix + propertyName, 
			value
		);		
	}

	/**
	 * Find property.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.base.jmi1.Property findProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName
	) throws ServiceException {
		StringPropertyDataBinding stringPropertyDataBinding = new StringPropertyDataBinding();
		return stringPropertyDataBinding.findProperty(
			propertySetHolder,
			propertySetPrefix + propertyName
		);
	}

	/**
	 * Set object reference property.
	 * 
	 * @param propertySetHolder
	 * @param propertySetPrefix
	 * @param propertyName
	 * @param value
	 * @throws ServiceException
	 */
	public void setObjectReferenceProperty(
		CrxObject propertySetHolder,
		String propertySetPrefix,
		String propertyName,
		JsfWizardController.ObjectReferenceBean value
	) throws ServiceException {
		this.setStringProperty(
			propertySetHolder,
			propertySetPrefix,
			propertyName,
			value == null ? null : value.getXri()
		);
	}

	/**
	 * Get activity template bean.
	 * 
	 * @param activityCreator
	 * @param locale
	 * @return
	 * @throws ServiceException
	 */
	public ActivityTemplateBean toActivityTemplateBean(
		ActivityCreator activityCreator,
		Short locale
	) throws ServiceException {
		ActivityTemplateBean dataBean = new ActivityTemplateBean();
		String propertySetPrefix = ":" + BulkActivityManager.PROPERTY_SET_NAME_SETTINS + "." + locale;
		dataBean.setActivityCreator(this.newObjectReferenceBean(activityCreator));
		dataBean.setLocale(locale);
		dataBean.setExcludeNoBulkEMail(
			Boolean.valueOf(
				this.getStringProperty(
					activityCreator, 
					propertySetPrefix,
					"!excludeNoBulkEMail"
				)
			)
		);
		dataBean.setIsEMail(
			activityCreator.getActivityType().getActivityClass() == Activities.ActivityClass.EMAIL.getValue()
		);
		dataBean.setName(
			this.getStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!name"
			)
		);
		dataBean.setDescription(
			this.getStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!description"
			)
		);
		dataBean.setDetailedDescription(
			this.getStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!detailedDescription"
			)
		);
		dataBean.setScheduledStart(
			this.getDateTimeProperty(
				activityCreator, 
				propertySetPrefix,
				"!scheduledStart"
			)
		);
		dataBean.setScheduledEnd(
			this.getDateTimeProperty(
				activityCreator, 
				propertySetPrefix,
				"!scheduledEnd"
			)
		);
		dataBean.setDueBy(
			this.getDateTimeProperty(
				activityCreator, 
				propertySetPrefix,
				"!dueBy"
			)
		);
		Integer priority = this.getIntegerProperty(
			activityCreator, 
			propertySetPrefix,
			"!priority"
		);
		dataBean.setPriority(priority == null ? null : priority.shortValue());
		dataBean.setMessageSubject(
			this.getStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!messageSubject"
			)
		);
		{
			String tempMessageBody = "";
			int idx = 0;
			try {
				String value = null;
				while((value = this.getStringProperty(activityCreator, propertySetPrefix, "!messageBody" + idx)) != null) {
					tempMessageBody += value;
					idx++;
				}
			} catch (Exception ignore) {}
			dataBean.setMessageBody(tempMessageBody);
		}
		dataBean.setSenderEMail(
			this.getObjectReferenceProperty(
				activityCreator, 
				propertySetPrefix, 
				"!sender"
			)
		);
		{
			String usage = this.getStringProperty(
				activityCreator, 
				propertySetPrefix, 
				"!usage." + Integer.toString(0)
			);
			dataBean.setRecipientEMailAddressUsage1(usage == null ? null : Short.parseShort(usage));
		}
		{
			String usage = this.getStringProperty(
				activityCreator, 
				propertySetPrefix, 
				"!usage." + Integer.toString(1)
			);
			dataBean.setRecipientEMailAddressUsage2(usage == null ? null : Short.parseShort(usage));
		}
		{
			String usage = this.getStringProperty(
				activityCreator, 
				propertySetPrefix, 
				"!usage." + Integer.toString(2)
			);
			dataBean.setRecipientEMailAddressUsage3(usage == null ? null : Short.parseShort(usage));
		}
		if(dataBean.getRecipientEMailAddressUsage1() == null) {
			dataBean.setRecipientEMailAddressUsage1(Addresses.USAGE_BUSINESS);
		}
		dataBean.setPlaceHolders(
			this.getStringProperty(
				activityCreator, 
				propertySetPrefix, 
				"!placeHolders"
			)
		);
		return dataBean;
	}

	/**
	 * Get bulk create activity bean.
	 * 
	 * @param activityGroup
	 * @param activityCreator
	 * @param templateLocale
	 * @return
	 * @throws ServiceException
	 */
	public BulkCreateActivityBean toBulkCreateActivityBean(
		ActivityGroup activityGroup,
		ActivityCreator activityCreator,
		Short templateLocale
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		BulkCreateActivityBean dataBean = new BulkCreateActivityBean();
		dataBean.setActivityGroup(this.newObjectReferenceBean(activityGroup));
		dataBean.setActivityCreator(this.newObjectReferenceBean(activityCreator));
		if(activityGroup.getTargetGroupAccounts() != null) {
			dataBean.setTargetGroup(this.newObjectReferenceBean(activityGroup.getTargetGroupAccounts()));
		}
		dataBean.setActivityTemplate(
			this.toActivityTemplateBean(
				activityCreator,
				templateLocale
			)
		);
		{
			List<BulkCreateActivityBean.BulkActivityFollowUpBean> bulkActivityFollowUps = new ArrayList<BulkCreateActivityBean.BulkActivityFollowUpBean>();
			ActivityProcessStateQuery activityProcessStateQuery = (ActivityProcessStateQuery)pm.newQuery(ActivityProcessState.class);
			activityProcessStateQuery.orderByName().ascending();
			List<ActivityProcessState> activityProcessStates = activityCreator.getActivityType().getControlledBy().<ActivityProcessState>getState(activityProcessStateQuery);
			for(ActivityProcessState activityProcessState: activityProcessStates) {
				bulkActivityFollowUps.add(
					dataBean.toBulkActivityFollowUpBean(
						activityGroup,
						activityProcessState,
						activityCreator
					)
				);
			}
			dataBean.setBulkActivityFollowUps(bulkActivityFollowUps);
		}
		dataBean.setDoBulkCreateActivityParams(new DoBulkCreateActivityParams());		
		return dataBean;
	}

	/**
	 * New data bean.
	 * 
	 * @param path
	 * @return
	 * @throws ServiceException
	 */
	public Map<String,Object> newData(
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		Path path = this.getObjectIdentity();
		Map<String,Object> dataBean = new HashMap<String,Object>();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path.getPrefix(7));
		if(obj instanceof ActivityGroup) {
			ActivityGroup activityGroup = (ActivityGroup)obj;
			dataBean.put("activityGroup", this.newObjectReferenceBean(activityGroup));
			List<BulkCreateActivityBean> bulkCreateActivityBeans = new ArrayList<BulkCreateActivityBean>();
			for(ActivityCreator activityCreator: activityGroup.<ActivityCreator>getActivityCreator()) {
				List<Short> locales = this.getTemplateLocales(activityCreator);
				for(Short locale: locales) {
					bulkCreateActivityBeans.add(
						this.toBulkCreateActivityBean(
							activityGroup,
							activityCreator, 
							locale
						)
					);
				}
			}
			dataBean.put("bulkCreateActivities", bulkCreateActivityBeans);
			List<TimerBean> timerBeans = new ArrayList<TimerBean>();
			TimerQuery timerQuery = (TimerQuery)pm.newQuery(Timer.class);
			timerQuery.thereExistsTarget().equalTo(activityGroup);
			timerQuery.forAllDisabled().isFalse();
			timerQuery.timerState().equalTo(UserHomes.TimerState.OPEN.getValue());
			timerQuery.orderByCreatedAt().descending();
			UserHome userHome = (UserHome)pm.getObjectById(this.getApp().getUserHomeIdentityAsPath());
			List<Timer> timers = userHome.getTimer(timerQuery);
			for(Timer timer: timers) {
				TimerBean timerBean = new TimerBean();
				timerBean.setTimer(this.newObjectReferenceBean(timer));
				timerBean.setName(timer.getName());
				timerBean.setTriggerAt(timer.getTimerStartAt());
				timerBeans.add(timerBean);
			}
			dataBean.put("timers", timerBeans);
			this.refreshWfProcesses(dataBean);
		}
		return dataBean;
	}

	/**
	 * Get contacts.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<JsfWizardController.ObjectReferenceBean> findContacts(
		Path path,
		JsfWizardController.QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(path);
		List<JsfWizardController.ObjectReferenceBean> result = new ArrayList<JsfWizardController.ObjectReferenceBean>();
		if(queryBean != null && !queryBean.getQuery().isEmpty()) {
			ContactQuery contactQuery =  (ContactQuery)pm.newQuery(Contact.class);
			contactQuery.forAllDisabled().isFalse();
			contactQuery.thereExistsFullName().like("(?i).*" + queryBean.getQuery() + ".*");
			contactQuery.orderByFullName().ascending();
			int count = 0;
			for(Iterator<Contact> i = accountSegment.<Contact>getAccount(contactQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
				result.add(this.newObjectReferenceBean(i.next()));
				count++;
				if(count > queryBean.getSize()) {
					break;
				}
			}
		}
		return result;
	}

	/**
	 * Find email addresses.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<JsfWizardController.ObjectReferenceBean> findEMailAddresses(
		Path path,
		JsfWizardController.QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(path);
		EMailAddressQuery emailAddressQuery =  (EMailAddressQuery)pm.newQuery(EMailAddress.class);
		emailAddressQuery.forAllDisabled().isFalse();
		if(queryBean != null) {
			String query = queryBean.getQuery();
			if(query.startsWith("*")) {
				query = query.substring(1);
			}
			query = query.trim();
			emailAddressQuery.thereExistsEmailAddress().like("(?i).*" + query + ".*");
		}
		emailAddressQuery.orderByEmailAddress().ascending();
		List<JsfWizardController.ObjectReferenceBean> result = new ArrayList<JsfWizardController.ObjectReferenceBean>();
		int count = 0;
		for(Iterator<EMailAddress> i = accountSegment.<EMailAddress>getAddress(emailAddressQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
			result.add(this.newObjectReferenceBean(i.next()));
			count++;
			if(count > queryBean.getSize()) {
				break;
			}
		}
		return result;
	}

	/**
	 * Find activity process transitions.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<JsfWizardController.ObjectReferenceBean> findActivityProcessTransitions(
		Path path,
		JsfWizardController.QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		List<JsfWizardController.ObjectReferenceBean> result = new ArrayList<JsfWizardController.ObjectReferenceBean>();
		ActivityProcess activityProcess = (ActivityProcess)pm.getObjectById(path.getPrefix(7));
		ActivityProcessState activityProcessState = (ActivityProcessState)pm.getObjectById(path.getPrefix(9));
		if(activityProcess != null && activityProcessState != null) {
			ActivityProcessTransitionQuery activityProcessTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
			activityProcessTransitionQuery.orderByName().ascending();
			if(queryBean != null) {
				activityProcessTransitionQuery.name().like("(?i).*" + queryBean.getQuery() + ".*");
			}
			activityProcessTransitionQuery.forAllDisabled().isFalse();
			activityProcessTransitionQuery.thereExistsPrevState().equalTo(activityProcessState);
			for(ActivityProcessTransition activityProcessTransition: activityProcess.<ActivityProcessTransition>getTransition(activityProcessTransitionQuery)) {
				result.add(this.newObjectReferenceBean(activityProcessTransition));
			}
		}
		return result;
	}

	/**
	 * Refresh workflow processes.
	 * 
	 * @param data
	 * @throws ServiceException
	 */
	public void refreshWfProcesses(
		Map<String,Object> data
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		pm.evictAll();
		ActivityGroup activityGroup = (ActivityGroup)pm.getObjectById(new Path(((ObjectReferenceBean)data.get("activityGroup")).getXri()));
		List<WfProcessInstanceBean> wfProcessInstanceBeans = new ArrayList<WfProcessInstanceBean>();
		{
			WfProcessInstanceQuery wfProcessInstanceQuery = (WfProcessInstanceQuery)pm.newQuery(WfProcessInstance.class);
			wfProcessInstanceQuery.thereExistsTargetObject().equalTo(activityGroup.getIdentity());
			wfProcessInstanceQuery.process().name().equalTo(Workflows.WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP);
			wfProcessInstanceQuery.orderByCreatedAt().descending();
			UserHome userHome = (UserHome)pm.getObjectById(this.getApp().getUserHomeIdentityAsPath());
			List<WfProcessInstance> wfProcessInstances = userHome.getWfProcessInstance(wfProcessInstanceQuery);
			int count = 0;
			for(WfProcessInstance wfProcessInstance: wfProcessInstances) {
				wfProcessInstanceBeans.add(
					this.toWfProcessInstanceBean(wfProcessInstance)
				);
				count++;
				if(count >= 1) {
					break;
				}
			}
		}
		for(BulkCreateActivityBean bulkCreateActivity: ((List<BulkCreateActivityBean>)data.get("bulkCreateActivities"))) {
			ActivityCreator activityCreator = (ActivityCreator)pm.getObjectById(new Path(bulkCreateActivity.getActivityCreator().getXri()));
			WfProcessInstanceQuery wfProcessInstanceQuery = (WfProcessInstanceQuery)pm.newQuery(WfProcessInstance.class);
			wfProcessInstanceQuery.thereExistsTargetObject().equalTo(activityCreator.getIdentity());
			wfProcessInstanceQuery.process().name().equalTo(Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY);
			wfProcessInstanceQuery.orderByCreatedAt().descending();
			UserHome userHome = (UserHome)pm.getObjectById(this.getApp().getUserHomeIdentityAsPath());
			List<WfProcessInstance> wfProcessInstances = userHome.getWfProcessInstance(wfProcessInstanceQuery);
			int count = 0;
			for(WfProcessInstance wfProcessInstance: wfProcessInstances) {
				wfProcessInstanceBeans.add(
					this.toWfProcessInstanceBean(wfProcessInstance)
				);
				count++;
				if(count >= 1) {
					break;
				}
			}
		}
		data.put("wfProcessInstances", wfProcessInstanceBeans);
	}

   	/**
   	 * doRefreshWfProcesses action.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void doRefreshWfProcesses(
   		jakarta.faces.event.AjaxBehaviorEvent event   		
   	) throws ServiceException {
   		this.refreshWfProcesses(this.getData());
   	}

   	/**
   	 * doRefresh action.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void doRefresh(
   		jakarta.faces.event.AjaxBehaviorEvent event   		
   	) throws ServiceException {
   		this.data = this.newData();
   	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	public static final String PROPERTY_SET_NAME_SETTINS = "BulkCreateActivityWizardSettings";
	private static final int DEFAULT_RESULT_SET_SIZE = 20;	
	
}
