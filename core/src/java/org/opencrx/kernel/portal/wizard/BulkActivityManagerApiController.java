/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BulkActivityManagerApiController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2015, CRIXP Corp., Switzerland
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

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
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
import org.openmdx.portal.servlet.AbstractApiController;
import org.openmdx.portal.servlet.ApplicationContext;

/**
 * BulkActivityManagerApiController
 *
 */
public class BulkActivityManagerApiController extends AbstractApiController {

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
		public AbstractApiController.ObjectReferenceBean getTransition() {
			return transition;
		}
		/**
		 * @param transition the transition to set
		 */
		public void setTransition(AbstractApiController.ObjectReferenceBean transition) {
			this.transition = transition;
		}
		private AbstractApiController.ObjectReferenceBean transition;
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
		public AbstractApiController.ObjectReferenceBean getTimer() {
			return timer;
		}
		/**
		 * @param timer the timer to set
		 */
		public void setTimer(AbstractApiController.ObjectReferenceBean timer) {
			this.timer = timer;
		}
		private AbstractApiController.ObjectReferenceBean timer;
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
		public AbstractApiController.ObjectReferenceBean getWfProcessInstance() {
			return wfProcessInstance;
		}
		/**
		 * @param wfProcessInstance the wfProcessInstance to set
		 */
		public void setWfProcessInstance(AbstractApiController.ObjectReferenceBean wfProcessInstance) {
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
		private AbstractApiController.ObjectReferenceBean wfProcessInstance;
		private String processState;
		private Date lastActivityOn;
		private Date startedOn;
		private Date createdAt;
		private Boolean isFailed;
		private Boolean isCompleted;
		private List<String> logEntries;
	}

	/**
	 * ActivityCreationStateBean
	 *
	 */
	public static class ActivityCreationStateBean {
		/**
		 * @return the activityProcessState
		 */
		public AbstractApiController.ObjectReferenceBean getActivityProcessState() {
			return activityProcessState;
		}
		/**
		 * @param activityProcessState the activityProcessState to set
		 */
		public void setActivityProcessState(AbstractApiController.ObjectReferenceBean activityProcessState) {
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
		private AbstractApiController.ObjectReferenceBean activityProcessState;
		private Boolean hasFollowUpTransitions;
		private Integer counter;
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
		public AbstractApiController.ObjectReferenceBean getSenderEMail() {
			return senderEMail;
		}
		/**
		 * @param senderEMail the senderEMail to set
		 */
		public void setSenderEMail(AbstractApiController.ObjectReferenceBean senderEMail) {
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
		public AbstractApiController.ObjectReferenceBean getActivityCreator() {
			return activityCreator;
		}
		/**
		 * @param activityCreator the activityCreator to set
		 */
		public void setActivityCreator(AbstractApiController.ObjectReferenceBean activityCreator) {
			this.activityCreator = activityCreator;
		}
		private AbstractApiController.ObjectReferenceBean activityCreator;
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
		private AbstractApiController.ObjectReferenceBean senderEMail;
		private Short recipientEMailAddressUsage1;
		private Short recipientEMailAddressUsage2;
		private Short recipientEMailAddressUsage3;
		private String placeHolders;
	}

	/**
	 * BulkCreateActivityBean
	 *
	 */
	public static class BulkCreateActivityBean {
		/**
		 * @return the activityCreator
		 */
		public AbstractApiController.ObjectReferenceBean getActivityCreator() {
			return activityCreator;
		}
		/**
		 * @param activityCreator the activityCreator to set
		 */
		public void setActivityCreator(AbstractApiController.ObjectReferenceBean activityCreator) {
			this.activityCreator = activityCreator;
		}
		/**
		 * @return the targetGroup
		 */
		public AbstractApiController.ObjectReferenceBean getTargetGroup() {
			return targetGroup;
		}
		/**
		 * @param targetGroup the targetGroup to set
		 */
		public void setTargetGroup(AbstractApiController.ObjectReferenceBean targetGroup) {
			this.targetGroup = targetGroup;
		}
		/**
		 * @return the bulkCreateActivityWfProcess
		 */
		public WfProcessInstanceBean getBulkCreateActivityWfProcess() {
			return bulkCreateActivityWfProcess;
		}
		/**
		 * @param bulkCreateActivityWfProcess the bulkCreateActivityWfProcess to set
		 */
		public void setBulkCreateActivityWfProcess(WfProcessInstanceBean bulkCreateActivityWfProcess) {
			this.bulkCreateActivityWfProcess = bulkCreateActivityWfProcess;
		}
		/**
		 * @return the activityCreationStates
		 */
		public List<ActivityCreationStateBean> getActivityCreationStates() {
			return activityCreationStates;
		}
		/**
		 * @param activityCreationStates the activityCreationStates to set
		 */
		public void setActivityCreationStates(List<ActivityCreationStateBean> activityCreationStates) {
			this.activityCreationStates = activityCreationStates;
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
		public AbstractApiController.ObjectReferenceBean getActivityGroup() {
			return activityGroup;
		}
		/**
		 * @param activityGroup the activityGroup to set
		 */
		public void setActivityGroup(AbstractApiController.ObjectReferenceBean activityGroup) {
			this.activityGroup = activityGroup;
		}
		private AbstractApiController.ObjectReferenceBean activityGroup;
		private AbstractApiController.ObjectReferenceBean activityCreator;
		private AbstractApiController.ObjectReferenceBean targetGroup;
		private ActivityTemplateBean activityTemplate;
		private WfProcessInstanceBean bulkCreateActivityWfProcess;
		private List<ActivityCreationStateBean> activityCreationStates;
	}

	/**
	 * DataBean
	 *
	 */
	public static class DataBean {
		/**
		 * @return the bulkActivityFollowUpWfProcess
		 */
		public WfProcessInstanceBean getBulkActivityFollowUpWfProcess() {
			return bulkActivityFollowUpWfProcess;
		}

		/**
		 * @param bulkActivityFollowUpWfProcess the bulkActivityFollowUpWfProcess to set
		 */
		public void setBulkActivityFollowUpWfProcess(WfProcessInstanceBean bulkActivityFollowUpWfProcess) {
			this.bulkActivityFollowUpWfProcess = bulkActivityFollowUpWfProcess;
		}
		/**
		 * @return the bulkCreateActivities
		 */
		public List<BulkCreateActivityBean> getBulkCreateActivities() {
			return bulkCreateActivities;
		}

		/**
		 * @param bulkCreateActivities the bulkCreateActivities to set
		 */
		public void setBulkCreateActivities(List<BulkCreateActivityBean> bulkCreateActivities) {
			this.bulkCreateActivities = bulkCreateActivities;
		}
		/**
		 * @return the activityGroup
		 */
		public AbstractApiController.ObjectReferenceBean getActivityGroup() {
			return activityGroup;
		}

		/**
		 * @param activityGroup the activityGroup to set
		 */
		public void setActivityGroup(AbstractApiController.ObjectReferenceBean activityGroup) {
			this.activityGroup = activityGroup;
		}
		/**
		 * @return the timers
		 */
		public List<TimerBean> getTimers() {
			return timers;
		}

		/**
		 * @param timers the timers to set
		 */
		public void setTimers(List<TimerBean> timers) {
			this.timers = timers;
		}
		private AbstractApiController.ObjectReferenceBean activityGroup;
		private WfProcessInstanceBean bulkActivityFollowUpWfProcess;
		private List<BulkCreateActivityBean> bulkCreateActivities;
		private List<TimerBean> timers;
	}

	/**
	 * MetaInfBean
	 *
	 */
	public static class MetaInfBean {

		/**
		 * @return the locale
		 */
		public String getLocale() {
			return locale;
		}
		/**
		 * @param locale the locale to set
		 */
		public void setLocale(String locale) {
			this.locale = locale;
		}
		/**
		 * @return the timezone
		 */
		public String getTimezone() {
			return timezone;
		}
		/**
		 * @param timezone the timezone to set
		 */
		public void setTimezone(String timezone) {
			this.timezone = timezone;
		}
		/**
		 * @return the labelLastActivityOn
		 */
		public String getLabelLastActivityOn() {
			return labelLastActivityOn;
		}
		/**
		 * @param labelLastActivityOn the labelLastActivityOn to set
		 */
		public void setLabelLastActivityOn(String labelLastActivityOn) {
			this.labelLastActivityOn = labelLastActivityOn;
		}
		/**
		 * @return the labelStartedOn
		 */
		public String getLabelStartedOn() {
			return labelStartedOn;
		}
		/**
		 * @param labelStartedOn the labelStartedOn to set
		 */
		public void setLabelStartedOn(String labelStartedOn) {
			this.labelStartedOn = labelStartedOn;
		}
		/**
		 * @return the labelCreatedAt
		 */
		public String getLabelCreatedAt() {
			return labelCreatedAt;
		}
		/**
		 * @param labelCreatedAt the labelCreatedAt to set
		 */
		public void setLabelCreatedAt(String labelCreatedAt) {
			this.labelCreatedAt = labelCreatedAt;
		}		
		/**
		 * @return the labelTargetGroupAccounts
		 */
		public String getLabelTargetGroupAccounts() {
			return labelTargetGroupAccounts;
		}
		/**
		 * @param labelTargetGroupAccounts the labelTargetGroupAccounts to set
		 */
		public void setLabelTargetGroupAccounts(String labelTargetGroupAccounts) {
			this.labelTargetGroupAccounts = labelTargetGroupAccounts;
		}
		/**
		 * @return the labelOK
		 */
		public String getLabelOK() {
			return labelOK;
		}
		/**
		 * @param labelOK the labelOK to set
		 */
		public void setLabelOK(String labelOK) {
			this.labelOK = labelOK;
		}
		/**
		 * @return the labelCancel
		 */
		public String getLabelCancel() {
			return labelCancel;
		}
		/**
		 * @param labelCancel the labelCancel to set
		 */
		public void setLabelCancel(String labelCancel) {
			this.labelCancel = labelCancel;
		}
		/**
		 * @return the labelActivityTemplate
		 */
		public String getLabelActivityTemplate() {
			return labelActivityTemplate;
		}
		/**
		 * @param labelActivityTemplate the labelActivityTemplate to set
		 */
		public void setLabelActivityTemplate(String labelActivityTemplate) {
			this.labelActivityTemplate = labelActivityTemplate;
		}
		/**
		 * @return the labelAssignTo
		 */
		public String getLabelAssignTo() {
			return labelAssignTo;
		}
		/**
		 * @param labelAssignTo the labelAssignTo to set
		 */
		public void setLabelAssignTo(String labelAssignTo) {
			this.labelAssignTo = labelAssignTo;
		}
		/**
		 * @return the labelTransition
		 */
		public String getLabelTransition() {
			return labelTransition;
		}
		/**
		 * @param labelTransition the labelTransition to set
		 */
		public void setLabelTransition(String labelTransition) {
			this.labelTransition = labelTransition;
		}		
		/**
		 * @return the labelFollowUpTitle
		 */
		public String getLabelFollowUpTitle() {
			return labelFollowUpTitle;
		}
		/**
		 * @param labelFollowUpTitle the labelFollowUpTitle to set
		 */
		public void setLabelFollowUpTitle(String labelFollowUpTitle) {
			this.labelFollowUpTitle = labelFollowUpTitle;
		}
		/**
		 * @return the labelFollowUpText
		 */
		public String getLabelFollowUpText() {
			return labelFollowUpText;
		}
		/**
		 * @param labelFollowUpText the labelFollowUpText to set
		 */
		public void setLabelFollowUpText(String labelFollowUpText) {
			this.labelFollowUpText = labelFollowUpText;
		}
		/**
		 * @return the labelTimerName
		 */
		public String getLabelTimerName() {
			return labelTimerName;
		}
		/**
		 * @param labelTimerName the labelTimerName to set
		 */
		public void setLabelTimerName(String labelTimerName) {
			this.labelTimerName = labelTimerName;
		}
		/**
		 * @return the labelTimerTriggerAt
		 */
		public String getLabelTimerTriggerAt() {
			return labelTimerTriggerAt;
		}
		/**
		 * @param labelTimerTriggerAt the labelTimerTriggerAt to set
		 */
		public void setLabelTimerTriggerAt(String labelTimerTriggerAt) {
			this.labelTimerTriggerAt = labelTimerTriggerAt;
		}
		/**
		 * @return the labelConfirmDoBulkCreateActivity
		 */
		public String getLabelConfirmDoBulkCreateActivity() {
			return labelConfirmDoBulkCreateActivity;
		}
		/**
		 * @param labelConfirmDoBulkCreateActivity the labelConfirmDoBulkCreateActivity to set
		 */
		public void setLabelConfirmDoBulkCreateActivity(String labelConfirmDoBulkCreateActivity) {
			this.labelConfirmDoBulkCreateActivity = labelConfirmDoBulkCreateActivity;
		}
		/**
		 * @return the labelActivityName
		 */
		public String getLabelActivityName() {
			return labelActivityName;
		}
		/**
		 * @param labelActivityName the labelActivityName to set
		 */
		public void setLabelActivityName(String labelActivityName) {
			this.labelActivityName = labelActivityName;
		}
		/**
		 * @return the labelActivityDetailedDescription
		 */
		public String getLabelActivityDetailedDescription() {
			return labelActivityDetailedDescription;
		}
		/**
		 * @param labelActivityDetailedDescription the labelActivityDetailedDescription to set
		 */
		public void setLabelActivityDetailedDescription(String labelActivityDetailedDescription) {
			this.labelActivityDetailedDescription = labelActivityDetailedDescription;
		}
		/**
		 * @return the labelActivityDescription
		 */
		public String getLabelActivityDescription() {
			return labelActivityDescription;
		}
		/**
		 * @param labelActivityDescription the labelActivityDescription to set
		 */
		public void setLabelActivityDescription(String labelActivityDescription) {
			this.labelActivityDescription = labelActivityDescription;
		}
		/**
		 * @return the labelActivityScheduledStart
		 */
		public String getLabelActivityScheduledStart() {
			return labelActivityScheduledStart;
		}
		/**
		 * @param labelActivityScheduledStart the labelActivityScheduledStart to set
		 */
		public void setLabelActivityScheduledStart(String labelActivityScheduledStart) {
			this.labelActivityScheduledStart = labelActivityScheduledStart;
		}
		/**
		 * @return the labelActivityScheduledEnd
		 */
		public String getLabelActivityScheduledEnd() {
			return labelActivityScheduledEnd;
		}
		/**
		 * @param labelActivityScheduledEnd the labelActivityScheduledEnd to set
		 */
		public void setLabelActivityScheduledEnd(String labelActivityScheduledEnd) {
			this.labelActivityScheduledEnd = labelActivityScheduledEnd;
		}
		/**
		 * @return the labelActivityDueBy
		 */
		public String getLabelActivityDueBy() {
			return labelActivityDueBy;
		}
		/**
		 * @param labelActivityDueBy the labelActivityDueBy to set
		 */
		public void setLabelActivityDueBy(String labelActivityDueBy) {
			this.labelActivityDueBy = labelActivityDueBy;
		}
		/**
		 * @return the labelActivityPriority
		 */
		public String getLabelActivityPriority() {
			return labelActivityPriority;
		}
		/**
		 * @param labelActivityPriority the labelActivityPriority to set
		 */
		public void setLabelActivityPriority(String labelActivityPriority) {
			this.labelActivityPriority = labelActivityPriority;
		}
		/**
		 * @return the labelPlaceHolders
		 */
		public String getLabelPlaceHolders() {
			return labelPlaceHolders;
		}
		/**
		 * @param labelPlaceHolders the labelPlaceHolders to set
		 */
		public void setLabelPlaceHolders(String labelPlaceHolders) {
			this.labelPlaceHolders = labelPlaceHolders;
		}
		/**
		 * @return the labelEMailAddressUsage
		 */
		public String getLabelEMailAddressUsage() {
			return labelEMailAddressUsage;
		}
		/**
		 * @param labelEMailAddressUsage the labelEMailAddressUsage to set
		 */
		public void setLabelEMailAddressUsage(String labelEMailAddressUsage) {
			this.labelEMailAddressUsage = labelEMailAddressUsage;
		}		
		/**
		 * @return the labelDoBulkActivityFollowUp
		 */
		public String getLabelDoBulkActivityFollowUp() {
			return labelDoBulkActivityFollowUp;
		}
		/**
		 * @param labelDoBulkActivityFollowUp the labelDoBulkActivityFollowUp to set
		 */
		public void setLabelDoBulkActivityFollowUp(String labelDoBulkActivityFollowUp) {
			this.labelDoBulkActivityFollowUp = labelDoBulkActivityFollowUp;
		}
		/**
		 * @return the labelDoBulkCreateActivity
		 */
		public String getLabelDoBulkCreateActivity() {
			return labelDoBulkCreateActivity;
		}
		/**
		 * @param labelDoBulkCreateActivity the labelDoBulkCreateActivity to set
		 */
		public void setLabelDoBulkCreateActivity(String labelDoBulkCreateActivity) {
			this.labelDoBulkCreateActivity = labelDoBulkCreateActivity;
		}
		/**
		 * @return the labelFollowUp
		 */
		public String getLabelFollowUp() {
			return labelFollowUp;
		}
		/**
		 * @param labelFollowUp the labelFollowUp to set
		 */
		public void setLabelFollowUp(String labelFollowUp) {
			this.labelFollowUp = labelFollowUp;
		}
		/**
		 * @return the labelRestrictTargetGroup
		 */
		public String getLabelRestrictTargetGroup() {
			return labelRestrictTargetGroup;
		}
		/**
		 * @param labelRestrictTargetGroup the labelRestrictTargetGroup to set
		 */
		public void setLabelRestrictTargetGroup(String labelRestrictTargetGroup) {
			this.labelRestrictTargetGroup = labelRestrictTargetGroup;
		}
		/**
		 * @return the labelEMailAddress
		 */
		public String getLabelEMailAddress() {
			return labelEMailAddress;
		}
		/**
		 * @param labelEMailAddress the labelEMailAddress to set
		 */
		public void setLabelEMailAddress(String labelEMailAddress) {
			this.labelEMailAddress = labelEMailAddress;
		}
		/**
		 * @return the labelEditActivityTemplate
		 */
		public String getLabelEditActivityTemplate() {
			return labelEditActivityTemplate;
		}
		/**
		 * @param labelEditActivityTemplate the labelEditActivityTemplate to set
		 */
		public void setLabelEditActivityTemplate(String labelEditActivityTemplate) {
			this.labelEditActivityTemplate = labelEditActivityTemplate;
		}
		/**
		 * @return the optionsPriority
		 */
		public List<AbstractApiController.OptionBean> getOptionsPriority() {
			return optionsPriority;
		}
		/**
		 * @param optionsPriority the optionsPriority to set
		 */
		public void setOptionsPriority(List<AbstractApiController.OptionBean> optionsPriority) {
			this.optionsPriority = optionsPriority;
		}
		/**
		 * @return the labelSenderEMail
		 */
		public String getLabelSenderEMail() {
			return labelSenderEMail;
		}
		/**
		 * @param labelSenderEMail the labelSenderEMail to set
		 */
		public void setLabelSenderEMail(String labelSenderEMail) {
			this.labelSenderEMail = labelSenderEMail;
		}
		/**
		 * @return the labelMessageSubject
		 */
		public String getLabelMessageSubject() {
			return labelMessageSubject;
		}
		/**
		 * @param labelMessageSubject the labelMessageSubject to set
		 */
		public void setLabelMessageSubject(String labelMessageSubject) {
			this.labelMessageSubject = labelMessageSubject;
		}
		/**
		 * @return the labelMessageBody
		 */
		public String getLabelMessageBody() {
			return labelMessageBody;
		}
		/**
		 * @param labelMessageBody the labelMessageBody to set
		 */
		public void setLabelMessageBody(String labelMessageBody) {
			this.labelMessageBody = labelMessageBody;
		}
		/**
		 * @return the labelExcludeNoBulkEMail
		 */
		public String getLabelExcludeNoBulkEMail() {
			return labelExcludeNoBulkEMail;
		}
		/**
		 * @param labelExcludeNoBulkEMail the labelExcludeNoBulkEMail to set
		 */
		public void setLabelExcludeNoBulkEMail(String labelExcludeNoBulkEMail) {
			this.labelExcludeNoBulkEMail = labelExcludeNoBulkEMail;
		}
		/**
		 * @return the optionsEMailAddressUsage
		 */
		public List<AbstractApiController.OptionBean> getOptionsEMailAddressUsage() {
			return optionsEMailAddressUsage;
		}
		/**
		 * @param optionsEMailAddressUsage the optionsEMailAddressUsage to set
		 */
		public void setOptionsEMailAddressUsage(List<AbstractApiController.OptionBean> optionsEMailAddressUsage) {
			this.optionsEMailAddressUsage = optionsEMailAddressUsage;
		}
		/**
		 * @return the labelPendingWorkflows
		 */
		public String getLabelPendingWorkflows() {
			return labelPendingWorkflows;
		}
		/**
		 * @param labelPendingWorkflows the labelPendingWorkflows to set
		 */
		public void setLabelPendingWorkflows(String labelPendingWorkflows) {
			this.labelPendingWorkflows = labelPendingWorkflows;
		}
		/**
		 * @return the labelWfProcessInstanceName
		 */
		public String getLabelWfProcessInstanceName() {
			return labelWfProcessInstanceName;
		}
		/**
		 * @param labelWfProcessInstanceName the labelWfProcessInstanceName to set
		 */
		public void setLabelWfProcessInstanceName(String labelWfProcessInstanceName) {
			this.labelWfProcessInstanceName = labelWfProcessInstanceName;
		}
		/**
		 * @return the labelActivityCreators
		 */
		public String getLabelActivityCreators() {
			return labelActivityCreators;
		}
		/**
		 * @param labelActivityCreators the labelActivityCreators to set
		 */
		public void setLabelActivityCreators(String labelActivityCreators) {
			this.labelActivityCreators = labelActivityCreators;
		}
		/**
		 * @return the labelActivityCreatorName
		 */
		public String getLabelActivityCreatorName() {
			return labelActivityCreatorName;
		}
		/**
		 * @param labelActivityCreatorName the labelActivityCreatorName to set
		 */
		public void setLabelActivityCreatorName(String labelActivityCreatorName) {
			this.labelActivityCreatorName = labelActivityCreatorName;
		}
		/**
		 * @return the labelEdit
		 */
		public String getLabelEdit() {
			return labelEdit;
		}
		/**
		 * @param labelEdit the labelEdit to set
		 */
		public void setLabelEdit(String labelEdit) {
			this.labelEdit = labelEdit;
		}
		/**
		 * @return the labelSave
		 */
		public String getLabelSave() {
			return labelSave;
		}
		/**
		 * @param labelSave the labelSave to set
		 */
		public void setLabelSave(String labelSave) {
			this.labelSave = labelSave;
		}
		/**
		 * @return the labelWfProcessState
		 */
		public String getLabelWfProcessState() {
			return labelWfProcessState;
		}
		/**
		 * @param labelWfProcessState the labelWfProcessState to set
		 */
		public void setLabelWfProcessState(String labelWfProcessState) {
			this.labelWfProcessState = labelWfProcessState;
		}
		/**
		 * @return the labelTimers
		 */
		public String getLabelTimers() {
			return labelTimers;
		}
		/**
		 * @param labelTimers the labelTimers to set
		 */
		public void setLabelTimers(String labelTimers) {
			this.labelTimers = labelTimers;
		}
		/**
		 * @return the labelIgnoreExecutionTimeLimit
		 */
		public String getLabelIgnoreExecutionTimeLimit() {
			return labelIgnoreExecutionTimeLimit;
		}
		/**
		 * @param labelIgnoreExecutionTimeLimit the labelIgnoreExecutionTimeLimit to set
		 */
		public void setLabelIgnoreExecutionTimeLimit(String labelIgnoreExecutionTimeLimit) {
			this.labelIgnoreExecutionTimeLimit = labelIgnoreExecutionTimeLimit;
		}
		private String locale;
		private String timezone;
		private String labelOK;
		private String labelCancel;
		private String labelEdit;
		private String labelSave;
		private String labelActivityTemplate;
		private String labelLastActivityOn;
		private String labelStartedOn;
		private String labelCreatedAt;
		private String labelTargetGroupAccounts;
		private String labelAssignTo;
		private String labelFollowUpTitle;
		private String labelFollowUpText;
		private String labelTransition;
		private String labelTimerName;
		private String labelTimerTriggerAt;
		private String labelEMailAddress;
		private String labelEMailAddressUsage;
		private String labelConfirmDoBulkCreateActivity;
		private String labelIgnoreExecutionTimeLimit;
		private String labelActivityName;
		private String labelActivityDescription;
		private String labelActivityDetailedDescription;
		private String labelActivityScheduledStart;
		private String labelActivityScheduledEnd;
		private String labelActivityDueBy;
		private String labelActivityPriority;
		private String labelPlaceHolders;
		private String labelDoBulkActivityFollowUp;
		private String labelDoBulkCreateActivity;
		private String labelEditActivityTemplate;
		private String labelFollowUp;
		private String labelRestrictTargetGroup;
		private String labelSenderEMail;
		private String labelMessageSubject;
		private String labelMessageBody;
		private String labelExcludeNoBulkEMail;
		private String labelPendingWorkflows;
		private String labelActivityCreators;
		private String labelTimers;
		private String labelWfProcessInstanceName;
		private String labelWfProcessState;
		private String labelActivityCreatorName;
		private List<AbstractApiController.OptionBean> optionsEMailAddressUsage;
		private List<AbstractApiController.OptionBean> optionsPriority;
	}

	/**
	 * DoBulkCreateActivityParams
	 *
	 */
	public static class DoBulkCreateActivityParams {
		/**
		 * @return the activityCreator
		 */
		public AbstractApiController.ObjectReferenceBean getActivityCreator() {
			return activityCreator;
		}
		/**
		 * @param activityCreator the activityCreator to set
		 */
		public void setActivityCreator(AbstractApiController.ObjectReferenceBean activityCreator) {
			this.activityCreator = activityCreator;
		}
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
		 * @return the activityGroup
		 */
		public AbstractApiController.ObjectReferenceBean getActivityGroup() {
			return activityGroup;
		}
		/**
		 * @param activityGroup the activityGroup to set
		 */
		public void setActivityGroup(AbstractApiController.ObjectReferenceBean activityGroup) {
			this.activityGroup = activityGroup;
		}
		/**
		 * @return the templateLocale
		 */
		public Short getTemplateLocale() {
			return templateLocale;
		}
		/**
		 * @param templateLocale the templateLocale to set
		 */
		public void setTemplateLocale(Short templateLocale) {
			this.templateLocale = templateLocale;
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
		public AbstractApiController.ObjectReferenceBean getTargetGroupEMail1() {
			return targetGroupEMail1;
		}
		/**
		 * @param targetGroupEMail1 the targetGroupEMail1 to set
		 */
		public void setTargetGroupEMail1(AbstractApiController.ObjectReferenceBean targetGroupEMail1) {
			this.targetGroupEMail1 = targetGroupEMail1;
		}
		/**
		 * @return the targetGroupEMail2
		 */
		public AbstractApiController.ObjectReferenceBean getTargetGroupEMail2() {
			return targetGroupEMail2;
		}
		/**
		 * @param targetGroupEMail2 the targetGroupEMail2 to set
		 */
		public void setTargetGroupEMail2(AbstractApiController.ObjectReferenceBean targetGroupEMail2) {
			this.targetGroupEMail2 = targetGroupEMail2;
		}
		/**
		 * @return the targetGroupEMail3
		 */
		public AbstractApiController.ObjectReferenceBean getTargetGroupEMail3() {
			return targetGroupEMail3;
		}
		/**
		 * @param targetGroupEMail3 the targetGroupEMail3 to set
		 */
		public void setTargetGroupEMail3(AbstractApiController.ObjectReferenceBean targetGroupEMail3) {
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
		private AbstractApiController.ObjectReferenceBean activityGroup;
		private AbstractApiController.ObjectReferenceBean activityCreator;
		private Boolean restrictTargetGroup;
		private AbstractApiController.ObjectReferenceBean targetGroupEMail1;
		private AbstractApiController.ObjectReferenceBean targetGroupEMail2;
		private AbstractApiController.ObjectReferenceBean targetGroupEMail3;		
		private Short templateLocale;
		private Boolean isConfirmed;
		private Boolean ignoreExecutionTimeLimit;
	}

	/**
	 * DoBulkActivityFollowUpParams
	 *
	 */
	public static class DoBulkActivityFollowUpParams {
		/**
		 * @return the activityGroup
		 */
		public AbstractApiController.ObjectReferenceBean getActivityGroup() {
			return activityGroup;
		}
		/**
		 * @param activityGroup the activityGroup to set
		 */
		public void setActivityGroup(AbstractApiController.ObjectReferenceBean activityGroup) {
			this.activityGroup = activityGroup;
		}
		/**
		 * @return the activityProcessState
		 */
		public AbstractApiController.ObjectReferenceBean getActivityProcessState() {
			return activityProcessState;
		}
		/**
		 * @param activityProcessState the activityProcessState to set
		 */
		public void setActivityProcessState(AbstractApiController.ObjectReferenceBean activityProcessState) {
			this.activityProcessState = activityProcessState;
		}
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
		public AbstractApiController.ObjectReferenceBean getAssignTo() {
			return assignTo;
		}
		/**
		 * @param assignTo the assignTo to set
		 */
		public void setAssignTo(AbstractApiController.ObjectReferenceBean assignTo) {
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
		private AbstractApiController.ObjectReferenceBean activityGroup;
		private AbstractApiController.ObjectReferenceBean activityProcessState;
		private AbstractApiController.ObjectReferenceBean assignTo;
		private ActivityFollowUpBean followUp1;
		private ActivityFollowUpBean followUp2;
		private TimerBean timer;
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
	public BulkActivityManagerApiController(
	) {
		com.google.gson.GsonBuilder gsonBuilder = new com.google.gson.GsonBuilder();
		gsonBuilder.setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
		this.gson = gsonBuilder.create();			
	}

	public Object fromJson(
		Reader reader,
		Class<?> clazz
	) throws ServiceException {
		return this.gson.fromJson(reader, clazz);
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractApiController#toJson(java.lang.Object)
	 */
	public String toJson(
		Object object
	) throws ServiceException {
		return this.gson.toJson(object);
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
			new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation())
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
			new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation())
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
		propertySetQuery.name().like(BulkActivityManagerApiController.PROPERTY_SET_NAME_SETTINS + "\\..*");
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
	public BulkActivityManagerApiController.ProcessState getWfProcessState(
    	WfProcessInstance wfProcessInstance
    ) {
		BulkActivityManagerApiController.ProcessState pstate = BulkActivityManagerApiController.ProcessState.NA;
    	if (wfProcessInstance != null) {
    		if (wfProcessInstance.getLastActivityOn() == null) {
    			pstate = BulkActivityManagerApiController.ProcessState.PENDING_NOTYETSTARTED;
    		} else  if (wfProcessInstance.getStartedOn() == null){
    			pstate = BulkActivityManagerApiController.ProcessState.PENDING_STARTED;
    		} else if (wfProcessInstance.isFailed() != null && wfProcessInstance.isFailed().booleanValue()) {
    			pstate = BulkActivityManagerApiController.ProcessState.COMPLETED_FAILURE;
    		} else {
    			pstate = BulkActivityManagerApiController.ProcessState.COMPLETED_SUCCESS;
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
		BulkActivityManagerApiController.ProcessState processState = this.getWfProcessState(wfProcessInstance);
		dataBean.setLastActivityOn(wfProcessInstance.getLastActivityOn());
		dataBean.setStartedOn(wfProcessInstance.getStartedOn());
		dataBean.setCreatedAt(wfProcessInstance.getCreatedAt());
		dataBean.setIsFailed(processState == BulkActivityManagerApiController.ProcessState.COMPLETED_FAILURE);
		{
			List<String> logEntries = new ArrayList<String>();
			try {
				WfActionLogEntryQuery wfActionLogEntryQuery = (WfActionLogEntryQuery)pm.newQuery(WfActionLogEntry.class);
				wfActionLogEntryQuery.orderByModifiedAt().descending();								
				int counter = processState == BulkActivityManagerApiController.ProcessState.COMPLETED_SUCCESS ? 1 : 3;
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
						processState = BulkActivityManagerApiController.ProcessState.PENDING_STARTED;
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
			processState == BulkActivityManagerApiController.ProcessState.COMPLETED_FAILURE ||
			processState == BulkActivityManagerApiController.ProcessState.COMPLETED_SUCCESS
		);
		return dataBean;
	}

	/**
	 * Get activity creation status.
	 * 
	 * @param activityGroup
	 * @param activityProcessState
	 * @return
	 * @throws ServiceException
	 */
	public ActivityCreationStateBean toActivityCreationStateBean(
		ActivityGroup activityGroup,
		ActivityProcessState activityProcessState,
		ActivityCreator activityCreator
	) throws ServiceException {
		ActivityCreationStateBean dataBean = new ActivityCreationStateBean();
		dataBean.setActivityProcessState(this.newObjectReferenceBean(activityProcessState));
		dataBean.setHasFollowUpTransitions(
			!this.getActivityProcessTransitions(activityProcessState.refGetPath(), null).isEmpty()
		);
		dataBean.setCounter(
			this.getCountActivities(
				activityGroup, 
				activityProcessState,
				activityCreator
			)
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
	public AbstractApiController.ObjectReferenceBean getObjectReferenceProperty(
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
		return xri == null
			? null
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
		AbstractApiController.ObjectReferenceBean value
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
		String propertySetPrefix = ":" + BulkActivityManagerApiController.PROPERTY_SET_NAME_SETTINS + "." + locale;
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
	 * Update activity template.
	 * 
	 * @param dataBean
	 * @throws ServiceException
	 */
	public void saveActivityTemplate(
		ActivityTemplateBean dataBean
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ActivityCreator activityCreator = (ActivityCreator)pm.getObjectById(new Path(dataBean.getActivityCreator().getXri()));		
		String propertySetPrefix = ":" + BulkActivityManagerApiController.PROPERTY_SET_NAME_SETTINS + "." + dataBean.getLocale();		
		String templatePlaceHolders = dataBean.getPlaceHolders();
		// Amend place holders
		try {
			Properties placeHolders = new Properties();
			if(templatePlaceHolders != null) {
				placeHolders.load(new StringReader(templatePlaceHolders));
			}
			BulkCreateActivityWorkflow bulkCreateActivityWorkflow = new BulkCreateActivityWorkflow();
			bulkCreateActivityWorkflow.updatePlaceHolders(
				placeHolders,
				dataBean.getDescription()
			);
			bulkCreateActivityWorkflow.updatePlaceHolders(
				placeHolders,
				dataBean.getDescription()
			);
			bulkCreateActivityWorkflow.updatePlaceHolders(
				placeHolders,
				dataBean.getMessageSubject()
			);
			bulkCreateActivityWorkflow.updatePlaceHolders(
				placeHolders,
				dataBean.getMessageBody()
			);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();			
			placeHolders.store(new OutputStreamWriter(bos, "UTF-8"), "");
			dataBean.setPlaceHolders(bos.toString("UTF-8"));
		} catch(Exception ignore) {}
		// Save template
		try {
			pm.currentTransaction().begin();
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!name", 
				dataBean.getName()
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!description", 
				dataBean.getDescription()
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!detailedDescription", 
				dataBean.getDetailedDescription()
			);
			this.setDateTimeProperty(
				activityCreator, 
				propertySetPrefix,
				"!scheduledStart", 
				dataBean.getScheduledStart()
			);
			this.setDateTimeProperty(
				activityCreator, 
				propertySetPrefix,
				"!scheduledEnd", 
				dataBean.getScheduledEnd()
			);
			this.setDateTimeProperty(
				activityCreator, 
				propertySetPrefix,
				"!dueBy", 
				dataBean.getDueBy()
			);
			this.setIntegerProperty(
				activityCreator, 
				propertySetPrefix,
				"!priority", 
				dataBean.getPriority() == null ? null : dataBean.getPriority().intValue()
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!messageSubject", 
				dataBean.getMessageSubject()
			);
			// Split messageBody into pieces of 2048 chars
			{
				List<String> messageBodyParts = Utils.splitString(dataBean.getMessageBody(), 2048);
				for(int i = 0; i < messageBodyParts.size(); i++) {
					try {
						this.setStringProperty(
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
					while(this.getStringProperty(activityCreator, propertySetPrefix, "!messageBody" + idx) != null) {
						org.opencrx.kernel.base.jmi1.Property property = this.findProperty(
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
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!placeHolders", 
				dataBean.getPlaceHolders()
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!excludeNoBulkEMail", 
				Boolean.toString(Boolean.TRUE.equals(dataBean.getExcludeNoBulkEMail()))
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!sender", 
				dataBean.getSenderEMail() == null ? null : dataBean.getSenderEMail().getXri()
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!usage." + Integer.toString(0),
				dataBean.getRecipientEMailAddressUsage1() == null ? null : Short.toString(dataBean.getRecipientEMailAddressUsage1())
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!usage." + Integer.toString(1),
				dataBean.getRecipientEMailAddressUsage2() == null ? null : Short.toString(dataBean.getRecipientEMailAddressUsage2())
			);
			this.setStringProperty(
				activityCreator, 
				propertySetPrefix,
				"!usage." + Integer.toString(2),
				dataBean.getRecipientEMailAddressUsage3() == null ? null : Short.toString(dataBean.getRecipientEMailAddressUsage3())
			);
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
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
			this.toActivityTemplateBean(activityCreator, templateLocale)
		);
		{
			WfProcessInstanceQuery wfProcessInstanceQuery = (WfProcessInstanceQuery)pm.newQuery(WfProcessInstance.class);
			wfProcessInstanceQuery.thereExistsTargetObject().equalTo(activityCreator.getIdentity());
			wfProcessInstanceQuery.process().name().equalTo(Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY);
			wfProcessInstanceQuery.orderByCreatedAt().descending();
			UserHome userHome = (UserHome)pm.getObjectById(this.getApp().getUserHomeIdentityAsPath());
			List<WfProcessInstance> wfProcessInstances = userHome.getWfProcessInstance(wfProcessInstanceQuery);
			if(!wfProcessInstances.isEmpty()) {
				WfProcessInstance wfProcessInstance = wfProcessInstances.iterator().next();
				dataBean.setBulkCreateActivityWfProcess(
					this.toWfProcessInstanceBean(wfProcessInstance)
				);
			}
		}
		{
			List<ActivityCreationStateBean> activityCreationStates = new ArrayList<ActivityCreationStateBean>();
			ActivityProcessStateQuery activityProcessStateQuery = (ActivityProcessStateQuery)pm.newQuery(ActivityProcessState.class);
			activityProcessStateQuery.orderByName().ascending();
			List<ActivityProcessState> activityProcessStates = activityCreator.getActivityType().getControlledBy().<ActivityProcessState>getState(activityProcessStateQuery);
			for(ActivityProcessState activityProcessState: activityProcessStates) {
				activityCreationStates.add(
					this.toActivityCreationStateBean(
						activityGroup,
						activityProcessState,
						activityCreator
					)
				);
			}
			dataBean.setActivityCreationStates(activityCreationStates);
		}
		return dataBean;
	}

	/**
	 * Get data bean.
	 * 
	 * @param path
	 * @return
	 * @throws ServiceException
	 */
	public DataBean getData(
		Path path
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		DataBean dataBean = new DataBean();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path.getPrefix(7));
		if(obj instanceof ActivityGroup) {
			ActivityGroup activityGroup = (ActivityGroup)obj;
			dataBean.setActivityGroup(this.newObjectReferenceBean(activityGroup));
			{
				WfProcessInstanceQuery wfProcessInstanceQuery = (WfProcessInstanceQuery)pm.newQuery(WfProcessInstance.class);
				wfProcessInstanceQuery.thereExistsTargetObject().equalTo(activityGroup.getIdentity());
				wfProcessInstanceQuery.process().name().equalTo(Workflows.WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP);
				wfProcessInstanceQuery.orderByCreatedAt().descending();
				UserHome userHome = (UserHome)pm.getObjectById(this.getApp().getUserHomeIdentityAsPath());
				List<WfProcessInstance> wfProcessInstances = userHome.getWfProcessInstance(wfProcessInstanceQuery);
				if(!wfProcessInstances.isEmpty()) {
					WfProcessInstance wfProcessInstance = wfProcessInstances.iterator().next();
					dataBean.setBulkActivityFollowUpWfProcess(
						this.toWfProcessInstanceBean(wfProcessInstance)
					);
				}
			}
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
			dataBean.setBulkCreateActivities(bulkCreateActivityBeans);
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
			dataBean.setTimers(timerBeans);
		}
		return dataBean;
	}

	/**
	 * Put data.
	 * 
	 * @param path
	 * @param activityTemplateBean
	 * @return
	 * @throws ServiceException
	 */
	public DataBean doSaveActivityTemplate(
		Path path,
		ActivityTemplateBean activityTemplateBean
	) throws ServiceException {
		this.saveActivityTemplate(activityTemplateBean);
		return this.getData(path);
	}

	/**
	 * Get meta data.
	 * 
	 * @param path
	 * @return
	 * @throws ServiceException
	 */
	public MetaInfBean getMetaInf(
		Path path
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		MetaInfBean metaInfBean = new MetaInfBean();
		metaInfBean.setLocale(app.getCurrentLocaleAsString().substring(0, 2));
		metaInfBean.setTimezone(app.getCurrentTimeZone());
		metaInfBean.setLabelOK(app.getTexts().getOkTitle());
		metaInfBean.setLabelCancel(app.getTexts().getCancelTitle());
		metaInfBean.setLabelEdit(app.getTexts().getEditTitle());
		metaInfBean.setLabelSave(app.getTexts().getSaveTitle());
		metaInfBean.setLabelCreatedAt(this.getLabel("org:openmdx:base:Creatable:createdAt"));
		metaInfBean.setLabelLastActivityOn(this.getLabel("org:opencrx:kernel:home1:WfProcessInstance:lastActivityOn"));
		metaInfBean.setLabelStartedOn(this.getLabel("org:opencrx:kernel:home1:WfProcessInstance:startedOn"));
		metaInfBean.setLabelTargetGroupAccounts(this.getLabel("org:opencrx:kernel:activity1:ActivityGroup:targetGroupAccounts"));
		metaInfBean.setLabelActivityTemplate(this.getLabel("org:opencrx:kernel:activity1:Mailing:template"));
		metaInfBean.setLabelAssignTo(this.getLabel("org:opencrx:kernel:activity1:Activity:assignedTo"));
		metaInfBean.setLabelFollowUpTitle(this.getLabel("org:opencrx:kernel:activity1:ActivityFollowUp:title"));
		metaInfBean.setLabelFollowUpText(this.getLabel("org:opencrx:kernel:activity1:ActivityFollowUp:text"));
		metaInfBean.setLabelTransition(this.getLabel("org:opencrx:kernel:activity1:ActivityFollowUp:transition"));
		metaInfBean.setLabelTimerName(this.getLabel("org:opencrx:kernel:home1:Timer:name"));
		metaInfBean.setLabelTimerTriggerAt(this.getLabel("org:opencrx:kernel:home1:Timer:timerStartAt"));
		metaInfBean.setLabelEMailAddress(this.getLabel("org:opencrx:kernel:address1:EMailAddressable:emailAddress"));
		metaInfBean.setLabelEMailAddressUsage(this.getLabel("org:opencrx:kernel:address1:Addressable:usage"));
		metaInfBean.setLabelActivityName(this.getLabel("org:opencrx:kernel:activity1:Activity:name"));
		metaInfBean.setLabelActivityDescription(this.getLabel("org:opencrx:kernel:activity1:Activity:description"));
		metaInfBean.setLabelActivityDetailedDescription(this.getLabel("org:opencrx:kernel:activity1:Activity:detailedDescription"));
		metaInfBean.setLabelActivityScheduledStart(this.getLabel("org:opencrx:kernel:activity1:Activity:scheduledStart"));
		metaInfBean.setLabelActivityScheduledEnd(this.getLabel("org:opencrx:kernel:activity1:Activity:scheduledEnd"));
		metaInfBean.setLabelActivityDueBy(this.getLabel("org:opencrx:kernel:activity1:Activity:dueBy"));
		metaInfBean.setLabelActivityPriority(this.getLabel("org:opencrx:kernel:activity1:Activity:priority"));
		metaInfBean.setLabelPlaceHolders("Place holders");
		metaInfBean.setLabelConfirmDoBulkCreateActivity("Check to confirm activity creation. Uncheck for a preview");
		metaInfBean.setLabelIgnoreExecutionTimeLimit("Ignore execution time limit");
		metaInfBean.setLabelDoBulkActivityFollowUp("Do bulk follow up");
		metaInfBean.setLabelDoBulkCreateActivity("Do bulk create activity");
		metaInfBean.setLabelEditActivityTemplate("Edit activity template");
		metaInfBean.setLabelFollowUp(app.getLabel("org:opencrx:kernel:activity1:ActivityFollowUp"));
		metaInfBean.setLabelRestrictTargetGroup("Restrict target group");
		metaInfBean.setLabelSenderEMail(this.getLabel("org:opencrx:kernel:activity1:EMail:sender"));
		metaInfBean.setLabelMessageSubject(this.getLabel("org:opencrx:kernel:activity1:EMail:messageSubject"));
		metaInfBean.setLabelMessageBody(this.getLabel("org:opencrx:kernel:activity1:EMail:messageBody"));
		metaInfBean.setLabelExcludeNoBulkEMail(this.getLabel("org:opencrx:kernel:account1:Contact:doNotEMail"));
		metaInfBean.setLabelPendingWorkflows(this.getLabel("org:opencrx:kernel:home1:UserHome:wfProcessInstance"));
		if(metaInfBean.getLabelPendingWorkflows().startsWith("»")) {
			metaInfBean.setLabelPendingWorkflows(metaInfBean.getLabelPendingWorkflows().substring(1));
		}
		metaInfBean.setLabelActivityCreators(this.getLabel("org:opencrx:kernel:activity1:ActivityGroup:activityCreator"));
		metaInfBean.setLabelTimers(this.getLabel("org:opencrx:kernel:home1:UserHome:timer"));
		metaInfBean.setLabelWfProcessInstanceName(this.getLabel("org:opencrx:kernel:home1:WfProcessInstance:name"));
		metaInfBean.setLabelWfProcessInstanceName(this.getLabel("org:opencrx:kernel:home1:WfProcessInstance:name"));
		metaInfBean.setLabelWfProcessState(this.getLabel("org:opencrx:kernel:home1:Alert:alertState"));
		metaInfBean.setOptionsEMailAddressUsage(this.getOptions("org:opencrx:kernel:account1:EMailAddress:usage", app.getCurrentLocaleAsIndex(), false));
		metaInfBean.setOptionsPriority(this.getOptions("org:opencrx:kernel:activity1:Activity:priority", app.getCurrentLocaleAsIndex(), false));
		return metaInfBean;
	}

	/**
	 * Get contacts.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> getContacts(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(path);
		ContactQuery contactQuery =  (ContactQuery)pm.newQuery(Contact.class);
		contactQuery.forAllDisabled().isFalse();
		contactQuery.thereExistsFullName().like("(?i).*" + queryBean.getQuery() + ".*");
		contactQuery.orderByFullName().ascending();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		int count = 0;
		for(Iterator<Contact> i = accountSegment.<Contact>getAccount(contactQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
			result.add(this.newObjectReferenceBean(i.next()));
			count++;
			if(count > queryBean.getSize()) {
				break;
			}
		}
		return result;
	}

	/**
	 * Get email addresses.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> getEMailAddresses(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(path);
		EMailAddressQuery emailAddressQuery =  (EMailAddressQuery)pm.newQuery(EMailAddress.class);
		emailAddressQuery.forAllDisabled().isFalse();
		emailAddressQuery.thereExistsEmailAddress().like("(?i).*" + queryBean.getQuery() + ".*");
		emailAddressQuery.orderByEmailAddress().ascending();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
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
	 * Get activity process transitions.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> getActivityProcessTransitions(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		ActivityProcess activityProcess = (ActivityProcess)pm.getObjectById(path.getPrefix(7));
		ActivityProcessState activityProcessState = (ActivityProcessState)pm.getObjectById(path.getPrefix(9));
		if(activityProcess != null && activityProcessState != null) {
			ActivityProcessTransitionQuery activityProcessTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
			activityProcessTransitionQuery.orderByName().ascending();
			activityProcessTransitionQuery.forAllDisabled().isFalse();
			activityProcessTransitionQuery.thereExistsPrevState().equalTo(activityProcessState);
			for(ActivityProcessTransition activityProcessTransition: activityProcess.<ActivityProcessTransition>getTransition(activityProcessTransitionQuery)) {
				result.add(this.newObjectReferenceBean(activityProcessTransition));
			}
		}
		return result;
	}

	/**
	 * Do bulk create activity.
	 * 
	 * @param path
	 * @param params
	 * @return
	 * @throws ServiceException
	 */
	public WfProcessInstanceBean doBulkCreateActivity(
		Path path,
		DoBulkCreateActivityParams params
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		BulkCreateActivityWorkflow.CreationType creationType = null;
		ActivityGroup activityGroup = (ActivityGroup)pm.getObjectById(new Path(params.getActivityGroup().getXri()));
		ActivityCreator activityCreator = (ActivityCreator)pm.getObjectById(new Path(params.getActivityCreator().getXri()));
		ActivityTemplateBean dataBean = this.toActivityTemplateBean(
			activityCreator,
			params.getTemplateLocale()
		);
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
			org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = this.getWorkflowSegment(path);
			WfProcess wfProcess = Workflows.getInstance().findWfProcess(org.opencrx.kernel.backend.Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY, workflowSegment);
			pm.currentTransaction().begin();
			WfProcessInstance wfProcessInstance = Workflows.getInstance().executeWorkflow(
				activityCreator.getName() + " / " + creationType.name() + " / " + currentUserHome.refGetPath().getLastSegment().toClassicRepresentation(),
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
				dataBean.getLocale()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_DEFAULT_PLACEHOLDERS,
				dataBean.getPlaceHolders()
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
				dataBean.getName()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_ACTIVITY_DESCRIPTION, 
				dataBean.getDescription()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_ACTIVITY_DETAILED_DESCRIPTION, 
				dataBean.getDetailedDescription()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance,
				BulkCreateActivityWorkflow.OPTION_ACTIVITY_SCHEDULED_START, 
				dataBean.getScheduledStart()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_ACTIVITY_SCHEDULED_END, 
				dataBean.getScheduledEnd()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_ACTIVITY_DUE_BY, 
				dataBean.getDueBy()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_EMAIL_SENDER, 
				dataBean.getSenderEMail() == null ? null : (AccountAddress)pm.getObjectById(new Path(dataBean.getSenderEMail().getXri()))
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_EMAIL_MESSAGE_SUBJECT, 
				dataBean.getMessageSubject()
			);
			if(dataBean.getMessageBody() != null) {
				int idx = 0;
				for(String messageBodyPart: Utils.splitString(dataBean.getMessageBody(), 2048)) {
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
				dataBean.getRecipientEMailAddressUsage1()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance, 
				BulkCreateActivityWorkflow.OPTION_EMAIL_ADDRESS_USAGE + Integer.toString(1),
				dataBean.getRecipientEMailAddressUsage2()
			);
			WorkflowHelper.addParameter(
				wfProcessInstance,
				BulkCreateActivityWorkflow.OPTION_EMAIL_ADDRESS_USAGE + Integer.toString(2), 
				dataBean.getRecipientEMailAddressUsage3()
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
				dataBean.getExcludeNoBulkEMail()
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
                        new Integer(wfProcessInstance.getStepCounter().intValue() + 1)
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
			return this.toWfProcessInstanceBean(wfProcessInstance);
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e1) {}
			new ServiceException(e).log();
		}
		return null;
	}

	/**
	 * Do bulk activity follow up.
	 * 
	 * @param path
	 * @param params
	 * @return
	 * @throws ServiceException
	 */
	public WfProcessInstanceBean doBulkActivityFollowUp(
		Path path,
		DoBulkActivityFollowUpParams params
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		ActivityGroup activityGroup = (ActivityGroup)pm.getObjectById(new Path(params.getActivityGroup().getXri()));
		ActivityProcessState activityProcessState = (ActivityProcessState)pm.getObjectById(new Path(params.getActivityProcessState().getXri()));
		ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
		activityQuery.thereExistsProcessState().equalTo(activityProcessState);
		activityQuery.forAllDisabled().isFalse();
		List<Activity> activities = activityGroup.getFilteredActivity(activityQuery);
		if(!activities.isEmpty()) {
			Activity activity = activities.iterator().next();
			if(params.getFollowUp1() != null) {
    	    	try {
					List<ActivityFollowUpBean> activityFollowUpBeans = new ArrayList<ActivityFollowUpBean>();
					if(params.getFollowUp1() != null && params.getFollowUp1().getTransition() != null) {
						activityFollowUpBeans.add(params.getFollowUp1());
					}
					if(params.getFollowUp2() != null && params.getFollowUp2().getTransition() != null) {
						activityFollowUpBeans.add(params.getFollowUp2());
					}
    	    		List<String> transitionNames = new ArrayList<String>();
    	    		for(ActivityFollowUpBean activityFollowUpBean: activityFollowUpBeans) {
    	    			transitionNames.add(activityFollowUpBean.getTransition().getTitle());
					}
			    	UserHome userHome = (UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());		  
					org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = this.getWorkflowSegment(path);
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
							activityGroup.getName() + " / " + transitionNames + " / " + userHome.refGetPath().getLastSegment().toClassicRepresentation() 
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
							activityGroup.getName() + " / " + transitionNames + " / " + userHome.refGetPath().getLastSegment().toClassicRepresentation(),
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
						if(params.getAssignTo() != null) {
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
						if(params.getAssignTo() != null) {
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
						return this.toWfProcessInstanceBean((WfProcessInstance)executionTarget);
					} else {
						return new WfProcessInstanceBean();
					}
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e1) {}
					new ServiceException(e).log();
				}
    	    }
		}
		return null;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	public static final String PROPERTY_SET_NAME_SETTINS = "BulkCreateActivityWizardSettings";

	private final com.google.gson.Gson gson;
	
}
