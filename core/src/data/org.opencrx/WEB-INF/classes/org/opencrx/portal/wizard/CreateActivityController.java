/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateActivityController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2015-2018, CRIXP Corp., Switzerland
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
package org.opencrx.portal.wizard;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.jdo.PersistenceManager;
import javax.servlet.http.HttpServletRequest;

import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery;
import org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery;
import org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.activity1.jmi1.ActivityLinkTo;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.NewActivityParams;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.attribute.DateValue;
import org.w3c.spi2.Datatypes;

/**
 * CreateActivityController
 *
 */
@SuppressWarnings("deprecation")
@ManagedBean
@SessionScoped
public class CreateActivityController extends AbstractWizardController {

	public static class DataBean {
		
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
		 * @return the activityCreator
		 */
		public ObjectReferenceBean getActivityCreator() {
			return activityCreator;
		}
		/**
		 * @param activityCreator the activityCreator to set
		 */
		public void setActivityCreator(ObjectReferenceBean activityCreator) {
			this.activityCreator = activityCreator;
		}
		/**
		 * @return the reportingAccount
		 */
		public ObjectReferenceBean getReportingAccount() {
			return reportingAccount;
		}
		/**
		 * @param reportingAccount the reportingAccount to set
		 */
		public void setReportingAccount(ObjectReferenceBean reportingAccount) {
			this.reportingAccount = reportingAccount;
		}
		/**
		 * @return the reportingContact
		 */
		public ObjectReferenceBean getReportingContact() {
			return reportingContact;
		}
		/**
		 * @param reportingContact the reportingContact to set
		 */
		public void setReportingContact(ObjectReferenceBean reportingContact) {
			this.reportingContact = reportingContact;
		}
		/**
		 * @return the assignedTo
		 */
		public ObjectReferenceBean getAssignedTo() {
			return assignedTo;
		}
		/**
		 * @param assignedTo the assignedTo to set
		 */
		public void setAssignedTo(ObjectReferenceBean assignedTo) {
			this.assignedTo = assignedTo;
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
		 * @return the misc1
		 */
		public String getMisc1() {
			return misc1;
		}
		/**
		 * @param misc1 the misc1 to set
		 */
		public void setMisc1(String misc1) {
			this.misc1 = misc1;
		}
		/**
		 * @return the misc2
		 */
		public String getMisc2() {
			return misc2;
		}
		/**
		 * @param misc2 the misc2 to set
		 */
		public void setMisc2(String misc2) {
			this.misc2 = misc2;
		}
		/**
		 * @return the misc3
		 */
		public String getMisc3() {
			return misc3;
		}
		/**
		 * @param misc3 the misc3 to set
		 */
		public void setMisc3(String misc3) {
			this.misc3 = misc3;
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
		 * @return the activityGroup1
		 */
		public ObjectReferenceBean getActivityGroup1() {
			return activityGroup1;
		}
		/**
		 * @param activityGroup1 the activityGroup1 to set
		 */
		public void setActivityGroup1(ObjectReferenceBean activityGroup1) {
			this.activityGroup1 = activityGroup1;
		}
		/**
		 * @return the activityGroup2
		 */
		public ObjectReferenceBean getActivityGroup2() {
			return activityGroup2;
		}
		/**
		 * @param activityGroup2 the activityGroup2 to set
		 */
		public void setActivityGroup2(ObjectReferenceBean activityGroup2) {
			this.activityGroup2 = activityGroup2;
		}
		/**
		 * @return the activityGroup3
		 */
		public ObjectReferenceBean getActivityGroup3() {
			return activityGroup3;
		}
		/**
		 * @param activityGroup3 the activityGroup3 to set
		 */
		public void setActivityGroup3(ObjectReferenceBean activityGroup3) {
			this.activityGroup3 = activityGroup3;
		}
		/**
		 * @return the activity
		 */
		public ObjectReferenceBean getActivity() {
			return activity;
		}
		/**
		 * @param activity the activity to set
		 */
		public void setActivity(ObjectReferenceBean activity) {
			this.activity = activity;
		}
		/**
		 * @return the errors
		 */
		public List<String> getErrors() {
			return errors;
		}
		/**
		 * @param errors the errors to set
		 */
		public void setErrors(List<String> errors) {
			this.errors = errors;
		}
		private List<ObjectReferenceBean> findActivityCreatorResult;
		/**
		 * @return the findActivityCreatorResult
		 */
		public List<ObjectReferenceBean> getFindActivityCreatorResult() {
			return findActivityCreatorResult;
		}
		/**
		 * @return the findReportingAccountResult
		 */
		public List<ObjectReferenceBean> getFindReportingAccountResult() {
			return findReportingAccountResult;
		}
		/**
		 * @return the findReportingContactResult
		 */
		public List<ObjectReferenceBean> getFindReportingContactResult() {
			return findReportingContactResult;
		}
		/**
		 * @return the findAssignedToResult
		 */
		public List<ObjectReferenceBean> getFindAssignedToResult() {
			return findAssignedToResult;
		}
		/**
		 * @return the findActivityGroup1Result
		 */
		public List<ObjectReferenceBean> getFindActivityGroup1Result() {
			return findActivityGroup1Result;
		}
		/**
		 * @return the findActivityGroup2Result
		 */
		public List<ObjectReferenceBean> getFindActivityGroup2Result() {
			return findActivityGroup2Result;
		}
		/**
		 * @return the findActivityGroup3Result
		 */
		public List<ObjectReferenceBean> getFindActivityGroup3Result() {
			return findActivityGroup3Result;
		}
		/**
		 * @param findActivityCreatorResult the findActivityCreatorResult to set
		 */
		public void setFindActivityCreatorResult(List<ObjectReferenceBean> findActivityCreatorResult) {
			this.findActivityCreatorResult = findActivityCreatorResult;
		}
		/**
		 * @param findReportingAccountResult the findReportingAccountResult to set
		 */
		public void setFindReportingAccountResult(List<ObjectReferenceBean> findReportingAccountResult) {
			this.findReportingAccountResult = findReportingAccountResult;
		}
		/**
		 * @param findReportingContactResult the findReportingContactResult to set
		 */
		public void setFindReportingContactResult(List<ObjectReferenceBean> findReportingContactResult) {
			this.findReportingContactResult = findReportingContactResult;
		}
		/**
		 * @param findAssignedToResult the findAssignedToResult to set
		 */
		public void setFindAssignedToResult(List<ObjectReferenceBean> findAssignedToResult) {
			this.findAssignedToResult = findAssignedToResult;
		}
		/**
		 * @param findActivityGroup1Result the findActivityGroup1Result to set
		 */
		public void setFindActivityGroup1Result(List<ObjectReferenceBean> findActivityGroup1Result) {
			this.findActivityGroup1Result = findActivityGroup1Result;
		}
		/**
		 * @param findActivityGroup2Result the findActivityGroup2Result to set
		 */
		public void setFindActivityGroup2Result(List<ObjectReferenceBean> findActivityGroup2Result) {
			this.findActivityGroup2Result = findActivityGroup2Result;
		}
		/**
		 * @param findActivityGroup3Result the findActivityGroup3Result to set
		 */
		public void setFindActivityGroup3Result(List<ObjectReferenceBean> findActivityGroup3Result) {
			this.findActivityGroup3Result = findActivityGroup3Result;
		}
		
		private String name;
		private ObjectReferenceBean activityCreator;
		private ObjectReferenceBean reportingAccount;
		private List<ObjectReferenceBean> findReportingAccountResult;
		private ObjectReferenceBean reportingContact;
		private List<ObjectReferenceBean> findReportingContactResult;
		private ObjectReferenceBean assignedTo;
		private List<ObjectReferenceBean> findAssignedToResult;
		private Short priority;
		private Date dueBy;
		private Date scheduledStart;
		private Date scheduledEnd;
		private String misc1;
		private String misc2;
		private String misc3;
		private String description;
		private String detailedDescription;
		private ObjectReferenceBean activityGroup1;
		private List<ObjectReferenceBean> findActivityGroup1Result;		
		private ObjectReferenceBean activityGroup2;
		private List<ObjectReferenceBean> findActivityGroup2Result;
		private ObjectReferenceBean activityGroup3;
		private List<ObjectReferenceBean> findActivityGroup3Result;
		private ObjectReferenceBean activity;
		private List<String> errors;
	}

	public static class MetaInfBean {
		
		/**
		 * @return the labelName
		 */
		public String getLabelName() {
			return labelName;
		}
		/**
		 * @param labelName the labelName to set
		 */
		public void setLabelName(String labelName) {
			this.labelName = labelName;
		}
		/**
		 * @return the labelActivityCreator
		 */
		public String getLabelActivityCreator() {
			return labelActivityCreator;
		}
		/**
		 * @param labelActivityCreator the labelActivityCreator to set
		 */
		public void setLabelActivityCreator(String labelActivityCreator) {
			this.labelActivityCreator = labelActivityCreator;
		}
		/**
		 * @return the labelReportingAccount
		 */
		public String getLabelReportingAccount() {
			return labelReportingAccount;
		}
		/**
		 * @param labelReportingAccount the labelReportingAccount to set
		 */
		public void setLabelReportingAccount(String labelReportingAccount) {
			this.labelReportingAccount = labelReportingAccount;
		}
		/**
		 * @return the labelReportingContact
		 */
		public String getLabelReportingContact() {
			return labelReportingContact;
		}
		/**
		 * @param labelReportingContact the labelReportingContact to set
		 */
		public void setLabelReportingContact(String labelReportingContact) {
			this.labelReportingContact = labelReportingContact;
		}
		/**
		 * @return the labelAssignedTo
		 */
		public String getLabelAssignedTo() {
			return labelAssignedTo;
		}
		/**
		 * @param labelAssignedTo the labelAssignedTo to set
		 */
		public void setLabelAssignedTo(String labelAssignedTo) {
			this.labelAssignedTo = labelAssignedTo;
		}
		/**
		 * @return the labelPriority
		 */
		public String getLabelPriority() {
			return labelPriority;
		}
		/**
		 * @param labelPriority the labelPriority to set
		 */
		public void setLabelPriority(String labelPriority) {
			this.labelPriority = labelPriority;
		}
		/**
		 * @return the labelDueBy
		 */
		public String getLabelDueBy() {
			return labelDueBy;
		}
		/**
		 * @param labelDueBy the labelDueBy to set
		 */
		public void setLabelDueBy(String labelDueBy) {
			this.labelDueBy = labelDueBy;
		}
		/**
		 * @return the labelScheduledStart
		 */
		public String getLabelScheduledStart() {
			return labelScheduledStart;
		}
		/**
		 * @param labelScheduledStart the labelScheduledStart to set
		 */
		public void setLabelScheduledStart(String labelScheduledStart) {
			this.labelScheduledStart = labelScheduledStart;
		}
		/**
		 * @return the labelScheduledEnd
		 */
		public String getLabelScheduledEnd() {
			return labelScheduledEnd;
		}
		/**
		 * @param labelScheduledEnd the labelScheduledEnd to set
		 */
		public void setLabelScheduledEnd(String labelScheduledEnd) {
			this.labelScheduledEnd = labelScheduledEnd;
		}
		/**
		 * @return the labelMisc1
		 */
		public String getLabelMisc1() {
			return labelMisc1;
		}
		/**
		 * @param labelMisc1 the labelMisc1 to set
		 */
		public void setLabelMisc1(String labelMisc1) {
			this.labelMisc1 = labelMisc1;
		}
		/**
		 * @return the labelMisc2
		 */
		public String getLabelMisc2() {
			return labelMisc2;
		}
		/**
		 * @param labelMisc2 the labelMisc2 to set
		 */
		public void setLabelMisc2(String labelMisc2) {
			this.labelMisc2 = labelMisc2;
		}
		/**
		 * @return the labelMisc3
		 */
		public String getLabelMisc3() {
			return labelMisc3;
		}
		/**
		 * @param labelMisc3 the labelMisc3 to set
		 */
		public void setLabelMisc3(String labelMisc3) {
			this.labelMisc3 = labelMisc3;
		}
		/**
		 * @return the labelDescription
		 */
		public String getLabelDescription() {
			return labelDescription;
		}
		/**
		 * @param labelDescription the labelDescription to set
		 */
		public void setLabelDescription(String labelDescription) {
			this.labelDescription = labelDescription;
		}
		/**
		 * @return the labelDetailedDescription
		 */
		public String getLabelDetailedDescription() {
			return labelDetailedDescription;
		}
		/**
		 * @param labelDetailedDescription the labelDetailedDescription to set
		 */
		public void setLabelDetailedDescription(String labelDetailedDescription) {
			this.labelDetailedDescription = labelDetailedDescription;
		}
		/**
		 * @return the labelActivityGroup
		 */
		public String getLabelActivityGroup() {
			return labelActivityGroup;
		}
		/**
		 * @param labelActivityGroup the labelActivityGroup to set
		 */
		public void setLabelActivityGroup(String labelActivityGroup) {
			this.labelActivityGroup = labelActivityGroup;
		}
		
		/**
		 * @return the optionsPriority
		 */
		public List<OptionBean> getOptionsPriority() {
			return optionsPriority;
		}
		
		/**
		 * @param optionsPriority the optionsPriority to set
		 */
		public void setOptionsPriority(List<OptionBean> optionsPriority) {
			this.optionsPriority = optionsPriority;
		}

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
		 * @return the labelFieldGroupActivity
		 */
		public String getLabelFieldGroupActivity() {
			return labelFieldGroupActivity;
		}
		/**
		 * @param labelFieldGroupActivity the labelFieldGroupActivity to set
		 */
		public void setLabelFieldGroupActivity(String labelFieldGroupActivity) {
			this.labelFieldGroupActivity = labelFieldGroupActivity;
		}
		/**
		 * @return the labelFieldGroupDetails
		 */
		public String getLabelFieldGroupDetails() {
			return labelFieldGroupDetails;
		}
		/**
		 * @param labelFieldGroupDetails the labelFieldGroupDetails to set
		 */
		public void setLabelFieldGroupDetails(String labelFieldGroupDetails) {
			this.labelFieldGroupDetails = labelFieldGroupDetails;
		}
		/**
		 * @return the dateFormatDueBy
		 */
		public String getDateFormatDueBy() {
			return dateFormatDueBy;
		}
		/**
		 * @param dateFormatDueBy the dateFormatDueBy to set
		 */
		public void setDateFormatDueBy(String dateFormatDueBy) {
			this.dateFormatDueBy = dateFormatDueBy;
		}
		/**
		 * @return the calendarFormatDueBy
		 */
		public String getCalendarFormatDueBy() {
			return calendarFormatDueBy;
		}
		/**
		 * @param calendarFormatDueBy the calendarFormatDueBy to set
		 */
		public void setCalendarFormatDueBy(String calendarFormatDueBy) {
			this.calendarFormatDueBy = calendarFormatDueBy;
		}
		/**
		 * @return the dateFormatScheduledStart
		 */
		public String getDateFormatScheduledStart() {
			return dateFormatScheduledStart;
		}
		/**
		 * @param dateFormatScheduledStart the dateFormatScheduledStart to set
		 */
		public void setDateFormatScheduledStart(String dateFormatScheduledStart) {
			this.dateFormatScheduledStart = dateFormatScheduledStart;
		}
		/**
		 * @return the calendarFormatScheduledStart
		 */
		public String getCalendarFormatScheduledStart() {
			return calendarFormatScheduledStart;
		}
		/**
		 * @param calendarFormatScheduledStart the calendarFormatScheduledStart to set
		 */
		public void setCalendarFormatScheduledStart(String calendarFormatScheduledStart) {
			this.calendarFormatScheduledStart = calendarFormatScheduledStart;
		}
		/**
		 * @return the dateFormatScheduledEnd
		 */
		public String getDateFormatScheduledEnd() {
			return dateFormatScheduledEnd;
		}
		/**
		 * @param dateFormatScheduledEnd the dateFormatScheduledEnd to set
		 */
		public void setDateFormatScheduledEnd(String dateFormatScheduledEnd) {
			this.dateFormatScheduledEnd = dateFormatScheduledEnd;
		}
		/**
		 * @return the calendarFormatScheduledEnd
		 */
		public String getCalendarFormatScheduledEnd() {
			return calendarFormatScheduledEnd;
		}
		/**
		 * @param calendarFormatScheduledEnd the calendarFormatScheduledEnd to set
		 */
		public void setCalendarFormatScheduledEnd(String calendarFormatScheduledEnd) {
			this.calendarFormatScheduledEnd = calendarFormatScheduledEnd;
		}

		private String locale;
		private String timezone;
		private String labelName;
		private String labelActivityCreator;
		private String labelReportingAccount;
		private String labelReportingContact;
		private String labelAssignedTo;
		private String labelPriority;
		private String labelDueBy;
		private String dateFormatDueBy;
		private String calendarFormatDueBy;
		private String labelScheduledStart;
		private String dateFormatScheduledStart;
		private String calendarFormatScheduledStart;
		private String labelScheduledEnd;
		private String dateFormatScheduledEnd;
		private String calendarFormatScheduledEnd;
		private String labelMisc1;
		private String labelMisc2;
		private String labelMisc3;
		private String labelDescription;
		private String labelDetailedDescription;
		private String labelActivityGroup;
		private String labelFieldGroupActivity;
		private String labelFieldGroupDetails;
		private List<OptionBean> optionsPriority;
	}
	
	public CreateActivityController(
	) {
	}

	public void init(
	) {
		try {
			if(!FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
				super.init(
					(HttpServletRequest)FacesContext.getCurrentInstance().getExternalContext().getRequest(),
					"UTF-8",
					true, // assertRequestId
					true // assertObjectXri
				);
				this.data = this.newData();
				this.metaInf = this.newMetaInf();
				this.doRefresh(null);
			}
		} catch(Exception e) {
			new ServiceException(e).log();
		}
	}

	public org.opencrx.kernel.activity1.jmi1.Segment getActivitySegment(
		Path path
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		return (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
			new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation())
		);
	}

	public org.opencrx.kernel.account1.jmi1.Segment getAccountSegment(
		Path path
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		return (org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
			new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation())
		);
	}

	public DataBean newData(
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		Path path = this.getObjectIdentity();
		DataBean data = new DataBean();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path);
		if(obj instanceof ActivityCreator) {
			ObjectReferenceBean activityCreatorBean = this.newObjectReferenceBean(obj);
			data.setActivityCreator(activityCreatorBean);
			data.setFindActivityCreatorResult(
				Arrays.asList(activityCreatorBean)
			);
		} else { 
			data.setActivityCreator(new ObjectReferenceBean());
		}
		data.setAssignedTo(new ObjectReferenceBean());
		data.setReportingContact(new ObjectReferenceBean());
		data.setReportingAccount(new ObjectReferenceBean());
		data.setActivityGroup1(new ObjectReferenceBean());
		data.setActivityGroup2(new ObjectReferenceBean());
		data.setActivityGroup3(new ObjectReferenceBean());
		data.setPriority( Activities.Priority.NORMAL.getValue());
		return data;
	}

    /**
     * Create error message.
     * 
     * @param message
     * @param parameters
     * @return
     */
	public String createErrorMessage(
		String message,
		String[] parameters
	) {
		String preparedMessage = "";
		int i = 0;
		while(i < message.length()) {
			if((i <= message.length()-4) && "${".equals(message.substring(i,i+2))) {
				short index = new Short(message.substring(i+2, i+3)).shortValue();
				try {
					preparedMessage += parameters[index];
				} catch(Exception e) {}
				i += 4;
			} else {
				preparedMessage += message.charAt(i);
				i++;
			}
		}
		return preparedMessage;
	}
	
	/**
	 * Get meta data.
	 * 
	 * @param path
	 * @return
	 * @throws ServiceException
	 */
	public MetaInfBean newMetaInf(
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		MetaInfBean metaInfBean = new MetaInfBean();
		metaInfBean.setLocale(app.getCurrentLocaleAsString().substring(0, 2));
		metaInfBean.setTimezone(app.getCurrentTimeZone());
		metaInfBean.setLabelName(this.getLabel("org:opencrx:kernel:activity1:Activity:name"));
		metaInfBean.setLabelActivityCreator(this.getLabel("org:opencrx:kernel:activity1:Activity:lastAppliedCreator"));
		metaInfBean.setLabelReportingAccount(this.getLabel("org:opencrx:kernel:activity1:Activity:reportingAccount"));
		metaInfBean.setLabelReportingContact(this.getLabel("org:opencrx:kernel:activity1:Activity:reportingContact"));
		metaInfBean.setLabelAssignedTo(this.getLabel("org:opencrx:kernel:activity1:Activity:assignedTo"));
		metaInfBean.setLabelPriority(this.getLabel("org:opencrx:kernel:activity1:Activity:priority"));
		SimpleDateFormat dateFormatDueBy = DateValue.getLocalizedDateTimeFormatter("org:opencrx:kernel:activity1:Activity:dueBy", true, app);
		metaInfBean.setLabelDueBy(this.getLabel("org:opencrx:kernel:activity1:Activity:dueBy"));
		metaInfBean.setDateFormatDueBy(dateFormatDueBy.toPattern());
		metaInfBean.setCalendarFormatDueBy(DateValue.getCalendarFormat(dateFormatDueBy));
		SimpleDateFormat dateFormatScheduledStart = DateValue.getLocalizedDateTimeFormatter("org:opencrx:kernel:activity1:Activity:scheduledStart", true, app);
		metaInfBean.setLabelScheduledStart(this.getLabel("org:opencrx:kernel:activity1:Activity:scheduledStart"));
		metaInfBean.setDateFormatScheduledStart(dateFormatScheduledStart.toPattern());
		metaInfBean.setCalendarFormatScheduledStart(DateValue.getCalendarFormat(dateFormatScheduledStart));
		SimpleDateFormat dateFormatScheduledEnd = DateValue.getLocalizedDateTimeFormatter("org:opencrx:kernel:activity1:Activity:scheduledEnd", true, app);
		metaInfBean.setLabelScheduledEnd(this.getLabel("org:opencrx:kernel:activity1:Activity:scheduledEnd"));
		metaInfBean.setDateFormatScheduledEnd(dateFormatScheduledEnd.toPattern());
		metaInfBean.setCalendarFormatScheduledEnd(DateValue.getCalendarFormat(dateFormatScheduledEnd));
		try {
			metaInfBean.setLabelMisc1(this.getLabel("org:opencrx:kernel:activity1:Activity:misc1"));
			metaInfBean.setLabelMisc2(this.getLabel("org:opencrx:kernel:activity1:Activity:misc2"));
			metaInfBean.setLabelMisc3(this.getLabel("org:opencrx:kernel:activity1:Activity:misc3"));
		} catch(Exception ignore) {}
		metaInfBean.setLabelDescription(this.getLabel("org:opencrx:kernel:activity1:Activity:description"));
		metaInfBean.setLabelDetailedDescription(this.getLabel("org:opencrx:kernel:activity1:Activity:detailedDescription"));
		metaInfBean.setLabelActivityGroup(this.getLabel("org:opencrx:kernel:activity1:ActivityGroupAssignment:activityGroup"));
		metaInfBean.setLabelFieldGroupActivity(app.getLabel("org:opencrx:kernel:activity1:Activity"));
		metaInfBean.setLabelFieldGroupDetails(this.getLabel("Tab:10"));
		metaInfBean.setOptionsPriority(this.getOptions("org:opencrx:kernel:activity1:Activity:priority", app.getCurrentLocaleAsIndex(), false));
		return metaInfBean;
	}

	/**
	 * Find activity creators.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> findActivityCreators(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		if(queryBean.getQuery() != null && !queryBean.getQuery().isEmpty()) {
			org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(path);
			org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery activityCreatorQuery = 
				(org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery)pm.newQuery(ActivityCreator.class);
			activityCreatorQuery.forAllDisabled().isFalse();
			activityCreatorQuery.name().like("(?i).*" + queryBean.getQuery() + ".*");
			activityCreatorQuery.orderByName().ascending();
			int count = 0;
			for(Iterator<ActivityCreator> i = activitySegment.getActivityCreator(activityCreatorQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
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
	 * Find accounts.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> findAccounts(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		if(queryBean.getQuery() != null && !queryBean.getQuery().isEmpty()) {
			org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(path);
			org.opencrx.kernel.account1.cci2.AccountQuery accountQuery = 
				(org.opencrx.kernel.account1.cci2.AccountQuery)pm.newQuery(Account.class);
			accountQuery.forAllDisabled().isFalse();
			accountQuery.thereExistsFullName().like("(?i).*" + queryBean.getQuery() + ".*");
			accountQuery.orderByFullName().ascending();
			int count = 0;
			for(Iterator<Account> i = accountSegment.getAccount(accountQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
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
	 * Get contacts.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> findContacts(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		if(queryBean.getQuery() != null && !queryBean.getQuery().isEmpty()) {
			org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(path);
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
	 * Get activity groups.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> findActivityGroups(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		if(queryBean.getQuery() != null && !queryBean.getQuery().isEmpty()) {
			org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(path);
			int count = 0;
			// Trackers
			{
				ActivityTrackerQuery activityGroupQuery = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
				activityGroupQuery.forAllDisabled().isFalse();
				activityGroupQuery.name().like("(?i).*" + queryBean.getQuery() + ".*");
				activityGroupQuery.orderByName().ascending();
				for(Iterator<ActivityTracker> i = activitySegment.getActivityTracker(activityGroupQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
					result.add(this.newObjectReferenceBean(i.next()));
					count++;
					if(count > queryBean.getSize()) {
						break;
					}
				}
			}
			// Milestones
			{
				ActivityMilestoneQuery activityGroupQuery = (ActivityMilestoneQuery)pm.newQuery(ActivityMilestone.class);
				activityGroupQuery.forAllDisabled().isFalse();
				activityGroupQuery.name().like("(?i).*" + queryBean.getQuery() + ".*");
				activityGroupQuery.orderByName().ascending();
				for(Iterator<ActivityMilestone> i = activitySegment.getActivityMilestone(activityGroupQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
					result.add(this.newObjectReferenceBean(i.next()));
					count++;
					if(count > queryBean.getSize()) {
						break;
					}
				}
			}
			// Categories
			{
				ActivityCategoryQuery activityGroupQuery = (ActivityCategoryQuery)pm.newQuery(ActivityCategory.class);
				activityGroupQuery.forAllDisabled().isFalse();
				activityGroupQuery.name().like("(?i).*" + queryBean.getQuery() + ".*");
				activityGroupQuery.orderByName().ascending();
				for(Iterator<ActivityCategory> i = activitySegment.getActivityCategory(activityGroupQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
					result.add(this.newObjectReferenceBean(i.next()));
					count++;
					if(count > queryBean.getSize()) {
						break;
					}
				}
			}
		}
		return result;
	}

	/**
	 * doCreate action.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public void doCreate(
   		javax.faces.event.AjaxBehaviorEvent event
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		Path path = this.getObjectIdentity();
		DataBean dataBean = this.getData();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path);
		boolean allMandatoryFieldsSet = true;
		List<String> errors = new ArrayList<String>();
		// Check name
		try {
			if(dataBean.getName() == null || dataBean.getName().isEmpty()) {
				allMandatoryFieldsSet = false;
				errors.add(
					this.createErrorMessage(
						app.getTexts().getErrorTextMandatoryField(),
		            	new String[]{this.getLabel("org:opencrx:kernel:activity1:Activity:name")}
					)
	          	);
			}
		} catch(Exception ignore) {}
		// Check ActivityCreator
		try {
			if(dataBean.getActivityCreator() == null || dataBean.getActivityCreator().getXri() == null || dataBean.getActivityCreator().getXri().isEmpty()) {
				allMandatoryFieldsSet = false;
				errors.add(
					this.createErrorMessage(
						app.getTexts().getErrorTextMandatoryField(),
		            	new String[]{this.getLabel("org:opencrx:kernel:activity1:Activity:lastAppliedCreator")}
		         	 )
				);
			}
		} catch(Exception ignore) {}
		if(allMandatoryFieldsSet) {
			try {
				String name = dataBean.getName();
				Contact reportingContact = null;
				try {
					reportingContact = (Contact)pm.getObjectById(
						new Path(dataBean.getReportingContact().getXri())
					);
				} catch (Exception ignore) {}
				Account reportingAccount = null;
				try {
					reportingAccount = (Account)pm.getObjectById(
						new Path(dataBean.getReportingAccount().getXri())
					);
				} catch (Exception ignore) {}
				Contact assignedTo = null;
				try {
					assignedTo = (Contact)pm.getObjectById(
						new Path(dataBean.getAssignedTo().getXri())
					);
				} catch (Exception ignore) {}
				ActivityCreator activityCreator = null;
				try {
					activityCreator = (ActivityCreator)pm.getObjectById(
						new Path(dataBean.getActivityCreator().getXri())
					);
				} catch(Exception ignore) {}
				short priority = dataBean.getPriority() == null 
					? Activities.Priority.NORMAL.getValue() 
					: dataBean.getPriority();
				Date dueBy = dataBean.getDueBy();
				Date scheduledStart = dataBean.getScheduledStart();
				Date scheduledEnd = dataBean.getScheduledEnd();
				String misc1 = dataBean.getMisc1();
				String misc2 = dataBean.getMisc2();
				String misc3 = dataBean.getMisc3();
				String description = dataBean.getDescription();
				String detailedDescription = dataBean.getDetailedDescription();
				if(
					(name != null) &&
					(name.trim().length() > 0) &&
					(dataBean.getActivityCreator() != null)
				) {
					org.opencrx.kernel.activity1.jmi1.NewActivityParams params = org.w3c.spi2.Structures.create(
						NewActivityParams.class,
						Datatypes.member(NewActivityParams.Member.description, description),
						Datatypes.member(NewActivityParams.Member.detailedDescription, detailedDescription),
						Datatypes.member(NewActivityParams.Member.dueBy, dueBy),
						Datatypes.member(NewActivityParams.Member.name, name),
						Datatypes.member(NewActivityParams.Member.priority, priority),
						Datatypes.member(NewActivityParams.Member.reportingContact, reportingContact),
						Datatypes.member(NewActivityParams.Member.scheduledEnd, scheduledEnd),
						Datatypes.member(NewActivityParams.Member.scheduledStart, scheduledStart),
						Datatypes.member(NewActivityParams.Member.icalType, ICalendar.ICAL_TYPE_NA)   							
					);
					pm.currentTransaction().begin();
					org.opencrx.kernel.activity1.jmi1.NewActivityResult result = activityCreator.newActivity(params);
					pm.currentTransaction().commit();
					Activity newActivity = (Activity)pm.getObjectById(result.getActivity().refGetPath());
					pm.currentTransaction().begin();
					newActivity.setMisc1(misc1);
					newActivity.setMisc2(misc2);
					newActivity.setMisc3(misc3);
					newActivity.setReportingAccount(reportingAccount);
					if (assignedTo != null) {
						newActivity.setAssignedTo(assignedTo);
					}
					pm.currentTransaction().commit();
					// Create new ActivityLinkTo
	                if(obj instanceof Activity) {
	                	try {
							pm.currentTransaction().begin();
							ActivityLinkTo activityLinkTo = pm.newInstance(ActivityLinkTo.class);
							activityLinkTo.setLinkTo(newActivity);
							activityLinkTo.setName(name);
							activityLinkTo.setActivityLinkType(Activities.ActivityLinkType.RELATES_TO.getValue());
							((Activity)obj).addActivityLinkTo(
								org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
								activityLinkTo
							);
							pm.currentTransaction().commit();
						} catch (Exception e) {
							try {
								pm.currentTransaction().rollback();
							} catch (Exception er) {}
						}
	                }
	                List<ActivityGroup> activityGroups = new ArrayList<ActivityGroup>();
					try {
						activityGroups.add(
							(ActivityGroup)pm.getObjectById(
								new Path(dataBean.getActivityGroup1().getXri())
							)
						);
					} catch(Exception ignore) {}
					try {
						activityGroups.add(
							(ActivityGroup)pm.getObjectById(
								new Path(dataBean.getActivityGroup2().getXri())
							)
						);
					} catch(Exception ignore) {}
					try {
						activityGroups.add(
							(ActivityGroup)pm.getObjectById(
								new Path(dataBean.getActivityGroup3().getXri())
							)
						);
					} catch(Exception ignore) {}
					for(ActivityGroup activityGroup: activityGroups) {
    					// Verify that this group has not been added already
    					boolean alreadyAssigned = false;
						for(ActivityGroupAssignment assignment: newActivity.<ActivityGroupAssignment>getAssignedGroup()) {
							try {
								if (
									assignment.getActivityGroup() != null &&
									assignment.getActivityGroup().refGetPath().equals(activityGroup.refGetPath())
								) {
									alreadyAssigned = true;
									break;
								}
							} catch (Exception e) {
								new ServiceException(e).log();
							}
						}
						if(!alreadyAssigned) {
							try {
								pm.currentTransaction().begin();
								ActivityGroupAssignment agass = pm.newInstance(ActivityGroupAssignment.class);
								agass.setActivityGroup(activityGroup);
								newActivity.addAssignedGroup(
							        false,
							        Utils.getUidAsString(),
							        agass
							    );
								pm.currentTransaction().commit();
							} catch (Exception e) {
								try {
									pm.currentTransaction().rollback();
								} catch (Exception er) {}
							}
						}
	                }
	                dataBean.setActivity(this.newObjectReferenceBean(result.getActivity()));
				}
			} catch (Exception e) {
				new ServiceException(e).log();
				try {
					Throwable root = e;  
					while (root.getCause() != null) {  
					    root = root.getCause();  
					}
					errors.add(root.toString());
				} catch (Exception e0) {}
				try {
					pm.currentTransaction().rollback();
				} catch (Exception er) {}
			}
		}
		if(errors.isEmpty()) {
	   		try {
	   			Action exitAction = new ObjectReference(
	   				(Activity)pm.getObjectById(new Path(dataBean.getActivity().getXri())),
	   				this.getApp()
	   			).getSelectObjectAction();
	   			ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
	   			externalContext.redirect(
	   				externalContext.getRequestContextPath() + "/" + exitAction.getEncodedHRef()
	   			);
	   		} catch(Exception e) {
	   			throw new ServiceException(e);
	   		}
		} else {
			dataBean.setErrors(errors);
		}
	}

   	/**
   	 * doRefresh action.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void doRefresh(
   		javax.faces.event.AjaxBehaviorEvent event   		
   	) throws ServiceException {
   	}
   	
   	/**
   	 * doCancel action.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void doCancel(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		try {
   			Action exitAction = new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction();
   			ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
   			externalContext.redirect(
   				externalContext.getRequestContextPath() + "/" + exitAction.getEncodedHRef()
   			);
   		} catch(Exception e) {
   			throw new ServiceException(e);
   		}
	}
	
   	/**
   	 * Find activity creator.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void findActivityCreator(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		Path path = this.getObjectIdentity();
   		DataBean data = this.getData();
   		QueryBean queryBean = new QueryBean();
   		queryBean.setPosition(0);
   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
   		queryBean.setQuery(data.getActivityCreator().getTitle());
   		data.setFindActivityCreatorResult(
	   		this.findActivityCreators(
	   			path,
	   			queryBean
	   		)
   		);
   	}
   	
   	/**
   	 * Find assignedTo.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void findAssignedTo(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		Path path = this.getObjectIdentity();
   		DataBean data = this.getData();
   		QueryBean queryBean = new QueryBean();
   		queryBean.setPosition(0);
   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
   		queryBean.setQuery(data.getAssignedTo().getTitle());
   		data.setFindAssignedToResult(
	   		this.findContacts(
	   			path,
	   			queryBean
	   		)
   		);
   	}
   	
   	/**
   	 * Find reportingContact.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void findReportingContact(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		Path path = this.getObjectIdentity();
   		DataBean data = this.getData();
   		QueryBean queryBean = new QueryBean();
   		queryBean.setPosition(0);
   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
   		queryBean.setQuery(data.getReportingContact().getTitle());
   		data.setFindReportingContactResult(
	   		this.findContacts(
	   			path,
	   			queryBean
	   		)
   		);
   	}
   	
   	/**
   	 * Find reportingAccount.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void findReportingAccount(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		Path path = this.getObjectIdentity();
   		DataBean data = this.getData();
   		QueryBean queryBean = new QueryBean();
   		queryBean.setPosition(0);
   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
   		queryBean.setQuery(data.getReportingAccount().getTitle());
   		data.setFindReportingAccountResult(
	   		this.findAccounts(
	   			path,
	   			queryBean
	   		)
   		);
   	}
   	
   	/**
   	 * Find activityGroup1.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void findActivityGroup1(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		Path path = this.getObjectIdentity();
   		DataBean data = this.getData();
   		QueryBean queryBean = new QueryBean();
   		queryBean.setPosition(0);
   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
   		queryBean.setQuery(data.getActivityGroup1().getTitle());
   		data.setFindActivityGroup1Result(
	   		this.findActivityGroups(
	   			path,
	   			queryBean
	   		)
   		);
   	}

   	/**
   	 * Find activityGroup2.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void findActivityGroup2(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		Path path = this.getObjectIdentity();
   		DataBean data = this.getData();
   		QueryBean queryBean = new QueryBean();
   		queryBean.setPosition(0);
   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);   		
   		queryBean.setQuery(data.getActivityGroup2().getTitle());
   		data.setFindActivityGroup2Result(
	   		this.findActivityGroups(
	   			path,
	   			queryBean
	   		)
   		);
   	}

   	/**
   	 * Find activityGroup3.
   	 * 
   	 * @param event
   	 * @throws ServiceException
   	 */
   	public void findActivityGroup3(
   		javax.faces.event.AjaxBehaviorEvent event
   	) throws ServiceException {
   		Path path = this.getObjectIdentity();
   		DataBean data = this.getData();
   		QueryBean queryBean = new QueryBean();
   		queryBean.setPosition(0);
   		queryBean.setSize(DEFAULT_RESULT_SET_SIZE);
   		queryBean.setQuery(data.getActivityGroup3().getTitle());
   		data.setFindActivityGroup3Result(
	   		this.findActivityGroups(
	   			path,
	   			queryBean
	   		)
   		);
   	}

   	/**
	 * @return the data
	 */
	public DataBean getData() {
		return data;
	}

	/**
	 * @param data the data to set
	 */
	public void setData(DataBean data) {
		this.data = data;
	}

	/**
	 * @return the metaInf
	 */
	public MetaInfBean getMetaInf() {
		return metaInf;
	}

	/**
	 * @param metaInf the metaInf to set
	 */
	public void setMetaInf(MetaInfBean metaInf) {
		this.metaInf = metaInf;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private static final int DEFAULT_RESULT_SET_SIZE = 20;

	private DataBean data;
	private MetaInfBean metaInf;
   	
}
