/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateActivityApiController
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

import java.io.Reader;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.jdo.PersistenceManager;

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
import org.openmdx.portal.servlet.AbstractApiController;
import org.openmdx.portal.servlet.ApplicationContext;
import org.w3c.spi2.Datatypes;

/**
 * CreateActivityApiController
 *
 */
public class CreateActivityApiController extends AbstractApiController {

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
		 * @return the reportingAccount
		 */
		public AbstractApiController.ObjectReferenceBean getReportingAccount() {
			return reportingAccount;
		}
		/**
		 * @param reportingAccount the reportingAccount to set
		 */
		public void setReportingAccount(AbstractApiController.ObjectReferenceBean reportingAccount) {
			this.reportingAccount = reportingAccount;
		}
		/**
		 * @return the reportingContact
		 */
		public AbstractApiController.ObjectReferenceBean getReportingContact() {
			return reportingContact;
		}
		/**
		 * @param reportingContact the reportingContact to set
		 */
		public void setReportingContact(AbstractApiController.ObjectReferenceBean reportingContact) {
			this.reportingContact = reportingContact;
		}
		/**
		 * @return the assignedTo
		 */
		public AbstractApiController.ObjectReferenceBean getAssignedTo() {
			return assignedTo;
		}
		/**
		 * @param assignedTo the assignedTo to set
		 */
		public void setAssignedTo(AbstractApiController.ObjectReferenceBean assignedTo) {
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
		public AbstractApiController.ObjectReferenceBean getActivityGroup1() {
			return activityGroup1;
		}
		/**
		 * @param activityGroup1 the activityGroup1 to set
		 */
		public void setActivityGroup1(AbstractApiController.ObjectReferenceBean activityGroup1) {
			this.activityGroup1 = activityGroup1;
		}
		/**
		 * @return the activityGroup2
		 */
		public AbstractApiController.ObjectReferenceBean getActivityGroup2() {
			return activityGroup2;
		}
		/**
		 * @param activityGroup2 the activityGroup2 to set
		 */
		public void setActivityGroup2(AbstractApiController.ObjectReferenceBean activityGroup2) {
			this.activityGroup2 = activityGroup2;
		}
		/**
		 * @return the activityGroup3
		 */
		public AbstractApiController.ObjectReferenceBean getActivityGroup3() {
			return activityGroup3;
		}
		/**
		 * @param activityGroup3 the activityGroup3 to set
		 */
		public void setActivityGroup3(AbstractApiController.ObjectReferenceBean activityGroup3) {
			this.activityGroup3 = activityGroup3;
		}
		/**
		 * @return the activity
		 */
		public AbstractApiController.ObjectReferenceBean getActivity() {
			return activity;
		}
		/**
		 * @param activity the activity to set
		 */
		public void setActivity(AbstractApiController.ObjectReferenceBean activity) {
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
		private String name;
		private AbstractApiController.ObjectReferenceBean activityCreator;
		private AbstractApiController.ObjectReferenceBean reportingAccount;
		private AbstractApiController.ObjectReferenceBean reportingContact;
		private AbstractApiController.ObjectReferenceBean assignedTo;
		private Short priority;
		private Date dueBy;
		private Date scheduledStart;
		private Date scheduledEnd;
		private String misc1;
		private String misc2;
		private String misc3;
		private String description;
		private String detailedDescription;
		private AbstractApiController.ObjectReferenceBean activityGroup1;
		private AbstractApiController.ObjectReferenceBean activityGroup2;
		private AbstractApiController.ObjectReferenceBean activityGroup3;
		private AbstractApiController.ObjectReferenceBean activity;
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

		private String locale;
		private String timezone;
		private String labelName;
		private String labelActivityCreator;
		private String labelReportingAccount;
		private String labelReportingContact;
		private String labelAssignedTo;
		private String labelPriority;
		private String labelDueBy;
		private String labelScheduledStart;
		private String labelScheduledEnd;
		private String labelMisc1;
		private String labelMisc2;
		private String labelMisc3;
		private String labelDescription;
		private String labelDetailedDescription;
		private String labelActivityGroup;
		private String labelFieldGroupActivity;
		private String labelFieldGroupDetails;
		private List<AbstractApiController.OptionBean> optionsPriority;
	}
	
	public CreateActivityApiController(
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

	public String toJson(
		Object object
	) throws ServiceException {
		return this.gson.toJson(object);
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

	public DataBean getData(
		Path path
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		DataBean dataBean = new DataBean();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path.getParent());
		if(obj instanceof ActivityCreator) {
			dataBean.setActivityCreator(this.newObjectReferenceBean(obj));				
		}
		return dataBean;
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
	 * Put data.
	 * 
	 * @param path
	 * @param dataBean
	 * @return
	 * @throws ServiceException
	 */
	public DataBean putData(
		Path path,
		DataBean dataBean
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path.getParent());
		boolean allMandatoryFieldsSet = true;
		org.openmdx.ui1.jmi1.ElementDefinition elDef = null;
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
			if(dataBean.getActivityCreator() == null || dataBean.getActivityCreator().getXri() == null) {
				allMandatoryFieldsSet = false;
				errors.add(
					this.createErrorMessage(
						app.getTexts().getErrorTextMandatoryField(),
		            	new String[]{this.getLabel("org:opencrx:kernel:activity1:Activity:lastAppliedCreator")}
		         	 )
				);
			}
		} catch(Exception ignore) {}
		// Check reportingAccount
		try {
			elDef = app.getUiElementDefinition("org:opencrx:kernel:activity1:Activity:reportingAccount");
			if (elDef != null && elDef.isMandatory() != null && elDef.isMandatory().booleanValue() && dataBean.getReportingAccount() == null) {
				allMandatoryFieldsSet = false;
				errors.add(
					this.createErrorMessage(
						app.getTexts().getErrorTextMandatoryField(),
		            	new String[]{this.getLabel("org:opencrx:kernel:activity1:Activity:reportingAccount")}
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
					: dataBean.getPriority().shortValue();
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
		dataBean.setErrors(errors.isEmpty() ? null : errors);
		return dataBean;
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
		metaInfBean.setLabelName(this.getLabel("org:opencrx:kernel:activity1:Activity:name"));
		metaInfBean.setLabelActivityCreator(this.getLabel("org:opencrx:kernel:activity1:Activity:lastAppliedCreator"));
		metaInfBean.setLabelReportingAccount(this.getLabel("org:opencrx:kernel:activity1:Activity:reportingAccount"));
		metaInfBean.setLabelReportingContact(this.getLabel("org:opencrx:kernel:activity1:Activity:reportingContact"));
		metaInfBean.setLabelAssignedTo(this.getLabel("org:opencrx:kernel:activity1:Activity:assignedTo"));
		metaInfBean.setLabelPriority(this.getLabel("org:opencrx:kernel:activity1:Activity:priority"));
		metaInfBean.setLabelDueBy(this.getLabel("org:opencrx:kernel:activity1:Activity:dueBy"));
		metaInfBean.setLabelScheduledStart(this.getLabel("org:opencrx:kernel:activity1:Activity:scheduledStart"));
		metaInfBean.setLabelScheduledEnd(this.getLabel("org:opencrx:kernel:activity1:Activity:scheduledEnd"));
		metaInfBean.setLabelMisc1(this.getLabel("org:opencrx:kernel:activity1:Activity:misc1"));
		metaInfBean.setLabelMisc2(this.getLabel("org:opencrx:kernel:activity1:Activity:misc2"));
		metaInfBean.setLabelMisc3(this.getLabel("org:opencrx:kernel:activity1:Activity:misc3"));
		metaInfBean.setLabelDescription(this.getLabel("org:opencrx:kernel:activity1:Activity:description"));
		metaInfBean.setLabelDetailedDescription(this.getLabel("org:opencrx:kernel:activity1:Activity:detailedDescription"));
		metaInfBean.setLabelActivityGroup(this.getLabel("org:opencrx:kernel:activity1:ActivityGroupAssignment:activityGroup"));
		metaInfBean.setLabelFieldGroupActivity(app.getLabel("org:opencrx:kernel:activity1:Activity"));
		metaInfBean.setLabelFieldGroupDetails(this.getLabel("Tab:10"));
		metaInfBean.setOptionsPriority(this.getOptions("org:opencrx:kernel:activity1:Activity:priority", app.getCurrentLocaleAsIndex(), false));
		return metaInfBean;
	}

	/**
	 * Get activity creators.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> getActivityCreators(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(path);
		org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery activityCreatorQuery = 
			(org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery)pm.newQuery(ActivityCreator.class);
		activityCreatorQuery.forAllDisabled().isFalse();
		activityCreatorQuery.name().like("(?i).*" + queryBean.getQuery() + ".*");
		activityCreatorQuery.orderByName().ascending();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		int count = 0;
		for(Iterator<ActivityCreator> i = activitySegment.getActivityCreator(activityCreatorQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
			result.add(this.newObjectReferenceBean(i.next()));
			count++;
			if(count > queryBean.getSize()) {
				break;
			}
		}
		return result;
	}

	/**
	 * Get accounts.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> getAccounts(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(path);
		org.opencrx.kernel.account1.cci2.AccountQuery accountQuery = 
			(org.opencrx.kernel.account1.cci2.AccountQuery)pm.newQuery(Account.class);
		accountQuery.forAllDisabled().isFalse();
		accountQuery.thereExistsFullName().like("(?i).*" + queryBean.getQuery() + ".*");
		accountQuery.orderByFullName().ascending();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
		int count = 0;
		for(Iterator<Account> i = accountSegment.getAccount(accountQuery).listIterator(queryBean.getPosition()); i.hasNext(); ) {
			result.add(this.newObjectReferenceBean(i.next()));
			count++;
			if(count > queryBean.getSize()) {
				break;
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
	 * Get activity groups.
	 * 
	 * @param path
	 * @param queryBean
	 * @return
	 * @throws ServiceException
	 */
	public List<ObjectReferenceBean> getActivityGroups(
		Path path,
		QueryBean queryBean
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		List<ObjectReferenceBean> result = new ArrayList<ObjectReferenceBean>();
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
		return result;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	private final com.google.gson.Gson gson;
	
}
