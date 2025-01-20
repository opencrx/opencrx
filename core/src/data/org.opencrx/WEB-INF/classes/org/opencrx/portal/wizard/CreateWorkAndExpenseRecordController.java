/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateWorkAndExpenseRecordController
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

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import jakarta.servlet.http.HttpServletRequest;
import javax.xml.datatype.XMLGregorianCalendar;

import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery;
import org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery;
import org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery;
import org.opencrx.kernel.activity1.cci2.CalendarDayQuery;
import org.opencrx.kernel.activity1.cci2.ResourceRateQuery;
import org.opencrx.kernel.activity1.cci2.WeekDayQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityAddExpenseRecordParams;
import org.opencrx.kernel.activity1.jmi1.ActivityAddWorkRecordParams;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult;
import org.opencrx.kernel.activity1.jmi1.CalendarDay;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.activity1.jmi1.ResourceAssignment;
import org.opencrx.kernel.activity1.jmi1.ResourceRate;
import org.opencrx.kernel.activity1.jmi1.WeekDay;
import org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JspWizardController;
import org.openmdx.portal.servlet.ObjectReference;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * CreateWorkAndExpenseRecordController
 *
 */
public class CreateWorkAndExpenseRecordController extends JspWizardController {

	/**
	 * FormFields
	 *
	 */
	public static class FormFields {
		
		public FormFields(
		) {			
		}
		
		/**
		 * @return the isInitialized
		 */
		public Boolean getIsInitialized() {
			return isInitialized;
		}
		/**
		 * @param isInitialized the isInitialized to set
		 */
		public void setIsInitialized(Boolean isInitialized) {
			this.isInitialized = isInitialized;
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
		 * @return the selectedDateStr
		 */
		public String getSelectedDateStr() {
			return selectedDateStr;
		}
		/**
		 * @param selectedDateStr the selectedDateStr to set
		 */
		public void setSelectedDateStr(String selectedDateStr) {
			this.selectedDateStr = selectedDateStr;
		}
		/**
		 * @return the recordType
		 */
		public Short getRecordType() {
			return recordType;
		}
		/**
		 * @param recordType the recordType to set
		 */
		public void setRecordType(Short recordType) {
			this.recordType = recordType;
		}
		/**
		 * @return the previousRecordType
		 */
		public Short getPreviousRecordType() {
			return previousRecordType;
		}
		/**
		 * @param previousRecordType the previousRecordType to set
		 */
		public void setPreviousRecordType(Short previousRecordType) {
			this.previousRecordType = previousRecordType;
		}
		/**
		 * @return the isBillable
		 */
		public Boolean getIsBillable() {
			return isBillable;
		}
		/**
		 * @param isBillable the isBillable to set
		 */
		public void setIsBillable(Boolean isBillable) {
			this.isBillable = isBillable;
		}
		/**
		 * @return the isReimbursable
		 */
		public Boolean getIsReimbursable() {
			return isReimbursable;
		}
		/**
		 * @param isReimbursable the isReimbursable to set
		 */
		public void setIsReimbursable(Boolean isReimbursable) {
			this.isReimbursable = isReimbursable;
		}
		/**
		 * @return the makePrivate
		 */
		public Boolean getMakePrivate() {
			return makePrivate;
		}
		/**
		 * @param makePrivate the makePrivate to set
		 */
		public void setMakePrivate(Boolean makePrivate) {
			this.makePrivate = makePrivate;
		}
		/**
		 * @return the isWorkRecord
		 */
		public Boolean getIsWorkRecord() {
			return isWorkRecord;
		}
		/**
		 * @param isWorkRecord the isWorkRecord to set
		 */
		public void setIsWorkRecord(Boolean isWorkRecord) {
			this.isWorkRecord = isWorkRecord;
		}
		/**
		 * @return the isWorkRecordInPercent
		 */
		public Boolean getIsWorkRecordInPercent() {
			return isWorkRecordInPercent;
		}
		/**
		 * @param isWorkRecordInPercent the isWorkRecordInPercent to set
		 */
		public void setIsWorkRecordInPercent(Boolean isWorkRecordInPercent) {
			this.isWorkRecordInPercent = isWorkRecordInPercent;
		}
		/**
		 * @return the hasProjects
		 */
		public Boolean getHasProjects() {
			return hasProjects;
		}
		/**
		 * @param hasProjects the hasProjects to set
		 */
		public void setHasProjects(Boolean hasProjects) {
			this.hasProjects = hasProjects;
		}
		/**
		 * @return the isResourceChange
		 */
		public Boolean getIsResourceChange() {
			return isResourceChange;
		}
		/**
		 * @param isResourceChange the isResourceChange to set
		 */
		public void setIsResourceChange(Boolean isResourceChange) {
			this.isResourceChange = isResourceChange;
		}
		/**
		 * @return the isContactChange
		 */
		public Boolean getIsContactChange() {
			return isContactChange;
		}
		/**
		 * @param isContactChange the isContactChange to set
		 */
		public void setIsContactChange(Boolean isContactChange) {
			this.isContactChange = isContactChange;
		}
		/**
		 * @return the resetActivityXri
		 */
		public Boolean getResetActivityXri() {
			return resetActivityXri;
		}
		/**
		 * @param resetActivityXri the resetActivityXri to set
		 */
		public void setResetActivityXri(Boolean resetActivityXri) {
			this.resetActivityXri = resetActivityXri;
		}
		/**
		 * @return the isFullStartedAtDate
		 */
		public Boolean getIsFullStartedAtDate() {
			return isFullStartedAtDate;
		}
		/**
		 * @param isFullStartedAtDate the isFullStartedAtDate to set
		 */
		public void setIsFullStartedAtDate(Boolean isFullStartedAtDate) {
			this.isFullStartedAtDate = isFullStartedAtDate;
		}
		/**
		 * @return the activityXri
		 */
		public String getActivityXri() {
			return activityXri;
		}
		/**
		 * @param activityXri the activityXri to set
		 */
		public void setActivityXri(String activityXri) {
			this.activityXri = activityXri;
		}
		/**
		 * @return the contactXri
		 */
		public String getContactXri() {
			return contactXri;
		}
		/**
		 * @param contactXri the contactXri to set
		 */
		public void setContactXri(String contactXri) {
			this.contactXri = contactXri;
		}
		/**
		 * @return the effortHH
		 */
		public String getEffortHH() {
			return effortHH;
		}
		/**
		 * @param effortHH the effortHH to set
		 */
		public void setEffortHH(String effortHH) {
			this.effortHH = effortHH;
		}
		/**
		 * @return the effortMM
		 */
		public String getEffortMM() {
			return effortMM;
		}
		/**
		 * @param effortMM the effortMM to set
		 */
		public void setEffortMM(String effortMM) {
			this.effortMM = effortMM;
		}
		/**
		 * @return the startedAtHH
		 */
		public String getStartedAtHH() {
			return startedAtHH;
		}
		/**
		 * @param startedAtHH the startedAtHH to set
		 */
		public void setStartedAtHH(String startedAtHH) {
			this.startedAtHH = startedAtHH;
		}
		/**
		 * @return the startedAtMM
		 */
		public String getStartedAtMM() {
			return startedAtMM;
		}
		/**
		 * @param startedAtMM the startedAtMM to set
		 */
		public void setStartedAtMM(String startedAtMM) {
			this.startedAtMM = startedAtMM;
		}
		/**
		 * @return the endedAtHH
		 */
		public String getEndedAtHH() {
			return endedAtHH;
		}
		/**
		 * @param endedAtHH the endedAtHH to set
		 */
		public void setEndedAtHH(String endedAtHH) {
			this.endedAtHH = endedAtHH;
		}
		/**
		 * @return the endedAtMM
		 */
		public String getEndedAtMM() {
			return endedAtMM;
		}
		/**
		 * @param endedAtMM the endedAtMM to set
		 */
		public void setEndedAtMM(String endedAtMM) {
			this.endedAtMM = endedAtMM;
		}
		/**
		 * @return the resourceXri
		 */
		public String getResourceXri() {
			return resourceXri;
		}
		/**
		 * @param resourceXri the resourceXri to set
		 */
		public void setResourceXri(String resourceXri) {
			this.resourceXri = resourceXri;
		}
		/**
		 * @return the previousResourceXri
		 */
		public String getPreviousResourceXri() {
			return previousResourceXri;
		}
		/**
		 * @param previousResourceXri the previousResourceXri to set
		 */
		public void setPreviousResourceXri(String previousResourceXri) {
			this.previousResourceXri = previousResourceXri;
		}
		/**
		 * @return the billingCurrency
		 */
		public Short getBillingCurrency() {
			return billingCurrency;
		}
		/**
		 * @param billingCurrency the billingCurrency to set
		 */
		public void setBillingCurrency(Short billingCurrency) {
			this.billingCurrency = billingCurrency;
		}
		/**
		 * @return the contactXriTitle
		 */
		public String getContactXriTitle() {
			return contactXriTitle;
		}
		/**
		 * @param contactXriTitle the contactXriTitle to set
		 */
		public void setContactXriTitle(String contactXriTitle) {
			this.contactXriTitle = contactXriTitle;
		}
		/**
		 * @return the activityFilter
		 */
		public String getActivityFilter() {
			return activityFilter;
		}
		/**
		 * @param activityFilter the activityFilter to set
		 */
		public void setActivityFilter(String activityFilter) {
			this.activityFilter = activityFilter;
		}
		/**
		 * @return the activityFilterXri
		 */
		public String getActivityFilterXri() {
			return activityFilterXri;
		}
		/**
		 * @param activityFilterXri the activityFilterXri to set
		 */
		public void setActivityFilterXri(String activityFilterXri) {
			this.activityFilterXri = activityFilterXri;
		}
		/**
		 * @return the filterActivityGroupName
		 */
		public String getFilterActivityGroupName() {
			return filterActivityGroupName;
		}
		/**
		 * @param filterActivityGroupName the filterActivityGroupName to set
		 */
		public void setFilterActivityGroupName(String filterActivityGroupName) {
			this.filterActivityGroupName = filterActivityGroupName;
		}
		/**
		 * @return the filterActivityName
		 */
		public String getFilterActivityName() {
			return filterActivityName;
		}
		/**
		 * @param filterActivityName the filterActivityName to set
		 */
		public void setFilterActivityName(String filterActivityName) {
			this.filterActivityName = filterActivityName;
		}
		/**
		 * @return the projectMain
		 */
		public String getProjectMain() {
			return projectMain;
		}
		/**
		 * @param projectMain the projectMain to set
		 */
		public void setProjectMain(String projectMain) {
			this.projectMain = projectMain;
		}
		/**
		 * @return the excludeClosedActivities
		 */
		public Boolean getExcludeClosedActivities() {
			return excludeClosedActivities;
		}
		/**
		 * @param excludeClosedActivities the excludeClosedActivities to set
		 */
		public void setExcludeClosedActivities(Boolean excludeClosedActivities) {
			this.excludeClosedActivities = excludeClosedActivities;
		}
		/**
		 * @return the showActivityGroupNameFilter
		 */
		public Boolean getShowActivityGroupNameFilter() {
			return showActivityGroupNameFilter;
		}
		/**
		 * @param showActivityGroupNameFilter the showActivityGroupNameFilter to set
		 */
		public void setShowActivityGroupNameFilter(Boolean showActivityGroupNameFilter) {
			this.showActivityGroupNameFilter = showActivityGroupNameFilter;
		}
		/**
		 * @return the isFullMonth
		 */
		public Boolean getIsFullMonth() {
			return isFullMonth;
		}
		/**
		 * @param isFullMonth the isFullMonth to set
		 */
		public void setIsFullMonth(Boolean isFullMonth) {
			this.isFullMonth = isFullMonth;
		}
		/**
		 * @return the activitySortOrder
		 */
		public Integer getActivitySortOrder() {
			return activitySortOrder;
		}
		/**
		 * @param activitySortOrder the activitySortOrder to set
		 */
		public void setActivitySortOrder(Integer activitySortOrder) {
			this.activitySortOrder = activitySortOrder;
		}
		/**
		 * @return the lastCreatedWorkRecordXri
		 */
		public String getLastCreatedWorkExpenseRecordXri() {
			return lastCreatedWorkExpenseRecordXri;
		}
		/**
		 * @param lastCreatedWorkRecordXri the lastCreatedWorkRecordXri to set
		 */
		public void setLastCreatedWorkExpenseRecordXri(String lastCreatedWorkRecordXri) {
			this.lastCreatedWorkExpenseRecordXri = lastCreatedWorkRecordXri;
		}
		/**
		 * @return the lastFocusId
		 */
		public String getLastFocusId() {
			return lastFocusId;
		}
		/**
		 * @param lastFocusId the lastFocusId to set
		 */
		public void setLastFocusId(String lastFocusId) {
			this.lastFocusId = lastFocusId;
		}
		/**
		 * @return the uomXri
		 */
		public String getUomXri() {
			return uomXri;
		}
		/**
		 * @param uomXri the uomXri to set
		 */
		public void setUomXri(String uomXri) {
			this.uomXri = uomXri;
		}
		/**
		 * @return the paymentType
		 */
		public Short getPaymentType() {
			return paymentType;
		}
		/**
		 * @param paymentType the paymentType to set
		 */
		public void setPaymentType(Short paymentType) {
			this.paymentType = paymentType;
		}
		/**
		 * @return the quantity
		 */
		public String getQuantity() {
			return quantity;
		}
		/**
		 * @param quantity the quantity to set
		 */
		public void setQuantity(String quantity) {
			this.quantity = quantity;
		}
		/**
		 * @return the updateCalendarDay
		 */
		public String getUpdateCalendarDay() {
			return updateCalendarDay;
		}
		/**
		 * @param updateCalendarDay the updateCalendarDay to set
		 */
		public void setUpdateCalendarDay(String updateCalendarDay) {
			this.updateCalendarDay = updateCalendarDay;
		}
		/**
		 * @return the createCalendarDay
		 */
		public String getCreateCalendarDay() {
			return createCalendarDay;
		}
		/**
		 * @param createCalendarDay the createCalendarDay to set
		 */
		public void setCreateCalendarDay(String createCalendarDay) {
			this.createCalendarDay = createCalendarDay;
		}

		/**
		 * @return the rate
		 */
		public String getRate() {
			return rate;
		}

		/**
		 * @param rate the rate to set
		 */
		public void setRate(String rate) {
			this.rate = rate;
		}

		/**
		 * @return the quantPercent
		 */
		public String getQuantPercent() {
			return quantPercent;
		}

		/**
		 * @param quantPercent the quantPercent to set
		 */
		public void setQuantPercent(String quantPercent) {
			this.quantPercent = quantPercent;
		}

		private Boolean isInitialized;
		private String name;
		private String description;
		private String selectedDateStr;
		private Short recordType;
		private Short previousRecordType;		
		private Boolean isBillable;
		private Boolean isReimbursable;
		private Boolean makePrivate;	
		private Boolean isWorkRecord;
		private Boolean isWorkRecordInPercent;
		private Boolean hasProjects;
		private Boolean isResourceChange;
		private Boolean isContactChange;
		private Boolean resetActivityXri;
		private Boolean isFullStartedAtDate;
		private String activityXri;
		private String contactXri;
		private String effortHH;
		private String effortMM;
		private String startedAtHH;
		private String startedAtMM;
		private String endedAtHH;
		private String endedAtMM;		
		private String resourceXri;
		private String previousResourceXri;
		private Short billingCurrency;
		private String contactXriTitle;
		private String activityFilter;
		private String activityFilterXri;
		private String filterActivityGroupName;
		private String filterActivityName;
		private String projectMain;
		private Boolean excludeClosedActivities;
		private Boolean showActivityGroupNameFilter;
		private Boolean isFullMonth;
		private Integer activitySortOrder;
		private String lastCreatedWorkExpenseRecordXri;
		private String lastFocusId;
		private String uomXri;
		private Short paymentType;
		private String quantity;
		private String updateCalendarDay;
		private String createCalendarDay;
		private String rate;
		private String quantPercent;
	}
	
	/**
	 * Constructor.
	 * 
	 */
	public CreateWorkAndExpenseRecordController(
	) {
		super();
	}
	
	/**
	 * Get date as string.
	 * 
	 * @param date
	 * @return
	 */
	public String getDateAsString(
		GregorianCalendar date
	) {
		return getDateAsString(
			date.get(GregorianCalendar.YEAR),
			date.get(GregorianCalendar.MONTH) + 1,
			date.get(GregorianCalendar.DAY_OF_MONTH)
		);
	}

	/**
	 * Get date as string.
	 * 
	 * @param year
	 * @param month
	 * @param dayOfMonth
	 * @return
	 */
	public String getDateAsString(
		int year,
		int month,
		int dayOfMonth
	) {
		return // YYYYMMDD
			Integer.toString(year) +
			((month < 10 ? "0" : "") + Integer.toString(month)) +
			((dayOfMonth < 10 ? "0" : "") + Integer.toString(dayOfMonth));
	}

	/**
	 * Get date as calendar.
	 * 
	 * @param dateAsString
	 * @param app
	 * @return
	 */
	public GregorianCalendar getDateAsCalendar(
		String dateAsString,
		ApplicationContext app
	) {
		GregorianCalendar date = new GregorianCalendar(app.getCurrentLocale());
		date.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		date.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
		date.set(GregorianCalendar.YEAR, Integer.valueOf(dateAsString.substring(0, 4)));
		date.set(GregorianCalendar.MONTH, Integer.valueOf(dateAsString.substring(4, 6)) - 1);
		date.set(GregorianCalendar.DAY_OF_MONTH, Integer.valueOf(dateAsString.substring(6, 8)));
		date.set(GregorianCalendar.HOUR_OF_DAY, 0);
		date.set(GregorianCalendar.MINUTE, 0);
		date.set(GregorianCalendar.SECOND, 0);
		date.set(GregorianCalendar.MILLISECOND, 0);
		return date;
	}

	/**
	 * Get day of week.
	 * 
	 * @param dateAsString
	 * @param app
	 * @return
	 */
	public int getDayOfWeek(
		String dateAsString,
		ApplicationContext app
	) {
		GregorianCalendar date = getDateAsCalendar(dateAsString, app);
		return date.get(Calendar.DAY_OF_WEEK);
	}

	/**
	 * Convert minutes to HH:MM.
	 * 
	 * @param decimalMinutes
	 * @return
	 */
	public String decimalMinutesToHhMm(
		double decimalMinutes
	) {
		NumberFormat hhFormatter = new DecimalFormat("#,##0");
		NumberFormat mmFormatter = new DecimalFormat("#,#00");
		int hours = (int)(decimalMinutes / 60.0);
		int minutes = (int)java.lang.Math.rint(decimalMinutes % 60.0);
		if (minutes == 60) {
				hours += 1;
				minutes = 0;
		}
		return hhFormatter.format(hours) + ":" + mmFormatter.format(minutes);
	}

	/**
	 * Get user name.
	 * 
	 * @param pm
	 * @param homeSegment
	 * @param resource
	 * @return
	 */
	public String getUsername(
		org.opencrx.kernel.home1.jmi1.Segment homeSegment,
		org.opencrx.kernel.activity1.jmi1.Resource resource
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(homeSegment);
		org.opencrx.kernel.home1.cci2.UserHomeQuery userHomeFilter = (org.opencrx.kernel.home1.cci2.UserHomeQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.UserHome.class);
		userHomeFilter.thereExistsContact().equalTo(resource.getContact());
		List<UserHome> userHomes = homeSegment.getUserHome(userHomeFilter);
		return userHomes.isEmpty() ? null : userHomes.iterator().next().refGetPath().getLastSegment().toString();
	}

	/**
	 * Find principal group.
	 * 
	 * @param principalGroupName
	 * @param realm
	 * @param pm
	 * @return
	 */
	public org.opencrx.security.realm1.jmi1.PrincipalGroup findPrincipalGroup(
		 String principalGroupName,
		 org.openmdx.security.realm1.jmi1.Realm realm,
		 javax.jdo.PersistenceManager pm
	) {
		try {
			org.opencrx.security.realm1.jmi1.PrincipalGroup principalGroup = (org.opencrx.security.realm1.jmi1.PrincipalGroup)org.opencrx.kernel.backend.SecureObject.getInstance().findPrincipal(
				principalGroupName,
				realm
			);
			return principalGroup;
		} catch (Exception e) {
			return null;
		}
	}
	
	/**
	 * Create or update calendar day.
	 * 
	 * @param name
	 * @param date
	 * @param calendar
	 * @param calendarDay
	 * @param activitySegment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.activity1.jmi1.CalendarDay createOrUpdateCalendarDay(
		String name,
		String date, /* in format YYYYMMDD */
		org.opencrx.kernel.activity1.jmi1.Calendar calendar,
		org.opencrx.kernel.activity1.jmi1.CalendarDay calendarDay,
		org.opencrx.kernel.activity1.jmi1.Segment activitySegment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(calendar);
		try {
			pm.currentTransaction().begin();
			if(calendarDay == null) {
				calendarDay = pm.newInstance(org.opencrx.kernel.activity1.jmi1.CalendarDay.class);
				calendarDay.setName(name);
				XMLGregorianCalendar cal = org.w3c.spi2.Datatypes.create(
						XMLGregorianCalendar.class,
						date
				);
				calendarDay.setDateOfDay(cal);
				calendar.addCalendarDay(
					false,
					org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
					calendarDay
				);
			}
			calendarDay.setName(name);
			pm.currentTransaction().commit();
		}
		catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return calendarDay;
	}	

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.JspWizardController#init(jakarta.servlet.http.HttpServletRequest, java.lang.String, boolean, boolean)
	 */
	@Override
    public boolean init(
    	HttpServletRequest request, 
    	String encoding, 
    	boolean assertRequestId, 
    	boolean assertObjectXri
    ) {
	    if(!super.init(request, encoding, assertRequestId, assertObjectXri)) {
	    	return false;
	    }
		ApplicationContext app = this.getApp();
		PersistenceManager pm = this.getPm();
		this.timezone = TimeZone.getTimeZone(app.getCurrentTimeZone());
		this.dtf = new SimpleDateFormat("EEEE", app.getCurrentLocale()); 
		this.dtf.setTimeZone(this.timezone);
		this.monthFormat = new java.text.SimpleDateFormat("MMMM", app.getCurrentLocale()); 
		this.monthFormat.setTimeZone(this.timezone);
		this.dayInWeekFormat = new java.text.SimpleDateFormat("E", app.getCurrentLocale()); 
		this.dayInWeekFormat.setTimeZone(this.timezone);
		this.weekDayFormat = new SimpleDateFormat("EE", app.getCurrentLocale()); 
		this.weekDayFormat.setTimeZone(this.timezone);
		this.dateOnlyFormat = new SimpleDateFormat("dd-MMM-yyyy", app.getCurrentLocale()); 
		this.dateOnlyFormat.setTimeZone(this.timezone);
		this.dateTimeFormat = new SimpleDateFormat("dd-MMM-yyyy HH:mm", app.getCurrentLocale());	
		this.dateTimeFormat.setTimeZone(this.timezone);
		this.dateFormat = new SimpleDateFormat("EE d-MMMM-yyyy", app.getCurrentLocale()); 
		this.dateFormat.setTimeZone(this.timezone);
		this.dateTimeSortFormat = new SimpleDateFormat("yyyyMMddHHmmss", app.getCurrentLocale()); 
		this.dateTimeSortFormat.setTimeZone(this.timezone);
		this.calendarDayFormat = new SimpleDateFormat("yyyyMMdd", app.getCurrentLocale()); 
		this.calendarDayFormat.setTimeZone(this.timezone);
		this.quantityFormat = new DecimalFormat("0.000");	
		this.formatter = new DecimalFormat("00000");
		this.ratesEpFormat = new DecimalFormat("#,##0.00");
		this.formatter0 = new DecimalFormat("0");		
		this.uomPercent = null;
		try {
			this.uomPercent = (Uom)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.uom1").getDescendant("provider", this.getProviderName(), "segment", "Root", "uom", "Percent")
			);
		} catch (Exception ignore) {}
		return true;
    }

	/**
	 * Parse date.
	 * 
	 * @param dateAsStr
	 * @return
	 * @throws ServiceException
	 */
	public GregorianCalendar parseDate(
		String dateAsStr
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		GregorianCalendar today = new GregorianCalendar(app.getCurrentLocale());
		today.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		String todayStr = getDateAsString(today);
		if(dateAsStr == null || dateAsStr.length() != 8) {
			dateAsStr = todayStr;
		}
		return getDateAsCalendar(dateAsStr, app); //		
	}

	/**
	 * Get calendar set day to first day of month.
	 * 
	 * @param dateStr
	 * @return
	 */
	public GregorianCalendar getFirstDayOfMonthCal(
		String dateStr
	) {
		ApplicationContext app = this.getApp();
		Integer calendarYear = Integer.parseInt(dateStr.substring(0,4));
		Integer calendarMonth = Integer.parseInt(dateStr.substring(4,6))-1;
		GregorianCalendar calendar = new GregorianCalendar(app.getCurrentLocale());
		calendar.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		calendar.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
		calendar.set(GregorianCalendar.YEAR, calendarYear);
		calendar.set(GregorianCalendar.MONTH, calendarMonth);
		calendar.set(GregorianCalendar.DAY_OF_MONTH, 1);
		return calendar;
	}

	/**
	 * Refresh work and expense records.
	 * 
	 * @throws ServiceException
	 */
	protected void refreshWorkAndExpenseRecords(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		if ((this.contact != null) || (this.resource != null)) {
			this.workAndExpenseRecords = new TreeMap<String,WorkAndExpenseRecord>();
			org.opencrx.kernel.activity1.cci2.ResourceQuery resourceQuery = (org.opencrx.kernel.activity1.cci2.ResourceQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Resource.class);
			if (this.contact != null) {
					resourceQuery.thereExistsContact().equalTo(this.contact);
			}
			resourceQuery.forAllDisabled().isFalse();
			this.calendarBeginOfWeek = this.getDateAsCalendar(this.formFields.getSelectedDateStr(), app);
			while (this.calendarBeginOfWeek.get(GregorianCalendar.DAY_OF_WEEK) != this.calendarBeginOfWeek.getFirstDayOfWeek()) {
				this.calendarBeginOfWeek.add(GregorianCalendar.DAY_OF_MONTH, -1);
			}
			this.calendarBeginOfPeriod = this.getDateAsCalendar(this.formFields.getSelectedDateStr(), app);
			if (Boolean.TRUE.equals(this.formFields.getIsFullMonth())) {
				this.calendarBeginOfPeriod.set(GregorianCalendar.DAY_OF_MONTH, 1);
			} else {
				this.calendarBeginOfPeriod = (GregorianCalendar)this.calendarBeginOfWeek.clone();
			}
			java.util.Date beginOfPeriod = this.calendarBeginOfPeriod.getTime();
			this.calendarEndOfPeriod = (GregorianCalendar)this.calendarBeginOfPeriod.clone();
			if (Boolean.TRUE.equals(this.formFields.getIsFullMonth())) {
				this.calendarEndOfPeriod.add(GregorianCalendar.MONTH, 1);
			} else {
				this.calendarEndOfPeriod.add(GregorianCalendar.DAY_OF_MONTH, 7);
			}
			this.calendarEndOfPeriod.add(GregorianCalendar.MILLISECOND, -1);
			java.util.Date endOfPeriod = this.calendarEndOfPeriod.getTime();

			org.opencrx.kernel.activity1.cci2.WorkAndExpenseRecordQuery workAndExpenseRecordFilter = (org.opencrx.kernel.activity1.cci2.WorkAndExpenseRecordQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord.class);
			//workAndExpenseRecordFilter.forAllDisabled().isFalse();
			workAndExpenseRecordFilter.thereExistsStartedAt().between(beginOfPeriod, endOfPeriod);
			if (this.formFields.getIsWorkRecord()) {
				if (this.formFields.getIsWorkRecordInPercent()) {
					workAndExpenseRecordFilter.recordType().equalTo(Short.valueOf((short)1));
					if (this.getUomPercent() != null) {
						workAndExpenseRecordFilter.thereExistsQuantityUom().equalTo(this.getUomPercent());
					}
				} else {
					workAndExpenseRecordFilter.recordType().between(Short.valueOf((short)1), Short.valueOf((short)CreateWorkAndExpenseRecordController.RECORDTYPE_WORK_MAX));
					if (this.getUomPercent() != null) {
							workAndExpenseRecordFilter.forAllQuantityUom().notEqualTo(this.getUomPercent());
					}
				}
			} else {
				workAndExpenseRecordFilter.recordType().greaterThan(Short.valueOf((short)CreateWorkAndExpenseRecordController.RECORDTYPE_WORK_MAX));
			}
			this.sumDays = new double[7];
			this.sumDaysBillable = new double[7];
			for(int i = 0; i < this.sumDays.length; i++) {
				this.sumDays[i] = 0.0;
				this.sumDaysBillable[i] = 0.0;
			}
			int counter = 0;
			org.opencrx.kernel.activity1.jmi1.Resource res = null;
			Iterator<org.opencrx.kernel.activity1.jmi1.Resource> r = null;
			if (this.contact != null) {
				// iterate through all resources of this contact
				r = this.getActivitySegment().getResource(resourceQuery).iterator();
				if (r.hasNext()) {
						res = (org.opencrx.kernel.activity1.jmi1.Resource)r.next();
				}
			} else {
				// process single resource only
				res = this.resource;
			}
			while(res != null) {
				for (org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord workAndExpenseRecord: res.getWorkReportEntry(workAndExpenseRecordFilter)) {
					GregorianCalendar startedAtCal = new GregorianCalendar(app.getCurrentLocale());
					startedAtCal.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
					startedAtCal.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
					String sortKey = org.opencrx.kernel.backend.Activities.getInstance().getUidAsString();
					try {
						if (workAndExpenseRecord.getStartedAt() == null) {
							sortKey = "yyyyMMddHHmmss";
							startedAtCal.setTime(beginOfPeriod);
						} else {
							sortKey = this.getDateTimeSortFormat().format(workAndExpenseRecord.getStartedAt());
							startedAtCal.setTime(workAndExpenseRecord.getStartedAt());
						}
						sortKey += workAndExpenseRecord.getActivity().getActivityNumber() + this.getFormatter().format(counter);
					} catch (Exception e) {};
					if (workAndExpenseRecord.getQuantity() != null) {
						this.sumDays[startedAtCal.get(GregorianCalendar.DAY_OF_WEEK) % 7] += workAndExpenseRecord.getQuantity().doubleValue();
						if (Boolean.TRUE.equals(workAndExpenseRecord.isBillable())) {
							this.sumDaysBillable[startedAtCal.get(GregorianCalendar.DAY_OF_WEEK) % 7] += workAndExpenseRecord.getQuantity().doubleValue();
						}
					}
					this.workAndExpenseRecords.put(sortKey, workAndExpenseRecord);
					counter++;
				}
				res = null;
				if ((r != null) && (r.hasNext())) {
					res = (org.opencrx.kernel.activity1.jmi1.Resource)r.next();
				}
			}
		}
	}

	/**
	 * Action NextMonth.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doNextMonth(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		GregorianCalendar date = this.parseDate(formFields.getSelectedDateStr());
		date.add(GregorianCalendar.MONTH, 1);
		Map<String,String[]> parameterMap = new HashMap<String,String[]>(
			this.getRequest().getParameterMap()
		);
		parameterMap.put(
			"selectedDateStr",
			new String[]{this.getDateAsString(date)}
		);
		this.forward(
			"Refresh",
			parameterMap
		);
	}

	/**
	 * Action PrevMonth.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doPrevMonth(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		GregorianCalendar date = this.parseDate(formFields.getSelectedDateStr());
		date.add(GregorianCalendar.MONTH, -1);
		Map<String,String[]> parameterMap = new HashMap<String,String[]>(
			this.getRequest().getParameterMap()
		);
		parameterMap.put(
			"selectedDateStr",
			new String[]{this.getDateAsString(date)}
		);
		this.forward(
			"Refresh",
			parameterMap
		);
	}

	/**
	 * Action NextYear.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doNextYear(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		GregorianCalendar date = this.parseDate(formFields.getSelectedDateStr());
		date.add(GregorianCalendar.YEAR, 1);
		Map<String,String[]> parameterMap = new HashMap<String,String[]>(
			this.getRequest().getParameterMap()
		);
		parameterMap.put(
			"selectedDateStr",
			new String[]{this.getDateAsString(date)}
		);
		this.forward(
			"Refresh",
			parameterMap
		);
	}

	/**
	 * Action PrevYear.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doPrevYear(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields		
	) throws ServiceException {
		GregorianCalendar date = this.parseDate(formFields.getSelectedDateStr());
		date.add(GregorianCalendar.YEAR, -1);
		Map<String,String[]> parameterMap = new HashMap<String,String[]>(
			this.getRequest().getParameterMap()
		);
		parameterMap.put(
			"selectedDateStr",
			new String[]{this.getDateAsString(date)}
		);
		this.forward(
			"Refresh",
			parameterMap
		);
	}

	/**
	 * Action SelectDate.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doSelectDate(
		@JspWizardController.RequestParameter(name = "dayOfMonth") Integer dayOfMonth,
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields		
	) throws ServiceException {
		Map<String,String[]> parameterMap = new HashMap<String,String[]>(
			this.getRequest().getParameterMap()
		);
		if(dayOfMonth != null) {
			Integer calendarYear = Integer.parseInt(formFields.getSelectedDateStr().substring(0, 4));
			Integer calendarMonth = Integer.parseInt(formFields.getSelectedDateStr().substring(4, 6)) - 1;
			parameterMap.put(
				"selectedDateStr",
				new String[]{this.getDateAsString(calendarYear, calendarMonth + 1, dayOfMonth)}
			);
		}
		this.forward(
			"Refresh",
			parameterMap
		);
	}

	/**
	 * Action SelectDateP.
	 * 
	 * @param dayOfMonth
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doSelectDateP(
		@JspWizardController.RequestParameter(name = "dayOfMonth") Integer dayOfMonth,
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		Map<String,String[]> parameterMap = new HashMap<String,String[]>(
			this.getRequest().getParameterMap()
		);
		if(dayOfMonth != null) {
			GregorianCalendar date = this.parseDate(formFields.getSelectedDateStr());
			date.add(GregorianCalendar.MONTH, -1);
			Integer calendarYear = date.get(Calendar.YEAR);
			Integer calendarMonth = date.get(Calendar.MONTH);
			parameterMap.put(
				"selectedDateStr",
				new String[]{this.getDateAsString(calendarYear, calendarMonth + 1, dayOfMonth)}
			);
		}
		this.forward(
			"Refresh",
			parameterMap
		);
	}

	/**
	 * Action SelectDateN.
	 * 
	 * @param dayOfMonth
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doSelectDateN(
		@JspWizardController.RequestParameter(name = "dayOfMonth") Integer dayOfMonth,
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		Map<String,String[]> parameterMap = new HashMap<String,String[]>(
			this.getRequest().getParameterMap()
		);
		if(dayOfMonth != null) {
			GregorianCalendar date = this.parseDate(formFields.getSelectedDateStr());
			date.add(GregorianCalendar.MONTH, +1);
			Integer calendarYear = date.get(Calendar.YEAR);
			Integer calendarMonth = date.get(Calendar.MONTH);
			parameterMap.put(
				"selectedDateStr",
				new String[]{this.getDateAsString(calendarYear, calendarMonth + 1, dayOfMonth)}
			);
		}
		this.forward(
			"Refresh",
			parameterMap
		);
	}

	/**
	 * Action AddWorkRecord.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doAddWorkRecord(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(formFields);
		this.creationFailed = false;
		this.canExecuteAdd = (this.formFields.getActivityXri() != null) &&
			(this.formFields.getResourceXri() != null) &&
			(this.startedAt != null) &&
			(
				(
					(this.formFields.getIsWorkRecord() && !this.formFields.getIsWorkRecordInPercent()) &&
					(this.paraEffortHH != null) &&
					(this.paraEffortMM != null)
				)
				||
				(
					(this.formFields.getIsWorkRecord() && this.formFields.getIsWorkRecordInPercent()) &&
					(this.paraQuantPercentage != null) &&
					!this.quantPercentageIsZero
				)
				||
				(
					!this.formFields.getIsWorkRecord() &&
					(this.paraRate != null) &&
					!this.quantityIsZero &&
					(this.formFields.getUomXri() != null) && !this.formFields.getUomXri().isEmpty()
				)
			);
		if(this.canExecuteAdd) {
			if((this.formFields.getName() == null) || this.formFields.getName().isEmpty()) {
				this.canExecuteAdd = false;
				this.creationFailed = true; // emulate creation failure to show warning sign next to add button
			} else {
				try {
					WorkAndExpenseRecord workAndExpenseRecord = null;
					if (this.formFields.getIsWorkRecord()) {
						// add WorkRecord
						if (this.endedAt == null && !this.formFields.getIsWorkRecordInPercent()) {
							// calculate as startedAt + paraEffort
							this.endedAt = (GregorianCalendar)this.startedAt.clone();
							this.endedAt.add(GregorianCalendar.HOUR_OF_DAY, this.paraEffortHH);
							this.endedAt.add(GregorianCalendar.MINUTE, this.paraEffortMM);
						}
						ActivityAddWorkRecordParams params = null;
						if (this.formFields.getIsWorkRecordInPercent()) {
							params = Structures.create(
								ActivityAddWorkRecordParams.class,
								Datatypes.member(ActivityAddWorkRecordParams.Member.depotSelector, (short)0),
								Datatypes.member(ActivityAddWorkRecordParams.Member.description, this.formFields.getDescription()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.durationHours, this.paraQuantPercentage),
								Datatypes.member(ActivityAddWorkRecordParams.Member.isBillable, false),
								Datatypes.member(ActivityAddWorkRecordParams.Member.name, this.formFields.getName()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.rate, BigDecimal.ZERO),
								Datatypes.member(ActivityAddWorkRecordParams.Member.rateCurrency, this.formFields.getBillingCurrency()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.recordType, this.formFields.getRecordType()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.resource, pm.getObjectById(new Path(this.formFields.getResourceXri()))),
								Datatypes.member(ActivityAddWorkRecordParams.Member.startAt, this.startedAt.getTime())
							);
							if(this.resourceCalendar != null && this.resourceCalendarDay == null) {
								try {
									if (this.resourceCalendarDayName != null && !this.resourceCalendarDayName.isEmpty()) {
										this.resourceCalendarDay = this.createOrUpdateCalendarDay(
											this.resourceCalendarDayName + "@" + this.resourceDefaultLoad,
											this.resourceCalendarDayName,
											this.resourceCalendar,
											null,
											this.getActivitySegment()
										);
									}
								} catch (Exception e) {
									new ServiceException(e).log();
								}
							}
						} else {
							params = Structures.create(
								ActivityAddWorkRecordParams.class,
								Datatypes.member(ActivityAddWorkRecordParams.Member.depotSelector, (short)0),
								Datatypes.member(ActivityAddWorkRecordParams.Member.description, this.formFields.getDescription()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.durationHours, this.paraEffortHH),
								Datatypes.member(ActivityAddWorkRecordParams.Member.durationMinutes, this.paraEffortMM),
								Datatypes.member(ActivityAddWorkRecordParams.Member.endAt, this.endedAt != null ? this.endedAt.getTime() : null),
								Datatypes.member(ActivityAddWorkRecordParams.Member.isBillable, this.formFields.getIsBillable()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.name, this.formFields.getName()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.rate, this.paraRate),
								Datatypes.member(ActivityAddWorkRecordParams.Member.rateCurrency, this.formFields.getBillingCurrency()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.recordType, this.formFields.getRecordType()),
								Datatypes.member(ActivityAddWorkRecordParams.Member.resource, pm.getObjectById(new Path(this.formFields.getResourceXri()))),
								Datatypes.member(ActivityAddWorkRecordParams.Member.startAt, this.startedAt.getTime())
							);
						}
						pm.currentTransaction().begin();
						AddWorkAndExpenseRecordResult result = ((Activity)pm.getObjectById(new Path(this.formFields.getActivityXri()))).addWorkRecord(params);
						pm.currentTransaction().commit();
						workAndExpenseRecord = (WorkAndExpenseRecord)pm.getObjectById(result.getWorkRecord().refGetPath());
						if(this.formFields.getIsWorkRecordInPercent()) {
							pm.currentTransaction().begin();
							workAndExpenseRecord.setQuantityUom(this.uomPercent);
							workAndExpenseRecord.setEndedAt(null);
							pm.currentTransaction().commit();
						}
					} else {
						// add ExpenseRecord
						if (this.endedAt == null) {
							// set endedAt = startedAt
							this.endedAt = (GregorianCalendar)this.startedAt.clone();
						}
						ActivityAddExpenseRecordParams params = Structures.create(
							ActivityAddExpenseRecordParams.class,
							Datatypes.member(ActivityAddExpenseRecordParams.Member.depotSelector, (short)0),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.description, this.formFields.getDescription()),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.endAt, this.endedAt != null ? this.endedAt.getTime() : null),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.isBillable, Boolean.TRUE.equals(this.formFields.getIsBillable())),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.isReimbursable, Boolean.TRUE.equals(this.formFields.getIsReimbursable())),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.name, this.formFields.getName()),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.paymentType, this.formFields.getPaymentType()),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.quantity, this.paraQuantity),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.quantityUom, pm.getObjectById(new Path(this.formFields.getUomXri()))),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.rate, this.paraRate),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.rateCurrency, this.formFields.getBillingCurrency()),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.recordType, this.formFields.getRecordType()),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.resource, pm.getObjectById(new Path(this.formFields.getResourceXri()))),
							Datatypes.member(ActivityAddExpenseRecordParams.Member.startAt, this.startedAt.getTime())
						);
						pm.currentTransaction().begin();
						org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult result = ((Activity)pm.getObjectById(new Path(this.formFields.getActivityXri()))).addExpenseRecord(params);
						pm.currentTransaction().commit();
						workAndExpenseRecord = (WorkAndExpenseRecord)pm.getObjectById(result.getWorkRecord().refGetPath());
					}
					this.formFields.setLastCreatedWorkExpenseRecordXri(workAndExpenseRecord.refGetPath().toXRI());
					this.creationFailed = false;
					if (Boolean.TRUE.equals(this.formFields.getMakePrivate())) {
						List<org.opencrx.security.realm1.jmi1.PrincipalGroup> newOwningGroups = new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
						// remove all OwningGroups that are not private
						for (org.opencrx.security.realm1.jmi1.PrincipalGroup currentPrincipalGroup: workAndExpenseRecord.<org.opencrx.security.realm1.jmi1.PrincipalGroup>getOwningGroup()) {
							if (currentPrincipalGroup.getName() != null && (currentPrincipalGroup.getName().toUpperCase().indexOf(PRIVATE_TOKEN) >= 0)) {
									newOwningGroups.add(currentPrincipalGroup);
							}
						}
						// determine primary owning group of principal (if inferrable from Resource)
						String userName = null;
						org.opencrx.security.realm1.jmi1.PrincipalGroup resourcePrincipalGroup = null;
						try {
							org.opencrx.kernel.home1.jmi1.Segment homeSegment =
								(org.opencrx.kernel.home1.jmi1.Segment)pm.getObjectById(
									new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", this.getProviderName(), "segment", this.getSegmentName())
								);
							userName = getUsername(homeSegment, (org.opencrx.kernel.activity1.jmi1.Resource)pm.getObjectById(new Path(this.formFields.getResourceXri())));
							org.openmdx.security.realm1.jmi1.Realm realm = org.opencrx.kernel.backend.SecureObject.getInstance().getRealm(
								pm,
								this.getProviderName(),
								this.getSegmentName()
							);
							if (userName != null) {
								resourcePrincipalGroup = findPrincipalGroup(userName + ".Group", realm, pm);
								if (resourcePrincipalGroup != null) {
									newOwningGroups.add(resourcePrincipalGroup);
								}
							}
						} catch (Exception e) {
								new ServiceException(e).log();
						}
						// set new OwningGroups
						pm.currentTransaction().begin();
						org.opencrx.kernel.base.jmi1.ModifyOwningGroupsParams replaceOwningGroupsParams = Structures.create(
							org.opencrx.kernel.base.jmi1.ModifyOwningGroupsParams.class, 
							Datatypes.member(org.opencrx.kernel.base.jmi1.ModifyOwningGroupsParams.Member.group, newOwningGroups),
							Datatypes.member(org.opencrx.kernel.base.jmi1.ModifyOwningGroupsParams.Member.mode, (short)1)							
						); 
						workAndExpenseRecord.replaceOwningGroup(replaceOwningGroupsParams);
						pm.currentTransaction().commit();
					}
					this.refreshWorkAndExpenseRecords();
				} catch (Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch (Exception er) {}
					this.errorMsg += "could not create Work / Expense Record<br>";
					new ServiceException(e).log();
				}
			}
		}
	}

	/**
	 * Action DeleteWorkRecord.
	 * 
	 * @param formFields
	 * @param deleteWorkRecordXri
	 * @throws ServiceException
	 */
	public void doDeleteWorkRecord(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields,
		@JspWizardController.RequestParameter(name = "deleteWorkRecordXri") String deleteWorkRecordXri
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(formFields);
		if(deleteWorkRecordXri != null && !deleteWorkRecordXri.isEmpty()) {
			try {
				RefObject_1_0 objToDelete = (RefObject_1_0)pm.getObjectById(new Path(deleteWorkRecordXri));
				pm.currentTransaction().begin();
				objToDelete.refDelete();
				pm.currentTransaction().commit();
				this.refreshWorkAndExpenseRecords();
			} catch (Exception e) {
				try {
					pm.currentTransaction().rollback();
				} catch (Exception er) {}
				new ServiceException(e).log();
			}
		}		
	}

	/**
	 * Action Reload.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doReload(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		this.doRefresh(formFields);
	}
	
	/**
	 * Action EvictReload.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doEvictReload(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		this.doRefresh(formFields);
	}

	/**
	 * Action Refresh.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doRefresh(
		@JspWizardController.RequestParameter(type = "Bean") FormFields formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		RefObject_1_0 obj = this.getObject();
		ApplicationContext app = this.getApp();
		this.formFields = formFields;
		if(!Boolean.TRUE.equals(this.formFields.getIsInitialized())) {
			org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, this.getProviderName(), this.getSegmentName());
			try {
				// try to derive initial settings from calling object
				if(obj instanceof Contact) {
					// called from Contact
					this.formFields.setContactXri(obj.refGetPath().toXRI());
				} else if(obj instanceof ActivityTracker) {
					// called from ActivityTracker
					this.formFields.setActivityFilterXri(obj.refGetPath().toXRI());
					this.formFields.setActivityFilter(ACTIVITY_FILTER_TRACKER);
				} else if(obj instanceof ActivityCategory) {
					// called from ActivityCategory
					this.formFields.setActivityFilterXri(obj.refGetPath().toXRI());
					this.formFields.setActivityFilter(ACTIVITY_FILTER_CATEGORY);
				} else if(obj instanceof ActivityMilestone) {
					// called from ActivityMilestone
					this.formFields.setActivityFilterXri(obj.refGetPath().toXRI());
					this.formFields.setActivityFilter(ACTIVITY_FILTER_MILESTONE);
				} else if((obj instanceof Activity) || (obj instanceof ResourceAssignment)) {
					if(obj instanceof ResourceAssignment) {
						if(((ResourceAssignment)obj).getResource() != null) {
							this.formFields.setResourceXri(obj.refGetPath().toXRI());
						}
						this.formFields.setActivityXri(obj.refGetPath().getParent().getParent().toXRI());
					} else {
						// called from Activity
						this.formFields.setActivityXri(obj.refGetPath().toXRI());
					}
					if(this.formFields.getActivityXri() != null && !this.formFields.getActivityXri().isEmpty()) {
						ActivityTracker tracker = null;
						ActivityCategory category = null;
						ActivityMilestone milestone = null;
						// hint: choose any of the assigned activity groups (preference: tracker > category > milestone), otherwise segment
						Activity activity = (Activity)pm.getObjectById(new Path(this.formFields.getActivityXri()));
						for(ActivityGroupAssignment ass: activity.<ActivityGroupAssignment>getAssignedGroup()) {
							if (ass.getActivityGroup() != null) {
								ActivityGroup ag = ass.getActivityGroup();
								if (
									ag instanceof ActivityTracker &&
									(
										((ActivityTracker)ag).isDisabled() == null ||
										!((ActivityTracker)ag).isDisabled().booleanValue()
									)
								) {
									tracker = (ActivityTracker)ag;
								} else if (
									ag instanceof ActivityCategory &&
									(
										((ActivityCategory)ag).isDisabled() == null ||
										!((ActivityCategory)ag).isDisabled().booleanValue()
									)
								) {
									category = (ActivityCategory)ag;
								} else if (
									ag instanceof ActivityMilestone &&
									(
										((ActivityMilestone)ag).isDisabled() == null ||
										!((ActivityMilestone)ag).isDisabled().booleanValue()
									)
								) {
									milestone = (ActivityMilestone)ag;
								}
							}
							if(tracker != null) {
								this.formFields.setActivityFilterXri(tracker.refGetPath().toXRI());
								this.formFields.setActivityFilter(ACTIVITY_FILTER_TRACKER);
							} else if (category != null) {
								this.formFields.setActivityFilterXri(category.refGetPath().toXRI());
								this.formFields.setActivityFilter(ACTIVITY_FILTER_CATEGORY);
							} else if (milestone != null) {
								this.formFields.setActivityFilterXri(milestone.refGetPath().toXRI());
								this.formFields.setActivityFilter(ACTIVITY_FILTER_MILESTONE);
							} else {
								this.formFields.setActivityFilterXri(null);
								this.formFields.setActivityFilter(ACTIVITY_FILTER_SEGMENT);
							}
						}
					}
				} else if (obj instanceof Resource) {
					// called from Resource
					this.formFields.setResourceXri(obj.refGetPath().toXRI());
					if (((Resource)obj).getContact() != null) {
						this.formFields.setContactXri(obj.refGetPath().toXRI());
					}
				}
			} catch (Exception e) {
				new ServiceException(e).log();
			}
			if (this.formFields.getActivityFilter() == null) {
				this.formFields.setActivityFilter(ACTIVITY_FILTER_TRACKER);
			}
			// determine whether there are ActivityTrackers with userString0 != null
			ActivityTrackerQuery trackerFilter = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
			trackerFilter.forAllDisabled().isFalse();
			trackerFilter.thereExistsUserBoolean0().isTrue();
			this.formFields.setHasProjects(!activitySegment.getActivityTracker(trackerFilter).isEmpty());
			this.formFields.setIsWorkRecordInPercent(false);
			this.formFields.setExcludeClosedActivities(true);
			this.formFields.setShowActivityGroupNameFilter(false);
			this.formFields.setIsFullMonth(false);
			this.formFields.setIsWorkRecord(true);
			this.formFields.setIsBillable(true);
		}
		try {
			if (this.formFields.getContactXri() != null && !this.formFields.getContactXri().isEmpty()) {
				if(this.formFields.getContactXri().equals("*")) {
					this.showAllResources = true;
					if (!Boolean.TRUE.equals(this.formFields.getIsResourceChange()) && (this.formFields.getResourceXri() != null && !this.formFields.getResourceXri().isEmpty())) {
						this.formFields.setResourceXri("*");
					}
				} else {
					this.contact = (Contact)pm.getObjectById(new Path(this.formFields.getContactXri()));
				}
			} else if (obj instanceof Contact) {
				this.contact = (Contact)obj;
			} else {
				// default is current users Contact (as defined in current user's UserHome
				// get UserHome
				org.opencrx.kernel.home1.jmi1.UserHome myUserHome = org.opencrx.kernel.backend.UserHomes.getInstance().getUserHome(obj.refGetPath(), pm);
				if (myUserHome.getContact() != null) {
					this.contact = myUserHome.getContact();
				}
			}
		} catch (Exception ignore) {}
		if(!Boolean.TRUE.equals(this.formFields.getIsContactChange())) {
			if ((this.formFields.getResourceXri() != null) && !this.formFields.getResourceXri().isEmpty()) {
				if(this.formFields.getResourceXri().equals("*")) {
					this.showAllResourcesOfContact = true;
				} else {
					try {
						this.resource = (Resource)pm.getObjectById(new Path(this.formFields.getResourceXri()));
						this.contact = this.resource.getContact();
						this.showAllResources = false;
					} catch (Exception e) {}
				}
			}
		}
		if (this.contact == null) {
			this.showAllResources = true;
			this.formFields.setContactXri("*");
			this.formFields.setContactXriTitle("*");
		} else {
			this.formFields.setContactXri(this.contact.refGetPath().toXRI());
			this.formFields.setContactXriTitle(app.getHtmlEncoder().encode(new ObjectReference(this.contact, app).getTitle(), false));
		}
		try {
			if ((this.formFields.getActivityFilterXri() != null) && !this.formFields.getActivityFilterXri().isEmpty()) {
				this.activityGroup = (ActivityGroup)pm.getObjectById(new Path(this.formFields.getActivityFilterXri()));
			}
		} catch (Exception e) {}
		if(this.formFields.getFilterActivityGroupName() == null) {
			this.formFields.setFilterActivityGroupName("");
		}
		this.formFields.setSelectedDateStr(
			this.getDateAsString(
				this.parseDate(this.formFields.getSelectedDateStr())
			)
		);
		if(this.formFields.getActivitySortOrder() == null) {
			this.formFields.setActivitySortOrder(1);			
		}
		if(this.formFields.getRecordType() == null) {
			this.formFields.setRecordType(
				Activities.WorkRecordType.STANDARD.getValue()
			);
		}
		if(this.formFields.getName() == null) {
			this.formFields.setName("");
		}
		if(this.formFields.getBillingCurrency() == null) {
			this.formFields.setBillingCurrency((short)756);
		}
		if(this.formFields.getFilterActivityName() == null) {
			this.formFields.setFilterActivityName("");
		}
		if(this.formFields.getPaymentType() == null) {
			this.formFields.setPaymentType((short)1);
		}
		if(this.formFields.getEffortHH() == null) {
			this.formFields.setEffortHH("8");
		}
		if(this.formFields.getEffortMM() == null) {
			this.formFields.setEffortMM("00");
		}
		if(this.formFields.getQuantPercent() == null) {
			this.formFields.setQuantPercent("100");
		}
		if(this.formFields.getStartedAtHH() == null) {
			this.formFields.setStartedAtHH("08");
		}
		if(this.formFields.getStartedAtMM() == null) {
			this.formFields.setStartedAtMM("00");
		}
		if(this.formFields.getEndedAtHH() == null) {
			this.formFields.setEndedAtHH("");
		}
		if(this.formFields.getEndedAtMM() == null) {
			this.formFields.setEndedAtMM("");
		}
		if(this.formFields.getDescription() == null) {
			this.formFields.setDescription("");
		}
		if(this.formFields.getQuantity() == null) {
			this.formFields.setQuantity("1");
		}
		this.resourceDefaultLoad = 100;
		// Collect projects and activity groups
		{
			int gCounter = 0;
			Map<String,ActivityGroup> sortedActivityGroups = new TreeMap<String,ActivityGroup>();
			Map<String,ActivityTracker> sortedProjects = new TreeMap<String,ActivityTracker>();
			if (CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_ANYGROUP.equals(this.formFields.getActivityFilter())) {
				// get ActivityTrackers
				ActivityTrackerQuery trackerQuery = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
				trackerQuery.forAllDisabled().isFalse();
				for(ActivityGroup ag: this.getActivitySegment().getActivityTracker(trackerQuery)) {
					if (
						!Boolean.TRUE.equals(this.formFields.getShowActivityGroupNameFilter()) ||
						(ag.getName() == null) || (ag.getName().toUpperCase().indexOf(this.formFields.getFilterActivityGroupName().toUpperCase()) >= 0)
					) {
						sortedActivityGroups.put(
							(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
							ag
						);
					}
				}
				// get ActivityCategories
				ActivityCategoryQuery categoryQuery = (ActivityCategoryQuery)pm.newQuery(ActivityCategory.class);
				categoryQuery.forAllDisabled().isFalse();
				for(ActivityGroup ag: this.getActivitySegment().getActivityCategory(categoryQuery)) {
					if (
						!Boolean.TRUE.equals(this.formFields.getShowActivityGroupNameFilter()) ||
						(ag.getName() == null) || (ag.getName().toUpperCase().indexOf(this.formFields.getFilterActivityGroupName().toUpperCase()) >= 0)
					) {
						sortedActivityGroups.put(
							(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
							ag
						);
					}
				}
				// get ActivityMilestones
				ActivityMilestoneQuery milestoneQuery = (ActivityMilestoneQuery)pm.newQuery(ActivityMilestone.class);
				milestoneQuery.forAllDisabled().isFalse();
				for(ActivityGroup ag: this.getActivitySegment().getActivityMilestone(milestoneQuery)) {
					if (
						!Boolean.TRUE.equals(this.formFields.getShowActivityGroupNameFilter()) ||
						(ag.getName() == null) || (ag.getName().toUpperCase().indexOf(this.formFields.getFilterActivityGroupName().toUpperCase()) >= 0)
					) {
						sortedActivityGroups.put(
							(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
							ag
						);
					}
				}
			} else if (CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_PROJECT.equals(this.formFields.getActivityFilter())) {
				// get projects, i.e. ActivityTrackers with userString0 != null
				ActivityTrackerQuery trackerQuery = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
				trackerQuery.forAllDisabled().isFalse();
				trackerQuery.thereExistsUserBoolean0().isTrue();
				trackerQuery.orderByUserString0().ascending();
				for(ActivityTracker at: this.getActivitySegment().getActivityTracker(trackerQuery)) {
					if(at.getUserString1() == null || at.getUserString1().isEmpty()) {
						if(this.formFields.getProjectMain().isEmpty() && at.getUserString0() != null) {
							// set initial value of projectName
							this.formFields.setProjectMain(at.getUserString0().trim());
						}
						sortedProjects.put(
							(at.getUserString0() != null ? at.getUserString0().trim() : "_?"),
							at
						);
					}
				}
				trackerQuery = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
				trackerQuery.forAllDisabled().isFalse();
				trackerQuery.userString1().isNonNull();
				trackerQuery.thereExistsUserString0().equalTo(this.formFields.getProjectMain());
				for(ActivityTracker at: this.getActivitySegment().getActivityTracker(trackerQuery)) {
					if ((at.getUserString1() != null) && (at.getUserString1().length() > 0)) {
						if (
							!Boolean.TRUE.equals(this.formFields.getShowActivityGroupNameFilter()) ||
							(at.getUserString1().toUpperCase().indexOf(this.formFields.getFilterActivityGroupName().toUpperCase()) >= 0)
						) {
							sortedActivityGroups.put(
								(at.getUserString1() != null ? at.getUserString1().trim() : "_?") + "		" + gCounter++,
								at
							);
						}
					}
				}
			} else if (CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_TRACKER.equals(this.formFields.getActivityFilter())) {
				// get ActivityTrackers
				ActivityTrackerQuery trackerQuery = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
				trackerQuery.forAllDisabled().isFalse();
				for(ActivityGroup ag: this.getActivitySegment().getActivityTracker(trackerQuery)) {
					if (
						!Boolean.TRUE.equals(this.formFields.getShowActivityGroupNameFilter()) ||
						(ag.getName() == null) || (ag.getName().toUpperCase().indexOf(this.formFields.getFilterActivityGroupName().toUpperCase()) >= 0)
					) {
						sortedActivityGroups.put(
							(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
							ag
						);
					}
				}
			} else if (CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_CATEGORY.equals(this.formFields.getActivityFilter())) {
				// get ActivityCategories
				ActivityCategoryQuery categoryQuery = (ActivityCategoryQuery)pm.newQuery(ActivityCategory.class);
				categoryQuery.forAllDisabled().isFalse();
				for(ActivityGroup ag: this.getActivitySegment().getActivityCategory(categoryQuery)) {
					if (
						!Boolean.TRUE.equals(this.formFields.getShowActivityGroupNameFilter()) ||
						(ag.getName() == null) || (ag.getName().toUpperCase().indexOf(this.formFields.getFilterActivityGroupName().toUpperCase()) >= 0)
					) {
						sortedActivityGroups.put(
							(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
							ag
						);
					}
				}
			} else if (CreateWorkAndExpenseRecordController.ACTIVITY_FILTER_MILESTONE.equals(this.formFields.getActivityFilter())) {
				// get ActivityMilestones
				ActivityMilestoneQuery milestoneQuery = (ActivityMilestoneQuery)pm.newQuery(ActivityMilestone.class);
				milestoneQuery.forAllDisabled().isFalse();
				for(ActivityGroup ag: this.getActivitySegment().getActivityMilestone(milestoneQuery)) {
					if (
						!Boolean.TRUE.equals(this.formFields.getShowActivityGroupNameFilter()) ||
						(ag.getName() == null) || (ag.getName().toUpperCase().indexOf(this.formFields.getFilterActivityGroupName().toUpperCase()) >= 0)
					) {
						sortedActivityGroups.put(
							(ag.getName() != null ? ag.getName() : "_?") + "		" + gCounter++,
							ag
						);
					}
				}
			}
			this.sortedActivityGroups = sortedActivityGroups.values();
			this.sortedProjects = sortedProjects.values();
		}
		{
			if(this.resource != null && Boolean.TRUE.equals(this.formFields.getIsWorkRecordInPercent())) {
				Resource currentResource = null;
				this.resourceCalendar = null;
				this.resourceCalendarDay = null;
				this.resourceCalendarDayName = null;
				try {
					this.resourceCalendarDayName = this.getCalendarDayFormat().format(this.getDateAsCalendar(this.formFields.getSelectedDateStr(), app).getTime());
				} catch (Exception e) {}
				this.resourceCalendarDayLoad = null;
				this.resourceDefaultLoad = 100;
				// try to get Default Calendar of Resource
				try {
					currentResource = (Resource)pm.getObjectById(new Path(this.formFields.getResourceXri()));
					if (currentResource != null) {
						if (currentResource.getCalendar() != null) {
							this.resourceCalendar = currentResource.getCalendar();
							// get default load from WeekDay 
							WeekDayQuery weekDayQuery = (WeekDayQuery)pm.newQuery(WeekDay.class);
							weekDayQuery.dayOfWeek().equalTo(Short.valueOf((short)this.getDayOfWeek(this.formFields.getSelectedDateStr(), app)));
							Collection<WeekDay> daysOfWeek = this.resourceCalendar.getWeekDay(weekDayQuery);
							if(!daysOfWeek.isEmpty()) {
								WeekDay weekDay = (org.opencrx.kernel.activity1.jmi1.WeekDay)daysOfWeek.iterator().next();
								if (weekDay.getWorkDurationHours() != null) {
									this.resourceDefaultLoad = weekDay.getWorkDurationHours().intValue();
									if (this.resourceDefaultLoad < 0) {
										this.resourceDefaultLoad = 0;
									}
									if (this.resourceDefaultLoad > 100) {
										this.resourceDefaultLoad = 100;
									}
								}
							}
							// try to get CalendarDay
							CalendarDayQuery calendarDayQuery = (CalendarDayQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.CalendarDay.class);
							calendarDayQuery.dateOfDay().equalTo(this.getDateAsCalendar(this.formFields.getSelectedDateStr(), app).getTime());
							Collection<CalendarDay> calendarDays = this.resourceCalendar.getCalendarDay(calendarDayQuery);
							if(!calendarDays.isEmpty()) {
								this.resourceCalendarDay = calendarDays.iterator().next();
								this.resourceCalendarDayName = this.resourceCalendarDay.getName();
								try {
									if(this.resourceCalendarDayName != null) {
										String[] calDayNameSplit = this.resourceCalendarDayName.split("@");
										if (calDayNameSplit.length >= 2) {
											this.resourceCalendarDayName = calDayNameSplit[0];
											this.resourceCalendarDayLoad = Integer.valueOf(calDayNameSplit[1]);
										}
									}
								} catch (Exception e) {}
							}
						}
						if(this.resourceCalendar != null && this.resourceCalendarDay != null) {
							String calendarDayName = null;
							if(this.formFields.getUpdateCalendarDay() != null && this.formFields.getUpdateCalendarDay().length() > 9) {
								calendarDayName = this.formFields.getUpdateCalendarDay();
							}
							if(calendarDayName != null && !calendarDayName.equals(this.resourceCalendarDay.getName())) {
								try {
									this.resourceCalendarDay = this.createOrUpdateCalendarDay(
										calendarDayName,
										calendarDayName.substring(0,8),
										this.resourceCalendar,
										this.resourceCalendarDay,
										this.getActivitySegment()
									);
								} catch (Exception e) {
									new ServiceException(e).log();
								}
								String[] calDayNameSplit = calendarDayName.split("@");
								if (calDayNameSplit.length >= 2) {
									this.resourceCalendarDayName = calDayNameSplit[0];
									this.resourceCalendarDayLoad = Integer.valueOf(calDayNameSplit[1]);
								}
							}
						}
						if(this.resourceCalendar != null && this.resourceCalendarDay == null) {
							String calendarDayName = null;
							if(this.formFields.getCreateCalendarDay() != null && this.formFields.getCreateCalendarDay().length() > 9) {
								calendarDayName = this.formFields.getCreateCalendarDay();
							}
							if(calendarDayName != null) {
								try {
									this.resourceCalendarDay = this.createOrUpdateCalendarDay(
										calendarDayName,
										calendarDayName.substring(0,8),
										this.resourceCalendar,
										null,
										this.getActivitySegment()
									);
								} catch (Exception e) {
									new ServiceException(e).log();
								}
							}
						}
					}
				} catch (Exception ignore) {}
			}
		}
		{
			this.resourceRate = this.formFields.getRate();
			if (
				this.isRecordTypeChange() ||
				!this.formFields.getResourceXri().equals(this.formFields.getPreviousResourceXri())
			) {
				// resource changed, get rate
				if((this.formFields.getResourceXri() != null) && !this.formFields.getResourceXri().isEmpty() && !this.formFields.getResourceXri().equals("*")) {
					org.opencrx.kernel.activity1.jmi1.Resource res = (org.opencrx.kernel.activity1.jmi1.Resource)pm.getObjectById(new Path(this.formFields.getResourceXri()));
					try {
						if(this.formFields.getRecordType() != null) {
							ResourceRateQuery query = (ResourceRateQuery)pm.newQuery(ResourceRate.class);
							query.rateType().equalTo(this.formFields.getRecordType());
							List<ResourceRate> resourceRates = res.getResourceRate(query);
							if(!resourceRates.isEmpty()) {
								ResourceRate resourceRate = resourceRates.iterator().next();
								this.resourceRate = resourceRate.getRate() != null 
									? this.getQuantityFormat().format(resourceRate.getRate()) 
									: "0.000";
								this.formFields.setBillingCurrency(resourceRate.getRateCurrency());
							} else {
								this.resourceRate = "0.000";
							}
						}
					} catch (Exception ignore) {}
				}
			}
		}
		{
			org.opencrx.kernel.activity1.jmi1.Activity selectedActivity = null;
			this.showMakePrivate = false;
			boolean hasPrivateOwningGroup = false;
			boolean atLeastOnePrivateMatch = false; // true if current principal is member of at least one private group which is also an owning group of the selected activity
			List<org.opencrx.security.realm1.jmi1.PrincipalGroup> privateOwningGroups = new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
			if (this.formFields.getActivityXri() != null && !this.formFields.getActivityXri().isEmpty()) {
				selectedActivity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(this.formFields.getActivityXri()));
				if(!this.showMakePrivate) {
					try {
						for (org.opencrx.security.realm1.jmi1.PrincipalGroup currentPrincipalGroup: selectedActivity.<org.opencrx.security.realm1.jmi1.PrincipalGroup>getOwningGroup()) {
							if (currentPrincipalGroup.getName() != null && (currentPrincipalGroup.getName().toUpperCase().indexOf(CreateWorkAndExpenseRecordController.PRIVATE_TOKEN) >= 0)) {
								privateOwningGroups.add(currentPrincipalGroup);
								hasPrivateOwningGroup = true;
								break;
							}
						}
					} catch(Exception ignore) {}
				}
			}
			this.groupNames = "";
			org.opencrx.kernel.home1.jmi1.UserHome myUserHome = null;
			try {
				// get UserHome
				myUserHome = org.opencrx.kernel.backend.UserHomes.getInstance().getUserHome(obj.refGetPath(), pm);
			} catch (Exception e) {
				new ServiceException(e).log();
			};
			org.openmdx.security.realm1.jmi1.Principal principal = null;
			try {
				org.openmdx.security.realm1.jmi1.Realm realm = org.opencrx.kernel.backend.SecureObject.getInstance().getRealm(
					pm,
					this.getProviderName(),
					this.getSegmentName()
				);
				principal = org.opencrx.kernel.backend.SecureObject.getInstance().findPrincipal(
					myUserHome.refGetPath().getLastSegment().toString(),
					realm
				);
				// check whether user is member of at least one private group which is also an owning group of the selected activity
				for (org.opencrx.security.realm1.jmi1.PrincipalGroup currentPrivateOwningGroup: privateOwningGroups) {
					if (!this.groupNames.isEmpty()) {this.groupNames += ", ";}
					this.groupNames += currentPrivateOwningGroup.getName();
					if(principal.getIsMemberOf().contains(currentPrivateOwningGroup)) {
						atLeastOnePrivateMatch = true;
					}
				}
			} catch (Exception e) {
				new ServiceException(e).log();
			};
			this.showMakePrivate = hasPrivateOwningGroup && atLeastOnePrivateMatch;
			if (!this.showMakePrivate) {
				// reset makePrivate
				this.formFields.setMakePrivate(false);
			}
		}
		this.refreshWorkAndExpenseRecords();
		{
			// try to parse startedAt
			this.startedAt = null;
			try {
				this.startedAt = this.getDateAsCalendar(this.formFields.getSelectedDateStr(), app);
				int numValueMM = Integer.parseInt(this.formFields.getStartedAtMM());
				if (numValueMM < 0) {
					numValueMM = 0;
					this.formFields.setStartedAtMM("00");
				} else if (numValueMM > 59) {
					numValueMM = 59;
					this.formFields.setStartedAtMM("59");
				}
				this.startedAt.set(GregorianCalendar.MINUTE, numValueMM);
				int numValueHH = Integer.parseInt(this.formFields.getStartedAtHH());
				if (numValueHH < 0) {
					numValueHH = 0;
					this.formFields.setStartedAtHH("00");
				} else if (numValueHH > 24) {
					numValueHH = 24;
					this.formFields.setStartedAtHH("24");
				}
				if ((numValueHH == 24) && (numValueMM > 0)) {
					numValueHH = 23;
					this.formFields.setStartedAtHH("23");
				}
				this.startedAt.set(GregorianCalendar.HOUR_OF_DAY, numValueHH);
			} catch (Exception e) {
				this.errorMsg += "started at is mandatory<br>";
				this.startedAt = null;
			}
			// try to parse endedAt
			try {
				this.endedAt = this.getDateAsCalendar(this.formFields.getSelectedDateStr(), app);
				int numValueMM = Integer.parseInt(this.formFields.getEndedAtMM());
				if (numValueMM < 0) {
					numValueMM = 0;
					this.formFields.setEndedAtMM("00");
				} else if (numValueMM > 59) {
					numValueMM = 59;
					this.formFields.setEndedAtMM("59");
				}
				this.endedAt.set(GregorianCalendar.MINUTE, numValueMM);
				int numValueHH = Integer.parseInt(this.formFields.getEndedAtHH());
				if (numValueHH < 0) {
					numValueHH = 0;
					this.formFields.setEndedAtHH("00");
				} else if (numValueHH > 24) {
					numValueHH = 24;
					this.formFields.setEndedAtHH("24");
				}
				if ((numValueHH == 24) && (numValueMM > 0)) {
					numValueHH = 23;
					this.formFields.setEndedAtHH("23");
				}
				this.endedAt.set(GregorianCalendar.HOUR_OF_DAY, numValueHH);
			} catch (Exception e) {
				this.endedAt = null; // this is an optional attribute
				this.formFields.setEndedAtHH("");
				this.formFields.setEndedAtMM("");
			}
		}
		{
			// try to parse paraEffort
			this.paraEffortHH = null;
			this.paraEffortMM = null;
			try {
				short numValueMM = Short.parseShort(this.formFields.getEffortMM());
				if (numValueMM < 0) {
						numValueMM = 0;
						this.formFields.setEffortMM("00");
				} else if (numValueMM > 59) {
						numValueMM = 59;
						this.formFields.setEffortMM("59");
				}
				this.paraEffortMM = numValueMM;

				short numValueHH = Short.parseShort(this.formFields.getEffortHH());
				if (numValueHH < 0) {
						numValueHH = 0;
						this.formFields.setEffortHH("00");
				}
				this.paraEffortHH = numValueHH;
			} catch (Exception e) {
				this.paraEffortHH = null;
				this.paraEffortMM = null;
				this.formFields.setEffortHH("0");
				this.formFields.setEffortMM("00");
			}
			// try to parse rate
			this.paraRate = null;
			try {
				this.paraRate = new java.math.BigDecimal(this.formFields.getRate());
			} catch (Exception e) {
				this.paraRate = null;
				if (!this.formFields.getIsWorkRecord()) {
					this.errorMsg += "rate is mandatory!<br>";
				}
			}
			// try to parse quantity
			this.paraQuantity = null;
			this.quantityIsZero = false;
			try {
				this.paraQuantity = new java.math.BigDecimal(this.formFields.getQuantity());
					if (!this.formFields.getIsWorkRecord() && this.paraQuantity.compareTo(java.math.BigDecimal.ZERO) == 0) {
						this.errorMsg += "quantity is 0!<br>";
						this.quantityIsZero = true;
					}
			} catch (Exception e) {
				this.paraQuantity = null;
			}
			// try to parse quantPercent
			this.quantPercentageIsZero = false;
			this.paraQuantPercentage = null;
			try {
				short numValuePercentage = Short.parseShort(this.formFields.getQuantPercent());
				if (numValuePercentage <= 0) {
					numValuePercentage = 0;
					this.formFields.setQuantPercent("0");
					this.quantPercentageIsZero = true;
				} else if (numValuePercentage > 100) {
					numValuePercentage = 100;
					this.formFields.setQuantPercent("100");
				}
				this.paraQuantPercentage = numValuePercentage;
			} catch (Exception e) {
				this.paraQuantPercentage = null;
				this.formFields.setQuantPercent("100");
			}			
		}
	}

	/**
	 * Action Cancel.
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
	 * Get activity segment.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public org.opencrx.kernel.activity1.jmi1.Segment getActivitySegment(
	) throws ServiceException {
		return Activities.getInstance().getActivitySegment(this.getPm(), this.getProviderName(), this.getSegmentName());
	}
	
	/**
	 * @param s
	 */
	public void appendErrorMsg(
		String s
	) {
		this.errorMsg += s;
	}

	/**
	 * @return
	 */
	public boolean isRecordTypeChange(
	) {
		return !Boolean.TRUE.equals(this.formFields.getIsInitialized()) || this.formFields.getRecordType() != this.formFields.getPreviousRecordType();
	}
	
	/**
	 * @return
	 */
	public String getErrorMsg(
	) {
    	return this.errorMsg;
    }

	/**
	 * @return
	 */
	public SimpleDateFormat getMonthFormat(
	) {
    	return this.monthFormat;
    }

	/**
	 * @return
	 */
	public SimpleDateFormat getDayInWeekFormat(
	) {
    	return this.dayInWeekFormat;
    }

	/**
	 * @return
	 */
	public Contact getContact(
	) {
    	return this.contact;
    }

	/**
	 * @return
	 */
	public Boolean getShowAllResourcesOfContact(
	) {
    	return this.showAllResourcesOfContact;
    }

	/**
	 * @return
	 */
	public ActivityGroup getActivityGroup(
	) {
    	return this.activityGroup;
    }

	/**
	 * @return
	 */
	public Boolean getShowAllResources(
	) {
    	return this.showAllResources;
    }

	/**
	 * @return
	 */
	public Collection<ActivityGroup> getSortedActivityGroups(
	) {
    	return this.sortedActivityGroups;
    }

	/**
	 * @return
	 */
	public Collection<ActivityTracker> getSortedProjects(
	) {
    	return sortedProjects;
    }

	/**
	 * @return the resource
	 */
	public Resource getResource() {
		return this.resource;
	}

	/**
	 * @return the quantityFormat
	 */
	public NumberFormat getQuantityFormat(
	) {
		return this.quantityFormat;
	}

	/**
	 * @return the dateFormat
	 */
	public SimpleDateFormat getDateFormat(
	) {
		return this.dateFormat;
	}

	/**
	 * @return the calendarDayFormat
	 */
	public SimpleDateFormat getCalendarDayFormat(
	) {
		return this.calendarDayFormat;
	}

	/**
	 * @return the dateTimeFormat
	 */
	public SimpleDateFormat getDateTimeFormat(
	) {
		return this.dateTimeFormat;
	}

	/**
	 * @return the weekDayFormat
	 */
	public SimpleDateFormat getWeekDayFormat(
	) {
		return this.weekDayFormat;
	}

	/**
	 * @return the formatter
	 */
	public NumberFormat getFormatter(
	) {
		return this.formatter;
	}

	/**
	 * @return the ratesEpFormat
	 */
	public NumberFormat getRatesEpFormat(
	) {
		return this.ratesEpFormat;
	}

	/**
	 * @return the formatter0
	 */
	public NumberFormat getFormatter0(
	) {
		return this.formatter0;
	}

	/**
	 * @return the dateTimeSortFormat
	 */
	public SimpleDateFormat getDateTimeSortFormat(
	) {
		return this.dateTimeSortFormat;
	}

	/**
	 * @return the dateOnlyFormat
	 */
	public SimpleDateFormat getDateOnlyFormat(
	) {
		return this.dateOnlyFormat;
	}

	/**
	 * @return the creationFailed
	 */
	public Boolean getCreationFailed(
	) {
		return this.creationFailed;
	}

	/**
	 * @return the canExecuteAdd
	 */
	public Boolean getCanExecuteAdd(
	) {
		return this.canExecuteAdd;
	}

	/**
	 * @return the uomPercent
	 */
	public Uom getUomPercent(
	) {
		return this.uomPercent;
	}

	/**
	 * @return the paraRate
	 */
	public BigDecimal getParaRate(
	) {
		return this.paraRate;
	}

	/**
	 * @return the startedAt
	 */
	public GregorianCalendar getStartedAt(
	) {
		return this.startedAt;
	}

	/**
	 * @return the paraQuantity
	 */
	public BigDecimal getParaQuantity(
	) {
		return this.paraQuantity;
	}

	/**
	 * @return the quantityIsZero
	 */
	public Boolean getQuantityIsZero(
	) {
		return this.quantityIsZero;
	}

	/**
	 * @return the paraEffortHH
	 */
	public Short getParaEffortHH(
	) {
		return this.paraEffortHH;
	}

	/**
	 * @return the paraEffortMM
	 */
	public Short getParaEffortMM(
	) {
		return this.paraEffortMM;
	}
	
	/**
	 * @return the resourceCalendar
	 */
	public org.opencrx.kernel.activity1.jmi1.Calendar getResourceCalendar(
	) {
		return this.resourceCalendar;
	}

	/**
	 * @return the resourceCalendarDay
	 */
	public org.opencrx.kernel.activity1.jmi1.CalendarDay getResourceCalendarDay(
	) {
		return this.resourceCalendarDay;
	}

	/**
	 * @return the resourceDefaultLoad
	 */
	public Integer getResourceDefaultLoad(
	) {
		return this.resourceDefaultLoad;
	}

	/**
	 * @return the resourceCalendarDayName
	 */
	public String getResourceCalendarDayName(
	) {
		return this.resourceCalendarDayName;
	}

	/**
	 * @return the resourceCalendarDayLoad
	 */
	public Integer getResourceCalendarDayLoad(
	) {
		return this.resourceCalendarDayLoad;
	}

	/**
	 * @return the resourceRate
	 */
	public String getResourceRate(
	) {
		return this.resourceRate;
	}

	/**
	 * @return the groupNames
	 */
	public String getGroupNames(
	) {
		return this.groupNames;
	}

	/**
	 * @return the showMakePrivate
	 */
	public Boolean getShowMakePrivate(
	) {
		return this.showMakePrivate;
	}

	/**
	 * @return the workAndExpenseRecords
	 */
	public Map<String, WorkAndExpenseRecord> getWorkAndExpenseRecords(
	) {
		return this.workAndExpenseRecords;
	}

	/**
	 * @return the calendarBeginOfPeriod
	 */
	public GregorianCalendar getCalendarBeginOfPeriod(
	) {
		return this.calendarBeginOfPeriod;
	}

	/**
	 * @return the calendarEndOfPeriod
	 */
	public GregorianCalendar getCalendarEndOfPeriod(
	) {
		return this.calendarEndOfPeriod;
	}

	/**
	 * @return the calendarBeginOfWeek
	 */
	public GregorianCalendar getCalendarBeginOfWeek(
	) {
		return this.calendarBeginOfWeek;
	}

	/**
	 * @return the sumDays
	 */
	public double[] getSumDays(
	) {
		return this.sumDays;
	}

	/**
	 * @return the sumDaysBillable
	 */
	public double[] getSumDaysBillable(
	) {
		return this.sumDaysBillable;
	}

	/**
	 * @return the formFields
	 */
	public FormFields getFormFields() {
		return formFields;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String ACTIVITY_CLASS = "org:opencrx:kernel:activity1:Activity";
	public static final String ACTIVITYFILTERGLOBAL_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGlobal";
	public static final String ACTIVITYFILTERGROUP_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGroup";
	public static final String ACTIVITYSEGMENT_CLASS = "org:opencrx:kernel:activity1:Segment";
	public static final String ACTIVITYGROUPASSIGNMENT_CLASS = "org:opencrx:kernel:activity1:ActivityGroupAssignment";
	public static final String ACTIVITYTRACKER_CLASS = "org:opencrx:kernel:activity1:ActivityTracker";
	public static final String ACTIVITYCATEGORY_CLASS = "org:opencrx:kernel:activity1:ActivityCategory";
	public static final String ACTIVITYMILESTONE_CLASS = "org:opencrx:kernel:activity1:ActivityMilestone";
	public static final String DISABLED_FILTER_PROPERTY_CLASS = "org:opencrx:kernel:activity1:DisabledFilterProperty";
	public static final String RESOURCE_CLASS = "org:opencrx:kernel:activity1:Resource";
	public static final String CALENDARDAY_CLASS = "org:opencrx:kernel:activity1:CalendarDay";
	public static final String ACCOUNT_CLASS = "org:opencrx:kernel:account1:Account";
	public static final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
	public static final String GROUP_CLASS = "org:opencrx:kernel:account1:Group";
	public static final String WORKANDEXPENSERECORD_CLASS = "org:opencrx:kernel:activity1:WorkAndExpenseRecord";
	public static final int RECORDTYPE_WORK_MAX = 99; // <=99 --> WorkRecord, >= 100 --> ExpenseRecord
	public static final String[] UOM_NAMES = {
		"s", "min", "hour", "day",
		"m", "km", "mile", "feet", "inch",
		"kg",
		"Piece(s)", "Unit(s)"
	};
	public static final String PRIVATE_TOKEN = "PRIVATE";
	public static final String ACTIVITY_FILTER_SEGMENT = "Segment";
	public static final String ACTIVITY_FILTER_ANYGROUP = "AnyGroup";
	public static final String ACTIVITY_FILTER_TRACKER = "Tracker";
	public static final String ACTIVITY_FILTER_PROJECT = "Project";
	public static final String ACTIVITY_FILTER_CATEGORY = "Category";
	public static final String ACTIVITY_FILTER_MILESTONE = "Milestone";

	private FormFields formFields;
	private Contact contact;
	private Resource resource;
	private	ActivityGroup activityGroup;
	private Boolean showAllResources;
	private Boolean showAllResourcesOfContact;
	private Boolean showMakePrivate;
	private Boolean creationFailed;
	private Boolean canExecuteAdd;
	private BigDecimal paraRate;
	private BigDecimal paraQuantity;
	private Short paraQuantPercentage;
	private GregorianCalendar startedAt;
	private GregorianCalendar endedAt;
	private Boolean quantPercentageIsZero;
	private Boolean quantityIsZero;
	private Short paraEffortHH;
	private Short paraEffortMM;
	private String errorMsg = "";
	private Collection<ActivityGroup> sortedActivityGroups;
	private Collection<ActivityTracker> sortedProjects;	
	private Map<String,WorkAndExpenseRecord> workAndExpenseRecords;
	private org.opencrx.kernel.activity1.jmi1.Calendar resourceCalendar;
	private org.opencrx.kernel.activity1.jmi1.CalendarDay resourceCalendarDay;
	private String resourceRate;
	private String resourceCalendarDayName;
	private Integer resourceCalendarDayLoad;
	private Integer resourceDefaultLoad;
	private String groupNames;
	private GregorianCalendar calendarBeginOfPeriod;
	private GregorianCalendar calendarEndOfPeriod;
	private GregorianCalendar calendarBeginOfWeek;
	private double[] sumDays;
	private double[] sumDaysBillable;
	protected TimeZone timezone;
	private SimpleDateFormat dtf;
	private SimpleDateFormat monthFormat;
	private SimpleDateFormat dayInWeekFormat;
	private SimpleDateFormat weekDayFormat;
	private SimpleDateFormat dateOnlyFormat;
	private SimpleDateFormat dateTimeFormat;
	private SimpleDateFormat dateFormat;
	private SimpleDateFormat dateTimeSortFormat;
	private SimpleDateFormat calendarDayFormat;
	private NumberFormat quantityFormat;
	private NumberFormat formatter = new DecimalFormat("00000");
	private NumberFormat ratesEpFormat = new DecimalFormat("#,##0.00");
	private NumberFormat formatter0 = new DecimalFormat("0");
	private Uom uomPercent;
}
