/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: Activities
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.mail.Address;
import javax.mail.Message;
import javax.mail.Message.RecipientType;
import javax.mail.MessagingException;
import javax.mail.Part;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.InternetHeaders;
import javax.mail.internet.MailDateFormat;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimePart;
import javax.mail.internet.MimeUtility;

import org.omg.mof.spi.Names;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.activity1.cci2.AbstractEMailRecipientQuery;
import org.opencrx.kernel.activity1.cci2.AbstractPhoneCallRecipientQuery;
import org.opencrx.kernel.activity1.cci2.ActivityLinkToQuery;
import org.opencrx.kernel.activity1.cci2.ActivityProcessActionQuery;
import org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.cci2.AddressGroupMemberQuery;
import org.opencrx.kernel.activity1.cci2.EMailQuery;
import org.opencrx.kernel.activity1.cci2.EMailRecipientQuery;
import org.opencrx.kernel.activity1.cci2.IncidentPartyQuery;
import org.opencrx.kernel.activity1.cci2.MailingRecipientQuery;
import org.opencrx.kernel.activity1.cci2.MeetingPartyQuery;
import org.opencrx.kernel.activity1.cci2.ResourceAssignmentQuery;
import org.opencrx.kernel.activity1.cci2.ResourceQuery;
import org.opencrx.kernel.activity1.cci2.ResourceRateQuery;
import org.opencrx.kernel.activity1.cci2.TaskPartyQuery;
import org.opencrx.kernel.activity1.cci2.WorkAndExpenseRecordQuery;
import org.opencrx.kernel.activity1.jmi1.AbstractActivityParty;
import org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient;
import org.opencrx.kernel.activity1.jmi1.AbstractFilterActivity;
import org.opencrx.kernel.activity1.jmi1.AbstractPhoneCallRecipient;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityCreationAction;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.activity1.jmi1.ActivityLinkTo;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityProcess;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessAction;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.ActivityType;
import org.opencrx.kernel.activity1.jmi1.ActivityVote;
import org.opencrx.kernel.activity1.jmi1.ActivityWorkRecord;
import org.opencrx.kernel.activity1.jmi1.AddressGroup;
import org.opencrx.kernel.activity1.jmi1.AddressGroupMember;
import org.opencrx.kernel.activity1.jmi1.Calendar;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.EMailRecipient;
import org.opencrx.kernel.activity1.jmi1.EMailRecipientGroup;
import org.opencrx.kernel.activity1.jmi1.EffortEstimate;
import org.opencrx.kernel.activity1.jmi1.Incident;
import org.opencrx.kernel.activity1.jmi1.IncidentParty;
import org.opencrx.kernel.activity1.jmi1.LinkedActivityFollowUpAction;
import org.opencrx.kernel.activity1.jmi1.Mailing;
import org.opencrx.kernel.activity1.jmi1.MailingRecipient;
import org.opencrx.kernel.activity1.jmi1.Meeting;
import org.opencrx.kernel.activity1.jmi1.MeetingParty;
import org.opencrx.kernel.activity1.jmi1.NewActivityParams;
import org.opencrx.kernel.activity1.jmi1.NewActivityResult;
import org.opencrx.kernel.activity1.jmi1.PhoneCall;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.activity1.jmi1.ResourceAssignment;
import org.opencrx.kernel.activity1.jmi1.ResourceRate;
import org.opencrx.kernel.activity1.jmi1.Segment;
import org.opencrx.kernel.activity1.jmi1.SetActualEndAction;
import org.opencrx.kernel.activity1.jmi1.SetActualStartAction;
import org.opencrx.kernel.activity1.jmi1.SetAssignedToAction;
import org.opencrx.kernel.activity1.jmi1.SubActivity;
import org.opencrx.kernel.activity1.jmi1.SubActivityTransition;
import org.opencrx.kernel.activity1.jmi1.Task;
import org.opencrx.kernel.activity1.jmi1.TaskParty;
import org.opencrx.kernel.activity1.jmi1.WeekDay;
import org.opencrx.kernel.activity1.jmi1.WfAction;
import org.opencrx.kernel.activity1.jmi1.WorkAndExpenseRecord;
import org.opencrx.kernel.address1.jmi1.EMailAddressable;
import org.opencrx.kernel.backend.Depots.BookingType;
import org.opencrx.kernel.backend.Depots.DepotUsage;
import org.opencrx.kernel.backend.ICalendar.ICalClass;
import org.opencrx.kernel.depot1.jmi1.CompoundBooking;
import org.opencrx.kernel.depot1.jmi1.Depot;
import org.opencrx.kernel.depot1.jmi1.DepotEntity;
import org.opencrx.kernel.depot1.jmi1.DepotPosition;
import org.opencrx.kernel.depot1.jmi1.DepotReference;
import org.opencrx.kernel.generic.OpenCrxException;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.generic.SecurityKeys.Action;
import org.opencrx.kernel.generic.jmi1.Media;
import org.opencrx.kernel.generic.jmi1.Note;
import org.opencrx.kernel.generic.jmi1.PropertySet;
import org.opencrx.kernel.home1.jmi1.EMailAccount;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.opencrx.kernel.utils.MimeUtils;
import org.opencrx.kernel.utils.QuotaByteArrayOutputStream;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.kernel.workflow1.jmi1.WfProcess;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.base.text.conversion.XMLEncoder;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.loading.Classes;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * Default Activities backend implementation. Methods of this class
 * should only be used by backend extensions. Some of the methods
 * can also be used in wizards, servlets, etc. However, they must
 * be used with care. Methods of this class are not part of the
 * openCRX API.
 *
 */
public class Activities extends AbstractImpl {

	/**
	 * Register this Activities backend implementation.
	 */
	public static void register(
	) {
		registerImpl(new Activities());
	}
	
	/**
	 * Get instance of the currently registered Activities backend implementation.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Activities getInstance(
	) throws ServiceException {
		return getInstance(Activities.class);
	}

	/**
	 * Constructor
	 */
	protected Activities(
	) {
		
	}
	
    /**
     * Refresh items for the given activity tracker.
     * 
     * @param activityTracker
     * @throws ServiceException
     */
    public void refreshItems(
        ActivityTracker activityTracker
    ) throws ServiceException {
        this.refreshTracker(
        	activityTracker
        );
    }

    /**
     * Mark given activity as dirty, i.e. 'touch' given activity. This will invoke
     * the pre-store callback on commit.
     * 
     * @param activity
     * @throws ServiceException
     */
    public void markActivityAsDirty(
        Activity activity
    ) throws ServiceException {
    	activity.setActivityState(activity.getActivityState());
    }

    /**
     * Find activity type according to given name.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    @Deprecated
    public ActivityType findActivityType(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        javax.jdo.PersistenceManager pm
    ) {
    	return this.findActivityType(name, segment);
    }

    /**
     * Find activity type according to given name.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    public ActivityType findActivityType(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        org.opencrx.kernel.activity1.cci2.ActivityTypeQuery activityTypeQuery = 
        	(org.opencrx.kernel.activity1.cci2.ActivityTypeQuery)pm.newQuery(ActivityType.class);
        activityTypeQuery.name().equalTo(name);
        List<ActivityType> activityTypes = segment.getActivityType(activityTypeQuery);
        return activityTypes.isEmpty() ? 
        	null : 
        	activityTypes.iterator().next();
    }

    /**
     * Find activity process according to given name.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    @Deprecated
    public ActivityProcess findActivityProcess(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        javax.jdo.PersistenceManager pm
    ) {
    	return this.findActivityProcess(name, segment);
    }

    /**
     * Find activity process according to given name.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    public ActivityProcess findActivityProcess(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        org.opencrx.kernel.activity1.cci2.ActivityProcessQuery activityProcessQuery = 
        	(org.opencrx.kernel.activity1.cci2.ActivityProcessQuery)pm.newQuery(ActivityProcess.class);
        activityProcessQuery.name().equalTo(name);
        List<ActivityProcess> activityProcesses = segment.getActivityProcess(activityProcessQuery);
        return activityProcesses.isEmpty() ? 
        	null : 
        	activityProcesses.iterator().next();
    }

    /**
     * Find activity process transition according to given name.
     * 
     * @param activity
     * @param transitionName
     * @return
     */
    public ActivityProcessTransition findActivityProcessTransition(
    	Activity activity,
    	String transitionName
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
    	ActivityProcessState state = activity.getProcessState();
    	ActivityProcess process = (ActivityProcess)pm.getObjectById(state.refGetPath().getParent().getParent());    	
    	ActivityProcessTransitionQuery transitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
    	transitionQuery.name().equalTo(transitionName);
    	List<ActivityProcessTransition> transitions = process.getTransition(transitionQuery);
    	if(!transitions.isEmpty()) {
    		return transitions.iterator().next();
    	}
    	return null;
    }
        
    /**
     * Find activity creator according to given name.
     * 
     * @param name
     * @param segment
     * @return
     */
    public ActivityCreator findActivityCreator(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery activityCreatorQuery = 
        	(org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery)pm.newQuery(ActivityCreator.class);
        activityCreatorQuery.name().equalTo(name);
        List<ActivityCreator> activityCreators = segment.getActivityCreator(activityCreatorQuery);
        return activityCreators.isEmpty() ? 
        	null : 
        	activityCreators.iterator().next();
    }

    /**
     * @deprecated use {@link #findActivityTracker(String, Segment)} instead.
     */
    @Deprecated
    public ActivityTracker findActivityTracker(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        javax.jdo.PersistenceManager pm
    ) {
    	return this.findActivityTracker(name, segment);
    }

    /**
     * Find activity tracker according to given name.
     * 
     * @param name
     * @param segment
     * @return
     */
    public ActivityTracker findActivityTracker(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment
    ) {
    	return this.findActivityTracker(name, segment, (ContextCapable)null);
    }
    
    /**
     * Find activity tracker according to given name and creation context.
     * 
     * @param name
     * @param segment
     * @param creationContext
     * @return
     */
    public ActivityTracker findActivityTracker(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        ContextCapable creationContext
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery activityTrackerQuery = 
        	(org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
        activityTrackerQuery.name().equalTo(name);
        if(creationContext != null) {
        	activityTrackerQuery.thereExistsCreationContext().equalTo(creationContext);
        }
        List<ActivityTracker> activityTrackers = segment.getActivityTracker(activityTrackerQuery);
        return activityTrackers.isEmpty() ? 
        	null : 
        	activityTrackers.iterator().next();
    }

    /**
     * Find activity milestone according to given name.
     * 
     * @param name
     * @param segment
     * @return
     */
    public ActivityMilestone findActivityMilestone(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment
    ) {
    	return this.findActivityMilestone(name, segment, (ContextCapable)null);
    }
    
    /**
     * Find activity milestone according to given name and creation context.
     * 
     * @param name
     * @param segment
     * @param creationContext
     * @return
     */
    public ActivityMilestone findActivityMilestone(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        ContextCapable creationContext
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery activityTrackerQuery = 
        	(org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery)pm.newQuery(ActivityMilestone.class);
        activityTrackerQuery.name().equalTo(name);
        if(creationContext != null) {
        	activityTrackerQuery.thereExistsCreationContext().equalTo(creationContext);
        }
        List<ActivityMilestone> activityMilestones = segment.getActivityMilestone(activityTrackerQuery);
        return activityMilestones.isEmpty() ? 
        	null : 
        	activityMilestones.iterator().next();
    }

    /**
     * @deprecated use {@link #findActivityCategory(String, Segment)} instead
     */
    @Deprecated
    public ActivityCategory findActivityCategory(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        javax.jdo.PersistenceManager pm
    ) {
    	return this.findActivityCategory(name, segment);
    }

    /**
     * Find activity category according to given name.
     * 
     * @param name
     * @param segment
     * @return
     */
    public ActivityCategory findActivityCategory(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment
    ) {
    	return this.findActivityCategory(name, segment, (ContextCapable)null);
    }
    
    /**
     * Find activity category according to given name.
     * 
     * @param name
     * @param segment
     * @param creationContext
     * @return
     */
    public ActivityCategory findActivityCategory(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        ContextCapable creationContext
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);    	
        org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery activityCategoryQuery = 
        	(org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery)pm.newQuery(ActivityCategory.class);
        activityCategoryQuery.name().equalTo(name);
        if(creationContext != null) {
        	activityCategoryQuery.thereExistsCreationContext().equalTo(creationContext);
        }
        List<ActivityCategory> activityCategories = segment.getActivityCategory(activityCategoryQuery);
        return activityCategories.isEmpty() ? 
        	null : 
        	activityCategories.iterator().next();
    }

    /**
     * @deprecated use {@link #findCalendar(String, Segment)} instead.
     */
    @Deprecated
    public Calendar findCalendar(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment,
        javax.jdo.PersistenceManager pm
    ) {
    	return this.findCalendar(name, segment);
    }
    
    /**
     * Find calendar according to given name.
     * 
     * @param name
     * @param segment
     * @return
     */
    public Calendar findCalendar(
        String name,
        org.opencrx.kernel.activity1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        org.opencrx.kernel.activity1.cci2.CalendarQuery calendarQuery = 
        	(org.opencrx.kernel.activity1.cci2.CalendarQuery)pm.newQuery(Calendar.class);
        calendarQuery.name().equalTo(name);
        List<Calendar> calendars = segment.getCalendar(calendarQuery);
        return calendars.isEmpty() ? 
        	null : 
        	calendars.iterator().next();
    }
    
    /**
     * @deprecated use {@link #initCalendar(String, PersistenceManager, String, String, List, short)} instead.
     */
    @Deprecated
    public Calendar initCalendar(
        String calendarName,
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
    	return this.initCalendar(
    		calendarName, 
    		pm, 
    		providerName, 
    		segmentName, 
    		null, // owningGroups 
    		SecurityKeys.ACCESS_LEVEL_NA
    	);
    }
    
    /**
     * Creates a new calendar with weekdays MO-SU or updates an existing.
     * 
     * @param calendarName
     * @param pm
     * @param providerName
     * @param segmentName
     * @param owningGroups
     * @param accessLevelUpdateDelete
     * @return
     */
    public Calendar initCalendar(
        String calendarName,
        PersistenceManager pm,
        String providerName,
        String segmentName,
        List<PrincipalGroup> owningGroups,
        short accessLevelUpdateDelete
    ) {
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
        if(owningGroups == null) {
        	owningGroups = activitySegment.getOwningGroup();
        }
        Calendar calendar = null;
        if((calendar = this.findCalendar(calendarName, activitySegment)) != null) {
            return calendar;            
        }                 
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        calendar = pm.newInstance(Calendar.class);
        calendar.setName(calendarName);
        calendar.getOwningGroup().addAll(owningGroups);
        calendar.setAccessLevelUpdate(accessLevelUpdateDelete);
        calendar.setAccessLevelDelete(accessLevelUpdateDelete);
        activitySegment.addCalendar(
            this.getUidAsString(),
            calendar
        );
        // Sunday
        WeekDay weekDay = pm.newInstance(WeekDay.class);
        weekDay.setDayOfWeek((short)1);
        weekDay.setWorkingDay(false);
        weekDay.getOwningGroup().addAll(owningGroups);
        weekDay.setAccessLevelUpdate(accessLevelUpdateDelete);
        weekDay.setAccessLevelDelete(accessLevelUpdateDelete);
        calendar.addWeekDay(
            this.getUidAsString(),
            weekDay
        );
        // Monday
        weekDay = pm.newInstance(WeekDay.class);
        weekDay.setDayOfWeek((short)2);
        weekDay.setWorkingDay(true);
        weekDay.setWorkDurationHours((short)8);
        weekDay.setWorkDurationMinutes((short)30);
        weekDay.getOwningGroup().addAll(owningGroups);
        weekDay.setAccessLevelUpdate(accessLevelUpdateDelete);
        weekDay.setAccessLevelDelete(accessLevelUpdateDelete);
        calendar.addWeekDay(
            this.getUidAsString(),
            weekDay
        );
        // Tuesday
        weekDay = pm.newInstance(WeekDay.class);
        weekDay.setDayOfWeek((short)3);
        weekDay.setWorkingDay(true);
        weekDay.setWorkDurationHours((short)8);
        weekDay.setWorkDurationMinutes((short)30);
        weekDay.getOwningGroup().addAll(owningGroups);
        weekDay.setAccessLevelUpdate(accessLevelUpdateDelete);
        weekDay.setAccessLevelDelete(accessLevelUpdateDelete);
        calendar.addWeekDay(
            this.getUidAsString(),
            weekDay
        );
        // Wednesday
        weekDay = pm.newInstance(WeekDay.class);
        weekDay.setDayOfWeek((short)4);
        weekDay.setWorkingDay(true);
        weekDay.setWorkDurationHours((short)8);
        weekDay.setWorkDurationMinutes((short)30);
        weekDay.getOwningGroup().addAll(owningGroups);
        weekDay.setAccessLevelUpdate(accessLevelUpdateDelete);
        weekDay.setAccessLevelDelete(accessLevelUpdateDelete);
        calendar.addWeekDay(
            this.getUidAsString(),
            weekDay
        );
        // Thursday
        weekDay = pm.newInstance(WeekDay.class);
        weekDay.setDayOfWeek((short)5);
        weekDay.setWorkingDay(true);
        weekDay.setWorkDurationHours((short)8);
        weekDay.setWorkDurationMinutes((short)30);
        weekDay.getOwningGroup().addAll(owningGroups);
        weekDay.setAccessLevelUpdate(accessLevelUpdateDelete);
        weekDay.setAccessLevelDelete(accessLevelUpdateDelete);
        calendar.addWeekDay(
            this.getUidAsString(),
            weekDay
        );
        // Friday
        weekDay = pm.newInstance(WeekDay.class);
        weekDay.setDayOfWeek((short)6);
        weekDay.setWorkingDay(true);
        weekDay.setWorkDurationHours((short)8);
        weekDay.setWorkDurationMinutes((short)30);
        weekDay.getOwningGroup().addAll(owningGroups);
        weekDay.setAccessLevelUpdate(accessLevelUpdateDelete);
        weekDay.setAccessLevelDelete(accessLevelUpdateDelete);
        calendar.addWeekDay(
            this.getUidAsString(),
            weekDay
        );
        // Saturday
        weekDay = pm.newInstance(WeekDay.class);
        weekDay.setDayOfWeek((short)7);
        weekDay.setWorkingDay(false);
        weekDay.getOwningGroup().addAll(owningGroups);
        weekDay.setAccessLevelUpdate(accessLevelUpdateDelete);
        weekDay.setAccessLevelDelete(accessLevelUpdateDelete);
        calendar.addWeekDay(
            this.getUidAsString(),
            weekDay
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
        return calendar;
    }
      
    /**
     * Create (if it does not exist) the e-mail activity process.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param owningGroups
     * @param accessLevelUpdateDelete
     * @return
     */
    public ActivityProcess initEmailProcess(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        List<PrincipalGroup> owningGroups,
        short accessLevelUpdateDelete
    ) {
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
        if(owningGroups == null) {
        	owningGroups = activitySegment.getOwningGroup();
        }
        ActivityProcess process = null;
        if((process = this.findActivityProcess(ACTIVITY_PROCESS_NAME_EMAILS, activitySegment, pm)) != null) {
            return process;            
        }                
        // Create email process
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        process = pm.newInstance(ActivityProcess.class);
        process.setName(ACTIVITY_PROCESS_NAME_EMAILS);
        process.getOwningGroup().addAll(owningGroups);
        process.setAccessLevelUpdate(accessLevelUpdateDelete);
        process.setAccessLevelDelete(accessLevelUpdateDelete);        
        activitySegment.addActivityProcess(
            this.getUidAsString(),
            process
        );
        // State New
        ActivityProcessState newState = pm.newInstance(ActivityProcessState.class);
        newState.setName("New");
        newState.getOwningGroup().addAll(owningGroups);
        newState.setAccessLevelUpdate(accessLevelUpdateDelete);
        newState.setAccessLevelDelete(accessLevelUpdateDelete);        
        process.addState(
            "StateNew",
            newState
        );
        // State Open
        ActivityProcessState openState = pm.newInstance(ActivityProcessState.class);
        openState.setName("Open");
        openState.getOwningGroup().addAll(owningGroups);
        openState.setAccessLevelUpdate(accessLevelUpdateDelete);
        openState.setAccessLevelDelete(accessLevelUpdateDelete);        
        process.addState(
            "StateOpen",
            openState
        );
        // State Closed
        ActivityProcessState closedState = pm.newInstance(ActivityProcessState.class);
        closedState.setName("Closed");
        closedState.getOwningGroup().addAll(owningGroups);
        closedState.setAccessLevelUpdate(accessLevelUpdateDelete);
        closedState.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addState(
            "StateClosed",
            closedState
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
        // Initial State
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        process.setStartState(newState);                    
        // Transition Assign: New->Open
        ActivityProcessTransition processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Assign");
        processTransition.setPrevState(newState);
        processTransition.setNextState(openState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)20));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionAssign",
            processTransition
        );
        // Create SetAssignedToAction
        SetAssignedToAction setAssignedToAction = pm.newInstance(SetAssignedToAction.class);
        setAssignedToAction.setName("Set assignedTo");
        setAssignedToAction.setDescription("Set assignedTo to current user");
        setAssignedToAction.getOwningGroup().addAll(owningGroups);
        setAssignedToAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setAssignedToAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        processTransition.addAction(
            "TransitionAssignTo",
            setAssignedToAction
        );
        // Create SetActualStartAction
        SetActualStartAction setActualStartAction = pm.newInstance(SetActualStartAction.class);
        setActualStartAction.setName("Set actual start");
        setActualStartAction.setDescription("Set actual start on activity assignment");
        setActualStartAction.getOwningGroup().addAll(owningGroups);
        setActualStartAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setActualStartAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        processTransition.addAction(
            this.getUidAsString(),
            setActualStartAction
        );
        // Transition Add Note: Open->Open
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Add Note");
        processTransition.setPrevState(openState);
        processTransition.setNextState(openState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)50));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionAddNote",
            processTransition
        );
        // Transition Export: Open->Open
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Export as mail attachment");
        processTransition.setPrevState(openState);
        processTransition.setNextState(openState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)50));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionSendAsMailAttachment",
            processTransition
        );
        // Create WorkflowAction for ExportMail
        WfAction wfAction = pm.newInstance(WfAction.class);
        wfAction.setName("Export Mail");
        wfAction.setName("Export Mail as attachment to current user");
        wfAction.setWfProcess(
            (WfProcess)pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", providerName, "segment", segmentName, "wfProcess", Workflows.WORKFLOW_EXPORT_MAIL)
            )
        );
        wfAction.getOwningGroup().addAll(owningGroups);
        wfAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        wfAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        processTransition.addAction(
            this.getUidAsString(),
            wfAction
        );
        // Transition Send: Open->Open
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Send as mail");
        processTransition.setPrevState(openState);
        processTransition.setNextState(openState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)50));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionSendAsMail",
            processTransition
        );
        // Create WorkflowAction for SendMail
        wfAction = pm.newInstance(WfAction.class);
        wfAction.setName("Send Mail");
        wfAction.setName("Send as mail");
        wfAction.setWfProcess(
            (WfProcess)pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", providerName, "segment", segmentName, "wfProcess", Workflows.WORKFLOW_SEND_MAIL)
            )
        );
        wfAction.getOwningGroup().addAll(owningGroups);
        wfAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        wfAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        processTransition.addAction(
            this.getUidAsString(),
            wfAction
        );
        // Transition Close: Open->Closed
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Close");
        processTransition.setPrevState(openState);
        processTransition.setNextState(closedState);
        processTransition.setNewActivityState((short)20);
        processTransition.setNewPercentComplete(Short.valueOf((short)100));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionClose",
            processTransition
        );
        // Create SetActualEndAction
        SetActualEndAction setActualEndAction = pm.newInstance(SetActualEndAction.class);
        setActualEndAction.setName("Set actual end");
        setActualEndAction.setName("Set actual end to current dateTime");
        setActualEndAction.getOwningGroup().addAll(owningGroups);
        setActualEndAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setActualEndAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        processTransition.addAction(
            this.getUidAsString(),
            setActualEndAction
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }        
        return process;
    }

    /**
     * Create (if it does not exist) the bulk e-mail activity process.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param owningGroups
     * @param accessLevelUpdateDelete
     * @return
     */
    public ActivityProcess initBulkEmailProcess(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        List<PrincipalGroup> owningGroups,
        short accessLevelUpdateDelete
    ) {
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
        if(owningGroups == null) {
        	owningGroups = activitySegment.getOwningGroup();
        }
        ActivityProcess process = null;
        if((process = this.findActivityProcess(ACTIVITY_PROCESS_NAME_BULK_EMAILS, activitySegment)) != null) {
            return process;            
        }                
        // Create process
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        process = pm.newInstance(ActivityProcess.class);
        process.setName(ACTIVITY_PROCESS_NAME_BULK_EMAILS);
        process.getOwningGroup().addAll(owningGroups);
        process.setAccessLevelUpdate(accessLevelUpdateDelete);
        process.setAccessLevelDelete(accessLevelUpdateDelete);        
        activitySegment.addActivityProcess(
            "BulkEMail",
            process
        );
        // State Draft
        ActivityProcessState draftState = pm.newInstance(ActivityProcessState.class);
        draftState.setName("Draft");
        draftState.getOwningGroup().addAll(owningGroups);
        draftState.setAccessLevelUpdate(accessLevelUpdateDelete);
        draftState.setAccessLevelDelete(accessLevelUpdateDelete);        
        process.addState(
        	"StateDraft",
            draftState
        );
        // State Open
        ActivityProcessState openState = pm.newInstance(ActivityProcessState.class);
        openState.setName("Open");
        openState.getOwningGroup().addAll(owningGroups);
        openState.setAccessLevelUpdate(accessLevelUpdateDelete);
        openState.setAccessLevelDelete(accessLevelUpdateDelete);        
        process.addState(
            "StateOpen",
            openState
        );
        // State Closed
        ActivityProcessState closedState = pm.newInstance(ActivityProcessState.class);
        closedState.setName("Closed");
        closedState.getOwningGroup().addAll(owningGroups);
        closedState.setAccessLevelUpdate(accessLevelUpdateDelete);
        closedState.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addState(
            "StateClosed",
            closedState
        );
        // State Cancelled
        ActivityProcessState cancelledState = pm.newInstance(ActivityProcessState.class);
        cancelledState.setName("Cancelled");
        cancelledState.getOwningGroup().addAll(owningGroups);
        cancelledState.setAccessLevelUpdate(accessLevelUpdateDelete);
        cancelledState.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addState(
            "StateCancelled",
            cancelledState
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
        // Initial State
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        process.setStartState(draftState);                    
        // Transition Add Note (Draft): Draft->Draft
        ActivityProcessTransition transitionAddNoteDraft = pm.newInstance(ActivityProcessTransition.class);
        transitionAddNoteDraft.setName("Add Note");
        transitionAddNoteDraft.setPrevState(draftState);
        transitionAddNoteDraft.setNextState(draftState);
        transitionAddNoteDraft.setNewActivityState((short)10);
        transitionAddNoteDraft.setNewPercentComplete(Short.valueOf((short)25));
        transitionAddNoteDraft.getOwningGroup().addAll(owningGroups);
        transitionAddNoteDraft.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionAddNoteDraft.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionAddNoteDraft",
            transitionAddNoteDraft
        );
        // Transition Send as attachment (Draft): Draft->Draft
        ActivityProcessTransition transitionSendAsAttachmentDraft = pm.newInstance(ActivityProcessTransition.class);
        transitionSendAsAttachmentDraft.setName("Export as mail attachment to current user");
        transitionSendAsAttachmentDraft.setPrevState(draftState);
        transitionSendAsAttachmentDraft.setNextState(draftState);
        transitionSendAsAttachmentDraft.setNewActivityState((short)10);
        transitionSendAsAttachmentDraft.setNewPercentComplete(Short.valueOf((short)25));
        transitionSendAsAttachmentDraft.getOwningGroup().addAll(owningGroups);
        transitionSendAsAttachmentDraft.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionSendAsAttachmentDraft.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionSendAsAttachmentDraft",
            transitionSendAsAttachmentDraft
        );
        // Action for ExportMail
        WfAction wfAction = pm.newInstance(WfAction.class);
        wfAction.setName("Export Mail");
        wfAction.setName("Export Mail as attachment to current user");
        wfAction.setWfProcess(
            (WfProcess)pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", providerName, "segment", segmentName, "wfProcess", Workflows.WORKFLOW_EXPORT_MAIL)
            )
        );
        wfAction.getOwningGroup().addAll(owningGroups);
        wfAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        wfAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        transitionSendAsAttachmentDraft.addAction(
            this.getUidAsString(),
            wfAction
        );
        // Transition Approve: Draft->Open
        ActivityProcessTransition transitionApprove = pm.newInstance(ActivityProcessTransition.class);
        transitionApprove.setName("Approve");
        transitionApprove.setPrevState(draftState);
        transitionApprove.setNextState(openState);
        transitionApprove.setNewActivityState((short)10);
        transitionApprove.setNewPercentComplete(Short.valueOf((short)50));
        transitionApprove.getOwningGroup().addAll(owningGroups);
        transitionApprove.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionApprove.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionApprove",
            transitionApprove
        );
        // Create SetAssignedToAction
        SetAssignedToAction setAssignedToAction = pm.newInstance(SetAssignedToAction.class);
        setAssignedToAction.setName("Set assignedTo");
        setAssignedToAction.setDescription("Set assignedTo to current user");
        setAssignedToAction.getOwningGroup().addAll(owningGroups);
        setAssignedToAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setAssignedToAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        transitionApprove.addAction(
            this.getUidAsString(),
            setAssignedToAction
        );
        // Create SetActualStartAction
        SetActualStartAction setActualStartAction = pm.newInstance(SetActualStartAction.class);
        setActualStartAction.setName("Set actual start");
        setActualStartAction.setDescription("Set actual start on activity assignment");
        setActualStartAction.getOwningGroup().addAll(owningGroups);
        setActualStartAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setActualStartAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        transitionApprove.addAction(
            this.getUidAsString(),
            setActualStartAction
        );
        // Transition Cancel (Draft): Draft->Cancelled
        ActivityProcessTransition transitionCancelDraft = pm.newInstance(ActivityProcessTransition.class);
        transitionCancelDraft.setName("Cancel");
        transitionCancelDraft.setPrevState(draftState);
        transitionCancelDraft.setNextState(cancelledState);
        transitionCancelDraft.setNewActivityState((short)30);
        transitionCancelDraft.setNewPercentComplete(Short.valueOf((short)100));
        transitionCancelDraft.getOwningGroup().addAll(owningGroups);
        transitionCancelDraft.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionCancelDraft.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionCancelDraft",
            transitionCancelDraft
        );
        // Transition Add Note: Open->Open
        ActivityProcessTransition transitionAddNote = pm.newInstance(ActivityProcessTransition.class);
        transitionAddNote.setName("Add Note");
        transitionAddNote.setPrevState(openState);
        transitionAddNote.setNextState(openState);
        transitionAddNote.setNewActivityState((short)10);
        transitionAddNote.setNewPercentComplete(Short.valueOf((short)50));
        transitionAddNote.getOwningGroup().addAll(owningGroups);
        transitionAddNote.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionAddNote.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionAddNote",
            transitionAddNote
        );
        // Transition Send as attachment (Draft): Draft->Draft
        ActivityProcessTransition transitionSendAsAttachment = pm.newInstance(ActivityProcessTransition.class);
        transitionSendAsAttachment.setName("Export as mail attachment to current user");
        transitionSendAsAttachment.setPrevState(openState);
        transitionSendAsAttachment.setNextState(openState);
        transitionSendAsAttachment.setNewActivityState((short)10);
        transitionSendAsAttachment.setNewPercentComplete(Short.valueOf((short)50));
        transitionSendAsAttachment.getOwningGroup().addAll(owningGroups);
        transitionSendAsAttachment.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionSendAsAttachment.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionSendAsMailAttachment",
            transitionSendAsAttachment
        );
        // Action for ExportMail
        wfAction = pm.newInstance(WfAction.class);
        wfAction.setName("Export Mail");
        wfAction.setName("Export Mail as attachment to current user");
        wfAction.setWfProcess(
            (WfProcess)pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", providerName, "segment", segmentName, "wfProcess", Workflows.WORKFLOW_EXPORT_MAIL)
            )
        );
        wfAction.getOwningGroup().addAll(owningGroups);
        wfAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        wfAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        transitionSendAsAttachment.addAction(
            this.getUidAsString(),
            wfAction
        );
        // Transition Send: Open->Open
        ActivityProcessTransition transitionSend = pm.newInstance(ActivityProcessTransition.class);
        transitionSend.setName("Send as mail");
        transitionSend.setPrevState(openState);
        transitionSend.setNextState(openState);
        transitionSend.setNewActivityState((short)10);
        transitionSend.setNewPercentComplete(Short.valueOf((short)50));
        transitionSend.getOwningGroup().addAll(owningGroups);
        transitionSend.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionSend.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionSendAsMail",
            transitionSend
        );
        // Action for SendMail
        wfAction = pm.newInstance(WfAction.class);
        wfAction.setName("Send Mail");
        wfAction.setName("Send as mail");
        wfAction.setWfProcess(
            (WfProcess)pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", providerName, "segment", segmentName, "wfProcess", Workflows.WORKFLOW_SEND_MAIL)
            )
        );
        wfAction.getOwningGroup().addAll(owningGroups);
        wfAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        wfAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        transitionSend.addAction(
            this.getUidAsString(),
            wfAction
        );
        // Transition Close: Open->Closed
        ActivityProcessTransition transitionClose = pm.newInstance(ActivityProcessTransition.class);
        transitionClose.setName("Close");
        transitionClose.setPrevState(openState);
        transitionClose.setNextState(closedState);
        transitionClose.setNewActivityState((short)20);
        transitionClose.setNewPercentComplete(Short.valueOf((short)100));
        transitionClose.getOwningGroup().addAll(owningGroups);
        transitionClose.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionClose.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionClose",
            transitionClose
        );
        // Create SetActualEndAction
        SetActualEndAction setActualEndAction = pm.newInstance(SetActualEndAction.class);
        setActualEndAction.setName("Set actual end");
        setActualEndAction.setName("Set actual end to current dateTime");
        setActualEndAction.getOwningGroup().addAll(owningGroups);
        setActualEndAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setActualEndAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        transitionClose.addAction(
            this.getUidAsString(),
            setActualEndAction
        );
        // Transition Cancel: Open->Cancelled
        ActivityProcessTransition transitionCancel = pm.newInstance(ActivityProcessTransition.class);
        transitionCancel.setName("Cancel");
        transitionCancel.setPrevState(openState);
        transitionCancel.setNextState(cancelledState);
        transitionCancel.setNewActivityState((short)30);
        transitionCancel.setNewPercentComplete(Short.valueOf((short)100));
        transitionCancel.getOwningGroup().addAll(owningGroups);
        transitionCancel.setAccessLevelUpdate(accessLevelUpdateDelete);
        transitionCancel.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionCancel",
            transitionCancel
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }        
        return process;
    }

    /**
     * Create (if it does not exist) the bug and feature activity process.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param owningGroups
     * @param accessLevelUpdateDelete
     * @return
     */
    public ActivityProcess initBugAndFeatureTrackingProcess(
        PersistenceManager pm,
        String providerName,
        String segmentName,
    	List<PrincipalGroup> owningGroups,
    	short accessLevelUpdateDelete
    ) {
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
        if(owningGroups == null) {
        	owningGroups = activitySegment.getOwningGroup();
        }
        ActivityProcess process = null;
        if((process = this.findActivityProcess(ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING, activitySegment, pm)) != null) {
            return process;            
        }                
        // Create process
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        process = pm.newInstance(ActivityProcess.class);
        process.setName(ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING);
        process.getOwningGroup().addAll(owningGroups);
        process.setAccessLevelUpdate(accessLevelUpdateDelete);
        process.setAccessLevelDelete(accessLevelUpdateDelete);
        activitySegment.addActivityProcess(
            "BugAndFeatures",
            process
        );
        // State New
        ActivityProcessState newState = pm.newInstance(ActivityProcessState.class);
        newState.setName("New");
        newState.getOwningGroup().addAll(owningGroups);
        newState.setAccessLevelUpdate(accessLevelUpdateDelete);
        newState.setAccessLevelDelete(accessLevelUpdateDelete);        
        process.addState(
            "StateNew",
            newState
        );
        // State In Progress
        ActivityProcessState inProgressState = pm.newInstance(ActivityProcessState.class);
        inProgressState.setName("In Progress");
        inProgressState.getOwningGroup().addAll(owningGroups);
        inProgressState.setAccessLevelUpdate(accessLevelUpdateDelete);
        inProgressState.setAccessLevelDelete(accessLevelUpdateDelete);
        process.addState(
            "InProgress",
            inProgressState
        );
        // State Complete
        ActivityProcessState completeState = pm.newInstance(ActivityProcessState.class);
        completeState.setName("Complete");
        completeState.getOwningGroup().addAll(owningGroups);
        completeState.setAccessLevelUpdate(accessLevelUpdateDelete);
        completeState.setAccessLevelDelete(accessLevelUpdateDelete);
        process.addState(
            "StateComplete",
            completeState
        );
        // State Closed
        ActivityProcessState closedState = pm.newInstance(ActivityProcessState.class);
        closedState.setName("Closed");
        closedState.getOwningGroup().addAll(owningGroups);
        closedState.setAccessLevelUpdate(accessLevelUpdateDelete);
        closedState.setAccessLevelDelete(accessLevelUpdateDelete);
        process.addState(
            "StateClosed",
            closedState
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
        // Initial State
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        process.setStartState(newState);                    
        // Transition Add Note: Open->Open
        ActivityProcessTransition processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Add Note");
        processTransition.setPrevState(inProgressState);
        processTransition.setNextState(inProgressState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)50));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);        
        process.addTransition(
            "TransitionAddNote",
            processTransition
        );
        // Transition Assign: New->In Progress
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Assign");
        processTransition.setPrevState(newState);
        processTransition.setNextState(inProgressState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)20));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionAssign",
            processTransition
        );
        // Create SetAssignedToAction
        SetAssignedToAction setAssignedToAction = pm.newInstance(SetAssignedToAction.class);
        setAssignedToAction.setName("Set assignedTo");
        setAssignedToAction.setDescription("Set assignedTo to current user");
        setAssignedToAction.getOwningGroup().addAll(owningGroups);
        setAssignedToAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setAssignedToAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        processTransition.addAction(
            this.getUidAsString(),
            setAssignedToAction
        );
        // Create SetActualStartAction
        SetActualStartAction setActualStartAction = pm.newInstance(SetActualStartAction.class);
        setActualStartAction.setName("Set actual start");
        setActualStartAction.setDescription("Set actual start on activity assignment");
        setActualStartAction.getOwningGroup().addAll(owningGroups);
        setActualStartAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setActualStartAction.setAccessLevelDelete(accessLevelUpdateDelete);                
        processTransition.addAction(
            this.getUidAsString(),
            setActualStartAction
        );
        // Transition Close: Complete->Closed
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Close");
        processTransition.setPrevState(completeState);
        processTransition.setNextState(closedState);
        processTransition.setNewActivityState((short)20);
        processTransition.setNewPercentComplete(Short.valueOf((short)100));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionClose",
            processTransition
        );
        // Transition Complete: In Progress->Complete
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Complete");
        processTransition.setPrevState(inProgressState);
        processTransition.setNextState(completeState);
        processTransition.setNewActivityState((short)20);
        processTransition.setNewPercentComplete(Short.valueOf((short)100));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                
        process.addTransition(
            "TransitionComplete",
            processTransition
        );
        // Create SetActualEndAction
        SetActualEndAction setActualEndAction = pm.newInstance(SetActualEndAction.class);
        setActualEndAction.setName("Set actual end");
        setActualEndAction.setName("Set actual end to current dateTime");
        setActualEndAction.getOwningGroup().addAll(owningGroups);
        setActualEndAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setActualEndAction.setAccessLevelDelete(accessLevelUpdateDelete);                        
        processTransition.addAction(
            this.getUidAsString(),
            setActualEndAction
        );
        // Create SetAssignedToAction
        setAssignedToAction = pm.newInstance(SetAssignedToAction.class);
        setAssignedToAction.setName("Set assignedTo");
        setAssignedToAction.setDescription("Set assignedTo to reporting contact");
        setAssignedToAction.getOwningGroup().addAll(owningGroups);
        setAssignedToAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setAssignedToAction.setAccessLevelDelete(accessLevelUpdateDelete);                        
        processTransition.addAction(
            this.getUidAsString(),
            setAssignedToAction
        );
        // Transition Create: New->New
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Create");
        processTransition.setPrevState(newState);
        processTransition.setNextState(newState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)0));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                        
        process.addTransition(
            "TransitionCreate",
            processTransition
        );
        // Transition Reopen: Complete->In Progress
        processTransition = pm.newInstance(ActivityProcessTransition.class);
        processTransition.setName("Reopen");
        processTransition.setPrevState(completeState);
        processTransition.setNextState(inProgressState);
        processTransition.setNewActivityState((short)10);
        processTransition.setNewPercentComplete(Short.valueOf((short)50));
        processTransition.getOwningGroup().addAll(owningGroups);
        processTransition.setAccessLevelUpdate(accessLevelUpdateDelete);
        processTransition.setAccessLevelDelete(accessLevelUpdateDelete);                        
        process.addTransition(
            "TransitionReopen",
            processTransition
        );
        // Create SetAssignedToAction
        setAssignedToAction = pm.newInstance(SetAssignedToAction.class);
        setAssignedToAction.setName("Set assignedTo");
        setAssignedToAction.setDescription("Set assignedTo to current user");
        setAssignedToAction.getOwningGroup().addAll(owningGroups);
        setAssignedToAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setAssignedToAction.setAccessLevelDelete(accessLevelUpdateDelete);                        
        processTransition.addAction(
            this.getUidAsString(),
            setAssignedToAction
        );        
        // Create SetActualEndAction
        setActualEndAction = pm.newInstance(SetActualEndAction.class);
        setActualEndAction.setName("Reset actualEnd");
        setActualEndAction.setDescription("Reset actualEnd");
        setActualEndAction.setResetToNull(true);
        setActualEndAction.getOwningGroup().addAll(owningGroups);
        setActualEndAction.setAccessLevelUpdate(accessLevelUpdateDelete);
        setActualEndAction.setAccessLevelDelete(accessLevelUpdateDelete);                        
        processTransition.addAction(
            this.getUidAsString(),
            setActualEndAction
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }        
        return process;
    }

    /**
     * Init activity type.
     * 
     * @param activityProcess
     * @param activityTypeName
     * @param activityClass
     * @param activityClassName
     * @param owningGroups
     * @param accessLevelUpdateDelete
     * @return
     * @throws ServiceException
     */
    public ActivityType initActivityType(
        ActivityProcess activityProcess,
        String activityTypeName,
        short activityClass,
        String activityClassName,
        List<PrincipalGroup> owningGroups,
        short accessLevelUpdateDelete
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activityProcess);
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	String providerName = activityProcess.refGetPath().getSegment(2).toString();
    	String segmentName = activityProcess.refGetPath().getSegment(4).toString();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
        if(owningGroups == null) {
        	owningGroups = activitySegment.getOwningGroup();
        }
        ActivityType activityType = null;
        if((activityType = this.findActivityType(activityTypeName, activitySegment, pm)) != null) {
            return activityType;            
        }
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        activityType = pm.newInstance(ActivityType.class);
        activityType.setName(activityTypeName);
        activityType.setActivityClass(activityClass);
        activityType.setActivityClassName(activityClassName);
        activityType.setControlledBy(activityProcess);
        activityType.getOwningGroup().addAll(owningGroups);
        activityType.setAccessLevelUpdate(accessLevelUpdateDelete);
        activityType.setAccessLevelDelete(accessLevelUpdateDelete);
        activitySegment.addActivityType(
            this.getUidAsString(),
            activityType
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
        return activityType;    	
    }

    /**
     * Creates a new activity type or updates an existing.
     * 
     * @param activityProcess
     * @param activityTypeName
     * @param activityClass
     * @param owningGroups
     * @param accessLevelUpdateDelete
     * @return
     */
    public ActivityType initActivityType(
        ActivityProcess activityProcess,
        String activityTypeName,
        short activityClass,
        List<PrincipalGroup> owningGroups,
        short accessLevelUpdateDelete
    ) throws ServiceException {
    	return this.initActivityType(
    		activityProcess, 
    		activityTypeName, 
    		activityClass, 
    		null, // activityClassName
    		owningGroups, 
    		accessLevelUpdateDelete
    	);
    }

    /**
     * Creates a new activity tracker or updates an existing.
     * 
     * @param trackerName
     * @param owningGroups
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    @Deprecated
    public ActivityTracker initActivityTracker(
        String trackerName,
        List<PrincipalGroup> owningGroups,        
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
    	return this.initActivityTracker(
    		trackerName, 
    		owningGroups, 
    		activitySegment
    	);
    }

    /**
     * Creates a new activity tracker or updates an existing.
     * 
     * @param trackerName
     * @param owningGroups
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public ActivityTracker initActivityTracker(
        String trackerName,
        List<PrincipalGroup> owningGroups,
        org.opencrx.kernel.activity1.jmi1.Segment activitySegment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activitySegment);
        ActivityTracker activityTracker = null;
        if((activityTracker = this.findActivityTracker(trackerName, activitySegment)) != null) {
            return activityTracker;            
        }        
        pm.currentTransaction().begin();
        activityTracker = pm.newInstance(ActivityTracker.class);
        activityTracker.setName(trackerName);
        activityTracker.getOwningGroup().addAll(
            owningGroups == null
                ? activitySegment.getOwningGroup()
                : owningGroups
        );
        activitySegment.addActivityTracker(
            this.getUidAsString(),
            activityTracker
        );                        
        pm.currentTransaction().commit();
        return activityTracker;
    }

    /**
     * Creates a new activity category or updates an existing.
     * 
     * @param categoryName
     * @param owningGroups
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    @Deprecated
    public ActivityCategory initActivityCategory(
        String categoryName,
        List<org.opencrx.security.realm1.jmi1.PrincipalGroup> owningGroups,        
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
    	return this.initActivityCategory(
    		categoryName, 
    		owningGroups, 
    		activitySegment
    	);
    }

    /**
     * Creates a new activity category or updates an existing.
     * 
     * @param categoryName
     * @param owningGroups
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public ActivityCategory initActivityCategory(
        String categoryName,
        List<org.opencrx.security.realm1.jmi1.PrincipalGroup> owningGroups,        
        org.opencrx.kernel.activity1.jmi1.Segment activitySegment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activitySegment);
        ActivityCategory activityCategory = null;
        if((activityCategory = this.findActivityCategory(categoryName, activitySegment)) != null) {
            return activityCategory;            
        }        
        pm.currentTransaction().begin();
        activityCategory = pm.newInstance(ActivityCategory.class);
        activityCategory.setName(categoryName);
        activityCategory.getOwningGroup().addAll(
            owningGroups == null
                ? activitySegment.getOwningGroup()
                : owningGroups
        );
        activitySegment.addActivityCategory(
            this.getUidAsString(),
            activityCategory
        );                        
        pm.currentTransaction().commit();
        return activityCategory;
    }

    /**
     * Creates a new activity creator if it does not exist or updates and existing.
     * 
     * @param creatorName
     * @param activityType
     * @param activityGroups
     * @param owningGroups
     * @return
     */
    public ActivityCreator initActivityCreator(
        String creatorName,
        ActivityType activityType,
        List<ActivityGroup> activityGroups,
        List<PrincipalGroup> owningGroups
    ) throws ServiceException {
    	return this.initActivityCreator(
    		creatorName, 
    		activityType, 
    		ICalendar.ICAL_TYPE_VEVENT,
    		ICalClass.PUBLIC,
    		activityGroups, 
    		owningGroups
    	);
    }
    
    /**
     * Initializes the activity creator.
     * 
     * @param creatorName
     * @param activityType
     * @param icalType
     * @param activityGroups
     * @param owningGroups
     * @return
     */
    public ActivityCreator initActivityCreator(
        String creatorName,
        ActivityType activityType,
        short icalType,
        ICalClass icalClass,
        List<ActivityGroup> activityGroups,
        List<PrincipalGroup> owningGroups
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activityType);
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	String providerName = activityType.refGetPath().getSegment(2).toString();
    	String segmentName = activityType.refGetPath().getSegment(4).toString();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(
            pm, 
            providerName, 
            segmentName
        );
    	if(owningGroups == null) {
    		owningGroups = activitySegment.getOwningGroup();
    	}
        ActivityCreator activityCreator = null;
        if((activityCreator = this.findActivityCreator(creatorName, activitySegment)) == null) {
        	if(isTxLocal) {
        		pm.currentTransaction().begin();
        	}
            activityCreator = pm.newInstance(ActivityCreator.class);
            this.initActivityCreator(
            	activityCreator, 
            	creatorName, 
            	activityType, 
            	icalType, 
            	icalClass, 
            	activityGroups, 
            	owningGroups
            );
            activitySegment.addActivityCreator(
                this.getUidAsString(),
                activityCreator
            );
            if(isTxLocal) {
            	pm.currentTransaction().commit();
            }
        }
        // Set default creator for activity groups
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        for(ActivityGroup activityGroup: activityGroups) {
            if(activityGroup.getDefaultCreator() == null) {
                activityGroup.setDefaultCreator(activityCreator);
            }
        }
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
        return activityCreator;
    }

    /**
     * Init given activity creator.
     * 
     * @param activityCreator
     * @param creatorName
     * @param activityType
     * @param icalType
     * @param icalClass
     * @param activityGroups
     * @param owningGroups
     */
    public void initActivityCreator(
    	ActivityCreator activityCreator,
    	String creatorName,
    	ActivityType activityType,
        short icalType,
        ICalClass icalClass,    	
    	List<ActivityGroup> activityGroups,
    	List<PrincipalGroup> owningGroups
    ) {
        activityCreator.setName(creatorName);
        activityCreator.setPriority((short)0);
        if(owningGroups != null) {
        	activityCreator.getOwningGroup().addAll(owningGroups);
        }
        if(activityGroups != null) {
	        activityCreator.getActivityGroup().addAll(activityGroups);
        }
        activityCreator.setActivityType(activityType);
        activityCreator.setIcalType(icalType);
        activityCreator.setIcalClass(icalClass.getValue());
    }

	/**
	 * Find activity filter.
	 * 
	 * @param activityFilterName
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal findActivityFilter(
		String activityFilterName,
		org.opencrx.kernel.activity1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);		
		org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery query =
		    (org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal.class);
		query.name().equalTo(activityFilterName);
		Collection<ActivityFilterGlobal> activityFilters = segment.getActivityFilter(query);
		if(!activityFilters.isEmpty()) {
			return (org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)activityFilters.iterator().next();
		}
		return null;
	}
    
	/**
	 * Init activity filter.
	 * 
	 * @param filterName
	 * @param filterProperties
	 * @param pm
	 * @param segment
	 * @param allUsers
	 * @return
	 */
	public org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal initActivityFilter(
		String filterName,
		org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[] filterProperties,
		org.opencrx.kernel.activity1.jmi1.Segment segment,
		List<PrincipalGroup> allUsers
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal activityFilter = findActivityFilter(
			filterName,
			segment
		);
		if(activityFilter != null) return activityFilter;
		try {
			pm.currentTransaction().begin();
			activityFilter = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal.class);
			activityFilter.setName(filterName);
			activityFilter.getOwningGroup().addAll(allUsers);
			segment.addActivityFilter(
				false,
				Activities.getInstance().getUidAsString(),
				activityFilter
			);
			for(int i = 0; i < filterProperties.length; i++) {
				filterProperties[i].getOwningGroup().addAll(allUsers);
				activityFilter.addFilterProperty(
					false,
					Activities.getInstance().getUidAsString(),
					filterProperties[i]
				);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return activityFilter;
	}
    
    /**
     * Refreshes an activity tracker. Currently recalculates the effort estimates.
     * 
     * @param activityTracker
     * @return
     * @throws ServiceException
     */
    public ActivityTracker refreshTracker(
      ActivityTracker activityTracker
    ) throws ServiceException {    	
        Collection<Activity> activities = activityTracker.getFilteredActivity();
        int estimateEffortHours = 0;
        int estimateEffortMinutes = 0;
        // Iterate all activities and sum up all main effort estimates. Don't care
        // if isMain=true if there is exactly one estimate for the activity
        for(Activity activity: activities) {
            Collection<EffortEstimate> effortEstimates = activity.getEffortEstimate();
            if(effortEstimates.size() == 1) {
                EffortEstimate effortEstimate = effortEstimates.iterator().next();
                if(effortEstimate.getEstimateEffortHours() != null) {
                    estimateEffortHours += effortEstimate.getEstimateEffortHours().intValue();
                }
                if(effortEstimate.getEstimateEffortMinutes() != null) {
                    estimateEffortMinutes += effortEstimate.getEstimateEffortMinutes().intValue();
                }
            }
            // Lookup main estimate
            else {
                for(EffortEstimate effortEstimate: effortEstimates) {
                    if(
                        (effortEstimate.isMain() != null) &&
                        effortEstimate.isMain().booleanValue()
                    ) {                    
                        if(effortEstimate.getEstimateEffortHours() != null) {
                            estimateEffortHours += effortEstimate.getEstimateEffortHours().intValue();
                        }
                        if(effortEstimate.getEstimateEffortMinutes() != null) {
                            estimateEffortMinutes += effortEstimate.getEstimateEffortMinutes();
                        }
                        // At most one main estimate is allowed. If the user entered more 
                        // than one include the first only
                        break;
                    }
                }
            }
        }
        // Update tracker
        activityTracker.setSumEstimateEffortHours(
        	Integer.valueOf(estimateEffortHours + estimateEffortMinutes / 60)
        );
        activityTracker.setSumEstimateEffortMinutes(
            Integer.valueOf(estimateEffortMinutes % 60)
        );
        return activityTracker;
    }
    
    /**
     * {@link #newActivity()} allows to create or update replicated activities.
     * 
     * @param activity activity to be replicated.
     */
    public void createOrUpdateReplicatedActivities(
    	Activity activity
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
    	String providerName = activity.refGetPath().getSegment(2).toString();
    	String segmentName = activity.refGetPath().getSegment(4).toString();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, providerName, segmentName);
    	ActivityQuery replicaQuery = (ActivityQuery)pm.newQuery(Activity.class);
    	replicaQuery.thereExistsActivityLinkTo().activityLinkType().elementOf(
        	Arrays.asList(
        		ActivityLinkType.IS_REPLICA_OF.getValue(), 
        		ActivityLinkType.IS_REPLICA_OF_OBFUSCATED.getValue()
        	)
    	);
    	replicaQuery.thereExistsActivityLinkTo().thereExistsLinkTo().equalTo(activity);
    	for(Activity replica: activitySegment.getActivity(replicaQuery)) {
    		ActivityLinkToQuery activityLinkToQuery = (ActivityLinkToQuery)pm.newQuery(ActivityLinkTo.class);
    		activityLinkToQuery.thereExistsLinkTo().equalTo(activity);
    		for(ActivityLinkTo activityLinkTo: replica.getActivityLinkTo(activityLinkToQuery)) {
	        	this.updateReplicatedActivity(
	        		activity,
	        		replica,
	        		activityLinkTo
	        	);
    		}
    	}    	
    }

    /**
     * Create a new activity.
     * 
     * @param activityCreator
     * @param name
     * @param description
     * @param detailedDescription
     * @param scheduledStart
     * @param scheduledEnd
     * @param dueBy
     * @param priority
     * @param icalType
     * @param reportingContact
     * @param creationContext
     * @return
     * @throws ServiceException
     */
    public Activity newActivity(
        ActivityCreator activityCreator,
        String name,
        String description,
        String detailedDescription,
        Date scheduledStart,
        Date scheduledEnd,
        Date dueBy,
        Number priority,
        Number icalType,
        ICalClass icalClass,
        Contact reportingContact,
        ContextCapable creationContext
    ) throws ServiceException {
    	List<ActivityGroup> activityGroups = activityCreator.getActivityGroup();
    	return this.newActivity(
    		activityCreator, 
    		name, 
    		description, 
    		detailedDescription, 
    		scheduledStart, 
    		scheduledEnd, 
    		dueBy, 
    		priority, 
    		icalType, 
    		icalClass,
    		reportingContact, 
    		creationContext, 
    		activityGroups
    	);
    }

    /**
     * Creates a new activity and links the tracker with this new activity.
     * 
     * @param activityCreator
     * @param name
     * @param description
     * @param detailedDescription
     * @param scheduledStart
     * @param scheduledEnd
     * @param dueBy
     * @param priority
     * @param icalType
     * @param reportingContact
     * @param creationContext
     * @param activityGroups
     * @return
     * @throws ServiceException
     */
    public Activity newActivity(
        ActivityCreator activityCreator,
        String name,
        String description,
        String detailedDescription,
        Date scheduledStart,
        Date scheduledEnd,
        Date dueBy,
        Number priority,
        Number icalType,
        ICalClass icalClass,
        Contact reportingContact,
        ContextCapable creationContext,
        List<ActivityGroup> activityGroups
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activityCreator);
    	String providerName = activityCreator.refGetPath().getSegment(2).toString();
    	String segmentName = activityCreator.refGetPath().getSegment(4).toString();
    	boolean useRunAsPrincipal = Utils.hasObjectRunAsPermission(
    		activityCreator.refGetPath(), 
    		Utils.getPermissions(
    			Utils.getRequestingPrincipal(pm, providerName, segmentName), 
    			Action.RUN_AS.getName()
    		)
    	);
    	// Create activity    	
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(pm, providerName, segmentName);
        if(activityCreator.getActivityType() != null) {
            ActivityType activityType = activityCreator.getActivityType();
            ActivityProcess activityProcess = activityType.getControlledBy();
            Date aScheduledStart = null;
            aScheduledStart = scheduledStart != null 
            	? scheduledStart 
            	: (activityCreator.getBaseDate() != null) && (activityCreator.getScheduledStart() != null) 
            		? new Date(System.currentTimeMillis() + activityCreator.getScheduledStart().getTime())
            		: new Date();
            Date aScheduledEnd = null;
            aScheduledEnd = scheduledEnd != null 
            	? scheduledEnd 
            	: (activityCreator.getBaseDate() != null) && (activityCreator.getScheduledEnd() != null) 
            		? new Date(System.currentTimeMillis() + activityCreator.getScheduledEnd().getTime() - activityCreator.getBaseDate().getTime()) 
            		: new Date(aScheduledStart.getTime() + 3600000L);
            Date aDueBy = null;
            aDueBy = dueBy != null 
            	? dueBy 
            	: (activityCreator.getBaseDate() != null) && (activityCreator.getDueBy() != null) 
            		? new Date(System.currentTimeMillis() + activityCreator.getDueBy().getTime() - activityCreator.getBaseDate().getTime()) 
            		: null;
            short aPriority = (priority == null) || (priority.shortValue() == 0) 
            	? activityCreator.getPriority() != 0 
            		? activityCreator.getPriority() 
            		: (short)2 
            	: priority.shortValue();
            short aIcalType = (icalType == null) || (icalType.shortValue() == 0) 
            	? activityCreator.getIcalType() 
            		: icalType.shortValue();
            short aIcalClass = (icalClass == null) || (icalClass.getValue() == 0) 
            	? activityCreator.getIcalClass() == 0 
            		? ICalClass.PUBLIC.getValue()
            		: activityCreator.getIcalClass()            		
            	: icalClass.getValue();
            String activityClass = activityType.getActivityClassName() != null 
            	? activityType.getActivityClassName() 
            	: ACTIVITY_TYPES[((Number)activityType.getActivityClass()).intValue()];
            Activity newActivity = null;
			try {
				String packageName = activityClass.substring(0, activityClass.lastIndexOf(":"));
				String className = activityClass.substring(activityClass.lastIndexOf(":") + 1);
				newActivity = (Activity)pm.newInstance(
					Classes.getApplicationClass(
						packageName.replace(":", ".") + "." + Names.JMI1_PACKAGE_SUFFIX + "." + className
					)
				);
			} catch(ClassNotFoundException e) {
				throw new ServiceException(e);
			}
            if(name != null) {
                newActivity.setName(name);
            }
            if(description != null) {
                newActivity.setDescription(description);
            }
            if(detailedDescription != null) {
                newActivity.setDetailedDescription(detailedDescription);
            }
            if(aScheduledStart != null) {
                newActivity.setScheduledStart(aScheduledStart);
            }
            if(aScheduledEnd != null) {
                newActivity.setScheduledEnd(aScheduledEnd);
            }
            if(reportingContact != null) {
                newActivity.setReportingContact(reportingContact);                    
            } else {
                org.opencrx.kernel.home1.jmi1.UserHome userHome = UserHomes.getInstance().getUserHome(
                    activityCreator.refGetPath(),
                    pm
                );
                newActivity.setReportingContact(userHome.getContact());
            }
            newActivity.setPriority(aPriority);
            newActivity.setIcalType(aIcalType);
            newActivity.setIcalClass(aIcalClass);
            if(aDueBy != null) {
                newActivity.setDueBy(aDueBy);
            }
            newActivity.setActivityType(activityType);
            if(activityProcess.getStartState() != null) {
            	ActivityProcessTransitionQuery activityProcessTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
            	activityProcessTransitionQuery.forAllDisabled().isFalse();
            	activityProcessTransitionQuery.thereExistsNextState().equalTo(activityProcess.getStartState());
            	activityProcessTransitionQuery.orderByNewPercentComplete().ascending();
            	List<ActivityProcessTransition> activityProcessTransitions = activityProcess.getTransition(activityProcessTransitionQuery);
            	// Init if we have a transition leading to the start state
            	if(!activityProcessTransitions.isEmpty()) {
                    newActivity.setProcessState(activityProcess.getStartState());
            		ActivityProcessTransition transition = activityProcessTransitions.iterator().next();
            		newActivity.setPercentComplete(transition.getNewPercentComplete());
            		newActivity.setActivityState(transition.getNewActivityState());
            	} else {
                    newActivity.setPercentComplete(Short.valueOf((short)0));
                	newActivity.setActivityState(Short.valueOf((short)0));
            	}
            } else {
                newActivity.setPercentComplete(Short.valueOf((short)0));
            	newActivity.setActivityState(Short.valueOf((short)0));
            }
            newActivity.setCreationContext(creationContext);
            // Set code values to 0 (non-optional attributes)
            if(newActivity instanceof Incident) {
            	Incident incident = (Incident)newActivity;
            	incident.setCaseOrigin(Short.valueOf((short)0));
            	incident.setCaseType(Short.valueOf((short)0));
            	incident.setCustomerSatisfaction(Short.valueOf((short)0));
            	incident.setSeverity(Short.valueOf((short)0));
            	incident.setReproducibility(Short.valueOf((short)0));
            }
            try {
            	activitySegment.addActivity(
            		this.getUidAsString(),
            		newActivity
            	);
                this.reapplyActivityCreator(
                    newActivity,
                    activityCreator,
                    activityGroups
                );
                Base.getInstance().assignToMe(
                	newActivity, 
                	false, 
                	useRunAsPrincipal 
                );
                this.updateIcal(newActivity);
            	this.createOrUpdateReplicatedActivities(newActivity);
                return newActivity;
            } catch(ServiceException e) {
            	SysLog.warning("Creation of new activity failed", e.getMessage());
            	SysLog.warning(e.getMessage(), e.getCause());
            }
        }
        return null;
    }

    /**
     * Create a vote for given activity.
     * 
     * @param activity
     * @param name
     * @param description
     * @return
     * @throws ServiceException
     */
    public ActivityVote voteForActivity(
        Activity activity,
        String name,
        String description
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);    	
        ActivityVote vote = pm.newInstance(ActivityVote.class);
        if(name != null) {
            vote.setName(name);
        }
        if(description != null) {
            vote.setDescription(description);
        }
        activity.addVote(
        	false,
        	this.getUidAsString(),
        	vote
        );
        Base.getInstance().assignToMe(
            vote,
            true, // overwrite
            false // useRunAsPrincipal
        );
        return vote;
    }

    /**
     * Mark activity as all-day event.
     * 
     * @param activity
     * @param timezoneID
     */
    public void markAsAllDayEvent(
        Activity activity,
        String timezoneID
    ) {
    	if(timezoneID == null) {
    		timezoneID = "GMT-0:00";
    	}
    	// scheduledStart
		java.util.Calendar localScheduledStart = GregorianCalendar.getInstance(java.util.TimeZone.getTimeZone(timezoneID));
		localScheduledStart.setTime(activity.getScheduledStart());
		java.util.Calendar scheduledStart = GregorianCalendar.getInstance(java.util.TimeZone.getTimeZone("GMT-0:00"));
		scheduledStart.set(java.util.Calendar.YEAR, localScheduledStart.get(java.util.Calendar.YEAR));
		scheduledStart.set(java.util.Calendar.MONTH, localScheduledStart.get(java.util.Calendar.MONTH));
		scheduledStart.set(java.util.Calendar.DAY_OF_MONTH, localScheduledStart.get(java.util.Calendar.DAY_OF_MONTH));		
		scheduledStart.set(java.util.Calendar.HOUR_OF_DAY, 0);
		scheduledStart.set(java.util.Calendar.MINUTE, 0);
		scheduledStart.set(java.util.Calendar.SECOND, 0);
		scheduledStart.set(java.util.Calendar.MILLISECOND, 0);
		// scheduledEnd
		java.util.Calendar localScheduledEnd = GregorianCalendar.getInstance(java.util.TimeZone.getTimeZone(timezoneID));		
		localScheduledEnd.setTime(activity.getScheduledEnd());
		java.util.Calendar scheduledEnd = GregorianCalendar.getInstance(java.util.TimeZone.getTimeZone("GMT-0:00"));
		scheduledEnd.set(java.util.Calendar.YEAR, localScheduledEnd.get(java.util.Calendar.YEAR));
		scheduledEnd.set(java.util.Calendar.MONTH, localScheduledEnd.get(java.util.Calendar.MONTH));
		scheduledEnd.set(java.util.Calendar.DAY_OF_MONTH, localScheduledEnd.get(java.util.Calendar.DAY_OF_MONTH));				
		scheduledEnd.set(java.util.Calendar.HOUR_OF_DAY, 0);
		scheduledEnd.set(java.util.Calendar.MINUTE, 0);
		scheduledEnd.set(java.util.Calendar.SECOND, 0);		
		scheduledEnd.set(java.util.Calendar.MILLISECOND, 0);		
		if(scheduledStart.get(java.util.Calendar.DAY_OF_MONTH) == scheduledEnd.get(java.util.Calendar.DAY_OF_MONTH)) {
			scheduledEnd.add(
				java.util.Calendar.DAY_OF_MONTH,
				1
			);
		}
		activity.setScheduledStart(scheduledStart.getTime());
		activity.setScheduledEnd(scheduledEnd.getTime());
    }
    
    /**
     * Perform a followUp and link it to given activity.
     * 
     * @param activity
     * @param processTransition
     * @param linkTo
     * @return
     * @throws ServiceException
     */
    public ActivityFollowUp linkToAndFollowUp(
        Activity activity,
        ActivityProcessTransition processTransition,
    	Activity linkTo,
    	WfProcessInstance parentProcessInstance
    ) throws ServiceException {
    	if(linkTo == null) return null;
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
    	String followUpTitle = null;
    	String followUpText = null;
    	// Append activity number to linkTo.name if not already present
    	if(linkTo.getName() != null && ((linkTo.getName().indexOf(" [#") < 0) || !linkTo.getName().endsWith("]"))) {
    		linkTo.setName(linkTo.getName() + " [#" + activity.getActivityNumber() + "]");
    	}
    	if(linkTo instanceof EMail) {
    		EMail email = (EMail)linkTo;
    		// append activity number to linkTo.messageSubject if not already present
    		if(email.getMessageSubject() != null && ((email.getMessageSubject().indexOf(" [#") < 0) || !email.getMessageSubject().endsWith("]"))) {
    			email.setMessageSubject(email.getMessageSubject() + " [#" + activity.getActivityNumber() + "]");
    		}
    		followUpTitle = email.getMessageSubject();
    		followUpText = email.getMessageBody();
    		// Cut the text. Only from start up to the first '> '.
    		if(followUpText != null && followUpText.indexOf("\n> ") > 0) {
    			followUpText = followUpText.substring(
    				0,
    				followUpText.indexOf("\n> ")
    			) + "\n...";
    		}
    	} else {
    		followUpTitle = linkTo.getName();
    		followUpText = linkTo.getDetailedDescription();
    	}
    	ActivityFollowUp followUp = this.doFollowUp(
    		activity, 
    		followUpTitle, 
    		followUpText, 
    		processTransition, 
    		linkTo.getReportingContact(),
    		parentProcessInstance
    	);
    	followUp.setActivity(linkTo);
    	ActivityLinkTo activityLink = pm.newInstance(ActivityLinkTo.class);
    	activityLink.setName("#" + linkTo.getActivityNumber() + ": " + activity.getName());
    	activityLink.setActivityLinkType(ActivityLinkType.RELATES_TO.getValue());
    	activityLink.setLinkTo(linkTo);
    	activity.addActivityLinkTo(
    		this.getUidAsString(),
    		activityLink
    	);
    	return followUp;
    }
    
    /**
     * Strip leading non-letter characters.
     * 
     * @param s
     * @return
     */
    protected String stripLeadingNonLetters(
    	String s
    ) {
    	if(s == null) return null;
    	int i = 0;
    	while(i < s.length() && !Character.isLetter(s.charAt(i))) {
    		i++;
    	}
    	return i < s.length() ? s.substring(i) : s;
    }
    
    /**
     * Strip trailing non letter characters.
     * 
     * @param s
     * @return
     */
    protected String stripTrailingNonLetters(
    	String s
    ) {
    	if(s == null) return null;
    	int i = s.length();
    	while(i > 0 && !Character.isLetter(s.charAt(i-1))) {
    		i--;
    	}
    	return i >= 0 ? s.substring(0, i) : s;
    }
    
    /**
     * Get activity name of sub-activity according to name pattern of sub-activity transition.
     * 
     * @param transition
     * @param activity
     * @param followUpTitle
     * @return
     */
    protected String getFollowUpSubActivityName(
    	SubActivityTransition transition,
    	Activity activity,
    	String followUpTitle
    ) {
    	String namePattern = transition.getSubActivityNamePattern() == null ?
    		"${A} - ${N} - ${F}" : 
    			transition.getSubActivityNamePattern();
    	String name = namePattern;
    	{
	    	String value = activity.getName() == null ? "" : activity.getName();
	    	String[] values = value.split("~");
	   		name = name.replace("${A}.Prefix", values[0]);
	   		name = name.replace("${A}.Suffix", values[values.length-1]);	    	
	   		name = name.replace("${A}", value);
    	}
    	{
    		String value = transition.getName() == null ? "" : this.stripLeadingNonLetters(transition.getName());
    		String[] values = value.split("~");
    		name = name.replace("${N}.Prefix", values[0]);
    		name = name.replace("${N}.Suffix", values[values.length-1]);
    		name = name.replace("${N}", value);    		
    	}
    	{
    		String value =  followUpTitle == null ? "" : followUpTitle;
    		String[] values = value.split("~");
    		name = name.replace("${F}.Prefix", values[0]);
    		name = name.replace("${F}.Suffix", values[values.length-1]);
    		name = name.replace("${F}", value);
    	}
    	return this.stripTrailingNonLetters(name);
    }
    
    /**
     * Get activity group name of sub-activity according to name pattern of sub-activity transition.
     * 
     * @param transition
     * @param activity
     * @param activityGroup
     * @param followUpTitle
     * @return
     */
    protected String getFollowUpSubActivityGroupName(
    	SubActivityTransition transition,
    	Activity activity,
    	ActivityGroup activityGroup,
    	String followUpTitle
    ) {
    	String namePattern = transition.getTemplateNamePattern() == null ?
    		"${G} - ${A}" : 
    			transition.getTemplateNamePattern();
    	String name = namePattern;
    	{
	    	String value = activity.getName() == null ? "" : activity.getName();
	    	String[] values = value.split("~");
	   		name = name.replace("${A}.Prefix", values[0]);
	   		name = name.replace("${A}.Suffix", values[values.length-1]);	    	
	   		name = name.replace("${A}", value);
    	}
    	{
    		String value = transition.getName() == null ? "" : this.stripLeadingNonLetters(transition.getName());
    		String[] values = value.split("~");
    		name = name.replace("${N}.Prefix", values[0]);
    		name = name.replace("${N}.Suffix", values[values.length-1]);
    		name = name.replace("${N}", value);    		
    	}
    	{
    		String value = activityGroup.getName() == null ? "" : activityGroup.getName();
    		String[] values = value.split("~");
    		name = name.replace("${G}.Prefix", values[0]);
    		name = name.replace("${G}.Suffix", values[values.length-1]);
    		name = name.replace("${G}", value);
    	}
    	{
    		String value =  followUpTitle == null ? "" : followUpTitle;
    		String[] values = value.split("~");
    		name = name.replace("${F}.Prefix", values[0]);
    		name = name.replace("${F}.Suffix", values[values.length-1]);
    		name = name.replace("${F}", value);
    	}
    	return this.stripTrailingNonLetters(name);
    }
    
    /**
     * Perform a follow up on an activity.
     * 
     * @param activity
     * @param followUpTitle
     * @param followUpText
     * @param processTransition
     * @param assignTo
     * @param parentProcessInstance
     * @return
     * @throws ServiceException
     */
    public ActivityFollowUp doFollowUp(
        Activity activity,
        String followUpTitle,
        String followUpText,
        ActivityProcessTransition processTransition,
        Contact assignTo,
        WfProcessInstance parentProcessInstance
    ) throws ServiceException {
    	return this.doFollowUp(
    		activity,
    		followUpTitle,
    		followUpText,
    		processTransition,
    		assignTo,
    		parentProcessInstance,
    		true // validateStates
    	);
    }
    
    /**
     * Perform a follow up on an activity.

     * @param activity
     * @param followUpTitle
     * @param followUpText
     * @param processTransition
     * @param assignTo
     * @param parentProcessInstance
     * @param validateStates
     * @return
     * @throws ServiceException
     */
    public ActivityFollowUp doFollowUp(
        Activity activity,
        String followUpTitle,
        String followUpText,
        ActivityProcessTransition processTransition,
        Contact assignTo,
        WfProcessInstance parentProcessInstance,
        boolean validateStates    
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);    	
    	String providerName = activity.refGetPath().getSegment(2).toString();
    	String segmentName = activity.refGetPath().getSegment(4).toString();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(pm, providerName, segmentName);
        ActivityProcessState processState = activity.getProcessState();
        if(processTransition != null) {
            if(processTransition.getNextState() == null) {
                throw new ServiceException(
                    OpenCrxException.DOMAIN,
                    OpenCrxException.ACTIVITY_UNDEFINED_NEXT_STATE, 
                    "Undefined next state. Transition not possible.",
                    new BasicException.Parameter("param0", processTransition.refGetPath())
                );
            }
            // Check that previous state of transition matches the current activity's state
            if(
                ((processTransition.getPrevState() == null) && (processState == null)) ||
                ((processTransition.getPrevState() != null) && processTransition.getPrevState().equals(processState))
            ) {
                // valid
            } else {
            	if(validateStates) {
	                throw new ServiceException(
	                    OpenCrxException.DOMAIN,
	                    OpenCrxException.ACTIVITY_TRANSITION_NOT_VALID_FOR_STATE, 
	                    "Transition is not valid for current state",
	                     new BasicException.Parameter("param0", processTransition.refGetPath()),
	                     new BasicException.Parameter("param1", processState.refGetPath())
	                );
            	}
            }
            // Apply transition to activity
            activity.setLastTransition(
                processTransition         
            );    
            activity.setPercentComplete(
                processTransition.getNewPercentComplete()
            );
            activity.setActivityState(
                processTransition.getNewActivityState()
            );
            
            /**
             * Execute actions. If at least the execution of one action fails
             * the transition is considered as failed. In this case the activity
             * is set to set errState if defined.
             */
            ActivityProcessState nextState = processTransition.getNextState();
            ActivityProcessState errState = processTransition.getErrState();
            ActivityProcessActionQuery activityProcessActionQuery = (ActivityProcessActionQuery)pm.newQuery(ActivityProcessAction.class);
            activityProcessActionQuery.orderByName().ascending();
            List<ActivityProcessAction> actions = processTransition.getAction(activityProcessActionQuery);
            boolean failed = false;
            for(ActivityProcessAction action: actions) {
                // SetActualEndAction
                if(action instanceof SetActualEndAction) {
                	SetActualEndAction setActualEndAction = (SetActualEndAction)action;
                    if((setActualEndAction.isResetToNull() != null) && setActualEndAction.isResetToNull().booleanValue()) {
                        activity.setActualEnd(null);
                    } else {
                        activity.setActualEnd(new Date());
                    }
                }
                // SetActualStartAction
                else if(action instanceof SetActualStartAction) {
                	SetActualStartAction setActualStartAction = (SetActualStartAction)action;
                    if((setActualStartAction.isResetToNull() != null) && setActualStartAction.isResetToNull().booleanValue()) {
                        activity.setActualStart(null);
                    } else {
                        activity.setActualStart(new Date());
                    }
                }
                // SetAssignedToAction
                else if(action instanceof SetAssignedToAction) {
                	SetAssignedToAction setAssignedToAction = (SetAssignedToAction)action;
                    // Determine contact to which activity is assigned to
                    Contact contact = null;
                    try {
                        if(setAssignedToAction.getContactFeatureName() != null) {
                        	try {
                        		contact = (Contact)activity.refGetValue(setAssignedToAction.getContactFeatureName());
                        	} catch(Exception e) {}
                        }
                        if(contact == null) {
                            UserHome userHome = UserHomes.getInstance().getUserHome(action.refGetPath(), pm);
                            contact = userHome.getContact();
                        }
                        if(contact != null) {
                        	List<Resource> resources = this.findResources(contact);
                        	if(!resources.isEmpty()) {
                                this.assignTo(
                                    activity,
                                    resources.iterator().next()
                                );
                            }
                        }
                    } catch(Exception e) {
                    	SysLog.warning("Execution of action failed --> transition failed.", action);
                        new ServiceException(e).log();
                        failed = true;
                    }                            
                }
                // WfAction
                else if(action instanceof WfAction) {
                    UserHome userHome = UserHomes.getInstance().getUserHome(action.refGetPath(), pm);
                    WfAction wfAction = (WfAction)action;
                    if(wfAction.getWfProcess() != null) {
                        try {
                            WfProcessInstance wfProcessInstance = 
                                Workflows.getInstance().executeWorkflow(
                                	Base.getInstance().getTitle(activity, null, (short)0, false) + " / " + processTransition.getName() + " / " + userHome.refGetPath().getLastSegment().toString(),
                                    userHome, // wfTarget
                                    wfAction.getWfProcess(), // wfProcess
                                    activity, // targetObject
                                    null, // booleanParams
                                    null, // stringParams
                                    null, // integerParams
                                    null, // decimalParams
                                    null, // dateTimeParams
                                    null, // uriParams
                                    parentProcessInstance
                                );
                            SysLog.detail("Execution of workflow successful.", action);
                            Boolean wfExecutionFailed = wfProcessInstance.isFailed();
                            if(Boolean.TRUE.equals(wfExecutionFailed)) {
                                failed = true;
                            }
                        } catch(Exception e) {
                        	SysLog.warning("Execution of action failed --> transition failed.", action);
                            new ServiceException(e).log();
                            failed = true;
                        }
                    }
                }
                // ActivityCreationAction
                else if(action instanceof ActivityCreationAction) {
                	ActivityCreationAction activityCreationAction = (ActivityCreationAction)action;
                    if(activityCreationAction.getActivityCreator() != null) {
                        try {
                            Activity newActivity = this.newActivity(
                            	activityCreationAction.getActivityCreator(),
                                activityCreationAction.getName(),
                                activityCreationAction.getActivityDescription(),
                                null, // detailedDescription
                                null, // scheduledStart
                                null, // scheduledEnd
                                null, // dueBy
                                null, // priority
                                ICalendar.ICAL_TYPE_NA, // icalType
                                ICalClass.NA,
                                activity.getReportingContact(),
                                activity // creationContext
                            );
                            // Link new activity with original
                            ActivityLinkTo activityLinkTo = pm.newInstance(ActivityLinkTo.class);
                            activityLinkTo.setName(activity.getName());
                            activityLinkTo.setActivityLinkType(ActivityLinkType.IS_DERIVED_FROM.getValue());
                            activityLinkTo.setLinkTo(activity);
                            newActivity.addActivityLinkTo(
                            	false,
                            	this.getUidAsString(),
                            	activityLinkTo
                            );
                        } catch(Exception e) {
                        	SysLog.warning("Execution of action failed --> transition failed.", action);
                            new ServiceException(e).log();
                            failed = true;
                        }         
                    }
                }
                // LinkedActivityFollowUpAction
                else if(activity instanceof LinkedActivityFollowUpAction) {
                	LinkedActivityFollowUpAction linkedActivityFollowUpAction = (LinkedActivityFollowUpAction)action;
                    Collection<ActivityLinkTo> activityLinks = activity.getActivityLinkTo();
                    for(ActivityLinkTo activityLink: activityLinks) {
                        short activityLinkType = activityLink.getActivityLinkType();
                        short actionActivityLinkType = linkedActivityFollowUpAction.getActivityLinkType();  
                        if(
                            (activityLink.getLinkTo() != null) &&
                            (linkedActivityFollowUpAction.getTransition() != null) &&
                            (actionActivityLinkType == activityLinkType)
                        ) {
                            try {
                                this.doFollowUp(
                                    activityLink.getLinkTo(),
                                    linkedActivityFollowUpAction.getTransitionTitle(),
                                    linkedActivityFollowUpAction.getTransitionText(),
                                    linkedActivityFollowUpAction.getTransition(),
                                    null,
                                    parentProcessInstance
                                );
                            } catch(Exception e) {
                            	SysLog.warning("Execution of action failed --> transition failed.", action);
                                new ServiceException(e).log();
                                failed = true;
                            }
                        }
                    }
                }                
            }
            activity.setProcessState(
                failed && errState != null ? 
                	errState : 
                	nextState
            );
        }        
        // Create follow up
        ActivityFollowUp followUp = null;
        if(processTransition instanceof SubActivityTransition) {
        	followUp = pm.newInstance(SubActivity.class);        	
            SubActivityTransition transition = (SubActivityTransition)processTransition;
            Activity subActivity = null;
            if(transition.getActivityCreator() != null) {
            	String subActivityName = this.getFollowUpSubActivityName(
            		transition,
            		activity,
            		followUpTitle
            	);
            	List<ActivityGroup> activityGroups = new ArrayList<ActivityGroup>();
            	if(Boolean.TRUE.equals(transition.isUseCreatorAsTemplate())) {
            		// If isUseCreatorAsTemplate is true the creator serves as template
            		// for activity creation. All activity groups of the creator are
            		// cloned and used for activity creation in addition to the template groups.
            		List<ActivityGroup> templateActivityGroups = transition.getActivityCreator().getActivityGroup();
            		activityGroups.addAll(templateActivityGroups);
            		for(ActivityGroup activityGroup: templateActivityGroups) {
            			String clonedGroupName = this.getFollowUpSubActivityGroupName(
            				transition,
            				activity,
            				activityGroup,
            				followUpTitle
            			);
            			ActivityGroup clonedGroup = null;
            			clonedGroup = this.findActivityTracker(clonedGroupName, activitySegment, activity);
            			if(clonedGroup == null) {
            				clonedGroup = this.findActivityCategory(clonedGroupName, activitySegment, activity);
            			}
            			if(clonedGroup == null) {
            				clonedGroup = this.findActivityMilestone(clonedGroupName, activitySegment, activity);
            			}
            			if(clonedGroup == null) {
            				clonedGroup = (ActivityGroup)Cloneable.getInstance().cloneObject(
            					activityGroup, 
            					activitySegment, 
            					activityGroup.refGetPath().getParent().getLastSegment().toString(), 
            					null, // objectMarshallers 
            					"", // referenceFilterAsString 
            					activityGroup.getOwningUser(), 
            					activityGroup.getOwningGroup()
            				);
            				clonedGroup.setName(clonedGroupName);
            				clonedGroup.setCreationContext(activity);
            			}
        				activityGroups.add(clonedGroup);
            		}
            	} else {
            		activityGroups = transition.getActivityCreator().getActivityGroup();
            	}
            	String subActivityDescription = followUpText;
            	subActivityDescription = subActivityDescription == null || subActivityDescription.length() < 100 ?
            		subActivityDescription :
            			subActivityDescription.substring(0, 100) + "...";
            	String subActivityDetailedDescription = followUpText;
            	subActivity = this.newActivity(
                	transition.getActivityCreator(),
                    subActivityName,
                    subActivityDescription,
                    subActivityDetailedDescription, // detailedDescription
                    null, // suppliedScheduledStart
                    null, // suppliedScheduledEnd
                    null, // suppliedDueBy
                    null, // suppliedPriority
                    ICalendar.ICAL_TYPE_NA,
                    ICalClass.NA,
                    activity.getReportingContact(),
                    followUp, // creationContext
                    activityGroups
                );
                // Link sub activity with original
                ActivityLinkTo activityLinkTo = pm.newInstance(ActivityLinkTo.class);
                activityLinkTo.setName(activity.getName());
                activityLinkTo.setActivityLinkType(ActivityLinkType.IS_CHILD_OF.getValue());
                activityLinkTo.setLinkTo(activity);
                subActivity.addActivityLinkTo(
                	this.getUidAsString(),
                	activityLinkTo
                );
            }
            followUp.setActivity(subActivity);
        } else {
        	followUp = pm.newInstance(ActivityFollowUp.class);
        }
        followUp.setTransition(processTransition);
        followUp.setTitle(followUpTitle);
        followUp.setText(followUpText);
        activity.addFollowUp(
        	this.getUidAsString(),
        	followUp
        );
        if(assignTo == null) {
            Base.getInstance().assignToMe(
                followUp,
                true, // overwrite
                false // useRunAsPrincipal
            );
        } else {
            followUp.setAssignedTo(assignTo);
        }
        return followUp;
    }
        
    /**
     * Update a work record.
     * 
     * @param workRecord
     * @throws ServiceException
     */
    protected void updateWorkAndExpenseRecord(
    	WorkAndExpenseRecord workRecord
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(workRecord);
        PersistenceManager pmRoot =  pm.getPersistenceManagerFactory().getPersistenceManager(
        	SecurityKeys.ROOT_PRINCIPAL,
        	null
        );
        BigDecimal amount = workRecord.getQuantity();
        BigDecimal rate = workRecord.getRate();
        workRecord.setBillableAmount(
        	amount == null || rate == null ?
        		null :
        		rate.multiply(workRecord.getQuantity())        	
        );
        BigDecimal oldAmount = null;
        try {
        	oldAmount = ((ActivityWorkRecord)pmRoot.getObjectById(workRecord.refGetPath())).getQuantity(); 
        }
        catch(Exception e) {}
        // Update booking
        if(!Utils.areEqual(amount, oldAmount)) {
	        // depotSelector
	        short depotSelector = workRecord.getDepotSelector() != 0 ? 
	        	workRecord.getDepotSelector() : 
	        	DepotUsage.WORK_EFFORT.getValue(); 
	        if(depotSelector == 0) {
	            depotSelector = DepotUsage.WORK_EFFORT.getValue();
	        }
	        // Update work booking if duration has been changed                
            if(workRecord.getWorkCb() != null) {
                try {
                    Depots.getInstance().removeCompoundBooking(
                        workRecord.getWorkCb(),
                        false
                    );
                } catch(Exception e) {}
            }
            workRecord.setWorkCb(null);
            // Depot credit
            // Get assigned depot of resource with usage DEPOT_USAGE_WORK_EFFORT
            Depot depotCredit = null;
            ResourceAssignment resourceAssignment = (ResourceAssignment)pm.getObjectById(
            	workRecord.refGetPath().getParent().getParent()
            );
            Resource resource = resourceAssignment.getResource();
            Collection<DepotReference> depotReferences = null;
            if(resource == null) {
                depotReferences = Collections.emptyList();
            } else {
                depotReferences = resource.getDepotReference();
            }
            // Depot selector
            for(DepotReference depotReference: depotReferences) {
                short depotUsage = depotReference.getDepotUsage();
                if(depotUsage == depotSelector) {
                    depotCredit = depotReference.getDepot();
                }
            }            
            // Depot debit
            // Get assigned depot of activity with usage DEPOT_USAGE_WORK_EFFORT
            Depot depotDebit = null;
            Activity activity = (Activity)pm.getObjectById(
                workRecord.refGetPath().getPrefix(workRecord.refGetPath().size() - 4)
            );
            depotReferences = activity.getDepotReference();
            for(DepotReference depotReference: depotReferences) {
                short depotUsage = depotReference.getDepotUsage();
                if(depotUsage == depotSelector) {
                    depotDebit = depotReference.getDepot();
               }
            }
            ActivityType activityType = activity.getActivityType();
            if(
                (depotCredit != null) &&
                (depotDebit != null) &&
                (activityType != null)
            ) {
                if(!depotCredit.refGetPath().getPrefix(7).equals(depotDebit.refGetPath().getPrefix(7))) {
                    throw new ServiceException(
                        OpenCrxException.DOMAIN,
                        OpenCrxException.DEPOT_POSITION_IS_LOCKED,
                        "Depot entity not equal",
                        new BasicException.Parameter("param0", depotDebit.refGetPath()),
                        new BasicException.Parameter("param1", depotCredit.refGetPath())
                    );
                } else {
                    Date valueDate = workRecord.getEndedAt();
                    DepotPosition positionCredit = Depots.getInstance().openDepotPosition(
                        depotCredit,
                        activityType.getName(),
                        activityType.getDescription(),
                        valueDate,
                        null, // if position does not exist open at value date
                        null,
                        Boolean.FALSE
                    );
                    DepotPosition positionDebit = Depots.getInstance().openDepotPosition(
                        depotDebit,
                        activityType.getName(),
                        activityType.getDescription(),
                        valueDate,
                        null, // if position does not exist open at value date
                        null,
                        Boolean.FALSE
                    );
                    CompoundBooking workCb = 
                        Depots.getInstance().createCreditDebitBooking(
                            (DepotEntity)pm.getObjectById(depotCredit.refGetPath().getPrefix(7)),
                            workRecord.getEndedAt(),
                            BookingType.STANDARD.getValue(),
                            workRecord.getQuantity(),
                            BOOKING_TEXT_NAME_WORK_EFFORT,
                            activityType.getWorkBt(),
                            positionCredit,
                            positionDebit,
                            workRecord, // origin
                            null, // no booking text suffix
                            new ArrayList<String>()
                        );
                    workRecord.setWorkCb(workCb);
                }
	        }
    	}
    }

    /**
     * Creates and adds a work record.
     * 
     * @param activity
     * @param resource
     * @param name
     * @param description
     * @param startedAt
     * @param endedAt
     * @param quantity
     * @param quantityUom
     * @param recordType
     * @param paymentType
     * @param depotSelector
     * @param rate
     * @param rateCurrency
     * @param isBillable
     * @param isReimbursable
     * @param owningGroups
     * @return
     * @throws ServiceException
     */
    public ActivityWorkRecord addWorkAndExpenseRecord(
        Activity activity,
        Resource resource,
        String name,
        String description,
        Date startedAt,
        Date endedAt,
        BigDecimal quantity,
        Uom quantityUom,
        short recordType,
        short paymentType,
        short depotSelector,
        BigDecimal rate,
        short rateCurrency,
        Boolean isBillable,
        Boolean isReimbursable,
        List<PrincipalGroup> owningGroups
    ) throws ServiceException {
        ActivityWorkRecord workRecord = null;
        if(resource == null) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.ACTIVITY_CAN_NOT_ADD_WORK_RECORD_MISSING_RESOURCE,
               "Can not add work record. Missing resource"
           );            
        }
        if(activity == null) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.ACTIVITY_CAN_NOT_ADD_WORK_RECORD_MISSING_ACTIVITY,
               "Can not add work record. Missing activity"
           );            
        }        
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
    	// Validate groups
    	List<PrincipalGroup> groups = new ArrayList<PrincipalGroup>();
    	for(PrincipalGroup group: owningGroups) {
    		if(group != null) {
    			groups.add(group);
    		}
    	}
        ResourceAssignmentQuery resourceAssignmentQuery = (ResourceAssignmentQuery)pm.newQuery(ResourceAssignment.class);
        resourceAssignmentQuery.thereExistsResource().equalTo(resource);
        Collection<ResourceAssignment> resourceAssignments = activity.getAssignedResource(resourceAssignmentQuery);
        ResourceAssignment resourceAssignment = null;
        if(resourceAssignments.isEmpty()) {
            resourceAssignment = this.createResourceAssignment(
                activity, 
                resource, 
                (short)0,
                groups
            );
        } else {
            resourceAssignment = resourceAssignments.iterator().next();
        }            
        workRecord = pm.newInstance(ActivityWorkRecord.class);
        if(name != null) {
            workRecord.setName(name);
        }
        if(description != null) {
            workRecord.setDescription(description);
        }           
        try {
	        workRecord.setStartedAt(
	            startedAt == null ? 
	            	(startedAt = new Date()) : 
	            	startedAt
	        );
	        workRecord.setEndedAt(
	            endedAt == null ?
	            	quantity == null ?
	            		null :
	            	    (endedAt = DateTimeFormat.BASIC_UTC_FORMAT.parse(DateTimeFormat.BASIC_UTC_FORMAT.format(new Date(startedAt.getTime() + quantity.multiply(new BigDecimal(3600000L)).longValue())))) : 
	            	endedAt
	        );
        } catch(Exception e) {}
        // Default amount is endedAt - startedAt in hours/minutes
        if(quantity == null && endedAt != null && startedAt != null) {
        	workRecord.setQuantity(
        		new BigDecimal(Math.abs(endedAt.getTime() - startedAt.getTime()) / 60000L).divide(new BigDecimal("60.0"), RoundingMode.FLOOR)
        	);
        	Uom uomHour = null;
        	try {
        		uomHour = (Uom)pm.getObjectById(
        			new Path("xri:@openmdx:org.opencrx.kernel.uom1/provider/" + activity.refGetPath().getSegment(2) + "/segment/Root/uom/hour")
        		);
        	} catch(Exception e) {}
        	workRecord.setQuantityUom(uomHour);
        } else {
        	workRecord.setQuantity(quantity);        	
            workRecord.setQuantityUom(quantityUom);
        }
        // Get default rate from resource in case of work records
        if(
        	(rate == null) && 
        	(recordType == WorkRecordType.OVERTIME.getValue() || recordType ==  WorkRecordType.STANDARD.getValue())
        ) {
			ResourceRateQuery query = (ResourceRateQuery)pm.newQuery(ResourceRate.class);
			query.rateType().equalTo(recordType);
			List<ResourceRate> resourceRates = resource.getResourceRate(query);
			if(!resourceRates.isEmpty()) {
				ResourceRate resourceRate = resourceRates.iterator().next();
				workRecord.setBillingCurrency(resourceRate.getRateCurrency());
				workRecord.setRate(resourceRate.getRate());
			}
        } else {
        	workRecord.setRate(rate);
            workRecord.setBillingCurrency(rateCurrency);
        }
        workRecord.setRecordType(recordType);
        workRecord.setPaymentType(paymentType);
        workRecord.setBillable(isBillable);
        workRecord.setReimbursable(isReimbursable);
        workRecord.setDepotSelector(depotSelector);
        if(groups != null && !groups.isEmpty()) {
        	workRecord.getOwningGroup().addAll(groups);
        } else {
        	workRecord.getOwningGroup().addAll(
        		resourceAssignment.getOwningGroup()
        	);
        }
        resourceAssignment.addWorkRecord(
        	false,
        	this.getUidAsString(),
        	workRecord
        );
        this.updateWorkAndExpenseRecord(
        	workRecord
        );
        return workRecord;
    }
    
    /**
     * Callback for removing a work record. Throws exception in case of
     * existing bookings.
     * 
     * @param workRecord
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeWorkRecord(
    	WorkAndExpenseRecord workRecord,
        boolean preDelete
    ) throws ServiceException {
        if(workRecord.getWorkCb() != null) {
            Depots.getInstance().removeCompoundBooking(
                workRecord.getWorkCb(),
                false
            );
        }
        if(!preDelete) {
        	workRecord.refDelete();
        }
    }
    
    /**
     * Callback for removing an activity group. Throws exception in case of
     * activities assigned to this group.
     * 
     * @param activityGroup
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeActivityGroup(
        ActivityGroup activityGroup,
        boolean preDelete
    ) throws ServiceException {
        Collection<Activity> activities = activityGroup.getFilteredActivity();
        // Don't allow removal if activity group has assigned activities
        for (Activity activity: activities) {
        	if (!JDOHelper.isDeleted(activity)){        
	            throw new ServiceException(
	                OpenCrxException.DOMAIN,
	                OpenCrxException.ACTIVITY_GROUP_HAS_ASSIGNED_ACTIVITIES, 
	                "Activity group has assigned activities. Can not remove.",
	                new BasicException.Parameter("param0", activityGroup.refGetPath())
	            );
        	}
        }
        if(!preDelete) {
        	activityGroup.refDelete();
        }
    }
    
    /**
     * Extract UID field from given ical.
     * 
     * @param ical
     * @return
     */
    protected String getICalUid(
    	String ical
    ) {
    	String uid = null;
    	if(ical.indexOf("UID:") > 0) {
    		int start = ical.indexOf("UID:");
    		int end = ical.indexOf("\n", start);
    		if(end > start) {
    			uid = ical.substring(start + 4, end).trim();
    		}
    	}    	
    	return uid;
    }

    /**
     * Update replicated activity.
     * 
     * @param activity
     * @param replica
     * @param linkTo
     */
    public void updateReplicatedActivity(
    	Activity activity,
    	Activity replica,
    	ActivityLinkTo linkTo
    ) {
    	// Original
    	if(linkTo.getActivityLinkType() == ActivityLinkType.IS_REPLICA_OF.getValue()) {
    		replica.setName(activity.getName());
    		replica.setDescription(activity.getDescription());
    		replica.setDetailedDescription(activity.getDetailedDescription());
    		replica.setMisc1(activity.getMisc1());
    		replica.setMisc2(activity.getMisc2());
    		replica.setMisc3(activity.getMisc3());
    		replica.setLocation(activity.getLocation());
    	} else if(linkTo.getActivityLinkType() == ActivityLinkType.IS_REPLICA_OF_OBFUSCATED.getValue()) {
        	// Obfuscate
    		replica.setName(linkTo.getName());
    		replica.setDescription(linkTo.getDescription());
    	}
    	String ical = activity.getIcal();
    	// Copy ical-specific fields to replica
    	if(ical != null) {
    		for(String field: Arrays.asList("EXDATE:")) {
	    		String fieldValue = null;
	    		int pos1 = ical.indexOf(field);
	    		if(pos1 >= 0) {
	    			int pos2 = ical.indexOf("\n", ical.lastIndexOf(field));
	    			if(pos2 > pos1) {
	    				fieldValue = ical.substring(pos1, pos2);
	    			}
	    		}
	    		if(fieldValue != null) {
	    			String icalReplica = replica.getIcal();
	    			if(icalReplica != null) {
	    				pos1 = icalReplica.indexOf(field);
	    				if(pos1 > 0) {
	    					int pos2 = icalReplica.indexOf("\n", icalReplica.lastIndexOf(field));
	    					if(pos2 > pos1) {
	    						icalReplica = icalReplica.substring(0, pos1) + icalReplica.substring(pos2 + 1);
	    					}
	    				}
	    				if(icalReplica.indexOf("END:VEVENT") > 0) {
	    					icalReplica = icalReplica.replace("END:VEVENT", fieldValue + "\n" + "END:VEVENT");
	    				} else if(icalReplica.indexOf("END:VTODO") > 0) {
	    					icalReplica = icalReplica.replace("END:VTODO", fieldValue + "\n" + "END:VTODO");	    					
	    				}
	    				replica.setIcal(icalReplica);
	    			}
	    		}
    		}
    	}
		replica.setScheduledStart(activity.getScheduledStart());
		replica.setScheduledEnd(activity.getScheduledEnd());
		replica.setActualStart(activity.getActualStart());
		replica.setActualEnd(activity.getActualEnd());
		replica.setDueBy(activity.getDueBy());
		replica.setPriority(activity.getPriority());
		replica.setRecurrenceRule(activity.getRecurrenceRule());
		replica.setDisabled(activity.isDisabled());
    }

    /**
     * Update calculated and derived fields of given activity. This
     * method delegates to updateActivity(activity, updateIcal=true, updateReplicas=true).
     * Override for custom-specific behavior.
     * 
     * @param activity
     * @throws ServiceException
     */
    protected void updateActivity(
        Activity activity
    ) throws ServiceException {
    	this.updateActivity(
    		activity,
    		true,
    		true
    	);
    }

    /**
     * Update calculated and derived fields of given activity.
     * 
     * @param activity
     * @param updateIcal
     * @param updateReplicas
     * @throws ServiceException
     */
    protected void updateActivity(
        Activity activity,
        boolean updateIcal,
        boolean updateReplicas
    ) throws ServiceException {
        if(!JDOHelper.isPersistent(activity) && JDOHelper.isNew(activity)) {
            if((activity.getDueBy() == null)) {
            	try {
            		activity.setDueBy(DateTimeFormat.BASIC_UTC_FORMAT.parse("99991231T000000.000Z"));
            	} catch(Exception e) {}
            }
            if(activity.getPercentComplete() == null) {
                activity.setPercentComplete(Short.valueOf((short)0));
            }
        }
        if(updateIcal) {
	        List<String> statusMessage = new ArrayList<String>();
	        String ical = ICalendar.getInstance().mergeIcal(
	        	activity, 
	        	activity.getIcal(),
	        	statusMessage 
	        );
	        {
	        	String newIcal = ical == null ? "" : ical
	        		.replace("\r", "")
	        		.replaceAll("LAST-MODIFIED:.*\n", "")
	        		.replaceAll("DTSTAMP:.*\n", "")
	        		.trim();
	        	String oldIcal = activity.getIcal() == null ? "" : activity.getIcal()
	        		.replaceAll("\r", "")
	        		.replaceAll("LAST-MODIFIED:.*\n", "")
	        		.replaceAll("DTSTAMP:.*\n", "")
	        		.trim();
	            if(!Utils.areEqual(newIcal, oldIcal)) {
	                activity.setIcal(ical);
	            }
	        }
	        // Assertion externalLink().contains(ical uid)
	        String uid = this.getICalUid(ical);
	        if(uid != null) {
		        boolean icalLinkMatches = false;
		        boolean hasICalLink = false;
		        for(String externalLink: activity.getExternalLink()) {
		        	if(externalLink.startsWith(ICalendar.ICAL_SCHEMA)) {
		        		hasICalLink = true;
		        		if(externalLink.endsWith(uid)) {
			        		icalLinkMatches = true;
			        		break;
		        		}
		        	}
		        }
		        if(!hasICalLink) {
		        	activity.getExternalLink().add(
		        		ICalendar.ICAL_SCHEMA + uid
		        	);
		        } else if(!icalLinkMatches) {
		        	// Should not happen. Log warning.
		        	// Activity must be fixed manually with updateICal() operation
		        	SysLog.warning("Activity's external link does not contain ical UID", Arrays.asList(activity.refGetPath().toString(), activity.getActivityNumber()));
		        	ServiceException e = new ServiceException(
		        		BasicException.Code.DEFAULT_DOMAIN,
		        		BasicException.Code.ASSERTION_FAILURE,
		        		"Activity's external link does not contain ical UID",
		        		new BasicException.Parameter("activity", activity),
		        		new BasicException.Parameter("externalLink", activity.getExternalLink()),
		        		new BasicException.Parameter("ical", ical)        		
		        	);
		        	SysLog.detail(e.getMessage(), e.getCause());
		        }
	        }
        }
        // Update replicas
        if(updateReplicas) {
        	this.createOrUpdateReplicatedActivities(activity);
        }
    }

    /**
     * Update calculated and derived fields of given activity group. Override for
     * custom-specific behavior.
     * 
     * @param activity
     * @throws ServiceException
     */    
    protected void updateActivityGroup(
    	ActivityGroup activityGroup
    ) throws ServiceException {    	
    }

    /**
     * Create resource assignment for given activity.
     * 
     * @param activity
     * @param resource
     * @param resourceOrder
     * @param owningGroups
     * @return
     * @throws ServiceException
     */
    public ResourceAssignment createResourceAssignment(
        Activity activity,
        Resource resource,
        short resourceOrder,
        List<PrincipalGroup> owningGroups
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);    	
        ResourceAssignment resourceAssignment = pm.newInstance(ResourceAssignment.class);
        resourceAssignment.setName(
            resource.getName()
        );
        resourceAssignment.setDescription(
            (activity.getName() != null ? activity.getName() : "") +
            " (" + 
            (resource.getName() != null ? resource.getName() : "") +
            ")"
        );
        resourceAssignment.setResource(resource);
        resourceAssignment.setResourceRole(Short.valueOf((short)0));
        resourceAssignment.setResourceOrder(Short.valueOf(resourceOrder));                    
        resourceAssignment.setWorkDurationPercentage(Short.valueOf((short)100));
        resourceAssignment.getOwningGroup().addAll(
        	activity.getOwningGroup()
        );
        if(owningGroups != null && !owningGroups.isEmpty()) {
        	resourceAssignment.getOwningGroup().addAll(owningGroups);
        } else {
        	resourceAssignment.getOwningGroup().addAll(
        		activity.getOwningGroup()
        	);
        }
        activity.addAssignedResource(
        	false,
        	this.getUidAsString(),
        	resourceAssignment
        );
        return resourceAssignment;
    }
    
    /**
     * Get resources which are assigned by default for given newly created activity.
     * 
     * @param activity
     * @return
     * @throws ServiceException
     */
    public List<Resource> getDefaultResources(
    	Activity activity
    ) throws ServiceException {
    	List<Resource> resources = new ArrayList<Resource>();
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
        // Add matching resources of current user
    	org.opencrx.kernel.account1.jmi1.Contact contact = 
    		UserHomes.getInstance().getUserHome(activity.refGetPath(), pm).getContact();
    	if(contact != null) {
        	resources.addAll(this.findResources(contact));
    	}
    	return resources;
    }

    /**
     * Get principal groups which are assigned by default to given newly created activity.
     * 
     * @param activity
     * @param activityGroups
     * @return
     * @throws ServiceException
     */
    public Set<PrincipalGroup> getDefaultOwningGroups(
    	Activity activity,
    	List<ActivityGroup> activityGroups
    ) throws ServiceException {
        // The owning groups of the activity is the
        // the union of all owning groups of the assigned activity groups. 
        // This way it is guaranteed that the activity can be viewed in all
        // assigned activity groups. 
    	// In case of private folders (name ends with suffix PRIVATE_FOLDER_SUFFIX) 
    	// only add principal groups matching the activity groups' name. 
    	// This way - by default - only the creator of the activity and the
    	// owner of the activity group have access to the activity.
        Set<PrincipalGroup> owningGroups = new HashSet<PrincipalGroup>();
        for(ActivityGroup activityGroup: activityGroups) {
            List<PrincipalGroup> groups = activityGroup.getOwningGroup();
            for(PrincipalGroup group: groups) {
                if(activityGroup.getName().endsWith(Base.PRIVATE_SUFFIX)) {
                	String activityGroupName = activityGroup.getName().substring(0, activityGroup.getName().indexOf("~"));
                	if(group.getName().indexOf(".") >= 0) {
                    	String principalGroupName = group.getName().substring(0, group.getName().indexOf("."));
                    	if(activityGroupName.equals(principalGroupName)) {
                    		owningGroups.add(group);
                    	}
                	}
                }
                else {                         	
                	owningGroups.add(group);
                }
            }
        }
        return owningGroups;
    }

    /**
     * Re-apply activity creator to given activity.
     * 
     * @param activity
     * @param activityCreator
     * @param activityGroups
     * @throws ServiceException
     */
    public void reapplyActivityCreator(
        Activity activity,
        ActivityCreator activityCreator,
        List<ActivityGroup> activityGroups
    ) throws ServiceException {
        if(activityCreator != null) {
        	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);        	
            ActivityType activityType = activityCreator.getActivityType();
            if(activityType != null) {
                // Type of activity must match defined activity class
                String activityClass = activityType.getActivityClassName() != null ? 
                	activityType.getActivityClassName() : 
                	ACTIVITY_TYPES[activityType.getActivityClass()];
                if(activityClass.equals(activity.refClass().refMofId())) {
                	Set<PrincipalGroup> owningGroups = this.getDefaultOwningGroups(
                		activity,
                		activityGroups
                	);
                	if(!owningGroups.containsAll(activity.getOwningGroup()) || !activity.getOwningGroup().containsAll(owningGroups)) {
	                    activity.getOwningGroup().clear();
	                    activity.getOwningGroup().addAll(owningGroups);
                	}
                    activity.setLastAppliedCreator(activityCreator);
                    // Create GroupAssignments
                    // Remove already assigned activity groups from list to be added
                    Collection<ActivityGroupAssignment> existingGroupAssignments = activity.getAssignedGroup();
                    List<ActivityGroup> excludeActivityGroups = new ArrayList<ActivityGroup>();
                    for(ActivityGroupAssignment existingGroupAssignment: existingGroupAssignments) {
                        if(existingGroupAssignment.getActivityGroup() != null) {
                            excludeActivityGroups.add(
                                existingGroupAssignment.getActivityGroup()
                            );
                        }
                    }
                    // Add new group assignments
                    for(ActivityGroup activityGroup: activityGroups) {
                        if(!excludeActivityGroups.contains(activityGroup)) {
                            ActivityGroupAssignment activityGroupAssignment = pm.newInstance(ActivityGroupAssignment.class);
                            activityGroupAssignment.setActivityGroup(activityGroup);
                            activityGroupAssignment.getOwningGroup().addAll(owningGroups);
                            activity.addAssignedGroup(
                            	false,
                            	this.getUidAsString(),
                            	activityGroupAssignment
                            );
                        }
                    }    
                    // Create ResourceAssignments
                    List<Resource> resources = new ArrayList<Resource>();
                    if(!activityCreator.getResource().isEmpty()) {
                        for(Resource resource: activityCreator.<Resource>getResource()) {
                            resources.add(resource);
                        }
                    } else {
                        for(Resource resource: this.getDefaultResources(activity)) {
                            resources.add(resource);
                        }
                    }
                    // Remove already assigned resources from list to be added 
                    Collection<ResourceAssignment> existingResourceAssignments = activity.getAssignedResource();
                    List<Resource> excludeResources = new ArrayList<Resource>();
                    for(ResourceAssignment existingResourceAssignment: existingResourceAssignments) {
                        if(existingResourceAssignment.getResource() != null) {
                            excludeResources.add(
                                existingResourceAssignment.getResource()
                            );
                        }
                    }                    
                    int ii = 0;
                    for(Resource resource: resources) {
                        if(!excludeResources.contains(resource)) {
                            this.createResourceAssignment(
                                activity,
                                resource,
                                (short)ii,
                                null
                            );
                            ii++;
                        }
                    }
                    // Create depot references
                    Collection<DepotReference> existingDepotReferences = activity.getDepotReference();
                    List<Short> excludesDepotUsages = new ArrayList<Short>();
                    for(DepotReference existingDepotReference: existingDepotReferences) {
                        excludesDepotUsages.add(
                        		Short.valueOf(existingDepotReference.getDepotUsage())
                        );
                    }                    
                    Collection<DepotReference> depotReferences = activityCreator.getDepotReference();
                    for(DepotReference depotReference: depotReferences) {
                        if(!excludesDepotUsages.contains(Short.valueOf(depotReference.getDepotUsage()))) {
                            Cloneable.getInstance().cloneObject(
                                depotReference,
                                activity,
                                "depotReference",
                                null, // marshallers
                                "",
                                activity.getOwningUser(),
                                activity.getOwningGroup()
                            );
                        }
                    }                            
                    // Create PropertySet
                    Collection<PropertySet> existingPropertySets = activity.getPropertySet();
                    List<String> excludePropertySets = new ArrayList<String>();
                    for(PropertySet existingPropertySet: existingPropertySets) {
                        excludePropertySets.add(
                            existingPropertySet.getName()
                        );
                    }                    
                    Collection<PropertySet> propertySets = activityCreator.getPropertySet();
                    for(PropertySet propertySet: propertySets) {
                    	// Only clone property sets from activity creator to 
                    	// activity which are marked as public (+|*)
                        if(
                        	(propertySet.getName().startsWith("+") || propertySet.getName().startsWith("*")) && 
                        	!excludePropertySets.contains(propertySet.getName())
                        ) {
                            Cloneable.getInstance().cloneObject(
                                propertySet,
                                activity,
                                "propertySet",
                                null,
                                "property",
                                activity.getOwningUser(),
                                activity.getOwningGroup()
                            );
                        }
                    }
                    // Set processState, lastTransition
                    activity.setActivityType(activityType);
                    activity.setProcessState(null);
                    activity.setLastTransition(null);
                    if(activityType.getControlledBy() != null) {
                        ActivityProcess activityProcess = activityType.getControlledBy();
                        // Try to find transition which most closely matches the current activity
                        // completeness and state. If no transition can be found set to start transition.
                        ActivityProcessTransition lastTransition = null;
                        ActivityProcessState processState = null;
                        if(activity.getPercentComplete() != null) {
                        	ActivityProcessTransitionQuery transitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
                        	transitionQuery.forAllDisabled().isFalse();
                        	transitionQuery.thereExistsNewPercentComplete().equalTo(activity.getPercentComplete());
                            List<ActivityProcessTransition> transitions = activityProcess.getTransition(transitionQuery);
                            if(!transitions.isEmpty()) {
                                lastTransition = transitions.iterator().next();
                                processState = lastTransition.getNextState();
                            }
                        }
                        if(lastTransition == null) {
                        	ActivityProcessTransitionQuery transitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
                        	transitionQuery.forAllDisabled().isFalse();
                        	transitionQuery.newActivityState().equalTo(activity.getActivityState());
                            List<ActivityProcessTransition> transitions = activityProcess.getTransition(transitionQuery); 
                            if(!transitions.isEmpty()) {
                                lastTransition = transitions.iterator().next();
                                processState = lastTransition.getNextState();
                            }
                        }
                        if(lastTransition == null) {
                            lastTransition = null;
                            processState = activityProcess.getStartState();
                        }
                        if(processState != null) {
                            activity.setProcessState(processState);
                        }
                        if(lastTransition != null) {
                            activity.setLastTransition(lastTransition);
                            activity.setPercentComplete(lastTransition.getNewPercentComplete());
                            activity.setActivityState(lastTransition.getNewActivityState());
                        }
                    }
                }
            }
        }
    }
    
    /**
     * Assign resource to given activity.
     * 
     * @param activity
     * @param resource
     * @throws ServiceException
     */
    public void assignTo(
        Activity activity,
        Resource resource
    ) throws ServiceException {
        if(resource != null) {
        	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);        	
            Contact contact = resource.getContact();
            ResourceAssignmentQuery resourceAssignmentQuery = (ResourceAssignmentQuery)pm.newQuery(ResourceAssignment.class);
            resourceAssignmentQuery.thereExistsResource().thereExistsContact().equalTo(contact);
            // Create resource assignment for contact if required
            if(activity.getAssignedResource(resourceAssignmentQuery).isEmpty()) {
                ResourceAssignment resourceAssignment = pm.newInstance(ResourceAssignment.class);
                resourceAssignment.setName(resource.getName());
                resourceAssignment.setDescription(
                    "#" + (activity.getActivityNumber() == null ? "" : activity.getActivityNumber()) + ": " + (activity.getName() == null ? "" : activity.getName())
                );
                resourceAssignment.setResource(resource);
                resourceAssignment.setResourceRole((short)0);
                resourceAssignment.setResourceOrder((short)0);
                resourceAssignment.setWorkDurationPercentage((short)100);
                activity.addAssignedResource(
                	this.getUidAsString(),
                	resourceAssignment
                );
            }
            activity.setAssignedTo(contact);
        }
    }

    /**
     * Update ical of given activity.
     * 
     * @param activity
     * @throws ServiceException
     */
    public void updateIcal(
        Activity activity
    ) throws ServiceException {
        List<String> messages = new ArrayList<String>();
        List<String> errors = new ArrayList<String>();
        List<String> report = new ArrayList<String>();
        String ical = ICalendar.getInstance().mergeIcal(
            activity,
            activity.getIcal(), 
            messages
        );
        // Add tag UID if either not exists or value is empty
        {
            String uidTag = "UID:" + activity.refGetPath().getLastSegment().toString();
    		if(ical.indexOf("UID:") < 0) {
    			if(ical.indexOf("BEGIN:VEVENT") > 0) {
        			int pos1 = ical.indexOf("BEGIN:VEVENT");
        			int pos2 = ical.indexOf("\n", pos1);
        			if(pos2 > pos1) {
        				ical = ical.substring(0, pos2 + 1) + uidTag + ical.substring(pos2);
        			}
    			} else if(ical.indexOf("BEGIN:VTODO") > 0) {
        			int pos1 = ical.indexOf("BEGIN:VTODO");
        			int pos2 = ical.indexOf("\n", pos1);
        			if(pos2 > pos1) {
        				ical = ical.substring(0, pos2 + 1) + uidTag + ical.substring(pos2);
        			}
    			}
    		} else if(ical.indexOf("UID:\n") > 0) {
    			int pos = ical.indexOf("UID:");
    			ical = ical.substring(0, pos) + uidTag + ical.substring(pos + 4);
    		}
        }
        byte[] item = null;
        try {
            item = ical.getBytes("UTF-8");
        } catch(Exception e) {
            item = ical.getBytes();    
        }
        ICalendar.getInstance().importItem(
            item, 
            activity, 
            (short)0, 
            errors, 
            report
        );
    }

    /**
     * Sum the work record quantities for the given work records.
     * 
     * @param workAndExpenseRecords
     * @param totalQuantities
     * @param quantityUoms
     */
    protected void calcTotalQuantity(
    	List<WorkAndExpenseRecord> workAndExpenseRecords,
        List<BigDecimal> totalQuantities,
        List<Uom> quantityUoms
    ) {
        for(WorkAndExpenseRecord workAndExpenseRecord: workAndExpenseRecords) {
        	boolean found = false;
        	int ii = 0;
        	for(Iterator<Uom> i = quantityUoms.iterator(); i.hasNext(); ii++) {
        		Uom quantityUom = i.next();
            	BigDecimal uomScaleFactor = Utils.getUomScaleFactor(
            		workAndExpenseRecord.getQuantityUom(),
            		quantityUom        		
            	);
            	if(uomScaleFactor.compareTo(BigDecimal.ONE) >= 0) {
                    totalQuantities.set(
                    	ii,
	                    totalQuantities.get(ii).add(
	                        workAndExpenseRecord.getQuantity().multiply(uomScaleFactor)
	                    )
	                );
                    found = true;
                    break;
            	} else if(uomScaleFactor.compareTo(BigDecimal.ZERO) > 0) {
            		totalQuantities.set(
            			ii,
            			totalQuantities.get(ii).divide(uomScaleFactor, RoundingMode.FLOOR)
            		);
            		quantityUoms.set(
            			ii,
            			workAndExpenseRecord.getQuantityUom()
            		);
            		totalQuantities.set(
            			ii,
            			totalQuantities.get(ii).add(
            				workAndExpenseRecord.getQuantity()
            			)
            		);
            		found = true;
            		break;
            	}            	
        	}
        	if(!found && workAndExpenseRecord.getQuantityUom() != null) {
        		totalQuantities.add(
        			workAndExpenseRecord.getQuantity()
        		);
        		quantityUoms.add(
        			workAndExpenseRecord.getQuantityUom()
        		);        		
        	}
        }
    }

    /**
     * Sum the work record quantities for the given activity.
     * 
     * @param activity
     * @param recordType
     * @param startAt
     * @param endAt
     * @param totalQuantities
     * @param quantityUoms
     * @throws ServiceException
     */
    public void calcTotalQuantity(
        Activity activity,
        short recordType,
        Date startAt,
        Date endAt,
        List<BigDecimal> totalQuantities,
        List<Uom> quantityUoms
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);    	
    	WorkAndExpenseRecordQuery workAndExpenseRecordQuery = (WorkAndExpenseRecordQuery)pm.newQuery(WorkAndExpenseRecord.class);
    	workAndExpenseRecordQuery.recordType().equalTo(recordType);
    	if(startAt != null) {
    		workAndExpenseRecordQuery.thereExistsEndedAt().greaterThanOrEqualTo(startAt);
    	}
    	if(endAt != null) {
    		workAndExpenseRecordQuery.thereExistsStartedAt().lessThanOrEqualTo(endAt);
    	}
    	List<WorkAndExpenseRecord> workAndExpenseRecords = activity.getWorkReportEntry(workAndExpenseRecordQuery);
        this.calcTotalQuantity(
        	workAndExpenseRecords, 
        	totalQuantities, 
        	quantityUoms
        );
    }
    
    /**
     * Sum the work record quantities for the given resource.
     * 
     * @param resource
     * @param recordType
     * @param startAt
     * @param endAt
     * @param totalQuantities
     * @param quantityUoms
     * @throws ServiceException
     */
    public void calcTotalQuantity(
        Resource resource,
        short recordType,
        Date startAt,
        Date endAt,
        List<BigDecimal> totalQuantities,
        List<Uom> quantityUoms
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(resource);    	
    	WorkAndExpenseRecordQuery workReportQuery = (WorkAndExpenseRecordQuery)pm.newQuery(WorkAndExpenseRecord.class);
    	workReportQuery.recordType().equalTo(recordType);
    	if(startAt != null) {
    		workReportQuery.thereExistsEndedAt().greaterThanOrEqualTo(startAt);
    	}
    	if(endAt != null) {
    		workReportQuery.thereExistsStartedAt().lessThanOrEqualTo(endAt);
    	}
    	List<WorkAndExpenseRecord> workAndExpenseRecords = resource.getWorkReportEntry(workReportQuery);
        this.calcTotalQuantity(
        	workAndExpenseRecords, 
        	totalQuantities, 
        	quantityUoms
        );
    }
    
    /**
     * Sum the work record quantities for the given activity group. 
     * 
     * @param activityGroup
     * @param recordType
     * @param startAt
     * @param endAt
     * @param totalQuantities
     * @param quantityUoms
     * @throws ServiceException
     */
    public void calcTotalQuantity(
        ActivityGroup activityGroup,
        short recordType,
        Date startAt,
        Date endAt,
        List<BigDecimal> totalQuantities,
        List<Uom> quantityUoms
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activityGroup);    	    	
    	WorkAndExpenseRecordQuery workReportQuery = (WorkAndExpenseRecordQuery)pm.newQuery(WorkAndExpenseRecord.class);
    	workReportQuery.recordType().equalTo(recordType);
    	if(startAt != null) {
    		workReportQuery.thereExistsEndedAt().greaterThanOrEqualTo(startAt);
    	}
    	if(endAt != null) {
    		workReportQuery.thereExistsStartedAt().lessThanOrEqualTo(endAt);
    	}    	
        List<WorkAndExpenseRecord> workAndExpenseRecords = activityGroup.getWorkReportEntry(workReportQuery);
        this.calcTotalQuantity(
        	workAndExpenseRecords, 
        	totalQuantities, 
        	quantityUoms
        );
    }
    
    /**
     * Sum the work record quantities for the given activity filter. 
     * 
     * @param activityFilter
     * @param recordType
     * @param startAt
     * @param endAt
     * @param totalQuantities
     * @param quantityUoms
     * @throws ServiceException
     */
    public void calcTotalQuantity(
        AbstractFilterActivity activityFilter,
        short recordType,
        Date startAt,
        Date endAt,        
        List<BigDecimal> totalQuantities,
        List<Uom> quantityUoms
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activityFilter);    	
    	ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
    	List<Activity> activities = activityFilter.getFilteredActivity(query);
        for(Activity activity: activities) {
        	WorkAndExpenseRecordQuery workReportQuery = (WorkAndExpenseRecordQuery)pm.newQuery(WorkAndExpenseRecord.class);
        	workReportQuery.recordType().equalTo(recordType);
        	if(startAt != null) {
        		workReportQuery.thereExistsEndedAt().greaterThanOrEqualTo(startAt);
        	}
        	if(endAt != null) {
        		workReportQuery.thereExistsStartedAt().lessThanOrEqualTo(endAt);
        	}        	
        	List<WorkAndExpenseRecord> workReportEntries = activity.getWorkReportEntry(workReportQuery);
            this.calcTotalQuantity(
            	workReportEntries, 
            	totalQuantities, 
            	quantityUoms
            );
        }
    }
        
    /**
     * Calculate main effort estimates.
     * 
     * @param activity
     * @return
     * @throws ServiceException
     */
    public Object[] calcMainEffortEstimate(
        Activity activity
    ) throws ServiceException {
        Collection<EffortEstimate> estimates = activity.getEffortEstimate();
        BigDecimal estimateEffortHours = BigDecimal.ZERO;
        BigDecimal effortEstimateMinutes = BigDecimal.ZERO;
        for(EffortEstimate estimate: estimates) {
            if(
                (estimate.isMain() != null) &&
                estimate.isMain().booleanValue()
            ) {
                estimateEffortHours = new BigDecimal(estimate.getEstimateEffortHours());
                effortEstimateMinutes = new BigDecimal(estimate.getEstimateEffortMinutes());    
                break;
            }
        }
        int hours = Math.abs(estimateEffortHours.intValue() + (effortEstimateMinutes.intValue() / 60));
        int minutes = Math.abs(effortEstimateMinutes.intValue() % 60);
        boolean isNegative = 
            estimateEffortHours.intValue() < 0 || 
            effortEstimateMinutes.intValue() < 0;
        return new Object[]{
        	hours,
        	minutes,
        	(isNegative ? "-" : "") + hours + ":" + (minutes < 10 ? "0" + minutes : "" + minutes) + "'"
        };
    }
    
    /**
     * Count activities of given activity filter.
     * 
     * @param activityFilter
     * @return
     * @throws ServiceException
     */
    public int countFilteredActivity(
        AbstractFilterActivity activityFilter
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activityFilter);
    	ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
    	QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);
    	List<Activity> activities = activityFilter.getFilteredActivity(query);
        return activities.size();
    }
    
    /**
     * Creates and adds an email recipient to the email activity.
     * 
     * @param pm
     * @param emailActivity
     * @param address
     * @param type
     */
    public void addEmailRecipient(
        PersistenceManager pm,
        EMail emailActivity,
        EMailAddress address,
        PartyType type
    ) {
        boolean isTxLocal = !pm.currentTransaction().isActive();
        if(isTxLocal) {
        	pm.currentTransaction().begin();
        }
        EMailRecipient recipient = pm.newInstance(EMailRecipient.class);
        emailActivity.addEmailRecipient(
            this.getUidAsString(),
            recipient
        );
        recipient.setParty(address);
        recipient.setPartyType(type.getValue());
        recipient.setEmailHint(address.getEmailAddress());
        // 'copy' the email's owning groups
        recipient.getOwningGroup().addAll(
            emailActivity.getOwningGroup()
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
    }

    /**
     * Adds an email recipient to the currently processed email activity if
     * the email message contains an email address which is contained in an
     * openCRX account. Email addresses for which no account can be found, are
     * recorded via a note attached to the email activity.
     * 
     * @param email
     * @param addresses
     * @param type
     * @throws ServiceException
     */
    public void mapAddressesToEMailRecipients(
        EMail email,
        String[] addresses,
        PartyType type
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(email);
    	String providerName = email.refGetPath().getSegment(2).toString();
    	String segmentName = email.refGetPath().getSegment(4).toString();
        if (addresses == null || addresses.length == 0) {
        	SysLog.trace("Message does not contain any recipient of type '" + type + "'");
        }
        Set<String> newAddresses = new HashSet<String>(Arrays.asList(addresses));
        newAddresses.remove("NO_ADDRESS_SPECIFIED");
        Collection<AbstractEMailRecipient> recipients = email.getEmailRecipient();
        for(AbstractEMailRecipient recipient: recipients) {
            if(recipient instanceof EMailRecipient) {
                EMailAddress address = (EMailAddress)((EMailRecipient)recipient).getParty();
                if((address != null) && (address.getEmailAddress() != null)) {
                    newAddresses.remove(address.getEmailAddress());
                }
            }
        }
        for(String address: newAddresses) {
            List<EMailAddress> emailAddresses = 
                Accounts.getInstance().lookupEmailAddress(
                    pm,
                    providerName,
                    segmentName,
                    address,
                    true, // exactCaseInsensitiveOnly
                    true // forceCreate
                );
            if(!emailAddresses.isEmpty()) {
                this.addEmailRecipient(
                    pm, 
                    email, 
                    emailAddresses.iterator().next(), 
                    type
                );
            }
        }
    }

    /**
     * Search email activity with the given external link, i.e. the given
     * message id.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param externalLink
     * @return
     */
    public List<Activity> lookupEmailActivity(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        String externalLink
    ) {
        if(externalLink == null) {
            return Collections.emptyList();
        } else {
            EMailQuery query = (EMailQuery)pm.newQuery(EMail.class);
            org.opencrx.kernel.activity1.jmi1.Segment activitySegment =
                this.getActivitySegment(
                    pm,
                    providerName,
                    segmentName
                );
            query.thereExistsExternalLink().equalTo(
                externalLink  
            );
            return activitySegment.getActivity(query);
        }
    }
        
    /**
     * Get activity segment.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.opencrx.kernel.activity1.jmi1.Segment getActivitySegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
            new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName, "segment", segmentName)
        );
    }

    /**
     * Find resources for given contact.
     * 
     * @param contact
     * @return
     * @throws ServiceException
     */
    public List<Resource> findResources(
    	Contact contact
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contact);
    	String providerName = contact.refGetPath().getSegment(2).toString();
    	String segmentName = contact.refGetPath().getSegment(4).toString();    	
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(pm, providerName, segmentName);
    	ResourceQuery resourceQuery = (ResourceQuery)pm.newQuery(Resource.class);
    	resourceQuery.thereExistsContact().equalTo(contact);
    	resourceQuery.forAllDisabled().isFalse();
    	List<Resource> resources = activitySegment.getResource(resourceQuery);
    	if(resources.isEmpty()) {
    		// Disabled resources in case there are no active
        	resourceQuery = (ResourceQuery)pm.newQuery(Resource.class);
        	resourceQuery.thereExistsContact().equalTo(contact);
        	resources = activitySegment.getResource(resourceQuery);
    	}
    	return resources;    	
    }

    /**
     * Formats a text containing all the addresses of the different types
     * (i.e., TO, CC, BCC) to be attached to the email activity by a note
     * indicating whether an account containing the email address could be
     * found.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param from
     * @param to
     * @param cc
     * @param bcc
     * @return
     * @throws ServiceException
     */
    public String getRecipientsAsNoteText(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        String[] from,
        String[] to,
        String[] cc,
        String[] bcc
    ) throws ServiceException {
        StringBuffer text = new StringBuffer();  

        // add 'FROM's to the note
        String addresses[] = from;
        for (int i = 0; i < addresses.length; i++) {
        	if(!"NO_ADDRESS_SPECIFIED".equalsIgnoreCase(addresses[i])) {
	            List<EMailAddress> emailAddresses = 
	                Accounts.getInstance().lookupEmailAddress(
	                    pm,
	                    providerName,
	                    segmentName,
	                    addresses[i]
	                );
	            text.append(
	            	"FROM: " + 
	            	addresses[i] + 
	            	" [" + 
	            	((emailAddresses == null || emailAddresses.size() == 0) ? "UNMATCHED" : "MATCHED") + "]\n"
	            );
        	}
        }
  
        // add 'TO's to the note
        addresses = to;
        for (int i = 0; i < addresses.length; i++) {
        	if(!"NO_ADDRESS_SPECIFIED".equalsIgnoreCase(addresses[i])) {
	            List<EMailAddress> emailAddresses = 
	                Accounts.getInstance().lookupEmailAddress(
	                    pm,
	                    providerName,
	                    segmentName,
	                    addresses[i]
	                );
	            text.append(
	            	"TO: " + 
	            	addresses[i] + 
	            	" [" + 
	            	((emailAddresses == null || emailAddresses.size() == 0) ? "UNMATCHED" : "MATCHED") + "]\n"
	            );
        	}
        }
  
        // add 'CC's to the note
        addresses = cc;
        for (int i = 0; i < addresses.length; i++) {
        	if(!"NO_ADDRESS_SPECIFIED".equalsIgnoreCase(addresses[i])) {
	            List<EMailAddress> emailAddresses = 
	                Accounts.getInstance().lookupEmailAddress(
	                    pm,
	                    providerName,
	                    segmentName,
	                    addresses[i]
	                );
	            text.append(
	            	"CC: " + 
	            	addresses[i] + 
	            	" [" + 
	            	((emailAddresses == null || emailAddresses.size() == 0) ? "UNMATCHED" : "MATCHED") + "]\n"
	            );
        	}
        }
  
        // add 'BCC's to the note
        addresses = bcc;
        for (int i = 0; i < addresses.length; i++) {
        	if(!"NO_ADDRESS_SPECIFIED".equalsIgnoreCase(addresses[i])) {
	            List<EMailAddress> emailAddresses = 
	                Accounts.getInstance().lookupEmailAddress(
	                    pm,
	                    providerName,
	                    segmentName,
	                    addresses[i]
	                );
	            text.append(
	            	"BCC: " + 
	            	addresses[i] + 
	            	" [" + 
	            	((emailAddresses == null || emailAddresses.size() == 0) ? "UNMATCHED" : "MATCHED") + "]\n"
	            );
        	}
        }
        return text.toString();
    }

    /**
     * Adds a note to the currently processed email activity.
     * 
     * @param pm
     * @param emailActivity
     * @param title
     * @param content
     */
    public void addNote(
        PersistenceManager pm,
        EMail emailActivity,
        String title,
        String content
    ) {
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	if(isTxLocal) {
    		pm.currentTransaction().begin();
    	}
        Note note = pm.newInstance(Note.class);
        emailActivity.addNote(
            this.getUidAsString(),
            note
        );
        note.setTitle(title);
        note.setText(content);
        // 'copy' the email's owning groups
        note.getOwningGroup().addAll(
            emailActivity.getOwningGroup()
        );
        if(isTxLocal) {
        	pm.currentTransaction().commit();
        }
    }
    
    /**
     * Extract the priority from the email message. Note that if no header
     * element is found this indicates a "normal" priority. Note that rfc822
     * does not define a standard header field for priority. The name of the
     * "priority" header field depends on your mail client used. "Importance"
     * with values high, normal and low "Priority" with values Urgent and
     * Non-Urgent "X-Priority" with values 1=high and 5=low These values are
     * mapped to:
     * <UL>
     * <LI>ACTIVITY_PRIORITY_LOW,
     * <LI>ACTIVITY_PRIORITY_NORMAL and
     * <LI>ACTIVITY_PRIORITY_HIGH
     * </UL>
     * respectively.
     * 
     * @return the subject of the message
     */
    public short getMessagePriority(
        Message message
    ) throws MessagingException {
        String priority = "normal";
        short priorityAsShort = Priority.NORMAL.getValue();
        String[] values = message.getHeader("Importance");
        if (values != null && values.length > 0) {
            priority = values[0];
        }
        values = message.getHeader("X-Priority");
        if (values != null && values.length > 0) {
            priority = values[0];
        }
        values = message.getHeader("Priority");
        if (values != null && values.length > 0) {
            priority = values[0];
        }
        if (priority.equalsIgnoreCase("normal") || priority.equalsIgnoreCase("3")) {
            priorityAsShort = Priority.NORMAL.getValue();
        } else if (priority.equalsIgnoreCase("high")
                || priority.equalsIgnoreCase("1")
                || priority.equalsIgnoreCase("Urgent")) {
            priorityAsShort = Priority.HIGH.getValue();
        } else if (priority.equalsIgnoreCase("low")
                || priority.equalsIgnoreCase("5")
                || priority.equalsIgnoreCase("Non-Urgent")) {
            priorityAsShort = Priority.LOW.getValue();
        }
        return priorityAsShort;
    }
    
    /**
     * Get first text part of mime content.
     * 
     * @param content
     * @return
     * @throws MessagingException
     * @throws IOException
     */
    private static Part getFirstTextPart(
        Object content
    ) throws MessagingException, IOException {
        if(content instanceof MimeMultipart) {
            MimeMultipart multipartMessage = (MimeMultipart)content;
            // Try to find a part with mimeType text/plain
            for(int i = 0; i < multipartMessage.getCount(); i++) {
                Part part = multipartMessage.getBodyPart(i);
                if(part.isMimeType("text/plain")) {
                    return part;
                }
                else if(part.getContent() instanceof MimeMultipart) {
                    return Activities.getFirstTextPart(part.getContent());
                }
            }
            return multipartMessage.getCount() > 0 ?
                multipartMessage.getBodyPart(0) :
                null;
        } else if(content instanceof Part) {
            Object c = ((Part)content).getContent();
            return c instanceof MimeMultipart ?
            	Activities.getFirstTextPart(c) :
                (Part)content;
        } else {
            return null;
        }
    }
    
    /**
     * Get message body of mime part.
     * 
     * @param messagePart
     * @return
     * @throws IOException
     * @throws MessagingException
     */
    public String getMessageBody(
        MimePart messagePart
    ) throws IOException, MessagingException {
        Part part = Activities.getFirstTextPart(messagePart);
        if(part == null) return null;
        Object content = part.getContent();
        if(content instanceof String) {
            return (String)content;
        } else if (content instanceof InputStream) {
            if(part.isMimeType("text/plain") || part.isMimeType("text/html")) {
                BufferedReader in = new BufferedReader(new InputStreamReader(
                    part.getInputStream())
                );
                StringBuffer body = new StringBuffer();
                while (in.ready()) {
                    body.append(in.readLine());
                    if(in.ready()) {
                        body.append(System.getProperty("line.separator", "\n"));
                    }
                }
                return body.toString();
            } else {
                return "";
            }
        }
        return null;
    }

    /**
     * Get name of original zipped mime message in format 1.
     * 
     * @return
     */
    protected String getOriginalMessageName1(
    ) {
        return ORIGINAL_MESSAGE_MEDIA_NAME + ".eml.zip";    	
    }
    
    /**
     * Get name of original zipped mime message attached to activity in format 2.
     * 
     * @param email
     * @return
     */
    protected String getOriginalMessageName2(
    	Activity email
    ) {
        return email.getActivityNumber().trim() + ".eml.zip";    	
    }
    
    /**
     * Maps email activity to message. If email activity has a media attachment
     * which contains the original MimeMessage the stream of this message is
     * returned in addition.
     * 
     * @param email
     * @param message
     * @return
     * @throws MessagingException
     */
    public InputStream mapMessageContent(
        EMail email,
        Message message
    ) throws MessagingException {
    	String originalMessageMediaName1 = this.getOriginalMessageName1();
        String originalMessageMediaName2 = this.getOriginalMessageName2(email);
        InputStream originalMessageStream = null;
        String text = email.getMessageBody();
        text = text == null ? "" : text;
        Collection<org.opencrx.kernel.generic.jmi1.Media> medias = email.getMedia();
        if(medias.isEmpty()) {
	        if(
	        	text.regionMatches(true, 0, "<!DOCTYPE html", 0, 14) ||
	        	text.regionMatches(true, 0, "<html>", 0, 6)
	        ) {
	        	message.setContent(text, "text/html; charset=utf-8");        	
	        } else {
	            message.setText(text);
	        }
        } else {
	        MimeMultipart messageMultipart = new MimeMultipart("mixed");
	        // Body
	        if(
	        	text.regionMatches(true, 0, "<!DOCTYPE html", 0, 14) || 
	        	text.regionMatches(true, 0, "<html>", 0, 6)
	        ) {
		        MimeBodyPart htmlPart = new MimeBodyPart();
		        htmlPart.setContent(text, "text/html; charset=UTF-8");
		        messageMultipart.addBodyPart(htmlPart);
	        } else {
		        MimeBodyPart textPart = new MimeBodyPart();
		        textPart.setText(text);
		        messageMultipart.addBodyPart(textPart);
	        }
	        // Attachments
	        for(Media media: medias) {
	            if(media.getContentName() != null) {
	                try {
	                    QuotaByteArrayOutputStream mediaContent = new QuotaByteArrayOutputStream(Activities.class.getName());
	                    try {
	                        InputStream is = media.getContent().getContent();
	                        BinaryLargeObjects.streamCopy(is, 0L, mediaContent);
	                    } catch(Exception e) {
	                    	SysLog.warning("Unable to get media content (see detail for more info)", e.getMessage());
	                    	SysLog.info(e.getMessage(), e.getCause());
	                    }
	                    mediaContent.close();
	                    // Test whether media is zipped original mail. 
	                    // If yes return as original message
	                    if(
	                    	originalMessageMediaName1.equals(media.getContentName()) || 
	                    	originalMessageMediaName2.equals(media.getContentName())
	                    ) {
	                        ZipInputStream zippedMessageStream = new ZipInputStream(mediaContent.toInputStream());
	                        zippedMessageStream.getNextEntry();
	                        originalMessageStream = zippedMessageStream;
	                    }
	                    InternetHeaders headers = new InternetHeaders();
	                    headers.addHeader("Content-Type", media.getContentMimeType() + "; name=\"" + MimeUtility.encodeText(media.getContentName()) + "\"");
	                    headers.addHeader("Content-Disposition", "attachment");
	                    headers.addHeader("Content-Transfer-Encoding", "base64");
	                    MimeBodyPart attachmentPart = new MimeBodyPart(                        
	                        headers,
	                        Base64.encode(mediaContent.getBuffer(), 0, mediaContent.size()).getBytes("US-ASCII")
	                    );
	                    messageMultipart.addBodyPart(attachmentPart);
	                } catch(Exception e) {
	                    new ServiceException(e).log();
	                }
	            }
	        }
	        message.setContent(messageMultipart);
        }
        return originalMessageStream;
    }

    /**
     * Get internet address of given address.
     * 
     * @param address
     * @param gateway
     * @return
     */
    public String getInternetAddress(
        AccountAddress address,
        BasicObject gateway
    ) {
        if(address instanceof EMailAddress) {
            return ((EMailAddress)address).getEmailAddress();
        } else if(address instanceof PhoneNumber) {
            String phoneNumber = ((PhoneNumber)address).getPhoneNumberFull();
            StringBuilder inetAddress = new StringBuilder();
            for(int i = 0; i < phoneNumber.length(); i++) {
                char c = phoneNumber.charAt(i);
                if((c == '+') && (inetAddress.length() == 0)) {
                    inetAddress.append("_");
                }
                else if(Character.isLetterOrDigit(c)) {
                    inetAddress.append(Character.toUpperCase(c));
                }
            }
            String gatewayName = null;
            if(gateway instanceof EMailAddressable) {
            	gatewayName = ((EMailAddressable)gateway).getEmailAddress();
            } else if(gateway instanceof EMailAccount) {
            	gatewayName = ((EMailAccount)gateway).getName();
            }
            if((gatewayName != null) && (gatewayName.indexOf("@") > 0)) {
                inetAddress.append(gatewayName.substring(gatewayName.indexOf("@")));
            }
            return inetAddress.toString();
        } else {
            return null;
        }
    }
    
    /**
     * Map message recipients to given email activity.
     * 
     * @param emailActivity
     * @return
     * @throws AddressException
     * @throws MessagingException
     */
    public List<Address> mapMessageRecipients(
        EMail emailActivity
    ) throws AddressException, MessagingException {
    	return this.mapMessageRecipients(emailActivity, null);
    }
    
    /**
     * Map message recipients to email activity.
     * 
     * @param email
     * @param message
     * @return
     * @throws AddressException
     * @throws MessagingException
     */
    public List<Address> mapMessageRecipients(
        EMail email,
        Message message            
    ) throws AddressException, MessagingException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(email);
        List<Address> recipients = new ArrayList<Address>();
        AccountAddress sender = null;
        try {
            sender = email.getSender();
        } catch(Exception e) {
            ServiceException e0 = new ServiceException(e);
            SysLog.detail(e0.getMessage(), e0.getCause());
        }
        if(sender != null) {
            String inetAddress = this.getInternetAddress(sender, email.getGateway());
            if(inetAddress != null) {
            	if(message != null) {
	                message.setFrom(
	                    new InternetAddress(inetAddress)
	                );
            	}
            }
        }
        Collection<AbstractEMailRecipient> emailRecipients = email.getEmailRecipient();
        for(AbstractEMailRecipient recipient: emailRecipients) {
            RecipientType recipientType = null;
            if(
            	recipient.getPartyType() == PartyType.REQUIRED.getValue() ||
            	recipient.getPartyType() == PartyType.EMAIL_TO.getValue()
            ) {
                recipientType = RecipientType.TO;
            } else if(
            	recipient.getPartyType() == PartyType.OPTIONAL.getValue() ||
            	recipient.getPartyType() == PartyType.EMAIL_CC.getValue()
            ) {
                recipientType = RecipientType.CC;
            } else if(recipient.getPartyType() == PartyType.EMAIL_BCC.getValue()) {
                recipientType = RecipientType.BCC;
            }
            if(recipientType != null) {
                if(recipient instanceof EMailRecipient) {
                	AccountAddress address = null;
                	try {
                		address = ((EMailRecipient)recipient).getParty();
                	} catch(Exception e) {
                		new ServiceException(e).log();
                	}
                	if((address != null) && !Boolean.TRUE.equals(address.isDisabled())) {
	                    String inetAddress = null;
	                    try {
	                        inetAddress = this.getInternetAddress(address, email.getGateway());
	                    } catch(Exception e) {
	                        ServiceException e0 = new ServiceException(e);
	                        SysLog.detail(e0.getMessage(), e0.getCause());
	                    }
	                    if(inetAddress != null) {
	                        try {
	                            Address to = new InternetAddress(inetAddress);
	                            recipients.add(to);
	                            if(message != null) {
		                            message.addRecipient(
		                                recipientType,
		                                to
		                            );
	                            }
	                        } catch(Exception e) {
	                        	SysLog.warning("Invalid recipient", Arrays.asList(email, inetAddress));
	                        }
	                    }
                	}
                } else if(recipient instanceof EMailRecipientGroup) {
                    EMailRecipientGroup recipientGroup = (EMailRecipientGroup)recipient;
                	AddressGroup addressGroup = null;
                	try {
                		addressGroup = recipientGroup.getParty();
                	} catch(Exception e) {
                		new ServiceException(e).log();
                	}                    
                    AddressGroupMemberQuery addressGroupMemberQuery = (AddressGroupMemberQuery)pm.newQuery(AddressGroupMember.class);
                    addressGroupMemberQuery.forAllDisabled().isFalse();
                    if((addressGroup != null) && !Boolean.TRUE.equals(addressGroup.isDisabled())) {
	                    Collection<AddressGroupMember> members = addressGroup.getMember(addressGroupMemberQuery);
	                    for(AddressGroupMember member: members) {
	                    	AccountAddress address = null;
	                    	try {
	                    		address = member.getAddress();
	                    	} catch(Exception e) {}                        		
	                        if(
	                        	(address != null) &&
	                        	!Boolean.TRUE.equals(address.isDisabled())
	                        ) {
	                            String inetAddress = this.getInternetAddress(address, email.getGateway());
	                            if(inetAddress != null) {
	                                try {
	                                    Address to = new InternetAddress(inetAddress);
	                                    recipients.add(to);
	                                    if(message != null) {
		                                    message.addRecipient(
		                                        recipientType,
		                                        to
		                                    );
	                                    }
	                                } catch(Exception e) {
	                                	SysLog.warning("Invalid recipient", Arrays.asList(email, inetAddress));
	                                }
	                            }
	                        }
	                    }
                    }
                }
            }
        }
        return recipients;
    }

    /**
     * Maps email to mime message. Either returns message or an input stream
     * which contains a mime message.
     * 
     * @param email
     * @param message
     * @return
     * @throws MessagingException
     */
    public Object mapToMessage(
        EMail email,
        Message message
    ) throws MessagingException {
        try {
            InputStream messageStream = this.mapMessageContent(
                email, 
                message
            );
            if(messageStream != null) {
                return messageStream;
            }
        } catch(Exception e) {
            new ServiceException(e).log();
        }
        try {
        	this.mapMessageRecipients(
                email, 
                message
            );
        } catch(Exception e) {
            new ServiceException(e).log();
        }
        message.setSubject(
        	email.getMessageSubject() != null ?
        		email.getMessageSubject() :
        			email.getName()
        );
        message.setHeader(
            "Date", 
            DATEFORMAT_MESSAGE_HEADER.format(
                email.getSendDate() != null ? 
                    email.getSendDate() : 
                    	email.getCreatedAt()
            )
        );
        return message;
    }
        
    /**
     * Find activity creator according to given activity class.
     * 
     * @param activityCreators
     * @param activityClass
     * @return
     */
    public ActivityCreator findActivityCreator(
        Collection<ActivityCreator> activityCreators,
        short activityClass
    ) {
        for(ActivityCreator creator: activityCreators) {
            if(
                (creator.getActivityType() != null) && 
                (creator.getActivityType().getActivityClass() == activityClass)
            ) {
                return creator;
            }
        }
        return null;
    }

    /**
     * Callback for activity removal.
     * 
     * @param activity
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeActivity(
        Activity activity,
        boolean preDelete
    ) throws ServiceException {
    }

    /**
     * Map email addresses to internet addresses.
     * 
     * @param addresses
     * @return
     * @throws AddressException
     */
    public String[] getInternetAddresses(
        javax.mail.Address[] addresses
    ) throws AddressException {
        String internetAddresses[] = null;
        if (addresses != null && addresses.length > 0) {
            internetAddresses = new String[addresses.length];
            for (int i = 0; i < addresses.length; i++) {
                if (addresses[0] instanceof InternetAddress) {
                    internetAddresses[i] = ((InternetAddress)addresses[i]).getAddress();
                } else {
                    InternetAddress temp = new InternetAddress(addresses[i].toString());
                    internetAddresses[i] = temp.getAddress();
                }
            }
        } else {
            internetAddresses = new String[]{UNSPECIFIED_ADDRESS};
        }
        return internetAddresses;
    }
    
    /**
     * Add attachments to given email activity.
     * 
     * @param mimeMessage
     * @param email
     * @throws IOException
     * @throws MessagingException
     * @throws ServiceException
     */
    public void addAttachments(
        MimeMessage mimeMessage,
        EMail email
    ) throws IOException, MessagingException, ServiceException {
        if(mimeMessage.getContent() instanceof MimeMultipart) {
            MimeMultipart multipart = (MimeMultipart)mimeMessage.getContent();
            for(int i = 1; i < multipart.getCount(); i++) {
                MimeBodyPart part = (MimeBodyPart)multipart.getBodyPart(i);
                String[] contentType = MimeUtils.parseContentType(part.getContentType());
                if(contentType[1] == null) contentType[1] = part.getContentID();
            	PersistenceManager pm = JDOHelper.getPersistenceManager(email);
            	boolean isTxLocal = !pm.currentTransaction().isActive();
            	if(isTxLocal) {
            		pm.currentTransaction().begin();
            	}
                Base.getInstance().createOrUpdateMedia(
                    email,
                    contentType[0],
                    contentType[1],
                    part.getInputStream()
                );
                if(isTxLocal) {
                	pm.currentTransaction().commit();
                }
            }
        }
    }

    /**
     * Import mime message and map to given email activity.
     * 
     * @param email
     * @param mimeMessage
     * @param isNew
     * @throws ServiceException
     * @throws MessagingException
     * @throws IOException
     * @throws ParseException
     */
    public void importMimeMessage(
    	EMail email,
    	MimeMessage mimeMessage,
    	boolean isNew
    ) throws ServiceException, MessagingException, IOException, ParseException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(email);
    	String providerName = email.refGetPath().getSegment(2).toString();
    	String segmentName = email.refGetPath().getSegment(4).toString();
        MailDateFormat mailDateFormat = new MailDateFormat();
        javax.mail.Address[] addressesFrom = mimeMessage.getFrom();
        javax.mail.Address[] addressesTo = mimeMessage.getRecipients(Message.RecipientType.TO);
        javax.mail.Address[] addressesCc = mimeMessage.getRecipients(Message.RecipientType.CC);
        javax.mail.Address[] addressesBcc = mimeMessage.getRecipients(Message.RecipientType.BCC);        
    	if(isNew) {
            // Update activity step 1
            String subject = mimeMessage.getSubject();
            email.setMessageSubject(
                subject == null ? "" : subject
            );
            email.getExternalLink().clear();
            if(mimeMessage.getMessageID() != null) {
            	String messageId = mimeMessage.getMessageID(); 
                email.getExternalLink().add(messageId);
                // Update ICALs UID
                email.getExternalLink().add(
                    ICalendar.ICAL_SCHEMA + messageId
                );
                if(email.getIcal() != null) {
                	String ical = email.getIcal();
                	int pos1 = ical.indexOf("UID:");
                	int pos2 = ical.indexOf("\n", pos1);
                	if(pos2 > pos1) {
                		ical = 
                			ical.substring(0, pos1) + 
                			"UID:" + messageId +
                			ical.substring(pos2);
                		email.setIcal(ical);
                	}
                }
            }
            // Update activity step 2
            // Append original email as media attachment
            String mimeMessageName = this.getOriginalMessageName1();
            QuotaByteArrayOutputStream mimeMessageBytes = new QuotaByteArrayOutputStream(Activities.class.getName());
            ZipOutputStream mimeMessageZipped = new ZipOutputStream(mimeMessageBytes);
            mimeMessageZipped.putNextEntry(
                new ZipEntry(
                	mimeMessageName.endsWith(".zip") ? 
                		mimeMessageName.substring(0, mimeMessageName.lastIndexOf(".zip")) : 
                			mimeMessageName
                )
            );
            mimeMessage.writeTo(mimeMessageZipped);
            mimeMessageZipped.close();
            Base.getInstance().createOrUpdateMedia(
                email, 
                "application/zip", 
                mimeMessageName, 
                mimeMessageBytes.toInputStream()
            );
            // Update activity step 3
            String body = this.getMessageBody(mimeMessage);
            if(body != null && body.indexOf("\0") > 0) {
            	body = body.replace("\0", " ");
            	SysLog.warning("Message contains body with invalid characters", email.getActivityNumber());
            }
            email.setMessageBody(
                body == null ? "" : body
            );
            String[] date = mimeMessage.getHeader("Date");
            if(date != null && date.length > 0) {
                email.setSendDate(
                    mailDateFormat.parse(date[0])
                );
            }
            // Add originator and recipients to a note
            this.addNote(
                pm,
                email,
                "Recipients",
                this.getRecipientsAsNoteText(
                    pm,
                    providerName,
                    segmentName,
                    this.getInternetAddresses(addressesFrom),
                    this.getInternetAddresses(addressesTo),
                    this.getInternetAddresses(addressesCc),
                    this.getInternetAddresses(addressesBcc)
                )
            );                  
            // Add headers as Note
            Activities.getInstance().addNote(
                pm,
                email,
                "Message-Header",
                MimeUtils.getHeadersAsRFC822(
                     mimeMessage, 
                     null
                )
            );
            this.addAttachments(
                mimeMessage, 
                email
            );
    	}
        // linkToAndFollowUp()
    	String subject = email.getMessageSubject();
        if(subject.indexOf("#") >= 0) {
        	StringTokenizer tokenizer = new StringTokenizer(subject, " .,()[]", false);
        	while(tokenizer.hasMoreTokens()) {
        		String split = tokenizer.nextToken();
        		if(split.startsWith("#")) {
        			String activityNumber = split.substring(1);
        			// Try to match activity if we have a potential activity number
        			if(activityNumber != null && activityNumber.length() >= 6) {
        				org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(pm, providerName, segmentName);
        				ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
        				activityQuery.thereExistsActivityNumber().like(".*" + activityNumber + ".*");
        				List<Activity> activities = activitySegment.getActivity(activityQuery);
        				for(Activity activity: activities) {
        					ActivityLinkToQuery linkToQuery = (ActivityLinkToQuery)pm.newQuery(ActivityLinkTo.class);
        					linkToQuery.thereExistsLinkTo().equalTo(email);
        					List<ActivityLinkTo> links = activity.getActivityLinkTo(linkToQuery);
        					// Only add link if not already there
        					if(links.isEmpty()) {
	        					try {
	        						ActivityProcess activityProcess = activity.getActivityType().getControlledBy();
	        						ActivityProcessTransitionQuery processTransitionQuery = (ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
	        						processTransitionQuery.thereExistsPrevState().equalTo(activity.getProcessState());
	        						processTransitionQuery.orderByNewPercentComplete().ascending();
	        						List<ActivityProcessTransition> processTransitions = activityProcess.getTransition(processTransitionQuery);
	        						if(!processTransitions.isEmpty()) {
		            					this.linkToAndFollowUp(
		            						activity, 
		            						processTransitions.iterator().next(), 
		            						email,
		            						null // parentProcessInstance
		            					);
	        						}
	        					} catch(Exception e) {
	        						// Log and ignore
	        						new ServiceException(e).log();
	        					}
        					}
        				}
        			}
        		}
        	}
        }
        // Add FROM as sender
        List<EMailAddress> addresses = 
            Accounts.getInstance().lookupEmailAddress(
                pm,
                providerName,
                segmentName,
                this.getInternetAddresses(addressesFrom)[0],
                true, // exactCaseInsensitiveOnly
                true // forceCreate
            );
        EMailAddress from = null;
        if(!addresses.isEmpty()) {
            from = addresses.iterator().next();
            email.setSender(from);
            EMailRecipientQuery recipientQuery = (EMailRecipientQuery)pm.newQuery(EMailRecipient.class);
            recipientQuery.partyType().equalTo(PartyType.EMAIL_FROM.getValue());
            List<EMailRecipient> recipients = email.getEmailRecipient(recipientQuery);
            if(recipients.isEmpty()) {
                this.addEmailRecipient(
                    pm, 
                    email, 
                    from, 
                    PartyType.EMAIL_FROM
                );
            } else {
            	EMailRecipient recipient = recipients.iterator().next();
            	recipient.setParty(from);
            	recipient.setEmailHint(from.getEmailAddress());
            }
        }
        this.mapAddressesToEMailRecipients(
            email,
            this.getInternetAddresses(addressesTo),
            PartyType.EMAIL_TO
        );
        this.mapAddressesToEMailRecipients(
            email,
            this.getInternetAddresses(addressesCc),
            PartyType.EMAIL_CC
        );
    }
    
    /**
     * Import mime message and map to email activities.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param mimeMessage
     * @param emailCreator
     * @return
     * @throws ServiceException
     * @throws MessagingException
     * @throws IOException
     * @throws ParseException
     */
    public List<EMail> importMimeMessage(
    	PersistenceManager pm,
    	String providerName,
    	String segmentName,
    	MimeMessage mimeMessage,
    	ActivityCreator emailCreator
    ) throws ServiceException, MessagingException, IOException, ParseException {
    	if(emailCreator != null) {
    		// Make sure that emailCreator is retrieved from pm
    		emailCreator = (ActivityCreator)pm.getObjectById(emailCreator.refGetPath());
    	}
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment(pm, providerName, segmentName);
    	List<MimeMessage> messages = new ArrayList<MimeMessage>();
    	// mimeMessage is a wrapper message which contains messages to be 
    	// imported. The wrapper message is mapped to an activity. Create or lookup. 
    	String activityNumber = null;
    	if(mimeMessage.getSubject() != null && mimeMessage.getSubject().startsWith("> ")) {
    		String subject = mimeMessage.getSubject().substring(2);
    		String specifier = subject;
    		if(subject.indexOf("  ") > 0) {
    			int pos = subject.indexOf("  ");
    			specifier = subject.substring(0, pos);
    			subject = subject.substring(pos + 2);
    		} else {
    			subject = null;
    		}
    		// Subject line is of the form
    		// > @<email creator name> [#<activity creator name or activity#>]  <subject>
    		// The activity creator name / activity# is optional. If specified an activity is created
    		// and the imported E-Mails are linked to this activity
    		// Derive email creator from subject line
    		if(emailCreator == null) {
    			int pos = specifier.indexOf("@");
    			if(pos >= 0) {    				
    				String creatorName = specifier.substring(pos + 1);
    				pos = creatorName.indexOf("  #");
    				if(pos > 0) {
    					creatorName = creatorName.substring(0, pos);
    				}
    				creatorName = creatorName.trim();    				
    				emailCreator = this.findActivityCreator(creatorName, activitySegment);
    			} else {
    				SysLog.warning("E-Mail creator not specified", specifier);
    			}
    		}
    		// Default Email creator
    		if(emailCreator == null) {
    			SysLog.detail("E-Mail creator not found. Using default creator", Activities.ACTIVITY_CREATOR_NAME_EMAILS);
    			emailCreator = this.findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_EMAILS, activitySegment);
    		}
    		Activity activity = null;
    		// Derive activity / activity creator from subject line
    		if(specifier.indexOf("#") >= 0) {
    			int pos = specifier.indexOf("#");
    			String activityQualifier = specifier.substring(pos + 1);
    			pos = activityQualifier.indexOf(" @");
    			if(pos > 0) {
    				activityQualifier = activityQualifier.substring(0, pos);
    			}
    			activityQualifier = activityQualifier.trim();
    			// Try to find activity where the activity number matches the activity qualifier
    			if(activityQualifier.length() >= 6) {
	    			ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
	    			activityQuery.thereExistsActivityNumber().like(".*" + activityQualifier + ".*");
	    			List<Activity> activities = activitySegment.getActivity(activityQuery);
	    			if(!activities.isEmpty()) {
	    				activity = activities.iterator().next();
	    			}
    			}
    			// Try to find activity creator based on the activity qualifier
    			if(activity == null) {
    				ActivityCreator creator = this.findActivityCreator(activityQualifier, activitySegment);
    				if(creator == null) {
    					SysLog.warning("Activity creator not found. No activity will be created", activityQualifier);
    				} else {
    					if(subject == null || subject.length() <= 5) {
        					SysLog.warning("Subject line must have at least five characters", subject);    						
    					} else {
				            pm.currentTransaction().begin();
				            NewActivityParams newActivityParams = Structures.create(
				            	NewActivityParams.class,
				            	Datatypes.member(NewActivityParams.Member.detailedDescription, this.getMessageBody(mimeMessage)),
				            	Datatypes.member(NewActivityParams.Member.icalType, ICalendar.ICAL_TYPE_NA),
				            	Datatypes.member(NewActivityParams.Member.name, subject),
				            	Datatypes.member(NewActivityParams.Member.priority, this.getMessagePriority(mimeMessage))
				            );				            
				            NewActivityResult newActivityResult = creator.newActivity(
				                newActivityParams
				            );
				            pm.currentTransaction().commit();                  
				            activity = (Activity)pm.getObjectById(newActivityResult.getActivity().refGetPath());
    					}
					}
    			}    			
    		}
    		else {
    			SysLog.warning("Activity creator not specified. No activity will be created", specifier);
    		}
    		if(activity != null) {
    			activityNumber = activity.getActivityNumber();
    		}
    		// Get attachments as mime messages
            if(mimeMessage.getContent() instanceof MimeMultipart) {
                MimeMultipart multipart = (MimeMultipart)mimeMessage.getContent();
                for(int i = 1; i < multipart.getCount(); i++) {
                	// Ignore attachments which are not a mime message
                    try {
                        MimeBodyPart part = (MimeBodyPart)multipart.getBodyPart(i);
	                	MimeMessage message = new MimeUtils.MimeMessageImpl(part.getInputStream());
	                	messages.add(message);
                    } catch(Exception e) {}
                }
            }    	
    	} else {    		
    		messages.add(mimeMessage);
    		// Default Email creator
    		if(emailCreator == null) {
    			SysLog.detail("E-Mail creator not specified. Using default creator", Activities.ACTIVITY_CREATOR_NAME_EMAILS);
    			emailCreator = this.findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_EMAILS, activitySegment);
    		}
    	}
    	List<EMail> emails = new ArrayList<EMail>();
    	for(MimeMessage message: messages) {
	        List<Activity> activities = this.lookupEmailActivity(
	            pm,
	            providerName,
	            segmentName,
	            message.getMessageID()
	        );
	        EMail email = null;
	        String activityRefTag = activityNumber == null ? null : " [#" + activityNumber + "]";
	        // Create new E-Mail
	        if(activities.isEmpty()) {
	    		if(activityRefTag != null) {
	    			message.setSubject(
	    				message.getSubject() + activityRefTag
	    			);
	    		}
	            pm.currentTransaction().begin();
	            NewActivityParams newActivityParams = Structures.create(
	            	NewActivityParams.class,
	            	Datatypes.member(NewActivityParams.Member.name, message.getSubject()),
	            	Datatypes.member(NewActivityParams.Member.icalType, ICalendar.ICAL_TYPE_NA),
	            	Datatypes.member(NewActivityParams.Member.priority, this.getMessagePriority(message))
	            );
	            NewActivityResult newActivityResult = emailCreator.newActivity(
	                newActivityParams
	            );
	            email = (EMail)newActivityResult.getActivity();
		        this.importMimeMessage(
		        	email, 
		        	message, 
		        	true // isNew 
		        );
		        pm.currentTransaction().commit();
	        }
	        // Update existing E-Mail
	        else {
	            email = (EMail)activities.iterator().next();
		        pm.currentTransaction().begin();
	            if(activityRefTag != null && email.getName() != null && email.getName().indexOf(activityRefTag) < 0) {
            		email.setName(email.getName() + activityRefTag);
            		email.setMessageSubject(email.getMessageSubject() + activityRefTag);
	            }
		        pm.currentTransaction().commit();
		        pm.currentTransaction().begin();
		        this.importMimeMessage(
		        	email, 
		        	message, 
		        	false // isNew 
		        );
	            if(activityRefTag != null && email.getName() != null && email.getName().indexOf(activityRefTag) < 0) {
            		email.setName(email.getName() + activityRefTag);
            		email.setMessageSubject(email.getMessageSubject() + activityRefTag);
	            }
		        pm.currentTransaction().commit();
	        }
	        emails.add(email);
    	}
	    return emails;
    }

    /** 
     * Add recipients to given email activity.
     * 
     * @param email
     * @param sender
     * @param recipientTo
     * @param recipientCc
     * @param recipientBcc
     * @throws ServiceException
     */
    public void addEMailRecipients(
    	EMail email,
    	String sender,
    	List<String> recipientTo,
    	List<String> recipientCc,
    	List<String> recipientBcc
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(email);
    	String providerName = email.refGetPath().getSegment(2).toString();
    	String segmentName = email.refGetPath().getSegment(4).toString();
		// Sender
		if(sender != null) {
			List<EMailAddress> emailAddresses =
				Accounts.getInstance().lookupEmailAddress(
					pm,
					providerName,
					segmentName,
					sender
				);
			if(!emailAddresses.isEmpty()) {
				email.setSender(emailAddresses.iterator().next());
			}
		}
		// Recipients TO  
		for(String recipientAddress: recipientTo) {
			List<EMailAddress> emailAddresses =
				Accounts.getInstance().lookupEmailAddress(
					pm,
					providerName,
					segmentName,
					recipientAddress
				);
			if(!emailAddresses.isEmpty()) {
				EMailRecipient recipient = pm.newInstance(EMailRecipient.class);
				recipient.setParty(emailAddresses.iterator().next());
				recipient.setPartyType(PartyType.EMAIL_TO.getValue());
				email.addEmailRecipient(
					this.getUidAsString(),
					recipient
				);
			}
		}
		// Recipients CC
		for(String recipientAddress: recipientCc) {
			List<EMailAddress> emailAddresses = 
				Accounts.getInstance().lookupEmailAddress(
					pm, 
					providerName,
					segmentName,
					recipientAddress 
				);
			if(!emailAddresses.isEmpty()) {
				EMailRecipient recipient = pm.newInstance(EMailRecipient.class);
				recipient.setParty(emailAddresses.iterator().next());
				recipient.setPartyType(PartyType.EMAIL_CC.getValue());
				email.addEmailRecipient(
					this.getUidAsString(),
					recipient
				);
			}
		}
		// Recipients BCC
		for(String recipientAddress: recipientBcc) {
			List<EMailAddress> emailAddresses = 
				Accounts.getInstance().lookupEmailAddress(
					pm, 
					providerName,
					segmentName,
					recipientAddress 
				);        	
			if(!emailAddresses.isEmpty()) {
				EMailRecipient recipient = pm.newInstance(EMailRecipient.class);
				recipient.setParty(emailAddresses.iterator().next());
				recipient.setPartyType(PartyType.EMAIL_BCC.getValue());
				email.addEmailRecipient(
					this.getUidAsString(),
					recipient
				);
			}
		}
    }

    /**
     * Send given e-mail, i.e. perform "Send E-Mail" transition given activity.
     * 
     * @param email
     * @throws ServiceException
     */
    public void sendEMail(
    	EMail email
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(email);
		// Assign
		ActivityProcessTransition transition = this.findActivityProcessTransition(
			email, 
			"Assign"
		);
		pm.currentTransaction().begin();
		ActivityDoFollowUpParams doFollowUpParams = Structures.create(
			ActivityDoFollowUpParams.class, 
			Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, null),
			Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, null),
			Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, "Accepted"),
			Datatypes.member(ActivityDoFollowUpParams.Member.transition, transition)
		);		
		email.doFollowUp(doFollowUpParams);
		pm.currentTransaction().commit();
		// Send as mail
		transition = this.findActivityProcessTransition(
			email, 
			"Send as Mail"
		);
		if(transition == null) {
			transition = this.findActivityProcessTransition(
				email, 
				"Send as mail"
			);         					
		}
		pm.currentTransaction().begin();
		doFollowUpParams = Structures.create(
			ActivityDoFollowUpParams.class, 
			Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, null),
			Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, null),
			Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, "Processing"),
			Datatypes.member(ActivityDoFollowUpParams.Member.transition, transition)
		);		
		email.doFollowUp(doFollowUpParams);
		pm.currentTransaction().commit();
		// Close
		transition = this.findActivityProcessTransition(
			email, 
			"Close"
		);
		pm.currentTransaction().begin();
		doFollowUpParams = Structures.create(
			ActivityDoFollowUpParams.class, 
			Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, null),
			Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, null),
			Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, "Sent"),
			Datatypes.member(ActivityDoFollowUpParams.Member.transition, transition)
		);		
		email.doFollowUp(doFollowUpParams);
		pm.currentTransaction().commit();    	
    }

    /**
     * Export activity process to SCXML.
     * 
     * @param activityProcess
     * @return
     */
    public String exportActivityProcessToScXml(
    	ActivityProcess activityProcess
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activityProcess);
    	String scxml = "<?xml version=\"1.0\"?>";
    	scxml += "<scxml xmlns=\"http://www.w3.org/2005/07/scxml\" xmlns:a1=\"http://www.opencrx.org/2011/07/scxml\" version=\"1.0\" id=\"" + XMLEncoder.encode(activityProcess.refGetPath().getLastSegment().toString()) + "\" name=\"" + XMLEncoder.encode(activityProcess.getName()) + "\" initialstate=\"" + XMLEncoder.encode((activityProcess.getStartState() == null ? "" : activityProcess.getStartState().refGetPath().getLastSegment().toString())) + "\">";
    	org.opencrx.kernel.activity1.cci2.ActivityProcessStateQuery processStateQuery = (org.opencrx.kernel.activity1.cci2.ActivityProcessStateQuery)pm.newQuery(ActivityProcessState.class);
    	processStateQuery.orderByName().ascending();
    	List<ActivityProcessState> processStates = activityProcess.getState(processStateQuery);
    	for(ActivityProcessState processState: processStates) {
    		scxml += "<state id=\"" + XMLEncoder.encode(processState.refGetPath().getLastSegment().toString()) + "\" name=\"" + XMLEncoder.encode(processState.getName()) + "\">";
    		org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery processTransitionQuery = (org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery)pm.newQuery(ActivityProcessTransition.class);
    		processTransitionQuery.thereExistsPrevState().equalTo(processState);
    		processTransitionQuery.orderByNewPercentComplete().ascending();
    		List<ActivityProcessTransition> processTransitions = activityProcess.getTransition(processTransitionQuery);
    		for(ActivityProcessTransition processTransition: processTransitions) {
    			if(processTransition instanceof SubActivityTransition) {
    				SubActivityTransition subActivityTransition = (SubActivityTransition)processTransition;
    				scxml += "<transition id=\"" + XMLEncoder.encode(subActivityTransition.refGetPath().getLastSegment().toString()) + "\" event=\"doFollowUp\" name=\"" + XMLEncoder.encode(subActivityTransition.getName()) + "\" a1:newActivityState=\"" + subActivityTransition.getNewActivityState() + "\" a1:newPercentComplete=\"" + subActivityTransition.getNewPercentComplete() + "\" a1:subActivityNamePattern=\"" + XMLEncoder.encode(subActivityTransition.getSubActivityNamePattern()) + "\" a1:templateNamePattern=\"" + XMLEncoder.encode(subActivityTransition.getTemplateNamePattern()) + "\" a1:useCreatorAsTemplate=\"" + subActivityTransition.isUseCreatorAsTemplate() + "\" a1:activityCreator=\"" + XMLEncoder.encode(subActivityTransition.getActivityCreator().getName()) + "\">";
    			} else {
    				scxml += "<transition id=\"" + XMLEncoder.encode(processTransition.refGetPath().getLastSegment().toString()) + "\" event=\"doFollowUp\" name=\"" + XMLEncoder.encode(processTransition.getName()) + "\" a1:newActivityState=\"" + processTransition.getNewActivityState() + "\" a1:newPercentComplete=\"" + processTransition.getNewPercentComplete() + "\">";				
    			}
    			scxml += "<target next=\"" + XMLEncoder.encode(processTransition.getNextState().refGetPath().getLastSegment().toString()) + "\" />";
    			scxml += "</transition>";
    		}
    		scxml += "</state>";
    	}
    	scxml += "</scxml>";
    	return scxml;
    }
    
    /**
     * Import activity process from SCXML stream.
     * 
     * @param activitySegment
     * @param scxml
     * @param report
     * @return
     */
    public ActivityProcess importActivityProcessFromScXml(
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment,
    	InputStream scxml,
    	List<String> report
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activitySegment);
		org.w3c.dom.Document doc = null;
		try {
			doc = org.opencrx.kernel.utils.XmlUtils.convertToDoc(scxml);
		} catch(Exception e) {
			report.add(e.getMessage());
		}
		ActivityProcess activityProcess = null;
		if(doc != null) {
			String activityProcessId = doc.getDocumentElement().getAttribute("id");
			String activityProcessName = doc.getDocumentElement().getAttribute("name");
			try {
				if(activityProcessName != null && !activityProcessName.isEmpty()) {
					// Lookup by id
					if(activityProcessId != null) {
						try {
							activityProcess = activitySegment.getActivityProcess(activityProcessId);
						} catch(Exception e) {}
					}
					// Search by name
					if(activityProcess == null) {
						org.opencrx.kernel.activity1.cci2.ActivityProcessQuery activityProcessQuery = (org.opencrx.kernel.activity1.cci2.ActivityProcessQuery)pm.newQuery(ActivityProcess.class);
						activityProcessQuery.name().equalTo(activityProcessName);
						List<ActivityProcess> activityProcesses = activitySegment.getActivityProcess(activityProcessQuery);
						if(activityProcesses.isEmpty()) {
							activityProcess = pm.newInstance(ActivityProcess.class);
							activityProcess.setName(activityProcessName);
							activitySegment.addActivityProcess(
								activityProcessId == null ? 
									Activities.getInstance().getUidAsString() :
										activityProcessId,
								activityProcess
							);
						} else {
							activityProcess = activityProcesses.iterator().next();
						}
					}
					Map<String,ActivityProcessState> processStates = new HashMap<String,ActivityProcessState>();
	    			org.w3c.dom.NodeList lStates = doc.getDocumentElement().getElementsByTagName("state");
	    			if(lStates != null) {
	    				// Phase 1: create states
	    				for(int i = 0; i < lStates.getLength(); i++) {
	    					org.w3c.dom.Element eState = (org.w3c.dom.Element)lStates.item(i);
	    					String stateId = eState.getAttribute("id");
	    					String stateName = eState.getAttribute("name");
	    					if(stateId == null || stateId.isEmpty() || stateName == null || stateName.isEmpty()) {
	    						report.add("Missing or empty attribute id and/or name for state " + i + "(state.id=" + stateId + ", state.name=" + stateName + ")");				    						
	    					} else {
		    					ActivityProcessState processState = activityProcess.getState(stateId);
		    					if(processState == null) {
		    						processState = pm.newInstance(ActivityProcessState.class);
		    						processState.setName(stateName);
		    						activityProcess.addState(
		    							stateId,
		    							processState
		    						);
		    					}
	    						processState.setName(stateName);
		    					processStates.put(
		    						stateId,
		    						processState
		    					);
	    					}
	    				}
	    				// Phase 2: create transitions
	    				for(int i = 0; i < lStates.getLength(); i++) {
	    					org.w3c.dom.Element eState = (org.w3c.dom.Element)lStates.item(i);
	    					String stateId = eState.getAttribute("id");
	    					if(stateId != null) {
	    						ActivityProcessState prevState = processStates.get(stateId);
	    						org.w3c.dom.NodeList lTransitions = eState.getElementsByTagName("transition");
	    						if(lTransitions != null) {
	    							for(int j = 0; j < lTransitions.getLength(); j++) {
	    								org.w3c.dom.Element eTransition = (org.w3c.dom.Element)lTransitions.item(j);
	    								String transitionId = eTransition.getAttributeNS(null, "id");
	    								String transitionName = eTransition.getAttributeNS(null, "name");
	    								String newActivityState = eTransition.getAttribute("a1:newActivityState");
	    								String newPercentComplete = eTransition.getAttribute("a1:newPercentComplete");				    								
	    								String subActivityNamePattern = eTransition.getAttribute("a1:subActivityNamePattern");
	    								String templateNamePattern = eTransition.getAttribute("a1:templateNamePattern");				    								
	    								String useCreatorAsTemplate = eTransition.getAttribute("a1:useCreatorAsTemplate");				    								
	    								String activityCreator = eTransition.getAttribute("a1:activityCreator");
	    								if(transitionId == null || transitionId.isEmpty() || transitionName == null || transitionId.isEmpty()) {
	    									report.add("Missing or empty attribute id and/or name for transition " + j + "(state.id=" + stateId + ", transition.id=" + transitionId + ", transition.name=" + transitionName + ")");
	    								} else {
					    					ActivityProcessTransition processTransition = activityProcess.getTransition(transitionId);
					    					if(processTransition == null) {
					    						if(
					    							(subActivityNamePattern != null && !subActivityNamePattern.isEmpty()) || 
					    							(templateNamePattern != null && !templateNamePattern.isEmpty()) || 
					    							(useCreatorAsTemplate != null && !useCreatorAsTemplate.isEmpty()) || 
					    							(activityCreator != null && !activityCreator.isEmpty())
					    						) {								    							
						    						processTransition = pm.newInstance(SubActivityTransition.class);
					    						} else {
					    							processTransition = pm.newInstance(ActivityProcessTransition.class);
					    						}
					    						processTransition.setName(transitionName);
					    						activityProcess.addTransition(
					    							transitionId,
					    							processTransition
					    						);
					    					}
					    					processTransition.setName(transitionName);
					    					if(newActivityState != null && !newActivityState.isEmpty()) {
					    						processTransition.setNewActivityState(Short.valueOf(newActivityState));
					    					}
					    					if(newPercentComplete != null && !newPercentComplete.isEmpty()) {
					    						processTransition.setNewPercentComplete(Short.valueOf(newPercentComplete));								    						
					    					}
					    					if(processTransition instanceof SubActivityTransition) {
					    						SubActivityTransition subActivityTransition = (SubActivityTransition)processTransition;
					    						subActivityTransition.setSubActivityNamePattern(subActivityNamePattern);
					    						subActivityTransition.setTemplateNamePattern(templateNamePattern);
					    						subActivityTransition.setUseCreatorAsTemplate(Boolean.valueOf(useCreatorAsTemplate));
					    						if(activityCreator != null && !activityCreator.isEmpty()) {
						    						org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery activityCreatorQuery = (org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery)pm.newQuery(ActivityCreator.class);
						    						activityCreatorQuery.name().equalTo(activityCreator);
						    						List<ActivityCreator> activityCreators = activitySegment.getActivityCreator(activityCreatorQuery);									    						
						    						if(!activityCreators.isEmpty()) {
						    							subActivityTransition.setActivityCreator(activityCreators.iterator().next());
						    						} else {
						    							report.add("Activity creator not found " + activityCreator);
						    						}
					    						}
					    					}
					    					processTransition.setPrevState(prevState);
					    					org.w3c.dom.NodeList lTargets = eTransition.getElementsByTagName("target");
					    					if(lTargets != null && lTargets.getLength() > 0) {
			    								org.w3c.dom.Element eTarget = (org.w3c.dom.Element)lTargets.item(0);
			    								ActivityProcessState nextState = processStates.get(eTarget.getAttributeNS("", "next"));
												processTransition.setNextState(nextState);
					    					}
	    								}
	    							}
	    						}
	    					}
	    				}
	    			}
	    			activityProcess.setStartState(
	    				processStates.get(doc.getDocumentElement().getAttributeNS("", "initialstate"))
	    			);
				} else {
					report.add("Attribute name for scxml is either not set or empty");
				}
			} catch(Exception e) {
				report.add("Exception occured while importing " + e.getMessage());
			}
		}
		return activityProcess;
    }
    
    /**
     * Get parties of given activity.
     * 
     * @param activity
     * @return
     */
    public List<AbstractActivityParty> getActivityParties(
    	Activity activity
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
        List<AbstractActivityParty> parties = new ArrayList<AbstractActivityParty>();
        if(activity instanceof EMail) {
        	AbstractEMailRecipientQuery query = (AbstractEMailRecipientQuery)pm.newQuery(AbstractEMailRecipient.class);
        	query.forAllDisabled().isFalse();
        	Collection<AbstractEMailRecipient> c = ((EMail)activity).getEmailRecipient(query);
        	parties.addAll(c);
        } else if(activity instanceof Incident) {
        	IncidentPartyQuery query = (IncidentPartyQuery)pm.newQuery(IncidentParty.class);
        	query.forAllDisabled().isFalse();
        	Collection<IncidentParty> c = ((Incident)activity).getIncidentParty(query);
        	parties.addAll(c);
        } else if(activity instanceof Mailing) {
        	MailingRecipientQuery query = (MailingRecipientQuery)pm.newQuery(MailingRecipient.class);
        	query.forAllDisabled().isFalse();
        	Collection<MailingRecipient> c = ((Mailing)activity).getMailingRecipient();
        	parties.addAll(c);
        } else if(activity instanceof Meeting) {
        	MeetingPartyQuery query = (MeetingPartyQuery)pm.newQuery(MeetingParty.class);
        	query.forAllDisabled().isFalse();
        	Collection<MeetingParty> c = ((Meeting)activity).getMeetingParty();
        	parties.addAll(c);
        } else if(activity instanceof PhoneCall) {
        	AbstractPhoneCallRecipientQuery query = (AbstractPhoneCallRecipientQuery)pm.newQuery(AbstractPhoneCallRecipient.class);
        	query.forAllDisabled().isFalse();
        	Collection<AbstractPhoneCallRecipient> c = ((PhoneCall)activity).getPhoneCallRecipient();
        	parties.addAll(c);
        } else if(activity instanceof Task) {
        	TaskPartyQuery query = (TaskPartyQuery)pm.newQuery(TaskParty.class);
        	query.forAllDisabled().isFalse();
        	Collection<TaskParty> c = ((Task)activity).getTaskParty();
        	parties.addAll(c);
        }
        return parties;
    }

	/**
	 * Get main activity tracker for given activity creator.
	 * 
	 * @param activityCreator
	 * @return
	 */
	public ActivityTracker getMainActivityTracker(
		List<ActivityGroup> activityGroups
	) {
		ActivityTracker tracker = null;
		ActivityTracker firstTracker = null;
		for(ActivityGroup activityGroup: activityGroups) {
			try {
				if(activityGroup instanceof ActivityTracker) {
					if (firstTracker == null) {
						firstTracker = (ActivityTracker)activityGroup;
					}
					if(!activityGroup.getFilteredActivity().isEmpty()) {
						tracker = (ActivityTracker)activityGroup;
					}
				}
			} catch (Exception e) {
				new ServiceException(e).log();
			}
			if(tracker != null) {
				break;
			}
		}
		return tracker != null
			 ? tracker
		     : firstTracker;		
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.backend.AbstractImpl#preDelete(org.opencrx.kernel.generic.jmi1.CrxObject, boolean)
	 */
	@Override
	public void preDelete(
		RefObject_1_0 object, 
		boolean preDelete
	) throws ServiceException {
		super.preDelete(object, preDelete);
		PersistenceManager pm = JDOHelper.getPersistenceManager(object);
		if(object instanceof Activity) {
			this.removeActivity((Activity)object, preDelete);
		} else if(object instanceof ActivityGroup) {
			this.removeActivityGroup((ActivityGroup)object, preDelete);
		} else if(object instanceof WorkAndExpenseRecord) {
			this.removeWorkRecord((WorkAndExpenseRecord)object, preDelete);
		} else if(object instanceof AbstractActivityParty) {
			Activity activity = (Activity)pm.getObjectById(object.refGetPath().getPrefix(7));
			this.markActivityAsDirty(activity);
		} else if(object instanceof ActivityGroupAssignment) {
			Activity activity = (Activity)pm.getObjectById(object.refGetPath().getPrefix(7));
			this.markActivityAsDirty(activity);
		}
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.backend.AbstractImpl#preStore(org.opencrx.kernel.generic.jmi1.CrxObject)
	 */
	@Override
	public void preStore(
		RefObject_1_0 object
	) throws ServiceException {
		super.preStore(object);
		PersistenceManager pm = JDOHelper.getPersistenceManager(object);
		if(object instanceof Activity) {
			this.updateActivity((Activity)object);
		} else if(object instanceof AbstractActivityParty) {
			Activity activity = (Activity)pm.getObjectById(object.refGetPath().getPrefix(7));
			this.markActivityAsDirty(activity);
		} else if(object instanceof ActivityGroupAssignment) {
			Activity activity = (Activity)pm.getObjectById(object.refGetPath().getPrefix(7));
			this.markActivityAsDirty(activity);
		} else if(object instanceof WorkAndExpenseRecord) {
			this.updateWorkAndExpenseRecord((WorkAndExpenseRecord)object);
		} else if(object instanceof ActivityGroup) {
			this.updateActivityGroup((ActivityGroup)object);
		}
	}
	
    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    private static final String[] ACTIVITY_TYPES = 
        new String[]{
            "org:opencrx:kernel:activity1:EMail",
            "org:opencrx:kernel:activity1:EMail", // Fax is deprecated
            "org:opencrx:kernel:activity1:Incident",
            "org:opencrx:kernel:activity1:Mailing",
            "org:opencrx:kernel:activity1:Meeting",
            "org:opencrx:kernel:activity1:EMail", // Sms is deprecated
            "org:opencrx:kernel:activity1:PhoneCall",
            "org:opencrx:kernel:activity1:EMail", // Mms is deprecated
            "org:opencrx:kernel:activity1:Task",
            "org:opencrx:kernel:activity1:Absence",
            "org:opencrx:kernel:activity1:ExternalActivity",
            "org:opencrx:kernel:activity1:SalesVisit"  
        };
    
    /**
     * Activity states.
     */
    public enum ActivityState {
    	
    	NA(0),
    	OPEN(10),
    	CLOSED(20),
    	CANCELLED(30);
    	
    	private final int value;
    	    	
    	private ActivityState(
    		int value
    	) {
    		this.value = value;
    	}
    	
    	public int getValue(
    	) {
    		return this.value;
    	}
    	
    }
    
    /**
     * Activity classes.
     */
    public enum ActivityClass {
    	EMAIL((short)0),
    	INCIDENT((short)2),
    	MAILING((short)3),
    	MEETING((short)4),
    	PHONE_CALL((short)6),
    	TASK((short)8),
    	ABSENCE((short)9),
    	EXTERNAL_ACTIVITY((short)10),    	
    	SALES_VISIT((short)11);
    	
    	private short value;
		
		private ActivityClass(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
    	
    }
    
    /**
     * Party types.
     */
    public enum PartyType {
    	
        NA((short)0),
        REQUIRED((short)410),
        OPTIONAL((short)420),
        ORGANIZER((short)460),
    	EMAIL_FROM((short)210),
    	EMAIL_TO((short)220),
    	EMAIL_CC((short)230),
    	EMAIL_BCC((short)240);

    	private short value;
		
		private PartyType(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
    	
    }
    
    /**
     * Work record types.
     */
    public enum WorkRecordType {
    	
    	NA((short)0),
    	STANDARD((short)1),
    	OVERTIME((short)2);
    	
		private short value;
		
		private WorkRecordType(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
    }
    
    /**
     * Priorities.
     */
    public enum Priority {
    	
    	LOW((short)1),
    	NORMAL((short)2),
    	HIGH((short)3),
    	URGENT((short)4),
    	IMMEDIATE((short)5);
    	
		private short value;
		
		private Priority(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
    	
    }
    
    /**
     * Party statuses.
     */
    public enum PartyStatus {
    	
        NA((short)0),
        NEEDS_ACTION((short)1),
        ACCEPTED((short)2),
        DECLINED((short)3),
        TENTATIVE((short)4),
        DELEGATED((short)5),
        COMPLETED((short)6);

        private short value;
        
        private PartyStatus(
        	short value
        ) {
        	this.value = value;
        }
        
        public short getValue(
        ) {
        	return this.value;
        }
        
    }
    
	/**
	 * Activity link types.
	 */
	public enum ActivityLinkType {
		
		IS_PARENT_OF((short)1),
		BLOCKS((short)2),
		WAS_CLONED_AS((short)3),
		DUPLICATES((short)4),
		INCORPORATES((short)5),
		RELATES_TO((short)6),
		IS_REPLICA_OF((short)7),
		IS_REPLICA_OF_OBFUSCATED((short)8),
		IS_ORIGINAL_OF_OBFUSCATED((short)92),
		IS_ORIGINAL_OF((short)93),
		IS_RELATED_TO((short)94),
		IS_PART_OF((short)95),
		IS_DUPLICATED_BY((short)96),
		IS_DERIVED_FROM((short)97),
		IS_BLOCKED_BY((short)98),
		IS_CHILD_OF((short)99);
		
		private short value;
		
		private ActivityLinkType(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
	}    
	
	/**
	 * Activity group types.
	 * 
	 */
	public enum ActivityGroupType {
		
		NONE((short)0),
		AGENDA((short)10),
		BUG_TRACKER((short)20),
		CAMPAIGN((short)30),
		PROJECT((short)40),
		EVENT((short)50),
		CASE((short)60);

		private short value;
		
		private ActivityGroupType(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
	}
	
    // Booking texts
    protected static final String BOOKING_TEXT_NAME_WORK_EFFORT = "work efforts";
    
    public static final String DEFAULT_EMAIL_CREATOR_ID = "EMailCreator";
    
    public static final String ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING = "Bug + feature tracking process";
    public static final String ACTIVITY_PROCESS_NAME_EMAILS = "E-Mail Process";
    public static final String ACTIVITY_PROCESS_NAME_BULK_EMAILS = "Bulk E-Mail Process";

    public static final String CALENDAR_NAME_DEFAULT_BUSINESS = "Default Business Calendar";

    public static final String ACTIVITY_TYPE_NAME_BUGS_AND_FEATURES = "Bugs + Features";
    public static final String ACTIVITY_TYPE_NAME_EMAILS = "E-Mails";
    public static final String ACTIVITY_TYPE_NAME_BULK_EMAILS = "Bulk E-Mails";
    public static final String ACTIVITY_TYPE_NAME_MEETINGS = "Meetings";
    public static final String ACTIVITY_TYPE_NAME_PHONE_CALLS = "Phone Calls";
    public static final String ACTIVITY_TYPE_NAME_TASKS = "Tasks";
    public static final String ACTIVITY_TYPE_NAME_MAILINGS = "Mailings";
    public static final String ACTIVITY_TYPE_NAME_SALES_VISITS = "Sales Visits";
    public static final String ACTIVITY_TYPE_NAME_ABSENCES = "Absences";
    public static final String ACTIVITY_TYPE_NAME_INCIDENTS = "Incidents";

    public static final String ACTIVITY_CREATOR_NAME_BUGS_AND_FEATURES = "Bugs + Features";
    public static final String ACTIVITY_CREATOR_NAME_EMAILS = "E-Mails";
    public static final String ACTIVITY_CREATOR_NAME_MEETINGS = "Meetings";
    public static final String ACTIVITY_CREATOR_NAME_PHONE_CALLS = "Phone Calls";
    public static final String ACTIVITY_CREATOR_NAME_TASKS = "Tasks";
    public static final String ACTIVITY_CREATOR_NAME_POLLS = "Polls";
    public static final String ACTIVITY_CREATOR_NAME_MEETING_ROOMS = "Meeting Rooms";
    public static final String ACTIVITY_CREATOR_NAME_MAILINGS = "Mailings";
    public static final String ACTIVITY_CREATOR_NAME_SALES_VISITS = "Sales Visits";
    public static final String ACTIVITY_CREATOR_NAME_ABSENCES = "Absences";
    public static final String ACTIVITY_CREATOR_NAME_INCIDENTS = "Incidents";
    public static final String ACTIVITY_CREATOR_NAME_PUBLIC_EMAILS = "Public E-Mails";
    public static final String ACTIVITY_CREATOR_NAME_PUBLIC_MEETINGS = "Public Meetings";
    public static final String ACTIVITY_CREATOR_NAME_PUBLIC_PHONE_CALLS = "Public Phone Calls";
    public static final String ACTIVITY_CREATOR_NAME_PUBLIC_TASKS = "Public Tasks";

    public static final String ACTIVITY_TRACKER_NAME_BUGS_AND_FEATURES = "Bugs + Features";
    public static final String ACTIVITY_TRACKER_NAME_EMAILS = "E-Mails";
    public static final String ACTIVITY_TRACKER_NAME_MEETINGS = "Meetings";
    public static final String ACTIVITY_TRACKER_NAME_PHONE_CALLS = "Phone Calls";
    public static final String ACTIVITY_TRACKER_NAME_TASKS = "Tasks";
    public static final String ACTIVITY_TRACKER_NAME_PUBLIC = "Public";
    public static final String ACTIVITY_TRACKER_NAME_TRASH = "Trash";
    public static final String ACTIVITY_TRACKER_NAME_POLLS = "Polls";
    public static final String ACTIVITY_TRACKER_NAME_MEETING_ROOMS = "Meeting Rooms";

    public static final String UNSPECIFIED_ADDRESS = "NO_ADDRESS_SPECIFIED";

	public static final String ORIGINAL_MESSAGE_MEDIA_NAME = "ORIGINAL";
	
	private static final SimpleDateFormat DATEFORMAT_MESSAGE_HEADER = (SimpleDateFormat)SimpleDateFormat.getDateTimeInstance(
		java.text.DateFormat.SHORT, 
		java.text.DateFormat.SHORT, 
		new Locale("US")
	);
	static {
		DATEFORMAT_MESSAGE_HEADER.applyPattern("d-MMM-yyyy HH:mm:ss Z");
	}
	
}

//--- End of File -----------------------------------------------------------
