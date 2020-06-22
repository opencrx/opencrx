/*
 * ====================================================================
 * Project:     openCRX/CalDAV, http://www.opencrx.org/
 * Description: ActivitiesQueryHelper
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.utils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery;
import org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery;
import org.opencrx.kernel.activity1.cci2.ActivityFilterGroupQuery;
import org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery;
import org.opencrx.kernel.activity1.cci2.ResourceQuery;
import org.opencrx.kernel.activity1.jmi1.AbstractFilterActivity;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.home1.cci2.UserHomeQuery;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.naming.Path;
import org.w3c.format.DateTimeFormat;

public class ActivityQueryHelper {

    //-----------------------------------------------------------------------
    public ActivityQueryHelper(
       PersistenceManager pm
    ) {
        this.pm = pm;
    }
    
    //-----------------------------------------------------------------------
    public void close(
    ) {
    	if(this.pm != null) {
    		this.pm.close();
    	}
    }
    
    //-----------------------------------------------------------------------
    /**
     *  Id has pattern
     *  {provider.id} "/" {segment.id} "/" tracker|milestone|category|resource|home|filter "/" {calendar.name} ["/filter/" {filter.name}]
     */
    public int parseQueryId(
       String id
    ) throws ServiceException {
    	this.queryId = id;
        List<String> l = ActivityQueryHelper.splitUri(id);
        if(l.size() >= 3) {
            String providerName = l.get(0);
            String segmentName = l.get(1);
            String calendarType = null;
            this.calendarName = null;
            this.filterName = null;
            if("filter".equals(l.get(l.size()-2)) && l.size() > 4) {
                calendarType = l.get(l.size()-4);
                this.calendarName = l.get(l.size()-3);
                this.filterName = l.get(l.size()-1);
            }
            else {
                calendarType = l.get(l.size()-2);
                this.calendarName = l.get(l.size()-1);
                this.filterName = null;                
            }
            if(".chandler".equals(this.calendarName)) {
                calendarType = l.get(l.size()-3);
                this.calendarName = l.get(l.size()-2);
            }
            else if("null".equals(this.calendarName)) {
                return l.size();
            }
            this.activitySegment = Activities.getInstance().getActivitySegment(this.pm, providerName, segmentName);
            this.userHomeSegment = UserHomes.getInstance().getUserHomeSegment(this.pm, providerName, segmentName);
            this.activityGroup = null;
            this.userHome = null;
            this.activityFilter = null;
            this.resource = null;
            if(
                "milestone".equals(calendarType) ||
                "category".equals(calendarType) ||
                "tracker".equals(calendarType)
            ) {
                List<? extends ActivityGroup> activityGroups = Collections.emptyList();
                if("milestone".equals(calendarType)) {
                    ActivityMilestoneQuery query = (ActivityMilestoneQuery)this.pm.newQuery(ActivityMilestone.class);
                    query.name().equalTo(this.calendarName);
                    List<org.opencrx.kernel.activity1.jmi1.ActivityMilestone> milestones = this.activitySegment.getActivityMilestone(query);
                    activityGroups = milestones;
                }
                else if("category".equals(calendarType)) {
                    ActivityCategoryQuery query = (ActivityCategoryQuery)this.pm.newQuery(ActivityCategory.class);
                    query.name().equalTo(this.calendarName);
                    List<org.opencrx.kernel.activity1.jmi1.ActivityCategory> categories = this.activitySegment.getActivityCategory(query);
                    activityGroups = categories;
                }
                else if("tracker".equals(calendarType)) {
                    ActivityTrackerQuery query = (ActivityTrackerQuery)this.pm.newQuery(ActivityTracker.class);
                    query.name().equalTo(this.calendarName);
                    List<org.opencrx.kernel.activity1.jmi1.ActivityTracker> trackers = this.activitySegment.getActivityTracker(query);
                    activityGroups = trackers;
                }
                if(!activityGroups.isEmpty()) {
                    this.activityGroup = activityGroups.iterator().next();
                    if(this.filterName != null) {
                        ActivityFilterGroupQuery query = (ActivityFilterGroupQuery)this.pm.newQuery(ActivityFilterGroup.class);
                        query.name().equalTo(this.filterName);
                        List<ActivityFilterGroup> activityFilters = this.activityGroup.getActivityFilter(query);
                        if(!activityFilters.isEmpty()) {
                            this.activityFilter = activityFilters.iterator().next();
                        }
                    }
                }                
            }
            else if("globalfilter".equals(calendarType) || "filter".equals(calendarType)) {
                ActivityFilterGlobalQuery query = (ActivityFilterGlobalQuery)this.pm.newQuery(ActivityFilterGlobal.class);
                query.name().equalTo(this.calendarName);
                List<ActivityFilterGlobal> globalFilters = this.activitySegment.getActivityFilter(query);
                this.activityFilter = globalFilters.iterator().next();
            }
            else if("userhome".equals(calendarType) || "home".equals(calendarType)) {
            	// Lookup by user's email address
            	if(this.calendarName.indexOf("@") > 0) {
            		UserHomeQuery query = (UserHomeQuery)this.pm.newQuery(UserHome.class);
            		query.thereExistsEMailAccount().name().equalTo(this.calendarName);
            		List<UserHome> userHomes = this.userHomeSegment.getUserHome(query);
            		if(!userHomes.isEmpty()) {
            			this.userHome = userHomes.iterator().next();
            		}
            	}
            	else {
	                this.userHome = (org.opencrx.kernel.home1.jmi1.UserHome)this.pm.getObjectById(
	                    new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/" + providerName + "/segment/" + segmentName + "/userHome/" + this.calendarName)
	                );
            	}
            }
            else if("resource".equals(calendarType)) {
                ResourceQuery query = (ResourceQuery)this.pm.newQuery(Resource.class);
                query.name().equalTo(this.calendarName);
                List<Resource> resources = this.activitySegment.getResource(query);
                this.resource = resources.iterator().next();
            }
        }
        return l.size();
    }
        
    //-----------------------------------------------------------------------
    public void parseDisabledFilter(
        String isDisabledFilter
    ) {
        this.isDisabledFilter = isDisabledFilter == null
            ? false
            : Boolean.valueOf(isDisabledFilter); 
    }
    
    //-----------------------------------------------------------------------
    public boolean isDisabledFilter(
    ) {
        return this.isDisabledFilter;
    }
    
    //-----------------------------------------------------------------------
    public ActivityGroup getActivityGroup(
    ) {
        return this.activityGroup;
    }
    
    //-----------------------------------------------------------------------
    public UserHome getUserHome(
    ) {
        return this.userHome;
    }
    
    //-----------------------------------------------------------------------
    public Resource getResource(
    ) {
        return this.resource;
    }
    
    //-----------------------------------------------------------------------
    public AbstractFilterActivity getActivityFilter(
    ) {
        return this.activityFilter;
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.Segment getActivitySegment(
    ) {
        return this.activitySegment;
    }
    
    //-----------------------------------------------------------------------
    public String getCalendarName(
    ) {
        return this.calendarName;
    }
    
    //-----------------------------------------------------------------------
    public String getFilterName(
    ) {
        return this.filterName;
    }

    //-----------------------------------------------------------------------
    public String getQueryId(
    ) {
    	return this.queryId;
    }
    
    //-----------------------------------------------------------------------
    public BasicObject getSource(
    ) {
        if(this.activityFilter != null) {
            return (BasicObject)this.activityFilter;
        }
        else if(this.activityGroup != null) {
            return this.activityGroup;
        }
        else if(this.userHome != null) {
            return this.userHome;
        }
        else if(this.resource != null) {
            return this.resource;
        }
        else {
            return null;
        }    	
    }
    
    //-----------------------------------------------------------------------
    public Collection<Activity> getFilteredActivities(
        ActivityQuery activityQuery            
    ) {
    	BasicObject source = this.getSource();
        if(source instanceof AbstractFilterActivity) {
            return ((AbstractFilterActivity)source).getFilteredActivity(activityQuery);
        }
        else if(source instanceof ActivityGroup) {
            return ((ActivityGroup)source).getFilteredActivity(activityQuery);
        }
        else if(source instanceof UserHome) {
            return ((UserHome)source).getAssignedActivity(activityQuery);
        }
        else if(source instanceof Resource) {
            return ((Resource)source).getAssignedActivity(activityQuery);
        }
        else {
            return Collections.emptyList();
        }
    }
    
    //-----------------------------------------------------------------------
    public static String formatDateTime(
        Date date
    ) {
        return DateTimeFormat.BASIC_UTC_FORMAT.format(date).substring(0, 15) + "Z";
    }
    
    //-----------------------------------------------------------------------
    public static String formatDate(
        Date date
    ) {
        return DateTimeFormat.BASIC_UTC_FORMAT.format(date).substring(0, 8);
    }
    
    //-----------------------------------------------------------------------
    public static Date getActivityGroupModifiedAt(
        ActivityGroup activityGroup
    ) {
        if(activityGroup instanceof ActivityTracker) {
            return ((ActivityTracker)activityGroup).getModifiedAt();
        }
        else if(activityGroup instanceof ActivityMilestone) {
            return ((ActivityMilestone)activityGroup).getModifiedAt();
        }
        else if(activityGroup instanceof ActivityCategory) {
            return ((ActivityCategory)activityGroup).getModifiedAt();
        }
        else {
            return new Date();
        }
    }

    //-----------------------------------------------------------------------
    public static Date getActivityGroupCreatedAt(
        ActivityGroup activityGroup
    ) {
        if(activityGroup instanceof ActivityTracker) {
            return ((ActivityTracker)activityGroup).getCreatedAt();
        }
        else if(activityGroup instanceof ActivityMilestone) {
            return ((ActivityMilestone)activityGroup).getCreatedAt();
        }
        else if(activityGroup instanceof ActivityCategory) {
            return ((ActivityCategory)activityGroup).getCreatedAt();
        }
        else {
            return new Date();
        }
    }
    
    //-----------------------------------------------------------------------
    public static List<String> splitUri(
        String uri 
    ) throws IllegalArgumentException  {
        try {
            String[] ss = uri.split("/");
            int pathLength = ss.length - 1;  // First element is empty string
            if (pathLength < 2) {
                throw new IllegalArgumentException ("Bad uri: " + uri);
            }
            List<String> l = Arrays.asList(ss);
            return l.subList(1, l.size());
        } 
        catch (Exception e) {
            throw new IllegalArgumentException ("Bad uri: " + uri);
        }
    }
            
    //-----------------------------------------------------------------------
    public PersistenceManager getPersistenceManager(
    ) {
        return this.pm;
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected final PersistenceManager pm;
    protected String queryId;
    protected ActivityGroup activityGroup = null;
    protected UserHome userHome = null;
    protected Resource resource = null;
    protected AbstractFilterActivity activityFilter = null;
    protected org.opencrx.kernel.activity1.jmi1.Segment activitySegment = null;
    protected org.opencrx.kernel.home1.jmi1.Segment userHomeSegment = null;
    protected String calendarName = null;
    protected String filterName = null;
    protected boolean isDisabledFilter;
    
}
