/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ConnectionHelperController
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
package org.opencrx.kernel.portal.wizard;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.jdo.PersistenceManager;

import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;

/**
 * ConnectionHelperController
 *
 */
public class ConnectionHelperController extends AbstractWizardController {

	/**
	 * ResourceType
	 *
	 */
	public enum ResourceType {
    	PROFILE("Profiles"),
    	EVENTS_AND_TASKS("Events / Tasks"),
    	CONTACT("Contacts");
    	
    	private ResourceType(
    		String label
    	) {
    		this.label = label;
    	}
    	public String getLabel(
    	) {
    		return this.label;
    	}
    	private final String label;
	}

    /**
     * SelectorType
     *
     */
    public enum SelectorType {
    	TRACKER,
    	TRACKER_FILTER,
    	CATEGORY,
    	CATEGORY_FILTER,
    	MILESTONE,
    	MILESTONE_FILTER,
    	GLOBAL_FILTER,
    	USERHOME,
    	AIRSYNCPROFILE,
    	CALENDARPROFILE,
    	RESOURCE,
    	BDAY,
    	VCARD,
    	DOCUMENTPROFILE,
    	CARDPROFILE
    }

	/**
	 * Constructor.
	 * 
	 */
	public ConnectionHelperController(
	) {
		super();
	}
	
	public void doReload(
		@RequestParameter(name = "isInitialized") Boolean isInitialized,
		@RequestParameter(name = "mustReload") Boolean mustReload,
		@RequestParameter(name = "anchorObjectXri") String anchorObjectXri,
		@RequestParameter(name = "type") String resourceType,
		@RequestParameter(name = "selectorType") String selectorType,
		@RequestParameter(name = "optionMax") Integer optionMax,
		@RequestParameter(name = "optionUser") String optionUser,
		@RequestParameter(name = "optionIsDisabled") Boolean optionIsDisabled,
		@RequestParameter(name = "optionSummaryPrefix") String optionSummaryPrefix,
		@RequestParameter(name = "optionCategories") String optionCategories,
		@RequestParameter(name = "optionYear") Integer optionYear,
		@RequestParameter(name = "optionTimelineHeight") Integer optionTimelineHeight,
		@RequestParameter(name = "optionAlarm") Boolean optionAlarm
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		PersistenceManager pm = this.getPm();
		RefObject_1_0 obj = this.getObject();
		this.isInitialized = isInitialized;
		this.anchorObjectXri = anchorObjectXri;
	    this.resourceType = resourceType != null ? ResourceType.valueOf(resourceType) : null;
	    this.selectorType = selectorType != null ? SelectorType.valueOf(selectorType) : null;
		this.optionMax = optionMax == null ? 500 : optionMax;
		this.optionUser = optionUser == null ? app.getLoginPrincipal() : optionUser;	
		this.optionIsDisabled = optionIsDisabled == null ? false : optionIsDisabled;
		this.optionSummaryPrefix = optionSummaryPrefix == null ? "Birthdays" : optionSummaryPrefix;
		this.optionCategories = optionCategories == null ? "Birthday" : optionCategories;
		this.optionYear =optionYear == null ? 2012 : optionYear;
		this.optionTimelineHeight = optionTimelineHeight == null ? 500 : optionTimelineHeight;
		this.optionAlarm = optionAlarm == null ? false : optionAlarm;
	    if(Boolean.TRUE.equals(isInitialized) && (anchorObjectXri != null)) {
	        try {
	            obj = (RefObject_1_0)pm.getObjectById(new Path(anchorObjectXri));
	        } catch (Exception e) {}
	    }
	    {
	    	NumberFormat formatter = new DecimalFormat("00000");	    	
		    this.urlBase = (this.getRequest().getRequestURL().toString()).substring(0, (this.getRequest().getRequestURL().toString()).indexOf(this.getRequest().getServletPath().toString()));
		    this.anchorObjectXriFromInitialObject = "";
		    String anchorObjectFilteredXriFromInitialObject = null;
	
		    // get current userHome
		    org.opencrx.kernel.home1.jmi1.UserHome currentUserHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());
		    org.opencrx.kernel.account1.jmi1.Segment accountSegment = org.opencrx.kernel.backend.Accounts.getInstance().getAccountSegment(pm, this.getProviderName(), this.getSegmentName());
		    org.opencrx.kernel.activity1.jmi1.Segment activitySegment = org.opencrx.kernel.backend.Activities.getInstance().getActivitySegment(pm, this.getProviderName(), this.getSegmentName());
		    org.opencrx.kernel.home1.jmi1.Segment homeSegment = org.opencrx.kernel.backend.UserHomes.getInstance().getUserHomeSegment(pm, this.getProviderName(), this.getSegmentName());
	
		    // Option activity filter at group level
		    boolean isGroupFilter = false;
		    if(obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup) {
		    	isGroupFilter = true;
				org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup activityFilterGroup =
		        	(org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup)obj;
		      	if((activityFilterGroup.getName() != null) && !activityFilterGroup.getName().isEmpty()) {
		      		this.anchorObjectXriFromInitialObject = activityFilterGroup.refMofId();
		      	}
		      	obj = (RefObject_1_0)pm.getObjectById(activityFilterGroup.refGetPath().getParent().getParent());
		    } 
		    if(obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal) {
		    	if(this.resourceType == null) {
		    		this.resourceType = ResourceType.EVENTS_AND_TASKS;
			    	this.selectorType = SelectorType.GLOBAL_FILTER;
		    	}
		      	org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal activityFilterGlobal =
		        	(org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal)obj;
		      	if((activityFilterGlobal.getName() != null) && !activityFilterGlobal.getName().isEmpty()) {
		        	anchorObjectFilteredXriFromInitialObject = activityFilterGlobal.refMofId();
		      	}
		    } else if(obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityTracker) {
				org.opencrx.kernel.activity1.jmi1.ActivityTracker activityTracker =
					(org.opencrx.kernel.activity1.jmi1.ActivityTracker)obj;
				if((activityTracker.getName() != null) && !activityTracker.getName().isEmpty()) {
			        if(!isGroupFilter) {
			        	this.anchorObjectXriFromInitialObject = activityTracker.refMofId();
			        }
		      	}
				if(this.resourceType == null) {
					this.resourceType = ResourceType.EVENTS_AND_TASKS;
					this.selectorType = isGroupFilter ? SelectorType.TRACKER_FILTER : SelectorType.TRACKER;
				}
		    } else if(obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityCategory) {
				org.opencrx.kernel.activity1.jmi1.ActivityCategory activityCategory =
					(org.opencrx.kernel.activity1.jmi1.ActivityCategory)obj;
				if((activityCategory.getName() != null) && !activityCategory.getName().isEmpty()) {
			        if (!isGroupFilter) {
			        	this.anchorObjectXriFromInitialObject = activityCategory.refMofId();
			        }
				}
				if(this.resourceType == null) {
					this.resourceType = ResourceType.EVENTS_AND_TASKS;
					this.selectorType = isGroupFilter ? SelectorType.CATEGORY_FILTER : SelectorType.CATEGORY;
				}
		    } else if(obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityMilestone) {
				org.opencrx.kernel.activity1.jmi1.ActivityMilestone activityMilestone =
					(org.opencrx.kernel.activity1.jmi1.ActivityMilestone)obj;
				if ((activityMilestone.getName() != null) && !activityMilestone.getName().isEmpty()) {
			        if(!isGroupFilter) {
			        	this.anchorObjectXriFromInitialObject = activityMilestone.refMofId();
			        }
				}
				if(this.resourceType == null) {
					this.resourceType = ResourceType.EVENTS_AND_TASKS;
					this.selectorType = isGroupFilter ? SelectorType.MILESTONE_FILTER : SelectorType.MILESTONE;
				}
		    } else if(obj instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
		    	if(this.resourceType == null) {
		    		this.resourceType = ResourceType.EVENTS_AND_TASKS;
			    	this.selectorType = SelectorType.USERHOME;
		    	}
		        if(Boolean.TRUE.equals(isInitialized)) {
		        	this.anchorObjectXriFromInitialObject = ((org.opencrx.kernel.home1.jmi1.UserHome)obj).refMofId();
		        }
		    } else if(obj instanceof org.opencrx.kernel.home1.jmi1.SyncProfile) {
		    	if(this.resourceType == null) {
		    		this.resourceType = ResourceType.PROFILE;
		    	}
		    	org.opencrx.kernel.home1.jmi1.SyncProfile syncProfile = (org.opencrx.kernel.home1.jmi1.SyncProfile)obj;
				if((syncProfile.getName() != null) && !syncProfile.getName().isEmpty()) {
			        if (!isGroupFilter) {
			        	this.anchorObjectXriFromInitialObject = syncProfile.refMofId();
			        }
				}
		    } else if(obj instanceof org.opencrx.kernel.activity1.jmi1.Resource) {
		    	if(this.resourceType == null) {
		    		this.resourceType = ResourceType.EVENTS_AND_TASKS;
			    	this.selectorType = SelectorType.RESOURCE;
		    	}
		    	org.opencrx.kernel.activity1.jmi1.Resource resource = (org.opencrx.kernel.activity1.jmi1.Resource)obj;
				if((resource.getName() != null) && !resource.getName().isEmpty()) {
			        if (!isGroupFilter) {
			        	this.anchorObjectXriFromInitialObject = resource.refMofId();
			        }
				}
		    } else if(obj instanceof org.opencrx.kernel.account1.jmi1.AccountFilterGlobal) {
		        if(this.resourceType == ResourceType.CONTACT) {
		        	this.selectorType = SelectorType.VCARD;
		        } else if(this.resourceType == null) {
		        	this.resourceType = ResourceType.EVENTS_AND_TASKS;
		        	this.selectorType = SelectorType.BDAY;
		        }
		        org.opencrx.kernel.account1.jmi1.AccountFilterGlobal accountFilterGlobal =
		          (org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)obj;
		        if ((accountFilterGlobal.getName() != null) && (accountFilterGlobal.getName().length() > 0)) {
		          anchorObjectFilteredXriFromInitialObject = accountFilterGlobal.refMofId();
		        }
		    }
		    if(anchorObjectFilteredXriFromInitialObject != null) {
		    	this.anchorObjectXriFromInitialObject = anchorObjectFilteredXriFromInitialObject;
		    }
		    this.anchorObjectLabel = "Anchor object";
	
		    this.anchorObjects = new TreeMap<String,String>();
	
		    if(this.selectorType == SelectorType.TRACKER) {
		    	this.anchorObjectLabel = app.getLabel(ACTIVITYTRACKER_CLASS);
		        // get ActivityTrackers (not disabled)
		        org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery trackerQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
		        trackerQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(org.opencrx.kernel.activity1.jmi1.ActivityGroup ag: activitySegment.getActivityTracker(trackerQuery)) {
		            String display = (ag.getName() != null ? ag.getName() : UNKNOWN);
		            String sortKey = display.toUpperCase() + formatter.format(index++);
		            this.anchorObjects.put(
		                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                ag.refMofId()
		            );
		        }
		    } else if(this.selectorType == SelectorType.TRACKER_FILTER) {
		    	this.anchorObjectLabel = app.getLabel(ACTIVITYTRACKER_CLASS);
		        // get ActivityFilters of ActivityTrackers (not disabled)
		        org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery trackerQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityTracker.class);
		        trackerQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityTracker> i = activitySegment.getActivityTracker(trackerQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = i.next();
		            for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup> j = ag.<org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup>getActivityFilter().iterator(); j.hasNext() && index < MAX_ENTRY_SELECT; ) {
		                org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup afg = j.next();
		                String display = (ag.getName() != null ? ag.getName() : UNKNOWN) + " &lt;" + (afg.getName() != null ? afg.getName() : UNKNOWN) + "&gt;";
		                String sortKey = display.toUpperCase() + formatter.format(index++);
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    afg.refMofId()
		                );
		            }
		        }
		    } else if(this.selectorType == SelectorType.CATEGORY) {
		    	this.anchorObjectLabel = app.getLabel(ACTIVITYCATEGORY_CLASS);
		        // get ActivityCategories (not disabled)
		        org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery categoryQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityCategory.class);
		        categoryQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityCategory> i = activitySegment.getActivityCategory(categoryQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = i.next();
		            String display = (ag.getName() != null ? ag.getName() : UNKNOWN);
		            String sortKey = display.toUpperCase() + formatter.format(index++);
		            this.anchorObjects.put(
		                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                ag.refMofId()
		            );
		        }
		    } else if(this.selectorType == SelectorType.CATEGORY_FILTER) {
		    	this.anchorObjectLabel = app.getLabel(ACTIVITYCATEGORY_CLASS);
		        // get ActivityFilters of ActivityCategories (not disabled)
		        org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery categoryQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityCategory.class);
		        categoryQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityCategory> i = activitySegment.getActivityCategory(categoryQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = i.next();
		            for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup> j = ag.<org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup>getActivityFilter().iterator(); j.hasNext() && index < MAX_ENTRY_SELECT; ) {
		                org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup afg = j.next();
		                String display = (ag.getName() != null ? ag.getName() : UNKNOWN) + " &lt;" + (afg.getName() != null ? afg.getName() : UNKNOWN) + "&gt;";
		                String sortKey = display.toUpperCase() + formatter.format(index++);
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    afg.refMofId()
		                );
		            }
		        }
		    } else if(this.selectorType == SelectorType.MILESTONE) {
		    	this.anchorObjectLabel = app.getLabel(ACTIVITYMILESTONE_CLASS);
		        // get ActivityMilestones (not disabled)
		        org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery milestoneQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityMilestone.class);
		        milestoneQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityMilestone> i = activitySegment.getActivityMilestone(milestoneQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = i.next();
		            String display = (ag.getName() != null ? ag.getName() : UNKNOWN);
		            String sortKey = display.toUpperCase() + formatter.format(index++);
		            this.anchorObjects.put(
		                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                ag.refMofId()
		            );
		        }
		    } else if(this.selectorType == SelectorType.MILESTONE_FILTER) {
		    	this.anchorObjectLabel = app.getLabel(ACTIVITYMILESTONE_CLASS);
		        // get ActivityFilters of ActivityMilestones (not disabled)
		        org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery milestoneQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityMilestone.class);
		        milestoneQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityMilestone> i = activitySegment.getActivityMilestone(milestoneQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.activity1.jmi1.ActivityGroup ag = i.next();
		            for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup> j = ag.<org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup>getActivityFilter().iterator(); j.hasNext() && index < MAX_ENTRY_SELECT; ) {
		                org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup afg = j.next();
		                String display = (ag.getName() != null ? ag.getName() : UNKNOWN) + " &lt;" + (afg.getName() != null ? afg.getName() : UNKNOWN) + "&gt;";
		                String sortKey = display.toUpperCase() + formatter.format(index++);
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    afg.refMofId()
		                );
		            }
		        }
		    } else if(this.selectorType == SelectorType.GLOBAL_FILTER) {
		    	this.anchorObjectLabel = app.getLabel(ACTIVITYFILTERGLOBAL_CLASS);
		        // get ActivityTrackers (not disabled)
		        org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery activityQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal.class);
		        activityQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal> i = activitySegment.getActivityFilter(activityQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal af = i.next();
		            String display = (af.getName() != null ? af.getName() : UNKNOWN);
		            String sortKey = display.toUpperCase() + formatter.format(index++);
		            anchorObjects.put(
		                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                af.refMofId()
		            );
		        }
		    } else if(this.selectorType == SelectorType.USERHOME) {
		    	this.anchorObjectLabel = app.getLabel(USERHOME_CLASS);
		        // get UserHomes
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.home1.jmi1.UserHome> i = homeSegment.<org.opencrx.kernel.home1.jmi1.UserHome>getUserHome().iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.home1.jmi1.UserHome userHome = i.next();
		            org.opencrx.kernel.account1.jmi1.Contact contact = null;
		            try {
		                contact = userHome.getContact();
		            } catch (Exception e) {}
		            String principal = userHome.refGetPath().getBase();
		            String display = (contact != null && contact.getFullName() != null ? contact.getFullName() : UNKNOWN) + " [" + principal + "]";
		            String sortKey = display.toUpperCase() + formatter.format(index++);
		            this.anchorObjects.put(
		                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                userHome.refMofId()
		            );
		        }
		    } else if(this.selectorType == SelectorType.CALENDARPROFILE) {
		    	this.anchorObjectLabel = app.getLabel(CALENDARPROFILE_CLASS);
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.home1.jmi1.SyncProfile> i = currentUserHome.<org.opencrx.kernel.home1.jmi1.SyncProfile>getSyncProfile().iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.home1.jmi1.SyncProfile syncProfile = i.next();
		            if(syncProfile instanceof org.opencrx.kernel.home1.jmi1.CalendarProfile) {
			            String display = (syncProfile.getName() != null ? syncProfile.getName() : "?");
			            String sortKey = display.toUpperCase() + formatter.format(index++);
			            this.anchorObjects.put(
			                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
			                syncProfile.refMofId()
			            );
		        	}
		        }
		    } else if(this.selectorType == SelectorType.AIRSYNCPROFILE) {
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.home1.jmi1.SyncProfile> i = currentUserHome.<org.opencrx.kernel.home1.jmi1.SyncProfile>getSyncProfile().iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.home1.jmi1.SyncProfile syncProfile = i.next();
		            if(syncProfile instanceof org.opencrx.kernel.home1.jmi1.AirSyncProfile) {
		                String display = (syncProfile.getName() != null ? syncProfile.getName() : "?");
		                String sortKey = display.toUpperCase() + formatter.format(index++);
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    syncProfile.refMofId()
		                );
		            }
		        }
		    } else if(this.selectorType == SelectorType.CARDPROFILE) {
		    	this.anchorObjectLabel = app.getLabel(CARDPROFILE_CLASS);
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.home1.jmi1.SyncProfile> i = currentUserHome.<org.opencrx.kernel.home1.jmi1.SyncProfile>getSyncProfile().iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.home1.jmi1.SyncProfile syncProfile = i.next();
		            if (syncProfile instanceof org.opencrx.kernel.home1.jmi1.CardProfile) {
		                String display = (syncProfile.getName() != null ? syncProfile.getName() : "?");
		                String sortKey = display.toUpperCase() + formatter.format(index++);
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    syncProfile.refMofId()
		                );
		            }
		        }
		    } else if(this.selectorType == SelectorType.DOCUMENTPROFILE) {
		    	this.anchorObjectLabel = app.getLabel(DOCUMENTPROFILE_CLASS);
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.home1.jmi1.SyncProfile> i = currentUserHome.<org.opencrx.kernel.home1.jmi1.SyncProfile>getSyncProfile().iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.home1.jmi1.SyncProfile syncProfile = i.next();
		            if (syncProfile instanceof org.opencrx.kernel.home1.jmi1.DocumentProfile) {
		                String display = (syncProfile.getName() != null ? syncProfile.getName() : "?");
		                String sortKey = display.toUpperCase() + formatter.format(index++);
		                this.anchorObjects.put(
		                    HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                    syncProfile.refMofId()
		                );
		            }
		        }
		    } else if(this.selectorType == SelectorType.RESOURCE) {
		    	this.anchorObjectLabel = app.getLabel(RESOURCE_CLASS);
		        // get Resources (not disabled)
		        org.opencrx.kernel.activity1.cci2.ResourceQuery resourceQuery = 
		        	(org.opencrx.kernel.activity1.cci2.ResourceQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Resource.class);
		        resourceQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.activity1.jmi1.Resource> i = activitySegment.getResource(resourceQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.activity1.jmi1.Resource resource = i.next();
		            org.opencrx.kernel.account1.jmi1.Contact contact = resource.getContact();
		            String display = (resource.getName() != null ? resource.getName() : UNKNOWN) + " [" + (contact != null && contact.getFullName() != null ? contact.getFullName() : UNKNOWN) + "]";
		            String sortKey = display.toUpperCase() + formatter.format(index++);
		            this.anchorObjects.put(
		                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                resource.refMofId()
		            );
		        }
		    } else if((this.selectorType == SelectorType.BDAY) || (this.selectorType == SelectorType.VCARD)) {
		    	this.anchorObjectLabel = app.getLabel(ACCOUNTFILTERGLOBAL_CLASS);
		        // get AccountFilterGlobals (not disabled)
		        org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery accountQuery = 
		        	(org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountFilterGlobal.class);
		        accountQuery.forAllDisabled().isFalse();
		        int index = 0;
		        for(Iterator<org.opencrx.kernel.account1.jmi1.AccountFilterGlobal> i = accountSegment.getAccountFilter(accountQuery).iterator(); i.hasNext() && index < MAX_ENTRY_SELECT; ) {
		            org.opencrx.kernel.account1.jmi1.AccountFilterGlobal af = i.next();
		            String display = (af.getName() != null ? af.getName() : UNKNOWN);
		            String sortKey = display.toUpperCase() + formatter.format(index++);
		            this.anchorObjects.put(
		                HTML_COMMENT_BEGIN + sortKey + HTML_COMMENT_END + display,
		                af.refMofId()
		            );
		        }
		    }
	    }
	}

    /**
	 * @return the resourceType
	 */
	public ResourceType getResourceType(
	) {
		return this.resourceType;
	}

	/**
	 * @return the selectorType
	 */
	public SelectorType getSelectorType(
	) {
		return this.selectorType;
	}
	/**
	 * @return the optionMax
	 */
	public Integer getOptionMax(
	) {
		return this.optionMax;
	}

	/**
	 * @return the optionUser
	 */
	public String getOptionUser(
	) {
		return this.optionUser;
	}

	/**
	 * @return the optionIsDisabled
	 */
	public Boolean getOptionIsDisabled(
	) {
		return this.optionIsDisabled;
	}

	/**
	 * @return the optionSummaryPrefix
	 */
	public String getOptionSummaryPrefix(
	) {
		return this.optionSummaryPrefix;
	}

	/**
	 * @return the optionCategories
	 */
	public String getOptionCategories(
	) {
		return this.optionCategories;
	}

	/**
	 * @return the optionYear
	 */
	public Integer getOptionYear(
	) {
		return this.optionYear;
	}

	/**
	 * @return the optionTimelineHeight
	 */
	public Integer getOptionTimelineHeight(
	) {
		return this.optionTimelineHeight;
	}

	/**
	 * @return the optionAlarm
	 */
	public Boolean getOptionAlarm(
	) {
		return this.optionAlarm;
	}

	/**
	 * @return the anchorObjects
	 */
	public Map<String, String> getAnchorObjects(
	) {
		return this.anchorObjects;
	}
	
	/**
	 * @return the anchorObjectXri
	 */
	public String getAnchorObjectXri(
	) {
		return this.anchorObjectXri;
	}
	
	/**
	 * @return the anchorObjectLabel
	 */
	public String getAnchorObjectLabel(
	) {
		return this.anchorObjectLabel;
	}
	
	/**
	 * @return the anchorObjectXriFromInitialObject
	 */
	public String getAnchorObjectXriFromInitialObject(
	) {
		return this.anchorObjectXriFromInitialObject;
	}

	/**
	 * @return the urlBase
	 */
	public String getUrlBase(
	) {
		return this.urlBase;
	}

    /**
	 * @return the isInitialized
	 */
	public Boolean getIsInitialized(
	) {
		return this.isInitialized;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
    public static final String ACTIVITYTRACKER_CLASS = "org:opencrx:kernel:activity1:ActivityTracker";
    public static final String ACTIVITYCATEGORY_CLASS = "org:opencrx:kernel:activity1:ActivityCategory";
    public static final String ACTIVITYMILESTONE_CLASS = "org:opencrx:kernel:activity1:ActivityMilestone";
    public static final String ACTIVITYFILTERGROUP_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGroup";
    public static final String ACTIVITYFILTERGLOBAL_CLASS = "org:opencrx:kernel:activity1:ActivityFilterGlobal";
    public static final String RESOURCE_CLASS = "org:opencrx:kernel:activity1:Resource";
    public static final String RESOURCEASSIGNMENT_CLASS = "org:opencrx:kernel:activity1:ResourceAssignment";
    public static final String USERHOME_CLASS = "org:opencrx:kernel:home1:UserHome";
    public static final String AIRSYNCPROFILE_CLASS = "org:opencrx:kernel:home1:AirSyncProfile";
    public static final String CALENDARPROFILE_CLASS = "org:opencrx:kernel:home1:CalendarProfile";
    public static final String ACCOUNTFILTERGLOBAL_CLASS = "org:opencrx:kernel:account1:AccountFilterGlobal";
    public static final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
    public static final String DOCUMENTPROFILE_CLASS = "org:opencrx:kernel:home1:DocumentProfile";
    public static final String CARDPROFILE_CLASS = "org:opencrx:kernel:home1:CardProfile";
    public static final String ABSTRACTPRICELEVEL_CLASS = "org:opencrx:kernel:product1:AbstractPriceLevel";

    public static final Integer MAX_ENTRY_SELECT = 200;

    public static final String HTML_COMMENT_BEGIN = "<!-- ";
    public static final String HTML_COMMENT_END = " -->";
    public static final String PROTOCOL_SPECIFIER_HTTP = "http:";
    public static final String PROTOCOL_SPECIFIER_HTTPS = "https:";
    public static final String UNKNOWN = "_?_";
    
    private Boolean isInitialized;
	private ResourceType resourceType;
	private SelectorType selectorType;
	private String anchorObjectXri;
	private String anchorObjectLabel;
	private String anchorObjectXriFromInitialObject;
	private String urlBase;
	private Integer optionMax;
	private String optionUser;	
	private Boolean optionIsDisabled;
	private String optionSummaryPrefix;
	private String optionCategories;
	private Integer optionYear;
	private Integer optionTimelineHeight;
	private Boolean optionAlarm;
	private Map<String,String> anchorObjects;
	
}
