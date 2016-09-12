/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ActivityCollectionResource
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
package org.opencrx.application.caldav;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.generic.ChainingCollection;
import org.opencrx.kernel.utils.ActivityQueryHelper;
import org.openmdx.base.collection.MarshallingCollection;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.marshalling.Marshaller;

/**
 * ActivityCollectionResource
 *
 */
abstract class ActivityCollectionResource extends CalDavResource {
	
	public enum Type {
		VEVENT, VTODO
	}

	/**
	 * ActivityResourceCollection
	 *
	 * @param <T>
	 */
	static class ActivityResourceCollection<T> extends MarshallingCollection<T> {
		
		public ActivityResourceCollection(
			final RequestContext requestContext,
			Collection<Activity> activities,
			final ActivityCollectionResource activityCollectionResource
		) {
			super(
				new Marshaller(){

					@Override
                    public Object marshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof Activity) {
							return new ActivityResource(
								requestContext,
								(Activity)source,
								activityCollectionResource
							);
						} else {
							return source;
						}
                    }

					@Override
                    public Object unmarshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof CalDavResource) {
							return ((CalDavResource)source).getObject();
						}
						else {
							return source;
						}
                    }
					
				},
				activities
			);
		}
		
        private static final long serialVersionUID = 6257982279508324945L;

	}
	
	/**
	 * Constructor.
	 * 
	 * @param requestContext
	 * @param object
	 * @param queryHelper
	 * @param type
	 * @param allowChange
	 * @param backColor
	 * @param runAs
	 */
	public ActivityCollectionResource(
		RequestContext requestContext,
		BasicObject object,
		ActivityQueryHelper queryHelper,
		ActivityCollectionResource.Type type,
		boolean allowChange,
		boolean allowAddDelete,
		String backColor,
		String runAs
	) {
		super(
			requestContext,
			object
		);
		this.type = type;
		this.allowChange = allowChange;
		this.allowAddDelete = allowAddDelete;
		this.backColor = backColor;
        if(runAs != null) {
        	PersistenceManager pm = this.getRequestContext().newPersistenceManager(runAs);
        	queryHelper = CalDavStore.getActivityQueryHelper(
	            pm, 
	            queryHelper.getQueryId(),
	            "false"
	        );
        }
        this.queryHelper = queryHelper;
        this.runAs = runAs;
	}

	/* (non-Javadoc)
	 * @see org.opencrx.application.caldav.CalDavResource#getObject()
	 */
	@Override
    public ContextCapable getObject(
    ) {
        return super.getObject();
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.Resource#getDisplayName()
	 */
	@Override
    public String getDisplayName(
    ) {
    	Set<String> features = this.getObject().refDefaultFetchGroup();
    	String name = this.getName();
    	if(features.contains("name")) {
    		name = (String)this.getObject().refGetValue("name");
    	}
    	if(this.type == Type.VEVENT) {
    		// no suffix
    	} else {
    		name += " - Todos";
    	}
    	return name;
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.Resource#isCollection()
	 */
	@Override
    public boolean isCollection(
    ) {
		return true;
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.caldav.CalDavResource#getName()
     */
    @Override
    public String getName(
    ) {
    	return super.getName() + (this.type == Type.VTODO ? ":VTODO" : "");
    }

    /**
     * Get activity collection type.
     * 
     * @return
     */
    public ActivityCollectionResource.Type getType(
    ) {
    	return this.type;
    }
    
    /**
     * Get allow change flag.
     * 
     * @return
     */
    public boolean allowChange(
    ) {
    	return this.allowChange;
    }
    
    /**
     * Get allow add/delete flag.
     * 
     * @return
     */
    public boolean allowAddDelete(
    ) {
    	return this.allowAddDelete;
    }
    
    /**
     * Get back color.
     * 
     * @return
     */
    public String getBackColor(
    ) {
    	return this.backColor;
    }
    
    /**
     * Get runAs principal.
     * 
     * @return
     */
    public String getRunAs(
    ) {
    	return this.runAs;
    }
    
	/* (non-Javadoc)
	 * @see org.opencrx.application.caldav.CalDavResource#getMimeType()
	 */
	@Override
    public String getMimeType(
    ) {
		return ICalendar.MIME_TYPE;
    }

    /**
     * Get query helper.
     * 
     * @return
     */
    public ActivityQueryHelper getQueryHelper(
    ) {
    	return this.queryHelper;
    }

    /**
     * Get base query for children.
     * 
     * @param pm
     * @return
     */
    protected ActivityQuery getChildrenBaseQuery(
    	PersistenceManager pm
    ) {
        ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
        if(this.queryHelper.isDisabledFilter()) {
            query.thereExistsDisabled().isTrue();                    
        } else {
            query.forAllDisabled().isFalse();                    
        }
        if(this.type == Type.VEVENT) {
        	query.icalType().elementOf(ICalendar.ICAL_TYPE_VEVENT);
        } else if(this.type == Type.VTODO) {
        	query.icalType().equalTo(ICalendar.ICAL_TYPE_VTODO);
        }
        query.ical().isNonNull();
        query.forAllExternalLink().startsNotWith(ICalendar.ICAL_RECURRENCE_ID_SCHEMA);
        query.orderByCreatedAt().ascending();
        ((Query)query).getFetchPlan().setFetchSize(FETCH_SIZE);
        return query;
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.caldav.CalDavResource#getChildren()
     */
    @Override
	public Collection<Resource> getChildren(
		Date timeRangeStart,
		Date timeRangeEnd
	) {
		PersistenceManager pm = this.queryHelper.getPersistenceManager();
		List<Collection<Activity>> childrenCollections = new ArrayList<Collection<Activity>>(); 
		// Get all children within timeRange. This includes
		// recurring and non-recurring events
		{
	        ActivityQuery queryActivitiesInRange = this.getChildrenBaseQuery(pm);
	        if(timeRangeStart != null) {
	        	queryActivitiesInRange.thereExistsScheduledEnd().greaterThanOrEqualTo(timeRangeStart);
	        }
	        if(timeRangeEnd != null) {
	        	queryActivitiesInRange.thereExistsScheduledStart().lessThanOrEqualTo(timeRangeEnd);
	        }
	        childrenCollections.add(
	        	this.queryHelper.getFilteredActivities(queryActivitiesInRange)
	        );
		}
		// Recurring events typically have a scheduled end which is less
		// than timeRangeStart so they are not selected by queryActivitiesInRange. 
		// Get these events with queryRecurringActivities.
		if(timeRangeStart != null) {
	        ActivityQuery queryRecurringActivities = this.getChildrenBaseQuery(pm);
	        queryRecurringActivities.thereExistsIcal().like(".*RRULE:.*");
	        if(timeRangeEnd != null) {
	        	queryRecurringActivities.thereExistsScheduledStart().lessThanOrEqualTo(timeRangeEnd);
	        }
	        childrenCollections.add(
	        	this.queryHelper.getFilteredActivities(queryRecurringActivities)
	        );
		}
        return new ActivityResourceCollection<Resource>(
        	this.getRequestContext(),
        	new ChainingCollection<Activity>(childrenCollections),
        	this
        );
	}

	//-----------------------------------------------------------------------
    // Members
	//-----------------------------------------------------------------------
    private static final int FETCH_SIZE = 200;
    
	private final ActivityCollectionResource.Type type;
	private final boolean allowChange;
	private final boolean allowAddDelete;	
	private final String backColor;
	private final String runAs;
	private final ActivityQueryHelper queryHelper;
	
}