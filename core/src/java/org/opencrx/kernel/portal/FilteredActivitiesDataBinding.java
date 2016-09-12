/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: FilteredActivitiesDataBinding
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
package org.opencrx.kernel.portal;

import java.util.Collections;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jmi.reflect.RefObject;

import org.opencrx.kernel.activity1.cci2.ActivityCategoryQuery;
import org.opencrx.kernel.activity1.cci2.ActivityFilterGlobalQuery;
import org.opencrx.kernel.activity1.cci2.ActivityFilterGroupQuery;
import org.opencrx.kernel.activity1.cci2.ActivityMilestoneQuery;
import org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DefaultDataBinding;

/**
 * FilteredActivitiesDataBinding
 *
 */
public class FilteredActivitiesDataBinding extends DefaultDataBinding {

    /**
     * Constructor.
     * 
     */
    public FilteredActivitiesDataBinding(
    ) {
    }
    
    /**
     * Get principal name.
     * 
     * @param app
     * @return
     */
    protected String getPrincipalName(
    	ApplicationContext app
    ) {
    	String userRole = app.getCurrentUserRole(); 
        return userRole.indexOf("@") > 0 ? 
        	userRole.substring(0, userRole.indexOf("@")) : 
        		userRole;
    	   	
    }
    
    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DefaultDataBinding#getValue(javax.jmi.reflect.RefObject, java.lang.String, org.openmdx.portal.servlet.ApplicationContext)
     */
    @Override
    public Object getValue(
        RefObject object, 
        String qualifiedFeatureName,
        ApplicationContext app
    ) {
        if(qualifiedFeatureName.endsWith("!filteredActivity")) {
            String featureName = qualifiedFeatureName.substring(qualifiedFeatureName.lastIndexOf(":") + 1);
            featureName = featureName.replace("${USER}", this.getPrincipalName(app));
            if(object instanceof UserHome) {
            	featureName = featureName.replace("${HOME}", ((UserHome)object).refGetPath().getBase());
            }
            String[] c = featureName.split("!");
            String providerName = ((RefObject_1_0)object).refGetPath().get(2);
            String segmentName = ((RefObject_1_0)object).refGetPath().get(4);
            PersistenceManager pm = JDOHelper.getPersistenceManager(object);
            org.opencrx.kernel.activity1.jmi1.Segment activitySegment = 
                (org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
                    new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName, "segment", segmentName)
                );
            if(
                "tracker".equals(c[0]) ||
                "milestone".equals(c[0]) ||
                "category".equals(c[0])
            ) {
                List<? extends ActivityGroup> groups = null;
                if("tracker".equals(c[0])) {
                    ActivityTrackerQuery query = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
                    query.name().equalTo(c[1]);
                    List<org.opencrx.kernel.activity1.jmi1.ActivityTracker> trackers = activitySegment.getActivityTracker(query);
                    groups = trackers;
                }
                else if("milestone".equals(c[0])) {
                    ActivityMilestoneQuery query = (ActivityMilestoneQuery)pm.newQuery(ActivityMilestone.class);
                    query.name().equalTo(c[1]);
                    List<org.opencrx.kernel.activity1.jmi1.ActivityMilestone> milestones = activitySegment.getActivityMilestone(query);
                    groups = milestones;
                }
                else if("category".equals(c[0])) {
                    ActivityCategoryQuery query = (ActivityCategoryQuery)pm.newQuery(ActivityCategory.class);
                    query.name().equalTo(c[1]);
                    List<org.opencrx.kernel.activity1.jmi1.ActivityCategory> categories = activitySegment.getActivityCategory(query);
                    groups = categories;
                }
                if(!groups.isEmpty()) {
                    ActivityGroup group = groups.iterator().next(); 
                    if("filter".equals(c[2])) {
                        ActivityFilterGroupQuery query = (ActivityFilterGroupQuery)pm.newQuery(ActivityFilterGroup.class);
                        query.name().equalTo(c[3]);
                        List<ActivityFilterGroup> filters = group.getActivityFilter(query);
                        if(!filters.isEmpty()) {
                            return super.getValue(
                                filters.iterator().next(), 
                                "filteredActivity",
                                app
                            );                            
                        }
                    }
                    else {
                        return super.getValue(
                            groups.iterator().next(), 
                            "filteredActivity",
                            app
                        );
                    }
                }
            }
            else if("globalfilter".equals(c[0])) {
                ActivityFilterGlobalQuery query = (ActivityFilterGlobalQuery)pm.newQuery(ActivityFilterGlobal.class);
                query.name().equalTo(c[1]);
                List<ActivityFilterGlobal> filters = activitySegment.getActivityFilter(query);
                if(!filters.isEmpty()) {
                    return super.getValue(
                        filters.iterator().next(), 
                        "filteredActivity",
                        app
                    );                                        
                }                
            }
            return Collections.EMPTY_LIST;                
        }
        else {
            return super.getValue(
                object, 
                qualifiedFeatureName,
                app
            );
        }
    }
    
}
