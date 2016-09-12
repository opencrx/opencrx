/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AssignedActivityGroupsDataBinding
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2008, CRIXP Corp., Switzerland
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.jmi.reflect.RefObject;

import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DataBinding;

public class AssignedActivityGroupsDataBinding extends DataBinding {

	enum ActivityGroupType {
		TRACKER,
		MILESTONE,
		CATEGORY
	}
	
	/**
	 * @param parameterString
	 */
	public AssignedActivityGroupsDataBinding(
		String parameterString
	) {
		if(parameterString != null && parameterString.startsWith("type=")) {
			this.activityGroupTypesFilter = new HashSet<ActivityGroupType>();
			String[] types = parameterString.substring(5).split(",");
			for(String type: types) {
				if(type.equalsIgnoreCase(ActivityGroupType.TRACKER.toString())) {
					this.activityGroupTypesFilter.add(ActivityGroupType.TRACKER);
				}
				if(type.equalsIgnoreCase(ActivityGroupType.MILESTONE.toString())) {
					this.activityGroupTypesFilter.add(ActivityGroupType.MILESTONE);
				}
				if(type.equalsIgnoreCase(ActivityGroupType.CATEGORY.toString())) {
					this.activityGroupTypesFilter.add(ActivityGroupType.CATEGORY);
				}
			}
		}
		else {
			this.activityGroupTypesFilter = new HashSet<ActivityGroupType>(
				Arrays.asList(
					ActivityGroupType.TRACKER, 
					ActivityGroupType.MILESTONE, 
					ActivityGroupType.CATEGORY
				)
			);			
		}
	}
	
	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.DataBinding#getValue(javax.jmi.reflect.RefObject, java.lang.String, org.openmdx.portal.servlet.ApplicationContext)
	 */
	@Override
    public Object getValue(
        RefObject object, 
        String qualifiedFeatureName,
        ApplicationContext app
    ) {
        if(object instanceof org.opencrx.kernel.activity1.jmi1.Activity) {
            org.opencrx.kernel.activity1.jmi1.Activity activity = (org.opencrx.kernel.activity1.jmi1.Activity)object;
            List<org.opencrx.kernel.activity1.jmi1.ActivityGroup> groups = new ArrayList<org.opencrx.kernel.activity1.jmi1.ActivityGroup>();
            Collection<org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment> assignments = activity.getAssignedGroup();
            for(org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment assignment: assignments) {
                if(assignment.getActivityGroup() != null) {
                	ActivityGroup group = assignment.getActivityGroup();
                	if(
                		((group instanceof ActivityTracker) && this.activityGroupTypesFilter.contains(ActivityGroupType.TRACKER)) ||
                		((group instanceof ActivityMilestone) && this.activityGroupTypesFilter.contains(ActivityGroupType.MILESTONE)) ||
                		((group instanceof ActivityCategory) && this.activityGroupTypesFilter.contains(ActivityGroupType.CATEGORY))
                	) {
                		groups.add(group);
                	}
                }
            }
            return groups;
        }
        return null;
    }

    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DataBinding#setValue(javax.jmi.reflect.RefObject, java.lang.String, java.lang.Object, org.openmdx.portal.servlet.ApplicationContext)
     */
    public void setValue(
        RefObject object, 
        String qualifiedFeatureName, 
        Object newValue,
        ApplicationContext app
    ) {
    }

    private final Set<ActivityGroupType> activityGroupTypesFilter;
    
}
