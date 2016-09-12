/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.kernel.activity1.jmi1.AbstractFilterActivity;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.home1.jmi1.ActivityFilterCalendarFeed;
import org.opencrx.kernel.home1.jmi1.ActivityGroupCalendarFeed;
import org.opencrx.kernel.home1.jmi1.SyncFeed;
import org.opencrx.kernel.utils.ActivityQueryHelper;

class SyncFeedBasedActivityCollectionResource extends ActivityCollectionResource {

	//-----------------------------------------------------------------------
	private static ActivityQueryHelper getQueryHelper(
		SyncFeed syncFeed
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(syncFeed);
		String id = syncFeed.refGetPath().get(2) + "/" + syncFeed.refGetPath().get(4);
		if(syncFeed instanceof ActivityGroupCalendarFeed) {
			ActivityGroup activityGroup = ((ActivityGroupCalendarFeed)syncFeed).getActivityGroup();
			if(activityGroup instanceof ActivityTracker) {
				id += "/tracker/" + activityGroup.getName();
			}
			else if(activityGroup instanceof ActivityMilestone) {
				id += "/milestone/" + activityGroup.getName();
			}
			else if(activityGroup instanceof ActivityCategory) {
				id += "/category/" + activityGroup.getName();
			}
		}
		else if(syncFeed instanceof ActivityFilterCalendarFeed) {
			AbstractFilterActivity activityFilter = ((ActivityFilterCalendarFeed)syncFeed).getActivityFilter();
			if(activityFilter instanceof ActivityFilterGlobal) {
				id += "/globalfilter/" + ((ActivityFilterGlobal)activityFilter).getName();
			}
			else if(activityFilter instanceof ActivityFilterGroup){
				ActivityGroup activityGroup = (ActivityGroup)pm.getObjectById(activityFilter.refGetPath().getParent().getParent());
				if(activityGroup instanceof ActivityTracker) {
					id += "/tracker/" + activityGroup.getName();
				}
				else if(activityGroup instanceof ActivityMilestone) {
					id += "/milestone/" + activityGroup.getName();
				}
				else if(activityGroup instanceof ActivityCategory) {
					id += "/category/" + activityGroup.getName();
				}		
				id += "/filter/" + ((ActivityFilterGroup)activityFilter).getName();
			}
		}
		return CalDavStore.getActivityQueryHelper(
			pm,
			id,
			"false"
		);
	}
			
	//-----------------------------------------------------------------------
	public SyncFeedBasedActivityCollectionResource(
		RequestContext requestContext,
		SyncFeed syncFeed,
		Type type,
		String runAs
	) {
		super(
			requestContext,
			syncFeed,
			getQueryHelper(syncFeed),
			type,
			Boolean.TRUE.equals(syncFeed.isAllowChange()),
			Boolean.TRUE.equals(syncFeed.isAllowAddDelete()),
			syncFeed.getBackColor(),
			runAs
		);
	}

}