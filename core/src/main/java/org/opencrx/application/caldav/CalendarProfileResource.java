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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.kernel.home1.jmi1.ActivityFilterCalendarFeed;
import org.opencrx.kernel.home1.jmi1.ActivityGroupCalendarFeed;
import org.opencrx.kernel.home1.jmi1.CalendarProfile;
import org.opencrx.kernel.home1.jmi1.SyncFeed;
import org.opencrx.kernel.home1.jmi1.SyncProfile;

class CalendarProfileResource extends CalDavResource {
	
	public CalendarProfileResource(
		RequestContext requestContext,
		CalendarProfile syncProfile,
		String runAs
	) {
		super(requestContext, syncProfile);
		this.runAs = runAs;
	}

	@Override
    public SyncProfile getObject(
    ) {
        return (SyncProfile)super.getObject();
    }

	public String getDisplayName(
	) {
		return this.getObject().getName();
	}
	
	@Override
    public boolean isCollection(
    ) {
		return true;
    }
	
	@Override
	public Collection<Resource> getChildren(
		Date timeRangeStart,
		Date timeRangeEnd
	) {
		SyncProfile syncProfile = this.getObject();
		Collection<Resource> children = new ArrayList<Resource>();
		Collection<SyncFeed> syncFeeds = syncProfile.getFeed();
		for(SyncFeed syncFeed: syncFeeds) {
			if(
				Boolean.TRUE.equals(syncFeed.isActive()) && 
				(syncFeed instanceof ActivityGroupCalendarFeed || syncFeed instanceof ActivityFilterCalendarFeed)
			) {
				children.add(
					new SyncFeedBasedActivityCollectionResource(
						this.getRequestContext(),
						syncFeed,
						ActivityCollectionResource.Type.VEVENT,
						runAs
					)
				);
				children.add(
					new SyncFeedBasedActivityCollectionResource(
						this.getRequestContext(),
						syncFeed,
						ActivityCollectionResource.Type.VTODO,
						runAs
					)
				);
			}
		}
		return children;
	}

	private final String runAs;
}