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
package org.opencrx.application.carddav;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.kernel.home1.cci2.ContactsFeedQuery;
import org.opencrx.kernel.home1.jmi1.CardProfile;
import org.opencrx.kernel.home1.jmi1.ContactsFeed;
import org.opencrx.kernel.home1.jmi1.SyncProfile;

class CardProfileResource extends CardDavResource {
	
	public CardProfileResource(
		RequestContext requestContext,
		CardProfile syncProfile
	) {
		super(requestContext, syncProfile);
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
		PersistenceManager pm = JDOHelper.getPersistenceManager(syncProfile);
		ContactsFeedQuery query = (ContactsFeedQuery)pm.newQuery(ContactsFeed.class);
		query.thereExistsIsActive().isTrue();
		Collection<Resource> children = new ArrayList<Resource>();
		Collection<ContactsFeed> contactFeeds = syncProfile.getFeed(query);
		for(ContactsFeed contactsFeed: contactFeeds) {
			children.add(
				new AccountCollectionResource(
					this.getRequestContext(),
					contactsFeed
				)
			);
		}
		return children;
	}

}