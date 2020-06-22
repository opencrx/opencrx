/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AccountCollectionResource
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
package org.opencrx.application.carddav;

import java.util.Collection;
import java.util.Date;
import java.util.Set;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.home1.jmi1.ContactsFeed;
import org.openmdx.base.collection.MarshallingCollection;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.marshalling.Marshaller;

/**
 * AccountCollectionResource
 *
 */
public class AccountCollectionResource extends CardDavResource {
	
	/**
	 * Marshaling collection which maps AccountMembers to AccountResources.
	 *
	 * @param <T>
	 */
	static class AccountResourceCollection<T> extends MarshallingCollection<T> {
		
		public AccountResourceCollection(
			final RequestContext requestContext,
			Collection<Member> members,
			final AccountCollectionResource accountCollectionResource
		) {
			super(
				new Marshaller(){

					@Override
                    public Object marshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof Member) {
							return new AccountResource(
								requestContext,
								((Member)source).getAccount(),
								accountCollectionResource
							);
						} else {
							return source;
						}
                    }

					@Override
                    public Object unmarshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof CardDavResource) {
							return ((CardDavResource)source).getObject();
						}
						else {
							return source;
						}
                    }
					
				},
				members
			);
		}
		
        private static final long serialVersionUID = 6257982279508324945L;

	}
	
	/**
	 * Constructor
	 * @param requestContext
	 * @param contactsFeed
	 */
	public AccountCollectionResource(
		RequestContext requestContext,
		ContactsFeed contactsFeed
	) {
		super(
			requestContext,
			contactsFeed
		);
	}

	/* (non-Javadoc)
	 * @see org.opencrx.application.carddav.CardDavResource#getObject()
	 */
	@Override
    public ContactsFeed getObject(
    ) {
        return (ContactsFeed)super.getObject();
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
     * @see org.opencrx.application.carddav.CardDavResource#getName()
     */
    @Override
    public String getName(
    ) {
    	return super.getName();
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.carddav.CardDavResource#getMimeType()
	 */
	@Override
    public String getMimeType(
    ) {
		return ICalendar.MIME_TYPE;
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.carddav.CardDavResource#getChildren()
     */
    @Override
	public Collection<Resource> getChildren(
		Date timeRangeStart,
		Date timeRangeEnd
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(this.getObject());
        MemberQuery query = (MemberQuery)pm.newQuery(Member.class);
        query.forAllDisabled().isFalse();                    
        query.thereExistsAccount().vcard().isNonNull();
        query.orderByCreatedAt().ascending();
        ((Query)query).getFetchPlan().setFetchSize(FETCH_SIZE);
        return new AccountResourceCollection<Resource>(
        	this.getRequestContext(),
        	this.getObject().getAccountGroup().getMember(query),
        	this
        );
	}
	
	//-----------------------------------------------------------------------
    // Members
	//-----------------------------------------------------------------------
    private static final int FETCH_SIZE = 200;    
	
}