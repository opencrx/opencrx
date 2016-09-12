/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CardDavStore
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010-2013, CRIXP Corp., Switzerland
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

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.uses.net.sf.webdav.Lock;
import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.VCard;
import org.opencrx.kernel.backend.VCard.PutVCardResult;
import org.opencrx.kernel.home1.cci2.CardProfileQuery;
import org.opencrx.kernel.home1.jmi1.CardProfile;
import org.opencrx.kernel.home1.jmi1.ContactsFeed;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * CardDavStore
 *
 */
public class CardDavStore implements WebDavStore {

	/**
	 * Constructor.
	 * 
	 * @param pmf
	 */
	public CardDavStore(
		PersistenceManagerFactory pmf
	) {
		this.pmf = pmf;
	}
	
    /**
     * Get persistence manager for this store.
     * 
     * @param req
     * @param pmf
     * @return
     */
    public static PersistenceManager getPersistenceManager(
        HttpServletRequest req,
        PersistenceManagerFactory pmf
    ) {
        return req.getUserPrincipal() == null ?
            null :
            pmf.getPersistenceManager(
                req.getUserPrincipal().getName(),
                null
            );
    }
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#begin(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
	 */
	@Override
	public RequestContext begin(
    	HttpServletRequest req,
    	HttpServletResponse resp		
	) {
		CardDavRequestContext requestContext = new CardDavRequestContext(req, resp, this.pmf);
		return requestContext;
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#commit(org.opencrx.application.uses.net.sf.webdav.RequestContext)
	 */
	@Override
	public void commit(
		RequestContext requestContext
	) {
		PersistenceManager pm = ((CardDavRequestContext)requestContext).getPersistenceManager();
		try {
			if(!pm.isClosed()) {
				pm.currentTransaction().commit();
				pm.close();
			}
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#createCollection(org.opencrx.application.uses.net.sf.webdav.RequestContext, java.lang.String)
	 */
	@Override
	public void createCollection(
		RequestContext requestContext, 
		String path
	) {
		throw new WebdavException("Not supported by CalDAV");
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#getChildren(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.Resource)
	 */
	@Override
	public Collection<Resource> getChildren(
		RequestContext requestContext, 
		Resource res,
		Date timeRangeStart,
		Date timeRangeEnd
	) {
		if(res instanceof CardDavResource) {
			return ((CardDavResource)res).getChildren(timeRangeStart, timeRangeEnd);
		} else {
			return Collections.emptyList();
		}
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#getResourceContent(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.Resource)
	 */
	@Override
	public ResourceContent getResourceContent(
		RequestContext requestContext, 
		Resource res
	) {
		if(res instanceof CardDavResource) {
			return ((CardDavResource)res).getContent();
		} else {
			return new WebDavStore.ResourceContent(){
				@Override
				public BinaryLargeObject getContent() {
					return BinaryLargeObjects.valueOf(new byte[]{});
				}
				@Override
				public Long getLength() {
					return 0L;
				}
			};
		}
	}

	/**
	 * Path is of the form:
	 * - Format 1: {provider.id} "/" {segment.id} "/" ["user" "/"] {user.id} "/" ["profile" "/"] {profile.id} "/" {feed.id} "/" {account.id}
	 */
	@Override
	public Resource getResourceByPath(
		RequestContext requestContext, 
		String path
	) {
		PersistenceManager pm = ((CardDavRequestContext)requestContext).getPersistenceManager();
		if(path.startsWith("/")) {
			path = path.substring(1);
		}
		path = path.replace("/user/", "/");
		path = path.replace("/profile/", "/");		
		String[] components = path.split("/");
		// Strip extra components (sent by iOS)
		{
			int posStripped = -1;
			int pos = 0;
			for(String component: components) {
				if(component.startsWith(":") || ".well-known".equals(component)) {
					posStripped = pos;
					break;
				}
				pos++;
			}
			if(posStripped >= 0) {
				List<String> strippedComponents = new ArrayList<String>(Arrays.asList(components));
				components = strippedComponents.subList(0, posStripped).toArray(new String[posStripped]);
			}
		}		
		if(components.length >= 3) {
			UserHome userHome = (UserHome)pm.getObjectById(
    			new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", components[0], "segment", components[1], "userHome", components[2])
    		);
			if(components.length == 3) {
				if(userHome == null) {
					return null;
				} else {
		    		return new UserHomeResource(
		    			requestContext, 
		    			userHome
		    		);
				}
			}
        	CardProfile cardProfile = null;
        	String profileId = components[3];
        	try {
	    		cardProfile = (CardProfile)userHome.getSyncProfile(profileId);
        	} catch(Exception ignore) {}
        	if(cardProfile == null) {
	        	CardProfileQuery cardProfileQuery = (CardProfileQuery)pm.newQuery(CardProfile.class);
	        	cardProfileQuery.name().equalTo(profileId);
	        	List<CardProfile> cardProfiles = userHome.getSyncProfile(cardProfileQuery);
	        	cardProfile = cardProfiles.isEmpty() ? null : cardProfiles.iterator().next();
        	}
        	if(components.length == 4) {
        		if(cardProfile == null) {
        			return null;
        		} else {
		    		return new CardProfileResource(
		    			requestContext, 
		    			cardProfile
		    		);
        		}
        	} else if(components.length == 5) {
				String feedId = components[4];
				ContactsFeed contactsFeed = (ContactsFeed)cardProfile.getFeed(feedId);
	    		return new AccountCollectionResource(
	    			requestContext,
	    			contactsFeed
	    		);
			} else if(components.length == 6) {
				AccountCollectionResource parent = (AccountCollectionResource)this.getResourceByPath(
		   			requestContext, 
		   			components[0] + "/" + components[1] + "/" + components[2] + "/" + components[3] + "/" + components[4]
		   		);
				String id = components[5];
				if(id.endsWith(".vcf")) {
					id = id.substring(0, id.indexOf(".vcf"));
				}
	    		if(this.uidMapping.containsKey(id)) {
	    			String newId = this.uidMapping.get(id);
	    			// old -> new mapping only available for one request
	    			this.uidMapping.remove(id);
	    			id = newId;
	    		}			
				// Get account
	    		try {
		    		Account account = (Account)pm.getObjectById(
		    			new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", components[0], "segment", components[1], "account", id)
		    		);
		    		return new AccountResource(
		    			requestContext, 
		    			account,
		    			parent
		    		);
	    		} catch(Exception e) {
	    			ServiceException e0 = new ServiceException(e);
					SysLog.detail(e0.getMessage(), e0.getCause());
	    		}
			}
		}
		return null;		
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#removeResource(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.Resource)
	 */
	@Override
	public Status removeResource(
		RequestContext requestContext,
		String path,
		Resource res
	) {
		String parentPath = this.getParentPath(path); 
		Resource parent = this.getResourceByPath(
			requestContext, 
			parentPath
		);
		if(
			parent instanceof AccountCollectionResource && 
			res instanceof AccountResource
		) {
			AccountCollectionResource accountCollection = (AccountCollectionResource)parent;
			if(Boolean.TRUE.equals(accountCollection.getObject().isAllowAddDelete())) {
				PersistenceManager pm = ((CardDavRequestContext)requestContext).getPersistenceManager();
				try {
					Account account = ((AccountResource)res).getObject();
					MemberQuery query = (MemberQuery)pm.newQuery(Member.class);
					query.thereExistsAccount().equalTo(account);
					List<Member> members = ((AccountResource)res).getAccountCollectionResource().getObject().getAccountGroup().getMember(query);
					pm.currentTransaction().begin();
					for(Member member: members) {
						member.setDisabled(true);
					}
					pm.currentTransaction().commit();
					return Status.OK;
				} catch(Exception e) {
					new ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
					return Status.FORBIDDEN;
				}
			} else {
				return Status.FORBIDDEN;
			}
		} else {
			return Status.FORBIDDEN;
		}
	}

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#moveResource(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.Resource, java.lang.String, java.lang.String)
	 */
	@Override
    public Status moveResource(
    	RequestContext requestContext, 
    	Resource res, 
    	String sourcePath,
    	String destinationPath
    ) {
		throw new WebdavException("Not supported by CardDAV");	    
    }
		
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#rollback(org.opencrx.application.uses.net.sf.webdav.RequestContext)
	 */
	@Override
	public void rollback(
		RequestContext requestContext
	) {
		PersistenceManager pm = ((CardDavRequestContext)requestContext).getPersistenceManager();		
		try {
			if(!pm.isClosed()) {
				pm.currentTransaction().rollback();
				pm.close();
			}
		} catch(Exception e) {}
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#getMimeType(org.opencrx.application.uses.net.sf.webdav.Resource)
	 */
	@Override
    public String getMimeType(
    	Resource res
    ) {
		if(res instanceof CardDavResource) {
			return ((CardDavResource)res).getMimeType();
		} else {
			return "application/xml";
		}
    }
	
    /**
     * Get parent of given path.
     * 
     * @param path
     * @return
     */
    protected String getParentPath(
    	String path
    ) {
        int slash = path.lastIndexOf('/');
        if (slash != -1) {
            return path.substring(0, slash);
        }
        return null;
    }
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#putResource(org.opencrx.application.uses.net.sf.webdav.RequestContext, java.lang.String, java.io.InputStream, java.lang.String)
	 */
	@Override
    public Status putResource(
    	RequestContext requestContext, 
    	String path, 
    	InputStream content, 
    	String contentType 
    ) {
		String parentPath = this.getParentPath(path); 
		Resource parent = this.getResourceByPath(
			requestContext, 
			parentPath
		);
		if(parent instanceof AccountCollectionResource) {
	    	try {
		    	BufferedReader reader = new BufferedReader(
		    		new InputStreamReader(content, "UTF-8")
		    	);
		    	// Create/Update account
		    	ContactsFeed feed = ((AccountCollectionResource)parent).getObject();
		    	if(feed.isAllowChange()) {
			    	AbstractGroup group = feed.getAccountGroup();		    	
			    	PersistenceManager pm = JDOHelper.getPersistenceManager(group);
			    	org.opencrx.kernel.account1.jmi1.Segment accountSegment =
			    		(org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
			    			group.refGetPath().getParent().getParent()
			    		);
		        	VCard.PutVCardResult result = VCard.getInstance().putVCard(
		        		reader, 
		        		accountSegment
		        	);
		        	if(result.getStatus() != PutVCardResult.Status.ERROR) {
			        	// Create membership
			        	if(result.getAccount() != null) {
			        		MemberQuery query = (MemberQuery)pm.newQuery(Member.class);
			        		query.thereExistsAccount().equalTo(result.getAccount());
			        		List<Member> members = group.getMember(query);
			        		if(members.isEmpty()) {
			        			boolean isTxLocal = !pm.currentTransaction().isActive();
			        			if(isTxLocal) {
			        				pm.currentTransaction().begin();
			        			}
			        			Member member = pm.newInstance(Member.class);
								member.setName(result.getAccount().getFullName());
								member.setAccount(result.getAccount());
								member.setQuality(Accounts.MEMBER_QUALITY_NORMAL);
								group.addMember(
									Base.getInstance().getUidAsString(), 
									member
								);
								if(isTxLocal) {
									pm.currentTransaction().commit();
								}
			        		}
		 	        	}
			            if(result.getOldUID() != null && result.getAccount() != null) {
			            	this.uidMapping.put(
			            		result.getOldUID(), 
			            		result.getAccount().refGetPath().getLastSegment().toClassicRepresentation()
			            	);
			            }
		        	}
		            switch(result.getStatus()) {
			            case CREATED: return Status.OK_CREATED;
			            case UPDATED: return Status.OK;
			            case ERROR: return Status.FORBIDDEN;
		            }
		    	} else {
		    		return Status.FORBIDDEN;
		    	}
	    	} catch(Exception e) {
	    		new ServiceException(e).log();
	    		return Status.FORBIDDEN;
	    	}
		}
	    return Status.FORBIDDEN;
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#getLocksByPath(org.opencrx.application.uses.net.sf.webdav.RequestContext, java.lang.String)
	 */
	@Override
    public List<Lock> getLocksByPath(
    	RequestContext requestContext, 
    	String path
    ) {
		return Collections.emptyList();
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#lock(org.opencrx.application.uses.net.sf.webdav.RequestContext, java.lang.String, java.lang.String, java.lang.String, java.lang.String, int, int)
	 */
	@Override
    public Lock lock(
    	RequestContext requestContext, 
    	String path,
    	String id,
    	String owner, 
    	String scope, 
    	String type, 
    	int depth, 
    	int timeout
    ) throws LockFailedException {
	    return null;
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#unlock(org.opencrx.application.uses.net.sf.webdav.RequestContext, java.lang.String)
	 */
	@Override
    public boolean unlock(
    	RequestContext requestContext,
    	String path,
    	String id
    ) {
	    return false;
    }

	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------			
	protected PersistenceManagerFactory pmf = null;
    protected final Map<String,String> uidMapping = new ConcurrentHashMap<String,String>();
	
}