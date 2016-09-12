/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CalDavStore
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
package org.opencrx.application.caldav;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;

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
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.home1.cci2.CalendarProfileQuery;
import org.opencrx.kernel.home1.jmi1.CalendarProfile;
import org.opencrx.kernel.home1.jmi1.SyncFeed;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.ActivityQueryHelper;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * CalDavStore
 *
 */
public class CalDavStore implements WebDavStore {

	/**
	 * Constructor.
	 * 
	 * @param pmf
	 */
	public CalDavStore(
		PersistenceManagerFactory pmf
	) {
		this.pmf = pmf;
	}
	
    /**
     * Get new, user-specific persistence manager for this request.
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
	
	/**
	 * Get query helper for given filter id.
	 * 
	 * @param pm
	 * @param filterId
	 * @param isDisabledFilter
	 * @return
	 */
	public static ActivityQueryHelper getActivityQueryHelper(
        PersistenceManager pm,
        String filterId,
        String isDisabledFilter
    ) {
        ActivityQueryHelper activitiesHelper = new ActivityQueryHelper(pm);
        if(filterId != null) {
            try {
                activitiesHelper.parseQueryId(                        
                    (filterId.startsWith("/") ? "" : "/") + filterId
                );
                activitiesHelper.parseDisabledFilter(
                   isDisabledFilter
                );
            }
            catch(Exception  e) {}
        }        
        return activitiesHelper;
    }
    		
	/**
	 * Path is of the form:
	 * - Format 1: {provider.id} "/" {segment.id} ["/user/"] {user.id} ["/profile/"] {profile.id}]
	 * - Format 2: {provider.id} "/" {segment.id} "/" {user.id} "/" {profile.id} "/" {feed.id} [":VTODO"] "/" {activity.id}
	 * - Format 3: {provider.id} "/" {segment.id} ["/user/" {user.id} ] "/" {tracker|milestone|category|home|resource|filter} "/" {calendar.name} ["/filter/" {filter.name}] ["/VTODO"] "/" {activity.id}
	 * 	
	 * @param requestContext
	 * @param path
	 * @param allowRunAs
	 * @return
	 */
	protected Resource getResourceByPath(
		RequestContext requestContext, 
		String path,
		boolean allowRunAs
	) {
		PersistenceManager pm = ((CalDavRequestContext)requestContext).getPersistenceManager();
		HttpServletRequest req = requestContext.getHttpServletRequest();
		if(this.pathMapping.containsKey(path)) {
			String mappedPath = this.pathMapping.get(path);
			this.pathMapping.remove(path);
			path = mappedPath;
		}
		if(path.startsWith("/")) {
			path = path.substring(1);
		}
		// Normalize path
		path = path.replace("/user/", "/");
		path = path.replace("/profile/", "/");
		String[] components = path.split("/");
		if(components.length < 3) {
			components = new String[]{components[0], components[1], req.getUserPrincipal().getName()};
		}
		if(components.length < 4) {
			// use default profile name DEFAULT_PROFILE_NAMES[0]
			components = new String[]{components[0], components[1], components[2], DEFAULT_PROFILE_NAMES[0]};			
		}
		// Format 3
		if(
			CALENDAR_TYPES.contains(components[2]) || 
			CALENDAR_TYPES.contains(components[3])
		) {
			String runAs = null;
			if(CALENDAR_TYPES.contains(components[3])) {
				String userId = components[2];
				if(allowRunAs && !userId.equals(req.getUserPrincipal().getName())) {
					runAs = userId;
				}
				path = components[0] + "/" + components[1];
				for(int i = 3; i < components.length; i++) {
					path += "/" + components[i];
				}
			}
			String resourceId = null;
			if(path.endsWith(".ics")) {
				int pos = path.lastIndexOf("/");
				if(pos > 0) {
					resourceId = path.substring(pos + 1);
					path = path.substring(0, pos);
				}
			}
			ActivityCollectionResource.Type type = ActivityCollectionResource.Type.VEVENT; 
			if(path.indexOf("/VTODO") > 0) {
				path = path.substring(0, path.indexOf("/VTODO"));
				type = ActivityCollectionResource.Type.VTODO;
			}
			ActivityQueryHelper queryHelper = CalDavStore.getActivityQueryHelper(
				pm, 
				path, 
				"false"
			);
			if(queryHelper.getSource() == null) {
				SysLog.log(Level.FINE, "Unable to get query helper for user >{0}< and path >{1}<", req.getUserPrincipal().getName(), Arrays.asList(components));
				return null;
			} else {
				ActivityCollectionResource activityCollectionResource = new QueryBasedActivityCollectionResource(
					requestContext,
					queryHelper,
					type,
					runAs
				);
				if(resourceId != null) {
		    		try {
			    		Activity activity = (Activity)activityCollectionResource.getQueryHelper().getPersistenceManager().getObjectById(
			    			new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", components[0], "segment", components[1], "activity", resourceId.endsWith(".ics") ? resourceId.substring(0, resourceId.length() - 4) : resourceId)
			    		);	    		
						return new ActivityResource(
							requestContext,
							activity,
							activityCollectionResource
						);
		    		} catch(Exception e) {
		    			ServiceException e0 = new ServiceException(e);
		    			SysLog.detail(e0.getMessage(), e0.getCause());
		    		}
				} else {
					return activityCollectionResource;
				}
			}
		} else {
			// Format 1, 2
			String userId = components[2];
    		UserHome userHome = (UserHome)pm.getObjectById(
    			new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", components[0], "segment", components[1], "userHome", userId)
    		);
        	CalendarProfile syncProfile = null;
        	// Find sync profile by qualifier
        	try {
        		syncProfile = (CalendarProfile)pm.getObjectById(
        			userHome.refGetPath().getDescendant("syncProfile", components[3])
        		);
        	} catch(Exception ignore) {}
        	// Find sync profile by name
        	if(syncProfile == null) {
        		List<String> profileNames = new ArrayList<String>();
        		profileNames.add(components[3]);
        		profileNames.addAll(Arrays.asList(DEFAULT_PROFILE_NAMES));
        		// Try with default profile names
        		for(String profileName: profileNames) {
        			CalendarProfileQuery calendarProfileQuery = (CalendarProfileQuery)pm.newQuery(CalendarProfile.class);
		        	calendarProfileQuery.name().equalTo(profileName);
		        	List<CalendarProfile> calendarProfiles = userHome.getSyncProfile(calendarProfileQuery);
		        	if(!calendarProfiles.isEmpty()) {
		        		syncProfile = calendarProfiles.iterator().next();
		        		break;
		        	}
        		}
        	}
        	if(syncProfile != null) {
	    		String runAs = null;
	    		if(!userId.equals(req.getUserPrincipal().getName())) {
	    			runAs = userId;
	    		}
        		if(components.length == 4) {
    	    		return new CalendarProfileResource(
    	    			requestContext, 
    	    			syncProfile,
    	    			runAs
    	    		);        			
    			} else if(components.length == 5) {
    				// SyncFeed
    				String id = components[4];
    				ActivityCollectionResource.Type type = ActivityCollectionResource.Type.VEVENT; 
    				if(id.endsWith(":VTODO")) {
    					id = id.substring(0, id.indexOf(":VTODO"));
    					type = ActivityCollectionResource.Type.VTODO;
    				}
    	    		SyncFeed syncFeed = syncProfile.getFeed(id);
    	    		if(syncFeed == null) {
    	    			return null;
    	    		} else {
	    	    		return new SyncFeedBasedActivityCollectionResource(
	    	    			requestContext, 
	    	    			syncFeed,
	    	    			type,
	    	    			runAs
	    	    		);
    	    		}
    			} else if(components.length == 6) {
    				// Activity
    				ActivityCollectionResource parent = (ActivityCollectionResource)this.getResourceByPath(
    		   			requestContext, 
    		   			components[0] + "/" + components[1] + "/" + components[2] + "/" + components[3] + "/" + components[4]
    		   		);
    				String id = components[5];
    				if(id.endsWith(".ics")) {
    					id = id.substring(0, id.indexOf(".ics"));
    				}
    				// Get activity
    	    		try {
    		    		Activity activity = (Activity)pm.getObjectById(
    		    			new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", components[0], "segment", components[1], "activity", id)
    		    		);
    		    		return new ActivityResource(
    		    			requestContext, 
    		    			activity,
    		    			parent
    		    		);
    	    		} catch(Exception e) {
    	    			ServiceException e0 = new ServiceException(e);
    					SysLog.detail(e0.getMessage(), e0.getCause());
    	    		}
    			}
        	}
		}
		return null;		
	}

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#begin(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
	 */
	@Override
	public RequestContext begin(
    	HttpServletRequest req,
    	HttpServletResponse resp		
	) {
		CalDavRequestContext requestContext = new CalDavRequestContext(req, resp, this.pmf);
		return requestContext;
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#commit(org.opencrx.application.uses.net.sf.webdav.RequestContext)
	 */
	@Override
	public void commit(
		RequestContext requestContext
	) {
		PersistenceManager pm = ((CalDavRequestContext)requestContext).getPersistenceManager();
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
		if(res instanceof CalDavResource) {
			return ((CalDavResource)res).getChildren(timeRangeStart, timeRangeEnd);
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
		if(res instanceof CalDavResource) {
			return ((CalDavResource)res).getContent();
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

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#getResourceByPath(org.opencrx.application.uses.net.sf.webdav.RequestContext, java.lang.String)
	 */
	@Override
	public Resource getResourceByPath(
		RequestContext requestContext, 
		String path
	) {
		return this.getResourceByPath(
			requestContext, 
			path, 
			true // allowRunAs
		);
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
			parentPath,
			false // allowRunAs
		);
		if(
			parent instanceof ActivityCollectionResource && 
			res instanceof ActivityResource
		) {
			ActivityCollectionResource activityCollectionRes = (ActivityCollectionResource)parent;
			if(activityCollectionRes.allowAddDelete()) {
				PersistenceManager pm = ((CalDavRequestContext)requestContext).getPersistenceManager();
				try {
					pm.currentTransaction().begin();
					((ActivityResource)res).getObject().setDisabled(true);
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
		try {
			Status status = this.putResource(
				requestContext, 
				destinationPath,
				this.getResourceContent(requestContext, res).getContent().getContent(),
				this.getMimeType(res)
			);
			if(status != Status.FORBIDDEN) {
				String mappedPath = pathMapping.get(destinationPath);
				this.removeResource(
					requestContext, 
					sourcePath, 
					res
				);
				// Refresh path mapping in case it was removed by removeSource()
				if(mappedPath != null) {
					pathMapping.put(
						destinationPath,
						mappedPath
					);
				}
			}
			return status;
		} catch(Exception e) {
			new ServiceException(e).log();
			return Status.FORBIDDEN;
		}
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.WebDavStore#rollback(org.opencrx.application.uses.net.sf.webdav.RequestContext)
	 */
	@Override
	public void rollback(
		RequestContext requestContext
	) {
		PersistenceManager pm = ((CalDavRequestContext)requestContext).getPersistenceManager();		
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
		if(res instanceof CalDavResource) {
			return ((CalDavResource)res).getMimeType();
		} else {
			return "application/xml";
		}
    }
	
    /**
     * Get parent element of path.
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
	
    /**
     * Get base of given path.
     * 
     * @param path
     * @return
     */
    protected String getBasePath(
    	String path
    ) {
    	int slash = path.lastIndexOf('/');
    	if(slash != -1) {
    		return path.substring(slash + 1);
    	} else {
    		return null;
    	}
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
		if(this.pathMapping.containsKey(path)) {
			String mappedPath = this.pathMapping.get(path);
			this.pathMapping.remove(path);
			path = mappedPath;
		}
		String parentPath = this.getParentPath(path); 
		Resource parent = this.getResourceByPath(
			requestContext, 
			parentPath,
			false // allowRunAs
		);
		if(parent instanceof ActivityCollectionResource) {
			ActivityCollectionResource activityCollectionRes = (ActivityCollectionResource)parent;
			if(activityCollectionRes.allowChange()) {
		    	try {
			    	BufferedReader reader = new BufferedReader(
			    		new InputStreamReader(content, "UTF-8")
			    	);
	            	String resId = this.getBasePath(path);
	            	if(resId != null && resId.endsWith(".ics")) {
	            		resId = resId.substring(0, resId.length() - 4);
	            	}
		        	ICalendar.PutICalResult result = ICalendar.getInstance().putICal(
		        		reader, 
		        		activityCollectionRes.getQueryHelper(),
		        		true,
		        		resId
		        	);
		            if(result.getActivity() != null) {
		            	if(resId != null) {
			            	this.pathMapping.put(
			            		path, 
			            		path.replace(resId, result.getActivity().refGetPath().getLastSegment().toClassicRepresentation())
			            	);
		            	}
		            }
		            switch(result.getStatus()) {
		            	case CREATED: return Status.OK_CREATED;
		            	case UPDATED: return Status.OK;
		            	case ERROR: return Status.FORBIDDEN;
		            }
		    	} catch(Exception e) {
		    		// FORBIDDEN in case of a generic error
		    		new ServiceException(e).log();
	    			return Status.FORBIDDEN;
		    	}
			} else {
				return Status.FORBIDDEN;
			}
		}
	    return null;
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
	protected static final Set<String> CALENDAR_TYPES =
		new HashSet<String>(Arrays.asList("tracker", "milestone", "category", "home", "resource", "filter", "globalfilter"));
	public static final String[] DEFAULT_PROFILE_NAMES = new String[]{"CalDAV", "Calendars", "Calendar"};
	protected PersistenceManagerFactory pmf = null;
	// Path mappings are required for some clients such as iOS, Lightning. These clients perform 
	// a PUT / GET immediately after having created a new resource ignoring a UID possibly rewritten by
	// the server.
    protected final Map<String,String> pathMapping = new ConcurrentHashMap<String,String>();
	
}