/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ConnectionHelper
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.application.utils;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.application.ical.ICalServlet.CalendarType;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.home1.cci2.SyncFeedQuery;
import org.opencrx.kernel.home1.jmi1.AccountFilterFeed;
import org.opencrx.kernel.home1.jmi1.ActivityFilterCalendarFeed;
import org.opencrx.kernel.home1.jmi1.ActivityGroupCalendarFeed;
import org.opencrx.kernel.home1.jmi1.CalendarProfile;
import org.opencrx.kernel.home1.jmi1.CardProfile;
import org.opencrx.kernel.home1.jmi1.ContactsFeed;
import org.opencrx.kernel.home1.jmi1.DocumentProfile;
import org.opencrx.kernel.home1.jmi1.SyncFeed;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;

/**
 * AdapterConnectionHelper
 *
 */
public class AdapterConnectionHelper {

	/**
	 * ConnectionURL
	 *
	 */
	public static class ConnectionURL {
		
		public ConnectionURL(
			URL url,
			RefObject_1_0 object
		) {
			this.url = url;
			this.object = object;
		}
		
		/**
		 * @return the description
		 */
		public RefObject_1_0 getObject() {
			return object;
		}
		/**
		 * @param description the description to set
		 */
		public void setObject(RefObject_1_0 object) {
			this.object = object;
		}
		/**
		 * @return the url
		 */
		public URL getUrl() {
			return url;
		}
		/**
		 * @param url the url to set
		 */
		public void setUrl(URL url) {
			this.url = url;
		}

		private RefObject_1_0 object;
		private URL url;

	}

	/**
	 * ResourcePath
	 *
	 */
	public static class ResourcePath {
		
		public ResourcePath(
			RefObject_1_0 object,
			String path
		) {
			this.object = object;
			this.path = path;
		}
		
		/**
		 * @return the object
		 */
		public RefObject_1_0 getObject() {
			return object;
		}
		/**
		 * @param object the object to set
		 */
		public void setObject(RefObject_1_0 object) {
			this.object = object;
		}
		/**
		 * @return the path
		 */
		public String getPath() {
			return path;
		}
		/**
		 * @param path the path to set
		 */
		public void setPath(String path) {
			this.path = path;
		}

		private RefObject_1_0 object;
		private String path;
		
	}
	
	enum PathType {
		SYNCFEED,
		ORIGIN
	}

	/**
	 * Map paths to URLs.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @param servletType
	 * @param paths
	 * @return
	 * @throws ServiceException
	 */
	private static List<ConnectionURL> mapToURLs(
		String baseUrl,
		RefObject_1_0 obj,
		String servletType,
		List<ResourcePath> paths
	) throws ServiceException {
		try {
			String providerName = obj.refGetPath().getSegment(2).toString();	
			String segmentName = obj.refGetPath().getSegment(4).toString();
			List<ConnectionURL> urls = new ArrayList<ConnectionURL>();
			for(ResourcePath path: paths) {
				urls.add(
					new ConnectionURL(
						new URL(baseUrl.replace("-core-", "-" + servletType + "-") + providerName + "/" + segmentName + path.getPath()),
						path.getObject()
					)
				);
			}
			return urls;
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get calendar paths for given object.
	 *
	 * @param obj
	 * @param isCollectionTypeTask
	 * @param suffix
	 * @return
	 * @throws ServiceException
	 */
	private static List<ResourcePath> getCalendarPaths(
		RefObject_1_0 obj,
		boolean isCollectionTypeTask,
		String suffix,
		PathType pathType
	) throws ServiceException {
		try {
			PersistenceManager pm = JDOHelper.getPersistenceManager(obj);
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
			if(obj instanceof ActivityFilterGroup) {
				ActivityFilterGroup activityFilterGroup = (ActivityFilterGroup)obj;
				if((activityFilterGroup.getName() != null) && !activityFilterGroup.getName().isEmpty()) {
					ActivityGroup group = (ActivityGroup)pm.getObjectById(activityFilterGroup.refGetPath().getParent().getParent());
					if(group instanceof ActivityTracker) {
						if(pathType == PathType.ORIGIN) {
							paths.add(
								new ResourcePath(
									group,
									"/tracker/" + group.getName() + "/filter/" + activityFilterGroup.getName() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
								)
							);
						}
					} else if(group instanceof ActivityMilestone) {
						if(pathType == PathType.ORIGIN) {
							paths.add(
								new ResourcePath(
									group,
									"/milestone/" +  group.getName() + "/filter/" + activityFilterGroup.getName() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
								)
							);
						}
					} else if(group instanceof ActivityCategory) {
						if(pathType == PathType.ORIGIN) {
							paths.add(
								new ResourcePath(
									group,
									"/category/" +  group.getName() + "/filter/" + activityFilterGroup.getName() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
								)
							);
						}
					}
				}
			} else if(obj instanceof ActivityFilterGlobal) {
				ActivityFilterGlobal activityFilterGlobal = (ActivityFilterGlobal)obj;
				if((activityFilterGlobal.getName() != null) && !activityFilterGlobal.getName().isEmpty()) {
					if(pathType == PathType.ORIGIN) {
						paths.add(
							new ResourcePath(
								activityFilterGlobal,
								"/globalfilter/" + activityFilterGlobal.getName() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
							)
						);
					}
				}
			} else if(obj instanceof ActivityTracker) {
				ActivityTracker activityTracker = (ActivityTracker)obj;
				if((activityTracker.getName() != null) && !activityTracker.getName().isEmpty()) {
					if(pathType == PathType.ORIGIN) {
						paths.add(
							new ResourcePath(
								activityTracker,
								"/tracker/" + activityTracker.getName() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
							)
						);
					}
				}
			} else if(obj instanceof ActivityCategory) {
				ActivityCategory activityCategory = (ActivityCategory)obj;
				if((activityCategory.getName() != null) && !activityCategory.getName().isEmpty()) {
					if(pathType == PathType.ORIGIN) {
						paths.add(
							new ResourcePath(
								activityCategory,
								"/category/" + activityCategory.getName() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
							)
						);
					}
				}
			} else if(obj instanceof ActivityMilestone) {
				ActivityMilestone activityMilestone = (ActivityMilestone)obj;
				if((activityMilestone.getName() != null) && !activityMilestone.getName().isEmpty()) {
					if(pathType == PathType.ORIGIN) {
						paths.add(
							new ResourcePath(
								activityMilestone,
								"/milestone/" + activityMilestone.getName() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
							)
						);
					}
				}
			} else if(obj instanceof CalendarProfile) {
				CalendarProfile calendarProfile = (CalendarProfile)obj;
				SyncFeedQuery syncFeedQuery = (SyncFeedQuery)pm.newQuery(SyncFeed.class);
				syncFeedQuery.orderByName().ascending();
				syncFeedQuery.forAllIsActive().isTrue();
				for(SyncFeed feed: calendarProfile.<SyncFeed>getFeed(syncFeedQuery)) {
					if(Boolean.TRUE.equals(feed.isActive())) {
						if(feed instanceof ActivityFilterCalendarFeed) {
							try {
								paths.addAll(
									getCalendarPaths(
										((ActivityFilterCalendarFeed)feed).getActivityFilter(),
										isCollectionTypeTask,
										suffix,
										pathType
									)
								);
							} catch(Exception ignore) {
								// In case of AUTHORIZATION_FAILUREs
							}
						} else if(feed instanceof ActivityGroupCalendarFeed) {
							try {
								paths.addAll(
									getCalendarPaths(
										((ActivityGroupCalendarFeed)feed).getActivityGroup(),
										isCollectionTypeTask,
										suffix,
										pathType
									)
								);
							} catch(Exception ignore) {
								// In case of AUTHORIZATION_FAILUREs								
							}
						}
						if(pathType == PathType.SYNCFEED) {
							paths.add(
								new ResourcePath(
									feed,
									"/user/" + calendarProfile.refGetPath().getSegment(6).toString() + "/profile/" + calendarProfile.refGetPath().getSegment(8).toString() + "/" + feed.refGetPath().getLastSegment().toString() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
								)
							);
						}
					}
				}
			} else if(obj instanceof UserHome) {
				paths.add(
					new ResourcePath(
						obj,
						"/home/" + obj.refGetPath().getLastSegment().toString() + (isCollectionTypeTask ? "/VTODO" : "") + suffix
					)
				);
			}
			return paths;
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get card paths for given object.
	 * 
	 * @param obj
	 * @param suffix
	 * @return
	 * @throws ServiceException
	 */
	private static List<ResourcePath> getCardPaths(
		RefObject_1_0 obj,
		String suffix,
		PathType pathType
	) throws ServiceException {
		try {
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
		    if(obj instanceof org.opencrx.kernel.account1.jmi1.AccountFilterGlobal) {
		    	org.opencrx.kernel.account1.jmi1.AccountFilterGlobal accountFilterGlobal =
		    		(org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)obj;
		    	if((accountFilterGlobal.getName() != null) && !accountFilterGlobal.getName().isEmpty()) {
		    		if(pathType == PathType.ORIGIN) {
			    		paths.add(
			    			new ResourcePath(
			    				accountFilterGlobal,
			    				"/filter/" + accountFilterGlobal.getName() + suffix
			    			)
			    		);
		    		}
		    	}
		    } else if(obj instanceof org.opencrx.kernel.account1.jmi1.AbstractGroup) {
		    	org.opencrx.kernel.account1.jmi1.AbstractGroup accountGroup =
		    		(org.opencrx.kernel.account1.jmi1.AbstractGroup)obj;
		    	if((accountGroup.getName() != null) && !accountGroup.getName().isEmpty()) {
		    		if(pathType == PathType.ORIGIN) {
			    		paths.add(
			    			new ResourcePath(
			    				accountGroup,
			    				"/group/" + accountGroup.getName() + suffix
			    			)
			    		);
		    		}
		    	}
		    } else if(obj instanceof CardProfile) {
		    	CardProfile cardProfile = (CardProfile)obj;
				for(SyncFeed feed: cardProfile.<SyncFeed>getFeed()) {
					if(Boolean.TRUE.equals(feed.isActive())) {
						if(feed instanceof ContactsFeed) {
							try {
								paths.addAll(
									getCardPaths(
										((ContactsFeed)feed).getAccountGroup(),
										suffix,
										pathType
									)
								);
							} catch(Exception igore) {
								// In case of AUTHORIZATION_FAILUREs							
							}
						} else if (feed instanceof AccountFilterFeed) {
							try {
								paths.addAll(
									getCardPaths(
										((AccountFilterFeed)feed).getAccountFilter(),
										suffix,
										pathType
									)
								);
							} catch(Exception igore) {
								// In case of AUTHORIZATION_FAILUREs							
							}
						}
						if(pathType == PathType.SYNCFEED) {
							paths.add(
								new ResourcePath(
									feed,
									"/user/" + cardProfile.refGetPath().getSegment(6).toString() + "/profile/" + cardProfile.refGetPath().getSegment(8).toString() + "/" + feed.refGetPath().getLastSegment().toString() + suffix
								)
							);
						}
					}
				}
			}
		    return paths;
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get document paths for given object.
	 * 
	 * @param obj
	 * @param suffix
	 * @return
	 * @throws ServiceException
	 */
	private static List<ResourcePath> getDocumentPaths(
		RefObject_1_0 obj,
		String suffix,
		PathType pathType
	) throws ServiceException {
		try {
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
		    if(obj instanceof DocumentProfile) {
		    	DocumentProfile documentProfile = (DocumentProfile)obj;
				for(SyncFeed feed: documentProfile.<SyncFeed>getFeed()) {
					if(Boolean.TRUE.equals(feed.isActive())) {
						if(pathType == PathType.SYNCFEED) {
							paths.add(
								new ResourcePath(
									feed,
									"/user/" + documentProfile.refGetPath().getSegment(6).toString() + "/profile/" + documentProfile.refGetPath().getSegment(8).toString() + "/" + feed.refGetPath().getLastSegment().toString() + suffix
								)
							);
						}
					}
				}
			}
		    return paths;
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get CalDAV collection set URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getCalDavCollectionSetURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {
		try {
			PersistenceManager pm = JDOHelper.getPersistenceManager(obj);
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
			if(obj instanceof CalendarProfile) {
				CalendarProfile calendarProfile = (CalendarProfile)obj;			
		        if((calendarProfile.getName() != null) && !calendarProfile.getName().isEmpty()) {
		        	UserHome userHome = (UserHome)pm.getObjectById(new Path(obj.refMofId()).getParent().getParent());
		        	paths.add(
		        		new ResourcePath(
		        			userHome,
		        			"/user/" + userHome.refGetPath().getLastSegment().toString() + "/profile/" + calendarProfile.getName()
		        		)
		        	);      	
		        }
			}
			return mapToURLs(
				baseUrl.endsWith("/") ? baseUrl : baseUrl + "/",
				obj,
				"caldav",
				paths
			);
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get CalDAV event collection URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getCalDavEventCollectionURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {
		return mapToURLs(
			baseUrl.endsWith("/") ? baseUrl : baseUrl + "/",
			obj,
			"caldav",
			getCalendarPaths(obj, false, "", PathType.SYNCFEED)
		);
	}

	/**
	 * Get CalDAV task collection URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getCalDavTaskCollectionURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {
		return mapToURLs(
			baseUrl.endsWith("/") ? baseUrl : baseUrl + "/",
			obj,
			"caldav",
			getCalendarPaths(obj, true, "", PathType.SYNCFEED)
		);
	}

	/**
	 * Get WebDAV collection set URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getWebDavCollectionSetURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {		
		try {
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
			PersistenceManager pm = JDOHelper.getPersistenceManager(obj);
			if(obj instanceof DocumentProfile) {
				DocumentProfile documentProfile = (DocumentProfile)obj;			
		        if((documentProfile.getName() != null) && !documentProfile.getName().isEmpty()) {
		        	UserHome userHome =  (UserHome)pm.getObjectById(obj.refGetPath().getParent().getParent());
		        	paths.add(
		        		new ResourcePath(
		        			userHome,
		        			"/user/" + userHome.refGetPath().getLastSegment().toString() + "/profile/" + documentProfile.getName()
		        		)
		        	);	        	
		        }			
			}
			return mapToURLs(
				baseUrl.endsWith("/") ? baseUrl : baseUrl + "/",
				obj,
				"webdav",
				paths
			);
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get WebDAV collection URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getWebDavCollectionURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {
		return mapToURLs(
			baseUrl.endsWith("/") ? baseUrl : baseUrl + "/",
			obj,
			"webdav",
			getDocumentPaths(obj, "", PathType.SYNCFEED)
		);
	}

	/**
	 * Get CardDAV collection set URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getCardDavCollectionSetURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {
		try {
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
			PersistenceManager pm = JDOHelper.getPersistenceManager(obj);			
			if(obj instanceof CardProfile) {
				CardProfile cardProfile = (CardProfile)obj;			
		        if((cardProfile.getName() != null) && !cardProfile.getName().isEmpty()) {
		        	UserHome userHome =  (UserHome)pm.getObjectById(obj.refGetPath().getParent().getParent());
		        	paths.add(
		        		new ResourcePath(
		        			userHome,
		        			"/user/" + userHome.refGetPath().getLastSegment().toString() + "/profile/" + cardProfile.getName()
		        		)
		        	);	        	
		        }
			}
			return mapToURLs(
				baseUrl.endsWith("/") ? baseUrl : baseUrl + "/",
				obj,
				"carddav",
				paths
			);
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get CardDAV collection URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getCardDavCollectionURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {
		return mapToURLs(
			baseUrl.endsWith("/") ? baseUrl : baseUrl + "/",
			obj,
			"carddav",
			getCardPaths(obj, "", PathType.SYNCFEED)
		);
	}

	/**
	 * Get activities ICAL URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @param optionMax
	 * @param optionIsDisabled
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getICalURLs(
		String baseUrl,
		RefObject_1_0 obj,
		String optionMax,
		String optionIsDisabled
	) throws ServiceException {
		try {
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
			String suffix = "&type=ics&max=" + (optionMax == null ? "" : optionMax) + "&disabled=" + (optionIsDisabled == null ? "" : optionIsDisabled);
			if(obj instanceof Resource) {
				Resource resource = (Resource)obj;
				if((resource.getName() != null) && !resource.getName().isEmpty()) {
					paths.add(
						new ResourcePath(
							resource,
							"/resource/" + resource.getName() + suffix
						)
					);						
			    }
			}
		    paths.addAll(
		    	getCalendarPaths(
		    		obj,
		    		false,
		    		suffix,
		    		PathType.ORIGIN
		    	)
		    );
			return mapToURLs(
				baseUrl.endsWith("/") ? baseUrl + "activities?id=" : baseUrl + "/activities?id=",
				obj,
				"ical",
				paths
			);
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get freebusy URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @param optionUser
	 * @param optionMax
	 * @param optionIsDisabled
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getFreeBusyURLs(
		String baseUrl,
		RefObject_1_0 obj,
		String optionUser,
		String optionMax,
		String optionIsDisabled
	) throws ServiceException {
		try {
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
			String suffix1 = "&user=" + (optionUser == null ? "" : optionUser) + "&max=" + (optionMax == null ? "" : optionMax) + "&disabled=" + (optionIsDisabled == null ? "" : optionIsDisabled);
			String suffix2 = "&type=ics&user=" + (optionUser == null ? "" : optionUser) + "&max=" + (optionMax == null ? "" : optionMax) + "&disabled=" + (optionIsDisabled == null ? "" : optionIsDisabled);
			if(obj instanceof Resource) {
				Resource resource = (Resource)obj;
				if((resource.getName() != null) && !resource.getName().isEmpty()) {
					paths.add(
						new ResourcePath(
							resource,
							"/resource/" + resource.getName() + suffix1
						)
					);      	
					paths.add(
						new ResourcePath(
							resource,
							"/resource/" +resource.getName() + suffix2
						)
					);      	
				}
			}	
		    paths.addAll(
		    	getCalendarPaths(
		    		obj, 
		    		false,
		    		suffix1,
		    		PathType.ORIGIN
		    	)
		    );
		    paths.addAll(
		    	getCalendarPaths(
		    		obj, 
		    		false,
		    		suffix2,
		    		PathType.ORIGIN
		    	)
		    );
			return mapToURLs(
				baseUrl.endsWith("/") ? baseUrl + "freebusy?id=" : baseUrl + "/freebusy?id=",
				obj,
				"ical",
				paths
			);
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get other calendar URLs (bdays, anniversaries, datesofdeath) for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @param optionMax
	 * @param optionSummaryPrefix
	 * @param optionCategories
	 * @param optionYear
	 * @param optionAlarm
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getOtherCalendarURLs(
		String baseUrl,
		RefObject_1_0 obj,
		String optionMax,
		String optionSummaryPrefix,
		String optionCategories,
		String optionYear,
		String optionAlarm
	) throws ServiceException {
		try {
			List<ResourcePath> paths = new ArrayList<ResourcePath>();
			paths.addAll(
				getCardPaths(
					obj,
					"&type=ics&max=" + optionMax + "&icalType=VEVENT&summaryPrefix=" + (optionSummaryPrefix == null ? "" : optionSummaryPrefix) + "&categories=" + (optionCategories == null ? "" : optionCategories) + "&year=" + (optionYear == null ? "" : optionYear) + "&alarm=" + (optionAlarm == null ? "" : optionAlarm),
					PathType.ORIGIN
				)
			);
			paths.addAll(
				getCardPaths(
					obj,
					"&type=ics&max=" + optionMax + "&icalType=VTODO&summaryPrefix=" + (optionSummaryPrefix == null ? "" : optionSummaryPrefix) + "&categories=" + (optionCategories == null ? "" : optionCategories) + "&year=" + (optionYear == null ? "" : optionYear) + "&alarm=" + (optionAlarm == null ? "" : optionAlarm),
					PathType.ORIGIN
				)
			);
			List<ConnectionURL> urls = new ArrayList<ConnectionURL>();
			for(CalendarType calendarType: CalendarType.values()) {
				urls.addAll(
					mapToURLs(
						baseUrl.endsWith("/") 
							? baseUrl + calendarType.getPath().substring(1) + "?id=" 
							: baseUrl + calendarType.getPath() + "?id=",
						obj,
						"ical",
						paths
					)
				);
			}
			return urls;
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Get VCARD URLs for given object.
	 * 
	 * @param baseUrl
	 * @param obj
	 * @return
	 * @throws ServiceException
	 */
	public static List<ConnectionURL> getVCardURLs(
		String baseUrl,
		RefObject_1_0 obj
	) throws ServiceException {
		try {
			List<ResourcePath> paths = getCardPaths(
				obj,
				"&type=vcf",
				PathType.ORIGIN
			);
			return mapToURLs(
				baseUrl.endsWith("/") ? baseUrl + "accounts?id=" : baseUrl + "/accounts?id=",
				obj,
				"vcard",
				paths
			);
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

}
