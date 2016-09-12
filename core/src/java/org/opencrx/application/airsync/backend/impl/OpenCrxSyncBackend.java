/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: Sync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010-2011, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.backend.impl;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Level;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.MimeMessage;
import javax.naming.Context;
import javax.naming.InitialContext;

import org.opencrx.application.airsync.backend.cci.ClientProfile;
import org.opencrx.application.airsync.backend.cci.GetChangedDataItemsResult;
import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.datatypes.AttachmentDataT;
import org.opencrx.application.airsync.datatypes.ContactT;
import org.opencrx.application.airsync.datatypes.DataType;
import org.opencrx.application.airsync.datatypes.EmailT;
import org.opencrx.application.airsync.datatypes.EventT;
import org.opencrx.application.airsync.datatypes.FolderType;
import org.opencrx.application.airsync.datatypes.IData;
import org.opencrx.application.airsync.datatypes.NoteT;
import org.opencrx.application.airsync.datatypes.SyncCollection;
import org.opencrx.application.airsync.datatypes.SyncDataItem;
import org.opencrx.application.airsync.datatypes.SyncFolder;
import org.opencrx.application.airsync.datatypes.TaskT;
import org.opencrx.kernel.account1.cci2.AccountMembershipQuery;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountMembership;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Absence;
import org.opencrx.kernel.activity1.jmi1.AbstractFilterActivity;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.ExternalActivity;
import org.opencrx.kernel.activity1.jmi1.Incident;
import org.opencrx.kernel.activity1.jmi1.Mailing;
import org.opencrx.kernel.activity1.jmi1.Meeting;
import org.opencrx.kernel.activity1.jmi1.NewActivityParams;
import org.opencrx.kernel.activity1.jmi1.NewActivityResult;
import org.opencrx.kernel.activity1.jmi1.PhoneCall;
import org.opencrx.kernel.activity1.jmi1.Task;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Activities.ActivityClass;
import org.opencrx.kernel.backend.Activities.Priority;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.backend.UserHomes.AlertState;
import org.opencrx.kernel.document1.cci2.DocumentQuery;
import org.opencrx.kernel.document1.cci2.FolderAssignmentQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.FolderAssignment;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.GenericAccount;
import org.opencrx.kernel.home1.cci2.AirSyncClientProfileQuery;
import org.opencrx.kernel.home1.cci2.AirSyncProfileQuery;
import org.opencrx.kernel.home1.cci2.AlertQuery;
import org.opencrx.kernel.home1.cci2.SyncFeedQuery;
import org.opencrx.kernel.home1.jmi1.ActivityFilterCalendarFeed;
import org.opencrx.kernel.home1.jmi1.ActivityGroupCalendarFeed;
import org.opencrx.kernel.home1.jmi1.AirSyncClientProfile;
import org.opencrx.kernel.home1.jmi1.AirSyncProfile;
import org.opencrx.kernel.home1.jmi1.Alert;
import org.opencrx.kernel.home1.jmi1.ContactsFeed;
import org.opencrx.kernel.home1.jmi1.DocumentFeed;
import org.opencrx.kernel.home1.jmi1.SyncData;
import org.opencrx.kernel.home1.jmi1.SyncFeed;
import org.opencrx.kernel.home1.jmi1.SyncProfile;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.persistence.cci.UserObjects;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.loading.Factory;
import org.openmdx.kernel.log.SysLog;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class OpenCrxSyncBackend implements SyncBackend {

	/**
	 * Create backend with default data type mapper.
	 */
	public OpenCrxSyncBackend(
		PersistenceManagerFactory pmf,
		String providerName
	) {
		this(
			pmf, 
			providerName,
			new DatatypeMapper()
		);
	}

	public OpenCrxSyncBackend(
		PersistenceManagerFactory pmf,
		String providerName,
		DatatypeMapper datatypeMapper
	) {
		this.pmf = pmf;
		this.providerName = providerName;
		this.datatypeMapper = datatypeMapper;
	}
	
	protected PersistenceManager newPersistenceManager(
		final RequestContext requestContext
	) throws ServiceException {
		try {
			String userId = requestContext.getUserId();
			if(userId == null || this.pmf == null) {
				throw new ServiceException(
					BasicException.Code.DEFAULT_DOMAIN,
					BasicException.Code.ASSERTION_FAILURE,
					"Unable to aquire persistence manager"
				);
			}			
			int pos = userId.indexOf(SyncBackend.DOMAIN_SEPARATOR);
			PersistenceManager pm = this.pmf.getPersistenceManager(
				pos > 0 ? userId.substring(pos + 1) : userId,
				null
			);
			UserObjects.setTransactionTime(
				pm, 
				new Factory<Date>(){
					
					@Override
                    public Class<? extends Date> getInstanceClass(
                    ) {
						return Date.class;
                    }

					@Override
                    public Date instantiate(
                    ) {
						// Reduce precision of syncKey to 1/100s
						// This solves rounding problems at persistence-level such as
						// http://blogs.msdn.com/b/cdnsoldevs/archive/2011/06/22/why-you-should-never-use-datetime-again.aspx
						long syncKeyMillis = Long.valueOf(requestContext.getSyncKey());
						return new Date((syncKeyMillis / 10L) * 10L);
                    }
				}
			);
			return pm;
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	protected UserHome getUserHome(
		PersistenceManager pm,
		String userId
	) {
		int pos = userId.indexOf(SyncBackend.DOMAIN_SEPARATOR);
		String segmentName = userId.substring(0, pos);
		String userName = userId.substring(pos + 1);
		return (UserHome)pm.getObjectById(
			new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/" + this.providerName + "/segment/" + segmentName + "/userHome/" + userName)
		);
	}

	protected AirSyncProfile getSyncProfile(
		UserHome user,
		String profileName
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(user);
		List<AirSyncProfile> profiles = Collections.emptyList();
		// profileName has pattern 'profilePrefix~deviceId'. 
		// Try to find device-specific AirSync profile
		AirSyncProfileQuery profileQuery = (AirSyncProfileQuery)pm.newQuery(AirSyncProfile.class);
		profileQuery.name().equalTo(profileName);				
		profiles = user.getSyncProfile(profileQuery);
		// If profile was not found fall back to non device-specific profile
		if(profiles.isEmpty() && profileName.indexOf("~") > 0) {
			profileQuery = (AirSyncProfileQuery)pm.newQuery(AirSyncProfile.class);
			profileQuery.name().equalTo(profileName.substring(0, profileName.indexOf("~")));
			profiles = user.getSyncProfile(profileQuery);
		}
		if(!profiles.isEmpty()) {				
			return profiles.iterator().next();
		}
		return null;
	}
		
	@Override
    public RequestContext newRequestContext(
    	final String userId, 
    	final Object context
    ) {
		final String syncKey = Long.toString(System.currentTimeMillis());
		return new RequestContext(){

			@Override
			public String getUserId(
			) {
				return userId;
			}

			@Override
			public Object getContext(
			) {
				return context;
			}

			@Override
            public String getSyncKey(
            ) {
	            return syncKey;
            }
			
		};
    }

	public RefObject_1_0 findFolder(
		UserHome user,
		String profileName,
		SyncCollection collection,
		boolean forAddDelete,
		boolean forChange
	) {
		String collectionId = collection.getCollectionId();
		DataType collectionType = collection.getDataType();
		// Default folders are not mapped to openCRX
		if(collectionId != null && DEFAULT_FOLDERS.keySet().contains(collectionId)) {
			return null;
		}
		PersistenceManager pm = JDOHelper.getPersistenceManager(user);		
		Path feedIdentity = this.datatypeMapper.toObjectIdentity(collectionId);
		// Fall back to data type. Take first active feed which matches the collection type
		if(feedIdentity == null && collectionType != null) {
			AirSyncProfile syncProfile = this.getSyncProfile(user, profileName);
			if(syncProfile != null) {
				SyncFeedQuery query = (SyncFeedQuery)pm.newQuery(SyncFeed.class);
				query.thereExistsIsActive().isTrue();
				query.orderByName().ascending();			
				List<SyncFeed> feeds = syncProfile.getFeed(query);
				for(SyncFeed feed: feeds) {
					if(feed instanceof ContactsFeed && collectionType == DataType.Contacts) {
						feedIdentity = feed.refGetPath();
						break;
					}
					else if(
						(feed instanceof ActivityGroupCalendarFeed || feed instanceof ActivityFilterCalendarFeed) &&
						(collectionType == DataType.Calendar || collectionType == DataType.Email || collectionType == DataType.Tasks)
					) {
						feedIdentity = feed.refGetPath();
						break;
					}
				}
			}
		}
		if(feedIdentity != null) {
			RefObject_1_0 feed = null;
			try {
				feed = (RefObject_1_0)pm.getObjectById(feedIdentity);
			} catch(Exception e) {}
			if(feed instanceof SyncFeed) {
				// Update collection id if not already set
				if(collection.getCollectionId() == null || collection.getCollectionId().length() == 0) {
					collection.setCollectionId(this.datatypeMapper.toObjectId(feed));
				}
				SyncFeed syncFeed = (SyncFeed)feed;
				if(
					(!forAddDelete || Boolean.TRUE.equals(syncFeed.isAllowAddDelete())) &&
					(!forChange || Boolean.TRUE.equals(syncFeed.isAllowChange()))
				) {
					return syncFeed instanceof ContactsFeed ?
						((ContactsFeed)syncFeed).getAccountGroup() :
							syncFeed instanceof ActivityGroupCalendarFeed ?
								((ActivityGroupCalendarFeed)syncFeed).getActivityGroup() :
									syncFeed instanceof ActivityFilterCalendarFeed ?
										((ActivityFilterCalendarFeed)syncFeed).getActivityFilter() :
											null;
				} else {
					return null;
				}
			} else {
				return feed;
			}
		} else {
			return null;
		}
	}
	
    public boolean isInitialSync(
    	RequestContext requestContext, 
    	String syncKey
    ) throws ServiceException {
		return
			INITIAL_SYNC_KEY.equals(syncKey) ||
			FIRST_SYNC_KEY.equals(syncKey);
    }

	@Override
    public String getNextSyncKey(
		RequestContext requestContext,
    	String syncKey
    ) {
		return INITIAL_SYNC_KEY.equals(syncKey) ?
			FIRST_SYNC_KEY :
				Long.toString(System.currentTimeMillis());
    }

	@Override
    public void deleteDataItem(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection,
    	String itemId
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		if(pm != null) {
			UserHome user = this.getUserHome(pm, requestContext.getUserId());
			RefObject_1_0 folder = this.findFolder(
				user,
				profileName,
				collection, 
				true, // forAddDelete 
				false // forChange
			);
			if(folder != null) {			
				Path objectIdentity = this.datatypeMapper.toObjectIdentity(itemId);
				if(objectIdentity != null) {
					RefObject_1_0 refObj = (RefObject_1_0)pm.getObjectById(objectIdentity);
					try {
						// In case of Contact folders disable relationship (Member) 
						// instead of contact object itself
						if(folder instanceof AbstractGroup && refObj instanceof GenericAccount) {
							MemberQuery memberQuery = (MemberQuery)pm.newQuery(Member.class);
							memberQuery.thereExistsAccount().equalTo(refObj);
							List<Member> members = ((AbstractGroup)folder).getMember(memberQuery);
							pm.currentTransaction().begin();
							for(Member member: members) {
								member.setDisabled(true);
							}
							pm.currentTransaction().commit();
						} 
						// In case of alert mark alert as accepted
						else if(refObj instanceof Alert) {
							pm.currentTransaction().begin();
							((Alert)refObj).setAlertState(AlertState.ACCEPTED.getValue());
							pm.currentTransaction().commit();
						} 
						// In case of Document folders disable folder assignment 
						else if(folder instanceof DocumentFolder && refObj instanceof Document) {							
							FolderAssignmentQuery folderAssignmentQuery = (FolderAssignmentQuery)pm.newQuery(FolderAssignment.class);
							folderAssignmentQuery.thereExistsDocumentFolder().equalTo(folder);
							List<FolderAssignment> folderAssignments = ((Document)refObj).getDocumentFolderAssignment(folderAssignmentQuery);
							pm.currentTransaction().begin();
							for(FolderAssignment folderAssignment: folderAssignments) {
								folderAssignment.setDisabled(true);
							}
							pm.currentTransaction().commit();
						}
						// Mark object as disabled. Physical deletion is 
						// not supported by this backend
						else if(refObj instanceof CrxObject) {
							pm.currentTransaction().begin();
							((CrxObject)refObj).setDisabled(true);
							pm.currentTransaction().commit();
						}
					} catch(Exception e) {
						new ServiceException(e).log();
						try {
							pm.currentTransaction().rollback();
						} catch(Exception e1) {}
					}
				}
			}
		}
    }

	@Override
    public SyncDataItem fetchDataItem(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection,
    	String itemId
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		if(pm != null) {
			UserHome user = this.getUserHome(pm, requestContext.getUserId());
			Path objectIdentity = this.datatypeMapper.toObjectIdentity(itemId);
			if(objectIdentity != null) {
				RefObject_1_0 object = (RefObject_1_0)pm.getObjectById(objectIdentity);	
				return this.datatypeMapper.toDataItem(
					object, 
					false, 
					user,
					requestContext
				);
			}
		}
		return null;
    }

	@Override
	public AttachmentDataT getAttachementData(
		RequestContext requestContext,
		String attachmentId
	) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		if(pm != null) {
			Path objectIdentity = this.datatypeMapper.toObjectIdentity(attachmentId);
			if(objectIdentity != null) {
				RefObject_1_0 object = (RefObject_1_0)pm.getObjectById(objectIdentity);
				return this.datatypeMapper.toAttachmentData(object);
			}
		}
		return null;
	}

	protected Activity createActivity(
		ActivityGroup group,
		short activityClass,
		String name,
		String detailedDescription,
		Date scheduledStart
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(group);
		ActivityCreator creator = null;
		Collection<ActivityCreator> activityCreators =  group.getActivityCreator();
		for(ActivityCreator activityCreator: activityCreators) {
			if(activityCreator.getActivityType() != null && activityCreator.getActivityType().getActivityClass() == activityClass) {
				creator = activityCreator;
				break;
			}										
		}
		Activity activity = null;
		if(creator != null) {
			NewActivityParams params = Structures.create(
				NewActivityParams.class, 
				Datatypes.member(NewActivityParams.Member.creationContext, null),
				Datatypes.member(NewActivityParams.Member.description, null),			
				Datatypes.member(NewActivityParams.Member.detailedDescription, detailedDescription),			
				Datatypes.member(NewActivityParams.Member.dueBy, null),			
				Datatypes.member(NewActivityParams.Member.icalType, ICalendar.ICAL_TYPE_NA),	
				Datatypes.member(NewActivityParams.Member.name, name),	
				Datatypes.member(NewActivityParams.Member.priority, Priority.NORMAL.getValue()),
				Datatypes.member(NewActivityParams.Member.reportingContact, null),
				Datatypes.member(NewActivityParams.Member.scheduledEnd, null),
				Datatypes.member(NewActivityParams.Member.scheduledStart, scheduledStart)
			);
			pm.currentTransaction().begin();
			NewActivityResult result = creator.newActivity(params);
			pm.currentTransaction().commit();
			if(result.getActivity() != null) {
				activity = (Activity)pm.getObjectById(result.getActivity().refGetPath());
			}
		}
		return activity;
	}
	
	protected Document createDocument(
		DocumentFolder documentFolder,
		String name
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(documentFolder);
		String providerName = documentFolder.refGetPath().get(2);
		String segmentName = documentFolder.refGetPath().get(4);
		org.opencrx.kernel.document1.jmi1.Segment documentSegment = 
			(org.opencrx.kernel.document1.jmi1.Segment)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.document1").getDescendant("provider", providerName, "segment", segmentName)
			);		
		Document document = pm.newInstance(Document.class);
		document.setName(name);
		document.setTitle(name);
		document.getOwningGroup().addAll(
			documentFolder.getOwningGroup()
		);
		documentSegment.addDocument(
			Documents.getInstance().getUidAsString(), 
			document
		);
		return document;
	}
	
	@Override
	public String createOrUpdateDataItem(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection,
		String itemId, 
		IData data
	) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		if(pm != null) {
			UserHome user = this.getUserHome(pm, requestContext.getUserId());
			RefObject_1_0 folderAddDelete = this.findFolder(
				user,
				profileName,
				collection,
				true, // forAddDelete
				false // forChange
			);
			RefObject_1_0 folderChange = this.findFolder(
				user, 
				profileName,
				collection,
				false, // forAddDelete 
				true // forChange
			);
			RefObject_1_0 object = null;
			// Try to match item in case no itemId is provided
			if(itemId == null) {
				itemId = this.datatypeMapper.matchItem(user, data);
			}
			// Add
			if(itemId == null && folderAddDelete != null) {
				switch(data.getType()) {

					case Calendar:
						if(folderAddDelete instanceof ActivityGroup) {
							EventT eventT = (EventT)data;
							object = this.createActivity(
								(ActivityGroup)folderAddDelete,
								ActivityClass.MEETING.getValue(),
								eventT.getSubject(),
								this.datatypeMapper.normalizeMultilineString(eventT.getBody()),
								eventT.getStartTime()
							);									
						}
						break;
						
					case Contacts:
						if(folderAddDelete instanceof AbstractGroup) {
							pm.currentTransaction().begin();
							AbstractGroup group = (AbstractGroup)folderAddDelete;
							String providerName = group.refGetPath().getSegment(2).toClassicRepresentation();
							String segmentName = group.refGetPath().getSegment(4).toClassicRepresentation();
							org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, providerName, segmentName);
							ContactT contactT = (ContactT)data;
							Account account = this.datatypeMapper.newAccount(pm, contactT);
							if(account instanceof Contact) {
								Contact contact = (Contact)account;
								contact.setLastName(contactT.getLastName());
								contact.setFirstName(contactT.getFirstName());
							} 
							else if(account instanceof AbstractGroup) {
								AbstractGroup g = (AbstractGroup)account;
								g.setName(contactT.getLastName());
							}
							account.getOwningGroup().addAll(
								group.getOwningGroup()
							);
							Member member = pm.newInstance(Member.class);
							member.setName(
								contactT.getLastName() + 
								(contactT.getFirstName() == null ? "" : ", " + contactT.getFirstName())
							);
							member.setAccount(account);
							member.setQuality(Accounts.MEMBER_QUALITY_NORMAL);
							group.addMember(
								Base.getInstance().getUidAsString(), 
								member
							);
							accountSegment.addAccount(
								Base.getInstance().getUidAsString(), 
								account
							);
							pm.currentTransaction().commit();
							object = account;
						}
						break;
						
					case Tasks:
						if(folderAddDelete instanceof ActivityGroup) {
							TaskT taskT = (TaskT)data;
							object = this.createActivity(
								(ActivityGroup)folderAddDelete,
								ActivityClass.TASK.getValue(),
								taskT.getSubject(),
								this.datatypeMapper.normalizeMultilineString(taskT.getBody()),
								taskT.getStartdate()
							);									
						}
						break;

					case Email:
						if(folderAddDelete instanceof ActivityGroup) {
							EmailT emailT = (EmailT)data;
							object = this.createActivity(
								(ActivityGroup)folderAddDelete,
								ActivityClass.EMAIL.getValue(),
								emailT.getSubject(),
								null, // detailedDescription
								emailT.getDateReceived()
							);
						}
						break;	
						
					case Notes:
						if(folderAddDelete instanceof DocumentFolder) {
							NoteT noteT = (NoteT)data;
							object = this.createDocument(
								(DocumentFolder)folderAddDelete,
								noteT.getSubject()
							);
						}
						break;
						
				}
			}
			// Change
			else {
				if(folderChange != null) {
					Path objectIdentity = this.datatypeMapper.toObjectIdentity(itemId);
					if(objectIdentity != null) {
						try {
							object = (RefObject_1_0)pm.getObjectById(objectIdentity);
						} catch(Exception e) {}
					}
					if(object != null) {
						switch(data.getType()) {
							case Contacts:
								// Assert that membership exists
								Account account = (Account)object;
								AbstractGroup group = (AbstractGroup)folderChange;
								ContactT contactT = (ContactT)data;								
								MemberQuery memberQuery = (MemberQuery)pm.newQuery(Member.class);
								memberQuery.thereExistsAccount().equalTo(account);
								List<Member> members = group.getMember(memberQuery);
								if(members.isEmpty()) {
									pm.currentTransaction().begin();
									Member member = pm.newInstance(Member.class);
									member.setName(
										contactT.getLastName() + 
										(contactT.getFirstName() == null ? "" : ", " + contactT.getFirstName())
									);
									member.setAccount(account);
									member.setQuality(Accounts.MEMBER_QUALITY_NORMAL);
									group.addMember(
										Base.getInstance().getUidAsString(), 
										member
									);									
									pm.currentTransaction().commit();
								}
								break;
							case Calendar:
								break;
							case Tasks:
								break;
							case Notes:
								// Assert that folder assignment exists
								Document document = (Document)object;
								DocumentFolder documentFolder = (DocumentFolder)folderChange;
								FolderAssignmentQuery folderAssignmentQuery = (FolderAssignmentQuery)pm.newQuery(FolderAssignment.class);
								folderAssignmentQuery.thereExistsDocumentFolder().equalTo(documentFolder);
								List<FolderAssignment> assignments = document.getDocumentFolderAssignment(folderAssignmentQuery);
								if(assignments.isEmpty()) {
									pm.currentTransaction().begin();
									FolderAssignment folderAssignment = pm.newInstance(FolderAssignment.class);
									folderAssignment.setName(documentFolder.getName());
									folderAssignment.setDocumentFolder(documentFolder);
									document.addDocumentFolderAssignment(
										Base.getInstance().getUidAsString(),
										folderAssignment
									);
									pm.currentTransaction().commit();										
								}
								break;
						}
					}
				}
			}
			if(object != null) {
				pm.currentTransaction().begin();
				this.datatypeMapper.toObject(
					data,
					object,
					user,
					requestContext
				);
				pm.currentTransaction().commit();
				String objectId = this.datatypeMapper.toObjectId(object);
				pm.close();
				return objectId;
			}
			pm.close();
		}
		return null;
	}

	@Override
	public String moveDataItem(
		RequestContext requestContext,
		String profileName,
		String srcFolderId,
		String dstFolderId,
		String itemId
	) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);		
		if(pm != null) {
			UserHome user = this.getUserHome(pm, requestContext.getUserId());
			SyncCollection srcCollection = new SyncCollection();
			srcCollection.setCollectionId(srcFolderId);
			SyncCollection dstCollection = new SyncCollection();
			dstCollection.setCollectionId(dstFolderId);
			RefObject_1_0 srcFolder = this.findFolder(
				user,
				profileName,
				srcCollection,
				true, // forAddDelete 
				false // forChange
			);
			RefObject_1_0 dstFolder = this.findFolder(
				user,
				profileName,
				dstCollection,
				true, // forAddDelete 
				false // forChange
			);
			Path itemIdentity = this.datatypeMapper.toObjectIdentity(itemId);
			if(srcFolder != null && dstFolder != null && itemIdentity != null) {
				RefObject_1_0 item = (RefObject_1_0)pm.getObjectById(itemIdentity);
				// Move activity
				if(srcFolder instanceof ActivityGroup && dstFolder instanceof ActivityGroup && item instanceof Activity) {
					Activity activity = (Activity)item;
					ActivityGroup srcGroup = (ActivityGroup)srcFolder;
					ActivityGroup dstGroup = (ActivityGroup)dstFolder;
					Collection<ActivityGroupAssignment> assignments = activity.getAssignedGroup();
					for(ActivityGroupAssignment assignment: assignments) {
						if(assignment.getActivityGroup().equals(srcGroup)) {
							pm.currentTransaction().begin();
							assignment.setActivityGroup(dstGroup);
							pm.currentTransaction().commit();		
							break;
						}
					}
				}
				// Move contact
				else if(srcFolder instanceof AbstractGroup && dstFolder instanceof AbstractGroup && item instanceof Contact) {
					Contact contact = (Contact)item;
					AbstractGroup srcGroup = (AbstractGroup)srcFolder;
					AbstractGroup dstGroup = (AbstractGroup)dstFolder;
					MemberQuery query = (MemberQuery)pm.newQuery(Member.class);
					query.thereExistsAccount().equalTo(contact);
					List<Member> members = srcGroup.getMember(query);
					if(!members.isEmpty()) {
						Member srcMember = members.iterator().next();
						pm.currentTransaction().begin();
						srcMember.refDelete();
						Member dstMember = pm.newInstance(Member.class);
						dstMember.setName(srcMember.getName());
						dstMember.setAccount(contact);
						dstGroup.addMember(
							Base.getInstance().getUidAsString(),
							dstMember
						);
						pm.currentTransaction().commit();
					}
				}
				pm.close();
				return itemId;
			}
			pm.close();
		}
		return null;
	}

	@Override
    public void setDataItemReadFlag(
		RequestContext requestContext,
    	String folderId, 
    	String itemId, 
    	boolean read
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);		
		if(pm != null) {
			Path itemIdentity = this.datatypeMapper.toObjectIdentity(itemId);
			if(itemIdentity != null) {
				RefObject_1_0 item = (RefObject_1_0)pm.getObjectById(itemIdentity);
				if(item instanceof Alert) {
					Alert alert = (Alert)item;
					pm.currentTransaction().begin();
					if(read) {
						UserHomes.getInstance().markAsRead(alert);
					}
					else {
						UserHomes.getInstance().markAsNew(alert);
					}
					pm.currentTransaction().commit();
				}
			}
			pm.close();
		}
    }

	@Override
    public GetChangedDataItemsResult getChangedDataItems(
		RequestContext requestContext,
		String profileName,
    	SyncCollection collection,
    	boolean noData,
    	int windowSize,
    	SyncDataItem.State state,
    	Set<String> excludes
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		String newSyncKey = collection.getSyncKey();
		Map<String,List<SyncDataItem>> changedDataItems = new TreeMap<String,List<SyncDataItem>>();
		boolean hasMore = false;
		if(pm != null) {
			UserHome user = this.getUserHome(pm, requestContext.getUserId());
			RefObject_1_0 folder = this.findFolder(
				user,
				profileName,
				collection, 
				false, // forAddDelete 
				false // forChange
			);
			Date since = new Date(Long.valueOf(collection.getSyncKey()));
			// Activity groups
			if(folder instanceof ActivityGroup) {
				ActivityGroup activityGroup = (ActivityGroup)folder;
				List<ActivityQuery> queries = new ArrayList<ActivityQuery>(); 
				switch(collection.getDataType()) {
					case Calendar:  
						ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
						PersistenceHelper.setClasses(query, Meeting.class, Incident.class, PhoneCall.class, Mailing.class, Absence.class, ExternalActivity.class);
						queries.add(query);
						break;
					case Tasks:  
						queries.add((ActivityQuery)pm.newQuery(Task.class));
						break;
					case Email:  
						queries.add((ActivityQuery)pm.newQuery(EMail.class));
						break;
				}
				int n = 0;
				for(ActivityQuery query: queries) {
					query.forAllDisabled().isFalse();
					if(state == SyncDataItem.State.NEW) {
						query.createdAt().greaterThan(since);
						query.orderByCreatedAt().ascending();
					} else {
						query.modifiedAt().greaterThan(since);
						query.orderByModifiedAt().ascending();
					}
					List<Activity> activities = activityGroup.getFilteredActivity(query);
					for(Activity activity: activities) {
						String syncKey = Long.toString(
							state == SyncDataItem.State.NEW ?
								activity.getCreatedAt().getTime() :									
									activity.getModifiedAt().getTime()
						);
						// !syncKey.equals(newSyncKey) asserts that next batch has different syncKey
						if(n >= windowSize && !syncKey.equals(newSyncKey)) {
							hasMore = true;
							break;
						} else {
							SyncDataItem dataItem = this.datatypeMapper.toDataItem(
								activity, 
								noData,
								user,
								requestContext
							);
							if(!excludes.contains(dataItem.getServerId())) {
								List<SyncDataItem> items = changedDataItems.get(syncKey);
								if(items == null) {
									changedDataItems.put(
										syncKey,
										items = new ArrayList<SyncDataItem>()
									);
								}
								items.add(dataItem);
								n++;
							}
							newSyncKey = syncKey;
						}
					}
				}
			}
			// Activity filters
			else if(folder instanceof AbstractFilterActivity) {
				AbstractFilterActivity activityFilter = (AbstractFilterActivity)folder;
				List<ActivityQuery> queries = new ArrayList<ActivityQuery>(); 
				switch(collection.getDataType()) {
					case Calendar:  
						ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
						PersistenceHelper.setClasses(query, Meeting.class, Incident.class, PhoneCall.class, Mailing.class, Absence.class, ExternalActivity.class);
						queries.add(query);
						break;
					case Tasks:  
						queries.add((ActivityQuery)pm.newQuery(Task.class));
						break;
					case Email:  
						queries.add((ActivityQuery)pm.newQuery(EMail.class));
						break;
				}
				int n = 0;
				for(ActivityQuery query: queries) {
					query.forAllDisabled().isFalse();
					if(state == SyncDataItem.State.NEW) {
						query.createdAt().greaterThan(since);
						query.orderByCreatedAt().ascending();							
					} else {
						query.modifiedAt().greaterThan(since);
						query.orderByModifiedAt().ascending();
					}
					List<Activity> activities = activityFilter.getFilteredActivity(query);
					for(Activity activity: activities) {
						String syncKey = Long.toString(
							state == SyncDataItem.State.NEW ?
								activity.getCreatedAt().getTime() :									
									activity.getModifiedAt().getTime()
						);
						// !syncKey.equals(newSyncKey) asserts that next batch has different syncKey
						if(n >= windowSize && !syncKey.equals(newSyncKey)) {
							hasMore = true;
							break;
						} else {				
							SyncDataItem dataItem = this.datatypeMapper.toDataItem(
								activity, 
								noData,
								user,
								requestContext
							);
							if(!excludes.contains(dataItem.getServerId())) {
								List<SyncDataItem> items = changedDataItems.get(syncKey);
								if(items == null) {
									changedDataItems.put(
										syncKey,
										items = new ArrayList<SyncDataItem>()
									);
								}
								items.add(dataItem);
								n++;
							}
							newSyncKey = syncKey;
						}
					} 
				}
			}
			// Contacts
			else if(folder instanceof AbstractGroup) {
				AbstractGroup group = (AbstractGroup)folder;
				Set<Path> changedContacts = new HashSet<Path>();
				// Get all changed contacts
				AccountMembershipQuery query = (AccountMembershipQuery)pm.newQuery(AccountMembership.class);
				query.forAllDisabled().isFalse();
				query.thereExistsAccountTo().forAllDisabled().isFalse();
				query.distance().equalTo(1);
				if(state == SyncDataItem.State.NEW) {
					query.createdAt().greaterThan(since);
					query.orderByCreatedAt().ascending();
				} else {
					query.modifiedAt().greaterThan(since);
					query.orderByModifiedAt().ascending();						
				}
				// For distance +/-1 memberships use ACCTMEMBERSHIP1 instead of ACCTMEMBERSHIP					
				QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
				queryExtension.setClause(
					Database_1_Attributes.HINT_DBOBJECT + "1 */ (1=1)" 						
				);					
				List<AccountMembership> memberships = group.getAccountMembership(query);
				int n = 0;
				for(AccountMembership membership: memberships) {
					if(!changedContacts.contains(membership.getAccountTo().refGetPath())) {
						String syncKey = Long.toString(
							state == SyncDataItem.State.NEW ?
								membership.getCreatedAt().getTime() :
									membership.getModifiedAt().getTime()										
						);
						// !syncKey.equals(newSyncKey) asserts that next batch has different syncKey
						if(n >= windowSize && !syncKey.equals(newSyncKey)) {
							hasMore = true;
							break;
						} else {
							SyncDataItem dataItem = this.datatypeMapper.toDataItem(
								membership.getAccountTo(), 
								noData,
								user,
								requestContext
							);
							if(!excludes.contains(dataItem.getServerId())) {
								List<SyncDataItem> items = changedDataItems.get(syncKey);
								if(items == null) {
									changedDataItems.put(
										syncKey,
										items = new ArrayList<SyncDataItem>()
									);
								}
								items.add(dataItem);
								changedContacts.add(membership.getAccountTo().refGetPath());
								n++;
							}
							newSyncKey = syncKey;
						}
					}
				}
			}
			else if(folder instanceof UserHome) {
				UserHome home = (UserHome)folder;
				AlertQuery query = (AlertQuery)pm.newQuery(Alert.class);
				// Alerts are treated as read-only objects 
				if(state == SyncDataItem.State.NEW) {
					query.createdAt().greaterThan(since);
					query.orderByCreatedAt().ascending();
					query.alertState().equalTo(AlertState.NEW.getValue());
					List<Alert> alerts = home.getAlert(query);
					int n = 0;
					for(Alert alert: alerts) {
						String syncKey = Long.toString(alert.getCreatedAt().getTime());
						// !syncKey.equals(newSyncKey) asserts that next batch has different syncKey
						if(n >= windowSize && !syncKey.equals(newSyncKey)) {
							hasMore = true;
							break;
						} else {
							SyncDataItem dataItem = this.datatypeMapper.toDataItem(
								alert, 
								noData,
								user,
								requestContext
							); 
							if(!excludes.contains(dataItem.getServerId())) {
								List<SyncDataItem> items = changedDataItems.get(syncKey);
								if(items == null) {
									changedDataItems.put(
										syncKey,
										items = new ArrayList<SyncDataItem>()
									);
								}
								items.add(dataItem);
								n++;
							}
							newSyncKey = syncKey;
						}
					}
				}
			}
			// Notes
			else if(folder instanceof DocumentFolder) {
				DocumentFolder documentFolder = (DocumentFolder)folder;
				Set<Path> changedDocuments = new HashSet<Path>();
				// Get all changed documents
				DocumentQuery query = (DocumentQuery)pm.newQuery(Document.class);				
				query.forAllDisabled().isFalse();
				query.thereExistsDocumentFolderAssignment().forAllDisabled().isFalse();
				query.thereExistsDocumentFolderAssignment().thereExistsDocumentFolder().equalTo(documentFolder);
				query.thereExistsContentType().equalTo(NOTE_MIME_TYPE);
				if(state == SyncDataItem.State.NEW) {
					query.createdAt().greaterThan(since);
					query.orderByCreatedAt().ascending();
				} else {
					query.modifiedAt().greaterThan(since);
					query.orderByModifiedAt().ascending();						
				}
				String providerName = documentFolder.refGetPath().get(2);
				String segmentName = documentFolder.refGetPath().get(4);
				org.opencrx.kernel.document1.jmi1.Segment documentSegment = 
					(org.opencrx.kernel.document1.jmi1.Segment)pm.getObjectById(
						new Path("xri://@openmdx*org.opencrx.kernel.document1").getDescendant("provider", providerName, "segment", segmentName)
					);
				List<Document> documents = documentSegment.getDocument(query);
				int n = 0;
				for(Document document: documents) {
					if(!changedDocuments.contains(document.refGetPath())) {
						String syncKey = Long.toString(
							state == SyncDataItem.State.NEW ?
								document.getCreatedAt().getTime() :
									document.getModifiedAt().getTime()										
						);
						// !syncKey.equals(newSyncKey) asserts that next batch has different syncKey
						if(n >= windowSize && !syncKey.equals(newSyncKey)) {
							hasMore = true;
							break;
						} else {
							SyncDataItem dataItem = this.datatypeMapper.toDataItem(
								document, 
								noData,
								user,
								requestContext
							);
							if(!excludes.contains(dataItem.getServerId())) {
								List<SyncDataItem> items = changedDataItems.get(syncKey);
								if(items == null) {
									changedDataItems.put(
										syncKey,
										items = new ArrayList<SyncDataItem>()
									);
								}
								items.add(dataItem);
								changedDocuments.add(document.refGetPath());
								n++;
							}
							newSyncKey = syncKey;
						}
					}
				}
			}
			pm.close();
		}
		return new GetChangedDataItemsResult(
			changedDataItems,
			hasMore,
			newSyncKey
		);
    }

	@Override
    public List<String> getDeletedDataItems(
		RequestContext requestContext,
		String profileName,
    	SyncCollection collection,
    	String syncKeyTo
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);	
		List<String> deletedDataItems = new ArrayList<String>();
		// An initial sync never has deleted items
		if(pm != null && !this.isInitialSync(requestContext, collection.getSyncKey())) {
			UserHome user = this.getUserHome(pm, requestContext.getUserId());
			RefObject_1_0 folder = this.findFolder(
				user,
				profileName,
				collection, 
				false, // forAddDelete 
				false // forChange
			);
			Date since = new Date(Long.valueOf(collection.getSyncKey()));
			Date to = new Date(Long.valueOf(syncKeyTo));
			// Activities
			if(folder instanceof ActivityGroup) {
				ActivityGroup activityGroup = (ActivityGroup)folder;
				List<ActivityQuery> queries = new ArrayList<ActivityQuery>(); 
				switch(collection.getDataType()) {
					case Calendar:  
						queries.add((ActivityQuery)pm.newQuery(Meeting.class));
						queries.add((ActivityQuery)pm.newQuery(Incident.class));
						break;
					case Tasks:  
						queries.add((ActivityQuery)pm.newQuery(Task.class));
						break;
					case Email:  
						queries.add((ActivityQuery)pm.newQuery(EMail.class));
						break;
				}
				for(ActivityQuery query: queries) {
					query.thereExistsDisabled().isTrue();
					query.modifiedAt().greaterThan(since);
					query.modifiedAt().lessThanOrEqualTo(to);
					query.orderByModifiedAt().ascending();
					List<Activity> activities = activityGroup.getFilteredActivity(query);
					for(Activity activity: activities) {
						deletedDataItems.add(
							this.datatypeMapper.toObjectId(activity)
						);
					}
				}
			}
			// Contacts
			else if(folder instanceof AbstractGroup) {
				AbstractGroup group = (AbstractGroup)folder;
				Set<Path> deletedContacts = new HashSet<Path>();
				// Get contacts where membership is disabled
				AccountMembershipQuery query = (AccountMembershipQuery)pm.newQuery(AccountMembership.class);
				query.thereExistsDisabled().isTrue();
				query.distance().equalTo(1);
				query.modifiedAt().greaterThan(since);
				query.modifiedAt().lessThanOrEqualTo(to);
				query.orderByModifiedAt().ascending();
				// For distance +/-1 memberships use ACCTMEMBERSHIP1 instead of ACCTMEMBERSHIP					
				QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
				queryExtension.setClause(
					Database_1_Attributes.HINT_DBOBJECT + "1 */ (1=1)" 						
				);					
				List<AccountMembership> memberships = group.getAccountMembership(query);
				for(AccountMembership membership: memberships) {
					if(
						membership.getAccountTo() instanceof Contact && 
						!deletedContacts.contains(membership.getAccountTo().refGetPath())
					) {
						deletedDataItems.add(
							this.datatypeMapper.toObjectId(membership.getAccountTo())
						);
						deletedContacts.add(membership.getAccountTo().refGetPath());
					}
				}
			}
			else if(folder instanceof UserHome) {
				UserHome home = (UserHome)folder;
				AlertQuery query = (AlertQuery)pm.newQuery(Alert.class);
				// Consider alerts as deleted if their state was modified to >= accepted
				query.modifiedAt().greaterThan(since);
				query.modifiedAt().lessThanOrEqualTo(to);
				query.alertState().equalTo(AlertState.ACCEPTED.getValue());		
				query.orderByModifiedAt().ascending();
				List<Alert> alerts = home.getAlert(query);
				for(Alert alert: alerts) {
					deletedDataItems.add(
						this.datatypeMapper.toObjectId(alert)
					);
				}
			}
			// Notes
			else if(folder instanceof DocumentFolder) {
				DocumentFolder documentFolder = (DocumentFolder)folder;
				Set<Path> deletedDocuments = new HashSet<Path>();
				// Get documents where folder assignment is disabled
				DocumentQuery query = (DocumentQuery)pm.newQuery(Document.class);
				query.thereExistsDocumentFolderAssignment().thereExistsDisabled().isTrue();
				query.thereExistsDocumentFolderAssignment().thereExistsDocumentFolder().equalTo(documentFolder);
				query.thereExistsDocumentFolderAssignment().modifiedAt().greaterThan(since);
				query.thereExistsDocumentFolderAssignment().modifiedAt().lessThanOrEqualTo(to);
				query.thereExistsContentType().equalTo(NOTE_MIME_TYPE);
				query.orderByModifiedAt().ascending();
				String providerName = documentFolder.refGetPath().get(2);
				String segmentName = documentFolder.refGetPath().get(4);
				org.opencrx.kernel.document1.jmi1.Segment documentSegment = 
					(org.opencrx.kernel.document1.jmi1.Segment)pm.getObjectById(
						new Path("xri://@openmdx*org.opencrx.kernel.document1").getDescendant("provider", providerName, "segment", segmentName)
					);				
				List<Document> documents = documentSegment.getDocument(query);
				for(Document document: documents) {
					if(!deletedDocuments.contains(document.refGetPath())) {
						deletedDataItems.add(
							this.datatypeMapper.toObjectId(document)
						);
						deletedDocuments.add(document.refGetPath());
					}
				}
			}
			pm.close();
		}
		return deletedDataItems;
    }

	@Override
    public String createOrUpdateFolder(
		RequestContext requestContext,
    	SyncFolder folder
    ) {
		// Creation or update of folders is not supported.
		// Folders must be managed on the user home's AirSync profile.
		return null;
    }

	@Override
    public String deleteFolder(
		RequestContext requestContext,
    	String folderId
    ) {
		// Deletion of folders is not supported.
		// Folders must be managed on the user home's AirSync profile.
	    return null;
    }

	@Override 
	public boolean folderIsValid(
		RequestContext requestContext,
		String profileName,
		SyncCollection collection
	) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);		
		UserHome user = this.getUserHome(pm, requestContext.getUserId());
		RefObject_1_0 folder = this.findFolder(
			user,
			profileName,
			collection,
			false, // forAddDelete 
			false // forChange
		);
		return folder != null;
	}
	
    public List<SyncFolder> getFolders(
    	RequestContext requestContext,
    	String profileName,
    	String syncKey,
    	boolean activeFoldersOnly
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		List<SyncFolder> changedFolders = new ArrayList<SyncFolder>();
		if(pm != null) {
			Date since = new Date(Long.valueOf(syncKey));
			UserHome user = this.getUserHome(pm, requestContext.getUserId()); 
			AirSyncProfile profile = this.getSyncProfile(user, profileName);
			if(profile != null) {
				SyncFolder syncFolder = new SyncFolder();				
				SyncFeedQuery feedQuery = (SyncFeedQuery)pm.newQuery(SyncFeed.class);
				feedQuery.modifiedAt().greaterThanOrEqualTo(since);
				if(activeFoldersOnly) {
					feedQuery.thereExistsIsActive().isTrue();
				} else {
					feedQuery.forAllIsActive().isFalse();					
				}
				List<SyncFeed> feeds = profile.getFeed(feedQuery);
				// Create sync folders for configured feeds
				for(SyncFeed feed: feeds) {
					// Activity filters
					if(feed instanceof ActivityFilterCalendarFeed) {
						// Meeting
						syncFolder = new SyncFolder();
						syncFolder.setServerId(this.datatypeMapper.toObjectId(feed) + "?type=" + ActivityClass.MEETING.getValue());
						syncFolder.setFolderType(
							feed.getName().endsWith(Base.PRIVATE_SUFFIX) ?
								FolderType.DEFAULT_CALENDAR_FOLDER :
									FolderType.USER_CREATED_CALENDAR_FOLDER
						);
						syncFolder.setDisplayName(feed.getName() + " - Calendar");
						syncFolder.setParentId("0");
						changedFolders.add(syncFolder);
						// EMail
						syncFolder = new SyncFolder();
						syncFolder.setServerId(this.datatypeMapper.toObjectId(feed) + "?type=" + ActivityClass.EMAIL.getValue());
						syncFolder.setFolderType(FolderType.USER_CREATED_EMAIL_FOLDER);
						syncFolder.setDisplayName(feed.getName() + " - Mails");
						syncFolder.setParentId("0");
						changedFolders.add(syncFolder);
						// Tasks
						syncFolder = new SyncFolder();
						syncFolder.setServerId(this.datatypeMapper.toObjectId(feed) + "?type=" + ActivityClass.TASK.getValue());
						syncFolder.setFolderType(
							feed.getName().endsWith(Base.PRIVATE_SUFFIX) ?
								FolderType.DEFAULT_TASKS_FOLDER :
									FolderType.USER_CREATED_TASKS_FOLDER
						);
						syncFolder.setDisplayName(feed.getName() + " - Tasks");
						syncFolder.setParentId("0");
						changedFolders.add(syncFolder);						
					}
					// Activity groups
					else if(feed instanceof ActivityGroupCalendarFeed) {
						ActivityGroupCalendarFeed activityGroupFeed = (ActivityGroupCalendarFeed)feed;
						ActivityGroup activityGroup = activityGroupFeed.getActivityGroup();
						Collection<ActivityCreator> activityCreators = activityGroup.getActivityCreator();
						// Create a folder for each creator
						for(ActivityCreator activityCreator: activityCreators) {
							int activityClass = activityCreator.getActivityType().getActivityClass();
							if(activityClass == ActivityClass.MEETING.getValue()) {
								syncFolder = new SyncFolder();
								syncFolder.setServerId(this.datatypeMapper.toObjectId(feed) + "?type=" + ActivityClass.MEETING.getValue());
								syncFolder.setFolderType(
									feed.getName().endsWith(Base.PRIVATE_SUFFIX) ?
										FolderType.DEFAULT_CALENDAR_FOLDER :
											FolderType.USER_CREATED_CALENDAR_FOLDER
								);
								syncFolder.setDisplayName(feed.getName() + " - Calendar");
								syncFolder.setParentId("0");
								changedFolders.add(syncFolder);
							} else if(activityClass == ActivityClass.EMAIL.getValue()) {
								syncFolder = new SyncFolder();
								syncFolder.setServerId(this.datatypeMapper.toObjectId(feed) + "?type=" + ActivityClass.EMAIL.getValue());
								syncFolder.setFolderType(FolderType.USER_CREATED_EMAIL_FOLDER);
								syncFolder.setDisplayName(feed.getName() + " - Mails");
								syncFolder.setParentId("0");
								changedFolders.add(syncFolder);
							} else if(activityClass == ActivityClass.TASK.getValue()) {
								syncFolder = new SyncFolder();
								syncFolder.setServerId(this.datatypeMapper.toObjectId(feed) + "?type=" + ActivityClass.TASK.getValue());
								syncFolder.setFolderType(
									feed.getName().endsWith(Base.PRIVATE_SUFFIX) ?
										FolderType.DEFAULT_TASKS_FOLDER :
											FolderType.USER_CREATED_TASKS_FOLDER
								);
								syncFolder.setDisplayName(feed.getName() + " - Tasks");
								syncFolder.setParentId("0");
								changedFolders.add(syncFolder);
							}
						}
					}
					// Contacts
					else if(feed instanceof ContactsFeed) {
						syncFolder = new SyncFolder();
						syncFolder.setServerId(this.datatypeMapper.toObjectId(feed));
						syncFolder.setFolderType(
							feed.getName().endsWith(Base.PRIVATE_SUFFIX) ?
								FolderType.DEFAULT_CONTACTS_FOLDER :
									FolderType.USER_CREATED_CONTACTS_FOLDER
						);
						syncFolder.setDisplayName(feed.getName() + " - Contacts");
						syncFolder.setParentId("0");
						changedFolders.add(syncFolder);						
					}
					// Notes
					else if(feed instanceof DocumentFeed) {
						syncFolder = new SyncFolder();
						syncFolder.setServerId(this.datatypeMapper.toObjectId(feed));
						syncFolder.setFolderType(
							feed.getName().endsWith(Documents.PRIVATE_DOCUMENTS_FOLDER_SUFFIX) ?
								FolderType.DEFAULT_NOTES_FOLDER :
									FolderType.USER_CREATED_NOTES_FOLDER
						);
						syncFolder.setDisplayName(feed.getName() + " - Notes");
						syncFolder.setParentId("0");
						changedFolders.add(syncFolder);						
					}
				}
				// User home as feed for alerts
				if(activeFoldersOnly && user.getModifiedAt().compareTo(since) > 0) {
					syncFolder = new SyncFolder();
					syncFolder.setServerId(this.datatypeMapper.toObjectId(user));
					syncFolder.setFolderType(FolderType.USER_CREATED_EMAIL_FOLDER);
					syncFolder.setDisplayName(user.refGetPath().getBase() + " - Alerts");
					syncFolder.setParentId("0");
					changedFolders.add(syncFolder);
				}
				// Some clients require the default folders to be returned in FolderSync. 
				// Otherwise the default folders are treated as inactive folders, 
				// i.e. findFolder() returns null and therefore getChangedDataItems() always
				// returns an empty item list and item creations and updates are not supported. 
				if(activeFoldersOnly && since.getTime() == 0) {
					for(SyncFolder defaultFolder: DEFAULT_FOLDERS.values()) {
						changedFolders.add(defaultFolder);
					}
				}
			}
			pm.close();
		}
		return changedFolders;
    }
	
	@Override
    public List<SyncFolder> getChangedFolders(
		RequestContext requestContext,
		String profileName,
    	String syncKey
    ) throws ServiceException {
		return this.getFolders(
			requestContext,
			profileName,
			syncKey, 
			true // activeFoldersOnly
		);
    }

	@Override
    public List<SyncFolder> getDeletedFolders(
		RequestContext requestContext,
		String profileName,
    	String syncKey
    ) throws ServiceException {
		return this.getFolders(
			requestContext,
			profileName,
			syncKey, 
			false // activeFoldersOnly
		);
    }

	@Override
    public void sendMail(
		RequestContext requestContext,
    	InputStream mimeMessage
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
        Transport transport = null;		
		if(pm != null) {
			UserHome user = this.getUserHome(pm, requestContext.getUserId());
			if(user != null) {
				String providerName = user.refGetPath().get(2);
				String segmentName = user.refGetPath().get(4);
				MimeMessage message = null;
				// Send EMail if mail session is configured and available
				try {
	                Context initialContext = new InitialContext();				
			        Session session = null;
			        String mailServiceName = "/mail/provider/" + providerName + "/segment/" + segmentName;		        
			        try {
			            session = (Session)initialContext.lookup("java:comp/env" + mailServiceName);
			        } catch(Exception e) {    
			        	SysLog.detail("Mail service not found", mailServiceName);
			            // Fallback to mail/provider/<provider>
			            mailServiceName = "/mail/provider/" + providerName;
			            SysLog.detail("Fall back to mail service", mailServiceName);
			            session = (Session)initialContext.lookup("java:comp/env" + mailServiceName);
			        }			
					message = new MimeMessage(session, mimeMessage);	
		            transport = session.getTransport();
		            String protocol = transport.getURLName().getProtocol();
		            String port = session.getProperty("mail." + protocol + ".port");
		            transport.connect(
		                session.getProperty("mail." + protocol + ".host"), 
		                port == null ? -1 : Integer.valueOf(port).intValue(),                           
		                session.getProperty("mail." + protocol + ".user"), 
		                session.getProperty("mail." + protocol + ".password")
		            );
		            transport.sendMessage(
		                message,
		                message.getAllRecipients()
		            );		            
				} catch(Exception e) {
					pm.close();
					throw new ServiceException(e);
				} finally {
		            if(transport != null) {
		                try {
		                    transport.close();
		                } catch(Exception e) {}
		            }
		        }
				// Import EMail
				try {
					ActivityCreator emailCreator = (ActivityCreator)pm.getObjectById(
						new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/" + providerName + "/segment/" + segmentName + "/activityCreator/" + user.refGetPath().getBase() + "~E-Mails")
					);
					if(emailCreator != null && message != null) {
						Activities.getInstance().importMimeMessage(
							pm,
							providerName, 
							segmentName, 
							message, 
							emailCreator 
						);
					}
				} catch(Exception e) {
					pm.close();
					throw new ServiceException(e);
				} finally {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e1) {}
		        }
			}
			pm.close();
		}
    }

	@Override
    public File getContextTempDir(
		RequestContext requestContext,
    	File tempDir
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);		
		UserHome user = this.getUserHome(pm, requestContext.getUserId());
		if(user != null) {
			String providerName = user.refGetPath().get(2);
			String segmentName = user.refGetPath().get(4);
			return new File(tempDir, providerName + File.separator + segmentName);
		} else {
			return tempDir;
		}
    }

	@Override
    public ClientProfile getClientProfile(
		RequestContext requestContext,
    	String profileName
    ) throws ServiceException {
		ClientProfile clientProfile = new ClientProfile();
		String userId = requestContext.getUserId();
		clientProfile.setUserId(userId);
		clientProfile.setName(profileName);
		clientProfile.setFolders(new ArrayList<ClientProfile.Folder>());		
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		if(pm != null) {
			UserHome user = this.getUserHome(pm, userId);
			AirSyncClientProfileQuery query = (AirSyncClientProfileQuery)pm.newQuery(AirSyncClientProfile.class);
			query.name().equalTo(profileName);
			List<SyncProfile> profiles = user.getSyncProfile(query);
			if(!profiles.isEmpty()) {
				AirSyncClientProfile syncProfile = (AirSyncClientProfile)profiles.iterator().next();
				Properties properties = new Properties();
				if(syncProfile.getDescription() != null) {
					try {
						properties.load(new StringReader(syncProfile.getDescription()));
						clientProfile.setUserAgent(properties.getProperty("UserAgent"));
						clientProfile.setPolicyKey(properties.getProperty("PolicyKey"));
					} catch(Exception e) {}
				}
				// Folders including sync keys and id mapping
				SyncFeedQuery feedQuery = (SyncFeedQuery)pm.newQuery(SyncFeed.class);
				feedQuery.thereExistsIsActive().isTrue();
				feedQuery.orderByName().ascending();
				List<SyncFeed> syncFeeds = syncProfile.getFeed(feedQuery);
				for(SyncFeed syncFeed: syncFeeds) {
					String clientId = this.datatypeMapper.toObjectId(syncFeed);
					if(clientId != null) {
						String description = syncFeed.getDescription();
						if(description != null) {
							try {
								properties = new Properties();
								properties.load(new StringReader(description));
								String serverId = properties.getProperty("ServerId");
								FolderType folderType = FolderType.valueOf(properties.getProperty("Type"));
								String generation = properties.getProperty("Generation");
								String syncKeyServer = properties.getProperty("SyncKey.Server");
								String syncKeyClient = properties.getProperty("SyncKey.Client");
								String parentId = properties.getProperty("ParentId");
								ClientProfile.Folder folder = new ClientProfile.Folder();
								folder.setName(syncFeed.getName());
								folder.setGeneration(
									generation == null ? 0 : Integer.valueOf(generation)
								);
								folder.setClientId(clientId);
								folder.setServerId(serverId == null ? null : serverId.trim());
								folder.setParentId(parentId == null ? null : parentId.trim());
								folder.setSyncKeyClient(syncKeyClient == null ? null : syncKeyClient.trim());
								folder.setSyncKeyServer(syncKeyServer == null ? null : syncKeyServer.trim());
								folder.setType(folderType);
								Properties idMapping = new Properties();
								Collection<org.opencrx.kernel.home1.jmi1.SyncData> syncDatas = syncFeed.getSyncData();
								for(org.opencrx.kernel.home1.jmi1.SyncData syncData: syncDatas) {
									if("Mapping".equals(syncData.getTitle())) {
										if(syncData.getText() != null) {
											idMapping.load(
												new StringReader(syncData.getText())
											);
											break;
										}												
									}
								}
								folder.setIdMap(idMapping);
								clientProfile.getFolders().add(folder);
							} catch(Exception e) {
								new ServiceException(e).log();
							}
						}
					}
				}
			}
		}
		return clientProfile;
    }

	@Override
    public void updateClientProfile(
		RequestContext requestContext,
    	ClientProfile clientSyncProfile,
    	Set<String> folderIds,
    	boolean noSyncKeys,
    	boolean noMappings
    ) throws ServiceException {
		PersistenceManager pm = this.newPersistenceManager(requestContext);
		if(pm != null) {
			UserHome user = this.getUserHome(pm, clientSyncProfile.getUserId());
			AirSyncClientProfileQuery profileQuery = (AirSyncClientProfileQuery)pm.newQuery(AirSyncClientProfile.class);
			profileQuery.name().equalTo(clientSyncProfile.getName());
			List<SyncProfile> profiles = user.getSyncProfile(profileQuery);
			if(!profiles.isEmpty()) {				
				AirSyncClientProfile syncProfile = (AirSyncClientProfile)profiles.iterator().next();
				Properties properties = new Properties();
				try {
					properties.load(
						new StringReader(
							syncProfile.getDescription() == null ? "" : syncProfile.getDescription()
						)
					);
					if(clientSyncProfile.getUserAgent() != null) {
						properties.setProperty("UserAgent", clientSyncProfile.getUserAgent());
					}
					if(clientSyncProfile.getPolicyKey() != null) {
						properties.setProperty("PolicyKey", clientSyncProfile.getPolicyKey());
					}
					ByteArrayOutputStream propertiesAsText = new ByteArrayOutputStream();
					properties.store(
						propertiesAsText,
						syncProfile.getName() 
					);
					propertiesAsText.close();
					pm.currentTransaction().begin();
					syncProfile.setDescription(
						propertiesAsText.toString("ISO8859-1")
					);
					pm.currentTransaction().commit();
				} 
				catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e1) {}
				}
				Collection<SyncFeed> feeds = syncProfile.getFeed();
				// Get feeds by serverId
				Map<String,SyncFeed> feedsById = new HashMap<String,SyncFeed>();
				for(SyncFeed feed: feeds) {
					try {
						properties = new Properties();
						properties.load(
							new StringReader(
								feed.getDescription() == null ? "" : feed.getDescription()
							)
						);
						String serverId = properties.getProperty("ServerId");
						if(serverId != null) {
							feedsById.put(
								serverId.trim(),
								feed
							);
						}
					} catch(Exception e) {
						new ServiceException(e).log();
					}						
				}
				for(ClientProfile.Folder folder: clientSyncProfile.getFolders()) {
					if(folderIds == null || folderIds.contains(folder.getClientId())) {
						SyncFeed feed = feedsById.get(folder.getServerId());
						if(feed != null) {
							// Update sync keys
							if(!noSyncKeys) {
								try {
									properties = new Properties();
									properties.load(
										new StringReader(
											feed.getDescription() == null ? "" : feed.getDescription()
										)
									);
									properties.setProperty("Generation", Integer.toString(folder.getGeneration()));
									if(folder.getSyncKeyServer() == null) {
										properties.remove("SyncKey.Server");
									}
									else if(!"0".equals(folder.getSyncKeyServer())) {
										properties.setProperty("SyncKey.Server", folder.getSyncKeyServer());
									}
									if(folder.getSyncKeyClient() == null) {
										properties.remove("SyncKey.Client");
									}
									else {
										properties.setProperty("SyncKey.Client", folder.getSyncKeyClient());
									}
									ByteArrayOutputStream propertiesAsText = new ByteArrayOutputStream();
									properties.store(
										propertiesAsText,
										feed.getName() 
									);
									propertiesAsText.close();
									SysLog.log(Level.FINE, "Storing properties {0} for folder {1}", properties, folder.getName());
									pm.currentTransaction().begin();
									feed.setDescription(
										propertiesAsText.toString("ISO8859-1")
									);
									pm.currentTransaction().commit();
								} catch(Exception e) {
									new ServiceException(e).log();
									try {
										pm.currentTransaction().rollback();
									} catch(Exception e0) {}
								}
							}
							// Update id mapping
							if(!noMappings) {
								try {
									Collection<SyncData> syncDatas = feed.getSyncData();
									org.opencrx.kernel.home1.jmi1.SyncData mappingData = null;
									for(org.opencrx.kernel.home1.jmi1.SyncData syncData: syncDatas) {
										if("Mapping".equals(syncData.getTitle())) {
											mappingData = syncData;
											break;
										}
									}
									pm.currentTransaction().begin();
									if(mappingData == null) {
										mappingData = pm.newInstance(org.opencrx.kernel.home1.jmi1.SyncData.class);
										mappingData.setTitle("Mapping");
										feed.addSyncData(
											Base.getInstance().getUidAsString(),
											mappingData
										);
									}
									ByteArrayOutputStream idMappingAsText = new ByteArrayOutputStream();
									folder.getIdMap().store(idMappingAsText, "Mapping");
									idMappingAsText.close();
									mappingData.setText(idMappingAsText.toString("ISO8859-1"));
									pm.currentTransaction().commit();
								} catch(Exception e) {
									new ServiceException(e).log();
									try {
										pm.currentTransaction().rollback();
									} catch(Exception e0) {}
								}
							}
						}
						// Create new feed
						else {
							FolderType folderType = folder.getType();
							switch(folderType) {
								case DEFAULT_TASKS_FOLDER:
								case DEFAULT_CALENDAR_FOLDER:
								case USER_CREATED_CALENDAR_FOLDER:
								case USER_CREATED_TASKS_FOLDER:
								case USER_CREATED_EMAIL_FOLDER:
									feed = pm.newInstance(org.opencrx.kernel.home1.jmi1.ActivityGroupCalendarFeed.class);
									break;
								case DEFAULT_CONTACTS_FOLDER:
								case USER_CREATED_CONTACTS_FOLDER:
									feed = pm.newInstance(org.opencrx.kernel.home1.jmi1.ContactsFeed.class);
									break;
							}
							if(feed != null) {
								feed.setName(folder.getName());		
								feed.setActive(false);
								feed.setAllowAddDelete(true);
								feed.setAllowChange(true);
								properties = new Properties();
								properties.setProperty("ServerId", folder.getServerId());
								properties.setProperty("ParentId", folder.getParentId());
								properties.setProperty("Type", folderType.toString());
								ByteArrayOutputStream propertiesAsText = new ByteArrayOutputStream();
								try {
									properties.store(
										propertiesAsText,
										folder.getName() 
									);
									propertiesAsText.close();
									feed.setDescription(
										propertiesAsText.toString("ISO8859-1")
									);
									pm.currentTransaction().begin();
									syncProfile.addFeed(
										Base.getInstance().getUidAsString(),
										feed
									);
									pm.currentTransaction().commit();
								} catch(Exception e) {
									new ServiceException(e).log();
									try {
										pm.currentTransaction().rollback();
									} catch(Exception e0) {}
								}
							}
						}
					}
				}
			}
		}
    }
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private static final String INITIAL_SYNC_KEY = "0";
	private static final String FIRST_SYNC_KEY = "1";
	private static final String NOTE_MIME_TYPE = "text/plain";
	private static final Map<String,SyncFolder> DEFAULT_FOLDERS;

	static {
		DEFAULT_FOLDERS = new HashMap<String,SyncFolder>();
		// Sent Items
		SyncFolder syncFolder = new SyncFolder();
		syncFolder.setServerId("3");
		syncFolder.setFolderType(FolderType.DEFAULT_DELETED_ITEMS_FOLDERS);
		syncFolder.setDisplayName("Deleted Items");
		syncFolder.setParentId("0");
		DEFAULT_FOLDERS.put("3", syncFolder);					
		// Drafts
		syncFolder = new SyncFolder();
		syncFolder.setServerId("4");
		syncFolder.setFolderType(FolderType.DEFAULT_DRAFTS_FOLDERS);
		syncFolder.setDisplayName("Drafts");
		syncFolder.setParentId("0");
		DEFAULT_FOLDERS.put("4", syncFolder);					
		// Inbox
		syncFolder = new SyncFolder();
		syncFolder.setServerId("5");
		syncFolder.setFolderType(FolderType.DEFAULT_INBOX_FOLDER);
		syncFolder.setDisplayName("Inbox");
		syncFolder.setParentId("0");
		DEFAULT_FOLDERS.put("5", syncFolder);
		// Outbox
		syncFolder = new SyncFolder();
		syncFolder.setServerId("9");
		syncFolder.setFolderType(FolderType.DEFAULT_OUTBOX_FOLDER);
		syncFolder.setDisplayName("Outbox");
		syncFolder.setParentId("0");
		DEFAULT_FOLDERS.put("9", syncFolder);					
		// Sent Items
		syncFolder = new SyncFolder();
		syncFolder.setServerId("10");
		syncFolder.setFolderType(FolderType.DEFAULT_SENT_EMAIL_FOLDER);
		syncFolder.setDisplayName("Sent Items");
		syncFolder.setParentId("0");
		DEFAULT_FOLDERS.put("10", syncFolder);									
	}
		
	private final PersistenceManagerFactory pmf;
	private final String providerName;
	private final DatatypeMapper datatypeMapper;

}
