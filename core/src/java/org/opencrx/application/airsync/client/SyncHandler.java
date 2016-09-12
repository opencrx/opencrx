/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AirSync Client SyncHandler
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.client;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;

import javax.xml.transform.stream.StreamResult;

import org.opencrx.application.airsync.backend.cci.ClientProfile;
import org.opencrx.application.airsync.backend.cci.GetChangedDataItemsResult;
import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.AttendeeT;
import org.opencrx.application.airsync.datatypes.ContactT;
import org.opencrx.application.airsync.datatypes.DataFormatFactory;
import org.opencrx.application.airsync.datatypes.DataType;
import org.opencrx.application.airsync.datatypes.EmailT;
import org.opencrx.application.airsync.datatypes.EventT;
import org.opencrx.application.airsync.datatypes.FolderType;
import org.opencrx.application.airsync.datatypes.IData;
import org.opencrx.application.airsync.datatypes.IDataFormat;
import org.opencrx.application.airsync.datatypes.SyncCollection;
import org.opencrx.application.airsync.datatypes.SyncDataItem;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.opencrx.application.airsync.utils.WbXMLTransformer;
import org.openmdx.base.exception.ServiceException;

public class SyncHandler extends AbstractClientHandler {

	public SyncHandler(
		SyncBackend backend
	) {
		this(
			backend,
			DEFAULT_BATCH_SIZE
		);
	}
	
	public SyncHandler(
		SyncBackend backend,
		int batchSize
	) {
		super(backend);
		this.batchSize = batchSize;		
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.airsync.client.ClientHandler#handle(org.opencrx.application.airsync.client.ClientHandler.SyncTarget, java.lang.String, java.lang.String, java.lang.Object)
	 */
	@Override
    public void handle(
    	SyncTarget target,
    	String userId,
    	String profileName,
    	Object context
    ) throws ServiceException {
		try {
			SyncBackend backend = this.getBackend();
			RequestContext requestContext = backend.newRequestContext(userId, context);
			ClientProfile clientProfile = backend.getClientProfile(
				requestContext,
				profileName
			);
			for(ClientProfile.Folder folder: clientProfile.getFolders()) {
				String collectionId = folder.getServerId();
				String folderId = folder.getClientId();
				FolderType folderType = folder.getType();
		    	ClientProfile.Folder.ItemIdMappings itemIdMappings = folder.getItemIdMappings();
				if(collectionId != null && collectionId.length() > 0 ) {
					DataType collectionType = null;
					switch(folderType) {
						case DEFAULT_TASKS_FOLDER:
						case USER_CREATED_TASKS_FOLDER:
							collectionType = DataType.Tasks;
							break;
						case DEFAULT_CALENDAR_FOLDER:
						case USER_CREATED_CALENDAR_FOLDER:
							collectionType = DataType.Calendar;
							break;
						case DEFAULT_CONTACTS_FOLDER:
						case USER_CREATED_CONTACTS_FOLDER:
							collectionType = DataType.Contacts;
							break;
						case USER_CREATED_EMAIL_FOLDER:
							collectionType = DataType.Email;
							break;
					}
					if(collectionType != null) {						
						// Get changed and deleted items
						String syncKeyClient = folder.getSyncKeyClient();
						List<SyncDataItem> changedDataItems = null;
						List<String> deletedItemIds = null;
						// Send changed items only if not sync init
						if(folder.getSyncKeyServer() != null && !folder.getSyncKeyServer().isEmpty() && !"0".equals(folder.getSyncKeyServer())) {
							changedDataItems = new ArrayList<SyncDataItem>();
					    	// Increment folder generation in case of initial sync. This 
					    	// migrates the clientId -> serverId mappings to the new generation
					    	if(syncKeyClient == null || "0".equals(syncKeyClient)) {
					    		folder.setSyncKeyClient(
					    			syncKeyClient = "0"
					    		);
					    		folder.setGeneration(folder.getGeneration() + 1);
					    	}							
							// Get changed and deleted items from backend
					    	org.opencrx.application.airsync.datatypes.SyncCollection clientCollection = 
					    		new org.opencrx.application.airsync.datatypes.SyncCollection();
					    	clientCollection.setCollectionId(folderId);
					    	clientCollection.setSyncKey(syncKeyClient);					    	
					    	clientCollection.setDataType(collectionType);					    	
					    	Set<String> excludes = new HashSet<String>();
							logger.log(Level.FINE, "Get new items for client collection {0} and syncKey {1}", new String[]{clientCollection.getCollectionId(), syncKeyClient});					    	
					    	GetChangedDataItemsResult getNewDataItemsResult = backend.getChangedDataItems(
					    		requestContext,
					    		profileName,
					    		clientCollection,
					    		false, // noData
					    		this.batchSize, // maxItems
					    		SyncDataItem.State.NEW,
					    		excludes
					    	);
					    	for(List<SyncDataItem> dataItems: getNewDataItemsResult.getDataItems().values()) {
						    	for(SyncDataItem dataItem: dataItems) {
						    		excludes.add(dataItem.getServerId());
						    	}
					    	}
							logger.log(Level.FINE, "Get changed items for client collection {0} and syncKey {1}", new String[]{clientCollection.getCollectionId(), syncKeyClient});					    	
					    	GetChangedDataItemsResult getChangedDataItemsResult = backend.getChangedDataItems(
					    		requestContext,
					    		profileName,
					    		clientCollection,
					    		false, // noData
					    		this.batchSize, // maxItems
					    		SyncDataItem.State.MODIFIED,
					    		excludes
					    	);
					    	syncKeyClient = getNewDataItemsResult.getDataItems().isEmpty() ?
					    		getChangedDataItemsResult.getSyncKey() :
					    			getChangedDataItemsResult.getDataItems().isEmpty() ?
					    				getNewDataItemsResult.getSyncKey() :
					    					getNewDataItemsResult.getSyncKey().compareTo(getChangedDataItemsResult.getSyncKey()) < 0 ?
					    						getNewDataItemsResult.getSyncKey() :
					    							getChangedDataItemsResult.getSyncKey();
					    	// Add new items to changedDataItems with syncKey <= syncKeyClient
					    	for(Map.Entry<String,List<SyncDataItem>> entries: getNewDataItemsResult.getDataItems().entrySet()) {
					    		if(entries.getKey().compareTo(syncKeyClient) <= 0) {
					    			changedDataItems.addAll(entries.getValue());
					    		}
					    	}
					    	// Add changed items to changedDataItems with syncKey <= syncKeyClient
					    	for(Map.Entry<String,List<SyncDataItem>> entries: getChangedDataItemsResult.getDataItems().entrySet()) {
					    		if(entries.getKey().compareTo(syncKeyClient) <= 0) {
					    			changedDataItems.addAll(entries.getValue());
					    		}
					    	}
					    	logger.log(Level.FINE, "Next SyncKey.Client is {0}", syncKeyClient);					    	
					    	deletedItemIds = backend.getDeletedDataItems(
					    		requestContext,
					    		profileName,
					    		clientCollection,
					    		syncKeyClient
					    	);
					    	// If nothing is new and changed then delete items of old generations
					    	if(changedDataItems.isEmpty()) {
					    		for(ClientProfile.Folder.ItemIdMapping mapping: itemIdMappings.getOldMappings()) {	
					    			deletedItemIds.add(
						    			mapping.getClientId()
						    		);
					    		}
					    	}
						}						
						logger.log(Level.FINE, "Number of changed data items is {0}", (changedDataItems == null ? 0 : changedDataItems.size()));					    	
						// In case the size of changedDataItems is greater than the batch 
						// size we have to send the changes in batches. Some servers such
						// as MS Exchange may otherwise reject the request if it is too large
						int slicePos = 0;						
						List<SyncDataItem> changedDataItemsSlice = changedDataItems == null ?
							Collections.<SyncDataItem>emptyList() :
								changedDataItems.subList(
									slicePos, 
									Math.min(slicePos + this.batchSize, changedDataItems.size())
								);
						// All deleted items are sent with first batch
						List<String> deletedItemIdsSlice = deletedItemIds == null ?
							Collections.<String>emptyList() :
								deletedItemIds;
						deletedItemIds = null;
						while(true) {
							String syncKeyServer = folder.getSyncKeyServer();
							org.w3c.dom.Document docRequest = DOMUtils.createDoc(
								"AirSync:", 
								"Sync",
								new String[]{"xmlns:Email", "POOMMAIL:"},
								new String[]{"xmlns:Email2", "POOMMAIL2:"},
								new String[]{"xmlns:Contacts", "POOMCONTACTS:"},
								new String[]{"xmlns:Contacts2", "POOMCONTACTS2:"},
								new String[]{"xmlns:Tasks", "POOMTASKS:"},
								new String[]{"xmlns:Calendar", "POOMCAL:"},
								new String[]{"xmlns:AirSyncBase", "AirSyncBase:"}
							);
							org.w3c.dom.Element eRoot = docRequest.getDocumentElement();
							org.w3c.dom.Element eCollections = DOMUtils.createElement(eRoot, null, "Collections");
							org.w3c.dom.Element eCollection = DOMUtils.createElement(eCollections, null, "Collection");
							DOMUtils.createElementAndText(eCollection, null, "Class", collectionType.toString());
							DOMUtils.createElementAndText(eCollection, null, "SyncKey", (syncKeyServer == null || syncKeyServer.isEmpty() ? "0" : syncKeyServer));		
							DOMUtils.createElementAndText(eCollection, null, "CollectionId", collectionId);
							// Only send commands if not sync init
							if(syncKeyServer != null && !folder.getSyncKeyServer().isEmpty() && !"0".equals(syncKeyServer)) {
								DOMUtils.createElement(eCollection, null, "GetChanges");
								DOMUtils.createElementAndText(eCollection, null, "WindowSize", Integer.toString(this.batchSize));
					    		// Generate commands for changed and deleted items
						    	if(
						    		!changedDataItemsSlice.isEmpty() || 
						    		!deletedItemIdsSlice.isEmpty()
						    	) {
						    		IDataFormat dataFormat = DataFormatFactory.getXmlFormat(collectionType);
									org.w3c.dom.Element eCommands = DOMUtils.createElement(eCollection, null, "Commands");
						    		for(SyncDataItem changedItem: changedDataItemsSlice) {					    				
						    			String clientId = changedItem.getServerId();
						    			String serverId = itemIdMappings.getServerId(clientId);					    			
						    			org.w3c.dom.Element eCommand = null;
						    			// Add item, if we do not have a ServerId
						    			if(serverId == null) {
							    			// AirSync does not allow adding of new Emails
							    			if(collectionType != DataType.Email) {
							    				eCommand = DOMUtils.createElement(eCommands, null, "Add");
												DOMUtils.createElementAndText(eCommand, null, "ClientId", clientId);
							    			}
						    			}
						    			// Change it
						    			else {
						    				// Update mapping. This moves mapping to current generation
						    				itemIdMappings.updateMappings(clientId, serverId);
							    			// AirSync does not allow changing of existing of Emails
							    			if(collectionType != DataType.Email) {					    				
							    				eCommand = DOMUtils.createElement(eCommands, null, "Change");
												DOMUtils.createElementAndText(eCommand, null, "ServerId", serverId);
							    			}
											// Exchange does not allow to change organizerName and organizerEMail
											if(changedItem.getData() instanceof EventT) {
												EventT eventT = (EventT)changedItem.getData();
												eventT.setOrganizerName(null);
												eventT.setOrganizerEmail(null);
											}
						    			}
						    			if(eCommand != null) { 
											org.w3c.dom.Element eApplicationData = DOMUtils.createElement(eCommand, null, "ApplicationData");
											if(changedItem.getData() instanceof EventT) {
												EventT eventT = (EventT)changedItem.getData();
												// Exchange requires a timezone
												eventT.setTimezone("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoAAAAFAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAAAFAAIAAAAAAAAAAAAAAA=="); // Bias = 0 minutes
												// Exchange: Status and type of attendees are not modifiable
												for(AttendeeT attendeeT: eventT.getAttendees()) {
													attendeeT.setAttendeeStatus(null);
													attendeeT.setAttendeeType(null);
												}
											}
											else if(changedItem.getData() instanceof ContactT) {
												ContactT contactT = (ContactT)changedItem.getData();
												// Alias must not be sent to server. According to spec:
												// The <Alias> element is only returned in a recipient information cache request
												contactT.setAlias(null);
											}
							    			dataFormat.format(
							    				eApplicationData,
							    				changedItem.getData(),
							    				2.5 // protocolVersion
							    			);
						    			}
						    		}
						    		for(String itemId: deletedItemIdsSlice) {
						    			String serverId = itemIdMappings.getServerId(itemId);
						    			if(serverId != null) {
						    				itemIdMappings.removeAllMappingsByClientId(itemId);
							    			org.w3c.dom.Element eCommand = DOMUtils.createElement(eCommands, null, "Delete");
						    				DOMUtils.createElementAndText(eCommand, null, "ServerId", serverId);
						    			}
						    		}
						    	}
					    	}
				    		// Invoke
							logger.log(Level.FINE, "Syncing with server for collection {0}", collectionId);				    	
					    	org.w3c.dom.Document docResponse = (org.w3c.dom.Document)target.perform(
					    		"Sync", 
					    		clientProfile.getPolicyKey() == null ? "0" : clientProfile.getPolicyKey(),
					    		clientProfile.getUserAgent(),
					    		docRequest
					    	);
				    		// Response
					    	eRoot = docResponse.getDocumentElement();
							org.w3c.dom.NodeList lCollection = eRoot.getElementsByTagNameNS("AirSync:", "Collection");
							for(int i = 0; i < lCollection.getLength(); i++) {
								try {
									eCollection = (org.w3c.dom.Element)lCollection.item(i);
									SyncCollection responseCollection = SyncCollection.decode("AirSync:", eCollection);
									if(collectionId.equals(responseCollection.getCollectionId())) {
										logger.log(Level.FINE, "Processing response for collection {0}. SyncKey={1}", new String[]{collectionId, responseCollection.getSyncKey()});
										// Commands
										org.w3c.dom.Element eCommands = DOMUtils.getUniqueElement(eCollection, "AirSync:", "Commands");
										logger.log(Level.FINE, "Processing commands for collection {0}", collectionId);
										if(eCommands != null) {										
											org.w3c.dom.NodeList lCommand = eCommands.getChildNodes();
											for(int j = 0; j < lCommand.getLength(); j++) {							
												org.w3c.dom.Node node = lCommand.item(j);
												if(node instanceof org.w3c.dom.Element) {
													org.w3c.dom.Element eCommand = (org.w3c.dom.Element)node;								
													String command = eCommand.getNodeName();
													String serverId = DOMUtils.getElementText(eCommand, "AirSync:", "ServerId");
													String clientId = DOMUtils.getElementText(eCommand, "AirSync:", "ClientId");
													// Should not happen. However, if clientId is not returned by 
													// server try to map serverId to clientId using id mapping
													if(clientId == null && serverId != null) {
														ClientProfile.Folder.ItemIdMapping mapping = itemIdMappings.getMappingByClientId(serverId);
														if(mapping != null) {
															clientId = mapping.getClientId();
														}
													}
													logger.log(Level.FINE, "Decode element {0}", serverId);
													org.w3c.dom.Element eApplicationData = DOMUtils.getUniqueElement(eCommand, "AirSync:", "ApplicationData");
													IDataFormat dataDecoder = DataFormatFactory.getXmlFormat(responseCollection.getDataType());
													IData data = null;
													if(eApplicationData != null) {
														data = dataDecoder.parse(eApplicationData);
													}
													logger.log(Level.FINE, "Processing element {0}", serverId);
													if(folderId != null) {
														SyncCollection collection = new SyncCollection();
														collection.setCollectionId(folderId);
														collection.setDataType(collectionType);
														if(command.endsWith("Modify")) {
															clientId = backend.createOrUpdateDataItem(
																requestContext,
																profileName,
																collection,
																clientId,
																data														
															);
														}
														else if(command.endsWith("Add")) {
															// Fetch E-Mails as MIME 
															if(collectionType == DataType.Email) {
																org.w3c.dom.Document fetchRequestDoc = DOMUtils.createDoc(
																	"AirSync:", 
																	"Sync",
																	new String[]{"xmlns:Email", "POOMMAIL:"},
																	new String[]{"xmlns:Email2", "POOMMAIL2:"},
																	new String[]{"xmlns:AirSyncBase", "AirSyncBase:"}
																);
																org.w3c.dom.Element eRootFetch = fetchRequestDoc.getDocumentElement();
																org.w3c.dom.Element eCollectionsFetch = DOMUtils.createElement(eRootFetch, null, "Collections");
																org.w3c.dom.Element eCollectionFetch = DOMUtils.createElement(eCollectionsFetch, null, "Collection");
																DOMUtils.createElementAndText(eCollectionFetch, null, "Class", collectionType.toString());
																DOMUtils.createElementAndText(eCollectionFetch, null, "SyncKey", responseCollection.getSyncKey());		
																DOMUtils.createElementAndText(eCollectionFetch, null, "CollectionId", responseCollection.getCollectionId());
																org.w3c.dom.Element eOptionsFetch = DOMUtils.createElement(eCollectionFetch, null, "Options");
																DOMUtils.createElementAndText(eOptionsFetch, null, "MIMESupport", "2");
																org.w3c.dom.Element eCommandsFetch = DOMUtils.createElement(eCollectionFetch, null, "Commands");
																org.w3c.dom.Element eCommandFetch = DOMUtils.createElement(eCommandsFetch, null, "Fetch");
																DOMUtils.createElementAndText(eCommandFetch, null, "ServerId", serverId);
														    	org.w3c.dom.Document fetchResponseDoc = (org.w3c.dom.Document)target.perform(
														    		"Sync",	    		
														    		null,
														    		clientProfile.getUserAgent(),
														    		fetchRequestDoc
														    	);													    	
																if(logger.isLoggable(Level.FINE)) {
																	ByteArrayOutputStream out = new ByteArrayOutputStream();
															    	WbXMLTransformer.transform(
															    		fetchResponseDoc,
															    		new StreamResult(out),
															    		true
															    	);
															    	out.close();
															    	logger.log(Level.FINE, "+-+-+-+-+- Response +-+-+-+-+-");
															    	logger.log(Level.FINE, out.toString());
																}
														    	eRootFetch = fetchResponseDoc.getDocumentElement();
																org.w3c.dom.NodeList lApplicationDataFetch = eRootFetch.getElementsByTagNameNS("AirSync:", "ApplicationData");
																if(lApplicationDataFetch.getLength() > 0) {															
																	org.w3c.dom.Element eApplicationDataFetch = (org.w3c.dom.Element)lApplicationDataFetch.item(0);
																	if(eApplicationDataFetch != null) {
																		EmailT emailT = (EmailT)dataDecoder.parse(eApplicationDataFetch);
																		// Use emailT if it has mime data
																		if(emailT.getMimeData() != null) {
																			data = emailT;
																		}
																	}
																}
															}
															clientId = backend.createOrUpdateDataItem(
																requestContext,
																profileName,
																collection,
																clientId,
																data														
															);
														}
														else if(command.endsWith("Change")) {
															clientId = backend.createOrUpdateDataItem(
																requestContext,
																profileName,
																collection,
																clientId,
																data														
															);
														}
														else if(command.endsWith("Delete")) {
															if(clientId != null) {
																backend.deleteDataItem(
																	requestContext,
																	profileName,
																	collection,
																	clientId
																);
																itemIdMappings.removeAllMappingsByClientId(clientId);
																clientId = null;
															}
														}
														if(clientId != null && serverId != null && clientId.length() > 0 && serverId.length() > 0) {
															// Move mapping to current generation
															itemIdMappings.updateMappings(clientId, serverId);
														}
													}
												}
											}
										}
										// Responses
										logger.log(Level.FINE, "Processing responses for collection {0}", collectionId);										
										org.w3c.dom.Element eResponses = DOMUtils.getUniqueElement(eCollection, "AirSync:", "Responses");
										if(eResponses != null) {										
											org.w3c.dom.NodeList lResponse = eResponses.getChildNodes();
											for(int j = 0; j < lResponse.getLength(); j++) {							
												org.w3c.dom.Node node = lResponse.item(j);
												if(node instanceof org.w3c.dom.Element) {
													org.w3c.dom.Element eCommand = (org.w3c.dom.Element)node;								
													String command = eCommand.getNodeName();
													String serverId = DOMUtils.getElementText(eCommand, "AirSync:", "ServerId");
													String clientId = DOMUtils.getElementText(eCommand, "AirSync:", "ClientId");
													if(folderId != null) {
														if(command.endsWith("Add")) {
															if(clientId != null && serverId != null && clientId.length() > 0 && serverId.length() > 0) {
																// Move mapping to current generation
																itemIdMappings.updateMappings(clientId, serverId);
															}
														}
														else if(command.endsWith("Delete")) {
															if(serverId.length() > 0) {
																itemIdMappings.removeAllMappingsByServerId(serverId);
															}
														}
													}
												}
											}
										}
										// Has somebody else touched items in this collection?
										// Only test if syncKeyServer != null --> syncKeyClient was updated
										logger.log(Level.FINE, "Test for locally touched items for collection {0}", collectionId);
										if(syncKeyServer != null && !folder.getSyncKeyServer().isEmpty() && !"0".equals(syncKeyServer)) {
									    	org.opencrx.application.airsync.datatypes.SyncCollection clientCollection = 
									    		new org.opencrx.application.airsync.datatypes.SyncCollection();
									    	clientCollection.setCollectionId(folderId);
									    	clientCollection.setSyncKey(syncKeyClient);					    	
									    	clientCollection.setDataType(collectionType);
									    	GetChangedDataItemsResult getChangedDataItemsResult = backend.getChangedDataItems(
									    		requestContext,
									    		profileName,
									    		clientCollection,
									    		true, // noData
									    		1, // maxItems
									    		SyncDataItem.State.MODIFIED,
									    		Collections.<String>emptySet()						    		
									    	);
									    	// There were no changes during [syncKeyClient, requestContext.getSyncKey()] if 
									    	// the syncKey of the first changed item matches the sync key of the request context
											if(getChangedDataItemsResult.getSyncKey().equals(requestContext.getSyncKey())) {
												syncKeyClient = requestContext.getSyncKey();
											}
										}
										// Update folder's sync keys									
										folder.setSyncKeyServer(responseCollection.getSyncKey());	
										logger.log(Level.FINE, "Set SyncKey.Server to {0} for collection {1}", new String[]{responseCollection.getSyncKey(), folder.getName()});
										folder.setSyncKeyClient(
											"0".equals(syncKeyClient) ? 
												backend.getNextSyncKey(requestContext, syncKeyClient) :
													syncKeyClient
										);
									}
								} catch(Exception e) {
									new ServiceException(e).log();
								}
							}
							if(logger.isLoggable(Level.FINE)) {
								ByteArrayOutputStream out = new ByteArrayOutputStream();
						    	WbXMLTransformer.transform(
						    		docResponse,
						    		new StreamResult(out),
						    		true
						    	);
						    	out.close();
						    	logger.log(Level.FINE, "+-+-+-+-+- Response +-+-+-+-+-");
						    	logger.log(Level.FINE, out.toString());
							}
							slicePos += this.batchSize;
							if(changedDataItems == null || slicePos >= changedDataItems.size()) {
								break;
							}
							changedDataItemsSlice = changedDataItems.subList(
								slicePos, 
								Math.min(slicePos + this.batchSize, changedDataItems.size())
							);
						}
					}
				}
			}
			// Save updated sync keys and mappings
			backend.updateClientProfile(
				requestContext,
				clientProfile,
				null, // all folders
				false, // noSyncKeys
				false // noMappings
			);
		} catch(Exception e) {
			throw new ServiceException(e);
		}
    }

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private static final int DEFAULT_BATCH_SIZE = 50;
	
	private final int batchSize;
	
}
