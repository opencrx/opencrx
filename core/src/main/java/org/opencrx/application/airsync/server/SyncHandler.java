/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: AirSync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;

import org.opencrx.application.airsync.backend.cci.GetChangedDataItemsResult;
import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.DataFormatFactory;
import org.opencrx.application.airsync.datatypes.IData;
import org.opencrx.application.airsync.datatypes.IDataFormat;
import org.opencrx.application.airsync.datatypes.SyncCollection;
import org.opencrx.application.airsync.datatypes.SyncDataItem;
import org.opencrx.application.airsync.datatypes.SyncStatus;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.opencrx.kernel.backend.Base;
import org.openmdx.base.exception.ServiceException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class SyncHandler extends AbstractServerHandler {

	//-----------------------------------------------------------------------
	public SyncHandler(
		SyncBackend backend,
		String profilePrefix
	) {
		super(
			backend,
			profilePrefix
		);
	}

	//-----------------------------------------------------------------------
	private static class SyncResponse {
		
		public SyncResponse(
			String command,
			String clientId,
			String serverId,
			int status
		) {
			this.command = command;
			this.clientId = clientId;
			this.serverId = serverId;
			this.status = status;
		}
		
		public final String command;
		public final String clientId;
		public final String serverId;
		public final int status;
	}
	
	//-----------------------------------------------------------------------
	@Override
	public Document handle(
		SyncRequest request, 
		Document docRequest
	) {
		RequestContext requestContext = this.backend.newRequestContext(
			request.getUserId(), 
			request.getContext()
		);
		String profileName = this.getProfileName(request);
		List<SyncCollection> requestedCollections = new ArrayList<SyncCollection>();
		Map<String,SyncResponse> syncResponses = new HashMap<String,SyncResponse>();
		List<String> fetchIds = new ArrayList<String>();
		// Empty sync request
		if(docRequest != null) {
			Element eRoot = docRequest.getDocumentElement();
			NodeList lCollection = eRoot.getElementsByTagNameNS("AirSync:", "Collection");
			for(int i = 0; i < lCollection.getLength(); i++) {
				try {
					Element eCollection = (Element) lCollection.item(i);
					SyncCollection collection = SyncCollection.decode("AirSync:", eCollection);
					Element eCommands = DOMUtils.getUniqueElement(eCollection, "AirSync:", "Commands");
					if(eCommands != null) {
						// Process commands for requested collections
						NodeList lCommand = eCommands.getChildNodes();
						for(int j = 0; j < lCommand.getLength(); j++) {							
							Node node = lCommand.item(j);
							if(node instanceof Element) {
								Element eCommand = (Element)node;								
								String command = eCommand.getNodeName();
								String serverId = DOMUtils.getElementText(eCommand, "AirSync:", "ServerId");
								String clientId = DOMUtils.getElementText(eCommand, "AirSync:", "ClientId");
								Element eApplicationData = DOMUtils.getUniqueElement(eCommand, "AirSync:", "ApplicationData");
								IDataFormat dataDecoder = DataFormatFactory.getXmlFormat(collection.getDataType());
								IData data = null;
								if(eApplicationData != null) {
									data = dataDecoder.parse(eApplicationData);
								}
								if(command.endsWith("Modify")) {
									if(data != null) {
										try {
											if(data.isRead()) {
												this.backend.setDataItemReadFlag(
													requestContext, 
													collection.getCollectionId(), 
													serverId, 
													data.isRead()
												);
											}
											else {
												this.backend.createOrUpdateDataItem(
													requestContext, 
													profileName, 
													collection, 
													serverId, 
													data
												);
											}
											collection.setImportedChanges(true);
											syncResponses.put(
												serverId,
												new SyncResponse(
													"Modify",
													clientId,
													serverId,
													1 // ok
												)
											);
										} catch(Exception e) {
											new ServiceException(e).log();
											syncResponses.put(
												serverId,
												new SyncResponse(
													"Modify",
													clientId,
													serverId,
													5 // server error
												)
											);
										}										
									}
								}
								else if(command.endsWith("Add")) {
									if(data != null) {
										try {
											String newServerId = this.backend.createOrUpdateDataItem(
												requestContext, 
												profileName, 
												collection,
												null, 
												data
											);
											if(newServerId != null && clientId != null) {
												syncResponses.put(
													newServerId,
													new SyncResponse(
														"Add",
														clientId,
														newServerId,
														1 // ok
													)
												);
												collection.setImportedChanges(true);
											}
										} catch(Exception e) {
											new ServiceException(e).log();
											syncResponses.put(
												Base.getInstance().getUidAsString(), // dummy
												new SyncResponse(
													"Add",
													clientId,
													null,
													5 // server error
												)
											);
										}
									}
								}
								else if(command.endsWith("Change")) {
									if(data != null) {
										try {
											this.backend.createOrUpdateDataItem(
												requestContext, 
												profileName, 
												collection,
												serverId, 
												data
											);
											syncResponses.put(
												serverId,
												new SyncResponse(
													"Change",
													clientId,
													serverId,
													1 // ok
												)
											);
											collection.setImportedChanges(true);
										} catch(Exception e) {
											new ServiceException(e).log();
											syncResponses.put(
												serverId,
												new SyncResponse(
													"Change",
													clientId,
													serverId,
													5 // server error
												)
											);
										}
									}
								}
								else if(command.endsWith("Delete")) {
									try {
										this.backend.deleteDataItem(
											requestContext, 
											profileName, 
											collection, 
											serverId
										);
										syncResponses.put(
											serverId,
											new SyncResponse(
												"Delete",
												clientId,
												serverId,
												1 // ok
											)
										);
										collection.setImportedChanges(true);
									} catch(Exception e) {
										new ServiceException(e).log();
										syncResponses.put(
											serverId,
											new SyncResponse(
												"Delete",
												clientId,
												serverId,
												5 // server error
											)
										);
									}
								}
								else if(command.endsWith("Fetch")) {
									fetchIds.add(serverId);
								}
							}
						}
					}
					requestedCollections.add(collection);
				} catch(Exception e) {
					new ServiceException(e).log();						
				}				
			}
		}
		// Response
		Document docResponse = null;
		docResponse = DOMUtils.createDoc(
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
		Element eRoot = docResponse.getDocumentElement();
		Element eCollections = DOMUtils.createElement(eRoot, null, "Collections");
		for(SyncCollection collection: requestedCollections) {
			// Calculate new sync key
			String newSyncKey = null;
			boolean hasMore = false;
			List<SyncDataItem> newDataItems = null;
			List<SyncDataItem> changedDataItems = null;
			List<String> deletedItemIds = null;
			// Get changes and derive new sync key 
			if(!"0".equals(collection.getSyncKey()) && collection.isGetChanges()) {
				try {
					int windowSize = collection.getWindowSize() == null ? BATCH_SIZE : collection.getWindowSize();
					// New items
					Set<String> excludes = new HashSet<String>();
					logger.log(Level.FINE, "Get new items for client collection {0} and syncKey {1}", new String[]{collection.getCollectionId(), collection.getSyncKey()});						
					GetChangedDataItemsResult getNewDataItemsResult = this.backend.getChangedDataItems(
						requestContext, 
						profileName, 
						collection,
						false, // noData
						windowSize,
						SyncDataItem.State.NEW,
						excludes
					);
			    	for(List<SyncDataItem> dataItems: getNewDataItemsResult.getDataItems().values()) {
				    	for(SyncDataItem dataItem: dataItems) {
				    		excludes.add(dataItem.getServerId());
				    	}
			    	}
			    	// Changed items
					logger.log(Level.FINE, "Get changed items for client collection {0} and syncKey {1}", new String[]{collection.getCollectionId(), collection.getSyncKey()});						
					GetChangedDataItemsResult getChangedDataItemsResult = this.backend.getChangedDataItems(
						requestContext, 
						profileName, 
						collection,
						false, // noData
						windowSize,
						SyncDataItem.State.MODIFIED,
						excludes
					);
			    	newSyncKey = getNewDataItemsResult.getDataItems().isEmpty() ?
			    		getChangedDataItemsResult.getSyncKey() :
			    			getChangedDataItemsResult.getDataItems().isEmpty() ?
			    				getNewDataItemsResult.getSyncKey() :
			    					getNewDataItemsResult.getSyncKey().compareTo(getChangedDataItemsResult.getSyncKey()) < 0 ?
			    						getNewDataItemsResult.getSyncKey() :
			    							getChangedDataItemsResult.getSyncKey();
			    	hasMore = getNewDataItemsResult.hasMore() || getChangedDataItemsResult.hasMore();
			    	// Add new items to newDataItems with syncKey < newSyncKey
			    	for(Map.Entry<String,List<SyncDataItem>> entries: getNewDataItemsResult.getDataItems().entrySet()) {
			    		if(entries.getKey().compareTo(newSyncKey) <= 0) {
			    			if(newDataItems == null) {
			    				newDataItems = new ArrayList<SyncDataItem>();
			    			}
			    			newDataItems.addAll(entries.getValue());
			    		} else {
			    			hasMore = true;
			    		}
			    	}
			    	// Add changed items to changedDataItems with syncKey < newSyncKey
			    	for(Map.Entry<String,List<SyncDataItem>> entries: getChangedDataItemsResult.getDataItems().entrySet()) {
			    		if(entries.getKey().compareTo(newSyncKey) <= 0) {
			    			if(changedDataItems == null) {
			    				changedDataItems = new ArrayList<SyncDataItem>();
			    			}
			    			changedDataItems.addAll(entries.getValue());
			    		} else {
			    			hasMore = true;
			    		}
			    	}
					// Deleted items
					if(
						(newDataItems != null && !newDataItems.isEmpty()) || 
						(changedDataItems != null && !changedDataItems.isEmpty())
					) {
						// Collect deleted items in range [collection.getSyncKey(), newSyncKey]							
						deletedItemIds = this.backend.getDeletedDataItems(
							requestContext, 
							profileName, 
							collection,
							newSyncKey
						);
					}
					else {
						String syncKeyTo = this.backend.getNextSyncKey(requestContext, newSyncKey);
						// Collect deleted items in range [collection.getSyncKey(), syncKeyTo]
						deletedItemIds = this.backend.getDeletedDataItems(
							requestContext, 
							profileName, 
							collection,
							syncKeyTo
						);
						if(!deletedItemIds.isEmpty()) {
							newSyncKey = syncKeyTo;
						}
					}
				} catch(Exception e) {
					new ServiceException(e).log();
				}
			}
			// Get initial sync key
			else if("0".equals(collection.getSyncKey())) {
				try {
					newSyncKey = this.backend.getNextSyncKey(
						requestContext, 
						collection.getSyncKey()
					);
				} catch(Exception e) {
					new ServiceException(e).log();
				}
			} 
			// The new sync key is identical to the old one
			else {
				newSyncKey = collection.getSyncKey();
			}
			Element eCollection = DOMUtils.createElement(eCollections, null, "Collection");
			if(collection.getDataType() != null) {
				DOMUtils.createElementAndText(eCollection, null, "Class", collection.getDataType().toString());
			}
			DOMUtils.createElementAndText(
				eCollection, 
				null, 
				"SyncKey", 
				newSyncKey
			);
			DOMUtils.createElementAndText(eCollection, null, "CollectionId", collection.getCollectionId());
			DOMUtils.createElementAndText(eCollection, null, "Status", "1");
			// Responses
			Element eResponses = DOMUtils.createElement(eCollection, null, "Responses");	
			for(SyncResponse syncResponse: syncResponses.values()) {
				Element eCommand = DOMUtils.createElement(eResponses, null, syncResponse.command);
				if(syncResponse.clientId != null) {
					DOMUtils.createElementAndText(eCommand, null, "ClientId", syncResponse.clientId);
				}
				if(syncResponse.serverId != null) {
					DOMUtils.createElementAndText(eCommand, null, "ServerId", syncResponse.serverId);
				}
				DOMUtils.createElementAndText(eCommand, null, "Status", Integer.toString(syncResponse.status));
			}
			for(String id: fetchIds) {
				SyncStatus syncStatus = SyncStatus.OK;
				SyncDataItem dataItem = null;
				try {
					dataItem = this.backend.fetchDataItem(
						requestContext, 
						profileName,
						collection,
						id
					);
					if(dataItem == null) {
						syncStatus = SyncStatus.OBJECT_NOT_FOUND;
					}
				} catch(Exception e) {
					new ServiceException(e).log();
					syncStatus = SyncStatus.SERVER_ERROR;
				}
				Element eFetch = DOMUtils.createElement(eResponses, null, "Fetch");
				DOMUtils.createElementAndText(eFetch, null, "ServerId", id);
				DOMUtils.createElementAndText(eFetch, null, "Status", Integer.toString(syncStatus.getValue()));
				if(syncStatus == SyncStatus.OK) {
					IData data = dataItem.getData();
					Element eApplicationData = DOMUtils.createElement(eFetch, null, "ApplicationData");
					try {
						DataFormatFactory.getXmlFormat(data.getType()).format(
							eApplicationData, 
							data,
							request.getProtocolVersion()
						);
					} catch(Exception e) {
						new ServiceException(e).log();
					}
				}
			}
			if(eResponses.getChildNodes().getLength() == 0) {
				eResponses.getParentNode().removeChild(eResponses);
			}
			// Commands
			if(collection.isGetChanges()) {
				if(
					(newDataItems != null && !newDataItems.isEmpty()) || 
					(changedDataItems != null && !changedDataItems.isEmpty()) || 
					(deletedItemIds != null && !deletedItemIds.isEmpty())
				) {
					Element eCommands = DOMUtils.createElement(eCollection, null, "Commands");
					// Add
					if(newDataItems != null) {
						for(SyncDataItem dataItem: newDataItems) {
							// Do not return objects which are add/changed during this sync
							if(!syncResponses.keySet().contains(dataItem.getServerId())) {
								IData data = dataItem.getData();
								Element eCommand = null;
								eCommand = DOMUtils.createElement(eCommands, null, "Add");							
								DOMUtils.createElementAndText(eCommand, null, "ServerId", dataItem.getServerId());
								Element eApplicationData = DOMUtils.createElement(eCommand, null, "ApplicationData");
								try {
									DataFormatFactory.getXmlFormat(data.getType()).format(
										eApplicationData, 
										data,
										request.getProtocolVersion()
									);
								} catch(Exception e) {
									new ServiceException(e).log();
								}
							}
						}
					}
					// Change
					if(changedDataItems != null) {
						for(SyncDataItem dataItem: changedDataItems) {
							// Do not return objects which are add/changed during this sync
							if(!syncResponses.keySet().contains(dataItem.getServerId())) {
								IData data = dataItem.getData();
								Element eCommand = null;
								eCommand = DOMUtils.createElement(eCommands, null, "Change");
								DOMUtils.createElementAndText(eCommand, null, "ServerId", dataItem.getServerId());
								Element eApplicationData = DOMUtils.createElement(eCommand, null, "ApplicationData");
								try {
									DataFormatFactory.getXmlFormat(data.getType()).format(
										eApplicationData, 
										data,
										request.getProtocolVersion()
									);
								} catch(Exception e) {
									new ServiceException(e).log();
								}
							}
						}
					}
					if(deletedItemIds != null) {
						for(String serverId: deletedItemIds) {
							if(!syncResponses.keySet().contains(serverId)) {
								Element eDelete = DOMUtils.createElement(eCommands, null, "Delete");
								DOMUtils.createElementAndText(eDelete, null, "ServerId", serverId);
							}
						}
					}
					if(eCommands.getChildNodes().getLength() == 0) {
						eCommands.getParentNode().removeChild(eCommands);
					}
				}
			}
			if(hasMore) {
				DOMUtils.createElement(eCollection, null, "MoreAvailable");					
			}
		}
		return docResponse;
	}

	private static final int BATCH_SIZE = 64;
	
}
