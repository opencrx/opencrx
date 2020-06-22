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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.opencrx.application.airsync.backend.cci.GetChangedDataItemsResult;
import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.DataType;
import org.opencrx.application.airsync.datatypes.PingStatus;
import org.opencrx.application.airsync.datatypes.SyncCollection;
import org.opencrx.application.airsync.datatypes.SyncDataItem;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.log.SysLog;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class PingHandler extends AbstractServerHandler {

	//-----------------------------------------------------------------------
	public PingHandler(
		SyncBackend backend,
		String profilePrefix
	) {
		super(
			backend,
			profilePrefix
		);
	}

	//-----------------------------------------------------------------------
	public static class PingRequest implements Serializable {
	
		public PingRequest(			
		) {		
			this.collections = new HashMap<String,SyncCollection>();
			this.intervalSeconds = 0;
			this.lastSyncKey = "0";
		}	
		
        private static final long serialVersionUID = -2905533554043970286L;
		public Map<String,SyncCollection> collections;
		public long intervalSeconds;
		public String lastSyncKey;
		
	}
	
	//-----------------------------------------------------------------------
	private File getTempFile(
		SyncRequest request,
		String fileName
	) throws ServiceException {
		RequestContext requestContext = this.backend.newRequestContext(
			request.getUserId(), 
			request.getContext()
		);
		File tempDir = this.backend.getContextTempDir(
			requestContext,
			request.getTempDir()
		);
		try {
			tempDir.mkdirs();
		} catch(Exception e) {}
		return new File(
			tempDir.getPath() + File.separator + PingHandler.class.getName() + "-" + fileName + ".ser"
		);
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
		List<String> changedFolders = new ArrayList<String>();		
		PingRequest pingRequest = new PingRequest();
		File cachedPingRequestFile = null;
		try {
			Element eRoot = docRequest == null ? null : docRequest.getDocumentElement();
			// If no ping parameters are provided try to get info from last cached ping
			cachedPingRequestFile = this.getTempFile(
				request,
				request.getUserId() + "@" + request.getDeviceId()
			);
			if(cachedPingRequestFile.exists()) {
				try {
					ObjectInputStream in = new ObjectInputStream(
						new FileInputStream(cachedPingRequestFile)
					);
					pingRequest = (PingRequest)in.readObject();
					in.close();
				} catch(Exception e) {}
			}
			Element eHeartbeatInterval = eRoot == null ? null : DOMUtils.getUniqueElement(eRoot, "Ping:", "HeartbeatInterval");
			if(eHeartbeatInterval != null) {
				pingRequest.intervalSeconds = Long.valueOf(eHeartbeatInterval.getTextContent());
				NodeList lFolder = eRoot.getElementsByTagNameNS("Ping:", "Folder");
				for (int i = 0; i < lFolder.getLength(); i++) {
					Node node =  lFolder.item(i);
					if(node instanceof Element) {
						Element eFolder = (Element)node;
						SyncCollection collection = new SyncCollection();
						collection.setCollectionId(DOMUtils.getElementText(eFolder, "Ping:", "Id"));
						collection.setDataType(DataType.valueOf(DOMUtils.getElementText(eFolder, "Ping:", "Class")));
						if(this.backend.folderIsValid(requestContext, profileName, collection)) {
							collection.setSyncKey(
								this.backend.getNextSyncKey(
									requestContext,
									this.backend.getNextSyncKey(requestContext, this.backend.getNextSyncKey(requestContext, "0"))
								)
							);
							pingRequest.collections.put(
								collection.getCollectionId(),
								collection
							);
						}
					}
				}
			}
			if(!pingRequest.collections.isEmpty()) {
				long waitUntil = System.currentTimeMillis() + (pingRequest.intervalSeconds - 60) * 1000L;
				while(System.currentTimeMillis() < waitUntil) {
					for(SyncCollection collection: pingRequest.collections.values()) {
						if(logger.isLoggable(Level.FINEST)) {
							logger.finest("PingHandler  Checking changes for collection " + collection.getCollectionId());
						}
						Set<String> excludes = new HashSet<String>();
						GetChangedDataItemsResult getChangedDataItemsResult = this.backend.getChangedDataItems(
							requestContext,
							profileName,
							collection, 							
							true, // noData
							1, // windowSize
							SyncDataItem.State.MODIFIED,
							excludes
						);
						String newSyncKey = getChangedDataItemsResult.getSyncKey();
						if(!getChangedDataItemsResult.getDataItems().isEmpty()) {
							changedFolders.add(collection.getCollectionId());
						} 
						else {
							List<String> deletedItemIds = this.backend.getDeletedDataItems(
								requestContext, 
								profileName,
								collection,
								newSyncKey
							);
							if(!deletedItemIds.isEmpty()) {
								changedFolders.add(collection.getCollectionId());
							}
						}
						collection.setSyncKey(newSyncKey);
						// Sleep for 10s to save CPU
						try {
				            Thread.sleep(PING_DELAY);
			            } catch (InterruptedException e) {}					
					}
					if(changedFolders != null && !changedFolders.isEmpty()) {
						break;
					}
				}
			}
		} catch(Exception e) {
			ServiceException e0 = new ServiceException(e);
			SysLog.warning("Exception occurred while processing ping request. For more info see detail log", e0.getMessage());
			SysLog.detail(e0.getMessage(), e0.getCause());
		}
		Document docResponse = DOMUtils.createDoc("Ping:", "Ping");
		Element eRoot = docResponse.getDocumentElement();
		if(changedFolders == null) {
			DOMUtils.createElementAndText(eRoot, "Ping:", "Status", Integer.toString(PingStatus.FOLDER_SYNC_REQUIRED.getValue()));
		} else if(changedFolders.isEmpty()) {
			DOMUtils.createElementAndText(eRoot, "Ping:", "Status", Integer.toString(PingStatus.NO_CHANGES.getValue()));
		} else {
			DOMUtils.createElementAndText(eRoot, "Ping:", "Status", Integer.toString(PingStatus.CHANGES_OCCURED.getValue()));
			Element eFolders = DOMUtils.createElement(eRoot, "Ping:", "Folders");
			for(String folderId: changedFolders) {
				DOMUtils.createElementAndText(eFolders, "Ping:", "Folder", folderId);
			}
		}
		// Cache ping request
		if(cachedPingRequestFile != null) {
			try {
				ObjectOutputStream out = new ObjectOutputStream(
					new FileOutputStream(cachedPingRequestFile)
				);
				out.writeObject(pingRequest);
				out.close();
			} catch(Exception e) {}
		}
		return docResponse;
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	protected static final long PING_DELAY = 10000L;
	protected static final Logger logger = Logger.getLogger(PingHandler.class.getPackage().getName());
	
}
