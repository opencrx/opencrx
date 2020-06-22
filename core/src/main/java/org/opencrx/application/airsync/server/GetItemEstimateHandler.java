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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.opencrx.application.airsync.backend.cci.GetChangedDataItemsResult;
import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.SyncCollection;
import org.opencrx.application.airsync.datatypes.SyncDataItem;
import org.opencrx.application.airsync.datatypes.SyncStatus;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.openmdx.base.exception.ServiceException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class GetItemEstimateHandler extends AbstractServerHandler {

	public GetItemEstimateHandler(
		SyncBackend backend,
		String profilePrefix
	) {
		super(
			backend,
			profilePrefix
		);
	}

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
		List<SyncCollection> collections = new ArrayList<SyncCollection>();
		Element eRoot = docRequest.getDocumentElement();
		NodeList lCollection = eRoot.getElementsByTagNameNS("ItemEstimate:", "Collection");		
		for(int i = 0; i < lCollection.getLength(); i++) {
			Element eCollection = (Element) lCollection.item(i);
			SyncCollection collection = SyncCollection.decode("ItemEstimate:", eCollection);
			collections.add(collection);
		}
		Document docResponse = DOMUtils.createDoc("ItemEstimate:", "GetItemEstimate");
		eRoot = docResponse.getDocumentElement();		
		for(SyncCollection collection: collections) {
			Element eResponse = DOMUtils.createElement(eRoot, null, "Response");
			SyncStatus syncStatus = SyncStatus.OK;
			int count = 0;
			try {				
				if(this.backend.folderIsValid(requestContext, profileName, collection)) {
					Set<String> excludes = new HashSet<String>();
					GetChangedDataItemsResult getNewDataItemsResult = this.backend.getChangedDataItems(
						requestContext, 
						profileName,
						collection,
						true, // noData
						MAX_ESTIMATE_COUNT,
						SyncDataItem.State.NEW,
						excludes
					);
					count = getNewDataItemsResult.getDataItems().size();
					for(List<SyncDataItem> dataItems: getNewDataItemsResult.getDataItems().values()) {
						for(SyncDataItem dataItem: dataItems) {
							excludes.add(dataItem.getServerId());
						}
					}
					if(count < MAX_ESTIMATE_COUNT) {
						collection.setSyncKey(getNewDataItemsResult.getSyncKey());
						GetChangedDataItemsResult getChangedDataItemsResult = this.backend.getChangedDataItems(
							requestContext, 
							profileName,
							collection,
							true, // noData
							MAX_ESTIMATE_COUNT,
							SyncDataItem.State.MODIFIED,
							excludes
						);				
						count += getChangedDataItemsResult.getDataItems().size();
					}					
				} else {
					syncStatus = SyncStatus.OBJECT_NOT_FOUND;
				}
			} catch(Exception e) {
				new ServiceException(e).log();
				syncStatus = SyncStatus.SERVER_ERROR; 
			}
			DOMUtils.createElementAndText(eResponse, null, "Status", Integer.toString(syncStatus.getValue()));
			Element eCollection = DOMUtils.createElement(eResponse, null, "Collection");
			if(collection.getDataType() != null) {
				DOMUtils.createElementAndText(eCollection, null, "Class", collection.getDataType().toString());
			}
			DOMUtils.createElementAndText(eCollection, null, "CollectionId", collection.getCollectionId());
			if(syncStatus == SyncStatus.OK) {
				Element eEstimate = DOMUtils.createElement(eCollection, null, "Estimate");
				eEstimate.setTextContent(Integer.toString(count));
			}
		}
		return docResponse;
	}

	private static final int MAX_ESTIMATE_COUNT = 256;
	
}
