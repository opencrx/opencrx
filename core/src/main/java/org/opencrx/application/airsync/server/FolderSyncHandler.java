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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.SyncFolder;
import org.opencrx.application.airsync.datatypes.SyncStatus;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.openmdx.base.exception.ServiceException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class FolderSyncHandler extends AbstractServerHandler {

	public FolderSyncHandler(
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
		SyncStatus status = SyncStatus.OK;
		List<SyncFolder> changedFolders = null;
		List<SyncFolder> deletedFolders = null;
		String newSyncKey = null;
		Element eChanges = null;
		RequestContext requestContext = this.backend.newRequestContext(
			request.getUserId(), 
			request.getContext()
		);
		try {
			String syncKey = docRequest == null ? 
				null : 
					DOMUtils.getElementText(docRequest.getDocumentElement(), null, "FolderHierarchy:SyncKey");
			syncKey = syncKey == null ? "0" : syncKey;
			eChanges = docRequest == null ? null : DOMUtils.getUniqueElement(docRequest.getDocumentElement(), null, "FolderHierarchy:Changes");
			if(eChanges != null) {
				Map<String,String> idMap = new HashMap<String,String>();
				NodeList nlChanges = eChanges.getChildNodes();
				for (int i = 0; i < nlChanges.getLength(); i++) {
					Element eChange = (Element) nlChanges.item(i);
					String command = eChange.getNodeName();
					String serverId = null;
					SyncFolder folder = SyncFolder.decode(eChange, null);
					if(command.equals("Add") || command.equals("Modify")) {
						serverId = this.backend.createOrUpdateFolder(requestContext, folder);
					}
					else if(command.equals("Remove")) {
						serverId = this.backend.deleteFolder(requestContext, folder.getServerId());
					}
					if(serverId != null) {
						idMap.put(serverId, folder.getClientId());
					}
				}
			}			
			changedFolders = this.backend.getChangedFolders(
				requestContext, 
				this.getProfileName(request),
				syncKey
			);
			deletedFolders = this.backend.getDeletedFolders(
				requestContext, 
				this.getProfilePrefix() + request.getDeviceId(),
				syncKey
			);
			newSyncKey = this.backend.getNextSyncKey(requestContext, syncKey);
			if("0".equals(syncKey)) {
				newSyncKey = this.backend.getNextSyncKey(requestContext, newSyncKey);
			}
		} catch(Exception e) {
			new ServiceException(e).log();
			status = SyncStatus.SERVER_ERROR;
		}
		// Response
		Document docResponse = DOMUtils.createDoc("FolderHierarchy:", "FolderSync");
		Element eRoot = docResponse.getDocumentElement();
		DOMUtils.createElementAndText(eRoot, null, "Status", Integer.toString(status.getValue()));
		if(status == SyncStatus.OK) {
			DOMUtils.createElementAndText(eRoot, null, "SyncKey", newSyncKey);
			if(changedFolders != null || deletedFolders != null) {
				eChanges = DOMUtils.createElement(eRoot, null, "Changes");
				int count = 
					(changedFolders == null ? 0 : changedFolders.size()) + 
					(deletedFolders == null ? 0 : deletedFolders.size());
				DOMUtils.createElementAndText(eChanges, null, "Count", Integer.toString(count));
				if(changedFolders != null) {
					for(SyncFolder folder: changedFolders) {
						Element e = DOMUtils.createElement(eChanges, null, "Add");
						folder.encode(e);
					}
				}
				if(deletedFolders != null) {
					for(SyncFolder folder: deletedFolders) {
						Element e = DOMUtils.createElement(eChanges, null, "Delete");
						folder.encode(e);
					}
				}
			}
		}
		return docResponse;
	}
	
}
