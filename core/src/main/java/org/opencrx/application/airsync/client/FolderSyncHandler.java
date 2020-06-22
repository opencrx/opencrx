/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AirSync Client FolderSyncHandler
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.logging.Level;

import javax.xml.transform.stream.StreamResult;

import org.opencrx.application.airsync.backend.cci.ClientProfile;
import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.SyncFolder;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.opencrx.application.airsync.utils.WbXMLTransformer;
import org.openmdx.base.exception.ServiceException;

public class FolderSyncHandler extends AbstractClientHandler {

	public FolderSyncHandler(
		SyncBackend backend
	) {
		super(backend);
	}
	
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
	    	String requestXml = 
	    		"<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + 
	    		"<FolderSync xmlns=\"FolderHierarchy:\">" +
	    		"<SyncKey>0</SyncKey>" +
	    		"</FolderSync>";
	    	org.w3c.dom.Document requestDoc = DOMUtils.parse(
	    		new ByteArrayInputStream(requestXml.getBytes("UTF-8"))
	    	);	    	
	    	org.w3c.dom.Document docResponse = (org.w3c.dom.Document)target.perform(
	    		"FolderSync", 
	    		clientProfile.getPolicyKey() == null ? "0" : clientProfile.getPolicyKey(),
	    		clientProfile.getUserAgent(),
	    		requestDoc
	    	);
	    	org.w3c.dom.Element eChanges = DOMUtils.getUniqueElement(docResponse.getDocumentElement(), null, "FolderHierarchy:Changes");
			if(eChanges != null) {
				org.w3c.dom.NodeList nlChanges = eChanges.getChildNodes();
				for(int i = 0; i < nlChanges.getLength(); i++) {
					org.w3c.dom.Element eChange = (org.w3c.dom.Element) nlChanges.item(i);
					String command = eChange.getNodeName();
					SyncFolder syncFolder = SyncFolder.decode(eChange, "FolderHierarchy");
					if(command.equals("FolderHierarchy:Add")) {
						ClientProfile.Folder folder = new ClientProfile.Folder();
						folder = new ClientProfile.Folder();
						folder.setName(syncFolder.getDisplayName());
						folder.setType(syncFolder.getFolderType());
						folder.setServerId(syncFolder.getServerId());
						folder.setParentId(syncFolder.getParentId());
						folder.setClientId(syncFolder.getClientId());						
						clientProfile.getFolders().add(folder);							
					}
				}
				backend.updateClientProfile(
					requestContext,
					clientProfile, 
					null, // all folders
					true, // noSyncKeys
					true // noMappings
				);
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
		} catch(Exception e) {
			throw new ServiceException(e);
		}
    }

}
