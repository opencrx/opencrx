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
import java.util.List;

import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.SyncStatus;
import org.opencrx.application.airsync.utils.DOMUtils;
import org.openmdx.base.exception.ServiceException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class MoveItemsHandler extends AbstractServerHandler {

	public MoveItemsHandler(
		SyncBackend backend,
		String profilePrefix
	) {
		super(
			backend,
			profilePrefix
		);
	}

	static class MoveItem {

		private String srcMsgId;
		private String srcFldId;
		private String dstFldId;

		public MoveItem() {
		}
		
		public void setDstFldId(String destinationFolderId) {
	    	this.dstFldId = destinationFolderId;
	    }

		public void setSrcFldId(String sourceFolderId) {
	    	this.srcFldId = sourceFolderId;
	    }

		public void setSrcMsgId(String sourceMessageId) {
	    	this.srcMsgId = sourceMessageId;
	    }

		public String getSrcMsgId() {
			return srcMsgId;
		}

		public String getSrcFldId() {
			return srcFldId;
		}

		public String getDstFldId() {
			return dstFldId;
		}

		public static MoveItem decode(
			Element e
		) {
			MoveItem moveItem = new MoveItem();
			moveItem.setSrcMsgId(DOMUtils.getElementText(e, null, "SrcMsgId"));
			moveItem.setSrcFldId(DOMUtils.getElementText(e, null, "SrcFldId"));
			moveItem.setDstFldId(DOMUtils.getElementText(e, null, "DstFldId"));
			return moveItem;		
		}
		
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
		NodeList lMove = docRequest.getDocumentElement().getElementsByTagName("Move");
		List<MoveItem> moveItems = new ArrayList<MoveItem>();
		for(int i = 0; i < lMove.getLength(); i++) {
			Element eMove = (Element) lMove.item(i);
			MoveItem moveItem = MoveItem.decode(eMove);
			moveItems.add(moveItem);
		}
		// Response
		Document docResponse = DOMUtils.createDoc("Move:", "Moves");
		Element eRoot = docResponse.getDocumentElement();
		for(MoveItem moveItem : moveItems) {
			Element eResponse = DOMUtils.createElement(eRoot, null, "Response");
			if(moveItem.getSrcFldId() == null) {
				DOMUtils.createElementAndText(eResponse, null, "Status", Integer.toString(SyncStatus.OBJECT_NOT_FOUND.getValue()));
			}
			else if(moveItem.getSrcFldId().equals(moveItem.getDstFldId())) {
				DOMUtils.createElementAndText(eResponse, null, "Status", Integer.toString(SyncStatus.PROTOCOL_ERROR.getValue()));
			}
			else {
				SyncStatus syncStatus = SyncStatus.OK;
				String dstMsgId = null;
				try {
					dstMsgId = this.backend.moveDataItem(
						requestContext,
						profileName,
						moveItem.getSrcFldId(),
						moveItem.getDstFldId(),
						moveItem.getSrcMsgId()
					);
				} catch(Exception e) {
					new ServiceException(e).log();
					syncStatus = SyncStatus.SERVER_ERROR;
				}
				DOMUtils.createElementAndText(eResponse, null, "Status", Integer.toString(syncStatus.getValue()));
				if(syncStatus == SyncStatus.OK) {
					DOMUtils.createElementAndText(eResponse, null, "DstMsgId", dstMsgId == null ? moveItem.getSrcMsgId() : dstMsgId);
					DOMUtils.createElementAndText(eResponse, null, "SrcMsgId", moveItem.getSrcMsgId());
				}
			}
		}
		return docResponse;
	}
	
}
