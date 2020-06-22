/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Sync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.datatypes;

import org.opencrx.application.airsync.utils.DOMUtils;
import org.w3c.dom.Element;

public class SyncFolder extends SyncObject {

	private String parentId;
	private String displayName;
	private FolderType folderType;
	
	public String getParentId() {
    	return parentId;
    }

	public void setParentId(String parentId) {
    	this.parentId = parentId;
    }

	public String getDisplayName() {
    	return displayName;
    }

	public void setDisplayName(String displayName) {
    	this.displayName = displayName;
    }

	public FolderType getFolderType() {
    	return this.folderType;
    }

	public void setFolderType(FolderType itemType) {
		this.folderType = itemType;		
	}
		
	public static SyncFolder decode(
		Element e,
		String prefix
	) {
		SyncFolder f = new SyncFolder();
		f.setServerId(DOMUtils.getElementText(e, prefix, "ServerId"));
		f.setParentId(DOMUtils.getElementText(e, prefix, "ParentId"));
		f.setDisplayName(DOMUtils.getElementText(e, prefix, "DisplayName"));
		String type = DOMUtils.getElementText(e, prefix, "Type");
		f.setFolderType(
			type == null ?
				FolderType.toFolderType(0) :
					FolderType.toFolderType(Integer.parseInt(type))
		);
		return f;
	}
	
	public void encode(
		Element e
	) {
		DOMUtils.createElementAndText(e, null, "ServerId", this.getServerId());
		DOMUtils.createElementAndText(e, null, "ParentId", this.getParentId());
		DOMUtils.createElementAndText(e, null, "DisplayName", this.getDisplayName());
		DOMUtils.createElementAndText(e, null, "Type", Integer.toString(this.getFolderType().getValue()));
	}
	
}
