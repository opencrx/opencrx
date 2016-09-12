/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AccountResource
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
package org.opencrx.application.webdav;

import java.util.Date;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

class DocumentResource extends WebDavResource {

	public DocumentResource(
		RequestContext requestContext,
		Document document,
		DocumentCollectionResource parent
	) {
		super(requestContext, document);
		this.parent = parent;
	}

	@Override
    public Document getObject(
    ) {
        return (Document)super.getObject();
    }

	@Override
    public boolean isCollection(
    ) {
		return false;
    }
	
	@Override 
	public String getMimeType(
	) {
		return this.getObject().getContentType();			
	}
	
    @Override
    public String getName(
    ) {
    	Document document = this.getObject();
    	DocumentRevision headRevision = document.getHeadRevision();
    	return headRevision == null
    		? document.getName()
    		: headRevision instanceof MediaContent
    			? ((MediaContent)headRevision).getContentName()
    			: headRevision.getName();
    }

    public String getDisplayName(
    ) {
    	return this.getObject().getTitle();
    }
        
    @Override
    public Date getLastModified(
    ) {
    	Document document = this.getObject();
    	return document.getHeadRevision() == null 
    		? super.getLastModified() 
    		: document.getHeadRevision().getModifiedAt();
    }

	public DocumentCollectionResource getDocumentCollectionResource(
    ) {
    	return this.parent;
    }
    
	@Override
    public WebDavStore.ResourceContent getContent(
    ) {
		Document document = this.getObject();
		if(document.getHeadRevision() instanceof MediaContent) {
			final MediaContent headRevision = (MediaContent)document.getHeadRevision();
			return new WebDavStore.ResourceContent(){
				@Override
				public BinaryLargeObject getContent() {
					return headRevision.getContent();
				}
				@Override
				public Long getLength() {
					try {
						return headRevision.getContentLength() == null
							? getContent().getLength()
							: headRevision.getContentLength();
					} catch(Exception ignore) {
						return 0L;
					}
 				}
			};
			//return ((MediaContent)document.getHeadRevision()).getContent();
		} else {
			return new WebDavStore.ResourceContent(){
				@Override
				public BinaryLargeObject getContent() {
					return BinaryLargeObjects.valueOf(new byte[]{});
				}
				@Override
				public Long getLength() {
					return 0L;
				}
			};
		}
    }

	private final DocumentCollectionResource parent;
	
}