/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
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
package org.opencrx.application.webdav;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.openmdx.base.jmi1.BasicObject;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

abstract class WebDavResource implements Resource {

	public WebDavResource(
		RequestContext requestContext,
		BasicObject object
	) {
		this.requestContext = requestContext;
		this.object = object;
	}

	@Override
    public Date getCreationDate(
    ) {
		return this.object.getCreatedAt();
    }

	@Override
    public Date getLastModified(
    ) {
		return this.object.getModifiedAt();
    }

	public BasicObject getObject(
	) {
		return this.object;
	}
	
	public <T extends Resource> Collection<T> getChildren(
		Date timeRangeStart,
		Date timeRangeEnd
	) {
		return Collections.emptyList();
	}
	
	public WebDavRequestContext getRequestContext(
	) {
		return (WebDavRequestContext)this.requestContext;
	}
	
	@Override
    public String getName(
    ) {
		return this.object.refGetPath().getLastSegment().toClassicRepresentation();
    }

	public String getMimeType(
	) {
		return "application/xml";
	}
	
	public WebDavStore.ResourceContent getContent(
	) {
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
	
	private final RequestContext requestContext;
	private final BasicObject object;
}