/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: Sync for openCRX
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

import java.io.IOException;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletResponse;
import javax.xml.transform.stream.StreamResult;

import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.application.airsync.utils.WbXMLTransformer;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.w3c.dom.Document;

public abstract class AbstractServerHandler implements ServerHandler {

	protected SyncBackend backend;

	protected AbstractServerHandler(
		SyncBackend backend,
		String profilePrefix
	) {
		this.backend = backend;
		this.profilePrefix = profilePrefix;
	}

	protected String getProfilePrefix(
	) {
		return this.profilePrefix;
	}
	
	protected String getProfileName(
		SyncRequest request
	) {
		return this.getProfilePrefix() + request.getDeviceId();
	}
	
	@Override
	public void handle(
		SyncRequest request,
		SyncResponse response
	) throws IOException {
		Document docRequest = null;
		PushbackInputStream in = new PushbackInputStream(request.getInputStream());
		int b = in.read();
		if(b >= 0) {
			try {
				in.unread(b);
				docRequest = WbXMLTransformer.transformFromWBXML(in);
				if(logger.isLoggable(Level.FINEST)) {
					logger.finest("=-=-=-= Request =-=-=-= ");
					QuotaByteArrayOutputStream xml = new QuotaByteArrayOutputStream(AbstractServerHandler.class.getName());
					WbXMLTransformer.transform(docRequest, new StreamResult(xml), true);
					xml.close();
					logger.finest(new String(xml.getBuffer(), 0, xml.size(), "UTF-8"));
					logger.finest("=-=-=-= Request =-=-=-= ");
				}
			} catch(Exception e) {
				new ServiceException(e).log();
			}
		}
		Document docResponse = this.handle(
			request, 
			docRequest 
		);
		if(logger.isLoggable(Level.FINEST)) {
			logger.finest("=-=-=-=  Response =-=-=-= ");
			QuotaByteArrayOutputStream xml = new QuotaByteArrayOutputStream(AbstractServerHandler.class.getName());
			WbXMLTransformer.transform(docResponse, new StreamResult(xml), true);
			xml.close();
			logger.finest(new String(xml.getBuffer(), 0, xml.size(), "UTF-8"));
			logger.finest("=-=-=-=  Response =-=-=-= ");
		}
		response.setContentType("application/vnd.ms-sync.wbxml");
		response.setStatus(HttpServletResponse.SC_OK);
		OutputStream out = response.getOutputStream();
		WbXMLTransformer.transformToWBXML(
			docResponse, 
			out
		);
		out.flush();
	}

	protected abstract Document handle(
		SyncRequest request, 
		Document docRequest
	);

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	protected static final Logger logger = Logger.getLogger(AbstractServerHandler.class.getPackage().getName());
	
	private final String profilePrefix;
	
}
