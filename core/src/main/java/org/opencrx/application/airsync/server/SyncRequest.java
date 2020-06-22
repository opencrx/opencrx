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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.resource.Records;
import org.openmdx.kernel.log.SysLog;

public class SyncRequest {

	private final HttpServletRequest context;	
	private double protocolVersion;
	private final String cmd;
	private final String attachmentName;
	private final String deviceId;
	private final String userId;
	
	public SyncRequest(
	) {
		this.context = null;
		this.cmd = null;
		this.attachmentName = null;
		this.deviceId = null;
		this.userId = null;
	}
	
	public SyncRequest(
		String cmd,
		String userId,
		String deviceId
	) {
		this.cmd = cmd;
		this.deviceId = deviceId;
		this.userId = userId;
		this.context = null;
		this.attachmentName = null;
	}
	
	public SyncRequest(
		HttpServletRequest httpServletRequest
	) {
		this.context = httpServletRequest;
		List<String> params = new ArrayList<String>();
		for(Object parameter: httpServletRequest.getParameterMap().keySet()) {
			if(parameter instanceof String) {
				params.add(parameter + "=" + httpServletRequest.getParameter((String)parameter));
			}
		}
		System.out.println(new Date() + "  SyncRequest: " + params);
		SysLog.warning("SyncRequest", params);
		this.cmd = httpServletRequest.getParameter("Cmd");
		this.attachmentName = httpServletRequest.getParameter("AttachmentName");
		this.deviceId = httpServletRequest.getParameter("DeviceId");
		String userId = httpServletRequest.getUserPrincipal() == null ? null : httpServletRequest.getUserPrincipal().getName();
		if(userId.indexOf(SyncBackend.DOMAIN_SEPARATOR) < 0) {
			userId = DEFAULT_SEGMENT_NAME + SyncBackend.DOMAIN_SEPARATOR + userId; 
		}
		this.userId = userId;
	}

	public File getTempDir(
	) throws ServiceException {
		if(System.getProperty("org.opencrx.airsyncdir") != null) {
			return new File(System.getProperty("org.opencrx.airsyncdir"));
		} else {
			return (File)this.context.getSession().getServletContext().getAttribute("javax.servlet.context.tempdir");
		}
	}
	
	public Object getContext(
	) {
		return this.context;
	}
	
	public double getProtocolVersion(
	) {
		return this.protocolVersion;
	}
	
	public InputStream getInputStream(
	) throws IOException {
		return this.context.getInputStream();
	}

	public String getCmd() {
    	return cmd;
    }

	public String getAttachmentName() {
    	return attachmentName;
    }

	public String getDeviceId() {
    	return deviceId;
    }

	public String getUserId() {
	    return userId;
    }

	@Override
    public String toString(
    ) {
        return Records.getRecordFactory().asMappedRecord(
            SyncRequest.class.getName(),
            null,
            new String[]{
            	"cmd",
            	"userId",
            	"deviceId",
            	"protocolVersion"
            },
            new Object[]{
                this.cmd,
                this.userId,
                this.deviceId,
                this.protocolVersion
            }
        ).toString();
    }

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String DEFAULT_SEGMENT_NAME = "Standard";
	
}
