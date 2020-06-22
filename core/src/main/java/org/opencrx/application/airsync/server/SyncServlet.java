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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.jdo.PersistenceManagerFactory;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.airsync.backend.cci.SyncBackend;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.loading.Classes;
import org.openmdx.kernel.log.SysLog;

public class SyncServlet extends HttpServlet {

	//-----------------------------------------------------------------------
	@Override
	public void init(
	) throws ServletException {
		try {
			super.init();
			// Provider name
			String providerName = this.getInitParameter("provider") != null && this.getInitParameter("provider").startsWith("provider/") ? 
				this.getInitParameter("provider").substring(9) :
					DEFAULT_PROVIDER_NAME;
			PersistenceManagerFactory pmf = Utils.getPersistenceManagerFactory();
			// Backend
			String backendClassName = this.getInitParameter("backendClassName") == null ?
				org.opencrx.application.airsync.backend.impl.OpenCrxSyncBackend.class.getName() :
					this.getInitParameter("backendClassName");
			Class<SyncBackend> backendClass = Classes.getApplicationClass(backendClassName);
			this.backend = backendClass.getConstructor(
				PersistenceManagerFactory.class,
				String.class
			).newInstance(pmf, providerName);
			// Profile prefix
			String profilePrefix = this.getInitParameter("profilePrefix") == null ?
				DEFAULT_PROFILE_PREFIX :
					this.getInitParameter("profilePrefix");
			// Handlers
			this.handlers = new HashMap<String, ServerHandler>();
			this.handlers.put("FolderSync", new FolderSyncHandler(this.backend, profilePrefix));
			this.handlers.put("Sync", new SyncHandler(this.backend, profilePrefix));
			this.handlers.put("GetItemEstimate", new GetItemEstimateHandler(this.backend, profilePrefix));
			this.handlers.put("Ping", new PingHandler(this.backend, profilePrefix));
			this.handlers.put("MoveItems", new MoveItemsHandler(this.backend, profilePrefix));
			this.handlers.put("GetAttachment", new GetAttachmentHandler(this.backend, profilePrefix));
			this.handlers.put("SendMail", new SendMailHandler(this.backend, profilePrefix));
			System.out.println("AirSyncServlet " + providerName + " is running");
		} 
		catch(Exception e) {
			new ServiceException(e).log();
			throw new ServletException(e);
		}
	}

	//-----------------------------------------------------------------------
	protected SyncRequest getSyncRequest(
		HttpServletRequest req
	) {
		return new SyncRequest(req);
	}

	//-----------------------------------------------------------------------
	protected SyncResponse getSyncResponse(
		HttpServletResponse res
	) {
		return new SyncResponse(res);
	}
	
	//-----------------------------------------------------------------------
	protected void logRequest(
		HttpServletRequest request, 
		String command
	) {
		SysLog.warning("Request URL", request.getRequestURL());
		SysLog.warning("Parameters", request.getParameterMap().keySet());
		for(Object parameter: request.getParameterMap().keySet()) {
			if(parameter instanceof String) {
				SysLog.warning((String)parameter, request.getParameter((String)parameter));
			}
		}
		Enumeration heads = request.getHeaderNames();
		while (heads.hasMoreElements()) {
			String h = (String) heads.nextElement();
			SysLog.warning(h + ": " + request.getHeader(h));
		}
	}

	//-----------------------------------------------------------------------
	protected void setActiveSyncHeader(
		HttpServletResponse response
	) {
		response.setHeader("Server", "Microsoft-IIS/6.0");
		response.setHeader("MS-Server-ActiveSync", "8.1");
		response.setHeader("Cache-Control", "private");
	}

	//-----------------------------------------------------------------------
	protected ServerHandler getHandler(
		String command
	) {
		return this.handlers.get(command);
	}

	//-----------------------------------------------------------------------
	protected String getUserId(
		HttpServletRequest request
	) {
		return request.getUserPrincipal() == null ? null : request.getUserPrincipal().getName();		
	}
		
	//-----------------------------------------------------------------------
	@Override
    protected void doOptions(
    	HttpServletRequest request, 
    	HttpServletResponse response
    ) throws ServletException, IOException {
		response.setStatus(HttpServletResponse.SC_OK);
		this.setActiveSyncHeader(response);
		response.setHeader("MS-ASProtocolVersions", "1.0,2.0,2.1,2.5");
		response.setHeader(
			"MS-ASProtocolCommands",
			"Sync,GetAttachment,FolderSync,MoveItems,GetItemEstimate,Ping,SendMail"
		);
		response.setHeader("Public", "OPTIONS,POST");
		response.setHeader("Allow", "OPTIONS,POST");
		response.setContentLength(0);
		return;
    }

	//-----------------------------------------------------------------------
	@Override
	protected void doPost(
		HttpServletRequest request,
		HttpServletResponse response
	) throws ServletException, IOException {
		request.getSession(true);
		SyncRequest syncRequest = this.getSyncRequest(request);
		SyncResponse syncResponse = this.getSyncResponse(response);
		String cmd = syncRequest.getCmd();
		ServerHandler handler = this.getHandler(cmd);
		if (handler == null) {
			response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			SysLog.warning("No handler for command", cmd);
			this.logRequest(request, cmd);
			return;
		}
		this.setActiveSyncHeader(response);
		try {
			handler.handle(
				syncRequest,
				syncResponse
			);
		} catch(Exception e) {
			new ServiceException(e).log();
		}
	}

	@Override
    protected void doGet(
    	HttpServletRequest request, 
    	HttpServletResponse response
    ) throws ServletException, IOException {
	    super.doGet(request, response);
    }

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private static final long serialVersionUID = -4136686109306545436L;
	
	public static final String DEFAULT_PROVIDER_NAME = "CRX";
	public static final String DEFAULT_SEGMENT_NAME = "Standard";
	public static final String DOMAIN_SEPARATOR = "\\";
	public static final String DEFAULT_PROFILE_PREFIX = "AirSync~";
	
	protected Map<String, ServerHandler> handlers;
	protected SyncBackend backend;
	
}
