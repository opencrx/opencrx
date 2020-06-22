/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: AbstractWebDavServlet
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

/*
 * This source was originally published under net.sf.webdav.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.opencrx.application.uses.net.sf.webdav;

import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.uses.net.sf.webdav.methods.DoCopy;
import org.opencrx.application.uses.net.sf.webdav.methods.DoDelete;
import org.opencrx.application.uses.net.sf.webdav.methods.DoGet;
import org.opencrx.application.uses.net.sf.webdav.methods.DoHead;
import org.opencrx.application.uses.net.sf.webdav.methods.DoLock;
import org.opencrx.application.uses.net.sf.webdav.methods.DoMkcol;
import org.opencrx.application.uses.net.sf.webdav.methods.DoMove;
import org.opencrx.application.uses.net.sf.webdav.methods.DoNotImplemented;
import org.opencrx.application.uses.net.sf.webdav.methods.DoOptions;
import org.opencrx.application.uses.net.sf.webdav.methods.DoPropfind;
import org.opencrx.application.uses.net.sf.webdav.methods.DoProppatch;
import org.opencrx.application.uses.net.sf.webdav.methods.DoPut;
import org.opencrx.application.uses.net.sf.webdav.methods.DoReport;
import org.opencrx.application.uses.net.sf.webdav.methods.DoUnlock;
import org.opencrx.application.uses.net.sf.webdav.methods.WebDavMethod;
import org.openmdx.base.exception.ServiceException;

public abstract class AbstractWebDavServlet extends HttpServlet {

    private static final long serialVersionUID = -4817456237932116287L;

	private static Logger LOG = Logger.getLogger(AbstractWebDavServlet.class.getPackage().getName());

    private WebDavStore _store;
    private HashMap<String, WebDavMethod> _methodMap = new HashMap<String, WebDavMethod>();

    public AbstractWebDavServlet(
    ) {
    }

    @Override
    public void init(
        ServletConfig conf    	
    ) throws ServletException {
    	super.init(conf);
        WebDavStore webdavStore = this.constructStore();
        boolean lazyFolderCreationOnPut = Boolean.valueOf(getInitParameter("lazyFolderCreationOnPut"));
        this.init(
        	webdavStore, 
            lazyFolderCreationOnPut
        );
    }

    protected abstract WebDavStore constructStore();
    
    protected WebDavMethod newDoGet(
    	WebDavStore store
    ) {    	
    	return new DoGet(store);    	
    }
    
    protected WebDavMethod newDoReport(
    	WebDavStore store
    ) {    	
    	return new DoReport(store);    	
    }
    
    protected WebDavMethod newDoHead(
    	WebDavStore store 
    ) {    	
    	return new DoHead(store);
    }
    
    protected WebDavMethod newDoDelete(
    	WebDavStore store
    ) {    	
    	return new DoDelete(store);
    }

    protected WebDavMethod newDoCopy(
    	WebDavStore store,
    	DoDelete doDelete
    ) {    	
    	return new DoCopy(store, doDelete);
    }

    protected WebDavMethod newDoLock(
    	WebDavStore store
    ) {    	
    	return new DoLock(store);
    }

    protected WebDavMethod newDoUnlock(
    	WebDavStore store
    ) {    	
    	return new DoUnlock(store);
    }

    protected WebDavMethod newDoMove(
    	WebDavStore store
    ) {    	
    	return new DoMove(store);
    }

    protected WebDavMethod newDoMkcol(
    	WebDavStore store
    ) {    	
    	return new DoMkcol(store);
    }

    protected WebDavMethod newDoOptions(
    	WebDavStore store
    ) {    	
    	return new DoOptions(store);
    }

    protected WebDavMethod newDoPut(
    	WebDavStore store, 
    	boolean lazyFolderCreationOnPut
    ) {    	
    	return new DoPut(store, lazyFolderCreationOnPut);
    }

    protected WebDavMethod newDoPropfind(
    	WebDavStore store 
    ) {    	
    	return new DoPropfind(store);
    }

    protected WebDavMethod newDoProppatch(
    	WebDavStore store
    ) {    	
    	return new DoProppatch(store);
    }

    public void init(
    	WebDavStore store, 
    	boolean lazyFolderCreationOnPut
    ) throws ServletException {

        _store = store;

        register("GET", this.newDoGet(store));
        register("REPORT", this.newDoReport(store));
        register("HEAD", this.newDoHead(store));
        DoDelete doDelete = (DoDelete) register("DELETE", this.newDoDelete(store));
        register("COPY", this.newDoCopy(store, doDelete));
        register("LOCK", this.newDoLock(store));
        register("UNLOCK", this.newDoUnlock(store));
        register("MOVE", this.newDoMove(store));
        register("MKCOL", this.newDoMkcol(store));
        register("OPTIONS", this.newDoOptions(store));
        register("PUT", this.newDoPut(store, lazyFolderCreationOnPut));
        register("PROPFIND", this.newDoPropfind(store));
        register("PROPPATCH", this.newDoProppatch(store));
        register("*NO*IMPL*", new DoNotImplemented());
    }

    private WebDavMethod register(
    	String methodName, 
    	WebDavMethod method
    ) {
        _methodMap.put(methodName, method);
        return method;
    }

    /**
     * Handles the special WebDAV methods.
     */
    @Override
    protected void service(
    	HttpServletRequest req, 
    	HttpServletResponse resp
    ) throws ServletException, IOException {
        String methodName = req.getMethod();
        RequestContext transaction = null;
        if (LOG.isLoggable(Level.FINEST)) {
            debugRequest(methodName, req);
        }
        try {
            transaction = _store.begin(req, resp);
        	WebDavMethod methodExecutor = _methodMap.get(methodName);
            if (methodExecutor == null) {
                methodExecutor = _methodMap.get("*NO*IMPL*");
            }
            methodExecutor.execute(transaction);
            _store.commit(transaction);
        } catch (Exception e) {
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);           
            _store.rollback(transaction);
            new ServiceException(e).log();
        } finally {
        }
    }

    private void debugRequest(
    	String methodName, 
    	HttpServletRequest req
    ) {
        LOG.finest("-----------");
        LOG.finest("WebdavServlet\n request: methodName = " + methodName);
        LOG.finest("time: " + System.currentTimeMillis());
        LOG.finest("path: " + req.getRequestURI());
        LOG.finest("-----------");
        Enumeration<?> e = req.getHeaderNames();
        while (e.hasMoreElements()) {
            String s = (String) e.nextElement();
            LOG.finest("header: " + s + " " + req.getHeader(s));
        }
        e = req.getAttributeNames();
        while (e.hasMoreElements()) {
            String s = (String) e.nextElement();
            LOG.finest("attribute: " + s + " " + req.getAttribute(s));
        }
        e = req.getParameterNames();
        while (e.hasMoreElements()) {
            String s = (String) e.nextElement();
            LOG.finest("parameter: " + s + " " + req.getParameter(s));
        }
    }

}
