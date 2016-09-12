/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoPut
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
package org.opencrx.application.uses.net.sf.webdav.methods;

import java.io.IOException;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.WebdavStatus;
import org.opencrx.application.uses.net.sf.webdav.exceptions.AccessDeniedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.openmdx.base.exception.ServiceException;

public class DoPut extends WebDavMethod {

    private static Logger LOG = Logger.getLogger(DoPut.class.getPackage().getName());

    private final WebDavStore _store;
    private final boolean _lazyFolderCreationOnPut;

    public DoPut(
    	WebDavStore store, 
        boolean lazyFolderCreationOnPut
    ) {
        _store = store;
        _lazyFolderCreationOnPut = lazyFolderCreationOnPut;
    }

    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
        LOG.finest("-- " + this.getClass().getName());
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();    	
        String path = getRelativePath(requestContext);
        String parentPath = getParentPath(path);
        Hashtable<String, Integer> errorList = new Hashtable<String, Integer>();
        if (!checkLocks(requestContext, _store, parentPath)) {
            errorList.put(parentPath, WebdavStatus.SC_LOCKED);
            sendReport(requestContext, errorList);
            return; // parent is locked
        }
        if (!checkLocks(requestContext, _store, path)) {
            errorList.put(path, WebdavStatus.SC_LOCKED);
            sendReport(requestContext, errorList);
            return; // resource is locked
        }
        Resource parentSo = null;
        try {
            parentSo = _store.getResourceByPath(requestContext, parentPath);
            if (parentPath != null && parentSo != null && !parentSo.isCollection()) {
                resp.sendError(HttpServletResponse.SC_FORBIDDEN);
                return;
            } else if (parentPath != null && parentSo == null && _lazyFolderCreationOnPut) {
                _store.createCollection(requestContext, parentPath);
            } else if (parentPath != null && parentSo == null && !_lazyFolderCreationOnPut) {
                errorList.put(parentPath, HttpServletResponse.SC_NOT_FOUND);
                sendReport(requestContext, errorList);
                return;
            }
            WebDavStore.Status putResult = _store.putResource(
            	requestContext, 
            	path, 
            	req.getInputStream(),
            	req.getContentType() 
            );
            if(putResult == WebDavStore.Status.FORBIDDEN) {
            	resp.sendError(HttpServletResponse.SC_FORBIDDEN);
            } else {
                resp.setCharacterEncoding("UTF-8");
                if(putResult != WebDavStore.Status.OK_CREATED) {
                	// Some DAV clients (e.g. CalDavZAP) require reload after creation of resource
                	resp.setHeader("ETag", Long.toString(System.currentTimeMillis()));
                }
	            resp.setStatus(
	            	putResult == WebDavStore.Status.OK_CREATED 
	            		? HttpServletResponse.SC_CREATED 
	            		: HttpServletResponse.SC_OK
	            );
            }
        } catch (AccessDeniedException e) {
            resp.sendError(HttpServletResponse.SC_FORBIDDEN);
        } catch (WebdavException e) {
        	new ServiceException(e).log();            	
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } finally {
        }

    }

}
