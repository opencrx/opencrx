/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoMove
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
import java.net.URLDecoder;
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
import org.opencrx.application.uses.net.sf.webdav.exceptions.ObjectAlreadyExistsException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.openmdx.base.exception.ServiceException;


public class DoMove extends WebDavMethod {

    private static Logger LOG = Logger.getLogger(DoMove.class.getPackage().getName());

    private final WebDavStore _store;

    public DoMove(
    	WebDavStore store 
    ) {
        _store = store;
    }

    /**
     * deletes the resources at "path"
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param path
     *      the folder to be deleted
     * @param errorList
     *      all errors that occurred
     * @param req
     *      HttpServletRequest
     * @param resp
     *      HttpServletResponse
     * @throws WebdavException
     *      if an error in the underlying store occurs
     * @throws IOException
     *      when an error occurs while sending the response
     */
    public void moveResource(
    	RequestContext requestContext, 
    	Resource res,
    	String sourcePath,
    	String destinationPath,
        Hashtable<String, Integer> errorList 
    ) throws IOException, WebdavException {
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
        resp.setStatus(HttpServletResponse.SC_NO_CONTENT);
        WebDavStore.Status status = _store.moveResource(
        	requestContext, 
        	res, 
        	sourcePath, 
        	destinationPath
        );
        resp.setStatus(
        	status == WebDavStore.Status.OK_CREATED 
        		? HttpServletResponse.SC_CREATED
        		: status == WebDavStore.Status.FORBIDDEN
        			? HttpServletResponse.SC_FORBIDDEN
        			: HttpServletResponse.SC_NO_CONTENT
        );
    }

    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
        LOG.finest("-- " + this.getClass().getName());
        String sourcePath = this.getRelativePath(requestContext);
        Hashtable<String, Integer> errorList = new Hashtable<String, Integer>();
        if (!checkLocks(requestContext, _store, sourcePath)) {
            errorList.put(sourcePath, WebdavStatus.SC_LOCKED);
            sendReport(requestContext, errorList);
            return;
        }
        String destinationPath = req.getHeader("Destination");            
        if (destinationPath == null) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }
        destinationPath = this.getRelativePath(requestContext, URLDecoder.decode(destinationPath, "UTF-8"));
        if (!checkLocks(requestContext, _store, destinationPath)) {
            errorList.put(destinationPath, WebdavStatus.SC_LOCKED);
            sendReport(requestContext, errorList);
            return;
        }
        try {
        	errorList = new Hashtable<String, Integer>();
            Resource sourceSo = _store.getResourceByPath(requestContext, sourcePath);
            this.moveResource(
            	requestContext, 
            	sourceSo, 
            	sourcePath, 
            	destinationPath, 
            	errorList
            );
            if(!errorList.isEmpty()) {
                sendReport(requestContext, errorList);
            }
        } catch (AccessDeniedException e) {
            resp.sendError(HttpServletResponse.SC_FORBIDDEN);
        } catch (ObjectAlreadyExistsException e) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND, req.getRequestURI());
        } catch (WebdavException e) {
        	new ServiceException(e).log();            	
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } finally {
        }
    }

}
