/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoMkcol
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
import java.util.logging.Logger;

import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.exceptions.AccessDeniedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.openmdx.base.exception.ServiceException;


public class DoMkcol extends WebDavMethod {

    private static Logger LOG = Logger.getLogger(DoMkcol.class.getPackage().getName());

    private final WebDavStore _store;

    public DoMkcol(
    	WebDavStore store
    ) {
        _store = store;
    }

    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
    	HttpServletResponse resp = requestContext.getHttpServletResponse();    	
        LOG.finest("-- " + this.getClass().getName());
        String path = getRelativePath(requestContext);
        String parentPath = getParentPath(getCleanPath(path));
        if (!checkLocks(requestContext, _store, parentPath)) {
            LOG.finest("MkCol on locked resource (parentPath) not executable!" + "\n Sending SC_FORBIDDEN (403) error response!");
            resp.sendError(HttpServletResponse.SC_FORBIDDEN);
            return;
        }
        try {
            Resource parentRes = _store.getResourceByPath(requestContext, parentPath);
			if (parentRes == null) {
				// parent not exists
				resp.sendError(HttpServletResponse.SC_CONFLICT);
				return;
			}
			if(parentPath != null) {
				if(parentRes.isCollection()) {
                    Resource res = _store.getResourceByPath(requestContext, path);
                    if(res == null) {
                        _store.createCollection(requestContext, path);
                        resp.setStatus(HttpServletResponse.SC_CREATED);
                    } else {
                        String methodsAllowed = this.determineMethodsAllowed(res);
                        resp.addHeader("Allow", methodsAllowed);
                        resp.setStatus(HttpServletResponse.SC_METHOD_NOT_ALLOWED);
                    }
				} else {
                    String methodsAllowed = this.determineMethodsAllowed(parentRes);
                    resp.addHeader("Allow", methodsAllowed);
                    resp.setStatus(HttpServletResponse.SC_METHOD_NOT_ALLOWED);
				}
			} else {
                resp.sendError(HttpServletResponse.SC_CONFLICT);
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
