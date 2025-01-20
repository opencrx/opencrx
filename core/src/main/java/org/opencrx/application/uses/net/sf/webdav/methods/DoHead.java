/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoHead
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.exceptions.AccessDeniedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.ObjectAlreadyExistsException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.openmdx.base.exception.ServiceException;

/**
 * DoHead
 *
 */
public class DoHead extends WebDavMethod {

    protected final WebDavStore _store;

    private static Logger LOG = Logger.getLogger(DoHead.class.getPackage().getName());

    /**
     * Constructor.
     * 
     * @param store
     */
    public DoHead(
    	WebDavStore store 
    ) {
        _store = store;
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.uses.net.sf.webdav.methods.WebDavMethod#execute(org.opencrx.application.uses.net.sf.webdav.RequestContext)
     */
    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        String path = getRelativePath(requestContext);
        LOG.finest("-- " + this.getClass().getName());
        Resource so = _store.getResourceByPath(requestContext, path);
        if(so == null) {
            resp.setStatus(HttpServletResponse.SC_NOT_FOUND);        	
        } else {
            try {
                String eTagMatch = req.getHeader("If-None-Match");
                if (eTagMatch != null) {
                    if (eTagMatch.equals(getETag(so))) {
                        resp.setStatus(HttpServletResponse.SC_NOT_MODIFIED);
                        return;
                    }
                }
                if(!so.isCollection()) {
                    // path points to a resource but ends with / or \
                    if (path.endsWith("/") || (path.endsWith("\\"))) {
                        resp.sendError(HttpServletResponse.SC_NOT_FOUND, req.getRequestURI());
                    } else {
                        // setting headers
                        long lastModified = so.getLastModified().getTime();
                        resp.setDateHeader("last-modified", lastModified);
                        String eTag = getETag(so);
                        resp.addHeader("ETag", eTag);
                        String mimeType = _store.getMimeType(so);
                        if (mimeType != null) {
                            resp.setContentType(mimeType);
                        } else {
                            int lastSlash = path.replace('\\', '/').lastIndexOf('/');
                            int lastDot = path.indexOf(".", lastSlash);
                            if (lastDot == -1) {
                                resp.setContentType("text/html");
                            }
                        }
                        this.doBody(requestContext, so);
                    }
                } else {
                    this.folderBody(requestContext, so);
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

    /**
     * Process request in case the resource is a folder.
     * 
     * @param requestContext
     * @param so
     * @throws IOException
     */
    protected void folderBody(
    	RequestContext requestContext, 
    	Resource so
    ) throws IOException {
        // no body for HEAD
    }

    /**
     * Process request in case the resource is not a folder.
     * 
     * @param requestContext
     * @param so
     * @throws IOException
     */
    protected void doBody(
    	RequestContext requestContext, 
        Resource so
    ) throws IOException {
        // no body for HEAD
    }
    
}
