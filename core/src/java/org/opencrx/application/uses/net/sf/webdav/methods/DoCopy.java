/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoCopy
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
import java.util.Collection;
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
import org.opencrx.application.uses.net.sf.webdav.exceptions.ObjectNotFoundException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.RequestUtil;
import org.openmdx.base.exception.ServiceException;


public class DoCopy extends WebDavMethod {

    private static Logger LOG = Logger.getLogger(DoCopy.class.getPackage().getName());

    private final WebDavStore _store;
    private final DoDelete _doDelete;

    public DoCopy(
    	WebDavStore store,
        DoDelete doDelete
    ) {
        _store = store;
        _doDelete = doDelete;
    }

    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
        LOG.finest("-- " + this.getClass().getName());
        HttpServletRequest req = requestContext.getHttpServletRequest();
        HttpServletResponse resp = requestContext.getHttpServletResponse();
        try {
            if (!copyResource(requestContext))
                return;
        } catch (AccessDeniedException e) {
            resp.sendError(HttpServletResponse.SC_FORBIDDEN);
        } catch (ObjectAlreadyExistsException e) {
            resp.sendError(HttpServletResponse.SC_CONFLICT, req.getRequestURI());
        } catch (ObjectNotFoundException e) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND, req.getRequestURI());
        } catch (WebdavException e) {
        	new ServiceException(e).log();
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } finally {
        }
    }

    /**
     * Copy a resource.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param req
     *      Servlet request
     * @param resp
     *      Servlet response
     * @return true if the copy is successful
     * @throws WebdavException
     *      if an error in the underlying store occurs
     * @throws IOException
     *      when an error occurs while sending the response
     * @throws LockFailedException
     */
    public boolean copyResource(
    	RequestContext requestContext
    ) throws WebdavException, IOException, LockFailedException {
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        // Parsing destination header
        String destinationPath = parseDestinationHeader(requestContext);

        if (destinationPath == null)
            return false;

        String path = getRelativePath(requestContext);

        if (path.equals(destinationPath)) {
            resp.sendError(HttpServletResponse.SC_FORBIDDEN);
            return false;
        }
        Hashtable<String, Integer> errorList = new Hashtable<String, Integer>();
        String parentDestinationPath = getParentPath(getCleanPath(destinationPath));
        if (!checkLocks(requestContext,  _store, parentDestinationPath)) {
            errorList.put(parentDestinationPath, WebdavStatus.SC_LOCKED);
            sendReport(requestContext, errorList);
            return false; // parentDestination is locked
        }
        if (!checkLocks(requestContext, _store, destinationPath)) {
            errorList.put(destinationPath, WebdavStatus.SC_LOCKED);
            sendReport(requestContext, errorList);
            return false; // destination is locked
        }
        // Parsing overwrite header
        boolean overwrite = true;
        String overwriteHeader = req.getHeader("Overwrite");
        if (overwriteHeader != null) {
            overwrite = overwriteHeader.equalsIgnoreCase("T");
        }
        // Overwriting the destination
        Resource copySo, destinationSo = null;
        try {
            copySo = _store.getResourceByPath(requestContext, path);
            // Retrieve the resources
            if (copySo == null) {
                resp.sendError(HttpServletResponse.SC_NOT_FOUND);
                return false;
            }
            errorList = new Hashtable<String, Integer>();
            destinationSo = _store.getResourceByPath(requestContext, destinationPath);
            if (overwrite) {
                // Delete destination resource, if it exists
                if (destinationSo != null) {
                    _doDelete.deleteResource(requestContext, destinationSo, destinationPath, errorList);
                } else {
                    resp.setStatus(HttpServletResponse.SC_CREATED);
                }
            } else {
                // If the destination exists, then it's a conflict
                if (destinationSo != null) {
                    resp.sendError(HttpServletResponse.SC_PRECONDITION_FAILED);
                    return false;
                } else {
                    resp.setStatus(HttpServletResponse.SC_CREATED);
                }
            }
            copy(requestContext, path, destinationPath, errorList);
            if (!errorList.isEmpty()) {
                sendReport(requestContext, errorList);
            }
        } finally {
        }
        return true;

    }

    /**
     * copies the specified resource(s) to the specified destination.
     * preconditions must be handled by the caller. Standard status codes must
     * be handled by the caller. a multi status report in case of errors is
     * created here.
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param sourcePath
     *      path from where to read
     * @param destinationPath
     *      path where to write
     * @param req
     *      HttpServletRequest
     * @param resp
     *      HttpServletResponse
     * @throws WebdavException
     *      if an error in the underlying store occurs
     * @throws IOException
     */
    private void copy(
    	RequestContext requestContext, 
    	String sourcePath,
        String destinationPath, 
        Hashtable<String, Integer> errorList
    ) throws WebdavException, IOException {
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
        Resource sourceSo = _store.getResourceByPath(requestContext, sourcePath);
        if(sourceSo != null) {
	        if(sourceSo.isCollection()) {
	            copyFolder(
	            	requestContext, 
	            	sourcePath, 
	            	destinationPath, 
	            	errorList
	            );        	
	        } else {
	            _store.putResource(
	            	requestContext,
	                destinationPath, 
	                _store.getResourceContent(
	                	requestContext,
	                    sourceSo
	                ).getContent().getContent(),
	                _store.getMimeType(sourceSo) 
	            );
	        }  
        } else {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND);
        }
    }

    /**
     * helper method of copy() recursively copies the FOLDER at source path to
     * destination path
     * 
     * @param requestContext
     *      indicates that the method is within the scope of a WebDAV
     *      transaction
     * @param sourcePath
     *      where to read
     * @param destinationPath
     *      where to write
     * @param errorList
     *      all errors that ocurred
     * @param req
     *      HttpServletRequest
     * @param resp
     *      HttpServletResponse
     * @throws WebdavException
     *      if an error in the underlying store occurs
     */
    private void copyFolder(
    	RequestContext requestContext, 
    	String sourcePath,
        String destinationPath, 
        Hashtable<String, Integer> errorList
    ) throws WebdavException {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        _store.createCollection(requestContext, destinationPath);
        boolean infiniteDepth = true;
        String depth = req.getHeader("Depth");
        if (depth != null) {
            if (depth.equals("0")) {
                infiniteDepth = false;
            }
        }
        if (infiniteDepth) {
        	Resource so = _store.getResourceByPath(requestContext, sourcePath);
            Collection<Resource> children = _store.getChildren(requestContext, so, null, null);
            for(Resource childSo: children) {
                try {
                    if(childSo.isCollection()) {
                        copyFolder(
                        	requestContext, 
                        	sourcePath + "/" + childSo.getName(),
                            destinationPath + childSo.getName(), 
                            errorList 
                        );
                    } else {
                        _store.putResource(
                        	requestContext, 
                        	destinationPath + "/" + childSo.getName(),
                            _store.getResourceContent(
                            	requestContext, 
                            	childSo
                            ).getContent().getContent(), 
                            _store.getMimeType(childSo) 
                        );
                    }
                } catch (AccessDeniedException e) {
                    errorList.put(destinationPath + "/" + childSo.getName(), new Integer(
                            HttpServletResponse.SC_FORBIDDEN));
                } catch (ObjectNotFoundException e) {
                    errorList.put(destinationPath + "/" + childSo.getName(), new Integer(
                            HttpServletResponse.SC_NOT_FOUND));
                } catch (ObjectAlreadyExistsException e) {
                    errorList.put(destinationPath + "/" + childSo.getName(), new Integer(
                            HttpServletResponse.SC_CONFLICT));
                } catch (Exception e) {
                    errorList.put(destinationPath + "/" + childSo.getName(), new Integer(
                            HttpServletResponse.SC_INTERNAL_SERVER_ERROR));
                }
            }
        }
    }

    /**
     * Parses and normalizes the destination header.
     * 
     * @param req
     *      Servlet request
     * @param resp
     *      Servlet response
     * @return destinationPath
     * @throws IOException
     *      if an error occurs while sending response
     */
    private String parseDestinationHeader(
    	RequestContext requestContext
    ) throws IOException {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
        String destinationPath = req.getHeader("Destination");
        if (destinationPath == null) {
            resp.sendError(HttpServletResponse.SC_BAD_REQUEST);
            return null;
        }
        // Remove url encoding from destination
        destinationPath = RequestUtil.URLDecode(destinationPath, "UTF8");

        int protocolIndex = destinationPath.indexOf("://");
        if (protocolIndex >= 0) {
            // if the Destination URL contains the protocol, we can safely
            // trim everything upto the first "/" character after "://"
            int firstSeparator = destinationPath.indexOf("/", protocolIndex + 4);
            if (firstSeparator < 0) {
                destinationPath = "/";
            } else {
                destinationPath = destinationPath.substring(firstSeparator);
            }
        } else {
            String hostName = req.getServerName();
            if ((hostName != null) && (destinationPath.startsWith(hostName))) {
                destinationPath = destinationPath.substring(hostName.length());
            }
            int portIndex = destinationPath.indexOf(":");
            if (portIndex >= 0) {
                destinationPath = destinationPath.substring(portIndex);
            }
            if (destinationPath.startsWith(":")) {
                int firstSeparator = destinationPath.indexOf("/");
                if (firstSeparator < 0) {
                    destinationPath = "/";
                } else {
                    destinationPath = destinationPath.substring(firstSeparator);
                }
            }
        }

        // Normalize destination path (remove '.' and' ..')
        destinationPath = normalize(destinationPath);

        String contextPath = req.getContextPath();
        if ((contextPath != null) && (destinationPath.startsWith(contextPath))) {
            destinationPath = destinationPath.substring(contextPath.length());
        }

        String pathInfo = req.getPathInfo();
        if (pathInfo != null) {
            String servletPath = req.getServletPath();
            if ((servletPath != null)
                    && (destinationPath.startsWith(servletPath))) {
                destinationPath = destinationPath.substring(servletPath.length());
            }
        }
        return destinationPath;
    }

    /**
     * Return a context-relative path, beginning with a "/", that represents the
     * canonical version of the specified path after ".." and "." elements are
     * resolved out. If the specified path attempts to go outside the boundaries
     * of the current context (i.e. too many ".." path elements are present),
     * return <code>null</code> instead.
     * 
     * @param path
     *      Path to be normalized
     * @return normalized path
     */
    protected String normalize(
    	String path
    ) {

        if (path == null)
            return null;

        // Create a place for the normalized path
        String normalized = path;

        if (normalized.equals("/."))
            return "/";

        // Normalize the slashes and add leading slash if necessary
        if (normalized.indexOf('\\') >= 0)
            normalized = normalized.replace('\\', '/');
        if (!normalized.startsWith("/"))
            normalized = "/" + normalized;

        // Resolve occurrences of "//" in the normalized path
        while (true) {
            int index = normalized.indexOf("//");
            if (index < 0)
                break;
            normalized = normalized.substring(0, index)
                    + normalized.substring(index + 1);
        }

        // Resolve occurrences of "/./" in the normalized path
        while (true) {
            int index = normalized.indexOf("/./");
            if (index < 0)
                break;
            normalized = normalized.substring(0, index)
                    + normalized.substring(index + 2);
        }

        // Resolve occurrences of "/../" in the normalized path
        while (true) {
            int index = normalized.indexOf("/../");
            if (index < 0)
                break;
            if (index == 0)
                return (null); // Trying to go outside our context
            int index2 = normalized.lastIndexOf('/', index - 1);
            normalized = normalized.substring(0, index2)
                    + normalized.substring(index + 3);
        }

        // Return the normalized path that we have completed
        return (normalized);

    }

}
