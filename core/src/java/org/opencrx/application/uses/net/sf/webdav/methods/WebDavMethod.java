/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: AbstractMethod
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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.opencrx.application.uses.net.sf.webdav.Lock;
import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.WebdavStatus;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLWriter;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.text.conversion.XMLEncoder;

public abstract class WebDavMethod {

    /**
     * Default depth is infite.
     */
    protected static final int INFINITY = 3;

    /**
     * Simple date format for the creation date ISO 8601 representation
     * (partial).
     */
    protected static final ThreadLocal<SimpleDateFormat> CREATION_DATE_FORMAT = new ThreadLocal<SimpleDateFormat>(){
		@Override
        protected SimpleDateFormat initialValue() {
			SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
			format.setTimeZone(TimeZone.getTimeZone("GMT"));
			return format;
        }
    };

    /**
     * Simple date format for the last modified date. (RFC 822 updated by RFC
     * 1123)
     */
    protected static final ThreadLocal<SimpleDateFormat> LAST_MODIFIED_DATE_FORMAT = new ThreadLocal<SimpleDateFormat>(){
		@Override
        protected SimpleDateFormat initialValue() {
			SimpleDateFormat format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US);
			format.setTimeZone(TimeZone.getTimeZone("GMT"));
			return format;
        }    	    	
    };

    /**
     * size of the io-buffer
     */
    protected static int BUF_SIZE = 65536;

    /**
     * Default lock timeout value.
     */
    protected static final int DEFAULT_TIMEOUT = 3600;

    /**
     * Maximum lock timeout.
     */
    protected static final int MAX_TIMEOUT = 604800;

    /**
     * Boolean value to temporary lock resources (for method locks)
     */
    protected static final boolean TEMPORARY = true;

    /**
     * Timeout for temporary locks
     */
    protected static final int TEMP_TIMEOUT = 10;

    /**
     * To be implemented by concrete method.
     * @param requestContext
     * @param req
     * @param resp
     * @throws IOException
     * @throws LockFailedException
     */
    public abstract void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException;
    
    /**
     * Return the relative path associated with this servlet.
     * 
     * @param request
     *      The servlet request we are processing
     */
    protected String getRelativePath(
    	RequestContext requestContext
    ) {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	return req.getServletPath();
    }

    protected String getRelativePath(
    	RequestContext requestContext,
    	String absolutePath
    ) {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	String contextPath = req.getContextPath();
    	int pos = absolutePath.indexOf(contextPath);
    	return pos > 0 ?
    		absolutePath.substring(pos + contextPath.length()) :
    			absolutePath;
    }
    	
    /**
     * creates the parent path from the given path by removing the last '/' and
     * everything after that
     * 
     * @param path
     *      the path
     * @return parent path
     */
    protected String getParentPath(
    	String path
    ) {
        int slash = path.lastIndexOf('/');
        if (slash != -1) {
            return path.substring(0, slash);
        }
        return null;
    }

    /**
     * removes a / at the end of the path string, if present
     * 
     * @param path
     *      the path
     * @return the path without trailing /
     */
    protected String getCleanPath(
    	String path
    ) {
        if (path.endsWith("/") && path.length() > 1)
            path = path.substring(0, path.length() - 1);
        return path;
    }

    /**
     * Return JAXP document builder instance.
     */
    protected DocumentBuilder getDocumentBuilder(
    ) throws ServiceException {
        DocumentBuilder documentBuilder = null;
        DocumentBuilderFactory documentBuilderFactory = null;
        try {
            documentBuilderFactory = DocumentBuilderFactory.newInstance();
            documentBuilderFactory.setNamespaceAware(true);
            documentBuilder = documentBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new ServiceException(e);
        }
        return documentBuilder;
    }

    /**
     * Reads the depth header from the request and returns it as a int.
     * 
     * @param req
     * @return the depth from the depth header
     */
    protected int getDepth(
    	RequestContext requestContext
    ) {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        int depth = 1; // default is 1
        String depthStr = req.getHeader("Depth");
        if(depthStr != null) {
        	try {
        		depth = Integer.parseInt(depthStr);
        	} catch(Exception ignore) {}
        }
        return depth;
    }

    /**
     * URL rewriter.
     * 
     * @param path
     *      Path which has to be rewritten
     * @return the rewritten path
     */
    protected String encodeURL(
    	HttpServletResponse resp,
    	String path
    ) {    	
    	path = path.replace("%", "%25");
   		path = path.replace(" ", "%20");
   		path = XMLEncoder.encode(path);
   		return path;
    }

    protected String getHRef(
    	HttpServletRequest req,
    	String suffix,
    	boolean isCollection
    ) {
        String href = req.getContextPath();
        if ((href.endsWith("/")) && (suffix.startsWith("/"))) {
            href += suffix.substring(1);
        }
        else {
            href += suffix;
        }
        if ((isCollection) && (!href.endsWith("/"))) {
            href += "/";
        }
        return href;
    }
    
    /**
     * Get the ETag associated with a resource.
     * 
     * @param so
     *      StoredObject to get resourceLength, lastModified and a hashCode of
     *      StoredObject
     * @return the ETag
     */
    protected String getETag(Resource so) {

        Date lastModified = new Date();
        if (so != null && so.getLastModified() != null) {        	
            lastModified = so.getLastModified();
        }
        return "W/\"" + Long.toString(lastModified.getTime()) + "\"";
    }

    protected String getVersion(
    ) {
    	return "1, 2";
    }
        
    protected String[] getLockIdFromIfHeader(
    	RequestContext requestContext
    ) {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        String[] ids = new String[2];
        String id = req.getHeader("If");

        if (id != null && !id.equals("")) {
            if (id.indexOf(">)") == id.lastIndexOf(">)")) {
                id = id.substring(id.indexOf("(<"), id.indexOf(">)"));

                if (id.indexOf("locktoken:") != -1) {
                    id = id.substring(id.indexOf(':') + 1);
                }
                ids[0] = id;
            } else {
                String firstId = id.substring(id.indexOf("(<"), id.indexOf(">)"));
                if (firstId.indexOf("locktoken:") != -1) {
                    firstId = firstId.substring(firstId.indexOf(':') + 1);
                }
                ids[0] = firstId;

                String secondId = id.substring(id.lastIndexOf("(<"), id
                        .lastIndexOf(">)"));
                if (secondId.indexOf("locktoken:") != -1) {
                    secondId = secondId.substring(secondId.indexOf(':') + 1);
                }
                ids[1] = secondId;
            }

        } else {
            ids = null;
        }
        return ids;
    }

    /**
     * Get lock token.
     * 
     * @param requestContext
     * @return
     */
    protected String getLockIdFromLockTokenHeader(
    	RequestContext requestContext
    ) {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        String id = req.getHeader("Lock-Token");
        if (id != null) {
            id = id.substring(id.indexOf(":") + 1, id.indexOf(">"));

        }
        return id;
    }

    /**
     * Checks if locks on resources at the given path exists and if so checks
     * the If-Header to make sure the If-Header corresponds to the locked
     * resource. Returning true if no lock exists or the If-Header is
     * corresponding to the locked resource
     * 
     * @param req
     *      Servlet request
     * @param resp
     *      Servlet response
     * @param store
     * @param path
     *      path to the resource
     * @return true if no lock on a resource with the given path exists or if
     *  the If-Header corresponds to the locked resource
     * @throws IOException
     * @throws LockFailedException
     */
    protected boolean checkLocks(
    	RequestContext requestContext,
        WebDavStore store, 
        String path
    ) throws IOException, LockFailedException {
        List<Lock> losByPath = store.getLocksByPath(requestContext, path);
        for(Lock lo: losByPath) {
            if(!Lock.SCOPE_SHARED.equals(lo.getScope())) {
	            // the resource is locked
	            String[] lockTokens = getLockIdFromIfHeader(requestContext);
	            String lockToken = null;
	            if (lockTokens != null)
	                lockToken = lockTokens[0];
	            else {
	                return false;
	            }
	            if(lockToken != null) {
	                if (!lockToken.equals(lo.getID())) {
	                    return false;
	                }
	            }
            }
        }
        return true;
    }

    /**
     * Send a multistatus element containing a complete error report to the
     * client.
     * 
     * @param req
     *      Servlet request
     * @param resp
     *      Servlet response
     * @param errorList
     *      List of error to be displayed
     */
    protected void sendReport(
    	RequestContext requestContext,
        Hashtable<String, Integer> errorList
     ) throws IOException {
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        resp.setStatus(WebdavStatus.SC_MULTI_STATUS);
        String absoluteUri = req.getRequestURI();
        // String relativePath = getRelativePath(req);
        HashMap<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("DAV:", "D");
        XMLWriter writer = new XMLWriter(resp.getWriter(), namespaces);
        writer.writeXMLHeader();
        writer.writeElement("DAV::multistatus", XMLWriter.OPENING);
        Enumeration<String> pathList = errorList.keys();
        while (pathList.hasMoreElements()) {
            String errorPath = pathList.nextElement();
            int errorCode = errorList.get(errorPath);
            writer.writeElement("DAV::response", XMLWriter.OPENING);
            writer.writeElement("DAV::href", XMLWriter.OPENING);
            String toAppend = null;
            if (absoluteUri.endsWith(errorPath)) {
                toAppend = absoluteUri;
            } else if (absoluteUri.contains(errorPath)) {
                int endIndex = absoluteUri.indexOf(errorPath) + errorPath.length();
                toAppend = absoluteUri.substring(0, endIndex);
            }
            if (!toAppend.startsWith("/") && !toAppend.startsWith("http:")) {
                toAppend = "/" + toAppend;
            }
            writer.writeText(errorPath);
            writer.writeElement("DAV::href", XMLWriter.CLOSING);
            writer.writeElement("DAV::status", XMLWriter.OPENING);
            writer.writeText("HTTP/1.1 " + errorCode);
            writer.writeElement("DAV::status", XMLWriter.CLOSING);
            writer.writeElement("DAV::response", XMLWriter.CLOSING);
        }
        writer.writeElement("DAV::multistatus", XMLWriter.CLOSING);
        writer.sendData();
    }

    protected String getNullResourceMethodsAllowed(
    ) {
    	return NULL_RESOURCE_METHODS_ALLOWED;
    }
    
    protected String getResourceMethodsAllowed(
    ) {
    	return RESOURCE_METHODS_ALLOWED;
    }
    
    public String getFolderMethodsAllowed(
    ) {
    	return FOLDER_METHODS_ALLOWED;
    }
    
    public String getDefaultMethodsAllowed(
    ) {
    	return DEFAULT_METHODS_ALLOWED;
    }
    
    /**
     * Determines the methods normally allowed for the resource.
     * 
     * @param so
     *      StoredObject representing the resource
     * @return all allowed methods, separated by commas
     */
    protected String determineMethodsAllowed(
    	Resource so
    ) {
        try {
            if (so != null) {
                if (so.isCollection()) {
                    return this.getDefaultMethodsAllowed() + ", " + this.getResourceMethodsAllowed() + ", " + this.getFolderMethodsAllowed();
                } else {
                	return this.getDefaultMethodsAllowed() + ", " + getResourceMethodsAllowed();
                }
            }
        } catch (Exception e) {
            // we do nothing, just return default allowed methods
        }
        return getDefaultMethodsAllowed();
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final String NULL_RESOURCE_METHODS_ALLOWED = "OPTIONS, MKCOL, PUT, PROPFIND, LOCK, UNLOCK";
    private static final String RESOURCE_METHODS_ALLOWED = "OPTIONS, GET, HEAD, POST, DELETE, TRACE, PROPPATCH, COPY, MOVE, LOCK, UNLOCK, PROPFIND";
    private static final String FOLDER_METHODS_ALLOWED = "PUT";
    private static final String DEFAULT_METHODS_ALLOWED = "OPTIONS, MKCOL, PUT";
    
}
