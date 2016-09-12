/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoPropfind
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;

import org.opencrx.application.uses.net.sf.webdav.Lock;
import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.WebdavStatus;
import org.opencrx.application.uses.net.sf.webdav.exceptions.AccessDeniedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLHelper;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLWriter;
import org.openmdx.base.exception.ServiceException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

public class DoPropfind extends WebDavMethod {

    private static Logger LOG = Logger.getLogger(DoPropfind.class.getPackage().getName());

    /**
     * PROPFIND - Specify a property mask.
     */
    private static final int FIND_BY_PROPERTY = 0;

    /**
     * PROPFIND - Display all properties.
     */
    private static final int FIND_ALL_PROP = 1;

    /**
     * PROPFIND - Return property names.
     */
    private static final int FIND_PROPERTY_NAMES = 2;

    private final WebDavStore _store;

    public DoPropfind(
    	WebDavStore store 
    ) {
        _store = store;
    }

    protected boolean handleExtension(
    	RequestContext requestContext,
    	XMLWriter writer,
    	String contextPath,    	
    	Resource res,
    	String property
    ) {
    	return false;
    }
    
    protected void writeCollectionType(
    	RequestContext requestContext,
    	XMLWriter writer,
    	Resource res
    ) {    	
    }

    protected Map<String,String> getNamespaces(
    ) {
        Map<String, String> namespaces = new HashMap<String, String>();
        namespaces.put("DAV:", "D");
        return namespaces;
    }
    
    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();    	    	
        LOG.finest("-- " + this.getClass().getName());
        // Retrieve the resources
        String path = this.getCleanPath(this.getRelativePath(requestContext));
        Resource res = null;
        try {
            res = _store.getResourceByPath(requestContext, path);
            if(res == null) {
            	resp.setCharacterEncoding("UTF-8");
                resp.setContentType("application/xml");
                resp.sendError(HttpServletResponse.SC_NOT_FOUND, req.getRequestURI());
                return;
            }
            int depth = this.getDepth(requestContext);
            List<String> properties = null;
            path = this.getCleanPath(getRelativePath(requestContext));
            int propertyFindType = FIND_ALL_PROP;
            Node propNode = null;
            if (req.getContentLength() > 0) {
                try {
                    DocumentBuilder documentBuilder = getDocumentBuilder();
                    Document document = documentBuilder.parse(new InputSource(req.getInputStream()));
                    // Get the root element of the document
                    Element rootElement = document.getDocumentElement();
                    propNode = XMLHelper.findSubElement(rootElement, "prop");
                    if (propNode != null) {
                        propertyFindType = FIND_BY_PROPERTY;
                    } else if (XMLHelper.findSubElement(rootElement, "propname") != null) {
                        propertyFindType = FIND_PROPERTY_NAMES;
                    } else if (XMLHelper.findSubElement(rootElement, "allprop") != null) {
                        propertyFindType = FIND_ALL_PROP;
                    }
                } catch (Exception e) {
                	new ServiceException(e).log();                	
                    resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                    return;
                }
            } else {
                // no content, which means it is a allprop request
                propertyFindType = FIND_ALL_PROP;
            }            
            Map<String, String> namespaces = this.getNamespaces();
            if (propertyFindType == FIND_BY_PROPERTY) {
                propertyFindType = 0;
                properties = XMLHelper.getPropertiesFromXML(propNode);
            }
            resp.setStatus(WebdavStatus.SC_MULTI_STATUS);
            resp.setCharacterEncoding("UTF-8");
            resp.setContentType("application/xml ");
            resp.addHeader("DAV", this.getVersion());
            // Create multistatus object
            XMLWriter writer = new XMLWriter(resp.getWriter(), namespaces);
            writer.writeXMLHeader();
            writer.writeElement("DAV::multistatus", XMLWriter.OPENING);
            if(depth == 0) {
                this.parseProperties(
                	requestContext, 
                	writer, 
                	res,
                	path,
                    propertyFindType, 
                    properties, 
                    depth
                );
            } else {
                this.recursiveParseProperties(
                	requestContext,
                    writer, 
                	res,
                	path, 
                    propertyFindType, 
                    properties, 
                    depth,
                    depth
                );
            }
            writer.writeElement("DAV::multistatus", XMLWriter.CLOSING);
            writer.sendData();
        } catch (AccessDeniedException e) {
            resp.sendError(HttpServletResponse.SC_FORBIDDEN);
        } catch (WebdavException e) {
        	new ServiceException(e).log();
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        } finally {
        }
    }

    /**
     * goes recursive through all folders. used by propfind
     * 
     * @param path
     *      the current path
     * @param req
     *      HttpServletRequest
     * @param writer
     * @param propertyFindType
     * @param properties
     * @param currentDepth
     *      depth of the propfind
     * @throws IOException
     *      if an error in the underlying store occurs
     */
    protected void recursiveParseProperties(
    	RequestContext requestContext,
        XMLWriter writer,
    	Resource res,
        String path, 
        int propertyFindType, 
        List<String> properties, 
        int currentDepth,
        int depth
    ) throws WebdavException {
        this.parseProperties(
        	requestContext, 
        	writer,   
        	res,
        	path,
            propertyFindType, 
            properties, 
            depth
        );
        if (currentDepth > 0) {
        	Resource parent = _store.getResourceByPath(requestContext, path);
            Collection<Resource> children = _store.getChildren(requestContext, parent, null, null);
            if(!path.endsWith("/")) {
            	path += "/";
            }
            for(Resource child: children) {
                String childPath = path + child.getName();
                this.recursiveParseProperties(
                	requestContext, 
                    writer, 
                	child,
                	childPath, 
                    propertyFindType, 
                    properties, 
                    currentDepth - 1,
                    depth
                );
            }
        }
    }

    /**
     * Propfind helper method.
     * 
     * @param req
     *      The servlet request
     * @param writer
     *      XML response to the Propfind request
     * @param path
     *      Path of the current resource
     * @param type
     *      Propfind type
     * @param propertiesVector
     *      If the propfind type is find properties by name, then this Vector
     *      contains those properties
     */
    protected void parseProperties(
    	RequestContext requestContext,
    	XMLWriter writer, 
    	Resource res,
    	String path,
    	int type, 
    	List<String> propertiesVector, 
    	int depth
    ) throws WebdavException {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
        boolean isCollection = res.isCollection();
        String creationdate = CREATION_DATE_FORMAT.get().format(res.getCreationDate());
        String lastModified = LAST_MODIFIED_DATE_FORMAT.get().format(res.getLastModified());        
        Long resourceLength = null;
        // ResourceInfo resourceInfo = new ResourceInfo(path, resources);
        writer.writeElement("DAV::response", XMLWriter.OPENING);
        String status = new String("HTTP/1.1 " + HttpServletResponse.SC_OK + " OK");
        writer.writeElement("DAV::href", XMLWriter.OPENING);
        writer.writeText(this.encodeURL(resp, this.getHRef(req, path, isCollection)));
        writer.writeElement("DAV::href", XMLWriter.CLOSING);
        String displayName = res.getDisplayName();
        switch (type) {
	        
	        case FIND_ALL_PROP:
	            writer.writeElement("DAV::propstat", XMLWriter.OPENING);
	            writer.writeElement("DAV::prop", XMLWriter.OPENING);
	            writer.writeProperty("DAV::creationdate", creationdate);
                writer.writeProperty("DAV::getlastmodified", lastModified);
	            writer.writeElement("DAV::displayname", XMLWriter.OPENING);
	            writer.writeData(displayName);
	            writer.writeElement("DAV::displayname", XMLWriter.CLOSING);
	            if(!isCollection) {
	                if(resourceLength == null) {
	                    try {
	                    	resourceLength = _store.getResourceContent(requestContext, res).getLength();
	                    } catch(Exception e) {
	                    	resourceLength = 0L;
	                    }
	                }
	                writer.writeProperty("DAV::getcontentlength", Long.toString(resourceLength));
	                String contentType = _store.getMimeType(res);
	                if (contentType != null) {
	                    writer.writeProperty("DAV::getcontenttype", contentType);
	                }
	                writer.writeProperty("DAV::getetag", getETag(res));
	                writer.writeElement("DAV::resourcetype", XMLWriter.NO_CONTENT);
	            } else {
	                writer.writeElement("DAV::resourcetype", XMLWriter.OPENING);
	                writer.writeElement("DAV::collection", XMLWriter.NO_CONTENT);
	                this.writeCollectionType(requestContext, writer, res);
	                writer.writeElement("DAV::resourcetype", XMLWriter.CLOSING);
	            }
	            writeSupportedLockElements(requestContext, writer, path);
	            writeLockDiscoveryElements(requestContext, writer, path, depth);
	            writer.writeProperty("DAV::source", "");
	            writer.writeElement("DAV::prop", XMLWriter.CLOSING);
	            writer.writeElement("DAV::status", XMLWriter.OPENING);
	            writer.writeText(status);
	            writer.writeElement("DAV::status", XMLWriter.CLOSING);
	            writer.writeElement("DAV::propstat", XMLWriter.CLOSING);
	
	            break;
	
	        case FIND_PROPERTY_NAMES:
	
	            writer.writeElement("DAV::propstat", XMLWriter.OPENING);
	            writer.writeElement("DAV::prop", XMLWriter.OPENING);
	            writer.writeElement("DAV::creationdate", XMLWriter.NO_CONTENT);
	            writer.writeElement("DAV::displayname", XMLWriter.NO_CONTENT);
	            if (!isCollection) {
	                writer.writeElement("DAV::getcontentlanguage", XMLWriter.NO_CONTENT);
	                writer.writeElement("DAV::getcontentlength", XMLWriter.NO_CONTENT);
	                writer.writeElement("DAV::getcontenttype", XMLWriter.NO_CONTENT);
	                writer.writeElement("DAV::getetag", XMLWriter.NO_CONTENT);
	                writer.writeElement("DAV::getlastmodified", XMLWriter.NO_CONTENT);
	            }
	            writer.writeElement("DAV::resourcetype", XMLWriter.NO_CONTENT);
	            writer.writeElement("DAV::supportedlock", XMLWriter.NO_CONTENT);
	            writer.writeElement("DAV::source", XMLWriter.NO_CONTENT);
	            writer.writeElement("DAV::prop", XMLWriter.CLOSING);
	            writer.writeElement("DAV::status", XMLWriter.OPENING);
	            writer.writeText(status);
	            writer.writeElement("DAV::status", XMLWriter.CLOSING);
	            writer.writeElement("DAV::propstat", XMLWriter.CLOSING);
	
	            break;
	
	        case FIND_BY_PROPERTY:
	
	            List<String> propertiesNotFound = new ArrayList<String>();
	            // Parse the list of properties
	            writer.writeElement("DAV::propstat", XMLWriter.OPENING);
	            writer.writeElement("DAV::prop", XMLWriter.OPENING);
	            Iterator<String> properties = propertiesVector.iterator();
	            while (properties.hasNext()) {
	                String property = properties.next();
	                if(property.indexOf("creationdate") > 0) {
	                    writer.writeProperty("DAV::creationdate", creationdate);
	                } else if(property.indexOf("displayname") > 0) {
	                    writer.writeElement("DAV::displayname", XMLWriter.OPENING);
	                    writer.writeData(displayName);
	                    writer.writeElement("DAV::displayname", XMLWriter.CLOSING);
	                } else if(property.indexOf("getcontentlanguage") > 0) {
	                    if (isCollection) {
	                        propertiesNotFound.add(property);
	                    } else {
	                        writer.writeElement("DAV::getcontentlanguage", XMLWriter.NO_CONTENT);
	                    }
	                } else if(property.indexOf("getcontentlength") > 0) {
	                    if (isCollection) {
	                        propertiesNotFound.add(property);
	                    } else {
	                    	if(resourceLength == null) {
	                            try {
	                            	resourceLength = _store.getResourceContent(requestContext, res).getLength();
	                            } catch(Exception e) {
	                            	resourceLength = 0L;	                            	
	                            }	                    		
	                    	}
	                        writer.writeProperty("DAV::getcontentlength", Long.toString(resourceLength));
	                    }
	                } else if(property.indexOf("getcontenttype") > 0) {
                        writer.writeProperty("DAV::getcontenttype", _store.getMimeType(res));
	                } else if(property.indexOf("getetag") > 0) {
	                    if (isCollection) {
	                        propertiesNotFound.add(property);
	                    } else {
	                        writer.writeProperty("DAV::getetag", getETag(res));
	                    }
	                } else if (property.equals("DAV::getlastmodified")) {
	                    if (isCollection) {
	                        propertiesNotFound.add(property);
	                    } else {
	                        writer.writeProperty("DAV::getlastmodified", lastModified);
	                    }
	                } else if(property.indexOf("resourcetype") > 0) {
	                    if (isCollection) {
	                        writer.writeElement("DAV::resourcetype", XMLWriter.OPENING);
	                        writer.writeElement("DAV::collection", XMLWriter.NO_CONTENT);
	    	                this.writeCollectionType(requestContext, writer, res);
	                        writer.writeElement("DAV::resourcetype", XMLWriter.CLOSING);
	                    } else {
	                        writer.writeElement("DAV::resourcetype", XMLWriter.NO_CONTENT);
	                    }
	                } else if (property.indexOf("source") > 0) {
	                    writer.writeProperty("DAV::source", "");
	                } else if(property.indexOf("supportedlock") > 0) {
	                    writeSupportedLockElements(requestContext, writer, path);
	                } else if(property.indexOf("lockdiscovery") > 0) {
	                    writeLockDiscoveryElements(requestContext, writer, path, depth);
	                } else if(!handleExtension(requestContext, writer, req.getContextPath(), res, property)) {
	                    propertiesNotFound.add(property);    		                		
	                }
	            }
	            writer.writeElement("DAV::prop", XMLWriter.CLOSING);
	            writer.writeElement("DAV::status", XMLWriter.OPENING);
	            writer.writeText(status);
	            writer.writeElement("DAV::status", XMLWriter.CLOSING);
	            writer.writeElement("DAV::propstat", XMLWriter.CLOSING);
	            Iterator<String> propertiesNotFoundList = propertiesNotFound.iterator();
	            if (propertiesNotFoundList.hasNext()) {
	                status = new String("HTTP/1.1 " + HttpServletResponse.SC_NOT_FOUND + " Not Found");
	                writer.writeElement("DAV::propstat", XMLWriter.OPENING);
	                writer.writeElement("DAV::prop", XMLWriter.OPENING);
	                while (propertiesNotFoundList.hasNext()) {
	                    writer.writeElement(propertiesNotFoundList .next(), XMLWriter.NO_CONTENT);
	                }
	                writer.writeElement("DAV::prop", XMLWriter.CLOSING);
	                writer.writeElement("DAV::status", XMLWriter.OPENING);
	                writer.writeText(status);
	                writer.writeElement("DAV::status", XMLWriter.CLOSING);
	                writer.writeElement("DAV::propstat", XMLWriter.CLOSING);
	            }	
	            break;

        }
        writer.writeElement("DAV::response", XMLWriter.CLOSING);
    }

    protected void writeSupportedLockElements(
    	RequestContext requestContext,
        XMLWriter writer, 
        String path
    ) {
        writer.writeElement("DAV::supportedlock", XMLWriter.OPENING);
        List<Lock> los = _store.getLocksByPath(requestContext, path);
        if(los.isEmpty()) {
            // both locks (shared/exclusive) can be granted
            writer.writeElement("DAV::lockentry", XMLWriter.OPENING);

            writer.writeElement("DAV::lockscope", XMLWriter.OPENING);
            writer.writeElement("DAV::exclusive", XMLWriter.NO_CONTENT);
            writer.writeElement("DAV::lockscope", XMLWriter.CLOSING);

            writer.writeElement("DAV::locktype", XMLWriter.OPENING);
            writer.writeElement("DAV::write", XMLWriter.NO_CONTENT);
            writer.writeElement("DAV::locktype", XMLWriter.CLOSING);

            writer.writeElement("DAV::lockentry", XMLWriter.CLOSING);

            writer.writeElement("DAV::lockentry", XMLWriter.OPENING);

            writer.writeElement("DAV::lockscope", XMLWriter.OPENING);
            writer.writeElement("DAV::shared", XMLWriter.NO_CONTENT);
            writer.writeElement("DAV::lockscope", XMLWriter.CLOSING);

            writer.writeElement("DAV::locktype", XMLWriter.OPENING);
            writer.writeElement("DAV::write", XMLWriter.NO_CONTENT);
            writer.writeElement("DAV::locktype", XMLWriter.CLOSING);

            writer.writeElement("DAV::lockentry", XMLWriter.CLOSING);

        } 
        else {
        	for(Lock lo: los) {
	            // LockObject exists, checking lock state
	            // if an exclusive lock exists, no further lock is possible
	            if (Lock.SCOPE_SHARED.equals(lo.getScope())) {
	                writer.writeElement("DAV::lockentry", XMLWriter.OPENING);
	                writer.writeElement("DAV::lockscope", XMLWriter.OPENING);
	                writer.writeElement("DAV::shared", XMLWriter.NO_CONTENT);
	                writer.writeElement("DAV::lockscope", XMLWriter.CLOSING);
	                writer.writeElement("DAV::locktype", XMLWriter.OPENING);
	                writer.writeElement("DAV::" + lo.getType(), XMLWriter.NO_CONTENT);
	                writer.writeElement("DAV::locktype", XMLWriter.CLOSING);
	                writer.writeElement("DAV::lockentry", XMLWriter.CLOSING);
	            }
        	}
        }
        writer.writeElement("DAV::supportedlock", XMLWriter.CLOSING);
    }

    protected void writeLockDiscoveryElements(
    	RequestContext requestContext,
        XMLWriter writer, 
        String path,
        int _depth
    ) {
        writer.writeElement("DAV::lockdiscovery", XMLWriter.OPENING);
        List<Lock> locks = _store.getLocksByPath(requestContext, path);
        for(Lock lock: locks) {
	        // Not expired
	        if(lock != null && (System.currentTimeMillis() < lock.getExpiresAt())) {	
	            writer.writeElement("DAV::activelock", XMLWriter.OPENING);

	            writer.writeElement("DAV::locktype", XMLWriter.OPENING);
	            writer.writeProperty("DAV::" + lock.getType());
	            writer.writeElement("DAV::locktype", XMLWriter.CLOSING);
	
	            writer.writeElement("DAV::lockscope", XMLWriter.OPENING);
	            if (Lock.SCOPE_EXCLUSIVE.equals(lock.getScope())) {
	                writer.writeProperty("DAV::exclusive");
	            } else {
	                writer.writeProperty("DAV::shared");
	            }
	            writer.writeElement("DAV::lockscope", XMLWriter.CLOSING);
	
	            writer.writeElement("DAV::depth", XMLWriter.OPENING);
	            if (_depth == INFINITY) {
	                writer.writeText("Infinity");
	            } else {
	                writer.writeText(String.valueOf(_depth));
	            }
	            writer.writeElement("DAV::depth", XMLWriter.CLOSING);
	
                writer.writeElement("DAV::owner", XMLWriter.OPENING);
                writer.writeElement("DAV::href", XMLWriter.OPENING);
                writer.writeText(lock.getOwner());
                writer.writeElement("DAV::href", XMLWriter.CLOSING);
                writer.writeElement("DAV::owner", XMLWriter.CLOSING);
	
	            int timeout = (int) ((lock.getExpiresAt() - System.currentTimeMillis()) / 1000);
	            String timeoutStr = new Integer(timeout).toString();
	            writer.writeElement("DAV::timeout", XMLWriter.OPENING);
	            writer.writeText("Second-" + timeoutStr);
	            writer.writeElement("DAV::timeout", XMLWriter.CLOSING);
	
	            String lockToken = lock.getID();
	
	            writer.writeElement("DAV::locktoken", XMLWriter.OPENING);
	            writer.writeElement("DAV::href", XMLWriter.OPENING);
	            writer.writeText("opaquelocktoken:" + lockToken);
	            writer.writeElement("DAV::href", XMLWriter.CLOSING);
	            writer.writeElement("DAV::locktoken", XMLWriter.CLOSING);
	
	            writer.writeElement("DAV::activelock", XMLWriter.CLOSING);
	        }
        }
        writer.writeElement("DAV::lockdiscovery", XMLWriter.CLOSING);
    }

}
