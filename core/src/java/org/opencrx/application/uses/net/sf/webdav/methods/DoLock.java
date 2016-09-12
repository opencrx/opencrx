/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoLock
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
import java.util.HashMap;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;

import org.opencrx.application.uses.net.sf.webdav.Lock;
import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.WebdavStatus;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLWriter;
import org.openmdx.base.exception.ServiceException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * DoLock
 *
 */
public class DoLock extends WebDavMethod {

    private static Logger LOG = Logger.getLogger(DoLock.class.getPackage().getName());

    private final WebDavStore _store;

    static class LockInformation {
    	public String scope;
    	public String type;
    	public String owner;
    }
    
    /**
     * Constructor.
     * 
     * @param store
     */
    public DoLock(
    	WebDavStore store
    ) {
        _store = store;
    }

    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
        LOG.finest("-- " + this.getClass().getName());
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
        String path = this.getRelativePath(requestContext);
        String parentPath = getParentPath(getCleanPath(path));
        Hashtable<String, Integer> errorList = new Hashtable<String, Integer>();
        if(!checkLocks(requestContext, _store, path)) {
            errorList.put(path, WebdavStatus.SC_LOCKED);
            this.sendReport(requestContext, errorList);
            return; // resource is locked
        }
        if(!checkLocks(requestContext, _store, parentPath)) {
            errorList.put(parentPath, WebdavStatus.SC_LOCKED);
            this.sendReport(requestContext, errorList);
            return; // parent is locked
        }
        try {
            this.executeLock(requestContext, path);
        } catch (LockFailedException e) {
            resp.sendError(WebdavStatus.SC_LOCKED);
            new ServiceException(e).log();
        } catch (Exception e) {
            resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            new ServiceException(e).log();
        } finally {
        }
    }

    /**
     * Executes the LOCK.
     * 
     * @param requestContext
     * @param res
     * @param path
     * @throws IOException
     * @throws LockFailedException
     */
    private void executeLock(
    	RequestContext requestContext,
        String path
    ) throws IOException, LockFailedException {
    	LockInformation lockInfo = this.getLockInformation(requestContext);
        int depth = this.getDepth(requestContext);
        int lockDuration = this.getTimeout(requestContext);
        Lock lock = _store.lock(
        	requestContext,
            path,
            requestContext.getHttpServletRequest().getHeader("If"),
            lockInfo == null ? null : lockInfo.owner,
            lockInfo == null ? null : lockInfo.scope, 
            lockInfo == null ? null : lockInfo.type,
            depth, 
            lockDuration
        );
        if(lock != null) {
            this.generateReport(requestContext, lock);
        } else {
            this.sendLockFailError(requestContext, path);
            throw new LockFailedException();
        }
    }

    /**
     * Get lock info from request. Is null on lock refresh.
     * 
     * @param requestContext
     * @return
     */
    private LockInformation getLockInformation(
    	RequestContext requestContext
    ) {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();    	
    	LockInformation lockInfo = new LockInformation();
        Node lockInfoNode = null;
        DocumentBuilder documentBuilder = null;
        try {
        	documentBuilder = this.getDocumentBuilder();
        	Document document = req.getContentLength() <= 0
        		? null
        		: documentBuilder.parse(req.getInputStream());
            Element rootElement = document == null 
            	? null 
            	: document.getDocumentElement();
            lockInfoNode = rootElement;
            // lockInfo is null on lock refresh
            if(lockInfoNode != null) {
                NodeList childList = lockInfoNode.getChildNodes();
                Node lockScopeNode = null;
                Node lockTypeNode = null;
                Node lockOwnerNode = null;
                Node currentNode = null;
                String nodeName = null;
                for (int i = 0; i < childList.getLength(); i++) {
                    currentNode = childList.item(i);
                    if (currentNode.getNodeType() == Node.ELEMENT_NODE || currentNode.getNodeType() == Node.TEXT_NODE) {
                        nodeName = currentNode.getNodeName();
                        if (nodeName.endsWith("locktype")) {
                            lockTypeNode = currentNode;
                        }
                        if (nodeName.endsWith("lockscope")) {
                            lockScopeNode = currentNode;
                        }
                        if (nodeName.endsWith("owner")) {
                            lockOwnerNode = currentNode;
                        }
                    } else {
                        return null;
                    }
                }
                if(lockScopeNode != null) {
                    String scope = null;
                    childList = lockScopeNode.getChildNodes();
                    for (int i = 0; i < childList.getLength(); i++) {
                        currentNode = childList.item(i);

                        if (currentNode.getNodeType() == Node.ELEMENT_NODE) {
                            scope = currentNode.getNodeName();
                            if (scope.endsWith("exclusive")) {
                            	lockInfo.scope = "exclusive";
                            } else if (scope.equals("shared")) {
                            	lockInfo.scope = "shared";
                            }
                        }
                    }
                    if (scope == null) {
                        return null;
                    }

                } else {
                    return null;
                }
                if (lockTypeNode != null) {
                    childList = lockTypeNode.getChildNodes();
                    for (int i = 0; i < childList.getLength(); i++) {
                        currentNode = childList.item(i);
                        if (currentNode.getNodeType() == Node.ELEMENT_NODE) {
                            String _type = currentNode.getNodeName();
                            if (_type.endsWith("write")) {
                                lockInfo.type = Lock.TYPE_WRITE;
                            } else if (_type.equals("read")) {
                                lockInfo.type = Lock.TYPE_READ;
                            }
                        }
                    }
                    if(lockInfo.type == null) {
                        return null;
                    }
                } else {
                    return null;
                }
                if (lockOwnerNode != null) {
                    childList = lockOwnerNode.getChildNodes();
                    for (int i = 0; i < childList.getLength(); i++) {
                        currentNode = childList.item(i);
                        if (currentNode.getNodeType() == Node.ELEMENT_NODE
                             || currentNode.getNodeType() == Node.TEXT_NODE ) {
                            lockInfo.owner = currentNode.getTextContent();
                        }
                    }
                }
                if(lockInfo.owner == null) {
                    return null;
                }
            } else {
                return null;
            }
        } catch (Exception e) {
        	try {
        		resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        	} catch(Exception e0) {}
            new ServiceException(e).log();
            return null;
        }
        return lockInfo;
    }

    /**
     * Ties to read the timeout from request
     */
    private int getTimeout(
    	RequestContext requestContext
    ) {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
        int lockDuration = DEFAULT_TIMEOUT;
        String lockDurationStr = req.getHeader("Timeout");

        if (lockDurationStr == null) {
            lockDuration = DEFAULT_TIMEOUT;
        } else {
            int commaPos = lockDurationStr.indexOf(',');
            // if multiple timeouts, just use the first one
            if (commaPos != -1) {
                lockDurationStr = lockDurationStr.substring(0, commaPos);
            }
            if (lockDurationStr.startsWith("Second-")) {
                lockDuration = new Integer(lockDurationStr.substring(7)).intValue();
            } else {
                if (lockDurationStr.equalsIgnoreCase("infinity")) {
                    lockDuration = MAX_TIMEOUT;
                } else {
                    try {
                        lockDuration = new Integer(lockDurationStr).intValue();
                    } catch (NumberFormatException e) {
                        lockDuration = MAX_TIMEOUT;
                    }
                }
            }
            if (lockDuration <= 0) {
                lockDuration = DEFAULT_TIMEOUT;
            }
            if (lockDuration > MAX_TIMEOUT) {
                lockDuration = MAX_TIMEOUT;
            }
        }
        return lockDuration;
    }

    /**
     * Generates the response XML with all lock information
     */
    private void generateReport(
    	RequestContext requestContext,
        Lock lo
    ) {
    	try {
	    	HttpServletResponse resp = requestContext.getHttpServletResponse();    	
	        HashMap<String, String> namespaces = new HashMap<String, String>();
	        namespaces.put("DAV:", "D");
	
	        resp.setStatus(HttpServletResponse.SC_OK);
	        resp.setContentType("application/xml");
	        resp.setCharacterEncoding("UTF-8");
	
	        XMLWriter generatedXML = new XMLWriter(resp.getWriter(), namespaces);
	        generatedXML.writeXMLHeader();
	        generatedXML.writeElement("DAV::prop", XMLWriter.OPENING);
	        generatedXML.writeElement("DAV::lockdiscovery", XMLWriter.OPENING);
	        generatedXML.writeElement("DAV::activelock", XMLWriter.OPENING);
	
	        generatedXML.writeElement("DAV::locktype", XMLWriter.OPENING);
	        generatedXML.writeProperty("DAV::" + lo.getType());
	        generatedXML.writeElement("DAV::locktype", XMLWriter.CLOSING);
	
	        generatedXML.writeElement("DAV::lockscope", XMLWriter.OPENING);
	        if (Lock.SCOPE_EXCLUSIVE.equals(lo.getScope())) {
	            generatedXML.writeProperty("DAV::exclusive");
	        } else {
	            generatedXML.writeProperty("DAV::shared");
	        }
	        generatedXML.writeElement("DAV::lockscope", XMLWriter.CLOSING);
	
	        int depth = lo.getLockDepth();
	        generatedXML.writeElement("DAV::depth", XMLWriter.OPENING);
	        if (depth == INFINITY) {
	            generatedXML.writeText("Infinity");
	        } else {
	            generatedXML.writeText(String.valueOf(depth));
	        }
	        generatedXML.writeElement("DAV::depth", XMLWriter.CLOSING);
	
	        generatedXML.writeElement("DAV::owner", XMLWriter.OPENING);
	        generatedXML.writeElement("DAV::href", XMLWriter.OPENING);
	        generatedXML.writeText(lo.getOwner());
	        generatedXML.writeElement("DAV::href", XMLWriter.CLOSING);
	        generatedXML.writeElement("DAV::owner", XMLWriter.CLOSING);
	
	        long timeout = lo.getExpiresAt() - System.currentTimeMillis();
	        generatedXML.writeElement("DAV::timeout", XMLWriter.OPENING);
	        generatedXML.writeText("Second-" + timeout / 1000);
	        generatedXML.writeElement("DAV::timeout", XMLWriter.CLOSING);
	
	        String lockToken = lo.getID();
	        generatedXML.writeElement("DAV::locktoken", XMLWriter.OPENING);
	        generatedXML.writeElement("DAV::href", XMLWriter.OPENING);
	        generatedXML.writeText("opaquelocktoken:" + lockToken);
	        generatedXML.writeElement("DAV::href", XMLWriter.CLOSING);
	        generatedXML.writeElement("DAV::locktoken", XMLWriter.CLOSING);
	
	        generatedXML.writeElement("DAV::activelock", XMLWriter.CLOSING);
	        generatedXML.writeElement("DAV::lockdiscovery", XMLWriter.CLOSING);
	        generatedXML.writeElement("DAV::prop", XMLWriter.CLOSING);
	
	        resp.addHeader("Lock-Token", "<opaquelocktoken:" + lockToken + ">");
	
	        generatedXML.sendData();
    	}
    	catch(Exception e) {
    		new ServiceException(e).log();
    	}
    }

    /**
     * Sends an error report to the client
     */
    private void sendLockFailError(
    	RequestContext requestContext,
        String path
    ) throws IOException {
        Hashtable<String, Integer> errorList = new Hashtable<String, Integer>();
        errorList.put(path, WebdavStatus.SC_LOCKED);
        sendReport(requestContext, errorList);
    }

}
