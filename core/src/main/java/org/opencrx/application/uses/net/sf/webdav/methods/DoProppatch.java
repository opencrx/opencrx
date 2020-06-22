/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoProppatch
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
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;

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

public class DoProppatch extends WebDavMethod {

    private static Logger LOG = Logger.getLogger(DoProppatch.class.getPackage().getName());

    private final WebDavStore _store;

    public DoProppatch(
    	WebDavStore store
    ) {
        _store = store;
    }

    @Override
    public void execute(
    	RequestContext requestContext 
    ) throws IOException, LockFailedException {
        LOG.finest("-- " + this.getClass().getName());
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();    	
        String path = getRelativePath(requestContext);
        String parentPath = getParentPath(getCleanPath(path));
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
        // TODO for now, PROPPATCH just sends a valid response, stating that
        // everything is fine, but doesn't do anything.
        // Retrieve the resources
        Resource so = null;
        try {
            so = _store.getResourceByPath(requestContext, path);
            if (so == null) {
                resp.sendError(HttpServletResponse.SC_NOT_FOUND);
                return;
                // we do not to continue since there is no root
                // resource
            }
            List<String> toset = null;
            List<String> toremove = null;
            List<String> tochange = new Vector<String>();
            // contains all properties from
            // toset and toremove
            path = getCleanPath(getRelativePath(requestContext));
            Node tosetNode = null;
            Node toremoveNode = null;
            if (req.getContentLength() != 0) {
                try {
                    DocumentBuilder documentBuilder = getDocumentBuilder();
                    Document document = documentBuilder.parse(new InputSource(req.getInputStream()));
                    // Get the root element of the document
                    Element rootElement = document.getDocumentElement();
                    tosetNode = XMLHelper.findSubElement(XMLHelper.findSubElement(rootElement, "set"), "prop");
                    toremoveNode = XMLHelper.findSubElement(XMLHelper.findSubElement(rootElement, "remove"), "prop");
                } catch (Exception e) {
                	new ServiceException(e).log();                	
                    resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                    return;
                }
            } else {
                resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                return;
            }
            HashMap<String, String> namespaces = new HashMap<String, String>();
            namespaces.put("DAV:", "D");
            if (tosetNode != null) {
                toset = XMLHelper.getPropertiesFromXML(tosetNode);
                tochange.addAll(toset);
            }
            if (toremoveNode != null) {
                toremove = XMLHelper.getPropertiesFromXML(toremoveNode);
                tochange.addAll(toremove);
            }
            resp.setStatus(WebdavStatus.SC_MULTI_STATUS);
            resp.setContentType("application/xml");
            resp.setCharacterEncoding("UTF-8");
            // Create multistatus object
            XMLWriter writer = new XMLWriter(resp.getWriter(), namespaces);
            writer.writeXMLHeader();
            writer.writeElement("DAV::multistatus", XMLWriter.OPENING);
            writer.writeElement("DAV::response", XMLWriter.OPENING);
            String status = new String("HTTP/1.1 " + HttpServletResponse.SC_OK);
            // Generating href element
            writer.writeElement("DAV::href", XMLWriter.OPENING);
            writer.writeText(resp.encodeURL(this.getHRef(req, req.getServletPath() + "/" + path, so.isCollection())));
            writer.writeElement("DAV::href", XMLWriter.CLOSING);
            for (Iterator<String> iter = tochange.iterator(); iter.hasNext();) {
                String property = iter.next();
                writer.writeElement("DAV::propstat", XMLWriter.OPENING);
                writer.writeElement("DAV::prop", XMLWriter.OPENING);
                writer.writeElement(property, XMLWriter.NO_CONTENT);
                writer.writeElement("DAV::prop", XMLWriter.CLOSING);
                writer.writeElement("DAV::status", XMLWriter.OPENING);
                writer.writeText(status);
                writer.writeElement("DAV::status", XMLWriter.CLOSING);
                writer.writeElement("DAV::propstat", XMLWriter.CLOSING);
            }
            writer.writeElement("DAV::response", XMLWriter.CLOSING);
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
}
