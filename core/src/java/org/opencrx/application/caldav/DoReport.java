/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoReport
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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
package org.opencrx.application.caldav;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.exceptions.LockFailedException;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLHelper;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.format.DateTimeFormat;
import org.xml.sax.InputSource;

/**
 * DoReport
 *
 */
public class DoReport extends org.opencrx.application.uses.net.sf.webdav.methods.DoReport {

    /**
     * Constructor.
     * 
     * @param store
     */
    public DoReport(
    	WebDavStore store 
    ) {
        super(
        	store 
        );
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.methods.DoHead#folderBody(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.Resource)
	 */
	@Override
    public void folderBody(
    	RequestContext requestContext, 
        Resource so
    ) throws IOException, LockFailedException {
		HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
    	Document document = null;
		try {
	    	DocumentBuilder documentBuilder = getDocumentBuilder();
	        document = documentBuilder.parse(new InputSource(req.getInputStream()));
	    } catch (Exception e) {
	    	new ServiceException(e).log();                	
	        resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	        return;
	    }
        Element rootElement = document.getDocumentElement();
        // Filter: time-range
        Date timeRangeStart = null;
        Date timeRangeEnd = null;
        NodeList timeRangeNodes = rootElement.getElementsByTagNameNS("urn:ietf:params:xml:ns:caldav", "time-range");
        if(timeRangeNodes != null) {
	        for(int i = 0; i < timeRangeNodes.getLength(); i++) {
	        	Node timeRangeNode = timeRangeNodes.item(i);
	        	Node start = timeRangeNodes.item(i).getAttributes().getNamedItem("start");
	        	if(start != null) {
	        		try {
	        			timeRangeStart = DateTimeFormat.BASIC_UTC_FORMAT.parse(start.getNodeValue());
	        		} catch(Exception ignore) {}
	        	}
	        	Node end = timeRangeNode.getAttributes().getNamedItem("end");
	        	if(end != null) {
	        		try {
	        			timeRangeEnd = DateTimeFormat.BASIC_UTC_FORMAT.parse(end.getNodeValue());
	        		} catch(Exception ignore) {}        		
	        	}
	        }
        }
        // href
    	Collection<Resource> resources = Collections.emptyList();
        List<Node> hrefNodes = XMLHelper.findSubElements(rootElement, "href");
        if(hrefNodes != null) {
        	resources = new ArrayList<Resource>();
    		for(Node hrefNode: hrefNodes) {
    			String href = hrefNode.getTextContent();
    			if(href != null) {
	    			if(href.startsWith(req.getContextPath())) {
	    				href = href.substring(req.getContextPath().length());
	    			}
	    			Resource res = _store.getResourceByPath(requestContext, href);
	    			if(res == null) {
	    				res =  _store.getResourceByPath(requestContext, URLDecoder.decode(href, "UTF-8"));
	    			}	    			
	    			if(res != null) {
	    				resources.add(res);
	    			}
    			}
    		}
    	} else if(so instanceof ActivityCollectionResource) {
        	// Query
    		resources = _store.getChildren(
    			requestContext, 
    			so, 
    			timeRangeStart, 
    			timeRangeEnd
    		);
       	}
        // Properties
        Node propNode = XMLHelper.findSubElement(rootElement, "prop");
        List<String> properties = XMLHelper.getPropertiesFromXML(propNode);
        // SyncToken
        Node syncTokenNode = XMLHelper.findSubElement(rootElement, "sync-token");
        // sync-token not supported
        if(syncTokenNode != null) {
        	resp.setStatus(HttpServletResponse.SC_NOT_IMPLEMENTED);
        } else {
	        resp.setStatus(SC_MULTI_STATUS);
	        resp.setCharacterEncoding("UTF-8");
	        resp.setContentType("application/xml");
	        resp.setHeader("DAV", "1, 2, calendar-access");
	    	PrintWriter p = resp.getWriter();
	    	p.println("<?xml version=\"1.0\" encoding=\"utf-8\" ?>");
	    	SysLog.detail("<D:multistatus>");
	    	p.println("<D:multistatus xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\">");
	        for(Resource resource: resources) {
	        	ActivityResource res = (ActivityResource)resource;
	        	if(
	        		(res.getSyncFeedResource().getRunAs() == null) || 
	        		res.getObject().getIcal().indexOf("CLASS:PRIVATE") < 0
	        	) {
	            	String name = resource.getName();
		        	p.println("  <D:response>");
		        	p.println("    <D:href>" + this.encodeURL(resp, this.getHRef(req, req.getServletPath() + name, false)) + "</D:href>");
		        	p.println("    <D:propstat>");
		        	p.println("      <D:prop>");
		        	for(String property: properties) {
		        		if(property.indexOf("getetag") > 0) {
		        			p.println("        <D:getetag>" + this.getETag(res) + "</D:getetag>");
		        		} else if(property.indexOf("calendar-data") > 0) {
				        	p.print  ("        <C:calendar-data xmlns:C=\"urn:ietf:params:xml:ns:caldav\">");
				        	p.print("<![CDATA[");
				        	BinaryLargeObject content = _store.getResourceContent(requestContext, resource).getContent();
				        	ByteArrayOutputStream bos = new ByteArrayOutputStream();
				        	BinaryLargeObjects.streamCopy(content.getContent(), 0L, bos);
				        	bos.close();
				        	p.print(bos.toString("UTF-8"));
				        	p.print("]]>");
			                p.println("</C:calendar-data>");
		        		} else if(property.indexOf("getcontenttype") > 0) {
		        			p.println("        <D:getcontenttype>" + _store.getMimeType(res) + "</D:getcontenttype>");
		        		}
		        	}
		        	p.println("      </D:prop>");
		        	p.println("      <D:status>HTTP/1.1 200 OK</D:status>");
		        	p.println("    </D:propstat>");
		        	p.println("  </D:response>");
	        	}
	        }
	    	p.println("</D:multistatus>");
	    	SysLog.detail("</D:multistatus>");
	    	p.flush();
        }
    }

    protected static final int SC_MULTI_STATUS = 207;

}
