/*
 * ====================================================================
 * Project:     openCRX/core, http://www.opencrx.org/
 * Description: DoGet
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
import java.io.InputStream;
import java.io.OutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Locale;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.HTMLWriter;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * DoGet
 *
 */
public class DoGet extends DoHead {

    private static Logger LOG = Logger.getLogger(DoGet.class.getPackage().getName());

    /**
     * Constructor.
     * 
     * @param store
     */
    public DoGet(
    	WebDavStore store 
    ) {
        super(
        	store 
        );
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.uses.net.sf.webdav.methods.DoHead#doBody(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.Resource)
     */
    @Override
    protected void doBody(
    	RequestContext requestContext, 
        Resource res
    ) {
    	HttpServletResponse resp = requestContext.getHttpServletResponse();
        try {
            OutputStream out = resp.getOutputStream();
            WebDavStore.ResourceContent resourceContent = _store.getResourceContent(requestContext, res);
            InputStream in = resourceContent.getContent().getContent();
            String mimeType = _store.getMimeType(res);
            if(mimeType != null) {
	            resp.setContentType(mimeType);
	            if(mimeType.startsWith("text/")) {
	                resp.setCharacterEncoding("UTF-8");            	
	            }
            }
            String range = requestContext.getHttpServletRequest().getHeader("Range");
            // No Range: option
            if(range == null) {
                resp.setStatus(HttpServletResponse.SC_OK);
	            long contentLength = BinaryLargeObjects.streamCopy(in, 0L, out);
	            resp.setContentLength((int)contentLength);
            } else {
            	// Range:
            	if(range.startsWith("bytes=")) {
            		range = range.substring(6);
                	Long rangeFrom = null;
                	Long rangeTo = null;
            		String[] rangeParts = range.split("-");
            		if(rangeParts.length > 0) {
            			try {
            				rangeFrom = Long.valueOf(rangeParts[0]);
            			} catch(Exception ignore) {}
            		}
            		if(rangeParts.length > 1) {
            			try {
            				rangeTo = Long.valueOf(rangeParts[1]);
            			} catch(Exception ignore) {}
            		}
            		if(rangeFrom != null) {
            			// Prevent chunking!
            			Long contentLength = resourceContent.getLength();
                        resp.setBufferSize((int)(contentLength - rangeFrom));
            			long skipped = in.skip(rangeFrom);
            			if(skipped == rangeFrom) {
	            			long pos = rangeFrom;
	            			long rangeLength = 0;
	            			int b;
	            			while((b = in.read()) != -1) {
            					out.write(b);
	            				pos++;
	            				rangeLength++;
	            				if(rangeTo != null && pos > rangeTo) {
	            					break;
	            				}
	            			}
	                        resp.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
	                        resp.setHeader("Content-Range", "bytes " + rangeFrom + "-" + (pos - 1) + "/" + contentLength);
	                        resp.setContentLength((int)rangeLength);
            			} else {
                            resp.setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);            			            				
            			}
            		} else {
                        resp.setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);            			
            		}
            	} else {
                    resp.setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);            		
            	}
            }
            out.flush();
        } catch (Exception e) {
            LOG.finest(e.toString());
        }
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.uses.net.sf.webdav.methods.DoHead#folderBody(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.Resource)
     */
    @Override
    protected void folderBody(
    	RequestContext requestContext, 
    	Resource so
    ) throws IOException {
    	HttpServletRequest req = requestContext.getHttpServletRequest();
    	HttpServletResponse resp = requestContext.getHttpServletResponse();    	
        if (so == null) {
            resp.sendError(HttpServletResponse.SC_NOT_FOUND, req.getRequestURI());
        } else {
            resp.setStatus(HttpServletResponse.SC_OK);
            if (so.isCollection()) {
                DateFormat shortDF = getDateTimeFormat(req.getLocale());
                resp.setContentType("text/html");
                resp.setCharacterEncoding("UTF-8");
                Collection<Resource> children = _store.getChildren(requestContext, so, null, null);
                HTMLWriter writer = new HTMLWriter(resp.getWriter());
                writer.writeText("<html><head><title>Content of folder");
                writer.writeText(so.getName());
                writer.writeText("</title><style type=\"text/css\">");
                this.writeCSS(writer);                
                writer.writeText("</style></head>");
                writer.writeText("<body>");
                this.writeHeader(writer, requestContext, so.getName());
                writer.writeText("<table>");
                writer.writeText("<tr><th>Name</th><th>Size</th><th>Created</th><th>Modified</th></tr>");
                writer.writeText("<tr>");
                writer.writeText("<td colspan=\"4\"><a href=\"../\">Parent</a></td></tr>");
                boolean isEven= false;
                for (Resource child: children) {
                    isEven= !isEven;
                    writer.writeText("<tr class=\"");
                    writer.writeText(isEven ? "even" : "odd");
                    writer.writeText("\">");
                    writer.writeText("<td>");
                    writer.writeText("<a href=\"");
                    writer.writeText(child.getName());
                    if (child.isCollection()) {
                    	writer.writeText("/");
                    }
                    writer.writeText("\">");
                    writer.writeText(child.getName());
                    writer.writeText("</a></td>");
                    if (child.isCollection()) {
                    	writer.writeText("<td>Folder</td>");
                    } else {
                    	writer.writeText("<td>");
                    	WebDavStore.ResourceContent resourceContent = _store.getResourceContent(requestContext, child);
                    	writer.writeText(Long.toString(resourceContent.getLength()));
                    	writer.writeText(" Bytes</td>");
                    }
                    if (child.getCreationDate() != null) {
                    	writer.writeText("<td>");
                    	writer.writeText(shortDF.format(child.getCreationDate()));
                    	writer.writeText("</td>");
                    } else {
                    	writer.writeText("<td></td>");
                    }
                    if(child.getLastModified() != null) {
                    	writer.writeText("<td>");
                    	writer.writeText(shortDF.format(child.getLastModified()));
                    	writer.writeText("</td>");
                    } else {
                    	writer.writeText("<td></td>");
                    }
                    writer.writeText("</tr>");
                }
                writer.writeText("</table>");
                this.writeFooter(writer, requestContext, so.getName());
                writer.writeText("</body></html>");
                writer.sendData();
            }
        }
    }

    /**
     * Write CSS.
     * 
     * @param writer
     */
    protected void writeCSS(
    	HTMLWriter writer
	) {
    	writer.writeText(
			"body {\n"+
			"	font-family: 'Open Sans', 'DejaVu Sans Condensed', 'lucida sans', tahoma, verdana, arial, sans-serif;\n"+
			"}\n"+
			"h1 {\n"+
			"	font-size: 1.5em;\n"+
			"}\n"+
			"th {\n"+
			"	background-color: #9DACBF;\n"+
			"}\n"+
			"table {\n"+
			"	border-top-style: solid;\n"+
			"	border-right-style: solid;\n"+
			"	border-bottom-style: solid;\n"+
			"	border-left-style: solid;\n"+
			"}\n"+
			"td {\n"+
			"	margin: 0px;\n"+
			"	padding-top: 2px;\n"+
			"	padding-right: 5px;\n"+
			"	padding-bottom: 2px;\n"+
			"	padding-left: 5px;\n"+
			"}\n"+
			"tr.even {\n"+
			"	background-color: #CCCCCC;\n"+
			"}\n"+
			"tr.odd {\n"+
			"	background-color: #FFFFFF;\n"+
			"}"
		);
    }
    
    /**
     * Return this as the Date/Time format for displaying Creation + Modification dates
     * 
     * @param browserLocale
     * @return DateFormat used to display creation and modification dates
     */
    protected DateFormat getDateTimeFormat(
    	Locale browserLocale
    ) {
        return SimpleDateFormat.getDateTimeInstance(SimpleDateFormat.SHORT, SimpleDateFormat.MEDIUM, browserLocale);
    }

    /**
     * Return the header to be displayed in front of the folder content
     * 
     * @param requestContext
     * @param path
     * @return String representing the header to be display in front of the folder content
     */
    protected void writeHeader(
    	HTMLWriter writer,
    	RequestContext requestContext, 
    	String name
    ) {
        writer.writeText("<h1>Content of folder " + name + "</h1>");
    }

    /**
     * Return the footer to be displayed after the folder content
     * 
     * @param requestContext
     * @param name
     * @return String representing the footer to be displayed after the folder content
     */
    protected void writeFooter(
    	HTMLWriter writer,
    	RequestContext requestContext, 
    	String name
    ) {
        writer.writeText("");
    }
    
}
