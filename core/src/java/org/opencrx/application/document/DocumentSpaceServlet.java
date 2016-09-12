/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DocumentSpaceServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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
package org.opencrx.application.document;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.StringTokenizer;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.backend.VCard;
import org.opencrx.kernel.document1.cci2.DocumentFolderEntryQuery;
import org.opencrx.kernel.document1.cci2.DocumentFolderQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.DocumentFolderEntry;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * DocumentSpaceServlet. This servlet allows to publish the entries of a 
 * document folder and make them available for web clients. The following entry
 * object types are supported:
 * <ul>
 *   <li>Document: the head revision is published
 *   <li>Activity: the activity's ICS is published
 *   <li>Account: the account's VCF is published
 * </ul>
 * 
 * A separate web application must be published for each space. Below is a sample web.xml:
 * 
 * <?xml version="1.0" encoding="UTF-8"?>
 *	<!DOCTYPE web-app PUBLIC "-//Sun Microsystems, Inc.//DTD Web Application 2.3//EN" "http://java.sun.com/dtd/web-app_2_3.dtd">
 *	<web-app>
 *		<display-name>www.sample.org</display-name>
 *		<servlet>
 *			<servlet-name>DocumentSpaceServlet</servlet-name>
 *			<servlet-class>org.opencrx.application.document.DocumentSpaceServlet</servlet-class>
 *			<init-param>
 *				<param-name>folderPrefix</param-name>
 *				<param-value>CRX/Standard/www.sample.org</param-value>
 *			</init-param>
 *			<init-param>
 *				<param-name>runAs</param-name>
 *				<param-value>admin-Standard</param-value>
 *			</init-param>
 *		</servlet>
 *		<servlet-mapping>
 *			<servlet-name>DocumentSpaceServlet</servlet-name>
 *			<url-pattern>/</url-pattern>
 *		</servlet-mapping>
 *		<resource-ref>
 *			<res-ref-name>jdbc_opencrx_CRX</res-ref-name>
 *			<res-type>javax.sql.DataSource</res-type>
 *			<res-auth>Container</res-auth>
 *		</resource-ref>
 *		<security-constraint>
 *			<web-resource-collection>
 *				<web-resource-name>Public</web-resource-name>
 *				<url-pattern>/*</url-pattern>
 *				<http-method>GET</http-method>
 *			</web-resource-collection>
 *		</security-constraint>
 *	</web-app> 
 *
 */
public class DocumentSpaceServlet extends HttpServlet {

	/* (non-Javadoc)
     * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(
        ServletConfig config            
    ) throws ServletException {
        super.init();        
        if(this.pmf == null) {                    
            try {
                Utils.getModel();
                this.pmf = Utils.getPersistenceManagerFactory();
            } catch (NamingException e) {
                throw new ServletException( 
                    "Can not get the initial context", 
                    e
                );                
            } catch(ServiceException e) {
                throw new ServletException( 
                    "Can not get persistence manager", 
                    e
                );                
            }   
        }
        String folderPrefix = config.getInitParameter("folderPrefix");
        if(folderPrefix != null) {
        	this.folderPrefix = Arrays.asList(folderPrefix.split("/"));
        }
        this.runAs = config.getInitParameter("runAs");        
    }

    /**
     * Get persistence manager for runAs user.
     * 
     * @return
     */
    protected PersistenceManager getPersistenceManager(
    	String segmentName
    ) {
        return this.pmf.getPersistenceManager(
            this.runAs == null
            	? SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName
            	: this.runAs,
            null
        );    	
    }

    /**
     * Send requested file.
     * 
     * @param req
     * @param resp
     * @param resourcePath
     * @throws IOException
     */
    protected boolean sendFile(
    	HttpServletRequest req,
    	HttpServletResponse resp,
    	String resourcePath
    ) throws IOException {
		// Fall-back to file system
    	resp.setContentType(req.getServletContext().getMimeType(resourcePath));
    	InputStream is = req.getServletContext().getResourceAsStream(resourcePath);
    	if(is == null) {
    		return false;
    	} else {
    		resp.setStatus(HttpServletResponse.SC_OK);
	    	OutputStream os = resp.getOutputStream();
	    	BinaryLargeObjects.streamCopy(is, 0L, os);
	    	os.close();
	    	return true;
    	}    	
    }

    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse resp
    ) throws ServletException, IOException {
    	String servletPath = req.getServletPath();
    	PersistenceManager pm = null;
    	HttpSession session = req.getSession(true);
    	try {
   			// Handle WELL_DEFINED_RESOURCES
   			for(String resource: WELL_DEFINED_RESOURCES) {
   				if(servletPath.endsWith(resource)) {
	   				if(!this.sendFile(req, resp, resource)) {
   			    		req.getServletContext().log("file not found " + servletPath);
   			    		resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Not found");
	   				}
	   				return;
   				}
   			}
    		// Assert that the path is of the form {provider.name}/{segment.name}/Spaces
    		// in case no folderPrefix is configured. This way only sub-folders of 'Spaces'
    		// are accessible.
   			getDocument: while(true) {
	    		List<String> path = new ArrayList<String>();
	   			path.addAll(this.folderPrefix);
	   			for(String component: servletPath.split("/")) {
	   				if(!component.isEmpty()) {
	   					path.add(component);
	   				}
	   			}
	    		if(this.folderPrefix.isEmpty() && path.size() >= 3 && !"Spaces".equals(path.get(2))) {
	    			path.add(2, "Spaces");
	    		}
	    		if(path.size() < 3) {
	    			req.getServletContext().log("document not found " + servletPath);
	    			resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Not found");
	    			return;
	    		}
	    		// Forbidden to read .passwd
	    		if(".passwd".equals(path.get(path.size() - 1))) {
	    			resp.sendError(HttpServletResponse.SC_FORBIDDEN);
	    			return;
	    		}
				String providerName = path.get(0);
				String segmentName = path.get(1);
				pm = this.getPersistenceManager(segmentName);
	    		DocumentFolder documentFolder = null;
	    		org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName);
	    		int index = 2;
	    		while(index < path.size() - 1) {
	    			String component = path.get(index);
	        		DocumentFolderQuery documentFolderQuery = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
	        		documentFolderQuery.name().equalTo(component);
	        		documentFolderQuery.forAllDisabled().isFalse();
	        		if(documentFolder != null) {
	        			documentFolderQuery.thereExistsParent().equalTo(documentFolder);
	        		}
	        		List<DocumentFolder> documentFolders = documentSegment.getFolder(documentFolderQuery);
	        		if(documentFolders.isEmpty()) {
	        			break;
	        		} else {
	        			documentFolder = documentFolders.iterator().next();
	        			// Test for .passwd
	        			if(!Boolean.TRUE.equals(session.getAttribute(documentFolder.refGetPath().toXRI() + ".auth"))) {
			        		DocumentFolderEntryQuery entryQuery = (DocumentFolderEntryQuery)pm.newQuery(DocumentFolderEntry.class);        	
			        		entryQuery.name().equalTo(".passwd");
			        		List<DocumentFolderEntry> entries = documentFolder.getFolderEntry(entryQuery);
			        		if(!entries.isEmpty()) {
			        			DocumentFolderEntry passwdEntry = entries.iterator().next();
			        			if(passwdEntry.getDocument() instanceof Document) {
			        				Document passwdDocument = (Document)passwdEntry.getDocument();
			        				if(passwdDocument.getHeadRevision() instanceof MediaContent) {
			        					MediaContent passwdContent = (MediaContent)passwdDocument.getHeadRevision();
			        					ByteArrayOutputStream os = new ByteArrayOutputStream();
			        					BinaryLargeObjects.streamCopy(passwdContent.getContent().getContent(), 0L, os);
			        					String passwd = os.toString("UTF-8");
			        					String authHeader = req.getHeader("Authorization");
			        				    if(authHeader != null) {
			        				        StringTokenizer st = new StringTokenizer(authHeader);
			        				        if(st.hasMoreTokens()) {
			        				            String authType = st.nextToken();
			        				            if("Basic".equals(authType)) {
		        				                    String credentials = new String(Base64.decode(st.nextToken()), "UTF-8");
		        				                    int p = credentials.indexOf(":");
		        				                    if(p != -1) {
		        				                        String user = credentials.substring(0, p).trim();
		        				                        String password = credentials.substring(p + 1).trim();
		        				                        if(passwd.indexOf(user + ":" + password) < 0) {
		        				                        	resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Forbidden");
		        				                        	return;
		        				                        } else {
		    				        				        session.setAttribute(documentFolder.refGetPath().toXRI() + ".auth", Boolean.TRUE);			        				                        	
		        				                        }
		        				                    }
			        				            }
			        				        }
			        				    } else {
			        				    	resp.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
			        				    	resp.setHeader("WWW-Authenticate", "basic realm=\"" + path.subList(0, index + 1) + "\"" );
			        				    	return;
			        				    }
			        				}
			        			}
			        		}
		        		}
	        		}
	        		index++;
	    		}
	    		DocumentFolderEntry entry = null;
	    		if(documentFolder != null) {    			
	        		DocumentFolderEntryQuery entryQuery = (DocumentFolderEntryQuery)pm.newQuery(DocumentFolderEntry.class);        	
	        		entryQuery.name().equalTo(path.get(index));
	        		entryQuery.forAllDisabled().isFalse();
	        		List<DocumentFolderEntry> entries = documentFolder.getFolderEntry(entryQuery);
	        		entry = entries.isEmpty() ? null : entries.iterator().next();
	    		}
	    		if(entry != null && entry.getDocument() != null) {
	    			Object object = entry.getDocument();
	    			if(object instanceof Document) {
	    				Document document = (Document)object;
		    			MediaContent mediaContent = (MediaContent)document.getHeadRevision();
		    	    	resp.setContentType(mediaContent.getContentMimeType());
		    	    	InputStream is = mediaContent.getContent().getContent();
			    		resp.setStatus(HttpServletResponse.SC_OK);
				    	OutputStream os = resp.getOutputStream();
				    	BinaryLargeObjects.streamCopy(is, 0L, os);
				    	os.close();
	    			} else if(object instanceof Account) {
	    				Account account = (Account)object;
	    				resp.setContentType(VCard.MIME_TYPE);
			    		resp.setStatus(HttpServletResponse.SC_OK);
	    				PrintWriter pw = resp.getWriter();
	    				pw.print(account.getVcard());
	    				pw.close();
	    			} else if(object instanceof Activity) {
	    				Activity activity = (Activity)object;
	    				resp.setContentType(ICalendar.MIME_TYPE);
			    		resp.setStatus(HttpServletResponse.SC_OK);
	    				PrintWriter pw = resp.getWriter();
	    				pw.print(activity.getIcal());
	    				pw.close();
	    			} else {
	    	    		resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	    	    		SysLog.warning("Unsupported entry type", entry);
	    			}
	    		} else {
	    			if(!this.sendFile(req, resp, servletPath)) {
	    	    		if(servletPath.endsWith("/index.html") || !servletPath.endsWith("/")) {
	    		    		req.getServletContext().log("file not found " + servletPath);
	    		    		resp.sendError(HttpServletResponse.SC_NOT_FOUND, "Not found");
	    	    		} else {
	    	    			servletPath += "index.html";
	    	    			continue getDocument;
	    	    		}
	    			}
	    		}
	    		break;
   			}
    	} catch(Exception e) {
    		resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
    		new ServiceException(e).log();
    	} finally {
    		if(pm != null) {
    			try {
    				pm.close();
    			} catch(Exception ignore) {}    				
    		}
    	}
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	private static final long serialVersionUID = -1420175264506058947L;

	protected static List<String> WELL_DEFINED_RESOURCES = Arrays.asList(
		"/js/jquery.min.js",
		"/js/bootstrap.min.js",
		"/js/jquery.blueimp-gallery.min.js",
		"/css/bootstrap.min.css",
		"/css/blueimp-gallery.min.css"
	);
    protected List<String> folderPrefix = Collections.emptyList();
    protected String runAs = null;
    protected PersistenceManagerFactory pmf = null;
    
}
