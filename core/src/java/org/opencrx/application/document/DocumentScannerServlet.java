/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DocumentScannerServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.concurrent.ConcurrentHashMap;

import javax.activation.MimetypesFileTypeMap;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.base.jmi1.SendAlertParams;
import org.opencrx.kernel.document1.cci2.DocumentFolderQuery;
import org.opencrx.kernel.document1.cci2.DocumentQuery;
import org.opencrx.kernel.document1.cci2.FolderAssignmentQuery;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.FolderAssignment;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.document1.jmi1.ResourceIdentifier;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * The DocumentScannerServlet scans a directory and its sub-directories. File names
 * are of the form <code>name { "#" folder name } "." extension</code>. Files with
 * the same name are mapped to the same Document. Documents are assigned to the
 * document folders specified by the # separated folder list. Successfully imported
 * files are removed so the import directory should be used as import directory only 
 * and not as document archive.  
 */  
public class DocumentScannerServlet 
    extends HttpServlet {

    //-----------------------------------------------------------------------
    public void init(
        ServletConfig config
    ) throws ServletException {
        super.init(config);        
        try {
            this.pmf = Utils.getPersistenceManagerFactory();
        }
        catch (Exception e) {
            throw new ServletException("Can not get connection to persistence manager", e);
        }        
    }

    //-----------------------------------------------------------------------
    private org.opencrx.kernel.base.jmi1.StringProperty getComponentConfigProperty(
        PersistenceManager rootPm,
        String providerName,
        String segmentName,
        String name
    ) {
        org.opencrx.kernel.admin1.jmi1.ComponentConfiguration componentConfiguration = 
            ComponentConfigHelper.getComponentConfiguration(
                COMPONENT_CONFIGURATION_ID,
                providerName,
                rootPm,
                true,
                new String[][]{
                    new String[]{providerName + ".Standard." + OPTION_SCAN_DIR, ""},
                    new String[]{providerName + ".Standard." + OPTION_URL_PREFIX, ""},                    
                    new String[]{providerName + ".Standard." + OPTION_UPLOAD, ""},                    
                    new String[]{providerName + ".Standard." + OPTION_GROUPS, ""}                    
                }
            );            
        if(componentConfiguration != null) {
            return ComponentConfigHelper.getComponentConfigProperty(
                providerName + "." + segmentName + "." + name, 
                componentConfiguration
            );
        }
        return null;
    }
        
    //-----------------------------------------------------------------------    
    private void sendAlert(
    	org.opencrx.kernel.document1.jmi1.Segment documentSegment,
    	String filename,
    	String message,
    	PersistenceManager pm
    ) {
    	String providerName = documentSegment.refGetPath().get(2);
    	String segmentName = documentSegment.refGetPath().get(4);
    	UserHome userHomeAdmin = (UserHome)pm.getObjectById(
    		new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName, "segment", segmentName, "userHome", SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName)
    	);
    	SendAlertParams sendAlertParams = Structures.create(
    		SendAlertParams.class, 
    		Datatypes.member(SendAlertParams.Member.description, DocumentScannerServlet.class.getSimpleName() + ": error importing document " + filename + "\n" + "Reason is:\n" + message),
    		Datatypes.member(SendAlertParams.Member.importance, (short)2),    		
    		Datatypes.member(SendAlertParams.Member.name, DocumentScannerServlet.class.getSimpleName() + ": error importing document " + filename),    		
    		Datatypes.member(SendAlertParams.Member.resendDelayInSeconds, 60),    		
    		Datatypes.member(SendAlertParams.Member.toUsers, SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName),	
    		Datatypes.member(SendAlertParams.Member.reference, null)	
    	);
    	try {
    		pm.currentTransaction().begin();
    		userHomeAdmin.sendAlert(sendAlertParams);
    		pm.currentTransaction().commit();
    	}
    	catch(Exception e) {
    		try {
    			pm.currentTransaction().rollback();
    		} catch(Exception e0) {}
    	}    	
    }
    
    //-----------------------------------------------------------------------    
    private void scanDocuments(
        File currentDir,
        File rootDir,
        org.opencrx.kernel.document1.jmi1.Segment documentSegment,
        String uriPrefix,
        Boolean upload,
        List<org.opencrx.security.realm1.jmi1.PrincipalGroup> principalGroups,
        PersistenceManager pm
    ) {
        File[] files = currentDir.listFiles();
        if(files != null) {
            for(File file: files) {
                if(file.isDirectory()) {
                    this.scanDocuments(
                        file,
                        rootDir,
                        documentSegment,
                        uriPrefix,
                        upload,
                        principalGroups,
                        pm
                    );
                }
                else {
                	boolean hasErrors = false;
                    String filename = file.getName();
                    String ext = null;
                    if(filename.indexOf(".") > 0) {
                        int pos = filename.lastIndexOf(".");
                        ext = filename.substring(pos + 1);
                        filename = filename.substring(0, pos);
                    }
                    String[] names = filename.split("#");
                    boolean hasVersion = 
                        (names.length > 0) && 
                        (names[names.length-1].length() > 1) &&
                        (names[names.length-1].charAt(0) == 'v') &&
                        Character.isDigit(names[names.length-1].charAt(1));
                    String version = hasVersion ? 
                        names[names.length-1].substring(1) : 
                        "1.0";  
                    Date activeOn = null;
                    for(String name: names) {
                    	if(name.startsWith("@")) {
                    		try {
                    			activeOn = DateTimeFormat.BASIC_UTC_FORMAT.parse(
                    				name.indexOf("T") > 0 ?
                    					name.substring(1) :
                    					name.substring(1) + "T000000.000Z"
                    			);
                    		}
                    		catch(Exception e) {}
                    	}
                    }
                    // Get/create document
                    DocumentQuery query = (DocumentQuery)pm.newQuery(org.opencrx.kernel.document1.jmi1.Document.class);
                    String documentName = names[0] + (ext == null ? "" : "." + ext);
                    query.name().equalTo(documentName);
                    List<org.opencrx.kernel.document1.jmi1.Document> documents = documentSegment.getDocument(query); 
                    org.opencrx.kernel.document1.jmi1.Document document = null;
                    if(documents.isEmpty()) {
                        document = pm.newInstance(org.opencrx.kernel.document1.jmi1.Document.class);
                        document.setName(documentName);
                        document.setActiveOn(activeOn);
                        document.getOwningGroup().clear();
                        document.getOwningGroup().addAll(principalGroups);
                        try {
                            pm.currentTransaction().begin();
                            documentSegment.addDocument(
                                Base.getInstance().getUidAsString(), 
                                document
                            );
                            pm.currentTransaction().commit();
                        }
                        catch(Exception e) {
                            try {
                                pm.currentTransaction().rollback();
                            } catch(Exception e0) {}
                            this.sendAlert(
                            	documentSegment, 
                            	filename, 
                            	e.getMessage(), 
                            	pm
                            );
                            new ServiceException(e).log();
                            hasErrors = true;
                        }                                                                        
                    }
                    else {
                        document = documents.iterator().next();                        
                    }                    
                    // Find revision with specified version
                    DocumentRevision revision = null;
                    Collection<DocumentRevision> revisions = document.getRevision();
                    for(DocumentRevision r: revisions) {
                        if(version.equals(r.getVersion())) {
                            revision = r;
                            break;
                        }
                    }
                    // Create revision with specified version
                    if(revision == null) {                    
                        // Upload file
                        if(upload) {
                            MediaContent mediaContent = pm.newInstance(MediaContent.class);
                            mediaContent.setContent(BinaryLargeObjects.valueOf(file));
                            mediaContent.setContentMimeType(
                                MimetypesFileTypeMap.getDefaultFileTypeMap().getContentType(file.getName())
                            );
                            mediaContent.setContentName(file.getName());
                            revision = mediaContent;
                        }
                        // Create a document link
                        else {
                            ResourceIdentifier resourceIdentifier = pm.newInstance(ResourceIdentifier.class);
                            resourceIdentifier.setUri(
                                uriPrefix + 
                                currentDir.getPath().substring(rootDir.getPath().length()).replace("\\", "/") + 
                                "/" + 
                                file.getName()
                            );
                            revision = resourceIdentifier;
                        }
                        revision.setName(file.getName());
                        revision.setVersion(version);
                        revision.getOwningGroup().clear();
                        revision.getOwningGroup().addAll(principalGroups);
                        try {
                            pm.currentTransaction().begin();
                            document.addRevision(
                                Base.getInstance().getUidAsString(), 
                                revision
                            );
                            document.setHeadRevision(revision);
                            pm.currentTransaction().commit();
                        }
                        catch(Exception e) {
                            try {
                                pm.currentTransaction().rollback();
                            } catch(Exception e0) {}
                            this.sendAlert(
                            	documentSegment, 
                            	filename, 
                            	e.getMessage(), 
                            	pm
                            );
                            new ServiceException(e).log();
                            hasErrors = true;
                        }                                            
                    }
                    // Assign document to folders
                    List<String> folderNames = new ArrayList<String>();
                    for(
                        int i = 1; 
                        i < (hasVersion ? names.length-1 : names.length); 
                        i++
                    ) {
                    	if(!names[i].startsWith("@")) {
                    		folderNames.add(names[i]);
                    	}
                    }
                    folderNames.add(currentDir.getName());
                    for(String folderName: folderNames) {
                        DocumentFolderQuery folderQuery = (DocumentFolderQuery)pm.newQuery(org.opencrx.kernel.document1.jmi1.DocumentFolder.class);
                        folderQuery.name().equalTo(folderName);
                        List<org.opencrx.kernel.document1.jmi1.DocumentFolder> folders = documentSegment.getFolder(folderQuery);
                        org.opencrx.kernel.document1.jmi1.DocumentFolder folder = null;
                        if(folders.isEmpty()) {
                            folder = pm.newInstance(org.opencrx.kernel.document1.jmi1.DocumentFolder.class);
                            folder.setName(folderName);
                            // Default security for folders
                            try {
                                pm.currentTransaction().begin();
                                documentSegment.addFolder(
                                    Base.getInstance().getUidAsString(), 
                                    folder
                                );
                                pm.currentTransaction().commit();
                            }
                            catch(Exception e) {
                                try {
                                    pm.currentTransaction().rollback();
                                } catch(Exception e0) {}
                                this.sendAlert(
                                	documentSegment, 
                                	filename, 
                                	e.getMessage(), 
                                	pm
                                );
                                new ServiceException(e).log();
                                hasErrors = true;
                            }                        
                        }
                        else {
                            try {
                                pm.currentTransaction().begin();
	                            folder = folders.iterator().next();
	                            List<org.opencrx.security.realm1.jmi1.PrincipalGroup> groups = documentSegment.getOwningGroup();
	                            for(org.opencrx.security.realm1.jmi1.PrincipalGroup group: groups) {
	                            	if(!folder.getOwningGroup().contains(group)) {
	                            		folder.getOwningGroup().add(group);
	                            	}
	                            }
                                pm.currentTransaction().commit();
                            }
                            catch(Exception e) {
                                try {
                                    pm.currentTransaction().rollback();
                                } catch(Exception e0) {}
                                this.sendAlert(
                                	documentSegment, 
                                	filename, 
                                	e.getMessage(), 
                                	pm
                                );
                                new ServiceException(e).log();
                                hasErrors = true;
                            }                        
                        }
                        // Assign document to folder
                        FolderAssignmentQuery assignmentQuery = (FolderAssignmentQuery)pm.newQuery(FolderAssignment.class);
                        assignmentQuery.thereExistsDocumentFolder().equalTo(folder);
                        List<FolderAssignment> assignments = document.getDocumentFolderAssignment(assignmentQuery);
                        // Add assignment
                        if(assignments.isEmpty()) {
                            FolderAssignment assignment = pm.newInstance(FolderAssignment.class);
                            assignment.setName(folder.getName());
                            assignment.setDocumentFolder(folder);
                            assignment.getOwningGroup().clear();
                            assignment.getOwningGroup().addAll(principalGroups);
                            try {
                                pm.currentTransaction().begin();
                                document.addDocumentFolderAssignment(
                                    Base.getInstance().getUidAsString(), 
                                    assignment
                                );
                                pm.currentTransaction().commit();
                                hasErrors = true;
                            }
                            catch(Exception e) {
                                try {
                                    pm.currentTransaction().rollback();
                                } catch(Exception e0) {}
                                this.sendAlert(
                                	documentSegment, 
                                	filename, 
                                	e.getMessage(), 
                                	pm
                                );
                                new ServiceException(e).log();
                                hasErrors = true;
                            }
                        }
                    }
                    // Delete file from import directory if no errors occurred
                    if(!hasErrors) {
                    	try {
                    		file.delete();
                    	}
                    	catch(Exception e) {
                    		new ServiceException(e).log();
                    	}
                    }
                }
            }
        }
    }
    
    //-----------------------------------------------------------------------    
    public void scanDocuments(
        String id,
        String providerName,
        String segmentName,
        HttpServletRequest req, 
        HttpServletResponse res        
    ) throws IOException {
        System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName);
        try {
            PersistenceManager pm = this.pmf.getPersistenceManager(
                SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName,
                null
            );
            PersistenceManager rootPm = Utils.getPersistenceManagerFactory().getPersistenceManager(
                SecurityKeys.ROOT_PRINCIPAL,
                null
            );
            Workflows.getInstance().initWorkflows(
                pm, 
                providerName, 
                segmentName
            );
            for(int i = -1; i < 10; i++) {
                String idxSuffix = i < 0 ? "" : "[" + i + "]";
                org.opencrx.kernel.base.jmi1.StringProperty scanDir = this.getComponentConfigProperty(
                    rootPm,
                    providerName, 
                    segmentName, 
                    OPTION_SCAN_DIR + idxSuffix
                );
                org.opencrx.kernel.base.jmi1.StringProperty urlPrefix = this.getComponentConfigProperty(
                    rootPm,
                    providerName, 
                    segmentName, 
                    OPTION_URL_PREFIX + idxSuffix
                );
                org.opencrx.kernel.base.jmi1.StringProperty upload = this.getComponentConfigProperty(
                    rootPm,
                    providerName, 
                    segmentName, 
                    OPTION_UPLOAD + idxSuffix
                );
                org.opencrx.kernel.base.jmi1.StringProperty groups = this.getComponentConfigProperty(
                    rootPm,
                    providerName, 
                    segmentName, 
                    OPTION_GROUPS + idxSuffix
                );
                if(scanDir != null) {
                    rootPm.currentTransaction().begin();
                    scanDir.setDescription(
                        "Last scan at " + new Date()
                    );
                    rootPm.currentTransaction().commit();
                    scanDir = (org.opencrx.kernel.base.jmi1.StringProperty)rootPm.getObjectById(scanDir.refGetPath());
                    if(
                        (scanDir.getStringValue() != null) && 
                        (scanDir.getStringValue().length() > 0)                
                    ) {
                        File dir = new File(scanDir.getStringValue());
                        org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName); 
                        List<org.opencrx.security.realm1.jmi1.PrincipalGroup> principalGroups = 
                            new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
                        if((groups == null) || (groups.getStringValue().length() == 0)) {
                            org.opencrx.security.realm1.jmi1.PrincipalGroup group =
                                (org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(
                                    new Path("xri://@openmdx*org.openmdx.security.realm1").getDescendant("provider", providerName, "segment", "Root", "realm", segmentName, "principal", SecurityKeys.USER_GROUP_USERS)
                                );                            
                            principalGroups.add(group);
                            group =
                                (org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(
                                    new Path("xri://@openmdx*org.openmdx.security.realm1").getDescendant("provider", providerName, "segment", "Root", "realm", segmentName, "principal", SecurityKeys.USER_GROUP_ADMINISTRATORS)
                                );                            
                            principalGroups.add(group);
                        }
                        else {
                            StringTokenizer tokenizer = new StringTokenizer(groups.getStringValue(), ",; ", false);
                            while(tokenizer.hasMoreTokens()) {
                                String groupName = tokenizer.nextToken();
                                org.opencrx.security.realm1.jmi1.PrincipalGroup group = null;
                                try {
                                    group = (org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(
                                        new Path("xri://@openmdx*org.openmdx.security.realm1").getDescendant("provider", providerName, "segment", "Root", "realm", segmentName, "principal", groupName)
                                    );
                                } catch(Exception e) {}
                                if(group != null) {
                                    principalGroups.add(group);
                                }                                
                            }
                        }
                        this.scanDocuments(
                            dir,
                            dir, 
                            documentSegment,
                            urlPrefix == null ? "http://localhost" : urlPrefix.getStringValue(),
                            upload == null ? false : Boolean.valueOf(upload.getStringValue()),
                            principalGroups,
                            pm
                        );                    
                    }
                }
            }
            try {
            	if(pm != null) {
            		pm.close();
            	}
            	if(rootPm != null) {
            		rootPm.close();
            	}
            } catch(Exception e) {}
        }
        catch(Exception e) {
            new ServiceException(e).log();
            System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": exception occured " + e.getMessage() + ". Continuing");
        }        
    }
    
    //-----------------------------------------------------------------------
    protected void handleRequest(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        if(System.currentTimeMillis() > this.startedAt + STARTUP_DELAY) {
            String segmentName = req.getParameter("segment");
            String providerName = req.getParameter("provider");
            String id = providerName + "/" + segmentName;
            if(COMMAND_EXECUTE.equals(req.getPathInfo())) {
                if(!runningSegments.containsKey(id)) {
	                try {
	                    runningSegments.put(
	                    	id,
	                    	Thread.currentThread()
	                    );
	                    this.scanDocuments(
	                        id,
	                        providerName,
	                        segmentName,
	                        req,
	                        res
	                    );
	                } 
	                catch(Exception e) {
	                    new ServiceException(e).log();
	                }
	                finally {
	                    runningSegments.remove(id);
	                }
                }
	        	else if(
	        		!runningSegments.get(id).isAlive() || 
	        		runningSegments.get(id).isInterrupted()
	        	) {
	            	Thread t = runningSegments.get(id);
	        		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": workflow " + t.getId() + " is alive=" + t.isAlive() + "; interrupted=" + t.isInterrupted() + ". Skipping execution.");
	        	}
            }
        }
    }

    //-----------------------------------------------------------------------
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        res.setStatus(HttpServletResponse.SC_OK);
        res.flushBuffer();
        this.handleRequest(
            req,
            res
        );
    }
        
    //-----------------------------------------------------------------------
    protected void doPost(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        res.setStatus(HttpServletResponse.SC_OK);
        res.flushBuffer();
        this.handleRequest(
            req,
            res
        );
    }
        
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = 4441731357561757549L;

    private static final String COMMAND_EXECUTE = "/execute";
    private static final String WORKFLOW_NAME = "DocumentScanner";
    private static final String COMPONENT_CONFIGURATION_ID = "DocumentScanner"; 
    private static final String OPTION_SCAN_DIR = "scanDir";
    private static final String OPTION_URL_PREFIX = "urlPrefix";
    private static final String OPTION_UPLOAD = "upload";
    private static final String OPTION_GROUPS = "groups";
    private static final long STARTUP_DELAY = 180000L;
    
    private PersistenceManagerFactory pmf = null;
    private static final Map<String,Thread> runningSegments = new ConcurrentHashMap<String,Thread>();
    private long startedAt = System.currentTimeMillis();
        
}

//--- End of File -----------------------------------------------------------
