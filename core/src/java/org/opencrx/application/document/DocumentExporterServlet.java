/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DocumentExporterServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2014, CRIXP Corp., Switzerland
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.admin1.jmi1.ComponentConfiguration;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.document1.cci2.DocumentBasedFolderEntryQuery;
import org.opencrx.kernel.document1.cci2.DocumentFolderQuery;
import org.opencrx.kernel.document1.cci2.DocumentFolderShareQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentBasedFolderEntry;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.DocumentFolderShare;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.opencrx.kernel.utils.FileUtils;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * The DocumentExporterServlet iterates the DocumentFolderShares and exports the documents
 * in the corresponding DocumentFolder and its sub-folders to the target folder 
 * {basedir}/{provider}/{segment}/{user}/{share.name}. basedir is specified by
 * the component configuration property 'basedir'. The export is performed as diff:
 * new documents are added, changed documents are updated, disabled or deleted documents
 * are removed.
 * 
 */  
public class DocumentExporterServlet extends HttpServlet {

	/* (non-Javadoc)
	 * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
	 */
	@Override
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

	/**
	 * Get qualified folder name.
	 * 
	 * @param documentFolder
	 * @param separator
	 * @return
	 */
	protected String getQualifiedFolderName(
		DocumentFolder documentFolder,
		String separator
	) {
		if(documentFolder == null) {
			return "";		
		} else if(documentFolder.getParent() == null) {
			return documentFolder.getName();
		} else {
			return this.getQualifiedFolderName(documentFolder.getParent(), separator) + separator + documentFolder.getName();
		}
	}

	/**
	 * Get normalized file name.
	 * 
	 * @param name
	 * @return
	 */
	protected String getNormalizedFilename(
		String name
	) {
		final int MAX_NAME_LENGHT = 250;
		String normalizedFileName = "";
		for(int i = 0; i < name.length(); i++) {
			char c = name.charAt(i);
			if(Character.isJavaIdentifierPart(c)) {
				normalizedFileName += c;
			} else if(c == '"') {
				normalizedFileName += "'";
			} else if(c == '.') {
				normalizedFileName += ".";
			} else if(c == '-') {
				normalizedFileName += "-";
			} else if(c == '(') {
				normalizedFileName += "(";
			} else if(c == ')') {
				normalizedFileName += ")";
			} else if(c == '~') {
				normalizedFileName += "~";
			} else {
				normalizedFileName += (normalizedFileName.length() > 0) && (normalizedFileName.charAt(normalizedFileName.length() - 1) != '_') ? "_" : "";
			}
		}
		if(normalizedFileName.length() > MAX_NAME_LENGHT) {
			int pos = normalizedFileName.lastIndexOf(".");
			if(pos > 0) {
				String suffix = normalizedFileName.substring(pos + 1);
				return normalizedFileName.substring(0, MAX_NAME_LENGHT - suffix.length()) + "." + suffix;						
			} else {
				return normalizedFileName.substring(0, MAX_NAME_LENGHT);
			}
		} else {
			return normalizedFileName;
		}
	}

	/**
	 * Execute hook.
	 * 
	 * @param hook
	 * @param targetDir
	 * @param providerName
	 * @param segmentName
	 */
	protected void executeHook(
		File hook,
		File targetDir,
		String providerName,
		String segmentName
	) {
		try {
			System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Running " + hook.getAbsolutePath());			            				
			Process p = Runtime.getRuntime().exec(hook.getAbsolutePath(), null, targetDir);
			// Print stdout
			{
				BufferedReader out = new BufferedReader(new InputStreamReader(p.getInputStream()));
				String line = null;
				while((line = out.readLine()) != null) {
					System.out.println(line);
				}
			}
			// Print errors
			{
				BufferedReader out = new BufferedReader(new InputStreamReader(p.getErrorStream()));
				String line = null;
				while((line = out.readLine()) != null) {
					System.out.println(line);
				}
			}
			p.waitFor();
		} catch(Exception e) {
			new ServiceException(e).log();
			System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Error running " + hook.getAbsolutePath() + ". Message is " + e.getMessage());			            				
		}		
	}

    /**
     * Recursively export documents for given folder to targetDir.
     *  
     * @param targetDir
     * @param documentFolder
     * @throws ServiceException
     */
    private void exportDocuments(
        File targetDir,
        DocumentFolder documentFolder,
        File hooksDir,
        long syncKey
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(documentFolder);
    	String providerName = documentFolder.refGetPath().get(2);
    	String segmentName = documentFolder.refGetPath().get(4);    	
    	DocumentBasedFolderEntryQuery folderEntryQuery = (DocumentBasedFolderEntryQuery)pm.newQuery(DocumentBasedFolderEntry.class);
    	folderEntryQuery.forAllDisabled().isFalse();
    	List<String> exportedFiles = new ArrayList<String>();
    	// Export documents
    	for(DocumentBasedFolderEntry entry: documentFolder.<DocumentBasedFolderEntry>getFolderEntry(folderEntryQuery)) {
    		if(entry.getDocument() instanceof Document) {
    			Document document = (Document)entry.getDocument();
    			if(document.getHeadRevision() instanceof MediaContent) {
    				FileOutputStream target = null;
    				InputStream source = null;
    				try {
    					// Only write content if changed
    					String name = document.getName() == null ? "unknown.bin" : this.getNormalizedFilename(document.getName());
    					File targetFile = new File(targetDir, name); 
    					if(!targetFile.exists() || document.getModifiedAt().getTime() > syncKey) {
		    				target = new FileOutputStream(targetFile);
		    				MediaContent mediaContent = (MediaContent)document.getHeadRevision();
		    				source = mediaContent.getContent().getContent();
		    				BinaryLargeObjects.streamCopy(source, 0L, target);
    					}
	    				exportedFiles.add(name);
    				} catch(Exception e) {
    					new ServiceException(e).log();
    					Base.getInstance().sendAlert(
    						document, 
    						SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName, 
    						DocumentExporterServlet.class.getName() + ": unable to export document", 
    						e.getMessage(), 
    						(short)2, 
    						0, 
    						null
    					);
    				} finally {
    					if(target != null) {
    						try {
    							target.close();
    						} catch(Exception ignore) {}
    					}
    					if(source != null) {
    						try {
    							source.close();
    						} catch(Exception ignore) {}
    					}
    				}
    			}
    		}
    	}
    	// Export folders
    	DocumentFolderQuery subFolderQuery = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
    	subFolderQuery.forAllDisabled().isFalse();
    	for(DocumentFolder subFolder: documentFolder.<DocumentFolder>getSubFolder(subFolderQuery)) {
    		String name = this.getNormalizedFilename(subFolder.getName());
    		File subDir = new File(targetDir, name);
    		subDir.mkdirs();
        	File preExportSubdirHook = new File(hooksDir, "pre-export-subdir");
        	if(preExportSubdirHook.exists()) {
    			this.executeHook(
    				preExportSubdirHook, 
    				subDir, 
    				providerName,
    				segmentName
    			);
        	}
    		this.exportDocuments(
    			subDir, 
    			subFolder,
    			hooksDir,
    			syncKey
    		);
        	File postExportSubdirHook = new File(hooksDir, "post-export-subdir");
        	if(postExportSubdirHook.exists()) {
    			this.executeHook(
    				postExportSubdirHook, 
    				subDir, 
    				providerName,
    				segmentName
    			);
        	}
    		exportedFiles.add(name);
    	}
    	// Clean targetDir
        File[] files = targetDir.listFiles();
        if(files != null) {
            for(File file: files) {
            	if(!exportedFiles.contains(file.getName())) {
            		FileUtils.deleteQuietly(file);
            	}
            }
        }
    }

    /**
     * Export documents for all active shares.
     * 
     * @param id
     * @param providerName
     * @param segmentName
     * @param req
     * @param res
     * @throws IOException
     */
    public void exportDocuments(
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
            ComponentConfiguration componentConfig = 
            	ComponentConfigHelper.getComponentConfiguration(
	            	CONFIGURATION_ID, 
	            	providerName, 
	            	rootPm, 
	            	true,
	            	new String[][]{
	    				{providerName + "." + segmentName + "." + OPTION_BASE_DIR, "./docdir"},
	    				{providerName + "." + segmentName + "." + OPTION_SYNC_KEY, "0"}
	            	}
	            );
            StringProperty baseDirProperty = ComponentConfigHelper.getComponentConfigProperty(
            	providerName + "." + segmentName + "." + OPTION_BASE_DIR,
            	componentConfig
            );
            StringProperty syncKeyProperty = ComponentConfigHelper.getComponentConfigProperty(
            	providerName + "." + segmentName + "." + OPTION_SYNC_KEY,
            	componentConfig
            );            
            long newSyncKey = System.currentTimeMillis();
            // Get all folder shares and export documents
            {
            	long syncKey = 0L;
            	try {
            		syncKey = Long.valueOf(syncKeyProperty.getStringValue());
            	} catch(Exception ignore) {}
	            org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName);
	            DocumentFolderShareQuery folderShareQuery = (DocumentFolderShareQuery)PersistenceHelper.newQuery(
		    		pm.getExtent(DocumentFolderShare.class),
		    		documentSegment.refGetPath().getDescendant("folder", ":*", "folderShare", ":*")
		    	);
	            folderShareQuery.orderByCreatedAt().ascending();
	            Map<File,DocumentFolderShare> exportedShares = new HashMap<File,DocumentFolderShare>();
	            for(DocumentFolderShare folderShare: documentSegment.<DocumentFolderShare>getExtent(folderShareQuery)) {
	            	File baseDir = new File(baseDirProperty.getStringValue() == null || baseDirProperty.getStringValue().isEmpty() ? "./docdir" : baseDirProperty.getStringValue());
	            	File hooksDir = new File(baseDir, "hooks");
	            	DocumentFolder documentFolder = (DocumentFolder)pm.getObjectById(folderShare.refGetPath().getParent().getParent());
	            	File userDir = new File(baseDir, providerName);
	            	userDir = new File(userDir, segmentName);
	            	userDir = new File(userDir, folderShare.getShareForUser().refGetPath().getBase());
	            	File folderDir = new File(userDir, folderShare.getName());
	            	if(exportedShares.containsKey(folderDir)) {
		            	folderDir = new File(folderDir, this.getQualifiedFolderName(documentFolder.getParent(), ".") + "~" + folderShare.getName());
	            	}
	            	if(exportedShares.containsKey(folderDir)) {
	            		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Duplicate share names. Will not export. Share.1=" + exportedShares.get(folderDir).refGetPath().toXRI() + ". Share.2=" + folderShare.refGetPath().toXRI() + ". Target.dir=" + folderDir);
	            	} else {
		            	if(Boolean.TRUE.equals(folderShare.isActive())) {
		            		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Exporting " + folderShare.refGetPath().toXRI() + " to " + folderDir);
			            	folderDir.mkdirs();
			            	// pre-export
			            	{
				            	File preExportHook = new File(hooksDir, "pre-export");
				            	if(preExportHook.exists()) {
			            			this.executeHook(
			            				preExportHook, 
			            				folderDir.exists() ? folderDir : userDir, 
			            				providerName,
			            				segmentName
			            			);
				            	}
			            	}
			            	// Export
			            	{
				            	this.exportDocuments(
				            		folderDir,
				            		documentFolder,
				            		hooksDir,
				            		syncKey
				            	);
				            	exportedShares.put(
				            		folderDir, 
				            		folderShare
				            	);
			            	}
		            	} else {
		            		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Removing " + folderShare.refGetPath().toXRI() + " at " + folderDir);
			            	// pre-export
			            	{
				            	File preExportHook = new File(hooksDir, "pre-export");
				            	if(preExportHook.exists()) {
			            			this.executeHook(
			            				preExportHook, 
			            				folderDir.exists() ? folderDir : userDir, 
			            				providerName,
			            				segmentName
			            			);
				            	}
			            	}
			            	// Remove
			            	{
			            		FileUtils.deleteQuietly(folderDir);
			            	}			            		
		            	}
		            	// post-export
		            	File postExportHook = new File(hooksDir, "post-export");
		            	if(postExportHook.exists()) {
	            			this.executeHook(
	            				postExportHook,
	            				folderDir.exists() ? folderDir : userDir, 
	            				providerName, 
	            				segmentName
	            			);
		            	}
	            	}
	            }
            }
            rootPm.currentTransaction().begin();
            syncKeyProperty.setStringValue(Long.toString(newSyncKey));
            rootPm.currentTransaction().commit();
            try {
            	if(pm != null) {
            		pm.close();
            	}
            	if(rootPm != null) {
            		rootPm.close();
            	}
            } catch(Exception e) {}
        } catch(Exception e) {
            new ServiceException(e).log();
            System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": ERROR: Exception occured " + e.getMessage() + ". Continuing");
        }
    }

    /**
     * Export Documents.
     * 
     * @param req
     * @param res
     * @throws ServletException
     * @throws IOException
     */
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
	                    this.exportDocuments(
	                        id,
	                        providerName,
	                        segmentName,
	                        req,
	                        res
	                    );
	                } catch(Exception e) {
	                    new ServiceException(e).log();
	                } finally {
	                    runningSegments.remove(id);
	                }
                } else if(
	        		!runningSegments.get(id).isAlive() || 
	        		runningSegments.get(id).isInterrupted()
	        	) {
	            	Thread t = runningSegments.get(id);
	        		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": workflow " + t.getId() + " is alive=" + t.isAlive() + "; interrupted=" + t.isInterrupted() + ". Skipping execution.");
	        	}
            }
        }
    }

    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
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

    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
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
    private static final String WORKFLOW_NAME = "DocumentExporter";
    private static final String CONFIGURATION_ID = "DocumentExporter"; 
    private static final String OPTION_BASE_DIR = "baseDir";
    private static final String OPTION_SYNC_KEY = "syncKey";
    private static final long STARTUP_DELAY = 180000L;
    
    private PersistenceManagerFactory pmf = null;
    private static final Map<String,Thread> runningSegments = new ConcurrentHashMap<String,Thread>();
    private long startedAt = System.currentTimeMillis();
        
}

//--- End of File -----------------------------------------------------------
