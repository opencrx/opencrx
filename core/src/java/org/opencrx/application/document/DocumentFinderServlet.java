/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DocumentFinderServlet
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.jdo.Query;
import javax.mail.internet.MimeUtility;
import javax.naming.NamingException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.base.cci2.IndexEntryQuery;
import org.opencrx.kernel.base.jmi1.CheckPermissionsParams;
import org.opencrx.kernel.base.jmi1.CheckPermissionsResult;
import org.opencrx.kernel.base.jmi1.IndexEntry;
import org.opencrx.kernel.document1.cci2.DocumentBasedFolderEntryQuery;
import org.opencrx.kernel.document1.cci2.DocumentFolderQuery;
import org.opencrx.kernel.document1.cci2.DocumentQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentBasedFolderEntry;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.home1.cci2.DocumentFeedQuery;
import org.opencrx.kernel.home1.cci2.DocumentProfileQuery;
import org.opencrx.kernel.home1.jmi1.DocumentFeed;
import org.opencrx.kernel.home1.jmi1.DocumentProfile;
import org.opencrx.kernel.home1.jmi1.ObjectFinder;
import org.opencrx.kernel.home1.jmi1.SearchBasicParams;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.persistence.cci.UserObjects;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.uses.org.apache.commons.fileupload.DiskFileUpload;
import org.openmdx.uses.org.apache.commons.fileupload.FileItem;
import org.openmdx.uses.org.apache.commons.fileupload.FileUpload;
import org.openmdx.uses.org.apache.commons.fileupload.FileUploadException;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

import com.google.gson.Gson;

/**
 * DocumentFinderServlet is the backend for the elFinder.
 * 
 */
public class DocumentFinderServlet extends HttpServlet {

	/**
	 * ErrorResult
	 *
	 */
	static class ErrorResult {

		/**
		 * @return the error
		 */
		public String getError() {
			return error;
		}

		/**
		 * @param error the error to set
		 */
		public void setError(String error) {
			this.error = error;
		}
		
		private String error;

	}
	
	/**
	 * FileDescr
	 *
	 */
	static class FileDescr {
		/**
		 * @return the hash
		 */
		public String getHash() {
			return hash;
		}
		/**
		 * @param hash the hash to set
		 */
		public void setHash(String hash) {
			this.hash = hash;
		}
		/**
		 * @return the name
		 */
		public String getName() {
			return name;
		}
		/**
		 * @param name the name to set
		 */
		public void setName(String name) {
			this.name = name;
		}
		/**
		 * @return the mime
		 */
		public String getMime() {
			return mime;
		}
		/**
		 * @param mime the mime to set
		 */
		public void setMime(String mime) {
			this.mime = mime;
		}
		/**
		 * @return the rel
		 */
		public String getRel() {
			return rel;
		}
		/**
		 * @param rel the rel to set
		 */
		public void setRel(String rel) {
			this.rel = rel;
		}
		/**
		 * @return the size
		 */
		public long getSize() {
			return size;
		}
		/**
		 * @param size the size to set
		 */
		public void setSize(long size) {
			this.size = size;
		}
		/**
		 * @return the date
		 */
		public Date getDate() {
			return date;
		}
		/**
		 * @param date the date to set
		 */
		public void setDate(Date date) {
			this.date = date;
		}
		/**
		 * @return the read
		 */
		public Boolean isRead() {
			return read;
		}
		/**
		 * @param read the read to set
		 */
		public void setRead(Boolean read) {
			this.read = read;
		}
		/**
		 * @return the rm
		 */
		public Boolean isRm() {
			return rm;
		}
		/**
		 * @param rm the rm to set
		 */
		public void setRm(Boolean rm) {
			this.rm = rm;
		}
		/**
		 * @return the write
		 */
		public Boolean isWrite() {
			return write;
		}
		/**
		 * @param write the write to set
		 */
		public void setWrite(Boolean write) {
			this.write = write;
		}
		/**
		 * @return the url
		 */
		public String getUrl() {
			return url;
		}
		/**
		 * @param url the url to set
		 */
		public void setUrl(String url) {
			this.url = url;
		}
		/**
		 * @return the dirs
		 */
		public Boolean getDirs() {
			return dirs;
		}
		/**
		 * @param dirs the dirs to set
		 */
		public void setDirs(Boolean dirs) {
			this.dirs = dirs;
		}
		/**
		 * @return the phash
		 */
		public String getPhash() {
			return phash;
		}
		/**
		 * @param phash the phash to set
		 */
		public void setPhash(String phash) {
			this.phash = phash;
		}
		/**
		 * @return the volumeid
		 */
		public String getVolumeid() {
			return volumeid;
		}
		/**
		 * @param volumeid the volumeid to set
		 */
		public void setVolumeid(String volumeid) {
			this.volumeid = volumeid;
		}
		private String hash;
		private String phash;
		private String name;
		private String mime;
		private String rel;
		private long size;
		private Date date;
		private Boolean read;
		private Boolean write;
		private Boolean rm;
		private String url;
		private String volumeid;
		private Boolean dirs;
	}

	/**
	 * TreeResult
	 *
	 */
	static class TreeResult {

		/**
		 * @return the tree
		 */
		public List<FileDescr> getTree() {
			return tree;
		}

		/**
		 * @param tree the tree to set
		 */
		public void setTree(List<FileDescr> tree) {
			this.tree = tree;
		}
		
		private List<FileDescr> tree;

	}

	/**
	 * OpenResult
	 *
	 */
	static class OpenResult {
		/**
		 * @return the cwd
		 */
		public FileDescr getCwd() {
			return cwd;
		}
		/**
		 * @param cwd the cwd to set
		 */
		public void setCwd(FileDescr cwd) {
			this.cwd = cwd;
		}
		public List<FileDescr> getFiles() {
			return files;
		}
		/**
		 * @param files the files to set
		 */
		public void setFiles(List<FileDescr> files) {
			this.files = files;
		}
		/**
		 * @return the netDrivers
		 */
		public List<String> getNetDrivers() {
			return netDrivers;
		}
		/**
		 * @param netDrivers the netDrivers to set
		 */
		public void setNetDrivers(List<String> netDrivers) {
			this.netDrivers = netDrivers;
		}
		/**
		 * @return the api
		 */
		public int getApi() {
			return api;
		}
		/**
		 * @param api the api to set
		 */
		public void setApi(int api) {
			this.api = api;
		}
		private FileDescr cwd;
		private List<FileDescr> files;
		private List<String> netDrivers;
		private int api;

	}

	/**
	 * LsResult
	 *
	 */
	static class LsResult {

		/**
		 * @return the list
		 */
		public List<FileDescr> getList() {
			return list;
		}

		/**
		 * @param list the list to set
		 */
		public void setList(List<FileDescr> list) {
			this.list = list;
		}
		
		private List<FileDescr> list;

	}
	
	/**
	 * FilesResult
	 *
	 */
	static class FilesResult {
		/**
		 * @return the files
		 */
		public List<FileDescr> getFiles() {
			return files;
		}

		/**
		 * @param files the files to set
		 */
		public void setFiles(List<FileDescr> files) {
			this.files = files;
		}
		private List<FileDescr> files;
	}

	/**
	 * FileOpResult
	 *
	 */
	static class FilesOpResult {

		/**
		 * @return the added
		 */
		public List<FileDescr> getAdded() {
			return added;
		}

		/**
		 * @param added the added to set
		 */
		public void setAdded(List<FileDescr> added) {
			this.added = added;
		}

		/**
		 * @return the removed
		 */
		public List<String> getRemoved() {
			return removed;
		}

		/**
		 * @param removed the removed to set
		 */
		public void setRemoved(List<String> removed) {
			this.removed = removed;
		}
		
		private List<String> removed;
		private List<FileDescr> added;

	}

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
        this.tempdir = (File)config.getServletContext().getAttribute("javax.servlet.context.tempdir");
        this.isDebug = Boolean.valueOf(config.getInitParameter("debug"));
        this.checkPermissions = Boolean.valueOf(config.getInitParameter("checkPermissions"));
    }

    /**
     * Get persistence manager for requesting user.
     * 
     * @return
     */
    protected PersistenceManager getPersistenceManager(
        HttpServletRequest req
    ) {
        return req.getUserPrincipal() == null ?
            null :
            this.pmf.getPersistenceManager(
                req.getUserPrincipal().getName(),
                null
            );
    }

    /**
     * Get temp directory.
     * 
     * @return
     */
    protected File getTempDirectory(
    ) {
    	return this.tempdir;
    }
    
    /**
     * Get session-unique prefix for temp files.
     * 
     * @param req
     * @return
     */
    protected String getTempFilePrefix(
    	HttpServletRequest req
    ) {
    	return req.getSession().getId() + "-";    	
    }

    /**
     * Get session-unique temp file name.
     * 
     * @param req
     * @param name
     * @param extension
     * @return
     */
    protected String getTempFileName(
    	HttpServletRequest req,
        String name,
        String extension
    ) {
        // eliminate special chars
        String fileName = "";
        for(int i = 0; i < name.length(); i++) {
            if(name.charAt(i) != ':') {
                fileName += name.charAt(i);
            }
        }
        return 
        	this.getTempDirectory().getPath() + File.separator + this.getTempFilePrefix(req) + "-" + fileName + extension;
    }

    /**
     * Parse request.
     * 
     * @param req
     * @return
     */
    @SuppressWarnings("unchecked")
	protected Map<String,String[]> parseRequest(
    	HttpServletRequest req
    ) {
    	boolean uploadFailed = false;
    	Map<String,String[]> parameterMap = req.getParameterMap();
    	if(FileUpload.isMultipartContent(req)) {
    		parameterMap = new HashMap<String,String[]>();
    		DiskFileUpload upload = new DiskFileUpload();
    		upload.setHeaderEncoding("UTF-8");
    		List<FileItem> items = null;
    		try {
    			items = upload.parseRequest(
    				req,
    				200, // in-memory threshold. Content for fields larger than threshold is written to disk
    				50000000, // max request size [overall limit]
    				this.getTempDirectory().getPath()
    			);
    		} catch(FileUploadException e) {
    			uploadFailed = true;
    			SysLog.warning("first try to upload file failed", e.getMessage());
    			new ServiceException(e).log();
    		}
    		try {
    			if(uploadFailed) {
    				items = upload.parseRequest(
    					req,
    					200, // in-memory threshold. Content for fields larger than threshold is written to disk
    					60000000, // max request size [overall limit]
    					this.getTempDirectory().getPath()
    				);
    			}
    			for(FileItem item: items) {
    				try {
	    				if(item.isFormField()) {
	    					String[] values = parameterMap.get(item.getFieldName());
	    					if(values == null) {
		    					values = new String[]{item.getString("UTF-8")};
	    					} else {
	    						List<String> l = new ArrayList<String>(Arrays.asList(values));
	    						l.add(item.getString("UTF-8"));
	    						values = l.toArray(new String[l.size()]);
	    					}
	    					parameterMap.put(
	    						item.getFieldName(),
	    						values
	    					);
	    				} else {
	    					String[] values = parameterMap.get(item.getFieldName());
	    					String itemName = item.getName();
	    					// Some browsers do not properly encode UTF-8 file names. Try to fix
	    					{
		    					String fixedItemName = new String(itemName.getBytes("ISO8859-1"), "UTF-8");
		    					if(fixedItemName.length() < itemName.length() && fixedItemName.indexOf(".") > 0) {
		    						itemName = fixedItemName;
		    					}
	    					}
	    					if(values == null) {
	    						values = new String[]{itemName};
	    					} else {
	    						List<String> l = new ArrayList<String>(Arrays.asList(values));
	    						l.add(itemName);
	    						values = l.toArray(new String[l.size()]);	    						
	    					}
    						parameterMap.put(
    							item.getFieldName(),
    							values
    						);
    						String location = this.getTempFileName(req, item.getFieldName(), Integer.toString(values.length));
    						// bytes
    						File outFile = new File(location);
    						item.write(outFile);
    						// type
    						PrintWriter pw = new PrintWriter(
    							new File(location + ".INFO"),
    							"UTF-8"
    						);
    						pw.println(item.getContentType());
    						int sep = item.getName().lastIndexOf("/");
    						if(sep < 0) {
    							sep = item.getName().lastIndexOf("\\");
    						}
    						pw.println(item.getName().substring(sep + 1));
    						pw.close();
	    				}
    				} catch(Exception e) {
    					ServiceException e0 = new ServiceException(
    						e,
    						BasicException.Code.DEFAULT_DOMAIN,
    						BasicException.Code.PARSE_FAILURE,
    						"Error parsing request. Ignoring",
    						new BasicException.Parameter("name", item.getFieldName())
    					);
    					e0.log();
    				}
    			}
    		} catch(FileUploadException e) {
    			SysLog.warning("File upload errror", e.getMessage());
    			new ServiceException(e).log();
    		}
    	}
    	return parameterMap;
    }

	/**
	 * URL encode given string.
	 * 
	 * @param s
	 * @return
	 * @throws ServiceException
	 */
	@SuppressWarnings("deprecation")
	protected String encodeURL(
		String s
	) throws ServiceException {
		try {
			return URLEncoder.encode(s, "UTF-8").replace("+", "%20");
		} catch(Exception ignore) {
			return URLEncoder.encode(s).replace("+", "%20");
		}
	}

    /**
     * Get WebDAV URL for given document.
     * 
     * @param req
     * @param folder
     * @param document
     * @param rootFolders
     * @return
     * @throws ServiceException
     */
    protected String getDocumentUrl(
    	HttpServletRequest req,
    	DocumentFolder folder,
    	Document document,
    	List<DocumentFolder> rootFolders,
    	boolean encode
    ) throws ServiceException {
    	String path = encode ? this.encodeURL(document.getName()) : document.getName();
    	DocumentFolder current = folder;
    	while(current != null) {
    		path = (encode ? this.encodeURL(current.getName()) : current.getName()) + "/" + path;
    		if(rootFolders.contains(current)) break;
    		current = current.getParent();
    	}
    	return req.getRequestURI().replace("-documents-", "-webdav-").replace("/connector", "") + "/" + path;
    }

    /**
     * Return true if flag is set.
     * 
     * @param flag
     * @return
     */
    protected boolean isSet(
    	String[] flag
    ) {
    	return flag != null && flag.length > 0 && ("1".equals(flag[0]) || Boolean.valueOf(flag[0]));
    }

    /**
     * Non-secure encoding of given string.
     * 
     * @param s
     * @return
     */
    protected String toHash(
    	String s
    ) {
    	String hash = null;
    	try {
    		hash = Base64.encode(s.getBytes("UTF-8"));
    	} catch(Exception ignore) {
    		hash = Base64.encode(s.getBytes());
    	}
    	hash = hash.replace("+", "u002B");
    	hash = hash.replace("=", "u003D");
    	hash = hash.replace("/", "u002F");
    	return hash;
    }

    /**
     * Decode hash.
     * 
     * @param e
     * @return
     */
    protected String fromHash(
    	String e
    ) {
    	if(e != null) {
    		e = e.replace("u002B", "+");
    		e = e.replace("u003D", "=");
    		e = e.replace("u002F", "/");
    	}
    	try {
    		return new String(Base64.decode(e), "UTF-8");
    	} catch(Exception e0) {
    		try {
    			return new String(Base64.decode(e));
    		} catch(Exception e1) {
    			return e;
    		}
    	}
    }

    /**
     * Send response.
     * 
     * @param req
     * @param resp
     * @param bean
     * @throws ServiceException
     */
    protected void sendResponse(
    	HttpServletRequest req,
    	HttpServletResponse resp,
    	Object bean
    ) throws ServiceException {
    	try {
	    	Gson gson = new Gson();
			resp.setContentType("application/json; charset=UTF-8");
			resp.getWriter();
			gson.toJson(bean, resp.getWriter());
			if(this.isDebug) {
				System.out.println(new Date() + "  DocumentFinderServlet: " + req.getRequestURL());
				gson.toJson(bean, System.out);
				System.out.println();
				System.out.println();
			}
    	} catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }

    /**
     * Send error response.
     * 
     * @param req
     * @param resp
     * @param error
     * @throws ServiceException
     */
    protected void sendErrorResponse(
    	HttpServletRequest req,
    	HttpServletResponse resp,
    	String error
    ) throws ServiceException {
    	ErrorResult errorBean = new ErrorResult();
    	errorBean.setError(error);
    	this.sendResponse(req, resp, errorBean);
    }

    /**
     * Get fd for given folder.
     * 
     * @param req
     * @param folder
     * @return
     * @throws ServiceException
     */
	protected FileDescr stat(
    	HttpServletRequest req,
    	DocumentFolder folder,
    	List<DocumentFolder> rootFolders,
    	boolean checkPermissions
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(folder);
    	CheckPermissionsResult checkPermissionsResult = null;
    	if(this.checkPermissions && checkPermissions) {
	    	UserHome userHome = UserHomes.getInstance().getUserHome(folder.refGetPath(), pm);
	    	CheckPermissionsParams checkPermissionsParams = Structures.create(
	    		CheckPermissionsParams.class,
	    		Datatypes.member(CheckPermissionsParams.Member.principalName, userHome.refGetPath().getLastSegment().toClassicRepresentation())
	    	);
	    	pm.currentTransaction().begin();
			checkPermissionsResult = folder.checkPermissions(checkPermissionsParams);
			pm.currentTransaction().commit();
    	}
		FileDescr fd = new FileDescr();
		fd.setHash(this.toHash(folder.refGetPath().toXRI()));
		if(folder.getParent() != null && rootFolders != null && !rootFolders.contains(folder)) {
			fd.setPhash(this.toHash(folder.getParent().refGetPath().toXRI()));
		}
		fd.setName(folder.getName());
		fd.setMime("directory");
		fd.setSize(0);
		fd.setDate(folder.getModifiedAt());
		fd.setRead(true);
		fd.setWrite(checkPermissionsResult == null ? true : checkPermissionsResult.isHasUpdatePermission());
		fd.setRm(checkPermissionsResult == null ? true : checkPermissionsResult.isHasDeletePermission());
		// Check for subfolders
    	DocumentFolderQuery subFolderQuery = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
    	subFolderQuery.orderByName().ascending();
    	subFolderQuery.forAllDisabled().isFalse();		
		fd.setDirs(!folder.getSubFolder(subFolderQuery).isEmpty());
		return fd;
    }

    /**
     * Get fd for given document.
     * 
     * @param req
     * @param document
     * @return
     * @throws ServiceException
     */
	protected FileDescr stat(
    	HttpServletRequest req,
    	DocumentFolder folder,
    	Document document,
    	List<DocumentFolder> rootFolders,
    	boolean checkPermissions
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(document);
    	UserHome userHome = UserHomes.getInstance().getUserHome(document.refGetPath(), pm);
		CheckPermissionsResult checkPermissionsResult = null;
		if(this.checkPermissions && checkPermissions) {
	    	CheckPermissionsParams checkPermissionsParams = Structures.create(
        		CheckPermissionsParams.class,
        		Datatypes.member(
        			CheckPermissionsParams.Member.principalName, 
        			userHome.refGetPath().getLastSegment().toClassicRepresentation()
        		)
        	);
			checkPermissionsResult = document.checkPermissions(checkPermissionsParams);
		}
		MediaContent headRevision = document.getHeadRevision() instanceof MediaContent ? 
			((MediaContent)document.getHeadRevision()) 
			: null;
		FileDescr fd = new FileDescr();
		fd.setName(document.getName());
		fd.setHash(this.toHash(document.refGetPath().toXRI()));
		// Derive folder from document's folder assignment
		if(folder == null) {
			if(document.getFolder().size() == 1) {
				folder = document.<DocumentFolder>getFolder().get(0);
			} else {
				for(DocumentFolder f: document.<DocumentFolder>getFolder()) {
					DocumentFolder current = f;
					while(current != null) {
						if(rootFolders.contains(current)) {
							folder = current;
							break;
						}
						current = current.getParent();
					}
				}
			}
		}
		if(folder != null) {
			fd.setPhash(this.toHash(folder.refGetPath().toXRI()));
		}
		String mimeType = headRevision == null 
			? document.getContentType() 
			: headRevision.getContentMimeType();
		fd.setMime(mimeType == null || mimeType.isEmpty() ? "application/octet-stream" : mimeType);
		fd.setDate(headRevision == null ? document.getModifiedAt() : headRevision.getModifiedAt());
		try {
			fd.setSize(
				headRevision == null 
					? 0L
					: headRevision.getContentLength() == null 
						? headRevision.getContent().getLength()
						: headRevision.getContentLength()
			);
		} catch(Exception ignore) {}
		fd.setRead(true); // always true for objects returned by pm
		fd.setWrite(checkPermissionsResult == null ? true : checkPermissionsResult.isHasUpdatePermission());
		fd.setRm(checkPermissionsResult == null ? true : checkPermissionsResult.isHasDeletePermission());
		if(folder != null) {
			fd.setUrl(this.getDocumentUrl(req, folder, document, rootFolders, false));
		}
		return fd;
    }

    /**
     * Get fds for folder content.
     * 
     * @param req
     * @param folder
     * @return
     * @throws ServiceException
     */
    protected List<FileDescr> ls(
    	HttpServletRequest req,
    	DocumentFolder folder,
    	List<DocumentFolder> rootFolders,
    	boolean includeDocuments,
    	boolean includeFolders
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(folder);
    	List<FileDescr> fds = new ArrayList<FileDescr>();
    	if(includeFolders) {
	    	DocumentFolderQuery subFolderQuery = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
	    	subFolderQuery.orderByName().ascending();
	    	subFolderQuery.forAllDisabled().isFalse();
	    	for(DocumentFolder subFolder: folder.getSubFolder(subFolderQuery)) {
	    		fds.add(this.stat(req, subFolder, rootFolders, true));
	    	}
    	}
    	if(includeDocuments) {
    		String providerName = folder.refGetPath().getSegment(2).toClassicRepresentation();
    		String segmentName = folder.refGetPath().getSegment(4).toClassicRepresentation();
    		org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName);
    		DocumentQuery documentQuery = (DocumentQuery)pm.newQuery(Document.class);
    		documentQuery.orderByName().ascending();
    		documentQuery.forAllDisabled().isFalse();
    		documentQuery.thereExistsFolder().equalTo(folder);
	    	((Query)documentQuery).getFetchPlan().setFetchSize(FETCH_SIZE);
	    	for(Document document: documentSegment.getDocument(documentQuery)) {
	    		fds.add(this.stat(req, folder, document, rootFolders, true));
	    	}
    	}
    	return fds;
    }

    /**
     * Get root folders.
     * 
     * @param req
     * @param pm
     * @return
     */
    protected List<DocumentFolder> getRootFolders(
    	HttpServletRequest req,
    	PersistenceManager pm
    ) {
    	String path = req.getPathInfo();
		if(path.startsWith("/")) {
			path = path.substring(1);
		}
		if(path.endsWith("/")) {
			path = path.substring(0, path.length() - 1);
		}
		String[] components = path.split("/");
		// path is of the form {provider}/{segment}/{user}/{profile}
		UserHome userHome = (UserHome)pm.getObjectById(
			new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", components[0], "segment", components[1], "userHome", components[2])
		);
		List<DocumentFolder> rootFolders = new ArrayList<DocumentFolder>();
		String profileName = components[3];
    	DocumentProfileQuery documentProfileQuery = (DocumentProfileQuery)pm.newQuery(DocumentProfile.class);
    	documentProfileQuery.name().equalTo(profileName);
    	List<DocumentProfile> documentProfiles = userHome.getSyncProfile(documentProfileQuery);
    	if(!documentProfiles.isEmpty()) {
    		DocumentProfile documentProfile = documentProfiles.iterator().next();
    		DocumentFeedQuery documentFeedQuery = (DocumentFeedQuery)pm.newQuery(DocumentFeed.class);
    		documentFeedQuery.thereExistsIsActive().isTrue();
    		documentFeedQuery.thereExistsDocumentFolder().forAllDisabled().isFalse();
    		for(DocumentFeed documentFeed: documentProfile.<DocumentFeed>getFeed(documentFeedQuery)) {
    			rootFolders.add(documentFeed.getDocumentFolder());
    		}
    	}
    	return rootFolders;
    }


    /**
     * Get subfolders (recursively) for given folder.
     * 
     * @param folder
     * @return
     * @throws ServiceException
     */
    protected List<DocumentFolder> getAllSubFolders(
    	DocumentFolder folder
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(folder);
    	List<DocumentFolder> subFolders = new ArrayList<DocumentFolder>();
    	DocumentFolderQuery subFolderQuery = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
    	subFolderQuery.forAllDisabled().isFalse();
    	for(DocumentFolder subFolder: folder.getSubFolder(subFolderQuery)) {
    		subFolders.add(subFolder);
    		subFolders.addAll(this.getAllSubFolders(subFolder));
    	}
    	return subFolders;
    }

    /**
     * Get file descriptor for volume.
     * 
     * @param req
     * @return
     * @throws ServiceException
     */
    protected FileDescr getVolumeFileDescr(
    	HttpServletRequest req
    ) throws ServiceException {
		FileDescr fd = new FileDescr();
		String id = req.getPathInfo();
		fd.setHash(this.toHash(id));
		fd.setVolumeid(id);
		fd.setName(id);
		fd.setMime("directory");
		fd.setRead(true);
		fd.setWrite(false);
		fd.setRm(false);
		fd.setDate(new Date());
		return fd;
    }

    /**
     * Search documents matching query.
     * 
     * @param req
     * @param folder
     * @param rootFolders
     * @param query
     * @return
     * @throws ServiceException
     */
    protected List<FileDescr> search(
    	HttpServletRequest req,
    	DocumentFolder folder,
    	List<DocumentFolder> rootFolders,
    	String query
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(folder);
		String providerName = folder.refGetPath().getSegment(2).toClassicRepresentation();
		String segmentName = folder.refGetPath().getSegment(4).toClassicRepresentation();
		org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName);
		List<DocumentFolder> folders = new ArrayList<DocumentFolder>();
		folders.add(folder);
		folders.addAll(this.getAllSubFolders(folder));
		List<FileDescr> files = new ArrayList<FileDescr>();
		// Find documents with matching name
		{
			DocumentQuery documentQuery = (DocumentQuery)pm.newQuery(Document.class);
			documentQuery.forAllDisabled().isFalse();
			documentQuery.name().like("(?i).*" + query + ".*");
			documentQuery.thereExistsFolder().elementOf(folders);
			documentQuery.orderByName().ascending();
			for(Document document: documentSegment.getDocument(documentQuery)) {
				files.add(this.stat(req, null, document, rootFolders, true));
			}
		}
		// Find documents with matching index entry
		{
			UserHome userHome = UserHomes.getInstance().getUserHome(documentSegment.refGetPath(), pm);
			org.opencrx.kernel.home1.jmi1.SearchResult searchResult = null;
			try {
				pm.currentTransaction().begin();
				SearchBasicParams params = Structures.create(
					SearchBasicParams.class, 
					Datatypes.member(SearchBasicParams.Member.searchExpression, query)
				);
				searchResult = userHome.searchBasic(params);
				pm.currentTransaction().commit();
			} catch(Exception e) {
				try {
					pm.currentTransaction().rollback();
				} catch(Exception e0) {}
			}
			if((searchResult != null) && (searchResult.getObjectFinder() != null)) {
				ObjectFinder objectFinder = (ObjectFinder)pm.getObjectById(searchResult.getObjectFinder().refGetPath());
				IndexEntryQuery indexEntryQuery = (IndexEntryQuery)pm.newQuery(IndexEntry.class);
				// Restrict search result to folders
				QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(indexEntryQuery);
				String[] stringParams = new String[folders.size()];
				String clause = "EXISTS (SELECT 0 FROM OOCKE1_DOCUMENT_ d_ INNER JOIN OOCKE1_MEDIA m ON m.p$$parent = d_.object_id AND v.indexed_object = m.object_id AND (";
				for(int i = 0; i < folders.size(); i++) {
					clause += (i > 0 ? " OR " : "") + "d_.folder LIKE '%/" + folders.get(i).refGetPath().getLastSegment().toClassicRepresentation() + "'";
				}
				clause += "))";
				queryExtension.setStringParam(stringParams);
				queryExtension.setClause(clause);
				for(IndexEntry indexEntry: objectFinder.getIndexEntryDocument(indexEntryQuery)) {
					MediaContent mediaContent = (MediaContent)indexEntry.getIndexedObject();
					Document document = (Document)pm.getObjectById(mediaContent.refGetPath().getParent().getParent());
					if(!Boolean.TRUE.equals(document.isDisabled())) {
						files.add(this.stat(req, null, document, rootFolders, true));
					}
				}
			}
		}
		// Find documents with matching keywords
		{
			DocumentQuery documentQuery = (DocumentQuery)pm.newQuery(Document.class);
			documentQuery.forAllDisabled().isFalse();
			documentQuery.thereExistsKeywords().like("(?i).*" + query + ".*");
			documentQuery.thereExistsFolder().elementOf(folders);
			documentQuery.orderByName().ascending();
			for(Document document: documentSegment.getDocument(documentQuery)) {
				files.add(this.stat(req, null, document, rootFolders, true));
			}
		}
		// Find folders matching query
		for(DocumentFolder f: folders) {
			if(f.getName().matches("(?i).*" + query + ".*")) {
				files.add(this.stat(req, f, rootFolders, true));
			}
		}
		return files;
    }
    
    /**
     * Get tree starting from given folder.
     *  
     * @param req
     * @param folder
     * @param rootFolders
     * @return
     * @throws ServiceException
     */
    protected List<FileDescr> getTree(
    	HttpServletRequest req,
    	DocumentFolder folder,
    	List<DocumentFolder> rootFolders
    ) throws ServiceException {
		List<FileDescr> tree = new ArrayList<FileDescr>();
		DocumentFolder current = folder;
		while(current != null) {
			tree.addAll(this.ls(req, current, rootFolders, false, true));
			if(rootFolders.contains(current)) break;
			current = current.getParent();
		}
		for(DocumentFolder rootFolder: rootFolders) {
			tree.add(this.stat(req, rootFolder, rootFolders, true));
		}
		return tree;
    }

    /**
     * mkdir command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void mkdirCommand(
    	HttpServletRequest req,
    	HttpServletResponse resp,
    	Map<String,String[]> requestParams,
    	PersistenceManager pm
    ) throws ServiceException {
    	String[] targets = requestParams.get("target");
    	String[] name = requestParams.get("name");
    	if(targets != null && targets.length > 0 && name != null && name.length > 0) {
    		org.opencrx.kernel.document1.jmi1.DocumentFolder folder = (org.opencrx.kernel.document1.jmi1.DocumentFolder)pm.getObjectById(new Path(this.fromHash(targets[0])));
    		List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
			DocumentFolderQuery subFolderQuery = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
			subFolderQuery.name().equalTo(name[0]);
			subFolderQuery.forAllDisabled().isFalse();
			List<DocumentFolder> folders = folder.getSubFolder(subFolderQuery);
			DocumentFolder newFolder = null;
			if(folders.isEmpty()) {
				org.opencrx.kernel.document1.jmi1.Segment documentSegment = 
					(org.opencrx.kernel.document1.jmi1.Segment)pm.getObjectById(
						folder.refGetPath().getPrefix(5)
					);
				pm.currentTransaction().begin();
				newFolder = pm.newInstance(DocumentFolder.class);
				newFolder.setName(name[0]);
				newFolder.setParent(folder);
				newFolder.getOwningGroup().addAll(folder.getOwningGroup());
				documentSegment.addFolder(
					Utils.getUidAsString(),					
					newFolder
				);
				pm.currentTransaction().commit();
			} else {
				newFolder = folders.iterator().next();
			}
			FilesOpResult mkdirResult = new FilesOpResult();
			mkdirResult.setAdded(Arrays.asList(this.stat(req, newFolder, rootFolders, true)));
	    	this.sendResponse(req, resp, mkdirResult);
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");
    	}
	}

    /**
     * tree command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void treeCommand(
    	HttpServletRequest req,
    	HttpServletResponse resp,
    	Map<String,String[]> requestParams,
    	PersistenceManager pm
    ) throws ServiceException {
    	String[] targets = requestParams.get("target");
    	if(targets != null && targets.length > 0) {
    		String target = "".equals(targets[0]) ? "" : this.fromHash(targets[0]);
    		DocumentFolder folder = (DocumentFolder)pm.getObjectById(new Path(target));
    		List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
    		List<FileDescr> tree = this.getTree(req, folder, rootFolders);
    		TreeResult treeDescr = new TreeResult();
    		treeDescr.setTree(tree);
    		this.sendResponse(req, resp, treeDescr);
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");
    	}
    }

    /**
     * open command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void openCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] targets = requestParams.get("target");
    	boolean isTree = this.isSet(requestParams.get("tree"));
    	boolean isInit = this.isSet(requestParams.get("init"));
    	if(targets != null && targets.length > 0) {
    		List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
    		FileDescr cwd = null;
    		List<FileDescr> files = new ArrayList<FileDescr>();
    		String target = "".equals(targets[0]) ? "" : this.fromHash(targets[0]);
    		DocumentFolder folder = null;
    		if("".equals(target) || req.getPathInfo().equals(target)) {
    			cwd = this.getVolumeFileDescr(req);
    			for(DocumentFolder rootFolder: rootFolders) {
    				files.add(this.stat(req, rootFolder, rootFolders, false));
    			}
    		} else {
    			folder = (DocumentFolder)pm.getObjectById(new Path(target));
    			cwd = this.stat(req, folder, rootFolders, true);
    			files.addAll(this.ls(req, folder, rootFolders, true, true));
    			if(isTree) {
    				// Tree starting from parent
    				files.addAll(this.getTree(req, folder.getParent(), rootFolders));
				}
			}
    		OpenResult openDescr = new OpenResult();
    		openDescr.setCwd(cwd);
    		openDescr.setFiles(files);
    		if(isInit) {
    			openDescr.setNetDrivers(Collections.<String>emptyList());
    			openDescr.setApi(2);
    		}
    		this.sendResponse(req, resp, openDescr);
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");
    	}
    }

    /**
     * ls command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void lsCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] targets = requestParams.get("target");
    	if(targets != null && targets.length > 0) {
    		DocumentFolder targetFolder = (DocumentFolder)pm.getObjectById(new Path(this.fromHash(targets[0])));
    		List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
    		List<FileDescr> files = this.ls(req, targetFolder, rootFolders, true, false);
    		LsResult lsResult = new LsResult();
    		lsResult.setList(files);
    		this.sendResponse(req, resp, lsResult);
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");    		
    	}
    }
    
    /**
     * Search command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void searchCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] q = requestParams.get("q");
    	if(q != null && q.length > 0) {
    		String query = q[0];
    		List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
    		List<FileDescr> files = null;
    		if(query.length() >= 3 && !rootFolders.isEmpty()) {
    			files = new ArrayList<FileDescr>();
    			for(DocumentFolder folder: rootFolders) {
    				files.addAll(this.search(req, folder, rootFolders, query));
    			}
    		} else {
    			files = Collections.emptyList();
    		}
    		FilesResult searchResult = new FilesResult();
    		searchResult.setFiles(files);
    		this.sendResponse(req, resp, searchResult);
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");    		
    	}
    }

    /**
     * Parents command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void parentsCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] targets = requestParams.get("target");
    	if(targets != null && targets.length > 0) {
    		String target = "".equals(targets[0]) ? "" : this.fromHash(targets[0]);
    		HttpSession session = req.getSession();
    		if(target.equals(session.getAttribute("parents.target"))) {
    			// Return errOpen to prevent end-less loop in ui/tree.js
    			this.sendErrorResponse(req, resp, "errOpen");
    		} else {
    			session.setAttribute("parents.target", target);
				List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
	    		List<FileDescr> parents = new ArrayList<FileDescr>();
	    		if(!"".equals(target) && !req.getPathInfo().equals(target)) {
	    			DocumentFolder folder = (DocumentFolder)pm.getObjectById(new Path(target));
					DocumentFolder current = folder.getParent();
					// All parents and their sub folders
					while(current != null && !rootFolders.contains(current)) {
						parents.addAll(this.ls(req, current, rootFolders, false, true));
						current = current.getParent();
					}
	    		}
				// All root folders and their sub folders
				for(DocumentFolder rootFolder: rootFolders) {
					parents.add(this.stat(req, rootFolder, rootFolders, true));
				}
	    		TreeResult treeResult = new TreeResult();
	    		treeResult.setTree(parents);
	    		this.sendResponse(req, resp, treeResult);
    		}
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen"); 		
    	}
    }

    /**
     * Rm command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void rmCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] targets = requestParams.get("targets[]");
    	if(targets != null) {
    		List<String> removed = new ArrayList<String>();
    		for(String target: targets) {
    			RefObject_1_0 targetObj = (RefObject_1_0)pm.getObjectById(new Path(this.fromHash(target)));
    			if(targetObj instanceof DocumentFolder) {
    				DocumentFolder folder = (DocumentFolder)targetObj;
    				try {
    					pm.currentTransaction().begin();
    					folder.setDisabled(true);
    					pm.currentTransaction().commit();
    					removed.add(this.toHash(folder.refGetPath().toXRI()));
    				} catch(Exception e) {
    					try {
    						pm.currentTransaction().rollback();
    					} catch(Exception ignore) {}
    				}
    			} else if(targetObj instanceof Document) {
    				Document document = (Document)targetObj;
    				try {
    					pm.currentTransaction().begin();
    					document.setDisabled(true);
    					pm.currentTransaction().commit();
    					removed.add(this.toHash(document.refGetPath().toXRI()));
    				} catch(Exception e) {
    					try {
    						pm.currentTransaction().rollback();
    					} catch(Exception ignore) {}
    				}
    			}
    		}
    		FilesOpResult rmResult = new FilesOpResult();
    		rmResult.setRemoved(removed);
    		this.sendResponse(req, resp, rmResult);
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");    		
    	}
    }

    /**
     * Rename command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void renameCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] targets = requestParams.get("target");
    	String[] name = requestParams.get("name");
    	if(targets != null && targets.length > 0 && name != null && name.length > 0) {
			RefObject_1_0 targetObj = (RefObject_1_0)pm.getObjectById(new Path(this.fromHash(targets[0])));
			String newName = name[0];
			if(targetObj instanceof DocumentFolder) {
				DocumentFolder folder = (DocumentFolder)targetObj;
				try {
					pm.currentTransaction().begin();
					folder.setName(newName);
					pm.currentTransaction().commit();
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
				}    				
			} else if(targetObj instanceof Document) {
				Document document = (Document)targetObj;
				try {
					pm.currentTransaction().begin();
					document.setName(newName);
					document.setTitle(newName);
					if(document.getHeadRevision() != null) {
						DocumentRevision headRevision = document.getHeadRevision();
						headRevision.setName(newName);
						if(headRevision instanceof MediaContent) {
							((MediaContent)headRevision).setContentName(newName);
						}
					}
					pm.currentTransaction().commit();
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
				}
			}
			FilesOpResult renameResult = new FilesOpResult();
    		// Renames only (no files removed, no files added) 
    		renameResult.setAdded(Collections.<FileDescr>emptyList());
    		renameResult.setRemoved(Collections.<String>emptyList());
    		this.sendResponse(req, resp, renameResult);
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");    		
    	}
    }

    /**
     * File command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void fileCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] targets = requestParams.get("target");
		boolean isDownload = this.isSet(requestParams.get("download"));
    	if(targets != null && targets.length > 0) {
			Document document = (Document)pm.getObjectById(new Path(this.fromHash(targets[0])));
			if(document.getHeadRevision() instanceof MediaContent) {
				try {
					List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
					MediaContent headRevision = (MediaContent)document.getHeadRevision();
					FileDescr fd = this.stat(req, null, document, rootFolders, false);
					boolean isMedia =
						fd.getMime().startsWith("image/") ||
						fd.getMime().startsWith("audio/") || 
						fd.getMime().startsWith("video/");
					if(isMedia) {
						OutputStream os = resp.getOutputStream();
						resp.setStatus(HttpServletResponse.SC_OK);
						resp.setContentType(fd.getMime());
						resp.setHeader("Content-disposition", (isDownload ? "attachment; " : "inline; ") + "filename=\"" + MimeUtility.encodeText(document.getName() + "\""));            
						long length = BinaryLargeObjects.streamCopy(headRevision.getContent().getContent(), 0L, os);
						resp.setContentLength((int)length);
					} else {
						// Redirect to download
						DocumentFolder folder = (DocumentFolder)pm.getObjectById(new Path(this.fromHash(fd.getPhash())));
						resp.sendRedirect(this.getDocumentUrl(req, folder, document, rootFolders, true));
					}
				} catch(Exception e) {
					SysLog.warning("Error sending file", e.getCause());
					try {
						this.sendErrorResponse(req, resp, e.getMessage());
					} catch(Exception ignore) {}
				}
			} else {
	    		this.sendErrorResponse(req, resp, "errOpen");				
			}
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");    		
    	}
    }

    /**
     * Upload command.
     * 
     * @param req
     * @param resp
     * @param requestParams
     * @param pm
     * @throws ServiceException
     */
    protected void uploadCommand(
		HttpServletRequest req,
		HttpServletResponse resp,
		Map<String,String[]> requestParams,
		PersistenceManager pm
	) throws ServiceException {
    	String[] targets = requestParams.get("target");
    	String[] uploads = requestParams.get("upload[]");
    	if(targets != null && targets.length > 0 && uploads != null && uploads.length > 0) {
			DocumentFolder folder = (DocumentFolder)pm.getObjectById(new Path(this.fromHash(targets[0])));
			String providerName = folder.refGetPath().getSegment(2).toClassicRepresentation();
			String segmentName = folder.refGetPath().getSegment(4).toClassicRepresentation();
			org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, providerName, segmentName);
			List<DocumentFolder> rootFolders = this.getRootFolders(req, pm);
			List<FileDescr> added = new ArrayList<FileDescr>();
			for(int i = 0; i < uploads.length; i++) {
				try {
					List<String> principalChain = UserObjects.getPrincipalChain(pm);
					String author = null;
					if(principalChain != null) {
						author = principalChain.size() == 1 ? principalChain.get(0) : principalChain.toString();
					}
					String name = uploads[i];
					String resourceLocation = this.getTempFileName(req, "upload[]", Integer.toString(i + 1));
					BufferedReader r = new BufferedReader(
						new InputStreamReader(
							new FileInputStream(resourceLocation + ".INFO"),
							"UTF-8"
						)
					);
					String contentMimeType = r.readLine();
					String contentName = r.readLine();
					r.close();
					// Add document
					DocumentBasedFolderEntryQuery folderEntryQuery = (DocumentBasedFolderEntryQuery)pm.newQuery(DocumentBasedFolderEntry.class);
					folderEntryQuery.forAllDisabled().isFalse();
					folderEntryQuery.name().equalTo(name);
					Document document = null;
					List<DocumentBasedFolderEntry> folderEntries = folder.getFolderEntry(folderEntryQuery);
					if(folderEntries.isEmpty()) {
						pm.currentTransaction().begin();
						document = pm.newInstance(Document.class);
						document.setName(name);
						document.setAuthor(author);
						document.setActiveOn(new Date());
						document.setContentType(contentMimeType);
						document.getFolder().add(folder);
						document.getOwningGroup().addAll(folder.getOwningGroup());
						documentSegment.addDocument(
							Utils.getUidAsString(),
							document
						);
						pm.currentTransaction().commit();
					} else {
						document = (Document)folderEntries.iterator().next().getDocument();
					}
					// Add revision
					pm.currentTransaction().begin();
					Documents.getInstance().addRevision(
						document, 
						contentName, 
						contentMimeType, 
						author, 
						BinaryLargeObjects.valueOf(new File(resourceLocation))
					);
					pm.currentTransaction().commit();
					added.add(this.stat(req, folder, document, rootFolders, false));
				} catch(Exception e) {
					new ServiceException(e).log();
					this.sendErrorResponse(req, resp, e.getMessage());
				}
			}
			FilesOpResult uploadResult = new FilesOpResult();
    		uploadResult.setAdded(added);
    		this.sendResponse(req, resp, uploadResult);			
    	} else {
    		this.sendErrorResponse(req, resp, "errOpen");    		
    	}
    }

    /**
     * Handle request.
     * 
     * @param req
     * @param resp
     * @throws ServletException
     * @throws IOException
     */
    protected void handleRequest(
        HttpServletRequest req, 
        HttpServletResponse resp
    ) throws ServletException, IOException {
    	PersistenceManager pm = null;
    	@SuppressWarnings("unused")
		HttpSession session = req.getSession(true);
    	try {
    		pm = this.getPersistenceManager(req);
    		req.setCharacterEncoding("UTF-8");
	    	Map<String,String[]> requestParams = this.parseRequest(req);
	    	String[] command = requestParams.get("cmd");
	    	if(command != null && command.length > 0) {
	    		String cmd = command[0];
	    		if("mkdir".equalsIgnoreCase(cmd)) {
	    			this.mkdirCommand(req, resp, requestParams, pm);
	    		} else if("open".equalsIgnoreCase(cmd)) {
	    			this.openCommand(req, resp, requestParams, pm);
	    		} else if("rename".equalsIgnoreCase(cmd)) {
	    			this.renameCommand(req, resp, requestParams, pm);
	    		} else if("rm".equalsIgnoreCase(cmd)) {
	    			this.rmCommand(req, resp, requestParams, pm);
	    		} else if("parents".equalsIgnoreCase(cmd)) {
	    			this.parentsCommand(req, resp, requestParams, pm);
	    		} else if("tree".equalsIgnoreCase(cmd)) {
	    			this.treeCommand(req, resp, requestParams, pm);	    			
	    		} else if("ls".equalsIgnoreCase(cmd)) {
	    			this.lsCommand(req, resp, requestParams, pm);
	    		} else if("file".equalsIgnoreCase(cmd)) {
	    			this.fileCommand(req, resp, requestParams, pm);
	    		} else if("upload".equalsIgnoreCase(cmd)) {
	    			this.uploadCommand(req, resp, requestParams, pm);
	    		} else if("search".equalsIgnoreCase(cmd)) {
	    			this.searchCommand(req, resp, requestParams, pm);
	    		} else {
	    			this.sendErrorResponse(req, resp, "Unknown Command");
	    		}
	    	} else {
	    		this.sendErrorResponse(req, resp, "Unknown Command");
	    	}
    	} catch(Exception e) {
    		new ServiceException(e).log();
    		try {
    			this.sendErrorResponse(req, resp, e.getMessage());
    		} catch(Exception ignore) {}
    	} finally {
    		if(pm != null) {
    			try {
    				pm.close();
    			} catch(Exception ignore) {}    				
    		}
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
		this.handleRequest(req, resp);
	}

	/* (non-Javadoc)
	 * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
	 */
	@Override
	protected void doPost(
		HttpServletRequest req, 
		HttpServletResponse resp
	) throws ServletException, IOException {
		this.handleRequest(req, resp);
	}

	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	private static final long serialVersionUID = 1206167904673263531L;
	
	protected static final int FETCH_SIZE = 200;
	protected File tempdir = null;
	protected boolean isDebug = false;
	protected boolean checkPermissions = false;
    protected PersistenceManagerFactory pmf = null;
    
}
