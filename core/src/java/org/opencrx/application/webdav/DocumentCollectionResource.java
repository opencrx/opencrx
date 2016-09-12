/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AccountCollectionResource
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.webdav;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Set;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.kernel.document1.cci2.DocumentBasedFolderEntryQuery;
import org.opencrx.kernel.document1.cci2.DocumentFolderQuery;
import org.opencrx.kernel.document1.cci2.DocumentQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentBasedFolderEntry;
import org.opencrx.kernel.document1.jmi1.DocumentFilterGlobal;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.generic.ChainingCollection;
import org.openmdx.base.collection.MarshallingCollection;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.marshalling.Marshaller;

public abstract class DocumentCollectionResource extends WebDavResource {
	
	/**
	 * DocumentResourceCollectionBasedOnFolderEntry
	 *
	 */
	static class DocumentResourceCollectionBasedOnFolderEntries extends MarshallingCollection<Resource> {
		
		public DocumentResourceCollectionBasedOnFolderEntries(
			final RequestContext requestContext,
			Collection<DocumentBasedFolderEntry> entries,
			final DocumentCollectionResource parentCollection
		) {
			super(
				new Marshaller(){

					@Override
                    public Object marshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof DocumentBasedFolderEntry) {
							return new DocumentResource(
								requestContext,
								(Document)((DocumentBasedFolderEntry)source).getDocument(),
								parentCollection
							);
						} else {
							return source;
						}
                    }

					@Override
                    public Object unmarshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof WebDavResource) {
							return ((WebDavResource)source).getObject();
						}
						else {
							return source;
						}
                    }
					
				},
				entries
			);
		}
		
        private static final long serialVersionUID = 6257982279508324945L;

	}

	/**
	 * DocumentResourceCollectionBasedOnDocument
	 *
	 */
	static class DocumentResourceCollectionBasedOnDocuments extends MarshallingCollection<Resource> {
		
		public DocumentResourceCollectionBasedOnDocuments(
			final RequestContext requestContext,
			Collection<Document> entries,
			final DocumentCollectionResource parentCollection
		) {
			super(
				new Marshaller(){

					@Override
                    public Object marshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof Document) {
							return new DocumentResource(
								requestContext,
								(Document)source,
								parentCollection
							);
						} else {
							return source;
						}
                    }

					@Override
                    public Object unmarshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof WebDavResource) {
							return ((WebDavResource)source).getObject();
						}
						else {
							return source;
						}
                    }
					
				},
				entries
			);
		}

        private static final long serialVersionUID = -4267414630873757556L;

	}
	
	/**
	 * DocumentFolderResourceCollection
	 *
	 */
	static class DocumentFolderResourceCollection extends MarshallingCollection<Resource> {

		public DocumentFolderResourceCollection(
			final RequestContext requestContext,
			Collection<DocumentFolder> entries
		) {
			super(
				new Marshaller(){

					@Override
                    public Object marshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof DocumentFolder) {
							return new DocumentFolderResource(
								requestContext,
								(DocumentFolder)source
							);
						} else {
							return source;
						}
                    }

					@Override
                    public Object unmarshal(
                    	Object source
                    ) throws ServiceException {
						if(source instanceof WebDavResource) {
							return ((WebDavResource)source).getObject();
						}
						else {
							return source;
						}
                    }
					
				},
				entries
			);
		}
		
        private static final long serialVersionUID = 6284983517339059654L;

	}
	
	/**
	 * Constructor.
	 * 
	 * @param requestContext
	 * @param documentFolder
	 */
	public DocumentCollectionResource(
		RequestContext requestContext,
		DocumentFolder documentFolder
	) {
		super(
			requestContext,
			documentFolder
		);
	}

	/**
	 * Constructor.
	 * 
	 * @param requestContext
	 * @param documentFilter
	 */
	public DocumentCollectionResource(
		RequestContext requestContext,
		DocumentFilterGlobal documentFilter
	) {
		super(
			requestContext,
			documentFilter
		);
	}

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.Resource#getDisplayName()
	 */
	@Override
    public String getDisplayName(
    ) {
    	Set<String> features = this.getObject().refDefaultFetchGroup();
    	String name = this.getName();
    	if(features.contains("name")) {
    		name = (String)this.getObject().refGetValue("name");
    	}
    	return name;
    }
    		
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.Resource#isCollection()
	 */
	@Override
    public boolean isCollection(
    ) {
		return true;
    }
	
    /* (non-Javadoc)
     * @see org.opencrx.application.webdav.WebDavResource#getName()
     */
    @Override
    public String getName(
    ) {
    	BasicObject documentCollection = this.getObject();
    	if(documentCollection instanceof DocumentFolder) {
    		return ((DocumentFolder)documentCollection).getName();
    	} else if(documentCollection instanceof DocumentFilterGlobal) {
    		return ((DocumentFilterGlobal)documentCollection).getName();
    	} else {
    		return "NA";
    	}
    }

	/**
	 * Get query for retrieving folder entries.
	 * 
	 * @return
	 */
	public DocumentBasedFolderEntryQuery getFolderEntryQuery(
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(this.getObject());
		DocumentBasedFolderEntryQuery query = (DocumentBasedFolderEntryQuery)pm.newQuery(DocumentBasedFolderEntry.class);
        query.forAllDisabled().isFalse();                    
        query.orderByCreatedAt().ascending();
        return query;
	}

	/**
	 * Get query for retrieving document folders.
	 * 
	 * @return
	 */
	public DocumentFolderQuery getFolderQuery(
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(this.getObject());
	    DocumentFolderQuery query = (DocumentFolderQuery)pm.newQuery(DocumentFolder.class);
	    query.forAllDisabled().isFalse();
	    return query;
	}

	/**
	 * Get query for retrieving documents.
	 * 
	 * @return
	 */
	public DocumentQuery getDocumentQuery(
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(this.getObject());
	    DocumentQuery query = (DocumentQuery)pm.newQuery(Document.class);
	    query.forAllDisabled().isFalse();
	    query.orderByCreatedAt().ascending();
	    return query;
	}

    /* (non-Javadoc)
     * @see org.opencrx.application.webdav.WebDavResource#getChildren()
     */
    @Override
    @SuppressWarnings("unchecked")
	public Collection<Resource> getChildren(
		Date timeRangeStart,
		Date timeRangeEnd
	) {
    	BasicObject documentCollection = this.getObject();
    	if(documentCollection instanceof DocumentFolder) {
    		DocumentFolder documentFolder = (DocumentFolder)documentCollection;
			DocumentBasedFolderEntryQuery folderEntryQuery = this.getFolderEntryQuery();
			MarshallingCollection<Resource> documentResourceCollection = new DocumentResourceCollectionBasedOnFolderEntries(
	        	this.getRequestContext(),
	        	documentFolder.<DocumentBasedFolderEntry>getFolderEntry(folderEntryQuery),
	        	this
	        );
	        DocumentFolderQuery folderQuery = this.getFolderQuery();
	        MarshallingCollection<Resource> documentFolderCollection = new DocumentFolderResourceCollection(
	        	this.getRequestContext(),
	        	documentFolder.getSubFolder(folderQuery)
	        );
	        return new ChainingCollection<Resource>(
	        	documentFolderCollection,
	        	documentResourceCollection
	        );
    	} else if(documentCollection instanceof DocumentFilterGlobal) {
    		DocumentFilterGlobal documentFilterGlobal = (DocumentFilterGlobal)documentCollection;
    		DocumentQuery documentQuery = this.getDocumentQuery();
    		MarshallingCollection<Resource> documentResourceCollection = new DocumentResourceCollectionBasedOnDocuments(
    			this.getRequestContext(),
    			documentFilterGlobal.<Document>getFilteredDocument(documentQuery),
    			this
    		);
    		return documentResourceCollection;
    	} else {
    		return Collections.emptyList();
    	}
	}

	//-----------------------------------------------------------------------
    // Members
	//-----------------------------------------------------------------------
	
}