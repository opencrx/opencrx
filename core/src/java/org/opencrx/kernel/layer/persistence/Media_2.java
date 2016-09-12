/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Media_2 plug-in
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
package org.opencrx.kernel.layer.persistence;

import java.io.File;
import java.io.FileOutputStream;
import java.util.List;

import javax.resource.ResourceException;
import javax.resource.cci.Connection;
import javax.resource.cci.Interaction;
import javax.resource.cci.MappedRecord;

import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.FileUtils;
import org.openmdx.application.dataprovider.cci.AttributeSelectors;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.Database_2;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.spi.PersistenceManagers;
import org.openmdx.base.resource.InteractionSpecs;
import org.openmdx.base.resource.Records;
import org.openmdx.base.resource.spi.ResourceExceptions;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.RestConnection;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * This plugin allows to store media objects on the file system.
 */
public class Media_2 extends Database_2 {

	/**
	 * Constructor.
	 * 
	 */
	public Media_2(
	) {
		super();
	}
	
    /* (non-Javadoc)
     * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1#getInteraction(javax.resource.cci.Connection)
     */
	@Override
    public Interaction getInteraction(
        RestConnection connection
    ) throws ResourceException {
        return new RestInteraction(connection);
    }

	/**
	 * Get media dir.
	 * 
	 * @param path
	 * @return
	 */
	public static File getMediaDir(
		Path path
	) {
		String dir = System.getProperty("org.opencrx.mediadir." + path.getSegment(2).toClassicRepresentation());
		if(dir != null) {
			File mediadir = new File(dir);
			if(!mediadir.exists()) {
				SysLog.warning("mediadir does not exist. Ignoring", mediadir);
			} else {
				return mediadir;
			}
		}
		return null;		
	}

	/**
	 * Map object identity to content dir.
	 * 
	 * @param name
	 * @return
	 */
	public static File toContentDir(
		File mediadir,
		Path path
	) {
		return new File(
			mediadir, 
			path.isContainerPath() 
				? path.toClassicRepresentation().replace("::", "_") 
				: path.getParent().toClassicRepresentation().replace("::", "_")
			);
	}

    protected List<String> getPrincipalChain(
    	Connection connection
    ) throws ResourceException {
    	return PersistenceManagers.toPrincipalChain(connection.getMetaData().getUserName());
    }

    /**
     * LayerInteraction
     *
     */
    public class RestInteraction extends Database_2.RestInteraction {
        
        /**
         * Constructor.
         * 
         * @param connection
         * @throws ResourceException
         */
        public RestInteraction(
        	RestConnection connection
        ) throws ResourceException {
            super(connection);
        }

        /**
         * Create new result record.
         * 
         * @return
         * @throws ResourceException
         */
        protected ResultRecord newResult(
        ) throws ResourceException {
        	return Records.getRecordFactory().createIndexedRecord(ResultRecord.class);
        }
        
        /**
         * Create new operation result.
         * 
         * @param recordType
         * @return
         * @throws ResourceException
         */
        protected MappedRecord newOperationResult(
        	String recordType
        ) throws ResourceException { 
		    return Records.getRecordFactory().createMappedRecord(recordType);
        }

        /**
         * Retrieve object with given identity.
         * 
         * @param resourceIdentifier
         * @param fetchGroupName
         * @return
         * @throws ResourceException
         */
        public ObjectRecord retrieveObject(
        	Path resourceIdentifier,
        	String fetchGroupName
        ) throws ResourceException {
        	ResultRecord result = this.newResult();
        	QueryRecord query = this.newQuery(resourceIdentifier);
        	query.setFetchGroupName(fetchGroupName);
        	super.get(
        		SUPER.GET, 
        		query, 
        		result
        	);
        	return result.isEmpty() ? null : (ObjectRecord)result.get(0);
        }

        /**
         * Store content.
         * 
         * @param contentFile
         * @param binaryContent
         * @throws ServiceException
         */
        protected void storeContent(
        	File contentFile,
        	BinaryLargeObject binaryContent
        ) throws ServiceException {
			try {
				FileOutputStream target = new FileOutputStream(contentFile);
				BinaryLargeObjects.streamCopy(binaryContent.getContent(), 0L, target);
				target.close();
			} catch(Exception e) {
				throw new ServiceException(e);
			}
        }

        /**
         * Set content.
         * 
         * @param object
         * @throws ServiceException
         */
        protected void setContent(
        	Object_2Facade object
        ) throws ServiceException {
        	File mediadir = Media_2.getMediaDir(object.getPath());        	
        	if(mediadir != null) {
        		Object content = (BinaryLargeObject)object.attributeValue("content");
        		File contentDir = Media_2.toContentDir(mediadir, object.getPath());
    			File contentFile = new File(contentDir, object.getPath().getLastSegment().toClassicRepresentation());
    			// Copy content silently from database to mediadir
    			if(!contentFile.exists() && content instanceof BinaryLargeObject) {
    				contentDir.mkdirs();
    				this.storeContent(contentFile, (BinaryLargeObject)content);
    			}
    			// Get content from contentFile
    			if(contentFile.exists()) {
    				object.clearAttributeValuesAsList("content");
    				object.addToAttributeValuesAsList("content", BinaryLargeObjects.valueOf(contentFile));
    				object.clearAttributeValuesAsList("contentLength");
    				object.addToAttributeValuesAsList("contentLength", contentFile.length());
    			}
        	}
        }

        /**
         * Return true if current user is ROOT_PRINCIPAL.
         * 
         * @param header
         * @return
         */
        protected boolean currentUserIsRoot(
        	Connection connection
        ) throws ResourceException {
        	List<String> principalChain = Media_2.this.getPrincipalChain(connection);
        	return principalChain.isEmpty() 
        		? false 
        		: SecurityKeys.ROOT_PRINCIPAL.equals(principalChain.get(0)); 
        }

	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1.LayerInteraction#get(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @Override
        public boolean get(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {	    	
	    	try {
	            Model_1_0 model = Model_1Factory.getModel();
	            super.get(ispec, request, response);
	            for(Object object: response) {
	            	if(object instanceof ObjectRecord) {
	            		Object_2Facade record = Facades.asObject((ObjectRecord)object);
	                    if(model.isSubtypeOf(record.getObjectClass(), "org:opencrx:kernel:document1:Media")) {
	                    	this.setContent(record);
	                    }
	            	}
	            }
	            return true;
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	}
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1.LayerInteraction#find(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @Override
        public boolean find(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
	    	try {
	            Model_1_0 model = Model_1Factory.getModel();
	            super.find(ispec, request, response);
	            for(Object object: response) {
	            	short attributeSelector = AttributeSelectors.getAttributeSelector(request);
	            	if(object instanceof MappedRecord) {
	            		Object_2Facade record = Facades.asObject((MappedRecord)object);
	                    if(model.isSubtypeOf(record.getObjectClass(), "org:opencrx:kernel:document1:Media")) {
	                    	if(attributeSelector == AttributeSelectors.ALL_ATTRIBUTES) {
	                    		this.setContent(record);
	                    	}
	                    }
	            	}
	            }
	            return true;
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	}
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1.LayerInteraction#delete(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @Override
        public boolean delete(
            RestInteractionSpec ispec, 
            ObjectRecord request
        ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
		    super.delete(ispec, request);
		    File mediadir = Media_2.getMediaDir(path);
		    if(mediadir != null) {
        		File contentDir = Media_2.toContentDir(mediadir,path);
    			File contentFile = new File(contentDir, path.getLastSegment().toClassicRepresentation());        			
		    	if(contentFile.exists()) {
		    		try {
		    			if(contentFile.isDirectory()) {
		    				FileUtils.deleteDirectory(contentFile);
		    			} else {
		    				contentFile.delete();
		    			}
		    		} catch(Exception e) {
		    			new ServiceException(e).log();
		    		}
		    	}
		    	// Remove empty dirs
		    	while(
		    		!contentDir.equals(mediadir) &&
		    		contentDir.isDirectory() &&
		    		contentDir.listFiles().length == 0
		    	) {
		    		try {
		    			contentDir.delete();
		    		} catch(Exception e) {
		    			new ServiceException(e).log();
		    		}
		    		contentDir = contentDir.getParentFile();
		    	}
		    }
	        return true;
	    }

		/* (non-Javadoc)
		 * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1.LayerInteraction#create(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, javax.resource.cci.IndexedRecord)
		 */
		@Override
        public boolean create(
            RestInteractionSpec ispec, 
            ObjectRecord request,
            ResultRecord response
        ) throws ResourceException {
			try {
				Path path = request.getResourceIdentifier();
	            Model_1_0 model = Model_1Factory.getModel();
	            File mediadir = Media_2.getMediaDir(path);
	            Object_2Facade input = Facades.asObject(request);
	            if(model.isSubtypeOf(input.getObjectClass(), "org:opencrx:kernel:document1:Media")) {
	            	if(mediadir != null) {
	            		Object content = (BinaryLargeObject)input.attributeValue("content");
	            		File contentDir = Media_2.toContentDir(mediadir, path);
	            		contentDir.mkdirs();
	        			File contentFile = new File(contentDir, input.getPath().getLastSegment().toClassicRepresentation());        			
	            		if(content instanceof BinaryLargeObject) {
	            			this.storeContent(contentFile, (BinaryLargeObject)content);
	            			// admin-Root stores media content always on database
	        				input.attributeValuesAsList("content").clear();            				
	            			if(this.currentUserIsRoot(this.getConnection())) {
	            				input.addToAttributeValuesAsList("content", BinaryLargeObjects.valueOf(contentFile));            				
	            			}
	            		}
	            	}
	            }
				return super.create(ispec, request, response);
			} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );				
			}
		}

		/* (non-Javadoc)
		 * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1.LayerInteraction#put(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, javax.resource.cci.IndexedRecord)
		 */
		@Override
	    public boolean update(
	        RestInteractionSpec ispec, 
	        ObjectRecord request, 
	        ResultRecord response
	    ) throws ResourceException {
			try {
				Path path = request.getResourceIdentifier();
	            Model_1_0 model = Model_1Factory.getModel();
	            File mediadir = Media_2.getMediaDir(path);
	            Object_2Facade input = Facades.asObject(request);
	            if(model.isSubtypeOf(input.getObjectClass(), "org:opencrx:kernel:document1:Media")) {
	            	if(mediadir != null) {
	            		Object content = (BinaryLargeObject)input.attributeValue("content");
	            		File contentDir = Media_2.toContentDir(mediadir, input.getPath());
	        			File contentFile = new File(contentDir, input.getPath().getLastSegment().toClassicRepresentation());        			
	            		if(content instanceof BinaryLargeObject) {
	            			input.attributeValuesAsList("content").clear();
	            			// Always store in database if root user (support for MigrateMediaToDB)
	            			if(this.currentUserIsRoot(this.getConnection())) {
	            				input.addToAttributeValuesAsList("content", BinaryLargeObjects.valueOf(contentFile));            				
	            			} else {
		            			// Replace existing content file (do not create if it does not exist!)
		            			if(contentFile.exists()) {
		            				this.storeContent(contentFile, (BinaryLargeObject)content);
		            			}
	            			}
	            		}
	            	}
	            }
				return super.update(ispec, request, response);
			} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );
			}
		}

    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected final InteractionSpecs SUPER = InteractionSpecs.getRestInteractionSpecs(false);
    
}
