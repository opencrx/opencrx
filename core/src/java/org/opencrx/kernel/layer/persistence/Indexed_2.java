/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Indexed_2 plug-in
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2016, CRIXP Corp., Switzerland
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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.zip.ZipInputStream;

import javax.jdo.FetchGroup;
import javax.resource.ResourceException;
import javax.resource.cci.Interaction;
import javax.resource.cci.MappedRecord;

import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.text.ExcelToText;
import org.opencrx.kernel.text.OpenOfficeToText;
import org.opencrx.kernel.text.PDFToText;
import org.opencrx.kernel.text.RTFToText;
import org.opencrx.kernel.text.WordToText;
import org.opencrx.kernel.text.XmlDocToText;
import org.openmdx.application.dataprovider.cci.AttributeSpecifier;
import org.openmdx.application.dataprovider.cci.FilterProperty;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.naming.Path;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.IsInCondition;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.query.SortOrder;
import org.openmdx.base.resource.Records;
import org.openmdx.base.resource.spi.ResourceExceptions;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.MessageRecord;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryFilterRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.RestConnection;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.FeatureOrderRecord;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObject;

/**
 * Indexer plug-in.
 * 
 */
public class Indexed_2 extends Media_2 {

	/**
	 * Constructor.
	 * 
	 * @throws ServiceException
	 */
	public Indexed_2(
	) {
		super();
        // Types to be indexed
        this.indexableTypes = new TreeSet<Path>();
        Model_1_0 model = this.getModel();
        try {
	        for(ModelElement_1_0 element: model.getContent()) {
	        	if(
	        		element.isClassType() &&
	        		model.isSubtypeOf(element, "org:opencrx:kernel:base:Indexed") &&
	        		!model.isSubtypeOf(element, "org:openmdx:base:Segment")
	        	) {
		            Path type = model.getIdentityPattern(element);	    
		            if(type != null) {
		            	this.indexableTypes.add(type);
		            }
	        	}
	        }
        } catch(ServiceException e) {
        	e.log();
        }
        // Manually add some more
        this.indexableTypes.addAll(
            Arrays.asList(
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityTracker/:*/followUp/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityMilestone/:*/followUp/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityCategory/:*/followUp/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/account/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/lead/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/opportunity/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/quote/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/salesOrder/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/invoice/:*/address/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/account/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/lead/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/opportunity/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/quote/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/salesOrder/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/invoice/:*/note/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/account/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activity/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/lead/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/opportunity/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/quote/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/salesOrder/:*/media/:*"),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/invoice/:*/media/:*")
            )
        );		
	}
	
	/* (non-Javadoc)
	 * @see org.opencrx.kernel.layer.persistence.Media_1#getInteraction(javax.resource.cci.Connection)
	 */
	@Override
    public Interaction getInteraction(
        RestConnection connection
    ) throws ResourceException {
        return new RestInteraction(connection);
    }

    /**
     * Get query for given filter properties and attribute specifiers.
     * 
     * @param resourceIdentifier
     * @param filterProperties
     * @param attributeSpecifiers
     * @return
     * @throws ResourceException
     */
    protected QueryRecord newQuery(
    	Path resourceIdentifier,
    	FilterProperty[] filterProperties,
    	AttributeSpecifier[] attributeSpecifiers
    ) throws ResourceException {
        QueryRecord query = new org.openmdx.base.rest.spi.QueryRecord();
        query.setResourceIdentifier(resourceIdentifier);
        query.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));
    	if(filterProperties != null) {
	    	query.getQueryFilter().getCondition().addAll(
	    		FilterProperty.toCondition(filterProperties)
	    	);
    	}
    	if(attributeSpecifiers != null) {
	    	query.getQueryFilter().getOrderSpecifier().addAll(
	    		AttributeSpecifier.toOrderSpecifier(attributeSpecifiers)
	    	);
    	}
    	return query;
    }
    
    /**
     * Extract keywords from object.
     * 
     * @param obj
     * @return
     * @throws ServiceException
     */
    protected Set<String> getKeywords(
    	MappedRecord obj,
        Integer keywordLengthMin,
        Integer keywordLengthMax,
        Set<String> indexedAttributes
    ) throws ServiceException {
    	Object_2Facade objFacade;
        try {
	        objFacade = Object_2Facade.newInstance(obj);
        }
        catch (ResourceException e) {
        	throw new ServiceException(e);
        }
        Set<String> keywords = new HashSet<String>();
        for(String attribute: indexedAttributes) {
            if(objFacade.getValue().keySet().contains(attribute)) {
                for(
                    Iterator<Object> j = objFacade.attributeValuesAsList(attribute).iterator(); 
                    j.hasNext(); 
                ) {
                    Object value = j.next();
                    Reader text = null;
                    boolean isXml = false;
                    if(value instanceof String) {
                        text = new StringReader((String)value);
                    } else if(value instanceof InputStream || value instanceof byte[] || value instanceof BinaryLargeObject) {
                    	if(value instanceof byte[]) {
                    		value = new ByteArrayInputStream((byte[])value);
                    	} else if(value instanceof BinaryLargeObject) {
                    		try {
                    			value = ((BinaryLargeObject)value).getContent();
                    		} catch(Exception e) {}
                    	}
                        String contentName = (String)objFacade.attributeValuesAsList(attribute + "Name").get(0);
                        String contentMimeType = (String)objFacade.attributeValuesAsList(attribute + "MimeType").get(0);
                        if(contentName != null) { 
                            if(
                                "text/rtf".equals(contentMimeType) ||
                                contentName.endsWith(".rtf")
                            ) {
                                 try {
                                     text = RTFToText.toTextAsReader(
                                         (InputStream)value
                                     );
                                 } catch(Exception e) {
                                	 SysLog.warning("Cannot extract text from a RTF document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                 }
                            } else if(
                                "application/pdf".equals(contentMimeType) ||
                                contentName.endsWith(".pdf")
                            ) {
                                try {
                                    text = new PDFToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from PDF document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                               "application/vnd.ms-excel".equals(contentMimeType) ||
                               "application/ms-excel".equals(contentMimeType) ||
                                contentName.endsWith(".xls")
                            ) {
                                try {
                                    text = new ExcelToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from Excel document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                               "application/vnd.ms-word".equals(contentMimeType) ||
                               "application/ms-word".equals(contentMimeType) ||
                                contentName.endsWith(".doc")
                            ) {
                                try {
                                    text = new WordToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from Word document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                            	(contentMimeType != null && contentMimeType.startsWith("application/vnd.openxmlformats")) ||
                                contentName.endsWith(".docx") ||
                                contentName.endsWith(".dotx") ||
                                contentName.endsWith(".xlsx") ||
                                contentName.endsWith(".xltx")
                            ) {
                                try {
                                    text = new XmlDocToText().parse(
                                        (InputStream)value
                                    );
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from XML document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                                contentName.endsWith(".odt") ||
                                contentName.endsWith(".odp") ||
                                contentName.endsWith(".ods")
                            ) {
                                try {
                                    ZipInputStream document = new ZipInputStream((InputStream)value);
                                    text = new OpenOfficeToText().parse(
                                        document
                                    );
                                    isXml = true;
                                } catch(Exception e) {
                                	SysLog.warning("Can not extract text from OpenOffice document", Arrays.asList(new String[]{contentName, e.getMessage()}));
                                }
                            } else if(
                                "text/plain".equals(contentMimeType) ||
                                contentName.endsWith(".txt")
                            ) {
                                text = new InputStreamReader((InputStream)value);                                
                            } else if(
                                "text/html".equals(contentMimeType) ||
                                "text/xml".equals(contentMimeType) ||
                                "application/xml".equals(contentMimeType) ||
                                contentName.endsWith(".xml") || 
                                contentName.endsWith(".html") || 
                                contentName.endsWith(".htm")
                            ) {
                                text = new InputStreamReader((InputStream)value);           
                                isXml = true;
                            }                           
                        }
                    }
                    if(text != null) {
                        try {
                            int ch = text.read();
                            while(ch != -1) {
                                // Skip tags if xml
                                if(isXml && (ch == '<')) {
                                    while(
                                        (ch != -1) &&
                                        (ch != '>')
                                    ) {
                                        ch = text.read();
                                    }
                                    if(ch != -1) {
                                        ch = text.read();
                                    }
                                }
                                StringBuilder keyword = new StringBuilder();
                                boolean isKeyword = false;
                                while(
                                    (ch != -1) && 
                                    (!isXml || (isXml && ch != '<')) &&
                                    Character.isLetterOrDigit((char)ch) || (ch == '-') || (ch == '_')                                    
                                ) {
                                    keyword.append((char)ch);
                                    ch = text.read();        
                                    isKeyword = true;
                                }
                                if(!isKeyword && (!isXml || (ch != '<'))) {
                                    ch = text.read();
                                } else if(
                                    (keyword.length() > keywordLengthMin) &&
                                    (keyword.length() < keywordLengthMax)
                                ) {
                                    keywords.add(keyword.toString().toLowerCase());
                                }
                            }
                        } catch(Exception e) {}
                    }
                }
            }
        }
        return keywords;
    }

    /**
     * Test whether object is instanceof of AccountAddress.
     * 
     * @param object
     * @return
     * @throws ServiceException
     */
    public boolean isAccountAddress(
    	MappedRecord object
    ) throws ServiceException {
        String objectClass = Object_2Facade.getObjectClass(object);
        return this.getModel().isSubtypeOf(
            objectClass,
            "org:opencrx:kernel:account1:AccountAddress"
        );
    }

    /**
     * RestInteraction
     *
     */
    public class RestInteraction extends Media_2.RestInteraction {
        
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
	     * Update index entry for given object.
	     * 
	     * @param header
	     * @param indexed
	     * @throws ServiceException
	     */
	    private void updateIndexEntry(
	    	RestInteractionSpec ispec,
	        ObjectRecord indexed,
	        Integer keywordLengthMin,
	        Integer keywordLengthMax,
	        Set<String> indexedAttributes
	    ) throws ServiceException {
	    	try {
		    	Path indexedPath = Object_2Facade.getPath(indexed);
		    	Object_2Facade indexedFacade = Object_2Facade.newInstance(indexed);
		    	ObjectRecord indexEntry = Object_2Facade.newInstance(
		            Object_2Facade.getPath(indexed).getPrefix(5).getDescendant(new String[]{"indexEntry", UUIDs.newUUID().toString()}),
		            "org:opencrx:kernel:base:IndexEntry"
		        ).getDelegate();
		    	Object_2Facade indexEntryFacade = Object_2Facade.newInstance(indexEntry);
		        indexEntryFacade.attributeValuesAsList("indexedObject").add(
		            indexedPath
		        );
		        Set<String> keywords = Indexed_2.this.getKeywords(
		            indexed,
		            keywordLengthMin,
		            keywordLengthMax,
		            indexedAttributes
		        );
		        // AccountAddress: add keywords of account
		        if(Indexed_2.this.isAccountAddress(indexed)) {
		        	ObjectRecord account = this.retrieveObject(
		        		indexedPath.getPrefix(indexedPath.size() - 2), 
		        		FetchGroup.ALL
		        	);
		        	if(account != null) {
			            keywords.addAll(	            	
			            	Indexed_2.this.getKeywords(
			            		account,
			            		keywordLengthMin,
			            		keywordLengthMax,
			            		indexedAttributes
			            	)
			            );
		        	}
		        }
		        indexEntryFacade.attributeValuesAsList("keywords").add(
		            keywords.toString()
		        );
		        indexEntryFacade.attributeValuesAsList(SystemAttributes.MODIFIED_AT).addAll(
		            indexedFacade.attributeValuesAsList(SystemAttributes.MODIFIED_AT)
		        );
		        indexEntryFacade.attributeValuesAsList(SystemAttributes.MODIFIED_BY).addAll(
		            indexedFacade.attributeValuesAsList(SystemAttributes.MODIFIED_BY)
		        );
		        indexEntryFacade.attributeValuesAsList(SystemAttributes.CREATED_AT).addAll(
		        	indexedFacade.attributeValuesAsList(SystemAttributes.MODIFIED_AT)
		        );
		        indexEntryFacade.attributeValuesAsList(SystemAttributes.CREATED_BY).addAll(
		        	indexedFacade.attributeValuesAsList(SystemAttributes.MODIFIED_BY)
		        );
		        indexEntryFacade.attributeValuesAsList("owner").addAll(
		        	indexedFacade.attributeValuesAsList("owner")
		        );
		        indexEntryFacade.attributeValuesAsList("accessLevelBrowse").addAll(
		        	indexedFacade.attributeValuesAsList("accessLevelBrowse")
		        );
		        indexEntryFacade.attributeValuesAsList("accessLevelUpdate").add(
		            new Short(SecurityKeys.ACCESS_LEVEL_NA)
		        );
		        indexEntryFacade.attributeValuesAsList("accessLevelDelete").add(
		            new Short(SecurityKeys.ACCESS_LEVEL_NA)
		        );                
		        // Create entry
		        super.create(
		        	SUPER.CREATE, 
		        	indexEntry, 
		        	this.newResult()
		        );
	    	} catch(ResourceException e) {
	    		throw new ServiceException(e);
	    	}
	    }
	    
	    /* (non-Javadoc)
	     * @see org.opencrx.kernel.layer.persistence.Media_1.LayerInteraction#get(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @SuppressWarnings("unchecked")
	    @Override
        public boolean get(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
    		Path path = request.getResourceIdentifier();
	        Path reference = path.getParent();
	        if("indexEntry".equals(reference.getLastSegment().toClassicRepresentation())) {
	        	ObjectRecord indexEntry = this.retrieveObject(
	        		path.getPrefix(5).getDescendant("indexEntry", path.getLastSegment().toClassicRepresentation()), 
	        		FetchGroup.ALL
	        	);
	        	if(indexEntry != null) {
		        	Object_2Facade indexEntryFacade = Object_2Facade.newInstance(indexEntry);
		        	Object_2Facade mappedIndexEntryFacade = Object_2Facade.newInstance(
		                path,
		                indexEntryFacade.getObjectClass()
		            );
		            mappedIndexEntryFacade.getValue().putAll(
		                indexEntryFacade.getValue()
		            );
	            	response.add(
	            		mappedIndexEntryFacade.getDelegate()
	            	);
		            return true;
	        	} else {
	        		return false;
	        	}
	        } else {
	            super.get(
	                ispec,
	                request,
	                response
	            );
	            return true;
	        }
	    }
	    
	    /**
	     * Find index entries matching the given filter.
	     * 
	     * @param header
	     * @param objectIdentity
	     * @param attributeFilter
	     * @param attributeSpecifier
	     * @return
	     * @throws ServiceException
	     */
	    protected ResultRecord findIndexEntries(
	        Path objectIdentity,
	        QueryRecord query
	    ) throws ServiceException {
	    	try {
	    		QueryRecord findRequest = query == null ? Records.getRecordFactory().createMappedRecord(QueryRecord.class) : query.clone();
	    		findRequest.setResourceIdentifier(objectIdentity.getPrefix(5).getChild("indexEntry"));
	    		if(findRequest.getQueryFilter() == null) {
	    			findRequest.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));
	    		}
	    		findRequest.getQueryFilter().getCondition().add(
	    			new IsInCondition(
	    				Quantifier.THERE_EXISTS,
	    				"indexedObject",
	    				true,
	    				objectIdentity
	    			)
	    		);
	    		findRequest.getQueryFilter().getOrderSpecifier().add(
	    			new FeatureOrderRecord(
	    				SystemAttributes.CREATED_AT,
	    				SortOrder.DESCENDING
	    			)
	    		);
	    		ResultRecord findResult = this.newResult();
	    		super.find(
	    			SUPER.GET,
	    			findRequest, 
	    			findResult
	    		);
		        return findResult;
	    	} catch(ResourceException e) {
	    		throw new ServiceException(e);
	    	}
	    }
	    
	    /* (non-Javadoc)
	     * @see org.opencrx.kernel.layer.persistence.Media_1.LayerInteraction#find(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @SuppressWarnings("unchecked")
	    @Override
        public boolean find(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
	    	try {
		        // If indexEntry is segment do not rewrite find request. Otherwise filter
		        // index entries with indexedObject = requested reference
		        if(
		            (path.size() > 6) &&
		            "indexEntry".equals(path.getLastSegment().toClassicRepresentation())
		        ) {
		        	ResultRecord indexEntries = this.findIndexEntries(
		                path.getParent(),
		                request
		            );
		            // Remap index entries so that the parent of the mapped
		            // index entries is the requesting object, i.e. the indexed object
		            List<ObjectRecord> mappedIndexEntries = new ArrayList<ObjectRecord>();
		            for(Object entry: indexEntries) {
		            	ObjectRecord indexEntry = (ObjectRecord)entry;
		            	ObjectRecord mappedIndexEntry = Object_2Facade.cloneObject(indexEntry);
		            	Object_2Facade.newInstance(mappedIndexEntry).setPath(
		                    path.getChild(Object_2Facade.getPath(indexEntry).getLastSegment().toClassicRepresentation())
		                );
		                mappedIndexEntries.add(
		                    mappedIndexEntry
		                );
		            }
		            // reply
	            	response.addAll(
	            		mappedIndexEntries
	            	);
		            response.setHasMore(Boolean.FALSE);
		            response.setTotal(new Integer(mappedIndexEntries.size()));
		            return true;
		        } else {
		            return super.find(
		                ispec,
		                request,
		                response
		            );
		        }
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );
	    	}
	    }
	    
	    /* (non-Javadoc)
	     * @see org.opencrx.kernel.layer.persistence.Media_1.LayerInteraction#delete(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @Override
        public boolean delete(
            RestInteractionSpec ispec, 
            ObjectRecord request
        ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
	    	try {
		        // Remove index entries of object to be removed
		        if(path.size() > 5) {
		        	ResultRecord indexEntries = this.findIndexEntries(
		                 path, 
		                 null
		            );
		            for(Object entry: indexEntries) {
		            	super.delete(
		            		SUPER.DELETE, 
		            		(ObjectRecord)entry
		            	);
		            }
		        }
		        return super.delete(
		            ispec,
		            request
		        );
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );
	    	}
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.spi.OperationAwareLayer_1.LayerInteraction#otherOperation(org.openmdx.application.dataprovider.cci.ServiceHeader, org.openmdx.application.dataprovider.cci.DataproviderRequest, java.lang.String, org.openmdx.base.naming.Path)
	     */
	    @Override
	    public boolean invoke(
	        RestInteractionSpec ispec, 
	        MessageRecord request, 
	        MessageRecord response
	    ) throws ResourceException {
            String operationName = request.getTarget().getLastSegment().toClassicRepresentation();
            Path path = request.getResourceIdentifier();
	    	try {
		        if("updateIndex".equals(operationName)) {
		            Integer keywordLengthMin = (Integer)request.getBody().get("keywordLengthMin");
		            keywordLengthMin = keywordLengthMin == null ? STANDARD_KEYWORD_LENGTH_MIN : keywordLengthMin;		            
		            Integer keywordLengthMax = (Integer)request.getBody().get("keywordLengthMax");
		            keywordLengthMax = keywordLengthMax == null ? STANDARD_KEYWORD_LENGTH_MAX : keywordLengthMax;
		            Set<String> indexedAttributes = new HashSet<String>(STANDARD_INDEXED_ATTRIBUTES);
		            @SuppressWarnings("unchecked")
					List<String> indexedAttributesInclude = (List<String>)request.getBody().get("indexedAttributesInclude");
		            if(indexedAttributesInclude != null) {
		            	indexedAttributes.addAll(indexedAttributesInclude);
		            }
 		            @SuppressWarnings("unchecked")
					List<String> indexedAttributesExclude = (List<String>)request.getBody().get("indexedAttributesExclude");
 		            if(indexedAttributesExclude != null) {
 		            	indexedAttributes.removeAll(indexedAttributesExclude);
 		            }
		            Path indexedIdentity = path.getPrefix(path.size() - 2);
		            int numberOfIndexedObjects = 0;
		            // At segment level update all objects to be indexed (up to a batch size)
		            if(indexedIdentity.size() == 5) {
		                for(Path type: Indexed_2.this.indexableTypes) {
		                    // Type must be composite to indexed segment
		                    if(
		                        ":*".equals(type.getSegment(0).toClassicRepresentation()) ||
		                        type.getSegment(0).toClassicRepresentation().equals(indexedIdentity.getSegment(0).toClassicRepresentation())
		                    ) {
		                        Path concreteType = new Path("");
		                        for(int i = 0; i < type.size(); i++) {
		                            if(i == 0 || i == 2 || i == 4) {
		                            	concreteType = concreteType.getChild(indexedIdentity.getSegment(i).toClassicRepresentation());
		                            } else {
		                            	concreteType = concreteType.getChild(type.getSegment(i).toClassicRepresentation());
		                            }
		                        }
		                        String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + UUIDs.newUUID().toString() + ":";
		                        Date syncKey = Indexed_2.this.syncKeys.get(concreteType);
		                        if(syncKey == null) {
		                        	syncKey = new Date(0);
		                        }
		                        QueryRecord findRequest = Indexed_2.this.newQuery(
		                        	indexedIdentity.getChild("extent"), 
		                        	new FilterProperty[]{
		                                new FilterProperty(
		                                    Quantifier.THERE_EXISTS.code(),
		                                    SystemAttributes.OBJECT_IDENTITY,
		                                    ConditionType.IS_LIKE.code(),
		                                    new Object[]{concreteType.toXRI()}
		                                ),
		                                new FilterProperty(
		                                    Quantifier.THERE_EXISTS.code(),
		                                    SystemAttributes.MODIFIED_AT,
		                                    ConditionType.IS_GREATER_OR_EQUAL.code(),
		                                    new Object[]{syncKey}
		                                ),
		                                // All objects which do not have an up-to-date index entry
		                                new FilterProperty(
		                                    Quantifier.codeOf(null),
		                                    queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
		                                    ConditionType.codeOf(null),
		                                    new Object[]{
		                                    	Database_1_Attributes.HINT_COLUMN_SELECTOR + " v.object_id, v.dtype */ NOT EXISTS (SELECT 0 FROM OOCKE1_INDEXENTRY e WHERE v.object_id = e.indexed_object AND v.modified_at <= e.created_at)"
		                                    }
		                                ),
		                                new FilterProperty(
		                                    Quantifier.codeOf(null),
		                                    queryFilterContext + SystemAttributes.OBJECT_CLASS,
		                                    ConditionType.codeOf(null),
		                                    new Object[]{Database_1_Attributes.QUERY_EXTENSION_CLASS}
		                                )
		                        	}, 
		                            new AttributeSpecifier[]{
	                            		new AttributeSpecifier(SystemAttributes.MODIFIED_AT, 0, SortOrder.ASCENDING.code())
	                            	}
		                        );
		                        ResultRecord findResult = null;
		                        super.find(
		                        	SUPER.GET, 
		                        	findRequest, 
		                        	findResult = this.newResult()
		                        );
	                        	Date newSyncKey = syncKey;
		                        for(Object object: findResult) {
		                            try {
		                            	Path objectToBeIndexedPath = Object_2Facade.getPath((ObjectRecord)object);
		                            	ObjectRecord indexed = this.retrieveObject(
		                            		objectToBeIndexedPath, 
		                            		FetchGroup.ALL
		                            	);
		                            	if(indexed != null) {
			                                this.updateIndexEntry(
			                                	ispec,
			                                	indexed,
			                                	keywordLengthMin,
			                                	keywordLengthMax,
			                                	indexedAttributes
			                                );
			                                newSyncKey = (Date)Facades.asObject(indexed).attributeValue(SystemAttributes.MODIFIED_AT);
			                                numberOfIndexedObjects++;
		                            	}
		                            } catch(Exception e) {
		                            	SysLog.log(Level.WARNING, "Unable to index {0}. The reason is {1}. See log at level detail for more info.", object, e.getMessage());
		                            	SysLog.detail(e.getMessage(), e.getCause());
		                            }
		                        }
	                        	Indexed_2.this.syncKeys.put(concreteType, newSyncKey);
	                        }
		                }
		            } else {
			            // Index object
		            	ObjectRecord indexed = this.retrieveObject(
		            		indexedIdentity, 
		            		FetchGroup.ALL
		            	);
		            	if(indexed != null) {
			                this.updateIndexEntry(
			                	ispec,
			                    indexed,
			                    keywordLengthMin,
			                    keywordLengthMax,
			                    indexedAttributes
			                );
			                numberOfIndexedObjects++;
		            	}
		            }
		            response.setBody(this.newOperationResult("org:opencrx:kernel:base:UpdateIndexResult"));
		            response.getBody().put("numberOfIndexedObjects",  new Integer(numberOfIndexedObjects));
		            return true;
		        } else {
			        // Delegate
		            return super.invoke(
		                ispec,
		                request,
		                response
		            );
		        }
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
    public static final int STANDARD_KEYWORD_LENGTH_MIN = 3;
    public static final int STANDARD_KEYWORD_LENGTH_MAX = 40;
    protected static final int BATCH_SIZE = 50;
    
    protected Set<Path> indexableTypes;
	protected Map<Path,Date> syncKeys = new ConcurrentHashMap<Path,Date>();
    
    protected static final Set<String> STANDARD_INDEXED_ATTRIBUTES =
        new HashSet<String>(
            Arrays.asList(
                "name",
                "description",
                "detailedDescription",
                "aliasName",
                "fullName",
                "postalAddressLine",
                "postalStreet",
                "postalCity",
                "postalCode",
                "emailAddress",
                "webUrl",
                "phoneNumberFull",
                "text",
                "content",
                "contractNumber",
                "activityNumber",
                "externalLink"
            )
        );
    
}
