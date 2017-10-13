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

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;

import javax.jdo.FetchGroup;
import javax.resource.ResourceException;
import javax.resource.cci.Interaction;

import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.generic.SecurityKeys;
import org.openmdx.application.dataprovider.cci.AttributeSpecifier;
import org.openmdx.application.dataprovider.cci.FilterProperty;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
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
		try {
			this.indexableTypes = Base.getInstance().getIndexableTypes();
		} catch(Exception e) {
			new ServiceException(e).log();
		}
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
		        Base.RestInteractionCallback restInteractionCallback = new Base.RestInteractionCallback(){
					@Override
					public void get(
						QueryRecord query, 
						ResultRecord result
					) throws ResourceException {
						RestInteraction.super.get(
							SUPER.GET,
							query,
							result
						);
					}
					@Override
					public void find(
						QueryRecord query, 
						ResultRecord result
					) throws ResourceException {
						RestInteraction.super.find(
							SUPER.GET,
							query,
							result
						);				
					}
					@Override
					public void create(
						ObjectRecord query, 
						ResultRecord result
					) throws ResourceException {
						RestInteraction.super.create(
							SUPER.CREATE,
							query,
							result
						);
					}
					@Override
					public void update(
						ObjectRecord object, 
						ResultRecord result
					) throws ResourceException {
						RestInteraction.super.update(
							SUPER.UPDATE,
							object,
							result
						);
					}
	            };
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
		        List<String> keywords = Base.getInstance().getKeywords(
		            indexed,
		            keywordLengthMin,
		            keywordLengthMax,
		            indexedAttributes,
		            restInteractionCallback
		        );
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
		        Base.getInstance().updateIndexEntry(
		        	restInteractionCallback,
		        	indexEntry
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
    
    protected List<Path> indexableTypes;
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
