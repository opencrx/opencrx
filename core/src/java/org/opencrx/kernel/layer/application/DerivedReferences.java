/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DerivedReferences
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
package org.opencrx.kernel.layer.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;

import javax.jdo.FetchGroup;
import javax.resource.ResourceException;
import javax.resource.cci.MappedRecord;
import javax.xml.datatype.XMLGregorianCalendar;

import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.generic.OpenCrxException;
import org.openmdx.application.dataprovider.cci.AttributeSpecifier;
import org.openmdx.application.dataprovider.cci.FilterProperty;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.dataprovider.cci.DataproviderRequestProcessor;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Filter;
import org.openmdx.base.query.IsInCondition;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.query.SortOrder;
import org.openmdx.base.resource.Records;
import org.openmdx.base.resource.spi.ResourceExceptions;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryFilterRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;

/**
 * DerivedReferences
 *
 */
public class DerivedReferences {

	private final Model_1_0 model;
	private final DataproviderRequestProcessor dataproviderRequestProcessor;
	private final List<String> readOnlyTypes;
	
	/**
     * Constructor.
     * 
     * @param context
     */
    public DerivedReferences(
    	DataproviderRequestProcessor dataproviderRequestProcessor,
        List<String> readOnlyTypes
    ) {
        this.model = Model_1Factory.getModel();
        this.dataproviderRequestProcessor = dataproviderRequestProcessor;
        this.readOnlyTypes = readOnlyTypes;
    }

    /**
	 * @return the readOnlyTypes
	 */
	public List<String> getReadOnlyTypes() {
		return readOnlyTypes;
	}

	/**
	 * @return the model
	 */
	public Model_1_0 getModel() {
		return model;
	}

	/**
	 * @return the dataproviderRequestProcessor
	 */
	public DataproviderRequestProcessor getDataproviderRequestProcessor() {
		return dataproviderRequestProcessor;
	}

    public ObjectRecord retrieveObject(
    	DataproviderRequestProcessor p,
        Path identity
    ) throws ResourceException {
		return p.addGetRequest(
			identity, 
			FetchGroup.ALL
		);
    }

    @SuppressWarnings("unchecked")
    public void testObjectIsChangeable(
        MappedRecord object
    ) throws ServiceException {
    	Object_2Facade facade;
        try {
	        facade = Object_2Facade.newInstance(object);
        }
        catch (ResourceException e) {
        	throw new ServiceException(e);
        }
        String objectClass = facade.getObjectClass();
        for(
            Iterator<String> i = this.getReadOnlyTypes().iterator();
            i.hasNext();
        ) {           
            String type = i.next();
            if(this.getModel().isSubtypeOf(objectClass, type)) {
                for(
                    Iterator<String> j = facade.getValue().keySet().iterator();
                    j.hasNext();
                ) {
                    String attributeName = j.next();
                    if(
                        !SystemAttributes.OBJECT_CLASS.equals(attributeName) &&
                        !SystemAttributes.MODIFIED_AT.equals(attributeName) &&
                        !SystemAttributes.MODIFIED_BY.equals(attributeName)                        
                    ) {
                        throw new ServiceException(
                            OpenCrxException.DOMAIN,
                            OpenCrxException.OBJECT_TYPE_IS_READONLY,
                            "Object type is readonly. Can not modify object.",
                            new BasicException.Parameter("param0", objectClass)
                        );
                    }
                }
            }
        }
    }
    
    /**
     * Remap find request to given reference and add additional filter properties 
     * and attribute specifiers.
     * 
     * @param request
     * @param reference
     * @param additionalFilterProperties
     * @param additionalAttributeSpecifiers
     * @return
     * @throws ServiceException
     */
    protected QueryRecord remapQuery(
        QueryRecord request,
        Path reference,
        FilterProperty[] additionalFilterProperties,
        AttributeSpecifier[] additionalAttributeSpecifiers
    ) throws ResourceException {
    	QueryRecord mappedQuery = request.clone();
    	mappedQuery.setResourceIdentifier(reference);
    	if(mappedQuery.getQueryFilter() == null) {
    		mappedQuery.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));
    	}
    	if(additionalFilterProperties != null) {
	    	mappedQuery.getQueryFilter().getCondition().addAll(
	    		FilterProperty.toCondition(additionalFilterProperties)
	    	);
    	}
    	if(additionalAttributeSpecifiers != null) {
	    	mappedQuery.getQueryFilter().getOrderSpecifier().addAll(
	    		AttributeSpecifier.toOrderSpecifier(additionalAttributeSpecifiers)
	    	);
    	}
    	return mappedQuery;
    }

    /**
     * Remap find request to given reference.
     * 
     * @param request
     * @param reference
     * @return
     * @throws ServiceException
     */
    protected QueryRecord remapFindRequest(
        QueryRecord request,
        Path reference
    ) throws ResourceException {
    	return this.remapQuery(request, reference, null, null);
    }

    /**
     * Remap find request to given reference and add additional filter properties.
     * 
     * @param request
     * @param reference
     * @param additionalFilterProperties
     * @return
     * @throws ServiceException
     */
    protected QueryRecord remapQuery(
        QueryRecord request,
        Path reference,
        FilterProperty[] additionalFilterProperties
    ) throws ResourceException {
    	return this.remapQuery(
    		request, 
    		reference, 
    		additionalFilterProperties, 
    		null
    	);
    }

    /**
     * Get reply.
     * 
     * @param header
     * @param request
     * @param reply
     * @return
     * @throws ServiceException
     */
    public boolean getReply(
        RestInteractionSpec ispec,
        QueryRecord request,
        ResultRecord response
    ) throws ResourceException {
    	Path path = request.getResourceIdentifier();
    	DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
    	try {
	        // GlobalFilterIncludesActivity
	        if(
	        	path.isLike(GLOBAL_FILTER_INCLUDES_ACTIVITY)
	        ) {
	        	MappedRecord globalFilter = this.retrieveObject(p, path.getPrefix(7));
	        	Object_2Facade globalFilterFacade = Facades.asObject(globalFilter);	        	
	        	Path reference = path.getPrefix(5).getChild("activity");
	        	if(globalFilterFacade.attributeValue("activitiesSource") != null) {
	        		Path activitiesSourceIdentity = (Path)globalFilterFacade.attributeValue("activitiesSource");
	        		MappedRecord activitiesSource = this.retrieveObject(p, activitiesSourceIdentity);
	        		Object_2Facade activitiesSourceFacade = Facades.asObject(activitiesSource);
	        		Model_1_0 model = this.getModel();
		        	if(model.isSubtypeOf(activitiesSourceFacade.getObjectClass(), "org:opencrx:kernel:activity1:ActivityGroup")) {
		        		reference = activitiesSourceIdentity.getChild("filteredActivity");		        		
		        	} else if(model.isSubtypeOf(activitiesSourceFacade.getObjectClass(), "org:opencrx:kernel:activity1:Segment")) {
		        		reference = activitiesSourceIdentity.getChild("activity");		        		
		        	} else if(model.isSubtypeOf(activitiesSourceFacade.getObjectClass(), "org:opencrx:kernel:home1:UserHome")) {
		        		reference = activitiesSourceIdentity.getChild("assignedActivity");		        		
		        	} else if(model.isSubtypeOf(activitiesSourceFacade.getObjectClass(), "org:opencrx:kernel:account1:Account")) {
		        		reference = activitiesSourceIdentity.getChild("assignedActivity");
		        	}
	        	}
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                reference,
		                this.getActivityFilterProperties(
		                	path.getPrefix(path.size() - 1)
		                )
		            ),
		            response
	        	);
	        	return true;
	        }
	        // GlobalFilterIncludesAccount
	        if(
	            path.isLike(GLOBAL_FILTER_INCLUDES_ACCOUNT)
	        ) {
	        	p.addFindRequest(
	        		 this.remapQuery(
	 	                request,
	 	                path.getPrefix(5).getChild("account"),
	 	                this.getAccountFilterProperties(
	 	                	path.getPrefix(path.size() - 1)
	 	                )
	 	            ), 
	        		response
	        	);
	            return true;
	        }
	        // GlobalFilterIncludesAddress
	        if(path.isLike(GLOBAL_FILTER_INCLUDES_ADDRESS)) {
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("address"),
		                this.getAddressFilterProperties(
		                	path.getPrefix(path.size() - 1)
		                )
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(ACTIVITY_FILTER_INCLUDES_ACTIVITY)) {
		        // ActivityFilterIncludesActivity
	            List<FilterProperty> filterProperties = new ArrayList<FilterProperty>();
	            filterProperties.addAll(
	                Arrays.asList(
	                    this.getActivityFilterProperties(
	                    	path.getPrefix(path.size() - 1)
	                    )
	                )
	            );
	            // Remap to ActivityGroupContainsActivity
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                path.getPrefix(7).getChild("filteredActivity"),
		                filterProperties.toArray(new FilterProperty[filterProperties.size()])
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(CONTRACT_FILTER_INCLUDES_CONTRACT)) {
		        // ContractFilterIncludesContract
	            List<FilterProperty> filterProperties = new ArrayList<FilterProperty>();
	            filterProperties.addAll(
	                Arrays.asList(
	                    this.getContractFilterProperties(
	                    	path.getPrefix(path.size() - 1)
	                    )
	                )
	            );
	            // Remap to ContractGroupContainsContract
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                path.getPrefix(7).getChild("filteredContract"),
		                filterProperties.toArray(new FilterProperty[filterProperties.size()])
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(GLOBAL_FILTER_INCLUDES_CONTRACT)) {
		        // GlobalFilterIncludesContract
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path,
		                this.getContractFilterProperties(
		                	path.getPrefix(path.size() - 1)
		                )
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(GLOBAL_FILTER_INCLUDES_PRODUCT)) {
		        // GlobalFilterIncludesProduct
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("product"),
		                this.getProductFilterProperties(
		                	path.getPrefix(path.size() - 1),
		                    false
		                )
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(GLOBAL_FILTER_INCLUDES_DOCUMENT)) {
		        // GlobalFilterIncludesDocument
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("document"),
		                this.getDocumentFilterProperties(
		                    path.getPrefix(path.size() - 1)
		                )
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(PRODUCT_PRICE_LEVEL_HAS_FILTERED_ACCOUNT)) {
		        // PriceLevelHasFilteredAccount
	            List<FilterProperty> filterProperties = new ArrayList<FilterProperty>();
	            filterProperties.addAll(
	                Arrays.asList(
	                    this.getAccountFilterProperties(
	                        path.getPrefix(path.size() - 1)
	                    )
	                )
	            );
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                new Path("xri:@openmdx:org.opencrx.kernel.account1/provider").getDescendant(
		                   new String[]{path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation(), "account"}
		                ),
		                filterProperties.toArray(new FilterProperty[filterProperties.size()])
		            ), 
	            	response
	            );
	            return true;
	        } else if(
	            path.isLike(PRODUCT_PRICE_LEVEL_INCLUDES_FILTERED_PRODUCT) ||
	            path.isLike(SALES_VOLUME_BUDGET_POSITION_INCLUDES_FILTERED_PRODUCT)
	        ) {
		        // FilterIncludesProduct
	            List<FilterProperty> filterProperties = new ArrayList<FilterProperty>();
	            filterProperties.addAll(
	                Arrays.asList(
	                    this.getProductFilterProperties(
	                        path.getPrefix(path.size() - 1),
	                        false
	                    )
	                )
	            );
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                new Path("xri://@openmdx*org.opencrx.kernel.product1/provider").getDescendant(
		                   new String[]{path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation(), "product"}
		                ),
		                filterProperties.toArray(new FilterProperty[filterProperties.size()])
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(PRODUCT_PRICE_LEVEL_HAS_ASSIGNED_PRICE_LIST_ENTRY)) {
		        // PriceLevelHasAssignedPriceListEntry
	            List<FilterProperty> filterProperties = new ArrayList<FilterProperty>();
	            filterProperties.add(
	                new FilterProperty(
	                    Quantifier.THERE_EXISTS.code(),
	                    "priceLevel",
	                    ConditionType.IS_IN.code(),
	                    new Object[]{path.getPrefix(7)}	                    
	                )
	            );
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                new Path("xri:@openmdx:org.opencrx.kernel.product1/provider").getDescendant(
		                   new String[]{path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation(), "priceListEntry"}
		                ),
		                filterProperties.toArray(new FilterProperty[filterProperties.size()])
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(COMPOUND_BOOKING_HAS_BOOKINGS)) {
		        // CompoundBookingHasBooking
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("booking"),
		                new FilterProperty[]{
		                    new FilterProperty(
		                        Quantifier.THERE_EXISTS.code(),
		                        "cb",
		                        ConditionType.IS_IN.code(),
		                        new Object[]{path.getPrefix(7)}
		                    )                        
		                }
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(DEPOT_POSITION_HAS_BOOKINGS)) {
		        // DepotPositionHasBooking
	        	p.addFindRequest(
	        		 this.remapQuery(
	 	                request,
	 	                path.getPrefix(5).getChild("booking"),
	 	                new FilterProperty[]{
	 	                    new FilterProperty(
	 	                        Quantifier.THERE_EXISTS.code(),
	 	                        "position",
	 	                        ConditionType.IS_IN.code(),
	 	                        new Object[]{path.getPrefix(13)}
	 	                    )                        
	 	                }
	 	            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(DEPOT_POSITION_HAS_SIMPLE_BOOKINGS)) {
		        // DepotPositionHasSimpleBooking
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("simpleBooking"),
		                new FilterProperty[]{
		                    new FilterProperty(
		                        Quantifier.THERE_EXISTS.code(),
		                        "position",
		                        ConditionType.IS_IN.code(),
		                        new Object[]{path.getPrefix(13)}
		                    )                        
		                }
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(CLASSIFIER_CLASSIFIES_TYPED_ELEMENT)) {
		        // ClassifierClassifiesTypedElement
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("element"),
		                new FilterProperty[]{
		                    new FilterProperty(
		                        Quantifier.THERE_EXISTS.code(),
		                        "type",
		                        ConditionType.IS_IN.code(),
		                        new Object[]{path.getPrefix(7)}
		                    )                        
		                }
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(DEPOT_REPORT_ITEM_HAS_BOOKING_ITEMS)) {
		        // DepotReportItemHasBookingItem
	        	MappedRecord itemPosition = this.retrieveObject(p, path.getParent());
            	p.addFindRequest(
            		this.remapQuery(
	                    request,
	                    path.getPrefix(path.size()-3).getChild("itemBooking"),
	                    new FilterProperty[]{
	                        new FilterProperty(
	                            Quantifier.THERE_EXISTS.code(),
	                            "position",
	                            ConditionType.IS_IN.code(),
	                            new Object[]{Object_2Facade.newInstance(itemPosition).attributeValue("position")}
	                        )                        
	                    }
	                ), 
            		response
            	);
                return true;
	        } else if(path.isLike(DEPOT_GROUP_CONTAINS_DEPOTS)) {
		        // DepotContainsDepot
	        	p.addFindRequest(
	        		 this.remapQuery(
	 	                request,
	 	                path.getPrefix(5).getChild("extent"),
	 	                new FilterProperty[]{
	 	                    new FilterProperty(
	 	                        Quantifier.THERE_EXISTS.code(),
	 	                        "depotGroup",
	 	                        ConditionType.IS_IN.code(),
	 	                        new Object[]{path.getParent()}
	 	                    ),                        
	 	                    new FilterProperty(
	 	                        Quantifier.THERE_EXISTS.code(),
	 	                        SystemAttributes.OBJECT_IDENTITY,
	 	                        ConditionType.IS_LIKE.code(),
	 	                        new Object[]{path.getPrefix(7).getDescendant(new String[]{"depotHolder", ":*", "depot", ":*"})}
	 	                    )                        
	 	                }
	 	            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(DEPOT_GROUP_CONTAINS_DEPOT_GROUPS)) {
		        // DepotContainsDepotGroup
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(path.size()-2),
		                new FilterProperty[]{
		                    new FilterProperty(
		                        Quantifier.THERE_EXISTS.code(),
		                        "parent",
		                        ConditionType.IS_IN.code(),
		                        new Object[]{path.getParent()}
		                    )                        
		                }
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(DEPOT_ENTITY_CONTAINS_DEPOTS)) {
		        // DepotEntityContainsDepot
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("extent"),
		                new FilterProperty[]{
		                    new FilterProperty(
		                        Quantifier.THERE_EXISTS.code(),
		                        SystemAttributes.OBJECT_IDENTITY,
		                        ConditionType.IS_LIKE.code(),
		                        new Object[]{path.getParent().getDescendant(new String[]{"depotHolder", ":*", "depot", ":*"})}
		                    )                        
		                }
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(FOLDER_CONTAINS_FOLDERS)) {
		        // FolderContainsFolder
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("folder"),
		                new FilterProperty[]{
		                    new FilterProperty(
		                        Quantifier.THERE_EXISTS.code(),
		                        "parent",
		                        ConditionType.IS_IN.code(),
		                        new Object[]{path.getParent()}
		                    )                        
		                }
		            ), 
	        		response
	        	);
	            return true;
	        } else if(path.isLike(OBJECT_FINDER_SELECTS_INDEX_ENTRY_ACTIVITY)) {
		        // ObjectFinderSelectsIndexEntryActivity
	            Path segmentIdentity = new Path(
	                new String[]{"org:opencrx:kernel:activity1", "provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation()}
	            );
	            ObjectRecord objectFinder = this.retrieveObject(p, path.getParent());
	            FilterProperty[] filter = this.mapObjectFinderToFilter(objectFinder);
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                segmentIdentity.getChild("indexEntry"),
		                filter,
		                new AttributeSpecifier[]{
		                	new AttributeSpecifier(SystemAttributes.CREATED_AT, 0, SortOrder.DESCENDING.code())
		                }
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(OBJECT_FINDER_SELECTS_INDEX_ENTRY_ACCOUNT)) {
		        // ObjectFinderSelectsIndexEntryAccount
	            Path segmentIdentity = new Path(
	                new String[]{"org:opencrx:kernel:account1", "provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation()}
	            );
	            ObjectRecord objectFinder = this.retrieveObject(p, path.getParent());
	            FilterProperty[] filter = this.mapObjectFinderToFilter(objectFinder);
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                segmentIdentity.getChild("indexEntry"),
		                filter,
		                new AttributeSpecifier[]{
		                	new AttributeSpecifier(SystemAttributes.CREATED_AT, 0, SortOrder.DESCENDING.code())
		                }
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(OBJECT_FINDER_SELECTS_INDEX_ENTRY_CONTRACT)) {
		        // ObjectFinderSelectsIndexEntryContract
	            Path segmentIdentity = new Path(
	                new String[]{"org:opencrx:kernel:contract1", "provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation()}
	            );
	            ObjectRecord objectFinder = this.retrieveObject(p, path.getParent());
	            FilterProperty[] filter = this.mapObjectFinderToFilter(objectFinder);
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                segmentIdentity.getChild("indexEntry"),
		                filter,
		                new AttributeSpecifier[]{
		                	new AttributeSpecifier(SystemAttributes.CREATED_AT, 0, SortOrder.DESCENDING.code())
		                }
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(OBJECT_FINDER_SELECTS_INDEX_ENTRY_PRODUCT)) {
		        // ObjectFinderSelectsIndexEntryProduct
	            Path segmentIdentity = new Path(
	                new String[]{"org:opencrx:kernel:product1", "provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation()}
	            );
	            ObjectRecord objectFinder = this.retrieveObject(p, path.getParent());
	            FilterProperty[] filter = this.mapObjectFinderToFilter(objectFinder);
	            p.addFindRequest(
	            	 this.remapQuery(
	 	                request,
	 	                segmentIdentity.getChild("indexEntry"),
	 	                filter,
	 	                new AttributeSpecifier[]{
	 	                	new AttributeSpecifier(SystemAttributes.CREATED_AT, 0, SortOrder.DESCENDING.code())
	 	                }	                
	 	            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(OBJECT_FINDER_SELECTS_INDEX_ENTRY_DOCUMENT)) {
		        // ObjectFinderSelectsIndexEntryDocument
	            Path segmentIdentity = new Path(
	                new String[]{"org:opencrx:kernel:document1", "provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation()}
	            );
	            ObjectRecord objectFinder = this.retrieveObject(p, path.getParent());
	            FilterProperty[] filter = this.mapObjectFinderToFilter(objectFinder);
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                segmentIdentity.getChild("indexEntry"),
		                filter,
		                new AttributeSpecifier[]{
		                	new AttributeSpecifier(SystemAttributes.CREATED_AT, 0, SortOrder.DESCENDING.code())
		                }	                
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(OBJECT_FINDER_SELECTS_INDEX_ENTRY_BUILDING)) {
		        // ObjectFinderSelectsIndexEntryBuilding
	            Path segmentIdentity = new Path(
	                new String[]{"org:opencrx:kernel:building1", "provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation()}
	            );
	            ObjectRecord objectFinder = this.retrieveObject(p, path.getParent());
	            FilterProperty[] filter = this.mapObjectFinderToFilter(objectFinder);
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                segmentIdentity.getChild("indexEntry"),
		                filter,
		                new AttributeSpecifier[]{
		                	new AttributeSpecifier(SystemAttributes.CREATED_AT, 0, SortOrder.DESCENDING.code())
		                }	                
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(OBJECT_FINDER_SELECTS_INDEX_ENTRY_DEPOT)) {
		        // ObjectFinderSelectsIndexEntryDepot
	            Path segmentIdentity = new Path(
	                new String[]{"org:opencrx:kernel:depot1", "provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation()}
	            );
	            ObjectRecord objectFinder = this.retrieveObject(p, path.getParent());
	            FilterProperty[] filter = this.mapObjectFinderToFilter(objectFinder);
	            p.addFindRequest(
	            	this.remapQuery(
		                request,
		                segmentIdentity.getChild("indexEntry"),
		                filter,
		                new AttributeSpecifier[]{
		                	new AttributeSpecifier(SystemAttributes.CREATED_AT, 0, SortOrder.DESCENDING.code())
		                }	                
		            ), 
	            	response
	            );
	            return true;
	        } else if(path.isLike(MODEL_NAMESPACE_CONTAINS_ELEMENTS)) {
		        // NamespaceContainsElement
	        	p.addFindRequest(
	        		this.remapQuery(
		                request,
		                path.getPrefix(5).getChild("element"),
		                new FilterProperty[]{
		                    new FilterProperty(
		                        Quantifier.THERE_EXISTS.code(),
		                        "container",
		                        ConditionType.IS_IN.code(),
		                        new Object[]{path.getPrefix(7)}
		                    )                        
		                }
		            ), 
	        		response
	        	);
	            return true;
	        } else if(
	            path.isLike(CONTRACT_POSITION_HAS_MODIFICATION) ||
	            path.isLike(REMOVED_CONTRACT_POSITION_HAS_MODIFICATION)
	        ) {
		        // ContractPositionHasModification
	        	p.addFindRequest(
	        		 this.remapQuery(
	 	                request,
	 	                path.getPrefix(7).getChild("positionModification"),
	 	                new FilterProperty[]{
	 	                    new FilterProperty(
	 	                        Quantifier.THERE_EXISTS.code(),
	 	                        "involved",
	 	                        ConditionType.IS_IN.code(),
	 	                        new Object[]{path.getPrefix(9)}
	 	                    )                        
	 	                }
	 	            ), 
	        		response
	        	);
	            return true;
	        } else {
	        	return false;
	        }
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
    	}
    }

    /**
     * Map object finder to query.
     * 
     * @param objectFinder
     * @return
     * @throws ServiceException
     */
    public FilterProperty[] mapObjectFinderToFilter(
    	MappedRecord objectFinder
    ) throws ResourceException {
    	try {
	    	Object_2Facade objectFinderFacade = Facades.asObject(objectFinder);
	        List<FilterProperty> filter = new ArrayList<FilterProperty>();
	        String allWords = (String)objectFinderFacade.attributeValue("allWords");
	        if((allWords != null) && (allWords.length() > 0)) {
	            String words[] = allWords.split("[\\s,]");
	            for(int i = 0; i < words.length; i++) {
	                filter.add(
	                    new FilterProperty(
	                        Quantifier.THERE_EXISTS.code(),
	                        "keywords",
	                        ConditionType.IS_LIKE.code(),
	                        ".*" + words[i] + ".*"
	                    )
	                );
	            }
	        }
	        String withoutWords = (String)objectFinderFacade.attributeValue("withoutWords");
	        if((withoutWords != null) && (withoutWords.length() > 0)) {
	            String words[] = withoutWords.split("[\\s,]");
	            for(int i = 0; i < words.length; i++) {
	                filter.add(
	                    new FilterProperty(
	                        Quantifier.THERE_EXISTS.code(),
	                        "keywords",
	                        ConditionType.IS_UNLIKE.code(),
	                        ".*" + words[i] + ".*"
	                    )
	                );
	            }
	        }
	        String atLeastOneOfTheWords = (String)objectFinderFacade.attributeValue("atLeastOneOfTheWords");
	        if((atLeastOneOfTheWords != null) && (atLeastOneOfTheWords.length() > 0)) {
	            String words[] = atLeastOneOfTheWords.split("[\\s,]");
	            for(int i = 0; i < words.length; i++) {
	                words[i] = ".*" + words[i] + ".*";
	            }
	            filter.add(
	                new FilterProperty(
	                    Quantifier.THERE_EXISTS.code(),
	                    "keywords",
	                    ConditionType.IS_LIKE.code(),
	                    (Object[])words
	                )
	            );
	        }
	        // Return newest index entry only
	        String queryExtensionId = SystemAttributes.CONTEXT_PREFIX + Base.getInstance().getUidAsString() + ":";
	        filter.add(
	            new FilterProperty(
	                Quantifier.codeOf(null),
	                queryExtensionId + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                ConditionType.codeOf(null),
	                "(v.created_at IN (SELECT MAX(created_at) FROM OOCKE1_INDEXENTRY e WHERE e.indexed_object = v.indexed_object))"
	            )
	        );
	        filter.add(
	            new FilterProperty(
	                Quantifier.codeOf(null),
	                queryExtensionId + SystemAttributes.OBJECT_CLASS,
	                ConditionType.codeOf(null),
	                new Object[]{Database_1_Attributes.QUERY_EXTENSION_CLASS}
	            )
	        );
	        return filter.toArray(new FilterProperty[filter.size()]);
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );    		
    	}
    }

    /**
     * Map product filter to query.
     * 
     * @param productFilterIdentity
     * @param forCounting
     * @param delegatingInteraction
     * @return
     * @throws ServiceException
     */
    public FilterProperty[] getProductFilterProperties(
        Path productFilterIdentity,
        boolean forCounting
    ) throws ResourceException {
    	try {
    		DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
    		ResultRecord filterProperties = p.addFindRequest(
    			productFilterIdentity.getChild("productFilterProperty"),
    			FetchGroup.ALL
    		);  	
	        List<FilterProperty> filter = new ArrayList<FilterProperty>();
	        boolean hasQueryFilterClause = false;        
	        for(Object filterProperty: filterProperties) {
	        	Object_2Facade filterPropertyFacade = Facades.asObject((ObjectRecord)filterProperty);
	            String filterPropertyClass = filterPropertyFacade.getObjectClass();
	            Boolean isActive = (Boolean)filterPropertyFacade.attributeValue("isActive");
	            if((isActive != null) && isActive.booleanValue()) {
	                // Query filter
	                if("org:opencrx:kernel:product1:ProductQueryFilterProperty".equals(filterPropertyClass)) {     
	                    String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + Products.getInstance().getUidAsString() + ":";
	                    // Clause and class
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                            ConditionType.codeOf(null),
	                            (forCounting ? Database_1_Attributes.HINT_COUNT : "") + filterPropertyFacade.attributeValue("clause") 
	                        )
	                    );
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + SystemAttributes.OBJECT_CLASS,
	                            ConditionType.codeOf(null),
	                            new Object[]{Database_1_Attributes.QUERY_EXTENSION_CLASS}
	                        )
	                    );
	                    // stringParam
	                    List<Object> values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // integerParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // decimalParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // booleanParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM)) {
	                        values.add(Datatypes.create(XMLGregorianCalendar.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateTimeParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM)) {
	                        values.add(Datatypes.create(Date.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    hasQueryFilterClause = true;
	                } else {
		                // Attribute filter
	                    // Get filterOperator, filterQuantor
	                    short filterOperator = filterPropertyFacade.attributeValuesAsList("filterOperator").isEmpty()
	                        ? ConditionType.IS_IN.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterOperator")).shortValue();
	                    filterOperator = filterOperator == 0
	                        ? ConditionType.IS_IN.code()
	                        : filterOperator;
	                    short filterQuantor = filterPropertyFacade.attributeValuesAsList("filterQuantor").isEmpty()
	                        ? Quantifier.THERE_EXISTS.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterQuantor")).shortValue();
	                    filterQuantor = filterQuantor == 0
	                        ? Quantifier.THERE_EXISTS.code()
	                        : filterQuantor;
	                    if("org:opencrx:kernel:product1:ProductClassificationFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "classification",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("classification").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:product1:DefaultSalesTaxTypeFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "salesTaxType",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("salesTaxType").toArray()                    
	                            )
	                        );
	                    } else if("org:opencrx:kernel:product1:CategoryFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "category",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("category").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:product1:PriceUomFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "priceUom",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("priceUom").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:product1:DisabledFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "disabled",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("disabled").toArray()
	                            )
	                        );
	                    }
	                }
	            }
	        }
	        if(!hasQueryFilterClause && forCounting) {
	            String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + Products.getInstance().getUidAsString() + ":";
	            // Clause and class
	            filter.add(
	                new FilterProperty(
	                    Quantifier.codeOf(null),
	                    queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                    ConditionType.codeOf(null),
	                    new Object[]{
	                        Database_1_Attributes.HINT_COUNT + "(1=1)"
	                    }
	                )
	            );
	            filter.add(
	                new FilterProperty(
	                    Quantifier.codeOf(null),
	                    queryFilterContext + SystemAttributes.OBJECT_CLASS,
	                    ConditionType.codeOf(null),
	                    new Object[]{Database_1_Attributes.QUERY_EXTENSION_CLASS}
	                )
	            );            
	        }        
	        return filter.toArray(new FilterProperty[filter.size()]);
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
    	}
    }

    /**
     * Map ContractFilter to query.
     * 
     * @param contractFilterIdentity
     * @param delegatingInteraction
     * @return
     * @throws ServiceException
     */
    public FilterProperty[] getContractFilterProperties(
        Path contractFilterIdentity
    ) throws ResourceException {
    	try {
    		DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
	    	ResultRecord filterProperties = p.addFindRequest(
	    		contractFilterIdentity.getChild("filterProperty"),
	    		FetchGroup.ALL
	    	);
	        List<FilterProperty> filter = new ArrayList<FilterProperty>();
	        for(Object filterProperty: filterProperties) {
	        	Object_2Facade filterPropertyFacade = Facades.asObject((ObjectRecord)filterProperty);
	            String filterPropertyClass = filterPropertyFacade.getObjectClass();
	            Boolean isActive = (Boolean)filterPropertyFacade.attributeValue("isActive");            
	            if((isActive != null) && isActive.booleanValue()) {
	                // Query filter
	                if("org:opencrx:kernel:contract1:ContractQueryFilterProperty".equals(filterPropertyClass)) {     
	                    String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + Contracts.getInstance().getUidAsString() + ":";
	                    // Clause and class
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                            ConditionType.codeOf(null),
	                            resolveQueryClause(
	                            	(String)filterPropertyFacade.attributeValue("clause"), 
	                            	contractFilterIdentity.getParent(),
	                            	"filterProperty"
	                            )	                            	                            
	                        )
	                    );
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + SystemAttributes.OBJECT_CLASS,
	                            ConditionType.codeOf(null),
	                            Database_1_Attributes.QUERY_EXTENSION_CLASS
	                        )
	                    );
	                    // stringParam
	                    List<Object> values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // integerParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // decimalParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // booleanParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM)) {
	                        values.add(Datatypes.create(XMLGregorianCalendar.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateTimeParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM)) {
	                        values.add(Datatypes.create(Date.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                } else {
		                // Attribute filter
	                    // Get filterOperator, filterQuantor
	                    short filterOperator = filterPropertyFacade.attributeValuesAsList("filterOperator").isEmpty()
	                        ? ConditionType.IS_IN.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterOperator")).shortValue();
	                    filterOperator = filterOperator == 0
	                        ? ConditionType.IS_IN.code()
	                        : filterOperator;
	                    short filterQuantor = filterPropertyFacade.attributeValuesAsList("filterQuantor").isEmpty()
	                        ? Quantifier.THERE_EXISTS.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterQuantor")).shortValue();
	                    filterQuantor = filterQuantor == 0
	                        ? Quantifier.THERE_EXISTS.code()
	                        : filterQuantor;
	                    if("org:opencrx:kernel:contract1:ContractTypeFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                SystemAttributes.OBJECT_CLASS,
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("contractType").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:contract1:ContractStateFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "contractState",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("contractState").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:contract1:ContractPriorityFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "priority",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("priority").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:contract1:TotalAmountFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "totalAmount",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("totalAmount").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:contract1:CustomerFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "customer",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("customer").toArray()                    
	                            )
	                        );
	                    } else if("org:opencrx:kernel:contract1:SupplierFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "supplier",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("supplier").toArray()                    
	                            )
	                        );
	                    } else if("org:opencrx:kernel:contract1:SalesRepFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "salesRep",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("salesRep").toArray()                    
	                            )
	                        );
	                    } else if("org:opencrx:kernel:contract1:DisabledFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "disabled",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("disabled").toArray()
	                            )
	                        );
	                    }
	                }
	            }
	        }        
	        return filter.toArray(new FilterProperty[filter.size()]);
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
    	}
    }

    /**
     * Map ActivityFilter to query.
     * 
     * @param activityFilterIdentity
     * @param delegatingInteraction
     * @return
     * @throws ServiceException
     */
    public FilterProperty[] getActivityFilterProperties(
        Path activityFilterIdentity
    ) throws ResourceException {
    	try {
    		DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
    		ResultRecord filterProperties = p.addFindRequest(
    			activityFilterIdentity.getChild("filterProperty"),
    			FetchGroup.ALL
    		);
	        List<FilterProperty> filter = new ArrayList<FilterProperty>();
	        for(Object filterProperty: filterProperties) {
	        	Object_2Facade filterPropertyFacade = Facades.asObject((ObjectRecord)filterProperty);
	            String filterPropertyClass = filterPropertyFacade.getObjectClass();
	            Boolean isActive = (Boolean)filterPropertyFacade.attributeValue("isActive");
	            if((isActive != null) && isActive.booleanValue()) {
	                // Query filter
	                if("org:opencrx:kernel:activity1:ActivityQueryFilterProperty".equals(filterPropertyClass)) {     
	                    String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + Activities.getInstance().getUidAsString() + ":";
	                    // Clause and class
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                            ConditionType.codeOf(null),
	                            resolveQueryClause(
	                            	(String)filterPropertyFacade.attributeValue("clause"), 
	                            	activityFilterIdentity.getParent(),
	                            	"filterProperty"
	                            )	                            	                            
	                        )
	                    );
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + SystemAttributes.OBJECT_CLASS,
	                            ConditionType.codeOf(null),
	                            Database_1_Attributes.QUERY_EXTENSION_CLASS
	                        )
	                    );
	                    // stringParam
	                    List<Object> values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // integerParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // decimalParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // booleanParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM)) {
	                        values.add(Datatypes.create(XMLGregorianCalendar.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateTimeParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM)) {
	                        values.add(Datatypes.create(Date.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                } else {
		                // Attribute filter
	                    // Get filterOperator, filterQuantor
	                    short filterOperator = filterPropertyFacade.attributeValuesAsList("filterOperator").isEmpty()
	                        ? ConditionType.IS_IN.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterOperator")).shortValue();
	                    filterOperator = filterOperator == 0
	                        ? ConditionType.IS_IN.code()
	                        : filterOperator;
	                    short filterQuantor = filterPropertyFacade.attributeValuesAsList("filterQuantor").isEmpty()
	                        ? Quantifier.THERE_EXISTS.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterQuantor")).shortValue();
	                    filterQuantor = filterQuantor == 0
	                        ? Quantifier.THERE_EXISTS.code()
	                        : filterQuantor;
	                    if("org:opencrx:kernel:activity1:ActivityStateFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "activityState",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("activityState").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:ScheduledStartFilterProperty".equals(filterPropertyClass)) {
	                        if(filterPropertyFacade.attributeValuesAsList("scheduledStart").isEmpty()) {
	                        	filterPropertyFacade.attributeValuesAsList("scheduledStart").add(
	                                DateTimeFormat.BASIC_UTC_FORMAT.format(new Date())
	                            );
	                        }
	                        if(!filterPropertyFacade.attributeValuesAsList("offsetInHours").isEmpty()) {
	                            int offsetInHours = ((Number)filterPropertyFacade.attributeValue("offsetInHours")).intValue();
	                            for(int j = 0; j < filterPropertyFacade.attributeValuesAsList("scheduledStart").size(); j++) {
	                                try {
	                                    GregorianCalendar date = new GregorianCalendar();
	                                    date.setTime(
	                                        DateTimeFormat.BASIC_UTC_FORMAT.parse((String)filterPropertyFacade.attributeValuesAsList("scheduledStart").get(j))
	                                    );
	                                    date.add(GregorianCalendar.HOUR_OF_DAY, offsetInHours);
	                                    filterPropertyFacade.attributeValuesAsList("scheduledStart").set(
	                                        j, 
	                                        DateTimeFormat.BASIC_UTC_FORMAT.format(date.getTime())
	                                    );
	                                } 
	                                catch(Exception e) {}
	                            }
	                        }
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "scheduledStart",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("scheduledStart").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:ScheduledEndFilterProperty".equals(filterPropertyClass)) {
	                        if(filterPropertyFacade.attributeValuesAsList("scheduledEnd").isEmpty()) {
	                        	filterPropertyFacade.attributeValuesAsList("scheduledEnd").add(
	                                DateTimeFormat.BASIC_UTC_FORMAT.format(new Date())
	                            );
	                        }
	                        if(!filterPropertyFacade.attributeValuesAsList("offsetInHours").isEmpty()) {
	                            int offsetInHours = ((Number)filterPropertyFacade.attributeValue("offsetInHours")).intValue();
	                            for(int j = 0; j < filterPropertyFacade.attributeValuesAsList("scheduledEnd").size(); j++) {
	                                try {
	                                    GregorianCalendar date = new GregorianCalendar();
	                                    date.setTime(
	                                        DateTimeFormat.BASIC_UTC_FORMAT.parse((String)filterPropertyFacade.attributeValuesAsList("scheduledEnd").get(j))
	                                    );
	                                    date.add(GregorianCalendar.HOUR_OF_DAY, offsetInHours);
	                                    filterPropertyFacade.attributeValuesAsList("scheduledEnd").set(
	                                        j, 
	                                        DateTimeFormat.BASIC_UTC_FORMAT.format(date.getTime())
	                                    );
	                                } 
	                                catch(Exception e) {}
	                            }
	                        }
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "scheduledEnd",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("scheduledEnd").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:ActivityProcessStateFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "processState",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("processState").toArray()
	                            )                    
	                        );
	                    } else if("org:opencrx:kernel:activity1:ActivityTypeFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "activityType",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("activityType").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:AssignedToFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "assignedTo",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("contact").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:ReportedByContactFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "reportingContact",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("contact").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:ReportedByAccountFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "reportingAccount",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("account").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:ActivityNumberFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "activityNumber",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("activityNumber").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:activity1:DisabledFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "disabled",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("disabled").toArray()
	                            )
	                        );
	                    }
	                }
	            }
	        }
	        return filter.toArray(new FilterProperty[filter.size()]);
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
    	}
    }

    /**
     * Map DocumentFilter to query.
     * 
     * @param documentFilterIdentity
     * @param delegatingInteraction
     * @return
     * @throws ServiceException
     */
    public FilterProperty[] getDocumentFilterProperties(
        Path documentFilterIdentity
    ) throws ResourceException {
    	try {
    		DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
    		ResultRecord filterProperties = p.addFindRequest(
    			documentFilterIdentity.getChild("filterProperty"),
    			FetchGroup.ALL
    		);
	        List<FilterProperty> filter = new ArrayList<FilterProperty>();
	        for(Object filterProperty: filterProperties) {
	        	Object_2Facade filterPropertyFacade = Facades.asObject((ObjectRecord)filterProperty);
	            String filterPropertyClass = filterPropertyFacade.getObjectClass();
	            Boolean isActive = (Boolean)filterPropertyFacade.attributeValue("isActive");
	            if((isActive != null) && isActive.booleanValue()) {
	                // Query filter
	                if("org:opencrx:kernel:document1:DocumentQueryFilterProperty".equals(filterPropertyClass)) {     
	                    String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + Activities.getInstance().getUidAsString() + ":";
	                    // Clause and class
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                            ConditionType.codeOf(null),
	                            resolveQueryClause(
	                            	(String)filterPropertyFacade.attributeValue("clause"), 
	                            	documentFilterIdentity.getParent(),
	                            	"filterProperty"
	                            )	                            	                            
	                        )
	                    );
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + SystemAttributes.OBJECT_CLASS,
	                            ConditionType.codeOf(null),
	                            Database_1_Attributes.QUERY_EXTENSION_CLASS
	                        )
	                    );
	                    // stringParam
	                    List<Object> values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // integerParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // decimalParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // booleanParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM)) {
	                        values.add(Datatypes.create(XMLGregorianCalendar.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateTimeParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM)) {
	                        values.add(Datatypes.create(Date.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                } else {
		                // Attribute filter
	                    // Get filterOperator, filterQuantor
	                    short filterOperator = filterPropertyFacade.attributeValuesAsList("filterOperator").isEmpty()
	                        ? ConditionType.IS_IN.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterOperator")).shortValue();
	                    filterOperator = filterOperator == 0
	                        ? ConditionType.IS_IN.code()
	                        : filterOperator;
	                    short filterQuantor = filterPropertyFacade.attributeValuesAsList("filterQuantor").isEmpty()
	                        ? Quantifier.THERE_EXISTS.code()
	                        : ((Number)filterPropertyFacade.attributeValue("filterQuantor")).shortValue();
	                    filterQuantor = filterQuantor == 0
	                        ? Quantifier.THERE_EXISTS.code()
	                        : filterQuantor;
	                    if("org:opencrx:kernel:document1:DocumentStateFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "documentState",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("documentState").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:document1:DocumentTypeFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "documentType",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("documentType").toArray()
	                            )                    
	                        );
	                    } else if("org:opencrx:kernel:document1:DocumentFolderFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "folder",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("documentFolder").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:document1:DocumentNameFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "name",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("namePattern").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:document1:DocumentLanguageFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "contentLanguage",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("contentLanguage").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:document1:DisabledFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "disabled",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("disabled").toArray()
	                            )
	                        );
	                    }
	                }
	            }
	        }
	        return filter.toArray(new FilterProperty[filter.size()]);
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
    	}
    }

    /**
     * Map AddressFilter to query.
     * 
     * @param addressFilterIdentity
     * @param delegatingInteraction
     * @return
     * @throws ServiceException
     */
    public FilterProperty[] getAddressFilterProperties(
        Path addressFilterIdentity
    ) throws ResourceException {    	
    	try {
    		DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
    		ResultRecord filterProperties = p.addFindRequest(
    			addressFilterIdentity.getChild("addressFilterProperty"),
    			FetchGroup.ALL
    		);
	        List<FilterProperty> filter = new ArrayList<FilterProperty>();
	        for(Object filterProperty: filterProperties) {
	        	Object_2Facade filterPropertyFacade = Facades.asObject((ObjectRecord)filterProperty);
	            String filterPropertyClass = filterPropertyFacade.getObjectClass();
	            Boolean isActive = (Boolean)filterPropertyFacade.attributeValue("isActive");            
	            if((isActive != null) && isActive.booleanValue()) {
	                // Query filter
	                if("org:opencrx:kernel:account1:AddressQueryFilterProperty".equals(filterPropertyClass)) {     
	                    String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + Accounts.getInstance().getUidAsString() + ":";
	                    // Clause and class
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                            ConditionType.codeOf(null),
	                            resolveQueryClause(
	                            	(String)filterPropertyFacade.attributeValue("clause"), 
	                            	addressFilterIdentity.getParent(),
	                            	"addressFilterProperty"
	                            )	                            
	                        )
	                    );
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + SystemAttributes.OBJECT_CLASS,
	                            ConditionType.codeOf(null),
	                            Database_1_Attributes.QUERY_EXTENSION_CLASS
	                        )
	                    );
	                    // stringParam
	                    List<Object> values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // integerParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // decimalParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // booleanParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM)) {
	                        values.add(Datatypes.create(XMLGregorianCalendar.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateTimeParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM)) {
	                        values.add(Datatypes.create(Date.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                } else {
		                // Attribute filter
	                    // Get filterOperator, filterQuantor
	                    short filterOperator = filterPropertyFacade.attributeValuesAsList("filterOperator").isEmpty() 
	                    	? ConditionType.IS_IN.code() 
	                    	: ((Number)filterPropertyFacade.attributeValue("filterOperator")).shortValue();
	                    filterOperator = filterOperator == 0 
	                    	? ConditionType.IS_IN.code() 
	                    	: filterOperator;
	                    short filterQuantor = filterPropertyFacade.attributeValuesAsList("filterQuantor").isEmpty() 
	                    	? Quantifier.THERE_EXISTS.code() 
	                    	: ((Number)filterPropertyFacade.attributeValuesAsList("filterQuantor").get(0)).shortValue();
	                    filterQuantor = filterQuantor == 0 
	                    	? Quantifier.THERE_EXISTS.code() 
	                    	: filterQuantor;	                    
	                    if("org:opencrx:kernel:account1:AddressCategoryFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "category",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("category").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:AddressTypeFilterProperty".equals(filterPropertyClass)) {
	                        List<String> addressTypes = new ArrayList<String>();
	                        for(Iterator<Object> j = filterPropertyFacade.attributeValuesAsList("addressType").iterator(); j.hasNext(); ) {
	                            int addressType = ((Number)j.next()).intValue();
	                            addressTypes.add(
	                                Addresses.ADDRESS_TYPES[addressType]
	                            );
	                        }
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                SystemAttributes.OBJECT_INSTANCE_OF,
	                                filterOperator,
	                                addressTypes.toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:AddressMainFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "isMain",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("isMain").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:AddressUsageFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "usage",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("usage").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:AddressDisabledFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "disabled",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("disabled").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:AddressAccountMembershipFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                            	Quantifier.THERE_EXISTS.code(),
	                                "account",
	                                ConditionType.IS_IN.code(),
                            		new Filter(
	                            		new IsInCondition(
	        	                    		Quantifier.THERE_EXISTS,
	        	                    		"accountMembership",
	        	                    		true,
	        	                    		new Filter(
	        	                    			new IsInCondition(
	        	                    				Quantifier.valueOf(filterQuantor),
	        	    	                    		"accountFrom",
	        	    	                    		filterOperator == ConditionType.IS_IN.code(),
	        	    	                    		filterPropertyFacade.attributeValuesAsList("account").toArray()
	        	    	                    	),
	        	                    			new IsInCondition(
	        	                    				Quantifier.FOR_ALL,
	        	    	                    		"distance",
	        	    	                    		true,
	        	    	                    		(short)-1
	        	    	                    	)
	        	                    		)
	        	                    	)
                            		)
	                            )
	                        );
	                    }	                    
	                }
	            }
	        }
	        return filter.toArray(new FilterProperty[filter.size()]);
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
    	}
    }

    /**
     * Resolve query clause. If clause contains place holders of the form 
     * ${"filter name"."filter property name"} they are resolved recursively.
     * 
     * @param filterReference
     * @param propertyKind
     * @param clause
     * @param delegatingInteraction
     * @return
     * @throws ServiceException
     * @throws ResourceException
     */
    public String resolveQueryClause(
    	String clause,
    	Path filterReference,
    	String propertyKind
    ) throws ResourceException {
    	try {
	    	DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
	    	while(true) {
	    		int placeHolderStart = clause.indexOf("${");
	    		int placeHolderEnd = clause.indexOf("}", placeHolderStart);
	    		if(placeHolderEnd <= placeHolderStart) {
	    			break;
	    		}
	    		String replacement = "(null=null)";
	    		String placeHolder[] = clause.substring(placeHolderStart + 2, placeHolderEnd).split("(?<!\\\\)\\.");
	    		// Place holder is of the form ${"filter name"."filter property name"} or {filter id.filter property id}
	    		// Lookup a filter and filter property with given names or ids
	    		if(placeHolder.length == 2) {
	    			String filterName = placeHolder[0].replace("\\.", ".");
	    			Object_2Facade filter = null;
	    			if(filterName.startsWith("\"")) {
	    				filterName = filterName.substring(1);
	    				filterName = filterName.endsWith("\"") ? filterName.substring(0, filterName.length() - 1) : filterName;
	    				@SuppressWarnings("deprecation")
	                    ResultRecord filterFindReply = p.addFindRequest(
	    					filterReference, 
	    					new FilterProperty[]{
	        	                new FilterProperty(
	        	                    Quantifier.THERE_EXISTS.code(),
	        	                    "name",
	        	                    ConditionType.IS_IN.code(),
	        	                    new Object[]{filterName}	                    
	        	                )    	            	
	        	            }
	    				);
	        	    	if(!filterFindReply.isEmpty()) {
	        	    		filter = Facades.asObject((ObjectRecord)filterFindReply.get(0));
	        	    	}
	    			} else {
	    				try {
	    					filter = Facades.asObject(this.retrieveObject(p, filterReference.getChild(filterName)));
	    				} catch(Exception ignore) {}
	    			}
	    	    	if(filter != null) {
	        			String filterPropertyName = placeHolder[1].replace("\\.", ".");
	        			Object_2Facade filterProperty = null;
	        			if(filterPropertyName.startsWith("\"")) {
	            			filterPropertyName = filterPropertyName.substring(1);
	            			filterPropertyName = filterPropertyName.endsWith("\"") ? filterPropertyName.substring(0, filterPropertyName.length() - 1) : filterPropertyName;        			
		    	    		// Filter found. Now find property
	            			@SuppressWarnings("deprecation")
	                        ResultRecord filterPropertyFindReply = p.addFindRequest(
	            				filter.getPath().getChild(propertyKind), 
	            				new FilterProperty[]{
		        	                new FilterProperty(
		        	                    Quantifier.THERE_EXISTS.code(),
		        	                    "name",
		        	                    ConditionType.IS_IN.code(),
		        	                    new Object[]{filterPropertyName}   
		        	                )    	            	
		        	            }
	            			);
		        	    	if(!filterPropertyFindReply.isEmpty()) {
		        	    		filterProperty = Facades.asObject((ObjectRecord)filterPropertyFindReply.get(0));
		        	    	}
	        			} else {
	        				try {
	            				try {
	            					filterProperty = Facades.asObject(this.retrieveObject(p, filter.getPath().getDescendant(propertyKind, filterPropertyName)));
	            				} catch(Exception ignore) {}        					
	        				} catch(Exception ignore) {}
	        			}
	    	    		if(filterProperty != null) {
	    	    			if(filterProperty.attributeValue("clause") != null) {
	    	    				replacement = resolveQueryClause(
	    	    					(String)filterProperty.attributeValue("clause"),
	    	    					filterReference,
	    	    					propertyKind
	    	    				);    	    				
	    	    			}
	    	    		}
	    	    	}
	    		}
	    		clause = 
	    			clause.substring(0, placeHolderStart) + 
	    			replacement + 
	    			clause.substring(placeHolderEnd + 1);
	    	}
	    	return clause;
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
        }
    }

    /**
     * Map AccountFilter to query.
     * 
     * @param accountFilterIdentity
     * @param delegatingInteraction
     * @return
     * @throws ServiceException
     */
    public FilterProperty[] getAccountFilterProperties(
        Path accountFilterIdentity      
    ) throws ResourceException {
    	try {
    		DataproviderRequestProcessor p = this.getDataproviderRequestProcessor();
    		ResultRecord filterProperties = p.addFindRequest(
    			accountFilterIdentity.getChild("accountFilterProperty"),
    			FetchGroup.ALL
    		);
	        List<FilterProperty> filter = new ArrayList<FilterProperty>();
	        for(Object filterProperty: filterProperties) {
	        	Object_2Facade filterPropertyFacade = Facades.asObject((ObjectRecord)filterProperty);
	            String filterPropertyClass = filterPropertyFacade.getObjectClass();
	            Boolean isActive = (Boolean)filterPropertyFacade.attributeValue("isActive");            
	            if((isActive != null) && isActive.booleanValue()) {
	                // Query filter
	                if("org:opencrx:kernel:account1:AccountQueryFilterProperty".equals(filterPropertyClass)) {     
	                    String queryFilterContext = SystemAttributes.CONTEXT_PREFIX + Accounts.getInstance().getUidAsString() + ":";
	                    // Clause and class
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_CLAUSE,
	                            ConditionType.codeOf(null),
	                            resolveQueryClause(
	                            	(String)filterPropertyFacade.attributeValue("clause"), 
	                            	accountFilterIdentity.getParent(),
	                            	"accountFilterProperty"
	                            )
	                        )
	                    );
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + SystemAttributes.OBJECT_CLASS,
	                            ConditionType.codeOf(null),
	                            Database_1_Attributes.QUERY_EXTENSION_CLASS
	                        )
	                    );
	                    // stringParam
	                    List<Object> values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_STRING_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // integerParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_INTEGER_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // decimalParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DECIMAL_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // booleanParam
	                    values = filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM);
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_BOOLEAN_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM)) {
	                        values.add(Datatypes.create(XMLGregorianCalendar.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATE_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                    // dateTimeParam
	                    values = new ArrayList<Object>();
	                    for(Object value: filterPropertyFacade.attributeValuesAsList(Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM)) {
	                        values.add(Datatypes.create(Date.class, value));
	                    }
	                    filter.add(
	                        new FilterProperty(
	                            Quantifier.codeOf(null),
	                            queryFilterContext + Database_1_Attributes.QUERY_EXTENSION_DATETIME_PARAM,
	                            ConditionType.codeOf(null),
	                            values.toArray()
	                        )
	                    );
	                } else {
		                // Attribute filter
	                    // Get filterOperator, filterQuantor
	                    short filterOperator = filterPropertyFacade.attributeValuesAsList("filterOperator").isEmpty() 
	                    	? ConditionType.IS_IN.code() 
	                    	: ((Number)filterPropertyFacade.attributeValue("filterOperator")).shortValue();
	                    filterOperator = filterOperator == 0 
	                    	? ConditionType.IS_IN.code() 
	                    	: filterOperator;
	                    short filterQuantor = filterPropertyFacade.attributeValuesAsList("filterQuantor").isEmpty() 
	                    	? Quantifier.THERE_EXISTS.code() 
	                    	: ((Number)filterPropertyFacade.attributeValue("filterQuantor")).shortValue();
	                    filterQuantor = filterQuantor == 0 
	                    	? Quantifier.THERE_EXISTS.code() 
	                    	: filterQuantor;	                    
	                    if("org:opencrx:kernel:account1:AccountTypeFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "accountType",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("accountType").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:AccountCategoryFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "accountCategory",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("accountCategory").toArray()                    
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:CategoryFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "category",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("category").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:DisabledFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                                filterQuantor,
	                                "disabled",
	                                filterOperator,
	                                filterPropertyFacade.attributeValuesAsList("disabled").toArray()
	                            )
	                        );
	                    } else if("org:opencrx:kernel:account1:AccountMembershipFilterProperty".equals(filterPropertyClass)) {
	                        filter.add(
	                            new FilterProperty(
	                            	Quantifier.THERE_EXISTS.code(),
	                                "accountMembership",
	                                ConditionType.IS_IN.code(),
    	                    		new Filter(
    	                    			new IsInCondition(
    	                    				Quantifier.valueOf(filterQuantor),
    	    	                    		"accountFrom",
    	    	                    		filterOperator == ConditionType.IS_IN.code(),
    	    	                    		filterPropertyFacade.attributeValuesAsList("account").toArray()
    	    	                    	),
    	                    			new IsInCondition(
    	                    				Quantifier.FOR_ALL,
    	    	                    		"distance",
    	    	                    		true,
    	    	                    		(short)-1
    	    	                    	)
    	                    		)
                           		)
	                        );
	                    }
	                }
	            }
	        }
	        return filter.toArray(new FilterProperty[filter.size()]);
    	} catch(ServiceException e) {
            throw ResourceExceptions.initHolder(
                new ResourceException(
                    BasicException.newEmbeddedExceptionStack(e)
                )
            );
        }
    }

	//-------------------------------------------------------------------------
    // Patterns for derived find requests
	//-------------------------------------------------------------------------
    private static final Path COMPOUND_BOOKING_HAS_BOOKINGS = new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/:*/segment/:*/cb/:*/booking");
    private static final Path DEPOT_POSITION_HAS_BOOKINGS = new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/:*/segment/:*/entity/:*/depotHolder/:*/depot/:*/position/:*/booking");
    private static final Path DEPOT_POSITION_HAS_SIMPLE_BOOKINGS = new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/:*/segment/:*/entity/:*/depotHolder/:*/depot/:*/position/:*/simpleBooking");
    private static final Path CLASSIFIER_CLASSIFIES_TYPED_ELEMENT = new Path("xri://@openmdx*org.opencrx.kernel.model1/provider/:*/segment/:*/element/:*/typedElement");
    private static final Path DEPOT_REPORT_ITEM_HAS_BOOKING_ITEMS = new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/:*/segment/:*/entity/:*/depotHolder/:*/depot/:*/report/:*/itemPosition/:*/itemBooking");
    private static final Path DEPOT_GROUP_CONTAINS_DEPOTS = new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/:*/segment/:*/entity/:*/depotGroup/:*/depot");
    private static final Path DEPOT_GROUP_CONTAINS_DEPOT_GROUPS = new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/:*/segment/:*/entity/:*/depotGroup/:*/depotGroup");
    private static final Path DEPOT_ENTITY_CONTAINS_DEPOTS = new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/:*/segment/:*/entity/:*/depot");
    private static final Path FOLDER_CONTAINS_FOLDERS = new Path("xri://@openmdx*org.opencrx.kernel.document1/provider/:*/segment/:*/folder/:*/subFolder");
    private static final Path MODEL_NAMESPACE_CONTAINS_ELEMENTS = new Path("xri://@openmdx*org.opencrx.kernel.model1/provider/:*/segment/:*/element/:*/content");
    private static final Path GLOBAL_FILTER_INCLUDES_ACTIVITY = new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityFilter/:*/filteredActivity");
    private static final Path GLOBAL_FILTER_INCLUDES_ACCOUNT = new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/accountFilter/:*/filteredAccount");
    private static final Path GLOBAL_FILTER_INCLUDES_ADDRESS = new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/:*/segment/:*/addressFilter/:*/filteredAddress");
    private static final Path GLOBAL_FILTER_INCLUDES_CONTRACT = new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/contractFilter/:*/filteredContract");
    private static final Path GLOBAL_FILTER_INCLUDES_PRODUCT = new Path("xri://@openmdx*org.opencrx.kernel.product1/provider/:*/segment/:*/productFilter/:*/filteredProduct");
    private static final Path GLOBAL_FILTER_INCLUDES_DOCUMENT = new Path("xri://@openmdx*org.opencrx.kernel.document1/provider/:*/segment/:*/documentFilter/:*/filteredDocument");
    private static final Path ACTIVITY_FILTER_INCLUDES_ACTIVITY = new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/:*/:*/activityFilter/:*/filteredActivity");
    private static final Path CONTRACT_FILTER_INCLUDES_CONTRACT = new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/contractGroup/:*/contractFilter/:*/filteredContract");
    private static final Path PRODUCT_PRICE_LEVEL_HAS_FILTERED_ACCOUNT = new Path("xri://@openmdx*org.opencrx.kernel.product1/provider/:*/segment/:*/priceLevel/:*/filteredAccount");
    private static final Path PRODUCT_PRICE_LEVEL_INCLUDES_FILTERED_PRODUCT = new Path("xri://@openmdx*org.opencrx.kernel.product1/provider/:*/segment/:*/priceLevel/:*/filteredProduct");
    private static final Path PRODUCT_PRICE_LEVEL_HAS_ASSIGNED_PRICE_LIST_ENTRY = new Path("xri://@openmdx*org.opencrx.kernel.product1/provider/:*/segment/:*/priceLevel/:*/priceListEntry");
    private static final Path CONTRACT_POSITION_HAS_MODIFICATION = new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/:*/:*/position/:*/positionModification");
    private static final Path REMOVED_CONTRACT_POSITION_HAS_MODIFICATION = new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/:*/segment/:*/:*/:*/removedPosition/:*/positionModification");
    private static final Path OBJECT_FINDER_SELECTS_INDEX_ENTRY_ACTIVITY = new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/objectFinder/:*/indexEntryActivity");
    private static final Path OBJECT_FINDER_SELECTS_INDEX_ENTRY_ACCOUNT = new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/objectFinder/:*/indexEntryAccount");
    private static final Path OBJECT_FINDER_SELECTS_INDEX_ENTRY_CONTRACT = new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/objectFinder/:*/indexEntryContract");
    private static final Path OBJECT_FINDER_SELECTS_INDEX_ENTRY_PRODUCT = new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/objectFinder/:*/indexEntryProduct");
    private static final Path OBJECT_FINDER_SELECTS_INDEX_ENTRY_DOCUMENT = new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/objectFinder/:*/indexEntryDocument");
    private static final Path OBJECT_FINDER_SELECTS_INDEX_ENTRY_BUILDING = new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/objectFinder/:*/indexEntryBuilding");
    private static final Path OBJECT_FINDER_SELECTS_INDEX_ENTRY_DEPOT = new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/objectFinder/:*/indexEntryDepot");
    private static final Path SALES_VOLUME_BUDGET_POSITION_INCLUDES_FILTERED_PRODUCT = new Path("xri://@openmdx*org.opencrx.kernel.forecast1/provider/:*/segment/:*/budget/:*/position/:*/filteredProduct");

}

//--- End of File -----------------------------------------------------------
