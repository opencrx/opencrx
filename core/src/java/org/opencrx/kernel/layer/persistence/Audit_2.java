/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: openCRX audit plugin
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
package org.opencrx.kernel.layer.persistence;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jdo.FetchGroup;
import javax.resource.ResourceException;
import javax.resource.cci.Interaction;
import javax.resource.cci.MappedRecord;

import org.opencrx.kernel.generic.SecurityKeys;
import org.openmdx.application.dataprovider.cci.DataproviderOperations;
import org.openmdx.application.dataprovider.cci.FilterProperty;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.collection.TreeSparseArray;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.resource.spi.ResourceExceptions;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.MessageRecord;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.RestConnection;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.w3c.cci2.SparseArray;
import org.w3c.format.DateTimeFormat;

/**
 * This plugin creates audit entries for modified objects.
 */
public class Audit_2 extends Indexed_2 {

	public Audit_2(
	) {
		super();
	}
	
    /* (non-Javadoc)
     * @see org.opencrx.kernel.layer.persistence.Indexed_1#getInteraction(javax.resource.cci.Connection)
     */
	@Override
    public Interaction getInteraction(
        RestConnection connection
    ) throws ResourceException {
        return new RestInteraction(connection);
    }

    /**
     * Test whether object is instance of Auditee.
     * 
     * @param object
     * @return
     * @throws ServiceException
     */
    protected boolean isAuditee(
      ObjectRecord object
    ) throws ServiceException {
        String objectClass = Object_2Facade.getObjectClass(object);
        return this.getModel().isSubtypeOf(
            objectClass,
            "org:opencrx:kernel:base:Auditee"
        );
    }

    /**
     * Test whether object is instance of BasicObject.
     * 
     * @param object
     * @return
     */
    protected boolean isInstanceOfBasicObject(
    	MappedRecord object
    ) {
        return Object_2Facade.getPath(object).size() > 5;
    }

    /**
     * Get qualified principal name.
     * 
     * @param accessPath
     * @param principalName
     * @return
     */
    protected String getQualifiedPrincipalName(
        Path accessPath,
        String principalName
    ) {
        return accessPath.getSegment(4).toClassicRepresentation() + ":" + principalName;
    }
    
    /**
     * Test whether two attribute values are equal.
     * 
     * @param v1
     * @param v2
     * @return
     */
    @SuppressWarnings("unchecked")
    private boolean areEqual(
        Object v1,
        Object v2
    ) {
        if(v1 == null) return v2 == null;
        if(v2 == null) return v1 == null;
        if(
        	(v1 instanceof Number) && 
        	(v2 instanceof Number)
        ) {
        	if(v1 instanceof Short || v2 instanceof Short) {
        		return ((Number)v1).shortValue() == ((Number)v2).shortValue();
        	} else if(v1 instanceof Integer || v2 instanceof Integer) {
        		return ((Number)v1).intValue() == ((Number)v2).intValue();        		
        	} else if(v1 instanceof Long || v2 instanceof Long) {
        		return ((Number)v1).longValue() == ((Number)v2).longValue();        		
        	} else {
                return ((Comparable<Object>)v1).compareTo(v2) == 0;        		
        	}
        } else if(
            (v1 instanceof Comparable) && 
            (v2 instanceof Comparable) &&
            (v1.getClass().equals(v2.getClass()))
        ) {
            return ((Comparable<Object>)v1).compareTo(v2) == 0;
        } else {
        	return v1.equals(v2);
        }
    }

    /**
     * Set security attributes for given object.
     * 
     * @param auditEntry
     * @throws ServiceException
     */
    protected void setSecurityAttributes(
    	MappedRecord auditEntry
    ) throws ServiceException {
    	Object_2Facade auditEntryFacade;
        try {
	        auditEntryFacade = Object_2Facade.newInstance(auditEntry);
        }
        catch (ResourceException e) {
        	throw new ServiceException(e);
        }
        // Owner of audit entries is segment administrator
        String segmentName = auditEntryFacade.getPath().get(4);
        auditEntryFacade.attributeValuesAsList("owner").clear();
        auditEntryFacade.attributeValuesAsList("owner").add(
            this.getQualifiedPrincipalName(auditEntryFacade.getPath(), SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName + "." + SecurityKeys.USER_SUFFIX)
        );
        auditEntryFacade.attributeValuesAsList("owner").add(
            this.getQualifiedPrincipalName(auditEntryFacade.getPath(), SecurityKeys.PRINCIPAL_GROUP_ADMINISTRATORS)
        );
        auditEntryFacade.attributeValuesAsList("accessLevelBrowse").clear();
        auditEntryFacade.attributeValuesAsList("accessLevelBrowse").add(
            new Short(SecurityKeys.ACCESS_LEVEL_BASIC)
        );
        auditEntryFacade.attributeValuesAsList("accessLevelUpdate").clear();
        auditEntryFacade.attributeValuesAsList("accessLevelUpdate").add(
            new Short(SecurityKeys.ACCESS_LEVEL_PRIVATE)
        );
        auditEntryFacade.attributeValuesAsList("accessLevelDelete").clear();
        auditEntryFacade.attributeValuesAsList("accessLevelDelete").add(
            new Short(SecurityKeys.ACCESS_LEVEL_NA)
        );        
    }
    
    /**
     * Set system attributes for given object.
     * 
     * @param header
     * @param object
     * @param operation
     * @throws ServiceException
     */
    protected void setSystemAttributes(
        List<String> principalChain,
        ObjectRecord object,
        short operation
    ) throws ServiceException {
    	Object_2Facade facade;
        try {
	        facade = Object_2Facade.newInstance(object);
        } catch (ResourceException e) {
        	throw new ServiceException(e);
        }
        Date at = new Date();
        List<String> by = principalChain;
        switch(operation) {
            case DataproviderOperations.OBJECT_CREATION:
                // exclude Authority, Provider, Segment
                if(this.isInstanceOfBasicObject(object)) {
                  facade.attributeValuesAsList(SystemAttributes.CREATED_BY).clear();
                  facade.attributeValuesAsList(SystemAttributes.CREATED_BY).addAll(by);
                  facade.attributeValuesAsList(SystemAttributes.CREATED_AT).clear();
                  facade.attributeValuesAsList(SystemAttributes.CREATED_AT).add(at);
                }
                // no break here!         
            case DataproviderOperations.OBJECT_REPLACEMENT:
                // exclude Authority, Provider, Segment
                if(this.isInstanceOfBasicObject(object)) {
                  facade.attributeValuesAsList(SystemAttributes.MODIFIED_BY).clear();
                  facade.attributeValuesAsList(SystemAttributes.MODIFIED_BY).addAll(by);
                  facade.attributeValuesAsList(SystemAttributes.MODIFIED_AT).clear();
                  facade.attributeValuesAsList(SystemAttributes.MODIFIED_AT).add(at);
                }
                break;
        }
    }

    /**
     * RestInteraction
     *
     */
    public class RestInteraction extends Indexed_2.RestInteraction {
        
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
         * Get visitedBy value.
         * 
         * @param visitorId
         * @return
         * @throws ServiceException
         */
        private String getVisitedBy(
        	String visitorId
        ) throws ResourceException {
        	// In case of bulk load mark audit entry as visited. 
        	return visitorId + ":" + (this.isBulkLoad() ?
        		DateTimeFormat.BASIC_UTC_FORMAT.format(new Date()) :
        			NOT_VISITED_SUFFIX);
        }

	    /**
	     * Test whether identity of given object is subject to audit.
	     * 
	     * @param header
	     * @param path
	     * @return
	     * @throws ServiceException
	     */
	    private boolean isAuditSegment(
	        Path path
	    ) throws ResourceException, ServiceException {
	        Path segmentPath = path.getPrefix(5);
	        Boolean isAuditSegment = Audit_2.this.auditSegments.get(path);
	        if(isAuditSegment == null) {
	        	ObjectRecord segment = this.retrieveObject(
	        		segmentPath, 
	        		FetchGroup.ALL
	        	);
	        	if(segment != null) {
		            Audit_2.this.auditSegments.put(
		                segmentPath,
		                isAuditSegment = new Boolean(Audit_2.this.isAuditee(segment))
		            );
	        	} else {
	        		return false;
	        	}
	        }
	        return isAuditSegment.booleanValue();
	    }

	    /**
         * Return true if path should be excluded from trivial updates.
         *
         */  
	    protected boolean excludeFromTouch(
	       Path path
	    ) {
	        return path.isLike(ACTIVITY_CREATOR_IDENTITY_PATTERN);
	    }

    	/**
    	 * Return true if the set of modified features is considered as object touch.
    	 * 
    	 * @param modifiedFeatures
    	 * @return
    	 */
    	protected boolean isObjectTouch(
    	    Set<String> modifiedFeatures
    	) {
    	    return
                (modifiedFeatures.size() == 1 &&
                modifiedFeatures.contains(SystemAttributes.MODIFIED_AT))
                ||
                (modifiedFeatures.size() == 2 &&
                modifiedFeatures.contains(SystemAttributes.MODIFIED_AT) &&
                modifiedFeatures.contains(SystemAttributes.MODIFIED_BY));
    	}

	    /* (non-Javadoc)
	     * @see org.opencrx.kernel.layer.persistence.Indexed_1.LayerInteraction#get(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @SuppressWarnings("unchecked")
	    @Override
        public boolean get(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
	    	try {
		        Path reference = path.getParent();
		        if("audit".equals(reference.getLastSegment().toClassicRepresentation())) {
		        	ObjectRecord auditEntry = this.retrieveObject(
		        		path.getPrefix(5).getDescendant(new String[]{"audit", path.getLastSegment().toClassicRepresentation()}), 
		        		FetchGroup.ALL
		        	);
		        	if(auditEntry != null) {
		            	ObjectRecord mappedAuditEntry = Object_2Facade.cloneObject(auditEntry);
		            	Object_2Facade.newInstance(mappedAuditEntry).setPath(path);
		            	response.add(mappedAuditEntry);
			            return true;
		        	} else {
		        		return false;
		        	}
		        } else {
		            return super.get(
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
	     * @see org.opencrx.kernel.layer.persistence.Indexed_1.LayerInteraction#find(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, javax.resource.cci.IndexedRecord)
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
		        // If auditee is segment do not rewrite find request. Otherwise filter
		        // audit entries with auditee = requested reference
		        if(
		            (path.size() > 6) &&
		            "audit".equals(path.getLastSegment().toClassicRepresentation())
		        ) {            
		            // Find audit entries assigned to requesting object,
		            // request.path().getParent() IS_IN auditee of audit entry
		        	QueryRecord findRequest = request.clone();
		        	findRequest.setResourceIdentifier(path.getPrefix(5).getChild("audit"));
		        	findRequest.getQueryFilter().getCondition().addAll(
		        		FilterProperty.toCondition(
		        			new FilterProperty[]{
		        				new FilterProperty(
				                    Quantifier.THERE_EXISTS.code(),
				                    "auditee",
				                    ConditionType.IS_IN.code(),
				                    path.getParent().toXri()
				                )        				
		        			}
		        		)
		        	);
		        	findRequest.setSize(500L);
		            ResultRecord auditEntries = this.newResult();
		            super.find(
		            	SUPER.GET, 
		            	findRequest, 
		            	auditEntries
		            );
		            // Remap audit entries so that the parent of the mapped
		            // audit entries is the requesting object, i.e. the auditee.
		            List<MappedRecord> mappedAuditEntries = new ArrayList<MappedRecord>();
		            for(Object object: auditEntries) {
		            	ObjectRecord auditEntry = (ObjectRecord)object;
		            	ObjectRecord mappedAuditEntry = Object_2Facade.cloneObject(auditEntry);
		            	mappedAuditEntry.setResourceIdentifier(path.getChild(auditEntry.getResourceIdentifier().getLastSegment().toClassicRepresentation()));
		                mappedAuditEntries.add(mappedAuditEntry);
		            }	            
		            // reply
	            	response.addAll(mappedAuditEntries);
		            response.setHasMore(Boolean.FALSE);
		            response.setTotal(new Integer(mappedAuditEntries.size()));
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
	     * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1.LayerInteraction#put(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @SuppressWarnings("unchecked")
	    @Override
	    public boolean update(
	        RestInteractionSpec ispec, 
	        ObjectRecord request, 
	        ResultRecord response
	    ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
	    	try {
		        // Ignore replace requests for top-level objects such as segments 
		        // (if not user is segment admin) providers, authorities
	    		List<String> principalChain = Audit_2.this.getPrincipalChain(this.getConnection());
		        String principalName = principalChain.isEmpty() ? null : principalChain.get(0);
		        if(
		            (path.size() > 5) ||
		            ((path.size() == 5) && principalName.startsWith(SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR)) 
		        ) {
		        	boolean propagatePut = true;
		            if(this.isAuditSegment(path)) {    
		            	ObjectRecord existing = this.retrieveObject(
		            		path, 
		            		FetchGroup.ALL
		            	);
		                // Create ObjectModificationAuditEntry and add it to segment
		                if(Audit_2.this.isAuditee(existing)) {
		                	ObjectRecord auditEntry = Object_2Facade.newInstance(
		                        path.getPrefix(5).getDescendant(
		                        	new String[]{
		                        		"audit", 
		                        		UUIDConversion.toUID(UUIDs.newUUID())
		                        	}
		                        ),
		                        "org:opencrx:kernel:base:ObjectModificationAuditEntry"
		                    ).getDelegate();
		                	Object_2Facade auditEntryFacade = Object_2Facade.newInstance(auditEntry);
		                	auditEntryFacade.attributeValuesAsList("auditee").add(
		                        path.toXri()
		                    );
		                    for(Iterator<String> i = Audit_2.this.visitorId.iterator(); i.hasNext(); ) {
		                        String visitorId = i.next();
		                        auditEntryFacade.attributeValuesAsList("visitedBy").add(
		                            this.getVisitedBy(visitorId)
		                        );
		                    }
		                    // Remove all attribute names from existing object (before
		                    // image) which are not modified. This produces a before image
		                    // which contains the attribute values before modification of
		                    // modified object attributes.
		                    MappedRecord beforeImage = Object_2Facade.cloneObject(existing);
		                    Object_2Facade.getValue(beforeImage).keySet().retainAll(
		                        Object_2Facade.getValue(request).keySet()
		                    );
		                    Set<String> modifiedFeatures = Audit_2.this.getChangedAttributes(
		                        beforeImage,
		                        request
		                    );
		                    // No modified features. Do not create audit entry and do not
		                    // propagate PUT. E.g. isQuery=true operations do not touch
		                    // an object.
		                    boolean isObjectTouch = this.isObjectTouch(modifiedFeatures);
		                    if(modifiedFeatures.isEmpty()) {
		                    	propagatePut = false;
		                    } else if(!isObjectTouch) {
			                    // Non-trivial update. Create audit entry	                    	
		                        Object_2Facade.getValue(beforeImage).keySet().retainAll(
		                            modifiedFeatures
		                        );
		                        auditEntryFacade.attributeValuesAsList("beforeImage").add(
		                        	Audit_2.this.getBeforeImageAsString(beforeImage)
		                        );
		                        String modifiedFeaturesAsString = modifiedFeatures.toString();
		                        auditEntryFacade.attributeValuesAsList("modifiedFeatures").add(
		                            modifiedFeaturesAsString.length() > 300 
		                                ? modifiedFeaturesAsString.substring(0, 280) + "..." 
		                                : modifiedFeaturesAsString
		                        );
		                        Audit_2.this.setSystemAttributes(
		                            Audit_2.this.getPrincipalChain(this.getConnection()),
		                            auditEntry, 
		                            DataproviderOperations.OBJECT_CREATION
		                        );
		                        Audit_2.this.setSecurityAttributes(
		                            auditEntry
		                        );
		                        super.create(
		                        	SUPER.CREATE, 
		                        	auditEntry, 
		                        	this.newResult()
		                        );
		                    } else {
                                // Test whether object is to be excluded from a touch. 
		                        // In this case do not generate an audit entry but propagate
                                // the PUT.	                        
		                        propagatePut = !this.excludeFromTouch(path);
		                    }
		                }
		            }
		            // In case of touch only updates do not propagate PUT
		            if(propagatePut) {
			            super.update(
			                ispec,
			                request,
			                response
			            );
		            }
		            return true;
		        } else {
	        		response.add(request);
		        	return true;
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
	     * @see org.openmdx.application.dataprovider.layer.persistence.jdbc.AbstractDatabase_1.LayerInteraction#create(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @SuppressWarnings("deprecation")
	    @Override
        public boolean create(
            RestInteractionSpec ispec, 
            ObjectRecord request,
            ResultRecord response
        ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
	    	try {
		        super.create(
		            ispec,
		            request,
		            response
		        );	       
		        // Create audit log entries for non-root-level objects only
		        // and for objects which are contained in an auditable segment
		        if(
		            (path.size() > 5) &&
		             this.isAuditSegment(path) &&
		             Audit_2.this.isAuditee(request)
		        ) {	
		            // Create audit entry
		        	ObjectRecord auditEntry = Object_2Facade.newInstance(
		                path.getPrefix(5).getDescendant(
		                	new String[]{
		                		"audit", 
		                		UUIDConversion.toUID(UUIDs.newUUID())
		                	}
		                ),
		                "org:opencrx:kernel:base:ObjectCreationAuditEntry"
		            ).getDelegate();
		        	Object_2Facade auditEntryFacade = Object_2Facade.newInstance(auditEntry);
		        	auditEntryFacade.attributeValuesAsList("auditee").add(
		                path.toXri()
		            );
		            for(Iterator<String> i = Audit_2.this.visitorId.iterator(); i.hasNext(); ) {
		                String visitorId = i.next();
		                auditEntryFacade.attributeValuesAsList("visitedBy").add(
		                    this.getVisitedBy(visitorId)
		                );
		            }
		            Audit_2.this.setSystemAttributes(
		                Audit_2.this.getPrincipalChain(this.getConnection()),
		                auditEntry, 
		                DataproviderOperations.OBJECT_CREATION
		            );
		            Audit_2.this.setSecurityAttributes(
		                auditEntry
		            );
		            super.create(
		            	SUPER.CREATE, 
		            	auditEntry, 
		            	this.newResult()
		            );
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
	     * @see org.opencrx.kernel.layer.persistence.Indexed_1.LayerInteraction#delete(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, javax.resource.cci.IndexedRecord)
	     */
	    @Override
        public boolean delete(
            RestInteractionSpec ispec, 
            ObjectRecord request
        ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
	    	try {
		        // Create audit log entries for non-root-level objects only
		        // and for objects which are contained in an auditable segment
		        if(
		            (path.size() > 5) &&
		             this.isAuditSegment(path)
		        ) {
		            // Create audit entry
		        	ObjectRecord existing = this.retrieveObject(
		        		path, 
		        		FetchGroup.ALL
		        	);
		            // Create ObjectRemovalAuditEntry and add it to segment
		            if(Audit_2.this.isAuditee(existing)) {
		            	ObjectRecord auditEntry = Object_2Facade.newInstance(
		                    path.getPrefix(5).getDescendant(
		                    	new String[]{
		                    		"audit", 
		                    		UUIDConversion.toUID(UUIDs.newUUID())
		                    	}
		                    ),
		                    "org:opencrx:kernel:base:ObjectRemovalAuditEntry"
		                ).getDelegate();
		            	Object_2Facade auditEntryFacade = Object_2Facade.newInstance(auditEntry);
		            	auditEntryFacade.attributeValuesAsList("auditee").add(
		                    path.toXri()
		                );
		                for(Iterator<String> i = Audit_2.this.visitorId.iterator(); i.hasNext(); ) {
		                    String visitorId = i.next();
		                    auditEntryFacade.attributeValuesAsList("visitedBy").add(
		                        this.getVisitedBy(visitorId)
		                    );
		                }
		                auditEntryFacade.attributeValuesAsList("beforeImage").add(
		                	Audit_2.this.getBeforeImageAsString(existing)
		                );
		                Audit_2.this.setSystemAttributes(
		                    Audit_2.this.getPrincipalChain(this.getConnection()),
		                    auditEntry, 
		                    DataproviderOperations.OBJECT_CREATION
		                );
		                Audit_2.this.setSecurityAttributes(
		                    auditEntry
		                );
		                super.create(
		                	SUPER.CREATE,
		                	auditEntry, 
		                	this.newResult()
		                );
		            }
		        }
		        // Remove object
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
	     * @see org.opencrx.kernel.layer.persistence.Indexed_1.LayerInteraction#otherOperation(org.openmdx.application.dataprovider.cci.ServiceHeader, org.openmdx.application.dataprovider.cci.DataproviderRequest, java.lang.String, org.openmdx.base.naming.Path)
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
		        if("testAndSetVisitedBy".equals(operationName)) {
		            Path auditEntryIdentity = path.getPrefix(path.size() - 2);
		            ObjectRecord auditEntry = this.retrieveObject(
		            	auditEntryIdentity, 
		            	FetchGroup.ALL
		            );
		            Object_2Facade auditEntryFacade = Object_2Facade.newInstance(auditEntry);
		            String visitorId = (String)request.getBody().get("visitorId");
		            response.setBody(this.newOperationResult("org:opencrx:kernel:base:TestAndSetVisitedByResult"));
		            int pos = 0;
		            if(
		                (visitorId == null) ||
		                !Audit_2.this.visitorId.values().contains(visitorId)
		            ) {
		            	response.getBody().put("visitStatus",  new Short((short)2));
		            } else if((pos = auditEntryFacade.attributeValuesAsList("visitedBy").indexOf(visitorId + ":" + NOT_VISITED_SUFFIX)) >= 0) {
			            // Not yet visited by visitorId
		            	auditEntryFacade.attributeValuesAsList("visitedBy").set(
		                    pos,
		                    visitorId + ":" + DateTimeFormat.BASIC_UTC_FORMAT.format(new Date())
		                );
		            	super.update(
		            		SUPER.UPDATE, 
		            		auditEntry, 
		            		this.newResult()
		            	);
		            	response.getBody().put("visitStatus", new Short((short)0));
		            } else {
		            	response.getBody().put("visitStatus", new Short((short)1));               
		            }
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
    
    /**
     * Return the set of attributes which's values changed in o2 relative to o1.
     * 
     * @param o1
     * @param o2
     * @return
     * @throws ServiceException
     */
    @SuppressWarnings("unchecked")
    protected Set<String> getChangedAttributes(
    	MappedRecord o1,
    	MappedRecord o2
    ) throws ServiceException {
        if(o2 == null) {
          return new HashSet<String>();
        }
        Object_2Facade o1Facade = Facades.asObject(o1);
        Object_2Facade o2Facade = Facades.asObject(o2);
        // touch all o1 attributes in o2
        for(
          Iterator<String> i = o1Facade.getValue().keySet().iterator();
          i.hasNext();
        ) {
        	o2Facade.attributeValuesAsList(i.next());
        }        
        // diff
        Set<String> changedAttributes = new HashSet<String>();
        for(
            Iterator<String> i = o2Facade.getValue().keySet().iterator();
            i.hasNext();
        ) {
            String attributeName = i.next();
            List<Object> v1 = o1Facade.attributeValuesAsList(attributeName);
            List<Object> v2 = o2Facade.attributeValuesAsList(attributeName);
            if(
                !SystemAttributes.OBJECT_INSTANCE_OF.equals(attributeName) &&
                !"identity".equals(attributeName) && 
                !attributeName.startsWith(SystemAttributes.CONTEXT_PREFIX)
            ) { 
                boolean isEqual = v1.size() == v2.size();
                if(isEqual) {
                    for(int j = 0; j < v1.size(); j++) {
                    	isEqual = this.areEqual(v1.get(j), v2.get(j));
                        if(!isEqual) break;
                    }
                }
                if(!isEqual) {
                    changedAttributes.add(attributeName);
                }
            }
        }
        return changedAttributes;
    }

    /**
     * Get before image.
     * 
     * @param beforeImage
     * @return
     * @throws ServiceException
     */
    @SuppressWarnings("unchecked")
    public String getBeforeImageAsString(
    	MappedRecord beforeImage
    ) throws ServiceException {
        String beforeImageAsString = "";
        Object_2Facade beforeImageFacade = Facades.asObject(beforeImage);
        for(
            Iterator<String> i = beforeImageFacade.getValue().keySet().iterator();
            i.hasNext();
        ) {
            String attributeName = i.next();
            beforeImageAsString += attributeName + ":\n";
            int jj = 0;
            for(
                Iterator<Object> j = beforeImageFacade.attributeValuesAsList(attributeName).iterator();
                j.hasNext();
                jj++
            ) {
                beforeImageAsString += "" + jj + ": " + j.next() + "\n";
            }
        }
        return beforeImageAsString;
    }
    
	/**
	 * @return the visitorId
	 */
	public SparseArray<String> getVisitorId() {
		return visitorId;
	}

	/**
	 * @param visitorId the visitorId to set
	 */
	public void setVisitorId(SparseArray<String> visitorId) {
		this.visitorId = visitorId;
	}

	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected static final String NOT_VISITED_SUFFIX = "-";
    protected final Path ACTIVITY_CREATOR_IDENTITY_PATTERN = new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityCreator/:*");    

    protected final Map<Path,Boolean> auditSegments = new HashMap<Path,Boolean>();
    protected SparseArray<String> visitorId = new TreeSparseArray<String>();
    
}
