/*
 * ====================================================================
 * Project:     openCRX/Security, http://www.opencrx.org/
 * Description: OpenCrxSecurity_2
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.security.layer.application;

import java.util.Date;
import java.util.List;

import javax.jdo.FetchGroup;
import jakarta.resource.ResourceException;
import jakarta.resource.cci.Connection;
import jakarta.resource.cci.Interaction;
import jakarta.resource.cci.MappedRecord;

import org.opencrx.kernel.generic.OpenCrxException;
import org.opencrx.kernel.generic.SecurityKeys;
import org.openmdx.application.dataprovider.cci.FilterProperty;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.spi.PersistenceManagers;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.IsInCondition;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.resource.InteractionSpecs;
import org.openmdx.base.resource.Records;
import org.openmdx.base.resource.cci.RestFunction;
import org.openmdx.base.resource.spi.ResourceExceptions;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.MessageRecord;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryFilterRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.RestConnection;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.AbstractRestInteraction;
import org.openmdx.base.rest.spi.AbstractRestPort;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.kernel.exception.BasicException;

/**
 * This plugin implements the models org:openmdx:security:realm1,
 * org:openmdx:security1:authorization1, org:openmdx:security:authentication1.
 * 
 * It manages the following classes:
 * - realm1: Group, Permission, Principal, Policy, Privilege, Realm, Role
 * - authorization1: Policy, Privilege
 * - authentication1: Password
 * 
 * This plugin delegates all object retrievals, updates and creations to the
 * persistence plugin which is typically the database plugin 
 * (e.g. super.get(header, request), super.replace(header, request)). An 
 * LDAP implementation of this plugin must delegate requests to an LDAP
 * service.
 * 
 */
public class OpenCrxSecurity_2 extends AbstractRestPort {

	/**
	 * Constructor.
	 */
	public OpenCrxSecurity_2(
	) {
	}

    /* (non-Javadoc)
     * @see org.openmdx.application.dataprovider.layer.application.Standard_1#getInteraction(jakarta.resource.cci.Connection)
     */
	@Override
    public Interaction getInteraction(
        RestConnection connection
    ) throws ResourceException {
        return new RestInteraction(connection);
    }

    /**
     * Set derived attributes.
     * 
     * @param obj
     * @throws ServiceException
     */
    public void setDerivedAttributes(
    	ObjectRecord obj
    ) throws ResourceException {
    	Model_1_0 model = Model_1Factory.getModel();
    	try {
	    	Object_2Facade objFacade = Facades.asObject(obj);
	        if(
	            model.objectIsSubtypeOf(obj, "org:openmdx:security:realm1:Principal") ||
	            model.objectIsSubtypeOf(obj, "org:openmdx:security:realm1:Realm") ||
	            model.objectIsSubtypeOf(obj, "org:openmdx:security:realm1:Role") ||
	            model.objectIsSubtypeOf(obj, "org:openmdx:security:realm1:Policy")
	        ) {
	        	if(
	        		objFacade.attributeValue("name") == null ||
	        		((String)objFacade.attributeValue("name")).isEmpty()
	        	) {
	        		// Derive name from qualifier if not supplied
		        	objFacade.attributeValuesAsList("name").clear();
		        	objFacade.attributeValuesAsList("name").add(
		        		objFacade.getPath().getLastSegment().toString()
		        	);
	        	}
	        } else if(
	        	model.objectIsSubtypeOf(obj, "org:openmdx:security:realm1:Credential")
	        ) {
	        	objFacade.attributeValuesAsList("id").clear();
	        	objFacade.attributeValuesAsList("id").add(
	        		objFacade.getPath().getLastSegment().toString()
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

    /**
     * RestInteraction
     * 
     */
    public class RestInteraction extends AbstractRestInteraction {
        
        /**
         * Constructor.
         * 
         * @param connection
         * @throws ResourceException
         */
        public RestInteraction(
            RestConnection connection
        ) throws ResourceException {
			super(connection, newDelegateInteraction(connection));
        }

        /**
         * Get request principal.
         * 
         * @param header
         * @return
         * @throws ServiceException
         */
        protected String getPrincipalName(
            RestInteractionSpec ispec
        ) throws ResourceException {
        	Connection conn = this.getConnection();
        	List<String> principalChain = PersistenceManagers.toPrincipalChain(conn.getMetaData().getUserName());
            return principalChain.isEmpty() ? null : principalChain.get(0);        	
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
         * Touch the realm if any object contained in the realm was modified.
         * 
         * @param header
         * @param request
         * @throws ServiceException
         */
        protected void touchRealm(
            RestInteractionSpec ispec,
            Path path
        ) throws ResourceException {
            if(
                (path.size() >= PATH_PATTERN_REALM_COMPOSITES.size()) &&
                path.getPrefix(PATH_PATTERN_REALM_COMPOSITES.size()).isLike(PATH_PATTERN_REALM_COMPOSITES)
            ) {
                try {
                    Path realmIdentity = path.getPrefix(7);
                    ObjectRecord realm = this.retrieveObject(
                    	realmIdentity, 
                    	FetchGroup.ALL
                    );
                    if(realm != null) {
	                    MappedRecord updatedRealm = (MappedRecord)realm.clone();
	                    Object_2Facade updatedRealmFacade = Facades.asObject(updatedRealm);
	                    updatedRealmFacade.attributeValuesAsList(SystemAttributes.MODIFIED_AT).clear();                	
	                    updatedRealmFacade.attributeValuesAsList(SystemAttributes.MODIFIED_AT).add(new Date());
	                    ResultRecord updateResult = Records.getRecordFactory().createIndexedRecord(ResultRecord.class);
	                    super.update(
	                    	SUPER.UPDATE, 
	                    	realm, 
	                    	updateResult
	                    );
                    }
                } catch(Exception e) {
                	ServiceException e0 = new ServiceException(e);
                    // Ignore if realm does not exist or in case of concurrent modifications
                    if(
                    	BasicException.Code.NOT_FOUND != e0.getExceptionCode() &&
                    	BasicException.Code.CONCURRENT_ACCESS_FAILURE != e0.getExceptionCode()
                    ) {
                        throw ResourceExceptions.initHolder(
                            new ResourceException(
                                BasicException.newEmbeddedExceptionStack(e)
                            )
                        );
                    }
                }
            }
        }

        /**
         * Change password credential.
         * 
         * @param header
         * @param passwordCredential
         * @param changePasswordParams
         * @throws ServiceException
         */
        protected boolean changePassword(
        	RestInteractionSpec ispec,
        	ObjectRecord passwordCredential,
        	MappedRecord changePasswordParams,
        	MessageRecord result
        ) throws ResourceException {
        	try {
	        	Object_2Facade passwordCredentialFacade = Facades.asObject(passwordCredential);
	            String oldPassword = changePasswordParams.get("oldPassword") != null 
	            	? Base64.encode((byte[])changePasswordParams.get("oldPassword")) 
	            	: null;
	            if((oldPassword != null) && !oldPassword.equals(passwordCredentialFacade.attributeValue("password"))) {
	    			throw ResourceExceptions.initHolder(
	            		new ResourceException(
		                    "old password verification mismatch",
	        				BasicException.newEmbeddedExceptionStack(
			                	OpenCrxException.DOMAIN,
			                    BasicException.Code.ASSERTION_FAILURE, 
			                    new BasicException.Parameter("credential", passwordCredential)
			                 )
		                )
		            );
	            }
	            ObjectRecord changedPasswordCredential = Object_2Facade.cloneObject(passwordCredential);
	            Object_2Facade changedPasswordCredentialFacade = Facades.asObject(changedPasswordCredential);
	            changedPasswordCredentialFacade.attributeValuesAsList("password").clear();
	            changedPasswordCredentialFacade.attributeValuesAsList("password").add(
	                Base64.encode((byte[])changePasswordParams.get("password"))
	            );
	            changedPasswordCredentialFacade.attributeValuesAsList(SystemAttributes.MODIFIED_AT).clear();
	            changedPasswordCredentialFacade.attributeValuesAsList(SystemAttributes.MODIFIED_AT).add(new Date());
	            result.setBody(this.newOperationResult("org:openmdx:base:Void"));
	            return super.update(
	            	SUPER.UPDATE,
	                changedPasswordCredential,
	                this.newResult()
	            );
        	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );
        	}
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
         * Check permissions.
         * 
         * @param header
         * @param request
         * @throws ServiceException
         */
        protected boolean checkPermission(
            RestInteractionSpec ispec,
            Path path
        ) throws ResourceException {
            String principalName = this.getPrincipalName(ispec);
            // principal is memberOf Administrators if principalName starts with admin-...
            boolean principalIsMemberOfAdministrators = principalName.startsWith("admin" + SecurityKeys.ID_SEPARATOR);
            // Check group membership
            if(!principalIsMemberOfAdministrators) {
	            try {
	                Path principalsPath = (path.startsWith(REALM_AUTHORITY) || path.startsWith(AUTHORIZATION_AUTHORITY)) && path.size() >= 7
	                	? REALM_AUTHORITY.getDescendant("provider", path.getSegment(2).toString(), "segment", "Root", "realm", path.getSegment(6).toString(), "principal")
	            		: REALM_AUTHORITY.getDescendant("provider", path.getSegment(2).toString(), "segment", "Root", "realm", path.getSegment(4).toString(), "principal");
	            	Object_2Facade principal = Facades.asObject(
		            	this.retrieveObject(
		            		principalsPath.getDescendant(principalName),
		            		FetchGroup.ALL
		            	)
		            );
	            	if(principal != null) {
	                    Path principalGroupAdministrators = principalsPath.getDescendant(SecurityKeys.PRINCIPAL_GROUP_ADMINISTRATORS);
	                    principalIsMemberOfAdministrators = principal.attributeValuesAsListContains("isMemberOf", principalGroupAdministrators);
	            	}
	            } catch(Exception e) {
	            	new ServiceException(e).log();
	            }
            }
            // read allowed for everybody
            if(ispec.getFunction() == RestFunction.GET) {
                return principalIsMemberOfAdministrators;
            }
            // 1) All other operations only allowed for admin principals
            // 2) Principal is allowed to update its principal objects
            if((
                !principalIsMemberOfAdministrators &&
                !(path.getParent().isLike(PATH_PATTERN_PRINCIPALS) && path.getLastSegment().toString().equals(principalName)))
            ) {
    			throw ResourceExceptions.initHolder(
            		new ResourceException(
            			"No permission for " + ispec.getFunction().name() + " on object",
        				BasicException.newEmbeddedExceptionStack(
		                    OpenCrxException.DOMAIN,
		                    OpenCrxException.AUTHORIZATION_FAILURE_UPDATE,                     
		                    new BasicException.Parameter("object", path),
		                    new BasicException.Parameter("param0", path)
		                )
		            )
                );
            }
            return principalIsMemberOfAdministrators;
        }

		private boolean failForUnknownOperation(
			MessageRecord input
		) throws ResourceException {
			final String operation = input.getTarget().getLastSegment().toString();
			throw ResourceExceptions.initHolder(
        		new ResourceException(
                    "unknown operation",
    				BasicException.newEmbeddedExceptionStack(
                        BasicException.Code.DEFAULT_DOMAIN,
                        BasicException.Code.ASSERTION_FAILURE, 
                        new BasicException.Parameter("xri", input.getResourceIdentifier()),
                        new BasicException.Parameter("target", input.getTarget()),
                        new BasicException.Parameter("id", input.getMessageId()),
                        new BasicException.Parameter("operation", operation)
                    )
                )
            );
		}
        
	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.spi.Layer_1.LayerInteraction#delete(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, jakarta.resource.cci.IndexedRecord)
	     */
	    @Override
	    protected boolean delete(
	        RestInteractionSpec ispec, 
	        ObjectRecord obj
	    ) throws ResourceException {
	    	try {
		        this.checkPermission(
		            ispec,
		            obj.getResourceIdentifier()
		        );
		    	Object_2Facade objFacade = Facades.asObject(obj);
		    	Path path = objFacade.getPath();
		    	// Assert if principal is referenced in isMemberOf
		    	if(path.isLike(PATH_PATTERN_PRINCIPAL)) {
		        	QueryRecord findRequest = this.newQuery(
		        		REALM_AUTHORITY.getDescendant(
		        			"provider",
		        			path.getSegment(2).toString(),
		        			"segment",
		        			path.getSegment(4).toString(),
		        			"realm",
		        			path.getSegment(6).toString(),
		        			"principal"
		        		)
		        	);
		        	findRequest.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));		        	
		        	findRequest.getQueryFilter().getCondition().addAll(
		        		FilterProperty.toCondition(
		        			new FilterProperty[]{
		        				new FilterProperty(
				                    Quantifier.THERE_EXISTS.code(),
				                    "isMemberOf",
				                    ConditionType.IS_IN.code(),
				                    path
				                )
		        			}
		        		)
		        	);
		        	findRequest.setSize(10L);
		            ResultRecord findResult = this.newResult();
		        	super.find(
		        		SUPER.GET, 
		        		findRequest,
			        	findResult
			        );
		        	if(!findResult.isEmpty()) {
		    			throw ResourceExceptions.initHolder(
		            		new ResourceException(
		                        "Unable to remove principal. Reason: referenced by Principal::isMemberOf",
		        				BasicException.newEmbeddedExceptionStack(
		        					OpenCrxException.DOMAIN,
		                            BasicException.Code.ASSERTION_FAILURE, 
		                            new BasicException.Parameter("xri", path),
		                            new BasicException.Parameter("references", findResult)
		                        )
		                    )
		                );
		        	}
		    	}
		    	// Assert if principal is referenced in grantedRole		    	
		    	if(path.isLike(PATH_PATTERN_ROLE)) {
		        	QueryRecord findRequest = this.newQuery(
		        		REALM_AUTHORITY.getDescendant(
		        			"provider",
		        			path.getSegment(2).toString(),
		        			"segment",
		        			path.getSegment(4).toString(),
		        			"realm",
		        			path.getSegment(6).toString(),
		        			"principal"
		        		)
		        	);
		        	findRequest.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));		        			        	
		        	findRequest.getQueryFilter().getCondition().addAll(
		        		FilterProperty.toCondition(
		        			new FilterProperty[]{
		        				new FilterProperty(
				                    Quantifier.THERE_EXISTS.code(),
				                    "grantedRole",
				                    ConditionType.IS_IN.code(),
				                    path
				                )
		        			}
		        		)
		        	);
		        	findRequest.setSize(10L);
		            ResultRecord findResult = this.newResult();
		        	super.find(
		        		SUPER.GET,
		        		findRequest,
		        		findResult
			        );
		        	if(!findResult.isEmpty()) {
		    			throw ResourceExceptions.initHolder(
		            		new ResourceException(
		                        "Unable to remove role. Reason: referenced by Principal::grantedRole",
		        				BasicException.newEmbeddedExceptionStack(
		        					OpenCrxException.DOMAIN,
		                            BasicException.Code.ASSERTION_FAILURE, 
		                            new BasicException.Parameter("xri", path),
		                            new BasicException.Parameter("references", findResult)
		                        )
		                    )
		                );
		        	}
		    	}
		        this.touchRealm(
		        	ispec,
		            obj.getResourceIdentifier()
		        );
		        return super.delete(
		            ispec,
		            obj
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
	     * @see org.openmdx.application.dataprovider.spi.Layer_1.LayerInteraction#create(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, jakarta.resource.cci.IndexedRecord)
	     */
	    @Override
	    public boolean create(
	        RestInteractionSpec ispec, 
	        ObjectRecord request, 
	        ResultRecord response
	    ) throws ResourceException {
            this.checkPermission(
	            ispec,
	            request.getResourceIdentifier()
	        );
            OpenCrxSecurity_2.this.setDerivedAttributes(request);
            this.touchRealm(
	            ispec,
	            request.getResourceIdentifier()
	        );
	        return super.create(
	        	ispec,
	        	request,
	        	response
	        );
	    }
	    
	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.spi.Layer_1.LayerInteraction#put(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Object_2Facade, jakarta.resource.cci.IndexedRecord)
	     */
	    @SuppressWarnings("unchecked")
	    @Override
		public boolean update(
			RestInteractionSpec ispec, 
			ObjectRecord request,
			ResultRecord response
		) throws ResourceException {
	    	try {
	            this.checkPermission(
		            ispec,
		            request.getResourceIdentifier()
		        );
		        // Only mark realm as dirty if group memberships are modified
		        // E.g. modifying lastLoginAt does not require to refresh the realm
		        if(request.getValue().containsKey("isMemberOf")) {
		        	this.touchRealm(
		                ispec,
		                request.getResourceIdentifier()
		            );
		        }
		        try {
		            return super.update(
		                ispec,
		                request,
		                response
		            );
		        } catch(Exception e) {
		        	ServiceException e0 = new ServiceException(e);
		            // Ignore CONCURRENT_ACCESS_FAILURE on realms (updateRealm could have
		            // been called before)
		            if(
		                (e0.getExceptionCode() == BasicException.Code.CONCURRENT_ACCESS_FAILURE) &&
		                request.getResourceIdentifier().isLike(PATH_PATTERN_REALM)
		            ) {
		            	if(response != null) {
		            		response.add(request);
		            	}
		            	return true;
		            }
		            throw e0;            
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
	     * @see org.openmdx.application.dataprovider.spi.Layer_1.LayerInteraction#get(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, jakarta.resource.cci.IndexedRecord)
	     */
	    @Override
        protected boolean get(
            RestInteractionSpec ispec,
            QueryRecord request,
            ResultRecord response
        ) throws ResourceException {
            this.checkPermission(
	            ispec,
	            request.getResourceIdentifier()
	        );
            return super.get(
	            ispec,
	            request,
	            response
            );
	    }
	    
	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.spi.Layer_1.LayerInteraction#invoke(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.cci.MessageRecord, org.openmdx.base.rest.cci.MessageRecord)
	     */
	    @Override
	    protected boolean invoke(
	        RestInteractionSpec ispec, 
	        MessageRecord request, 
	        MessageRecord response
	    ) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
            this.checkPermission(
	            ispec,
	            path
	        );
	        String operationName = path.getSegment(path.size() - 2).toString();
	        ObjectRecord object = this.retrieveObject(
	            path.getPrefix(path.size() - 2),
	            FetchGroup.ALL
	        );
	        if(object != null) {
		        String objectClass = object.getValue().getRecordName();
		        MappedRecord param = request.getBody();
		        // Operations on org:openmdx:security:authentication1:Password
		        if("org:openmdx:security:authentication1:Password".equals(objectClass)) {
		        	if("change".equals(operationName)) {
		        		return this.changePassword(ispec, object, param, response);
		        	} else {
		        		return this.failForUnknownOperation(request);
		        	}
		        } else {
	        		return this.failForUnknownOperation(request);		        	
		        }
	        } else {
	        	return super.invoke(
	        		ispec, 
	        		request, 
	        		response
	        	);
	        }
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.application.dataprovider.spi.Layer_1.LayerInteraction#find(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.rest.spi.Query_2Facade, jakarta.resource.cci.IndexedRecord)
	     */
	    @Override
		protected boolean find(
			RestInteractionSpec ispec, 
			QueryRecord request,
			ResultRecord response
		) throws ResourceException {
	    	Path path = request.getResourceIdentifier();
            boolean principalIsMemberOfAdministrators = this.checkPermission(
            	ispec,
	            request.getResourceIdentifier()
	        );
	        String principalName = this.getPrincipalName(ispec);
	        // Restrict browsing on principals
	        if(path.isLike(PATH_PATTERN_PRINCIPALS)) {
	            boolean containsSubjectFilter = false;
	            List<FilterProperty> attributeFilter = FilterProperty.getFilterProperties(request.getQueryFilter());
	            for(FilterProperty p: attributeFilter) {
	                if("subject".equals(p.name())) {
	                    containsSubjectFilter = true;
	                    break;
	                }
	            }
	            // Return users and groups only if requesting principal is not admin-Root or segment admin
	            if(
	                !containsSubjectFilter &&
	                !principalName.equals(SecurityKeys.ROOT_PRINCIPAL) &&
	                !principalIsMemberOfAdministrators
	            ) {
	            	request.getQueryFilter().getCondition().add(
	            		new IsInCondition(
	            			Quantifier.THERE_EXISTS,
	            			SystemAttributes.OBJECT_CLASS,
	            			true,
	                        "org:opencrx:security:realm1:PrincipalGroup",   
	                        "org:opencrx:security:realm1:User"	            			
	            		)
	            	);
	            }
	        } else if(path.isLike(PATH_PATTERN_SUBJECTS)) {
		        // Restrict browsing on subjects
	            // Do not restrict Root            
	            if(!principalName.equals(SecurityKeys.ROOT_PRINCIPAL)) {
	            	request.getQueryFilter().getCondition().add(
	            		new IsInCondition(
	            			Quantifier.FOR_ALL,
	            			SystemAttributes.OBJECT_CLASS,
	            			true            			
	            		)
	            	);
	            }
	        } else if(path.isLike(PATH_PATTERN_POLICIES)) {
		        // Restrict browsing on policies
	            // Do not restrict Root            
	            if(!principalName.equals(SecurityKeys.ROOT_PRINCIPAL)) {
	            	request.getQueryFilter().getCondition().add(
	            		new IsInCondition(
	            			Quantifier.FOR_ALL,
	            			SystemAttributes.OBJECT_CLASS,
	            			true            			
	            		)
	            	);
	            }
	        }
	        return super.find(
	        	ispec, 
	        	request, 
	        	response
	        );
	    }
    }

    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    protected final InteractionSpecs SUPER = InteractionSpecs.getRestInteractionSpecs(false);

    protected static final Path AUTHORIZATION_AUTHORITY =
    	new Path("xri://@openmdx*org.openmdx.security.authorization1");
    protected static final Path REALM_AUTHORITY =
    	new Path("xri://@openmdx*org.openmdx.security.realm1");
    protected static final Path IDENTITY_AUTHORITY =
    	new Path("xri://@openmdx*org.opencrx.security.identity1");
    protected static final Path PATH_PATTERN_PRINCIPALS =
    	REALM_AUTHORITY.getDescendant("provider", ":*", "segment", ":*", "realm", ":*", "principal");
    protected static final Path PATH_PATTERN_PRINCIPAL =
    	PATH_PATTERN_PRINCIPALS.getDescendant(":*");
    protected static final Path PATH_PATTERN_REALM =
    	REALM_AUTHORITY.getDescendant("provider", ":*", "segment", ":*", "realm", ":*");
    protected static final Path PATH_PATTERN_REALM_COMPOSITES =
    	REALM_AUTHORITY.getDescendant("provider", ":*", "segment", ":*", "realm", ":*", ":*");
    protected static final Path PATH_PATTERN_SUBJECTS =
    	IDENTITY_AUTHORITY.getDescendant("provider", ":*", "segment", ":*", "subject");
    protected static final Path PATH_PATTERN_POLICIES =
    	AUTHORIZATION_AUTHORITY.getDescendant("provider", ":*", "segment", ":*", "policy");
    protected static final Path PATH_PATTERN_ROLES =
    	AUTHORIZATION_AUTHORITY.getDescendant("provider", ":*", "segment", ":*", "policy", ":*", "role");
    protected static final Path PATH_PATTERN_ROLE =
    	PATH_PATTERN_ROLES.getDescendant(":*");
}
