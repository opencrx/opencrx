/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX access control plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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

package org.opencrx.kernel.layer.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.jdo.FetchGroup;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.resource.ResourceException;
import javax.resource.cci.Connection;
import javax.resource.cci.Interaction;
import javax.resource.cci.MappedRecord;

import org.opencrx.kernel.generic.OpenCrxException;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.generic.SecurityKeys.Action;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.security.realm1.cci2.PrincipalGroupQuery;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.application.dataprovider.cci.AttributeSpecifier;
import org.openmdx.application.dataprovider.cci.FilterProperty;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.accessor.jmi.spi.EntityManagerFactory_1;
import org.openmdx.base.accessor.rest.DataManagerFactory_1;
import org.openmdx.base.dataprovider.cci.DataproviderRequestProcessor;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.ConfigurableProperty;
import org.openmdx.base.persistence.spi.PersistenceManagers;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.resource.Records;
import org.openmdx.base.resource.cci.ConnectionFactory;
import org.openmdx.base.resource.spi.ResourceExceptions;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.MessageRecord;
import org.openmdx.base.rest.cci.ObjectRecord;
import org.openmdx.base.rest.cci.QueryFilterRecord;
import org.openmdx.base.rest.cci.QueryRecord;
import org.openmdx.base.rest.cci.RequestRecord;
import org.openmdx.base.rest.cci.RestConnection;
import org.openmdx.base.rest.cci.ResultRecord;
import org.openmdx.base.rest.spi.AbstractRestInteraction;
import org.openmdx.base.rest.spi.AbstractRestPort;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Object_2Facade;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.security.realm1.jmi1.Group;
import org.openmdx.security.realm1.jmi1.Permission;
import org.openmdx.security.realm1.jmi1.Principal;
import org.openmdx.security.realm1.jmi1.Realm;
import org.openmdx.security.realm1.jmi1.Role;

/**
 * Access control plugin. Implements the openCRX access control logic.
 * 
 */
public class AccessControl_2 extends AbstractRestPort {
	
	/**
	 * Constructor.
	 * 
	 */
	public AccessControl_2(
	) {
		super();
	}

    /* (non-Javadoc)
     * @see org.openmdx.application.dataprovider.layer.model.Standard_1#getInteraction(javax.resource.cci.Connection)
     */
	@Override
    public Interaction getInteraction(
    	RestConnection connection
    ) throws ResourceException {
        return new RestInteraction(connection);
    }

    /**
     * Get user identity for given principal.
     * 
     * @param principal
     * @return
     */
    protected Path getUserIdentity(
    	CachedPrincipal principal
    ) {
    	return this.getUserIdentity(
    		principal.getIdentity().getSegment(6).toClassicRepresentation(), 
    		principal.getIdentity().getLastSegment().toClassicRepresentation()
    	);
    }

    /**
     * GetRunAsPrincipalResult
     *
     */
    interface GetRunAsPrincipalResult {
    	CachedPrincipal getPrincipal();
    	Path getUserIdentity();
    }
    
	/**
	 * CachedPrincipal
	 *
	 */
	public class CachedPrincipal {
	
		/**
		 * Constructor.
		 * 
		 * @param realm
		 * @param principal
		 * @param allSupergroups
		 * @param expiresAt
		 */
		public CachedPrincipal(
			DefaultRealm realm,
			Principal principal,
			Set<String> allSupergroups,
			long expiresAt
		) {
			this.realm = realm;
			this.principalIdentity = principal.refGetPath();
			this.isMemberOf = new TreeSet<String>();
			List<Group> groups = principal.getIsMemberOf();			
			for(Iterator<Group> i = groups.iterator(); i.hasNext(); ) {
				try {
					Group group = i.next();
					this.isMemberOf.add(group.refGetPath().getLastSegment().toClassicRepresentation());
				} catch(Exception e) {
					ServiceException e0 = new ServiceException(
						e,
						OpenCrxException.DOMAIN,
						BasicException.Code.GENERIC,
						"Unable to principal's group membership",
						new BasicException.Parameter("principal", principal.refGetPath())
					);
					e0.log();
				}
			}
			this.permissions = new HashSet<String[]>();
			if(principal instanceof org.opencrx.security.realm1.jmi1.Principal) {
				List<Role> roles = ((org.opencrx.security.realm1.jmi1.Principal)principal).getGrantedRole();
				for(Role role: roles) {
					Collection<Permission> permissions = role.getPermission();
					for(Permission permission: permissions) {
						for(String action: permission.getAction()) {
							this.permissions.add(
								new String[]{permission.getName(), action}
							);
						}
					}
				}
			}
			this.allSupergroups = allSupergroups;
			this.expiresAt = expiresAt;
		}
		
		public Path getIdentity(
		) {
			return this.principalIdentity;
		}
		
		public DefaultRealm getRealm(
		) {
			return this.realm;
		}
		
		public void setPrimaryGroup(
			Path primaryGroup
		) {
			this.primaryGroup = primaryGroup;
		}
		
		public Path getPrimaryGroup(
		) {
			return this.primaryGroup;
		}
		
		public Set<String> getIsMemberOf(
		) {
			return this.isMemberOf;
		}
		
		public Set<String> getPermissions(
			String action
		) {
			Set<String> permissions = new HashSet<String>();
			for(String[] permission: this.permissions) {
				if(action.equals(permission[1])) {
					permissions.add(permission[0]);
				}
			}
			return permissions;
		}
		
		public long getExpiresAt(
		) {
			return this.expiresAt;
		}
		
	    private List<PrincipalGroup> getSubgroups(
	    	PersistenceManager pm
	    ) throws ResourceException {
	    	Realm realm = (Realm)pm.getObjectById(this.realm.getRealmIdentity());
	    	Principal group = (Principal)pm.getObjectById(
	    		this.principalIdentity
	    	);
	    	// Do not calculate subgroups for final groups
	    	if(group instanceof PrincipalGroup && Boolean.TRUE.equals(((PrincipalGroup)group).isFinal())) {
	    		return Collections.emptyList();
	    	}
		    PrincipalGroupQuery query = (PrincipalGroupQuery)pm.newQuery(PrincipalGroup.class);
	    	query.thereExistsIsMemberOf().equalTo(group);
	    	return realm.getPrincipal(query);
	    }

		private Set<String> getAllSubgroups(
			PersistenceManager pm
		) throws ResourceException {
			if(this.allSubgroups == null) {
	            Set<String> allSubgroups = new HashSet<String>();
	            allSubgroups.add(AccessControl_2.this.getQualifiedPrincipalName(this.principalIdentity));
	            for(PrincipalGroup subgroup: this.getSubgroups(pm)) {
	            	String qualifiedGroupName = AccessControl_2.this.getQualifiedPrincipalName(subgroup.refGetPath());
	            	if(!allSubgroups.contains(qualifiedGroupName)) {
	                	allSubgroups.addAll(
	                		this.realm.getPrincipal(qualifiedGroupName, pm).getAllSubgroups(pm)
	                	);
	            	}
	            }
	            this.allSubgroups = allSubgroups;
			}
            return this.allSubgroups;
		}

		public Set<String> getAllSupergroups(
		) {
			return this.allSupergroups;
		}
		
		public String toString(
		) {
			return this.getIdentity().toString();
		}
		
		private final DefaultRealm realm;
		private long expiresAt;
		private final Path principalIdentity;
		private final Set<String> isMemberOf;
		private final Set<String[]> permissions;
		private Set<String> allSubgroups = null;
		private final Set<String> allSupergroups;
		private Path primaryGroup;
	}

    /**
     * Default realm implementation. Overload for custom-specific policies.
     */
    public class DefaultRealm {
        
        /**
         * Constructor.
         * 
         * @param realmIdentity
         * @throws ServiceException
         */
        public DefaultRealm(
            Path realmIdentity
        ) throws ResourceException {
            this.realmIdentity = realmIdentity;
            this.isActive = true;
            if(System.getProperty(SecurityKeys.ENABLE_SECURITY_PROPERTY) != null) {
                this.isActive = "true".equals(System.getProperty(SecurityKeys.ENABLE_SECURITY_PROPERTY));
            }
            if(!this.isActive) {
                System.out.println("WARNING: AccessControl_1 is not active. Activate with system property " + SecurityKeys.ENABLE_SECURITY_PROPERTY + "=true. Default is true.");
            }
            if(System.getProperty(SecurityKeys.REALM_REFRESH_RATE_MILLIS) != null) {
            	this.cachedPrincipalsTTL = Long.valueOf(System.getProperty(SecurityKeys.REALM_REFRESH_RATE_MILLIS));
            }
        }

        /**
         * Retrieve principal for given principal name.
         * 
         * @param principalName
         * @return
         * @throws ServiceException
         */
        protected CachedPrincipal getPrincipal(
            String principalName,
            PersistenceManager pm
        ) throws ResourceException {
        	// In case name is qualified
        	if(principalName.indexOf(":") > 0) {
        		principalName = principalName.substring(principalName.indexOf(":") + 1);
        	}
        	CachedPrincipal cachedPrincipal = this.cachedPrincipals.get(principalName);
        	if(cachedPrincipal == null || System.currentTimeMillis() > cachedPrincipal.getExpiresAt()) {
                Principal principal = null;
                try {
                    principal = (Principal)pm.getObjectById(
                        this.realmIdentity.getDescendant(new String[]{"principal", principalName})
                    );
                    Set<String> allSupergroups = new HashSet<String>();
                    allSupergroups.add(AccessControl_2.this.getQualifiedPrincipalName(principal.refGetPath()));
                    List<Group> supergroups = principal.getIsMemberOf();
                    for(Iterator<Group> i = supergroups.iterator(); i.hasNext(); ) {
                    	try {
                    		Group supergroup = i.next();
    	                	String qualifiedGroupName = AccessControl_2.this.getQualifiedPrincipalName(
    	                		supergroup.refGetPath()
    	                	);
    	                	if(!allSupergroups.contains(qualifiedGroupName)) {
    		                	allSupergroups.addAll(
    		                		this.getPrincipal(qualifiedGroupName, pm).getAllSupergroups()
    		                	);
    	                	}
                    	} catch(Exception e) {
        					ServiceException e0 = new ServiceException(
        						e,
        						OpenCrxException.DOMAIN,
        						BasicException.Code.GENERIC,
        						"Unable to principal's group membership",
        						new BasicException.Parameter("principal", principal.refGetPath())
        					);
        					e0.log();
                    	}
                    }
                    this.cachedPrincipals.put(
                        principalName,
                        cachedPrincipal = new CachedPrincipal(
                        	this,
                        	principal,
                        	allSupergroups,
                        	System.currentTimeMillis() + this.cachedPrincipalsTTL
                        )
                    );
                } catch(Exception e) {
                	new ServiceException(e).log();
                    this.cachedPrincipals.remove(principalName);
                }
        	}
    	    if(cachedPrincipal == null) {
            	SysLog.warning("principal not found", principalName);
    			throw ResourceExceptions.initHolder(
            		new ResourceException(
            			"principal not found",
        				BasicException.newEmbeddedExceptionStack(
                            BasicException.Code.DEFAULT_DOMAIN,
                            BasicException.Code.NOT_FOUND, 
                            new BasicException.Parameter("realm", this.realmIdentity),
                            new BasicException.Parameter("principal", principalName)
        				)
        			)
                );             	
    	    }
    	    return cachedPrincipal;
        }

	    /**
	     * Get runAs principal according to service header and available runAs permissions.
	     * 
	     * @param header
	     * @param request
	     * @param interaction
	     * @return
	     * @throws ServiceException
	     */
	    public GetRunAsPrincipalResult getRunAsPrincipal(
	    	RequestRecord request,
	    	List<String> principalChain,
	    	DataproviderRequestProcessor p,
	    	PersistenceManager pm
	    ) throws ResourceException {
	    	try {
		        CachedPrincipal principal = this.getPrincipal(
		        	principalChain.get(0),
		        	pm
		        );
		        Path userIdentity = AccessControl_2.this.getUser(principal);
		        // Check for runAs permission
		        Set<String> runAsPermissions = new HashSet<String>();
		        if(
		        	(principalChain.size() >= 2) &&
		        	this.hasPermission(request, null, null, principal, userIdentity, Action.RUN_AS, runAsPermissions, p, pm)
		        ) {
		        	boolean hasRunAsPermission = false;
		        	for(String runAsPermission: runAsPermissions) {
		        		if(runAsPermission.indexOf("@") > 0) {
		        			String runAsPrincipalName = runAsPermission.substring(runAsPermission.indexOf("@") + 1);
		        			if(runAsPrincipalName.equals(principalChain.get(1))) {
		        				if(hasRunAsPermission) {
		        	        		SysLog.warning("Multiple runAs permissions found. Accepting first only.", Arrays.asList(principalChain, principal, runAsPermissions));        					
		        				} else {
				        			CachedPrincipal runAsPrincipal = this.getPrincipal(runAsPrincipalName, pm);
				        	        Path runAsUserIdentity = AccessControl_2.this.getUser(runAsPrincipal);
				        	        SysLog.detail("Applying runAs permission", Arrays.asList(principal, userIdentity, runAsPrincipal, runAsUserIdentity));
				        	        principal = runAsPrincipal;
				        	        userIdentity = runAsUserIdentity;
				        	        hasRunAsPermission = true;
		        				}
		        			}
		        		} else if(!runAsPermission.equals(ALL_PERMISSION)){
			        		SysLog.warning("Invalid format for runAs permission. Accepted format is 'authority@principal'. Ignoring.", Arrays.asList(principal, runAsPermissions));	        			
		        		}
		        	}
		        }
		        final CachedPrincipal fPrincipal = principal;
		        final Path fUserIdentity = userIdentity;
		        return new GetRunAsPrincipalResult(){
					@Override
	                public CachedPrincipal getPrincipal() {
						return fPrincipal;
	                }
					@Override
	                public Path getUserIdentity() {
						return fUserIdentity;
	                }	        	
		        };
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	}
	    }

        /**
         * Get primary group for given principal.
         * 
         * @param principal
         * @return
         * @throws ServiceException
         */
        protected Path getPrimaryGroup(
        	CachedPrincipal principal,
        	PersistenceManager pm
        ) throws ResourceException {
            try {
            	if(principal.getPrimaryGroup() == null) {
    	            String providerName = principal.getIdentity().getSegment(2).toClassicRepresentation();
    	            String segmentName = principal.getIdentity().getSegment(principal.getIdentity().size()-3).toClassicRepresentation();         
    	            UserHome userHome = (UserHome)pm.getObjectById(
    	                new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName, "segment", segmentName, "userHome", principal.getIdentity().getLastSegment().toClassicRepresentation())
    	            );
    	            PrincipalGroup userGroup = userHome.getPrimaryGroup();
    	            Path primaryGroup = userGroup == null 
    	            	? this.getPrincipal(principal.getIdentity().getLastSegment().toClassicRepresentation() + "." + SecurityKeys.USER_SUFFIX, pm).getIdentity() 
    	            	: this.getPrincipal(userGroup.refGetPath().getLastSegment().toClassicRepresentation(), pm).getIdentity();
    	            principal.setPrimaryGroup(primaryGroup);           
            	}
            	return principal.getPrimaryGroup();
            } catch(Exception e) {
                // In case the principal does not have a user home
                // which defines the primary group fallback to user principal
            	Path primaryGroup = 
            		this.getPrincipal(principal.getIdentity().getLastSegment().toClassicRepresentation() + "." + SecurityKeys.USER_SUFFIX, pm).getIdentity();
            	principal.setPrimaryGroup(primaryGroup);
            	return primaryGroup;
            }
        }

        /**
         * Get security action for given object request.
         * 
         * @param ispec
         * @param object
         * @return
         */
        protected Action getAccessControlAction(
        	RestInteractionSpec ispec,
        	Object_2Facade object
        ) {
        	switch(ispec.getFunction()) {
        		case GET: return Action.READ;
        		case DELETE: return Action.DELETE;
        		case PUT: {
        			// Handle object touch (modifiedAt, modifiedBy) as operation invocation on object
        			if(object.getValue().size() == 2) {
        				if(
        					// Operation invocations on activity creator require only read permission
        					object.getPath().isLike(ACTIVITY_CREATOR_IDENTITY_PATTERN) 
        				) {
        					return Action.READ;
        				} else {
        					return Action.UPDATE;
        				}
        			} else {
        				return Action.UPDATE;
        			}
        		}
        		case POST: return Action.UPDATE;
        	}
        	return Action.UPDATE;
        }

        /**
         * Get permissions for given principal and access level.
         * 
         * @param request
         * @param principal
         * @param userIdentity
         * @param accessLevel
         * @param action
         * @return
         */
        protected Set<String> getPermissions(
        	CachedPrincipal principal,
        	Path userIdentity,
            short accessLevel,
            Action action,
            PersistenceManager pm
        ) {
            Set<String> permissions = new HashSet<String>();
        	//
        	// Map group memberships to permissions
        	//
        	{
	            // GLOBAL or Root
	            if(
	            	!this.isActive || 
	            	(accessLevel == SecurityKeys.ACCESS_LEVEL_GLOBAL) || 
	            	principal.getIdentity().getLastSegment().toClassicRepresentation().equals(SecurityKeys.ROOT_PRINCIPAL)
	            ) {
	                return null;
	            }
	            // PRIVATE --> grant requesting user access to all owned objects
	            if(accessLevel >= SecurityKeys.ACCESS_LEVEL_PRIVATE) {
	                permissions.add(
	                    AccessControl_2.this.getQualifiedPrincipalName(principal.getIdentity())
	                );
	                permissions.add(
	                	AccessControl_2.this.getQualifiedPrincipalName(userIdentity)
	                );
	            }
	            // BASIC, DEEP --> all direct subgroups, supergroups 
	            if(
	            	(accessLevel == SecurityKeys.ACCESS_LEVEL_DEEP) || 
	            	(accessLevel == SecurityKeys.ACCESS_LEVEL_BASIC)
	            ) {
	            	Set<String> groups = principal.getIsMemberOf();
	                if(AccessControl_2.this.useExtendedAccessLevelBasic) {	            	
	                    for(String group: groups) {
	                    	try {
	                    		CachedPrincipal p = this.getPrincipal(group, pm);
	    	                    permissions.addAll(
	    	                        p.getAllSubgroups(pm)
	    	                    );
	                    	} catch(Exception e) {
	                    		new ServiceException(e).log();
	                    	}
	                    }
	                }
	                // Add all supergroups
	                for(String group: groups) {
	                	try {
	                		CachedPrincipal p = this.getPrincipal(group, pm);
	    	                permissions.addAll(
	    	                	p.getAllSupergroups()
	    	                );
	                	} catch(Exception e) {
	                		new ServiceException(e).log();
	                	}
	                }
	            }
	            // DEEP --> all subgroups of direct and supergroups
	            if(accessLevel == SecurityKeys.ACCESS_LEVEL_DEEP) {
	                // All subgroups of all supergroups
	            	Set<String> newMemberships = new HashSet<String>();
	                for(String group: permissions) {
	                	try {
	                		CachedPrincipal p = this.getPrincipal(group, pm);
	    	                newMemberships.addAll(
	    	                	p.getAllSubgroups(pm)
	    	                );
	                	} catch(Exception e) {
	                		new ServiceException(e).log();
	                	}
	                }
	                permissions.addAll(newMemberships);
	                newMemberships.clear();
	                // ... and their supergroups
	                for(String group: permissions) {
	                	try {
	                		CachedPrincipal p = this.getPrincipal(group, pm);
	                        newMemberships.addAll(
	                            p.getAllSupergroups()
	                        );
	                	} catch(Exception e) {
	                		new ServiceException(e).log();
	                	}
	                }
	                permissions.addAll(newMemberships);
	            }
        	}
        	// Assigned permissions
        	{
        		permissions.addAll(
        			principal.getPermissions(action.getName())
        		);
        	}
            // Member of Root:Administrators --> access level global
            return permissions.contains(SecurityKeys.ROOT_ADMINISTRATORS_GROUP) ? 
            	null : 
            	permissions;
        }

        /**
         * Get identity of realm.
         * 
         * @return
         */
        public Path getRealmIdentity(
        ) {
        	return this.realmIdentity;
        }
        
        /**
         * Return true if principal has permission to perform the request.
         * 
         * @param request
         * @param secureObject
         * @param parent
         * @param principal
         * @param userIdentity
         * @param action
         * @param grantedPermissions
         * @param interaction
         * @return
         * @throws ServiceException
         */
        public boolean hasPermission(
        	RequestRecord request,
        	Object_2Facade secureObject,
        	Object_2Facade parent,        	
        	CachedPrincipal principal,
        	Path userIdentity,
        	Action action,
        	Set<String> grantedPermissions,
        	DataproviderRequestProcessor p,
        	PersistenceManager pm
        ) throws ResourceException {
        	try {
	        	Path path = request.getResourceIdentifier();
	        	if(secureObject == null && Object_2Facade.isDelegate(request)) {
					secureObject = Facades.asObject(request);
				}
	        	// DELETE
	        	if(action == Action.DELETE) {
		            Set<String> permissions = new HashSet<String>();
		            if(secureObject.attributeValuesAsList("accessLevelDelete").isEmpty()) {
		            	SysLog.error("Missing value for attribute 'accessLevelDelete'", secureObject);
		            } else {
		                permissions = this.getPermissions(
			                principal,
		                    userIdentity,
			                ((Number)secureObject.attributeValue("accessLevelDelete")).shortValue(),
			                action,
			                pm
		                );
		            }
		            if(permissions != null) {
		            	if(grantedPermissions != null) {
		            		grantedPermissions.addAll(permissions);
		            	}
		                permissions.retainAll(secureObject.attributeValuesAsList("owner"));
			            return !permissions.isEmpty();
		            } else {
		            	if(grantedPermissions != null) {
		            		grantedPermissions.add(ALL_PERMISSION);
		            	}
		            	return true;
		            }
	        	} else if(action == Action.UPDATE) {
	            	// UPDATE
		            Set<String> permissions = new HashSet<String>();
		            if(secureObject.attributeValuesAsList("accessLevelUpdate").isEmpty()) {
		            	SysLog.error("Missing value for attribute 'accessLevelUpdate'", secureObject);
		            } else {
		                permissions = this.getPermissions(
			                principal,
		                    userIdentity,
			                ((Number)secureObject.attributeValue("accessLevelUpdate")).shortValue(),
			                action,
			                pm
		                );
		            }
		            if(permissions != null) {
		            	if(grantedPermissions != null) {
		            		grantedPermissions.addAll(permissions);
		            	}
		                permissions.retainAll(secureObject.attributeValuesAsList("owner"));
			            return !permissions.isEmpty();
		            } else {
		            	if(grantedPermissions != null) {
		            		grantedPermissions.add(ALL_PERMISSION);
		            	}
		            	return true;
		            }
	        	} else if(action == Action.READ) {
	            	// READ
	                Set<String> permissions = new HashSet<String>();
	                short accessLevelBrowse = parent == null || parent.attributeValuesAsList("accessLevelBrowse").isEmpty()
	                	? ((Number)secureObject.attributeValue("accessLevelBrowse")).shortValue()
	                	: ((Number)parent.attributeValue("accessLevelBrowse")).shortValue();
                    permissions = this.getPermissions(
                        principal,
                        userIdentity,
	                    accessLevelBrowse,                        
                        action,
                        pm
                    );
	                if(permissions != null) {
	                	if(grantedPermissions != null) {
	                		grantedPermissions.addAll(permissions);
	                	}
	                    permissions.retainAll(secureObject.attributeValuesAsList("owner"));
	                    return !permissions.isEmpty();
	                } else {
		            	if(grantedPermissions != null) {
		            		grantedPermissions.add(ALL_PERMISSION);
		            	}
	                	return true;
	                }
	        	}
	        	// RUN_AS
	        	if(action == Action.RUN_AS) {
	        		Set<String> permissions = this.getPermissions(
	                    principal,
	                    userIdentity,
	                   	SecurityKeys.ACCESS_LEVEL_NA, 
	                    action,
	                    pm
	                );
	                if(permissions != null) {
	                	// runAs permissions are of the form
	                	// * object:authority/object path@runAsPrincipal', e.g. object:org.opencrx.kernel.activity1/tracker/guest@admin-Standard
	                	// * groupMembership:authority/group path@runAsPrincipal, e.g. groupMembership:org.opencrx.kernel.activity1/tracker/guest@admin-Standard@admin-Standard
	                	for(Iterator<String> i = permissions.iterator(); i.hasNext(); ) {
	                		String permission = i.next();
	                		boolean matches = false;
	                		// object: runAs permission
	                		if(permission.startsWith("object:")) {
	                			String[] components = permission.substring(7, permission.indexOf("@")).split("/");
	                			if(components.length > 0) {
	                				Path pattern = new Path("xri://@openmdx*" + components[0]).getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation());
	                				for(int j = 1; j < components.length; j++) {
	                					pattern = pattern.getDescendant(components[j]);
	                				}
	                        		if(path.isLike(pattern)) {
	                        			matches = true;
	                        		} else if(
	                        			secureObject != null &&
	                        			secureObject.getPath().startsWith(new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation(), "activity"))
	                        		) {
	                            		// runAs permission defined for activityCreator implies runAs permission for 
	                            		// activities created with this creator and their composites
	                        			Object_2Facade activity = null;
	                        			if(path.isLike(new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation(), "activity", ":*"))) {
	                        				activity = secureObject;
	                        			} else {
	                        				try {
	                        					activity = Facades.asObject(AccessControl_2.this.retrieveObject(p, secureObject.getPath().getPrefix(7), false));
	                        				} catch(Exception e) {}
	                        			}
	                        			if(
	                        				activity != null &&
	                        				activity.attributeValue("lastAppliedCreator") != null &&
	                        				((Path)activity.attributeValue("lastAppliedCreator")).isLike(pattern)
		                        		) {
		                        			matches = true;
		                        		}
	                        		}
	                			}
	                		} else if(permission.startsWith("groupMembership:")) {
	                    		// groupMembership: runAs permission
		    					// Activity group membership
		    					if(
		    						path.startsWith(new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation(), "activity")) &&
		    						path.size() >= 7
		    					) {
		                			String[] groupIdentityComponents = permission.substring(16, permission.indexOf("@")).split("/");
		                			if(groupIdentityComponents.length > 0) {
		                				Path groupIdentity = new Path("xri://@openmdx*" + groupIdentityComponents[0]).getDescendant("provider", path.getSegment(2).toClassicRepresentation(), "segment", path.getSegment(4).toClassicRepresentation());
		                				for(int j = 1; j < groupIdentityComponents.length; j++) {
		                					groupIdentity = groupIdentity.getDescendant(groupIdentityComponents[j]);
		                				}
		                				Object_2Facade group = null;
		                				try {
		                					group = Facades.asObject(AccessControl_2.this.retrieveObject(p, groupIdentity, false));
		                				} catch(Exception e) {}
		                				if(group != null) {
		            						Path activityIdentity = path.getPrefix(7);
		            						ResultRecord groupAssignments = AccessControl_2.this.findObjects(p, activityIdentity.getDescendant("assignedGroup"));
		            						for(Object object: groupAssignments) {
		            							Object_2Facade groupAssignment = null;
		            							try {
		            								groupAssignment = Facades.asObject((ObjectRecord)object);
		            							} catch(Exception e) {}
		            							if(groupAssignment != null) {
		            								if(group.getPath().equals(groupAssignment.attributeValue("activityGroup"))) {
		            									matches = true;
		            									break;
		            								}
		            							}
		            						}
	                					}
		                			}
		    					} else {
	        						// Test for other memberships TBD
	        					}
	                		}
	                		if(!matches) {
	                			i.remove();
	                		}
	                	}
	                	if(grantedPermissions != null) {
	                		grantedPermissions.addAll(permissions);
	                	}
	                    return !permissions.isEmpty();
	                } else {
		            	if(grantedPermissions != null) {
		            		grantedPermissions.add(ALL_PERMISSION);
		            	}
	                	return true;
	                }
	        	} else {
	            	SysLog.error("Unknown action", action.toString());
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
         * Restrict query according to permissions of given principal.
         * 
         * @param request
         * @param object
         * @param principal
         * @param userIdentity
         * @throws ServiceException
         */
        public void restrictQuery(
        	QueryRecord request,
        	Object_2Facade object,
        	CachedPrincipal principal,
            Path userIdentity,
            PersistenceManager pm
        ) throws ServiceException, ResourceException {
        	Set<String> memberships = new HashSet<String>();    	
            if(object.attributeValuesAsList("accessLevelBrowse").isEmpty()) {
            	SysLog.error("Missing attribute value for accessLevelBrowse", object);
            } else {
                memberships = this.getPermissions(
                    principal,
                    userIdentity,
                    ((Number)object.attributeValue("accessLevelBrowse")).shortValue(),
                    Action.READ,
                    pm
                );
            }
            // allowedPrincipals == null --> global access. Do not restrict to allowed subjects
            if(memberships != null) {
            	// Restrict access to objects for which user has read access
            	FilterProperty ownerFilterProperty = null;
            	for(FilterProperty p: FilterProperty.getFilterProperties(request.getQueryFilter())) {
            		if("owner".equals(p.name())) {
            			ownerFilterProperty = p;
            			break;
            		}
            	}
            	if(ownerFilterProperty != null) {
            		memberships.removeAll(
            			ownerFilterProperty.values()
            		);
            	}
        		// Add condition for owner
            	if(!memberships.isEmpty()) {
            		if(request.getQueryFilter() == null) {
            			request.setQueryFilter(Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class));
            		}
    	            request.getQueryFilter().getCondition().addAll(
    	            	FilterProperty.toCondition(
    	            		new FilterProperty[]{
		    	                new FilterProperty(
		    	                    Quantifier.THERE_EXISTS.code(),
		    	                    "owner",
		    	                    ConditionType.IS_IN.code(),
		    	                    memberships.toArray()
		    	                )
    	            		}
		    	         )
    	            );
            	}
            }
        }

        //-----------------------------------------------------------------------
        // Variables
        //-----------------------------------------------------------------------
        protected final Path ACTIVITY_CREATOR_IDENTITY_PATTERN = new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/:*/segment/:*/activityCreator/:*");
        
        private final Path realmIdentity;        
        private Map<String,CachedPrincipal> cachedPrincipals = new ConcurrentHashMap<String,CachedPrincipal>();
        private boolean isActive = true;
        private long cachedPrincipalsTTL = 120000;        
    }

    /**
     * Get user identity for given principal.
     * 
     * @param qualifiedPrincipalName
     * @return
     */
    protected Path getUserIdentity(
    	String qualifiedPrincipalName
    ) {
        int pos = qualifiedPrincipalName.indexOf(":");
        String realmName = null;
        String principalName = null;
        if(pos < 0) {
            SysLog.error("FATAL: object has illegal formatted owner (<realm segment>:<subject name>): " + qualifiedPrincipalName);
            realmName = "Root";
            principalName = qualifiedPrincipalName;
        } else {
            realmName = qualifiedPrincipalName.substring(0, pos);
            principalName = qualifiedPrincipalName.substring(pos+1);
        }
        return this.getUserIdentity(
        	realmName, 
        	principalName
        );
    }

    /**
     * Get user identity for principal of given realm.
     * 
     * @param realmName
     * @param principalName
     * @return
     */
    protected Path getUserIdentity(
    	String realmName,
    	String principalName
    ) {
        // <= 1.7.1 compatibility. Principals with name 'admin'|'loader' must be
        // mapped to 'admin-<segment>'
        if(SecurityKeys.ADMIN_PRINCIPAL.equals(principalName) || SecurityKeys.LOADER_PRINCIPAL.equals(principalName)) {
            principalName = principalName + SecurityKeys.ID_SEPARATOR + realmName;
        }
        // <= 1.7.1 compatibility. Qualified principal name must have the
        // format of a user principal
        if(!principalName.endsWith(SecurityKeys.USER_SUFFIX)) {
            principalName += "." + SecurityKeys.USER_SUFFIX;
        }
        Path userIdentity = this.realmIdentity.getParent().getDescendant(
            new String[]{realmName, "principal", principalName}             
        );
        return userIdentity;
    }

    /**
     * Get user identity for principal.
     * 
     * @param principal
     * @return
     * @throws ServiceException
     */
    protected Path getUser(
    	CachedPrincipal principal
    ) throws ServiceException {
    	return this.getUserIdentity(principal);
    }
        
    /**
     * Get group identity for principal.
     * 
     * @param accessPath
     * @param qualifiedPrincipalName
     * @return
     */
    protected Path getGroupIdentity(
        Path accessPath,
        String qualifiedPrincipalName
    ) {
        int pos = qualifiedPrincipalName.indexOf(":");
        String segmentName = null;
        String principalName = null;
        if(pos < 0) {
            System.err.println("FATAL: object has illegal formatted owner (<realm segment>:<subject name>): " + qualifiedPrincipalName + "; path=" + accessPath.toXRI());
            segmentName = "Root";
            principalName = qualifiedPrincipalName;
        } else {
            segmentName = qualifiedPrincipalName.substring(0, pos);
            principalName = qualifiedPrincipalName.substring(pos+1);
        }
        Path principalIdentity = this.realmIdentity.getParent().getDescendant(
            new String[]{segmentName, "principal", principalName}             
        );
        return principalIdentity;
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
     * Get qualified principal name.
     * 
     * @param principalIdentity
     * @return
     */
    protected String getQualifiedPrincipalName(
        Path principalIdentity
    ) {
        return principalIdentity.getSegment(6).toClassicRepresentation() + ":" + principalIdentity.getLastSegment().toClassicRepresentation();
    }
    
    protected DataproviderRequestProcessor newDelegateRequestProcessor(
    	RestConnection connection
    ) throws ResourceException {
    	return new DataproviderRequestProcessor(
			 PersistenceManagers.toPrincipalChain(connection.getMetaData().getUserName()),
			 this.getDelegate()
		);
    }

    /**
     * Get delegate persistence manager.
     * 
     * @return
     */
    protected PersistenceManager newDelegatePersistenceManager(
    	RestConnection connection
    ) {
    	ConnectionFactory connectionFactory = this.connectionFactory == null 
    		? connection.getConnectionFactory() 
    		: this.connectionFactory;
    	Map<String,Object> entityManagerConfiguration = new HashMap<String,Object>();
    	entityManagerConfiguration.put(
        	ConfigurableProperty.ConnectionFactory.qualifiedName(),
        	DataManagerFactory_1.getPersistenceManagerFactory(
    			Collections.singletonMap(
    				ConfigurableProperty.ConnectionFactory.qualifiedName(),
    				connectionFactory
    			)            		
        	)
        );
        entityManagerConfiguration.put(
        	ConfigurableProperty.PersistenceManagerFactoryClass.qualifiedName(),
        	EntityManagerFactory_1.class.getName()
        );	            	
    	return JDOHelper.getPersistenceManagerFactory(
    		entityManagerConfiguration
    	).getPersistenceManager(
    		SecurityKeys.ROOT_PRINCIPAL,
    		null
    	);
    }

    /**
     * Retrieve object with given identity. 
     * 
     * @param identity
     * @param preferringNotFoundException
     * @return
     * @throws ServiceException
     */
    protected ObjectRecord retrieveObject(
    	DataproviderRequestProcessor p,
        Path resourceIdentifier,
        boolean preferringNotFoundException
    ) throws ResourceException {
        QueryRecord getRequest = new org.openmdx.base.rest.spi.QueryRecord();
        getRequest.setResourceIdentifier(resourceIdentifier);
        getRequest.setFetchGroupName(FetchGroup.ALL);
        QueryFilterRecord query = Records.getRecordFactory().createMappedRecord(QueryFilterRecord.class);
        query.getOrderSpecifier().addAll(
    		AttributeSpecifier.toOrderSpecifier(
    			 new AttributeSpecifier[]{
                	new AttributeSpecifier("owner")
    			 }	        			
    		)
    	);
        getRequest.setQueryFilter(query);
    	return p.addGetRequest(getRequest);
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
     * Find objects.
     * 
     * @param reference
     * @return
     * @throws ServiceException
     */
    protected ResultRecord findObjects(
    	DataproviderRequestProcessor p,
        Path reference
    ) throws ResourceException {
    	return p.addFindRequest(reference);
    }

    /**
     * RestInteraction
     *
     */
    public class RestInteraction extends AbstractRestInteraction {
        
        public RestInteraction(
        	RestConnection connection
        ) throws ResourceException {
            super(connection,  newDelegateInteraction(connection));
        }

	    /**
	     * Get owning user for new object.
	     * 
	     * @param requestingUser
	     * @param newObjectFacade
	     * @param parentFacade
	     * @param realm
	     * @return
	     * @throws ServiceException
	     */
	    protected String getOwningUserForNewObject(
	    	Path requestingUser,
	    	Object_2Facade newObjectFacade,
	    	Object_2Facade parentFacade,
	    	DefaultRealm realm
	    ) throws ResourceException {
	    	try {
		        String owningUser = null;
		        // If new object is composite to user home set owning user to owning user of user home
		        if(
		            (newObjectFacade.getPath().size() > USER_HOME_PATH_PATTERN.size()) &&
		            newObjectFacade.getPath().getPrefix(USER_HOME_PATH_PATTERN.size()).isLike(USER_HOME_PATH_PATTERN) &&
		            !parentFacade.attributeValuesAsList("owner").isEmpty()
		        ) {
		           owningUser = (String)parentFacade.attributeValue("owner");
		        } else if(!newObjectFacade.attributeValuesAsList("owningUser").isEmpty()) {
			        // Owning user set on new object
		            owningUser = AccessControl_2.this.getQualifiedPrincipalName(
		                (Path)newObjectFacade.attributeValue("owningUser")
		            );
		        } else {
			        // Set requesting principal as default
		            // If no user found set owner to segment administrator
		            owningUser = newObjectFacade.attributeValuesAsList("owner").isEmpty() ? 
		            	requestingUser == null ? 
		            		AccessControl_2.this.getQualifiedPrincipalName(newObjectFacade.getPath(), SecurityKeys.ADMIN_PRINCIPAL) : 
		            			AccessControl_2.this.getQualifiedPrincipalName(requestingUser) : 
		            	(String)newObjectFacade.attributeValue("owner");
		        }
		        return owningUser;
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	}
	    }
	    
	    /**
	     * Get owning groups for new object.
	     * 
	     * @param requestingPrincipal
	     * @param newObjectFacade
	     * @param parentFacade
	     * @return
	     * @throws ServiceException
	     */
	    protected Set<String> getOwningGroupsForNewObject(
	    	CachedPrincipal requestingPrincipal,
	    	Object_2Facade newObjectFacade,
	    	Object_2Facade parentFacade,
	    	PersistenceManager pm
	    ) throws ResourceException {
	    	try {
		    	DefaultRealm realm = requestingPrincipal.getRealm();
		        Set<String> owningGroup = new HashSet<String>();
		        if(
		        	(newObjectFacade.getAttributeValues("owningGroup") != null) && 
		        	!newObjectFacade.attributeValuesAsList("owningGroup").isEmpty()
		        ) {
		            for(Iterator<Object> i = newObjectFacade.attributeValuesAsList("owningGroup").iterator(); i.hasNext(); ) {
		                owningGroup.add(
		                	AccessControl_2.this.getQualifiedPrincipalName((Path)i.next())
		                );
		            }
		        } else {
		        	Path userGroup = requestingPrincipal == null ? 
		        		null : 
		        		realm.getPrimaryGroup(requestingPrincipal, pm);
		            owningGroup = new HashSet<String>();
		            if(parentFacade != null) {
	                    @SuppressWarnings({
	                        "rawtypes", "unchecked"
	                    })
	                    List<String> ownersParent = new ArrayList(parentFacade.attributeValuesAsList("owner"));
		                // Do not inherit group Users at segment-level
		                if(parentFacade.getPath().size() == 5) {
		                    ownersParent.remove(
		                    	AccessControl_2.this.getQualifiedPrincipalName(newObjectFacade.getPath(), SecurityKeys.USER_GROUP_USERS)
		                    );
		                }
		                if(!ownersParent.isEmpty()) {
		                    owningGroup.addAll(
		                        ownersParent.subList(1, ownersParent.size())
		                    );
		                }
		            }
		            owningGroup.add(
		                userGroup == null ? 
		                	AccessControl_2.this.getQualifiedPrincipalName(newObjectFacade.getPath(), "Unassigned") : 
		                		AccessControl_2.this.getQualifiedPrincipalName(userGroup)
		            );
		        }  
		        return owningGroup;
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	}
	    }
	    
	    /**
	     * Get object from cache. If not in cached, retrieve it and add it to cache.
	     * 
	     * @param header
	     * @param path
	     * @return
	     * @throws ServiceException
	     */
	    private ObjectRecord getCachedObject(
	    	DataproviderRequestProcessor p,
	        Path path
	    ) throws ResourceException {    	
	    	ConcurrentMap<Path,Object[]> objectCache = AccessControl_2.getObjectCache();
	        // Remove cached parents if expired
	        for(Iterator<Map.Entry<Path,Object[]>> i = objectCache.entrySet().iterator(); i.hasNext(); ) {
	            Map.Entry<Path,Object[]> entry = i.next();
	            try {
	                if(entry != null) {
	                    if(entry.getValue() == null) {
	                        i.remove();
	                    } else {
	                        Long expiresAt = (Long)entry.getValue()[1];
	                        if((expiresAt == null) || (expiresAt < System.currentTimeMillis())) {
	                            i.remove();
	                        }
	                    }
	                }
	            } catch(Exception e) {}
	        }
	        Object[] entry = objectCache.get(path);
	        ObjectRecord object = null;
	        if(entry == null) {
	            object = AccessControl_2.this.retrieveObject(p, path, true);
	            this.addToObjectCache(object);
	        } else {
	            object = (ObjectRecord)entry[0];
	        }
	        return object;
	    }

	    /**
	     * Add object to cache.
	     * 
	     * @param object
	     * @throws ServiceException
	     */
	    private void addToObjectCache(
	    	ObjectRecord object
	    ) throws ResourceException {
	    	try {
		    	Object_2Facade facade = Facades.asObject(object);
		        if(facade.getObjectClass() != null) {
	            	AccessControl_2.getObjectCache().put(
		                facade.getPath(),
		                new Object[]{
		                    Object_2Facade.cloneObject(object),
		                    new Long(System.currentTimeMillis() + TTL_CACHED_OBJECTS)
		                }
		            );
		        } else {
		        	SysLog.error("Missing object class. Object not added to cache", facade.getPath());
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
	     * @see org.openmdx.compatibility.base.dataprovider.spi.Layer_1_0#create(org.openmdx.compatibility.base.dataprovider.cci.ServiceHeader, org.openmdx.compatibility.base.dataprovider.cci.DataproviderRequest)
	     */
	    @Override
        public boolean create(
            RestInteractionSpec ispec, 
            ObjectRecord request,
            ResultRecord response
        ) throws ResourceException {
	    	PersistenceManager pm = AccessControl_2.this.newDelegatePersistenceManager(this.getConnection());
	    	DataproviderRequestProcessor p = AccessControl_2.this.newDelegateRequestProcessor(this.getConnection());
	    	try {
		    	Path path = request.getResourceIdentifier();
		        DefaultRealm realm = AccessControl_2.this.getRealm(request, this.getPrincipalChain(), pm);
		        GetRunAsPrincipalResult getRunAsPrincipalResult = realm.getRunAsPrincipal(request, this.getPrincipalChain(), p, pm);
		        CachedPrincipal principal = getRunAsPrincipalResult.getPrincipal();
		        Path userIdentity = getRunAsPrincipalResult.getUserIdentity();
		        // Check permission. Must have update permission for parent
		        MappedRecord parent = null;
		        Object_2Facade parentFacade = null;
		        if(path.size() >= 7) {
			        parent = this.getCachedObject(p, path.getPrefix(path.size() - 2));
			        parentFacade = Facades.asObject(parent);
			        if(AccessControl_2.this.isSecureObject(parent)) {
			        	boolean hasPermission = realm.hasPermission(
			        		request, 
			        		parentFacade, 
			        		null, // parent 
			        		principal, 
			        		userIdentity, 
			        		realm.getAccessControlAction(ispec, parentFacade), 
			        		null, // grantedPermissions
			        		p,
			        		pm
			        	);
			        	if(!hasPermission) {
			    			throw ResourceExceptions.initHolder(
			            		new ResourceException(
			            			"No permission to create object.",
			        				BasicException.newEmbeddedExceptionStack(
					                    OpenCrxException.DOMAIN,
					                    OpenCrxException.AUTHORIZATION_FAILURE_CREATE,
			                            new BasicException.Parameter("object", path),
			                            new BasicException.Parameter("param0", path),
			                            new BasicException.Parameter("param1", principal.getIdentity().getSegment(6).toClassicRepresentation() + ":" +  principal.getIdentity().getLastSegment().toClassicRepresentation())
			        				)
			        			)
			                );
			            }
			        }
		        }
		        // Create object
		        // Set owner in case of secure objects
		        ObjectRecord newObject = request;
		        if(AccessControl_2.this.isSecureObject(newObject)) {
		            Object_2Facade newObjectFacade = Facades.asObject(newObject);
		            String owningUser = this.getOwningUserForNewObject(
		            	userIdentity, 
		            	newObjectFacade, 
		            	parentFacade, 
		            	realm
		            );
		            newObjectFacade.attributeValuesAsList("owner").clear();
		            newObjectFacade.attributeValuesAsList("owner").add(owningUser);		            
			        Set<String> owningGroup = this.getOwningGroupsForNewObject(
			        	principal, 
			        	newObjectFacade, 
			        	parentFacade,
			        	pm
			        );
			        newObjectFacade.attributeValuesAsList("owner").addAll(owningGroup);            
			        newObjectFacade.getValue().keySet().remove("owningUser");
			        newObjectFacade.getValue().keySet().remove("owningGroup");
		            // Access levels
		            for(
		                Iterator<String> i = Arrays.asList("accessLevelBrowse", "accessLevelUpdate", "accessLevelDelete").iterator();
		                i.hasNext();
		            ) {
		                String mode = i.next();
		                if(
		                    (newObjectFacade.attributeValuesAsList(mode).size() != 1) ||
		                    (((Number)newObjectFacade.attributeValue(mode)).shortValue() == SecurityKeys.ACCESS_LEVEL_NA)
		                ) {
		                	newObjectFacade.attributeValuesAsList(mode).clear();
		                	newObjectFacade.attributeValuesAsList(mode).add(
		                        new Short(
		                            "accessLevelBrowse".equals(mode) ? 
		                            	SecurityKeys.ACCESS_LEVEL_DEEP : 
		                            	SecurityKeys.ACCESS_LEVEL_BASIC
		                        )
		                    );
		                }
		            }
		        }
		        if(super.create(ispec, request, response)) {
		        	this.addToObjectCache(request);
		        }
		        AccessControl_2.this.completeReply(response);
		        return true;
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	} finally {
	    		if(pm != null) {
	    			pm.close();
	    		}
	    	}
	    }
	    
	    /* (non-Javadoc)
	     * @see org.openmdx.compatibility.base.dataprovider.spi.Layer_1_0#find(org.openmdx.compatibility.base.dataprovider.cci.ServiceHeader, org.openmdx.compatibility.base.dataprovider.cci.DataproviderRequest)
	     */
	    @Override
        public boolean find(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
	    	PersistenceManager pm = AccessControl_2.this.newDelegatePersistenceManager(this.getConnection());
	    	DataproviderRequestProcessor p = AccessControl_2.this.newDelegateRequestProcessor(this.getConnection());	    	
	    	try {
		    	Path path = request.getResourceIdentifier();
		        DefaultRealm realm = AccessControl_2.this.getRealm(request, this.getPrincipalChain(), pm);
		        GetRunAsPrincipalResult getRunAsPrincipalResult = realm.getRunAsPrincipal(request, this.getPrincipalChain(), p, pm);
		        CachedPrincipal principal = getRunAsPrincipalResult.getPrincipal();
		        Path userIdentity = getRunAsPrincipalResult.getUserIdentity();
		        MappedRecord parent = this.getCachedObject(p, path.getParent());
		        Object_2Facade parentFacade = Facades.asObject(parent);
		        ModelElement_1_0 referencedType = AccessControl_2.this.getReferencedType(
		        	path,
		            FilterProperty.getFilterProperties(request.getQueryFilter())
		        );
		        if(
		        	AccessControl_2.this.isSecureObject(referencedType) && 
		        	AccessControl_2.this.isSecureObject(parent)
		        ) {
		        	boolean containsSharedAssociation = AccessControl_2.this.model.containsSharedAssociation(path);
		        	if(containsSharedAssociation) {
		        		Object_2Facade objectParentFacade = null;
		        		Path compositeParentPath = null;
		        		for(Map.Entry<Path,Path> e: sharedAssociationToCompositeParentPathMap.entrySet()) {
		        			if(path.isLike(e.getKey())) {
		        				compositeParentPath = e.getValue();
		        				break;
		        			}
		        		}
		        		if(compositeParentPath != null) {
				        	objectParentFacade = Facades.asObject(
				        		this.getCachedObject(p, compositeParentPath)
				        	);
		        		} else {
			        		Long originalSize = request.getSize();
			        		Long originalPosition = request.getPosition();
			        		// In case of shared association get the composite parent and 
			        		// restrict query to the parent's security settings
			        		request.setSize(1L);
			        		request.setPosition(0L);
					        super.find(
					        	ispec, 
					        	request, 
					        	response
					        );
			        		if(!response.isEmpty()) {
			        			compositeParentPath = Object_2Facade.getPath((ObjectRecord)response.get(0)).getParent().getParent();
					        	objectParentFacade = Facades.asObject(
					        		this.getCachedObject(p, compositeParentPath)
					        	);
				        		request.setPosition(originalPosition);
				        		request.setSize(originalSize);
				        		response.clear();
				        		// Only add to cache if composite parent is segment
					        	if(compositeParentPath.size() == 5) {
					        		Path sharedAssociationPathPattern = path.getPrefix(5);
					        		for(int i = 5; i < path.size(); i++) {
					        			if(i % 2 == 0) {
					        				sharedAssociationPathPattern = sharedAssociationPathPattern.getChild(":*");
					        			} else {
					        				sharedAssociationPathPattern = sharedAssociationPathPattern.getChild(path.getSegment(i).toClassicRepresentation());
					        			}
					        		}
						        	sharedAssociationToCompositeParentPathMap.put(
						        		sharedAssociationPathPattern,
						        		compositeParentPath
						        	);
					        	}
			        		}
			        	}
			        	// Restrict query according to security settings of composite parent
		        		if(objectParentFacade != null) {
				        	realm.restrictQuery(
				        		request,
				        		objectParentFacade, 
				        		principal, 
				        		userIdentity,
				        		pm
				        	);
		        		}
		        	}
		        	// Restrict query according to security settings of parent
		        	realm.restrictQuery(
		        		request, 
		        		parentFacade, 
		        		principal, 
		        		userIdentity,
		        		pm
		        	);
			        super.find(
			        	ispec, 
			        	request, 
			        	response
			        );
		        } else {
			        super.find(
			        	ispec, 
			        	request, 
			        	response
			        );
		        }
		        AccessControl_2.this.completeReply(response);
		        return true;
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	} finally {
	    		if(pm != null) {
	    			pm.close();
	    		}
	    	}
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.compatibility.base.dataprovider.spi.Layer_1_0#get(org.openmdx.compatibility.base.dataprovider.cci.ServiceHeader, org.openmdx.compatibility.base.dataprovider.cci.DataproviderRequest)
	     */
	    @Override
        public boolean get(
            RestInteractionSpec ispec, 
            QueryRecord request, 
            ResultRecord response
        ) throws ResourceException {
	    	PersistenceManager pm = AccessControl_2.this.newDelegatePersistenceManager(this.getConnection());
	    	DataproviderRequestProcessor p = AccessControl_2.this.newDelegateRequestProcessor(this.getConnection());		    	
	    	try {
		    	Path path = request.getResourceIdentifier();
		    	Model_1_0 model = Model_1Factory.getModel();
		        DefaultRealm realm = AccessControl_2.this.getRealm(request, this.getPrincipalChain(), pm);
		        GetRunAsPrincipalResult getRunAsPrincipalResult = realm.getRunAsPrincipal(request, this.getPrincipalChain(), p, pm);
		        CachedPrincipal principal = getRunAsPrincipalResult.getPrincipal();
		        Path userIdentity = getRunAsPrincipalResult.getUserIdentity();
		        // Need multi-valued attribute 'owner' of retrieved object. This
		        // is required for permission check in next step --> realm.hasPermission()
		        try {
		        	request.setFetchGroupName(FetchGroup.ALL);
		        } catch(Exception e) {}
		        super.get(
		            ispec,
		            request,
		            response
		        );
		        if(response.isEmpty()) {
		        	return true;
		        } else {
			        MappedRecord parent = null;
			        if(path.size() >= 7) {
				        parent = this.getCachedObject(p, path.getPrefix(path.size() - 2));
				        Object_2Facade parentFacade = Facades.asObject(parent);
				        ModelElement_1_0 referencedType = model.getTypes(path)[2];
				        if(AccessControl_2.this.isSecureObject(referencedType) && AccessControl_2.this.isSecureObject(parent)) {
				        	Object_2Facade objectFacade = Facades.asObject((ObjectRecord)response.get(0));
				        	boolean hasPermission = realm.hasPermission(
				        		request,
				        		objectFacade, 
				        		parentFacade, 
				        		principal, 
				        		userIdentity,
				        		realm.getAccessControlAction(ispec, parentFacade),
				        		null, // grantedPermissions
				        		p,
				        		pm
				        	);
				        	if(hasPermission) {
				            	AccessControl_2.this.completeReply(response);
				                return true;
				            } else {
				    			throw ResourceExceptions.initHolder(
				            		new ResourceException(
				            			"No permission to access requested object.",
				        				BasicException.newEmbeddedExceptionStack(
				    	                    OpenCrxException.DOMAIN,
				    	                    OpenCrxException.AUTHORIZATION_FAILURE_READ, 
				                            new BasicException.Parameter("object", path),
				                            new BasicException.Parameter("param0", path.toXRI()),
				                            new BasicException.Parameter("param1", principal.getIdentity().getSegment(6).toClassicRepresentation() + ":" +  principal.getIdentity().getLastSegment().toClassicRepresentation()),                            
				                            new BasicException.Parameter("param2", userIdentity.toXRI())                            
				        				)
				        			)
				                );	            	
				            }
				        }
			        }
			        AccessControl_2.this.completeReply(response);
			        return true;
		        }
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	} finally {
	    		if(pm != null) {
	    			pm.close();
	    		}
	    	}
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.compatibility.base.dataprovider.spi.Layer_1_0#remove(org.openmdx.compatibility.base.dataprovider.cci.ServiceHeader, org.openmdx.compatibility.base.dataprovider.cci.DataproviderRequest)
	     */
	    @Override
        public boolean delete(
            RestInteractionSpec ispec, 
            ObjectRecord request
        ) throws ResourceException {
	    	PersistenceManager pm = AccessControl_2.this.newDelegatePersistenceManager(this.getConnection());
	    	DataproviderRequestProcessor p = AccessControl_2.this.newDelegateRequestProcessor(this.getConnection());
	    	try {
		    	Path path = request.getResourceIdentifier();
		        DefaultRealm realm = AccessControl_2.this.getRealm(request, this.getPrincipalChain(), pm);
		        GetRunAsPrincipalResult getRunAsPrincipalResult = realm.getRunAsPrincipal(request, this.getPrincipalChain(), p, pm);
		        CachedPrincipal principal = getRunAsPrincipalResult.getPrincipal();
		        Path userIdentity = getRunAsPrincipalResult.getUserIdentity();
		        MappedRecord object = AccessControl_2.this.retrieveObject(p, path, true);
		        if(AccessControl_2.this.isSecureObject(object)) {   	        	
		            Object_2Facade objectFacade = Facades.asObject(object);
		            boolean hasPermission = realm.hasPermission(
		            	request, 
		            	objectFacade,
		            	null, // parent
		            	principal, 
		            	userIdentity, 
		            	realm.getAccessControlAction(ispec, objectFacade),
		            	null, // grantedPermissions
		            	p,
		            	pm
		            );
		            if(!hasPermission) {
		    			throw ResourceExceptions.initHolder(
		            		new ResourceException(
		            			"No permission to delete requested object.",
		        				BasicException.newEmbeddedExceptionStack(
		    	                    OpenCrxException.DOMAIN,
		    	                    OpenCrxException.AUTHORIZATION_FAILURE_DELETE, 
		                            new BasicException.Parameter("object", path),
		                            new BasicException.Parameter("param0", path.toXRI()),
		                            new BasicException.Parameter("param1", principal.getIdentity().getSegment(6).toClassicRepresentation() + ":" +  principal.getIdentity().getLastSegment().toClassicRepresentation()),                            
		                            new BasicException.Parameter("param2", userIdentity.toXRI())                            
		        				)
		        			)
		                );	            	
		            }
		        }
		        objectCache.remove(path);
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
	    	} finally {
	    		if(pm != null) {
	    			pm.close();
	    		}
	    	}
	    }

	    /* (non-Javadoc)
	     * @see org.openmdx.compatibility.base.dataprovider.spi.Layer_1_0#replace(org.openmdx.compatibility.base.dataprovider.cci.ServiceHeader, org.openmdx.compatibility.base.dataprovider.cci.DataproviderRequest)
	     */
	    @Override
	    protected boolean update(
	        RestInteractionSpec ispec, 
	        ObjectRecord request, 
	        ResultRecord response
	    ) throws ResourceException {
	    	PersistenceManager pm = AccessControl_2.this.newDelegatePersistenceManager(this.getConnection());
	    	DataproviderRequestProcessor p = AccessControl_2.this.newDelegateRequestProcessor(this.getConnection());		    	
	    	try {
		    	Path path = request.getResourceIdentifier();
		        DefaultRealm realm = AccessControl_2.this.getRealm(request, this.getPrincipalChain(), pm);
	            ObjectRecord replacement = request;
	            Object_2Facade replacementFacade = Facades.asObject(replacement);
		        GetRunAsPrincipalResult getRunAsPrincipalResult = realm.getRunAsPrincipal(request, this.getPrincipalChain(), p, pm);
		        CachedPrincipal principal = getRunAsPrincipalResult.getPrincipal();
		        Path userIdentity = getRunAsPrincipalResult.getUserIdentity();
		        MappedRecord object = AccessControl_2.this.retrieveObject(p, path, true);
		        if(AccessControl_2.this.isSecureObject(object)) {
		            Object_2Facade objectFacade = Facades.asObject(object);
		            boolean hasPermission = 
		            	realm.hasPermission(
			            	request, 
			            	objectFacade, 
			            	null, // parent
			            	principal, 
			            	userIdentity, 
			            	realm.getAccessControlAction(ispec, replacementFacade),
			            	null, // grantedPermissions
			            	p,
			            	pm
			            );
		            if(!hasPermission) {
		    			throw ResourceExceptions.initHolder(
		            		new ResourceException(
		            			"No permission to update requested object.",
		        				BasicException.newEmbeddedExceptionStack(
		    	                    OpenCrxException.DOMAIN,
		    	                    OpenCrxException.AUTHORIZATION_FAILURE_UPDATE, 
		                            new BasicException.Parameter("object", path),
		                            new BasicException.Parameter("param0", path.toXRI()),
		                            new BasicException.Parameter("param1", principal.getIdentity().getSegment(6).toClassicRepresentation() + ":" +  principal.getIdentity().getLastSegment().toClassicRepresentation()),                            
		                            new BasicException.Parameter("param2", userIdentity.toXRI())                            
		        				)
		        			)
		                );
		            }
		            // Derive attribute owner from owningUser and owningGroup
			        // Derive newOwner from owningUser and owningGroup
			        List<Object> newOwner = new ArrayList<Object>();	        
			        // owning user
			        String owningUser = null;
			        if(!replacementFacade.attributeValuesAsList("owningUser").isEmpty()) {
			            owningUser = AccessControl_2.this.getQualifiedPrincipalName(
			            	(Path)replacementFacade.attributeValue("owningUser")
			            );
			        } else {
			            // if no user found set owner to segment administrator
			            owningUser = objectFacade.attributeValuesAsList("owner").isEmpty() 
			            	? userIdentity == null 
			            		? AccessControl_2.this.getQualifiedPrincipalName(replacementFacade.getPath(), SecurityKeys.ADMIN_PRINCIPAL) 
			            		: AccessControl_2.this.getQualifiedPrincipalName(userIdentity) 
			            	: (String)objectFacade.attributeValue("owner");
			        }
			        newOwner.add(owningUser);
			        // owning group
			        Set<Object> owningGroup = new HashSet<Object>();
			        if(replacementFacade.getAttributeValues("owningGroup") != null) {
		                // replace owning group
			            for(Iterator<Object> i = replacementFacade.attributeValuesAsList("owningGroup").iterator(); i.hasNext(); ) {
		                    Path group = (Path)i.next();
		                    if(group != null) {
		    	                owningGroup.add(
		    	                	AccessControl_2.this.getQualifiedPrincipalName(group)
		    	                );
		                    }
			            }
			        } else {
		                // keep existing owning group
		                if(objectFacade.attributeValuesAsList("owner").size() > 1) {
		                    owningGroup.addAll(
		                    	objectFacade.attributeValuesAsList("owner").subList(1, objectFacade.attributeValuesAsList("owner").size())
		                    );
		                }
			        }
			        newOwner.addAll(owningGroup);
			        // Only replace owner if modified
			        if(!objectFacade.attributeValuesAsList("owner").containsAll(newOwner) || !newOwner.containsAll(objectFacade.attributeValuesAsList("owner"))) {
			        	replacementFacade.attributeValuesAsList("owner").clear();
			        	replacementFacade.attributeValuesAsList("owner").addAll(newOwner);
			        }
			        replacementFacade.getValue().keySet().remove("owningUser");
			        replacementFacade.getValue().keySet().remove("owningGroup");
		        }
		        objectCache.remove(path);
		        super.update(
		        	ispec, 
		        	request, 
		        	response
		        );
		        AccessControl_2.this.completeReply(response);
		        return true;
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	} finally {
	    		if(pm != null) {
	    			pm.close();
	    		}
	    	}
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
	    	PersistenceManager pm = AccessControl_2.this.newDelegatePersistenceManager(this.getConnection());
	    	DataproviderRequestProcessor p = AccessControl_2.this.newDelegateRequestProcessor(this.getConnection());
            String operationName = request.getTarget().getLastSegment().toClassicRepresentation();
	    	try {
		    	Path path = request.getResourceIdentifier();
		        DefaultRealm realm = AccessControl_2.this.getRealm(request, this.getPrincipalChain(), pm);
		        if("checkPermissions".equals(operationName)) {
		            String principalName = (String)request.getBody().get("principalName");
		            CachedPrincipal principal = null;
		            if(principalName != null) {
		            	principal = realm.getPrincipal(principalName, pm);
		            }
		            if(principal == null) {
		    			throw ResourceExceptions.initHolder(
		            		new ResourceException(
			                    "Requested principal not found.",
		        				BasicException.newEmbeddedExceptionStack(
		    	                    OpenCrxException.DOMAIN,
		    	                    OpenCrxException.AUTHORIZATION_FAILURE_MISSING_PRINCIPAL, 
		    	                    new BasicException.Parameter("principal", principalName),
		    	                    new BasicException.Parameter("param0", principalName),
		    	                    new BasicException.Parameter("param1", AccessControl_2.this.realmIdentity)
		        				)
		        			)
		                );	            	
		            }
		            Path objectIdentity = path.getPrefix(path.size() - 2);
		            if(objectIdentity.size() >= 5) {
		            	Path userIdentity = AccessControl_2.this.getUser(principal);
		            	Object_2Facade parentFacade = null;
		            	if(objectIdentity.size() >= 7) {
					        MappedRecord parent = this.getCachedObject(p, path.getPrefix(path.size() - 2));
				            parentFacade = Facades.asObject(parent);
		            	}
			            MappedRecord object = AccessControl_2.this.retrieveObject(p, objectIdentity, true);
			            Object_2Facade objectFacade = Facades.asObject(object);
			            response.setBody(AccessControl_2.this.newOperationResult("org:opencrx:kernel:base:CheckPermissionsResult"));
			            // Read permissions
			            Set<String> grantedPermissions = new HashSet<String>();
			            boolean hasPermission = realm.hasPermission(
			            	request, 
			            	objectFacade, 
			            	parentFacade, 
			            	principal, 
			            	userIdentity, 
			            	Action.READ, 
			            	grantedPermissions,
			            	p,
			            	pm
			            );
		                response.getBody().put("grantedPermissionsRead", grantedPermissions);
			            response.getBody().put("hasReadPermission", hasPermission);
			            // Delete permissions
			            grantedPermissions = new HashSet<String>();
			            hasPermission = realm.hasPermission(
			            	request, 
			            	objectFacade, 
			            	parentFacade, 
			            	principal, 
			            	userIdentity, 
			            	Action.DELETE, 
			            	grantedPermissions,
			            	p,
			            	pm
			            );
			            response.getBody().put("grantedPermissionsDelete", grantedPermissions);
			            response.getBody().put("hasDeletePermission", hasPermission);
			            // Update
			            grantedPermissions = new HashSet<String>();
			            hasPermission = realm.hasPermission(
			            	request, 
			            	objectFacade, 
			            	parentFacade, 
			            	principal, 
			            	userIdentity, 
			            	Action.UPDATE, 
			            	grantedPermissions,
			            	p,
			            	pm
			            );
			            response.getBody().put("grantedPermissionsUpdate", grantedPermissions);
			            response.getBody().put("hasUpdatePermission", hasPermission);
			            // Return result
			            return true;
		            } else {
		    			throw ResourceExceptions.initHolder(
		            		new ResourceException(
			                    "Can not invoke checkPermissions on this object",
		        				BasicException.newEmbeddedExceptionStack(
		    	                    BasicException.Code.DEFAULT_DOMAIN,
		    	                    BasicException.Code.ASSERTION_FAILURE, 
		                            new BasicException.Parameter("path", path),
		    	                    new BasicException.Parameter("principal", principalName),
		    	                    new BasicException.Parameter("param0", principalName),
		    	                    new BasicException.Parameter("param1", AccessControl_2.this.realmIdentity)
		        				)
		        			)
		                );	            	
		            }
		        }
		        return super.invoke(
		            ispec,
		            request,
		            response
		        );
	    	} catch(ServiceException e) {
                throw ResourceExceptions.initHolder(
                    new ResourceException(
                        BasicException.newEmbeddedExceptionStack(e)
                    )
                );	    		
	    	} finally {
	    		if(pm != null) {
	    			pm.close();
	    		}
	    	}
	    }
    }

    /**
     * Complete derived attributes.
     * 
     * @param header
     * @param object
     * @throws ServiceException
     */
    protected void completeOwningUserAndGroup(
      ObjectRecord object
    ) throws ResourceException {
    	try {
	    	Object_2Facade facade = Facades.asObject(object);
	    	facade.getValue().keySet().remove("owningUser");
	    	facade.getValue().keySet().remove("owningGroup");
	        if(!facade.attributeValuesAsList("owner").isEmpty()) {
	            if((String)facade.attributeValue("owner") == null) {
	            	SysLog.error("Values of attribute owner are corrupt. Element at index 0 (owning user) is missing. Fix the database", object);
	            } else {
	            	facade.attributeValuesAsList("owningUser").add(
		                this.getUserIdentity((String)facade.attributeValue("owner"))
		            );
	            }
	        }
	        for(int i = 1; i < facade.attributeValuesAsList("owner").size(); i++) {
	        	facade.attributeValuesAsList("owningGroup").add(
	        		this.getGroupIdentity(facade.getPath(), (String)facade.attributeValuesAsList("owner").get(i))
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
     * Complete derived attributes for returned objects.
     * 
     * @param header
     * @param object
     * @throws ServiceException
     */
    protected void completeObject(
      ObjectRecord object
    ) throws ResourceException {
        this.completeOwningUserAndGroup(object);
    }

    /**
     * Complete reply.
     * 
     * @param header
     * @param reply
     * @return
     * @throws ServiceException
     */
    protected void completeReply(
      ResultRecord objects
    ) throws ResourceException {
    	if(objects != null) {
	    	for(Object object : objects) {
	          this.completeObject(
	              (ObjectRecord)object
	          );
	    	}
    	}
    }

    /**
     * Test whether object is instance of PrincipalGroup.
     * 
     * @param object
     * @return
     * @throws ServiceException
     */
    protected boolean isPrincipalGroup(
    	MappedRecord object
    ) throws ServiceException {
        String objectClass = Object_2Facade.getObjectClass(object);
        return this.model.isSubtypeOf(
            objectClass,
            "org:opencrx:security:realm1:PrincipalGroup"
        );
    }

    /**
     * Test whether object's type is a subclass of SecureObject.
     * 
     * @param object
     * @return
     * @throws ServiceException
     */
    protected boolean isSecureObject(
    	MappedRecord object
    ) throws ResourceException {
    	try {
	        String objectClass = Object_2Facade.getObjectClass(object);
	        if(objectClass == null) {
	        	SysLog.error("Undefined object class", Object_2Facade.getPath(object));
	            return true;
	        } else {
	            return this.model.isSubtypeOf(
	                objectClass,
	                "org:opencrx:kernel:base:SecureObject"
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
     * Test whether type is a subclass of SecureObject.
     * 
     * @param type
     * @return
     * @throws ServiceException
     */
    protected boolean isSecureObject(
      ModelElement_1_0 type
    ) throws ServiceException {
        return this.model.isSubtypeOf(
            type,
            "org:opencrx:kernel:base:SecureObject"
        );
    }

    /**
     * Extract principal name from request header.
     * 
     * @param header
     * @return
     */
    protected List<String> getPrincipalChain(
    	Connection connection
    ) throws ResourceException {
    	return PersistenceManagers.toPrincipalChain(connection.getMetaData().getUserName());
    }

    /**
     * Allows to provide a custom-specific realm implementation.
     * 
     */
    protected DefaultRealm newRealm(
    	Path realmIdentity
    ) throws ResourceException {
        return new DefaultRealm(realmIdentity);
    }

    /**
     * Get realm for given request.
     * 
     * @param header
     * @param request
     * @return
     * @throws ServiceException
     */
    protected DefaultRealm getRealm(
        RequestRecord request,
        List<String> principalChain,
        PersistenceManager pm
    ) throws ResourceException {
    	Path path = request.getResourceIdentifier();
        String principalName = principalChain.get(0); 
        String realmName = SecurityKeys.ROOT_PRINCIPAL.equals(principalName) ? 
        	"Root" : 
        	path.getSegment(4).toClassicRepresentation();
        if(this.cachedRealms.get(realmName) == null) {
            this.cachedRealms.put(
                realmName,
                this.newRealm(
                    this.realmIdentity.getParent().getChild(realmName)
                )
            );
        }
        DefaultRealm realm = this.cachedRealms.get(realmName);
        CachedPrincipal requestingPrincipal = realm.getPrincipal(principalName, pm);        
        if(requestingPrincipal == null) {
			throw ResourceExceptions.initHolder(
        		new ResourceException(
                    "Requested principal not found.",
    				BasicException.newEmbeddedExceptionStack(
    	                OpenCrxException.DOMAIN,
    	                OpenCrxException.AUTHORIZATION_FAILURE_MISSING_PRINCIPAL, 
    	                new BasicException.Parameter("principal", principalChain),
    	                new BasicException.Parameter("param0", principalChain),
    	                new BasicException.Parameter("param1", this.realmIdentity)
    				)
    			)
            );        	
        }
        SysLog.detail("Requesting principal", requestingPrincipal);
        return realm;
    }

    /**
     * Get type referenced by access path.
     * 
     * @param accessPath
     * @param filter
     * @return
     * @throws ServiceException
     */
    protected ModelElement_1_0 getReferencedType(
        Path accessPath,
        List<FilterProperty> filter
    ) throws ServiceException {
        // Extent access requires special treatment
        // get the set of referenceIds specified by filter property 'identity'
        boolean isExtent = false;        
        if(filter != null && accessPath.isLike(EXTENT_PATTERN)) {
            for(FilterProperty p: filter) {
                if(SystemAttributes.OBJECT_IDENTITY.equals(p.name())) {
                    if(p.values().size() > 1) {
                        throw new ServiceException(
                            BasicException.Code.DEFAULT_DOMAIN,
                            BasicException.Code.NOT_SUPPORTED, 
                            "at most one value allowed for filter property 'identity'",
                            new BasicException.Parameter("filter", filter)
                        );                        
                    }
                    isExtent = true;
                    accessPath = new Path(p.values().iterator().next().toString());
                }
            }
            if(!isExtent) {
                throw new ServiceException(
                    BasicException.Code.DEFAULT_DOMAIN,
                    BasicException.Code.NOT_SUPPORTED, 
                    "extent lookups require at least a filter value for property 'identity'",
                    new BasicException.Parameter("filter", filter)
                );            
            }
        }
        return this.model.getTypes(accessPath)[2];        
    }

    /**
     * Get object cache.
     * 
     * @return
     */
    protected static ConcurrentMap<Path,Object[]> getObjectCache(
    ) {
    	return objectCache;
    }
    
    /**
	 * @return the realmIdentity
	 */
	public Path getRealmIdentity() {
		return realmIdentity;
	}

	/**
	 * @param realmIdentity the realmIdentity to set
	 */
	public void setRealmIdentity(Path realmIdentity) {
		this.realmIdentity = realmIdentity;
	}

    /**
	 * @return the useExtendedAccessLevelBasic
	 */
	public boolean isUseExtendedAccessLevelBasic() {
		return useExtendedAccessLevelBasic;
	}

	/**
	 * @param useExtendedAccessLevelBasic the useExtendedAccessLevelBasic to set
	 */
	public void setUseExtendedAccessLevelBasic(boolean useExtendedAccessLevelBasic) {
		this.useExtendedAccessLevelBasic = useExtendedAccessLevelBasic;
	}
    
	/**
	 * @return the connectionFactory
	 * @deprecated
	 */
	public ConnectionFactory getConnectionFactory() {
		return connectionFactory;
	}

	/**
	 * @param connectionFactory the connectionFactory to set
	 * @deprecated
	 */
	public void setConnectionFactory(
		ConnectionFactory connectionFactory
	) {
		this.connectionFactory = connectionFactory;
	}
	
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected final static Path EXTENT_PATTERN = 
        new Path("xri:@openmdx:**/provider/**/segment/**/extent");
    
    protected static final Path USER_HOME_PATH_PATTERN =
        new Path("xri://@openmdx*org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*");

    protected static final String ALL_PERMISSION = "*";
   
    protected Path realmIdentity = null;
	protected Model_1_0 model = Model_1Factory.getModel();
    protected boolean useExtendedAccessLevelBasic = false;
    protected ConnectionFactory connectionFactory = null;

    // Cached realms
    private Map<String,DefaultRealm> cachedRealms = 
    	new ConcurrentHashMap<String,DefaultRealm>();
    
    // TTL of cached objects
    private static final long TTL_CACHED_OBJECTS = 20000L;
    
    // Entry contains (path,[object,ttl])
    protected static final ConcurrentMap<Path,Object[]> objectCache = 
        new ConcurrentHashMap<Path,Object[]>();

    // Mapping of shared association paths to composite parent path
    protected static final ConcurrentMap<Path,Path> sharedAssociationToCompositeParentPathMap =
    	new ConcurrentHashMap<Path,Path>();
}
