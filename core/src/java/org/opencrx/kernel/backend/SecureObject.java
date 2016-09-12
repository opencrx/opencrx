/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SecureObject
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
package org.opencrx.kernel.backend;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.oasisopen.jmi1.RefContainer;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.marshalling.Marshaller;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.ModelHelper;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.UserObjects;
import org.openmdx.base.persistence.spi.PersistenceManagers;

/**
 * SecureObject
 *
 */
public class SecureObject extends AbstractImpl {

	/**
	 * Register plugin.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new SecureObject());
	}
	
	/**
	 * Get registered plugin instance.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static SecureObject getInstance(
	) throws ServiceException {
		return getInstance(SecureObject.class);
	}

	/**
	 * Constructor.
	 * 
	 */
	protected SecureObject(
	) {
		
	}
	
	/**
	 * AclMarshaller
	 *
	 */
	interface AclMarshaller extends Marshaller {
		
		AclMarshaller clone(PersistenceManager pm);
		
	}
	
    static class SetOwningUserMarshaller implements AclMarshaller {

    	public SetOwningUserMarshaller(
    		org.opencrx.security.realm1.jmi1.User user    		
    	) {
    		this.user = user;
    	}
    	@Override
    	public AclMarshaller clone(
    		PersistenceManager pm
    	) {
    		return new SetOwningUserMarshaller(
    			(org.opencrx.security.realm1.jmi1.User)pm.getObjectById(this.user.refGetPath())
    		);
    	}
    	@Override
        public Object marshal(Object s) throws ServiceException {
            if(s instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
            	org.opencrx.kernel.base.jmi1.SecureObject obj = (org.opencrx.kernel.base.jmi1.SecureObject)s;
            	if(this.user != null && !this.user.equals(obj.getOwningUser())) {
            		obj.setOwningUser(this.user);
            	}
            }
            return s;
        }
    	@Override
        public Object unmarshal(Object s) {
          throw new UnsupportedOperationException();
        }
        
        private final org.opencrx.security.realm1.jmi1.User user;
        
    }

    /**
     * AddOwningGroupMarshaller
     *
     */
    static class AddOwningGroupMarshaller implements AclMarshaller {

    	public AddOwningGroupMarshaller(
    		org.opencrx.security.realm1.jmi1.PrincipalGroup group
    	) {
    		this.group = group;
    	}
    	@Override    	
    	public AclMarshaller clone(
    		PersistenceManager pm
    	) {
    		return new AddOwningGroupMarshaller(
    			(org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(this.group.refGetPath())
    		);
    	}
    	@Override
        public Object marshal(Object s) throws ServiceException {
            if(s instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
            	org.opencrx.kernel.base.jmi1.SecureObject obj = (org.opencrx.kernel.base.jmi1.SecureObject)s;
            	if(!obj.getOwningGroup().contains(this.group)) {
            		obj.getOwningGroup().add(this.group);
            	}
            }
            return s;
        }
    	@Override        
        public Object unmarshal(Object s) {
          throw new UnsupportedOperationException();
        }
    	
        private final org.opencrx.security.realm1.jmi1.PrincipalGroup group;
    }

    /**
     * ReplaceOwningGroupMarshaller
     *
     */
    static class ReplaceOwningGroupMarshaller implements AclMarshaller {
    
    	public ReplaceOwningGroupMarshaller(
    		List<org.opencrx.security.realm1.jmi1.PrincipalGroup> groups
    	) {
    		this.groups = groups;
    	}
    	@Override    	
    	public AclMarshaller clone(
    		PersistenceManager pm
    	) {
    		List<org.opencrx.security.realm1.jmi1.PrincipalGroup> groups = new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
    		for(org.opencrx.security.realm1.jmi1.PrincipalGroup group: this.groups) {
    			groups.add(
    				(org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(group.refGetPath())
    			);
    		}
    		return new ReplaceOwningGroupMarshaller(groups);
    	}
    	@Override    	
        public Object marshal(Object s) throws ServiceException {
            if(s instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
            	org.opencrx.kernel.base.jmi1.SecureObject obj = (org.opencrx.kernel.base.jmi1.SecureObject)s;
            	if(!this.groups.containsAll(obj.getOwningGroup()) || !obj.getOwningGroup().containsAll(this.groups)) {
	            	obj.getOwningGroup().clear();
	            	obj.getOwningGroup().addAll(this.groups);
            	}
            }
            return s;
        }
    	@Override        
        public Object unmarshal(Object s) {
          throw new UnsupportedOperationException();
        }

        private final List<org.opencrx.security.realm1.jmi1.PrincipalGroup> groups;
    	
    }

    /**
     * RemoveOwningGroupMarshaller
     *
     */
    static class RemoveOwningGroupMarshaller implements AclMarshaller {
    	
    	public RemoveOwningGroupMarshaller(
    		org.opencrx.security.realm1.jmi1.PrincipalGroup group    		
    	) {
    		this.group = group;
    	}
    	@Override    	
    	public AclMarshaller clone(
    		PersistenceManager pm
    	) {
    		return new RemoveOwningGroupMarshaller(
    			(org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(this.group.refGetPath())
    		);
    	}
    	@Override    	
        public Object marshal(Object s) throws ServiceException {
            if(s instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
            	org.opencrx.kernel.base.jmi1.SecureObject obj = (org.opencrx.kernel.base.jmi1.SecureObject)s;
            	if(obj.getOwningGroup().contains(this.group)) {
            		obj.getOwningGroup().remove(this.group);
            	}
            }
            return s;
        }
    	@Override        
        public Object unmarshal(Object s) {
          throw new UnsupportedOperationException();
        }

        private final org.opencrx.security.realm1.jmi1.PrincipalGroup group;
        
    }

    /**
     * SetAccessLevelMarshaller
     *
     */
    static class SetAccessLevelMarshaller implements AclMarshaller {
    	
    	public SetAccessLevelMarshaller(
    		short accessLevelBrowse,
    		short accessLevelUpdate,
    		short accessLevelDelete
    	) {
    		this.accessLevelBrowse = accessLevelBrowse;
    		this.accessLevelUpdate = accessLevelUpdate;
    		this.accessLevelDelete = accessLevelDelete;
    		
    	}
    	@Override
        public Object marshal(Object s) throws ServiceException {
            if(s instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
            	org.opencrx.kernel.base.jmi1.SecureObject obj = (org.opencrx.kernel.base.jmi1.SecureObject)s; 
            	if(obj.getAccessLevelBrowse() != this.accessLevelBrowse) {
            		obj.setAccessLevelBrowse(this.accessLevelBrowse);
            	}
            	if(obj.getAccessLevelUpdate() != this.accessLevelUpdate) {
            		obj.setAccessLevelUpdate(this.accessLevelUpdate);
            	}
            	if(obj.getAccessLevelDelete() != this.accessLevelDelete) {
            		obj.setAccessLevelDelete(this.accessLevelDelete);
            	}
            }
	        return s;
        }
    	@Override
        public AclMarshaller clone(
    		PersistenceManager pm
    	) {
        	return this;
    	}
    	@Override        
        public Object unmarshal(Object s) {
          throw new UnsupportedOperationException();
        }

		private final short accessLevelBrowse;
		private final short accessLevelUpdate;
		private final short accessLevelDelete;
        
    }

    /**
     * Find principal.
     * 
     * @param name
     * @param realm
     * @return
     */
    public org.openmdx.security.realm1.jmi1.Principal findPrincipal(
        String name,
        org.openmdx.security.realm1.jmi1.Realm realm
    ) {
        try {
        	List<String> principalChain = PersistenceManagers.toPrincipalChain(name);
        	PersistenceManager pm = JDOHelper.getPersistenceManager(realm);
            return (org.openmdx.security.realm1.jmi1.Principal)pm.getObjectById(
                realm.refGetPath().getDescendant(new String[]{"principal", principalChain.get(0)})
            );
        } catch(Exception e) {
            return null;
        }
    }

    /**
     * Find principal.
     * 
     * @param name
     * @param realmIdentity
     * @param pm
     * @return
     * @throws ServiceException
     */
    public org.openmdx.security.realm1.jmi1.Principal findPrincipal(
        String name,
        Path realmIdentity,
        PersistenceManager pm
    ) throws ServiceException {
    	org.openmdx.security.realm1.jmi1.Realm realm = (org.openmdx.security.realm1.jmi1.Realm)pm.getObjectById(realmIdentity);
    	return this.findPrincipal(name, realm);
    }

    /**
     * Get realm.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.openmdx.security.realm1.jmi1.Realm getRealm(
        javax.jdo.PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.openmdx.security.realm1.jmi1.Realm)pm.getObjectById(
            new Path("xri://@openmdx*org.openmdx.security.realm1").getDescendant("provider", providerName, "segment", "Root", "realm", segmentName)
        );
    }

    /**
     * Get policy.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.openmdx.security.authorization1.jmi1.Policy getPolicy(
        javax.jdo.PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.openmdx.security.authorization1.jmi1.Policy)pm.getObjectById(
            new Path("xri://@openmdx*org.openmdx.security.authorization1").getDescendant("provider", providerName, "segment", "Root", "policy", segmentName)
        );
    }

    /**
     * Create principal group if required and init.
     * 
     * @param groupName
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public PrincipalGroup initPrincipalGroup(
        String groupName,
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        org.openmdx.security.realm1.jmi1.Realm realm = this.getRealm(
            pm, 
            providerName, 
            segmentName
        );
        PrincipalGroup principalGroup = null;
        if((principalGroup = (PrincipalGroup)this.findPrincipal(groupName, realm)) != null) {
            return principalGroup;            
        }        
        pm.currentTransaction().begin();
        principalGroup = pm.newInstance(PrincipalGroup.class);
        principalGroup.setDescription(segmentName + "\\\\" + groupName);
        realm.addPrincipal(                
            groupName,
            principalGroup
        );                        
        pm.currentTransaction().commit();
        return principalGroup;
    }
                
    /**
     * Apply acls.
     * 
     * @param obj
     * @param marshaller
     * @param mode
     * @param reportText
     * @param report
     * @param level
     */
    public void applyAcls(
    	org.opencrx.kernel.base.jmi1.SecureObject obj,
    	AclMarshaller marshaller,
        Short mode,
        String reportText,
        List<String> report,
        int level
    ) {
    	PersistenceManager pm1 =  null;
        try {
        	PersistenceManager pm = JDOHelper.getPersistenceManager(obj);
        	// Apply acls of objects below segments in separate pm to prevent 
        	// timeouts, OutOfMemory, etc. in case of recursive calls
        	if(
        		JDOHelper.isPersistent(obj) && 
        		(level > 0) && 
        		(obj.refGetPath().size() == 7)
        	) {
        		pm1 = pm.getPersistenceManagerFactory().getPersistenceManager(
        			UserObjects.getPrincipalChain(pm).toString(),
        			null
        		);
        		pm1.currentTransaction().begin();
        		obj = (org.opencrx.kernel.base.jmi1.SecureObject)pm1.getObjectById(obj.refGetPath());
        		marshaller = marshaller.clone(pm1);
        	}
            marshaller.marshal(obj);
            report.add(reportText);           
            if((mode != null) && (mode.intValue() == MODE_RECURSIVE)) {
            	Model_1_0 model = Model_1Factory.getModel();
                @SuppressWarnings({"unchecked"})
                Map<String,ModelElement_1_0> references = model.getElement(
                    obj.refClass().refMofId()
                ).objGetMap("reference");
                for(ModelElement_1_0 featureDef: references.values()) {
                    ModelElement_1_0 referencedEnd = model.getElement(
                        featureDef.getReferencedEnd()
                    );                	
                    if(
                    	ModelHelper.isCompositeEnd(featureDef, false) &&
                    	((Boolean)referencedEnd.isChangeable()).booleanValue()
                    ) {
                        String referenceName = (String)featureDef.getName();
                        RefContainer<?> container = (RefContainer<?>)obj.refGetValue(referenceName);
                        List<?> content = container.refGetAll(null);
                        for(Object contained: content) {
                        	if(contained instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
                        		this.applyAcls(
	                                (org.opencrx.kernel.base.jmi1.SecureObject)contained,
	                                marshaller,
	                                mode,
	                                reportText,
	                                report,
	                                level + 1
	                            );
                        	}
                        }
                    }
                }
            }
            if(pm1 != null) {
            	pm1.currentTransaction().commit();
            }
        } catch(Exception e) {
            report.add(e.getMessage());
        	new ServiceException(e).log();
        	try {
        		if(pm1 != null) {
        			pm1.currentTransaction().rollback();
        		}
        	} catch(Exception e1) {}
        }
        finally {
        	try {
        		if(pm1 != null) {
        			pm1.close();
        		}
        	} catch(Exception e1) {}
        }
    }

    /**
     * Set owning user.
     * 
     * @param obj
     * @param user
     * @param mode
     * @param report
     * @throws ServiceException
     */
    public void setOwningUser(
    	org.opencrx.kernel.base.jmi1.SecureObject obj,
        org.opencrx.security.realm1.jmi1.User user,
        short mode,
        List<String> report
    ) throws ServiceException {   
    	this.applyAcls(
            obj,
            new SetOwningUserMarshaller(
        		user
        	),
            mode,
            "setOwningUser",
            report,
            0 // level
        );
    }

    /**
     * Add owning group.
     * 
     * @param obj
     * @param group
     * @param mode
     * @param report
     * @throws ServiceException
     */
    public void addOwningGroup(
    	org.opencrx.kernel.base.jmi1.SecureObject obj,
    	org.opencrx.security.realm1.jmi1.PrincipalGroup group,
    	short mode,
        List<String> report
    ) throws ServiceException {        
    	this.applyAcls(
            obj,
            new AddOwningGroupMarshaller(
            	group
            ),
            mode,
            "addOwningGroup",
            report,
            0 // level
        );
    }

    /**
     * Replace owning groups.
     * 
     * @param obj
     * @param groups
     * @param mode
     * @param report
     * @throws ServiceException
     */
    public void replaceOwningGroups(
    	org.opencrx.kernel.base.jmi1.SecureObject obj,
    	List<org.opencrx.security.realm1.jmi1.PrincipalGroup> groups,
    	short mode,
        List<String> report
    ) throws ServiceException {        
    	this.applyAcls(
            obj,
            new ReplaceOwningGroupMarshaller(
            	groups
            ),
            mode,
            "replaceOwningGroup",
            report,
            0 // level
        );
    }

    /**
     * Remove owning group.
     * 
     * @param obj
     * @param group
     * @param mode
     * @param report
     * @throws ServiceException
     */
    public void removeOwningGroup(
    	org.opencrx.kernel.base.jmi1.SecureObject obj,
    	org.opencrx.security.realm1.jmi1.PrincipalGroup group,
    	short mode,
        List<String> report
    ) throws ServiceException {        
    	this.applyAcls(
            obj,
            new RemoveOwningGroupMarshaller(
            	group
            ),
            mode,
            "removeOwningGroup",
            report,
            0 // level
        );
    }

    /**
     * Remove all owning groups.
     * 
     * @param obj
     * @param mode
     * @param report
     * @throws ServiceException
     */
    public void removeAllOwningGroup(
    	org.opencrx.kernel.base.jmi1.SecureObject obj,
    	short mode,
        List<String> report
    ) throws ServiceException {        
    	this.applyAcls(
            obj,
            new AclMarshaller() {
            	@Override
                public Object marshal(Object s) throws ServiceException {
                    if(s instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
                        ((org.opencrx.kernel.base.jmi1.SecureObject)s).getOwningGroup().clear();
                    }
                    return s;
                }
                @Override
                public Object unmarshal(Object s) {
                  throw new UnsupportedOperationException();
                }
				@Override
                public AclMarshaller clone(PersistenceManager pm) {
					return this;
				}
            },
            mode,
            "removeAllOwningGroup",
            report,
            0 // level
        );
    }

    /**
     * Set access level.
     * 
     * @param obj
     * @param accessLevelBrowse
     * @param accessLevelUpdate
     * @param accessLevelDelete
     * @param mode
     * @param report
     * @throws ServiceException
     */
    public void setAccessLevel(
    	org.opencrx.kernel.base.jmi1.SecureObject obj,
    	short accessLevelBrowse,
    	short accessLevelUpdate,
    	short accessLevelDelete,
    	short mode,
        List<String> report
    ) throws ServiceException {        
    	this.applyAcls(
            obj,
            new SetAccessLevelMarshaller(
            	accessLevelBrowse,
            	accessLevelUpdate,
            	accessLevelDelete
            ),
            mode,
            "setAccessLevel",
            report,
            0 // level
        );
    }

    /**
     * Get login realm identity.
     * 
     * @param providerName
     * @return
     */
    public Path getLoginRealmIdentity(
    	String providerName
    ) {
    	return SecureObject.getRealmIdentity(providerName, "Default");
    }

    /**
     * Get realm identity.
     * 
     * @param providerName
     * @param segmentName
     * @return
     */
    public static Path getRealmIdentity(
    	String providerName,
    	String segmentName
    ) {
        return new Path("xri://@openmdx*org.openmdx.security.realm1").getDescendant("provider", providerName, "segment", "Root", "realm", segmentName);
    }

    /**
     * Get policy identity.
     * 
     * @param providerName
     * @param segmentName
     * @return
     */
    public static Path getPolicyIdentity(
    	String providerName,
    	String segmentName
    ) {
        return new Path("xri://@openmdx*org.openmdx.security.authorization1").getDescendant("provider", providerName, "segment", "Root", "policy", segmentName);
    }

    /**
     * Update SecureObject callback.
     * 
     * @param secureObject
     * @throws ServiceException
     */
    public void updateSecureObject(
        org.opencrx.kernel.base.jmi1.SecureObject secureObject
    ) throws ServiceException {
    	
    }
    
    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final int MODE_LOCAL = 0;
    public static final int MODE_RECURSIVE = 1;
        
}

//--- End of File -----------------------------------------------------------
