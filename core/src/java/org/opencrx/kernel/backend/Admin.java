/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Admin
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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.generic.SecurityKeys;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Quantifier;
import org.openmdx.kernel.log.SysLog;

/**
 * Admin backend class.
 *
 */
public class Admin extends AbstractImpl {

	/**
	 * Register backend.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new Admin());
	}
	
	/**
	 * Get admin backend instance.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Admin getInstance(		
	) throws ServiceException {
		return getInstance(Admin.class);
	}

	public static final String SALES_TAX_TYPE_NAME_8_5 = "Sales Tax 8.5%";
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String ACCOUNT_FILTER_NAME_ALL = "All Accounts";
	public static final String ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD = "Accounts with missing or broken vCard";
	public static final String ADDRESS_FILTER_NAME_ALL = "All Addresses";
	public static final String CONTRACT_FILTER_NAME_LEAD_FORECAST = "Lead Forecast";
	public static final String CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST = "Opportunity Forecast";
	public static final String CONTRACT_FILTER_NAME_QUOTE_FORECAST = "Quote Forecast";
	public static final String CONTRACT_FILTER_NAME_WON_LEADS = "Won Leads";
	public static final String CONTRACT_FILTER_NAME_WON_OPPORTUNITIES = "Won Opportunities";
	public static final String CONTRACT_FILTER_NAME_WON_QUOTES = "Won Quotes";
	public static final String ACTIVITY_FILTER_NAME_PHONE_CALLS = "Phone Calls";
	public static final String ACTIVITY_FILTER_NAME_NEW_ACTIVITIES = "New Activities";
	public static final String ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES = "Open Activities";
	public static final String ACTIVITY_FILTER_NAME_MEETINGS = "Meetings";

	/**
	 * Constructor.
	 * 
	 */
	protected Admin(
	) {
		
	}
	
	/**
	 * PrincipalType
	 *
	 */
	public enum PrincipalType {
		PRINCIPAL,
		USER,
		GROUP
	}
	
    /**
     * Create segment.
     * 
     * @param provider
     * @param segment
     * @param segmentName
     * @param owningUser
     * @param owningGroup
     * @param errors
     */
    public void createSegment(
    	org.openmdx.base.jmi1.Provider provider,
        org.openmdx.base.jmi1.Segment segment,
        String segmentName,
        org.opencrx.security.realm1.jmi1.User owningUser,
        org.opencrx.security.realm1.jmi1.PrincipalGroup owningGroup,
        List<String> errors
    ) {
    	org.openmdx.base.jmi1.Segment test = null;
	    try {
	        test = provider.getSegment(segmentName);
	    } catch(Exception e) {}
	    if(test == null) {
	        if(segment instanceof org.opencrx.kernel.base.jmi1.SecureObject) {
		        ((org.opencrx.kernel.base.jmi1.SecureObject)segment).setOwningUser(owningUser);
		        ((org.opencrx.kernel.base.jmi1.SecureObject)segment).getOwningGroup().add(owningGroup);
		        ((org.opencrx.kernel.base.jmi1.SecureObject)segment).setAccessLevelBrowse(new Short(SecurityKeys.ACCESS_LEVEL_GLOBAL));
		        ((org.opencrx.kernel.base.jmi1.SecureObject)segment).setAccessLevelUpdate(new Short(SecurityKeys.ACCESS_LEVEL_DEEP));
		        ((org.opencrx.kernel.base.jmi1.SecureObject)segment).setAccessLevelDelete(new Short(SecurityKeys.ACCESS_LEVEL_PRIVATE));                
	        }
	        try {
	        	provider.addSegment(
	        		segmentName,
	        		segment
	        	);
	        } catch(Exception e0) {
	            ServiceException e1 = new ServiceException(e0);
	            SysLog.warning(e1.getMessage(), e1.getCause());
	            errors.add("Unable to create segment " + segment);
	            errors.add("reason is " + e0.getMessage());
	        }
	    }
    }
    
    /**
     * Create contact.
     * 
     * @param adminSegment
     * @param segmentName
     * @param principalName
     * @param owningUser
     * @param owningGroups
     * @param errors
     * @return
     */
    public Contact createContact(
        org.opencrx.kernel.admin1.jmi1.Segment adminSegment,
        String segmentName,
        String principalName,
        org.opencrx.security.realm1.jmi1.User owningUser,
        List<org.opencrx.security.realm1.jmi1.PrincipalGroup> owningGroups,
        List<String> errors
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(adminSegment);
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = 
    		(org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
    			new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", adminSegment.refGetPath().get(2), "segment", segmentName)
    		);
    	Contact contact = null;
        try {
        	contact = (Contact)accountSegment.getAccount(principalName);
        } catch(Exception e) {}
        if(contact == null) {
        	contact = pm.newInstance(Contact.class);
            contact.setLastName(principalName);
            contact.setOwningUser(owningUser);
            contact.getOwningGroup().addAll(owningGroups);
            accountSegment.addAccount(
            	principalName,
            	contact
            );
        }
        return contact;
    }

    /**
     * Create subject.
     * 
     * @param identitySegment
     * @param subjectName
     * @param subjectDescription
     * @param errors
     * @return
     */
    public org.opencrx.security.identity1.jmi1.Subject createSubject(
    	org.opencrx.security.identity1.jmi1.Segment identitySegment,
        String subjectName,
        String subjectDescription,
        List<String> errors
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(identitySegment);
    	org.opencrx.security.identity1.jmi1.Subject subject = identitySegment.getSubject(subjectName);
    	if(subject == null) {
        	subject = pm.newInstance(org.opencrx.security.identity1.jmi1.Subject.class);
        	subject.setDescription(
        		subjectDescription == null ? subjectName : subjectDescription
        	);
        	identitySegment.addSubject(
        		false,
        		subjectName,
        		subject
        	);
        }
        return subject;
    }
    
    /**
     * Create or update principal. In case the principal already exists only the
     * group memberships are updated.
     * 
     * @param id
     * @param description
     * @param realm
     * @param principalType
     * @param memberOfGroups
     * @param subject
     * @return
     */
    public org.openmdx.security.realm1.jmi1.Principal createPrincipal(
        String id,
        String description,
        org.openmdx.security.realm1.jmi1.Realm realm,
        PrincipalType principalType,
        List<org.openmdx.security.realm1.jmi1.Group> memberOfGroups,
        org.openmdx.security.realm1.jmi1.Subject subject
    ) {
    	return this.createPrincipal(
    		id,
    		null, // name
    		description,
    		realm,
    		principalType,
    		memberOfGroups,
    		subject
    	);
    }

    /**
     * Create or update principal. In case the principal already exists only the
     * group memberships are updated.
     * 
     * @param id
     * @param name
     * @param description
     * @param realm
     * @param principalType
     * @param memberOfGroups
     * @param subject
     * @return
     */
    public org.openmdx.security.realm1.jmi1.Principal createPrincipal(
        String id,
        String name,
        String description,
        org.openmdx.security.realm1.jmi1.Realm realm,
        PrincipalType principalType,
        List<org.openmdx.security.realm1.jmi1.Group> memberOfGroups,
        org.openmdx.security.realm1.jmi1.Subject subject
    ) {
        PersistenceManager pm = JDOHelper.getPersistenceManager(realm);
        org.openmdx.security.realm1.jmi1.Principal principal = realm.getPrincipal(id);
        if(principal != null) {
            Set<org.openmdx.security.realm1.jmi1.Group> mergedGroups = new LinkedHashSet<org.openmdx.security.realm1.jmi1.Group>(
            	principal.<org.openmdx.security.realm1.jmi1.Group>getIsMemberOf()
            );
            mergedGroups.addAll(memberOfGroups);
            principal.getIsMemberOf().clear();
            principal.getIsMemberOf().addAll(mergedGroups);
        } else {
        	switch(principalType) {
	        	case USER: 
	        		principal = pm.newInstance(org.opencrx.security.realm1.jmi1.User.class);
	        		break;
	        	case PRINCIPAL:
	        		principal = pm.newInstance(org.opencrx.security.realm1.jmi1.Principal.class);
	        		break;
	        	case GROUP:
	        		principal = pm.newInstance(org.opencrx.security.realm1.jmi1.PrincipalGroup.class);
	        		break;
        	}
        	principal.setName(
            	name == null ?
            		id :
            	    name
        	);
            principal.setDescription(
            	description == null ?
            		realm.refGetPath().getLastSegment().toClassicRepresentation() + "\\\\" + id :
            	    description
            );
            principal.setDisabled(Boolean.FALSE);
            principal.getIsMemberOf().addAll(
            	new LinkedHashSet<org.openmdx.security.realm1.jmi1.Group>(memberOfGroups)
            );
            principal.setSubject(subject);
            realm.addPrincipal(
            	id,
            	principal
            );
        }
        return principal;
    }
    
    /**
     * Create policy.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.openmdx.security.authorization1.jmi1.Policy createPolicy(
    	PersistenceManager pm,
    	String providerName,
    	String segmentName
    ) {
        // Create policy for segment
        Path policyIdentity = SecureObject.getPolicyIdentity(providerName, segmentName);
        org.openmdx.security.authorization1.jmi1.Policy policy = null;
        try {
        	policy = ( org.openmdx.security.authorization1.jmi1.Policy)pm.getObjectById(policyIdentity);
        } catch(Exception e) {}
        if(policy == null) {
            org.openmdx.security.authorization1.jmi1.Segment policySegment = 
            	(org.openmdx.security.authorization1.jmi1.Segment)pm.getObjectById(
            		policyIdentity.getParent().getParent()
            	);
            policy = pm.newInstance(org.openmdx.security.authorization1.jmi1.Policy.class);
            policy.setDescription(segmentName + " Policy");
            policySegment.addPolicy(
            	false,
            	segmentName,
            	policy
            );
        }    
        return policy;
    }
    
    /**
     * Creates a new segment and segment administrators. <segmentName>:ADMIN_PRINCIPAL 
     * is the owner of all created objects.
     *  
     * @param adminSegment
     * @param segmentName
     * @param principalId
     * @param initialPassword
     * @param initialPasswordVerification
     * @param errors
     * @throws ServiceException
     */
    public void createAdministrator(
        org.opencrx.kernel.admin1.jmi1.Segment adminSegment,
        String segmentName,
        String principalId,
        String initialPassword,
        String initialPasswordVerification,
        List<String> errors
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(adminSegment);
        String providerName = adminSegment.refGetPath().get(2);
        Path realmIdentity = SecureObject.getRealmIdentity(providerName, segmentName);
        Path contactIdentity = null;
        Path groupAdministratorsIdentity = null;
        // Principal must exist in login realm
        {
        	PersistenceManager pmRoot = pm.getPersistenceManagerFactory().getPersistenceManager(
        		SecurityKeys.ROOT_PRINCIPAL,
        		null
        	);
        	pmRoot.currentTransaction().begin();
	        String adminPrincipalName = SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName;
	        Path loginRealmIdentity = SecureObject.getInstance().getLoginRealmIdentity(providerName);
	        org.openmdx.security.realm1.jmi1.Realm loginRealm = (org.openmdx.security.realm1.jmi1.Realm)pmRoot.getObjectById(
	        	loginRealmIdentity
	        );
	        org.openmdx.security.realm1.jmi1.Subject segmentAdminSubject = null;
	    	if(loginRealm.getPrincipal(principalId) != null) {
	            try {
	            	segmentAdminSubject = loginRealm.getPrincipal(principalId).getSubject();
	            } catch(Exception ignore) {}
	    	}
	        if(segmentAdminSubject == null) {
	            // Create segment administrator if it does not exist
	        	org.opencrx.security.identity1.jmi1.Segment identitySegment = 
	        		(org.opencrx.security.identity1.jmi1.Segment)pmRoot.getObjectById(
	        			new Path("xri://@openmdx*org.opencrx.security.identity1").getDescendant("provider", loginRealmIdentity.get(2), "segment", "Root")
	        		);
	            // Create segment administrator's subject
	        	segmentAdminSubject = this.createSubject(
	            	identitySegment,
	                adminPrincipalName,
	                null,
	                errors
	            );
	            if(!errors.isEmpty()) return;            
	            // Create segment administrator's principal
	            List<org.openmdx.security.realm1.jmi1.Group> groups = new ArrayList<org.openmdx.security.realm1.jmi1.Group>();
	            groups.add(
	            	(org.openmdx.security.realm1.jmi1.Group)loginRealm.getPrincipal(SecurityKeys.PRINCIPAL_GROUP_ADMINISTRATORS)
	            );
	            this.createPrincipal(
	                principalId,
	                null, // name
	                null, // description
	                loginRealm,
	                PrincipalType.PRINCIPAL,
	                groups,
	                segmentAdminSubject
	            );
	        }
	        // Principal admin-<segment name> must exist before any other
	        // segment administrator can be created
	        if(loginRealm.getPrincipal(adminPrincipalName) == null) {
	            // admin-<segment name> does not exist --> principalName must match segment administrator
	            if(!principalId.equals(adminPrincipalName)) {
	                errors.add("primary principal name must match " + adminPrincipalName);
	                return;
	            }
	        }
	        if(
	            (principalId.startsWith(SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR)) &&
	            !principalId.equals(adminPrincipalName)
	        ) {
	            errors.add("admin principal for segment " + segmentName + " must match " + adminPrincipalName);
	            return;            
	        }
	        // Create policy for segment
	        this.createPolicy(
	        	pmRoot, 
	        	providerName, 
	        	segmentName
	        );
	        // Create realm for segment
	        org.openmdx.security.realm1.jmi1.Realm realm = null;
	        try {
	        	realm = (org.openmdx.security.realm1.jmi1.Realm)pmRoot.getObjectById(realmIdentity);
	        } catch(Exception ignore) {}
	        if(realm == null) {
	            org.openmdx.security.realm1.jmi1.Segment realmSegment = 
	            	(org.openmdx.security.realm1.jmi1.Segment)pmRoot.getObjectById(
	            		realmIdentity.getParent().getParent()
	            	);
	            realm = pmRoot.newInstance(org.openmdx.security.realm1.jmi1.Realm.class);
	            realm.setDescription(segmentName + " Realm");
	            realmSegment.addRealm(
	            	segmentName,
	            	realm
	            );
	        }
	        org.opencrx.security.realm1.jmi1.User adminUser = (org.opencrx.security.realm1.jmi1.User)this.createPrincipal(
	        	principalId + "." + SecurityKeys.USER_SUFFIX,
	        	null, // name
	        	null, // description
	        	realm, 
	        	PrincipalType.USER, 
	        	new ArrayList<org.openmdx.security.realm1.jmi1.Group>(), 
	        	segmentAdminSubject
	        );
	        List<org.openmdx.security.realm1.jmi1.Group> groups = new ArrayList<org.openmdx.security.realm1.jmi1.Group>();
	        groups.add(adminUser);
	        this.createPrincipal(
	        	principalId,
	        	null, // name
	        	null, // description
	        	realm, 
	        	PrincipalType.PRINCIPAL, 
	        	groups, 
	        	segmentAdminSubject
	        );
	        org.opencrx.security.realm1.jmi1.PrincipalGroup groupUnspecified = (org.opencrx.security.realm1.jmi1.PrincipalGroup)this.createPrincipal(
	            SecurityKeys.USER_GROUP_UNSPECIFIED,
	            null, // name
	            null, // description
	            realm,
	            PrincipalType.GROUP,
	            new ArrayList<org.openmdx.security.realm1.jmi1.Group>(),
	            segmentAdminSubject
	        );
	        groups = new ArrayList<org.openmdx.security.realm1.jmi1.Group>();
	        groups.add(groupUnspecified);
	        org.opencrx.security.realm1.jmi1.PrincipalGroup groupAdministrators = (org.opencrx.security.realm1.jmi1.PrincipalGroup)this.createPrincipal(
	            SecurityKeys.USER_GROUP_ADMINISTRATORS,
	            null, // name
	            null, // description
	            realm,
	            PrincipalType.GROUP,
	            groups,
	            segmentAdminSubject
	        );
	        groupAdministratorsIdentity = groupAdministrators.refGetPath();
	        org.opencrx.security.realm1.jmi1.PrincipalGroup groupUsers = (org.opencrx.security.realm1.jmi1.PrincipalGroup)this.createPrincipal(
	            SecurityKeys.USER_GROUP_USERS,
	            null, // name
	            null, // description
	            realm,
	            PrincipalType.GROUP,
	            groups,
	            segmentAdminSubject
	        );
	        groups = new ArrayList<org.openmdx.security.realm1.jmi1.Group>();
	        groups.add(groupUsers);
	        this.createPrincipal(
	            SecurityKeys.USER_GROUP_UNASSIGNED,
	            null, // name
	            null, // description
	            realm,
	            PrincipalType.GROUP,
	            groups,
	            segmentAdminSubject
	        );
	        this.createPrincipal(
	            SecurityKeys.USER_GROUP_PUBLIC,
	            null, // name
	            null, // description
	            realm,
	            PrincipalType.GROUP,
	            groups,
	            segmentAdminSubject
	        );
	        // Create segments        
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.account1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.home1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.activity1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.contract1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.product1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.product1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.document1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.document1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.building1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.building1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.uom1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.uom1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.forecast1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.forecast1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.workflow1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.depot1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.depot1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.model1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.model1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        this.createSegment(
	        	(org.openmdx.base.jmi1.Provider)pmRoot.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.code1").getDescendant("provider", providerName)), 
	        	pmRoot.newInstance(org.opencrx.kernel.code1.jmi1.Segment.class), 
	        	segmentName,
	        	adminUser, 
	        	groupAdministrators, 
	        	errors
	        );
	        if(!errors.isEmpty()) return;        
	        // Create contact: adminPrincipalName
	        Contact contact = this.createContact(
	            (org.opencrx.kernel.admin1.jmi1.Segment)pmRoot.getObjectById(adminSegment.refGetPath()), 
	            segmentName, 
	            principalId, 
	            adminUser,
	            Arrays.asList(groupUsers, groupAdministrators),
	            errors
	        );
	        if(contact == null) return;
	        contactIdentity = contact.refGetPath();
	        pmRoot.currentTransaction().commit();
        }
        // Create user home for segment admin
        {
	        UserHomes.getInstance().createUserHome(
	            (org.openmdx.security.realm1.jmi1.Realm)pm.getObjectById(realmIdentity),
	            (Contact)pm.getObjectById(contactIdentity),
	            (org.opencrx.security.realm1.jmi1.PrincipalGroup)pm.getObjectById(groupAdministratorsIdentity),
	            principalId,
	            new ArrayList<org.openmdx.security.realm1.jmi1.Group>(),            
	            true,
	            initialPassword,
	            initialPasswordVerification,
	            null, // eMailAddress
	            null, // timezone
	            errors
	        );
        }
    }
    
    /**
     * Import login principals.
     * 
     * @param adminSegment
     * @param item
     * @return
     * @throws ServiceException
     */
    public String importLoginPrincipals(
        org.opencrx.kernel.admin1.jmi1.Segment adminSegment,
        byte[] item
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(adminSegment);
        BufferedReader reader = null;
        try {
	        reader = new BufferedReader(
	            new InputStreamReader(new ByteArrayInputStream(item), "UTF-8")
	        );
        } catch (UnsupportedEncodingException e) {}
        org.openmdx.security.realm1.jmi1.Realm loginRealm = (org.openmdx.security.realm1.jmi1.Realm)pm.getObjectById(
        	SecureObject.getInstance().getLoginRealmIdentity(adminSegment.refGetPath().get(2))
        );
        org.opencrx.security.identity1.jmi1.Segment identitySegment = (org.opencrx.security.identity1.jmi1.Segment)pm.getObjectById( 
            new Path("xri://@openmdx*org.opencrx.security.identity1").getDescendant("provider", adminSegment.refGetPath().get(2), "segment", "Root")
        );
        int nCreatedPrincipals = 0;
        int nExistingPrincipals = 0;
        int nFailedPrincipals = 0;
        int nCreatedSubjects = 0;
        int nExistingSubjects = 0;
        int nFailedSubjects = 0;
        try {
            while(reader.ready()) {
                String l = reader.readLine();
                if(l.indexOf("Principal;") >= 0) {
                    StringTokenizer t = new StringTokenizer(l, ";");
                    t.nextToken();
                    String principalId = t.nextToken();
                    String principalDescription = t.nextToken();
                    String subjectName = t.nextToken();
                    String groups = t.nextToken();                       
                    org.openmdx.security.realm1.jmi1.Principal principal = null;
                    try {
                    	principal = loginRealm.getPrincipal(principalId);
                    } catch(Exception e) {}
                    if(principal == null) {
                        try {
                        	principal = this.createPrincipal(
                        		principalId,
                        		principalId, // name
                        		principalDescription, // description
                        		loginRealm, 
                        		PrincipalType.PRINCIPAL, 
                        		new ArrayList<org.openmdx.security.realm1.jmi1.Group>(),
                        		identitySegment.getSubject(subjectName)
                        	);
                            nCreatedPrincipals++;
                        } catch(Exception e) {
                            new ServiceException(e).log();
                            nFailedPrincipals++;
                        }
                    } else {
                        nExistingPrincipals++;
                    }
                    if(principal != null) {
                        StringTokenizer g = new StringTokenizer(groups, ",");
                        while(g.hasMoreTokens()) {
                            String groupPrincipalName = g.nextToken();
                            org.openmdx.security.realm1.jmi1.Group groupPrincipal = 
                            	(org.openmdx.security.realm1.jmi1.Group)loginRealm.getPrincipal(groupPrincipalName);
                            if(!principal.getIsMemberOf().contains(groupPrincipal)) {
                                principal.getIsMemberOf().add(groupPrincipal);
                            }
                        }
                    }
                } else if(l.indexOf("Subject") >= 0) {
                    StringTokenizer t = new StringTokenizer(l, ";");
                    t.nextToken();
                    String subjectName = t.nextToken();
                    String subjectDescription = t.nextToken();
                    org.opencrx.security.identity1.jmi1.Subject subject = null;
                    try {
                    	subject = identitySegment.getSubject(subjectName);
                    } catch(Exception e) {}
                    if(subject == null) {
                        try {
                        	subject = this.createSubject(
                        		identitySegment,
                        		subjectName,
                        		subjectDescription,
                        		new ArrayList<String>()
                        	);
                            nCreatedSubjects++;
                        } catch(Exception e) {
                            new ServiceException(e).log();
                            nFailedSubjects++;
                        }
                    } else {
                        nExistingSubjects++;
                    }
                }
            }
        } catch(IOException e) {
            new ServiceException(e).log();
        }
        return 
            "Principals=(created:" + nCreatedPrincipals + ",existing:" + nExistingPrincipals + ",failed:" + nFailedPrincipals + "); " +
            "Subjects:(created:" + nCreatedSubjects + ",existing:" + nExistingSubjects + ",failed:" + nFailedSubjects + ")";
    }

    /**
     * Init all segments for given segment administrator.
     * 
     * @param segmentAdminHome
     */
    public void initSegments(
    	org.opencrx.kernel.home1.jmi1.UserHome segmentAdminHome
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segmentAdminHome);
    	String providerName = segmentAdminHome.refGetPath().getSegment(2).toClassicRepresentation();
    	String segmentName = segmentAdminHome.refGetPath().getSegment(4).toClassicRepresentation();
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, providerName, segmentName);
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, providerName, segmentName);
    	org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(pm, providerName, segmentName);
    	org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(pm, providerName, segmentName);
		org.opencrx.security.realm1.jmi1.PrincipalGroup publicPrincipalGroup =
			SecureObject.getInstance().initPrincipalGroup(
				"Public",
				pm,
				providerName,
				segmentName
			);
		org.opencrx.security.realm1.jmi1.PrincipalGroup usersPrincipalGroup =
			SecureObject.getInstance().initPrincipalGroup(
				"Users",
				pm,
				providerName,
				segmentName
			);
		org.opencrx.security.realm1.jmi1.PrincipalGroup administratorsPrincipalGroup =
			SecureObject.getInstance().initPrincipalGroup(
				"Administrators",
				pm,
				providerName,
				segmentName
			);
		List<org.opencrx.security.realm1.jmi1.PrincipalGroup> allUsers = new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
		allUsers.add(usersPrincipalGroup);
		allUsers.add(administratorsPrincipalGroup);
		Workflows.getInstance().initWorkflows(
			pm,
			providerName,
			segmentName
		);
		org.opencrx.kernel.activity1.jmi1.ActivityProcess bulkEmailProcess =
			Activities.getInstance().initBulkEmailProcess(
				pm,
				providerName,
				segmentName,
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		org.opencrx.kernel.activity1.jmi1.ActivityProcess emailProcess =
			Activities.getInstance().initEmailProcess(
				pm,
				providerName,
				segmentName,
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		org.opencrx.kernel.activity1.jmi1.ActivityProcessState emailProcessStateNew = null;
		org.opencrx.kernel.activity1.jmi1.ActivityProcessState emailProcessStateOpen = null;
		for(Iterator<ActivityProcessState> i = emailProcess.<ActivityProcessState>getState().iterator(); i.hasNext(); ) {
			org.opencrx.kernel.activity1.jmi1.ActivityProcessState state = i.next();
			if("New".equals(state.getName())) {
				emailProcessStateNew = state;
			}
			if("Open".equals(state.getName())) {
				emailProcessStateOpen = state;
			}
		}
		org.opencrx.kernel.activity1.jmi1.ActivityProcess bugAndFeatureTrackingProcess =
			Activities.getInstance().initBugAndFeatureTrackingProcess(
				pm,
				providerName,
				segmentName,
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		org.opencrx.kernel.activity1.jmi1.ActivityProcessState bugAndFeatureTrackingProcessStateNew = null;
		org.opencrx.kernel.activity1.jmi1.ActivityProcessState bugAndFeatureTrackingProcessStateInProgress  = null;
		for(Iterator<ActivityProcessState> i = bugAndFeatureTrackingProcess.<ActivityProcessState>getState().iterator(); i.hasNext(); ) {
			org.opencrx.kernel.activity1.jmi1.ActivityProcessState state = i.next();
			if("New".equals(state.getName())) {
				bugAndFeatureTrackingProcessStateNew = state;
			}
			if("In Progress".equals(state.getName())) {
				bugAndFeatureTrackingProcessStateInProgress = state;
			}
		}
		Activities.getInstance().initCalendar(
			Activities.CALENDAR_NAME_DEFAULT_BUSINESS,
			pm,
			providerName,
			segmentName,
			allUsers,
			SecurityKeys.ACCESS_LEVEL_PRIVATE
		);
		// Activity Types
		org.opencrx.kernel.activity1.jmi1.ActivityType bugsAndFeaturesType =
			Activities.getInstance().initActivityType(
				bugAndFeatureTrackingProcess,
				Activities.ACTIVITY_TYPE_NAME_BUGS_AND_FEATURES,
				Activities.ActivityClass.INCIDENT.getValue(),
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		@SuppressWarnings("unused")
        org.opencrx.kernel.activity1.jmi1.ActivityType bulkEmailsType =
			Activities.getInstance().initActivityType(
				bulkEmailProcess,
				Activities.ACTIVITY_TYPE_NAME_BULK_EMAILS,
				Activities.ActivityClass.EMAIL.getValue(),
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		org.opencrx.kernel.activity1.jmi1.ActivityType emailsType =
			Activities.getInstance().initActivityType(
				emailProcess,
				Activities.ACTIVITY_TYPE_NAME_EMAILS,
				Activities.ActivityClass.EMAIL.getValue(),
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		org.opencrx.kernel.activity1.jmi1.ActivityType tasksType =
			Activities.getInstance().initActivityType(
				bugAndFeatureTrackingProcess,
				Activities.ACTIVITY_TYPE_NAME_TASKS,
				Activities.ActivityClass.TASK.getValue(),
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		org.opencrx.kernel.activity1.jmi1.ActivityType meetingsType =
			Activities.getInstance().initActivityType(
				bugAndFeatureTrackingProcess,
				Activities.ACTIVITY_TYPE_NAME_MEETINGS,
				Activities.ActivityClass.MEETING.getValue(),
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		org.opencrx.kernel.activity1.jmi1.ActivityType phoneCallsType =
			Activities.getInstance().initActivityType(
				bugAndFeatureTrackingProcess,
				Activities.ACTIVITY_TYPE_NAME_PHONE_CALLS,
				Activities.ActivityClass.PHONE_CALL.getValue(),
				allUsers,
				SecurityKeys.ACCESS_LEVEL_PRIVATE
			);
		// Activity Trackers
		org.opencrx.kernel.activity1.jmi1.ActivityTracker bugsAndFeaturesTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_BUGS_AND_FEATURES,
				allUsers,
				activitySegment
			);
		org.opencrx.kernel.activity1.jmi1.ActivityTracker emailsTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_EMAILS,
				allUsers,
				activitySegment
			);
		org.opencrx.kernel.activity1.jmi1.ActivityTracker tasksTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_TASKS,
				allUsers,
				activitySegment
			);
		org.opencrx.kernel.activity1.jmi1.ActivityTracker pollsTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_POLLS,
				allUsers,
				activitySegment
			);
		org.opencrx.kernel.activity1.jmi1.ActivityTracker meetingRoomsTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_MEETING_ROOMS,
				allUsers,
				activitySegment
			);
		org.opencrx.kernel.activity1.jmi1.ActivityTracker meetingsTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_MEETINGS,
				allUsers,
				activitySegment
			);
		org.opencrx.kernel.activity1.jmi1.ActivityTracker phoneCallsTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_PHONE_CALLS,
				allUsers,
				activitySegment
			);
		org.opencrx.kernel.activity1.jmi1.ActivityTracker publicTracker =
			Activities.getInstance().initActivityTracker(
				Activities.ACTIVITY_TRACKER_NAME_PUBLIC,
				Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup}),
				activitySegment
			);
		// Activity Creators
		try {
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_BUGS_AND_FEATURES,					
				bugsAndFeaturesType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{bugsAndFeaturesTracker}),
				allUsers
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_EMAILS,
				emailsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{emailsTracker}),
				allUsers
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_TASKS,
				tasksType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{tasksTracker}),
				allUsers
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_POLLS,
				emailsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{pollsTracker}),
				allUsers
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_MEETING_ROOMS,
				emailsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{meetingRoomsTracker}),
				allUsers
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_MEETINGS,
				meetingsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{meetingsTracker}),
				allUsers
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_PHONE_CALLS,
				phoneCallsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{phoneCallsTracker}),
				allUsers
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_PUBLIC_EMAILS,
				emailsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
				Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_PUBLIC_TASKS,
				tasksType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
				Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_PUBLIC_MEETINGS,
				meetingsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
				Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
			);
			Activities.getInstance().initActivityCreator(
				Activities.ACTIVITY_CREATOR_NAME_PUBLIC_PHONE_CALLS,
				phoneCallsType,
				Arrays.asList(new org.opencrx.kernel.activity1.jmi1.ActivityGroup[]{publicTracker}),
				Arrays.asList(new org.opencrx.security.realm1.jmi1.PrincipalGroup[]{publicPrincipalGroup})
			);
		} catch (Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch (Exception re) {}
		}
		// PricingRule
		Products.getInstance().initPricingRule(
			Products.PRICING_RULE_NAME_LOWEST_PRICE,
			Products.PRICING_RULE_DESCRIPTION_LOWEST_PRICE,
			Products.PRICING_RULE_GET_PRICE_LEVEL_SCRIPT_LOWEST_PRICE,
			pm,
			providerName,
			segmentName
		);
		// SalesTaxType
		Products.getInstance().initSalesTaxType(
		    SALES_TAX_TYPE_NAME_8_5,
		    new java.math.BigDecimal(8.5),
		    productSegment
		);
		// CalculationRule
		Contracts.getInstance().initCalculationRule(
			Contracts.CALCULATION_RULE_NAME_DEFAULT,
			null,
			Contracts.DEFAULT_GET_POSITION_AMOUNTS_SCRIPT,
			Contracts.DEFAULT_GET_CONTRACT_AMOUNTS_SCRIPT,
			pm,
			providerName,
			segmentName
		);
		// AccountFilter
		// ACCOUNT_FILTER_NAME_ALL
		Accounts.getInstance().initAccountFilter(
			ACCOUNT_FILTER_NAME_ALL,
			new org.opencrx.kernel.account1.jmi1.AccountFilterProperty[]{},
			accountSegment,
			allUsers
		);
		// ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD
		org.opencrx.kernel.account1.jmi1.AccountQueryFilterProperty accountQueryFilterProperty = pm.newInstance(org.opencrx.kernel.account1.jmi1.AccountQueryFilterProperty .class);
		accountQueryFilterProperty.setName("external_link is null or vcard is null");
		accountQueryFilterProperty.setActive(new Boolean (true));
		accountQueryFilterProperty.setClause("object_id IN (\n" +
          "  select oocke1_account.object_id from oocke1_account, oocke1_account_\n" +
          "  where oocke1_account.object_id = oocke1_account_.object_id\n" +
          "  and oocke1_account_.idx=0\n" +
          "  and ((oocke1_account_.external_link is null) or (oocke1_account.vcard is null))\n" +
          "  )"
		);
		@SuppressWarnings("unused")
        org.opencrx.kernel.account1.jmi1.AccountFilterGlobal accountFilterBrokenVcard = Accounts.getInstance().initAccountFilter(
        	ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD,
        	new org.opencrx.kernel.account1.jmi1.AccountFilterProperty[]{
		      accountQueryFilterProperty
        	},
        	accountSegment,
        	allUsers
		);
		// ADDRESS_FILTER_NAME_ALL
		Accounts.getInstance().initAddressFilter(
			ADDRESS_FILTER_NAME_ALL,
			new org.opencrx.kernel.account1.jmi1.AddressFilterProperty[]{},
			accountSegment,
			allUsers
		);
		// ContractFilter
		// CONTRACT_FILTER_NAME_LEAD_FORECAST
		org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty.class);
		contractTypeFilterProperty.setName("Lead");
		contractTypeFilterProperty.setActive(new Boolean (true));
		contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Lead");
		org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty contractQueryFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty .class);
		contractQueryFilterProperty.setName("Estimated close date >= Today");
		contractQueryFilterProperty.setActive(new Boolean (true));
		contractQueryFilterProperty.setClause("(v.estimated_close_date >= now())");
		Contracts.getInstance().initContractFilter(
			CONTRACT_FILTER_NAME_LEAD_FORECAST,
			new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
				contractTypeFilterProperty,
				contractQueryFilterProperty
			},
			contractSegment,
			allUsers
		);
		// CONTRACT_FILTER_NAME_WON_LEADS
		contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
		contractTypeFilterProperty.setName("Lead");
		contractTypeFilterProperty.setActive(new Boolean (true));
		contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Lead");
		org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty contractStateFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty.class);
		contractStateFilterProperty.setName("Won");
		contractStateFilterProperty.setActive(new Boolean (true));
		contractStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractStateFilterProperty.getContractState().add(new Short((short)1110));
		Contracts.getInstance().initContractFilter(
			CONTRACT_FILTER_NAME_WON_LEADS,
			new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
				contractTypeFilterProperty,
				contractStateFilterProperty
			},
			contractSegment,
			allUsers
		);
		// CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST
		contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
		contractTypeFilterProperty.setName("Opportunity");
		contractTypeFilterProperty.setActive(new Boolean (true));
		contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Opportunity");
		contractQueryFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty .class);
		contractQueryFilterProperty.setName("Estimated close date >= Today");
		contractQueryFilterProperty.setActive(new Boolean (true));
		contractQueryFilterProperty.setClause("(v.estimated_close_date >= now())");
		Contracts.getInstance().initContractFilter(
			CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST,
			new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
				contractTypeFilterProperty,
				contractQueryFilterProperty
			},
			contractSegment,
			allUsers
		);
		// CONTRACT_FILTER_NAME_WON_OPPORTUNITIES
		contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
		contractTypeFilterProperty.setName("Opportunity");
		contractTypeFilterProperty.setActive(new Boolean (true));
		contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Opportunity");
		contractStateFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty.class);
		contractStateFilterProperty.setName("Won");
		contractStateFilterProperty.setActive(new Boolean (true));
		contractStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractStateFilterProperty.getContractState().add(new Short((short)1210));
		Contracts.getInstance().initContractFilter(
			CONTRACT_FILTER_NAME_WON_OPPORTUNITIES,
			new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
				contractTypeFilterProperty,
				contractStateFilterProperty
			},
			contractSegment,
			allUsers
		);
		// CONTRACT_FILTER_NAME_QUOTE_FORECAST
		contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
		contractTypeFilterProperty.setName("Quote");
		contractTypeFilterProperty.setActive(new Boolean (true));
		contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Quote");
		contractQueryFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractQueryFilterProperty .class);
		contractQueryFilterProperty.setName("Estimated close date >= Today");
		contractQueryFilterProperty.setActive(new Boolean (true));
		contractQueryFilterProperty.setClause("(v.estimated_close_date >= now())");
		Contracts.getInstance().initContractFilter(
			CONTRACT_FILTER_NAME_QUOTE_FORECAST,
			new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
				contractTypeFilterProperty,
				contractQueryFilterProperty
			},
			contractSegment,
			allUsers
		);
		// CONTRACT_FILTER_NAME_WON_QUOTES
		contractTypeFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractTypeFilterProperty .class);
		contractTypeFilterProperty.setName("Quote");
		contractTypeFilterProperty.setActive(new Boolean (true));
		contractTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractTypeFilterProperty.getContractType().add("org:opencrx:kernel:contract1:Quote");
		contractStateFilterProperty = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractStateFilterProperty.class);
		contractStateFilterProperty.setName("Won");
		contractStateFilterProperty.setActive(new Boolean (true));
		contractStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		contractStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		contractStateFilterProperty.getContractState().add(new Short((short)1310));
		Contracts.getInstance().initContractFilter(
			CONTRACT_FILTER_NAME_WON_QUOTES,
			new org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[]{
				contractTypeFilterProperty,
				contractStateFilterProperty
			},
			contractSegment,
			allUsers
		);
		// ActivityFilter
		// ACTIVITY_FILTER_NAME_PHONE_CALLS
		org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
		activityTypeFilterProperty.setName("Phone Calls");
		activityTypeFilterProperty.setActive(new Boolean (true));
		activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		activityTypeFilterProperty.getActivityType().add(phoneCallsType);
		Activities.getInstance().initActivityFilter(
			ACTIVITY_FILTER_NAME_PHONE_CALLS,
			new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
				activityTypeFilterProperty
			},
			activitySegment,
			allUsers
		);
		// ACTIVITY_FILTER_NAME_MEETINGS
		activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
		activityTypeFilterProperty.setName("Meetings");
		activityTypeFilterProperty.setActive(new Boolean (true));
		activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		activityTypeFilterProperty.getActivityType().add(meetingsType);
		Activities.getInstance().initActivityFilter(
			ACTIVITY_FILTER_NAME_MEETINGS,
			new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
				activityTypeFilterProperty
			},
			activitySegment,
			allUsers
		);
		// ACTIVITY_FILTER_NAME_NEW_ACTIVITIES
		activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
		activityTypeFilterProperty.setName("All Types");
		activityTypeFilterProperty.setActive(new Boolean (true));
		activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		activityTypeFilterProperty.getActivityType().add(bugsAndFeaturesType);
		activityTypeFilterProperty.getActivityType().add(meetingsType);
		activityTypeFilterProperty.getActivityType().add(emailsType);
		activityTypeFilterProperty.getActivityType().add(phoneCallsType);
		org.opencrx.kernel.activity1.jmi1.ActivityProcessStateFilterProperty activityProcessStateFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityProcessStateFilterProperty.class);
		activityProcessStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		activityProcessStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		activityProcessStateFilterProperty.getProcessState().add(bugAndFeatureTrackingProcessStateNew);
		activityProcessStateFilterProperty.getProcessState().add(emailProcessStateNew);
		Activities.getInstance().initActivityFilter(
			ACTIVITY_FILTER_NAME_NEW_ACTIVITIES,
			new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
				activityTypeFilterProperty,
				activityProcessStateFilterProperty
			},
			activitySegment,
			allUsers
		);
		// ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES
		activityTypeFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityTypeFilterProperty.class);
		activityTypeFilterProperty.setName("All Types");
		activityTypeFilterProperty.setActive(new Boolean (true));
		activityTypeFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		activityTypeFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		activityTypeFilterProperty.getActivityType().add(bugsAndFeaturesType);
		activityTypeFilterProperty.getActivityType().add(meetingsType);
		activityTypeFilterProperty.getActivityType().add(emailsType);
		activityTypeFilterProperty.getActivityType().add(phoneCallsType);
		activityProcessStateFilterProperty = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityProcessStateFilterProperty.class);
		activityProcessStateFilterProperty.setName("Open");
		activityProcessStateFilterProperty.setActive(new Boolean (true));
		activityProcessStateFilterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
		activityProcessStateFilterProperty.setFilterOperator(ConditionType.IS_IN.code());
		activityProcessStateFilterProperty.getProcessState().add(bugAndFeatureTrackingProcessStateInProgress);
		activityProcessStateFilterProperty.getProcessState().add(emailProcessStateOpen);
		Activities.getInstance().initActivityFilter(
			ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES,
			new org.opencrx.kernel.activity1.jmi1.ActivityFilterProperty[]{
				activityTypeFilterProperty,
				activityProcessStateFilterProperty
			},
			activitySegment,
			allUsers
		);
    }
    
}
