/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestApi
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011, CRIXP Corp., Switzerland
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
package test.org.opencrx.kernel.api;

import java.io.IOException;
import java.text.ParseException;
import java.util.Date;
import java.util.List;

import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.cci2.PostalAddressQuery;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.SecureObject;
import org.opencrx.kernel.base.jmi1.CheckPermissionsParams;
import org.opencrx.kernel.base.jmi1.CheckPermissionsResult;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.jmi1.CreateUserParams;
import org.opencrx.kernel.home1.jmi1.CreateUserResult;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

import test.org.opencrx.generic.AbstractTest;

@RunWith(Suite.class)
@SuiteClasses(
    {
        TestApi.TestAll.class
    }
)

/**
 * TestApi
 */
public class TestApi {

    @BeforeClass
    public static void initialize(
    ) throws NamingException, ServiceException {
    	entityManagerFactory = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactoryProxy(
    		"http://127.0.0.1:8080/opencrx-rest-CRX/", 
    		"admin-Standard", 
    		"admin-Standard", 
    		"application/vnd.openmdx.wbxml" // text/xml
    	);        
    }
    
    /**
     * TestAll
     * 
     */
    public static class TestAll extends AbstractTest {
    	
		public TestAll(
		) {
			super(TestApi.entityManagerFactory);
		}
	
        @Test
        public void run(
        ) throws ServiceException, IOException, ParseException{
        	this.testUpdateAddressLine();
            this.testReflection();
            this.testCheckPermissions();
            this.testCreateUser();
        }
		
        protected void testUpdateAddressLine(
        ) throws ServiceException {
    		org.opencrx.kernel.account1.jmi1.Segment accountSegment = (org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
    			new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", providerName, "segment", segmentName)
    		);
    		ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
    		contactQuery.thereExistsFullName().like(".*test.*");
    		contactQuery.thereExistsAddress().modifiedAt().greaterThanOrEqualTo(new Date(0));
    		List<Contact> contacts = accountSegment.getAccount(contactQuery);
    		int count = 0;
    		for(Contact contact: contacts) {
    			PostalAddressQuery postalAddressQuery = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
    			List<PostalAddress> addresses = contact.getAddress(postalAddressQuery);
    			if(!addresses.isEmpty()) {
    				PostalAddress address = addresses.iterator().next();
    				System.out.println("updating address " + address.refGetPath());
    				{
    					System.out.println("1.postalAddressLine=" + address.getPostalAddressLine());
    					System.out.println("1.postalStreet=" + address.getPostalStreet());
	    				pm.currentTransaction().begin();
	    				address.getPostalAddressLine().clear();
	    				address.getPostalStreet().clear();
	    				pm.currentTransaction().commit();
    				}
    				{
    					System.out.println("2.postalAddressLine=" + address.getPostalAddressLine());
    					System.out.println("2.postalStreet=" + address.getPostalStreet());
    					pm.currentTransaction().begin();
    					address.getPostalStreet().add("postal street 1");
    					pm.currentTransaction().commit();
    				}
    				{
    					System.out.println("3.postalAddressLine=" + address.getPostalAddressLine());
    					System.out.println("3.postalStreet=" + address.getPostalStreet());
    					pm.currentTransaction().begin();
    					address.getPostalAddressLine().add("address line 1");
    					pm.currentTransaction().commit();
    				}
    				{
    					System.out.println("4.postalAddressLine=" + address.getPostalAddressLine());
    					System.out.println("4.postalStreet=" + address.getPostalStreet());
    					pm.currentTransaction().begin();
    					address.getPostalAddressLine().add("address line 2");
    					address.getPostalStreet().add("postal street 2");
    					pm.currentTransaction().commit();
    				}
    				{
    					System.out.println("5.postalAddressLine=" + address.getPostalAddressLine());
    					System.out.println("5.postalStreet=" + address.getPostalStreet());    					
    				}
    				break;
    			}
    			count++;
    			if(count > 100) break;
    		}
        }
        
	    protected void testReflection(
	    ) throws ServiceException {
	        try {
	        	// Test 1: set array
	        	EMailAddress emailAddress = pm.newInstance(EMailAddress.class);
	        	emailAddress.setUsage(new short[]{1, 2, 3});
	        	// Test 2: set array
	        	Member member = pm.newInstance(Member.class);
	        	member.setMemberRole(new short[]{1, 2, 3});
	        } finally {
	        }
	    }

	    protected void testCheckPermissions(
	    ) throws ServiceException {
	    	CheckPermissionsParams params = Structures.create(
    			CheckPermissionsParams.class, 
				Datatypes.member(CheckPermissionsParams.Member.principalName, "guest")
			);
	    	Contact contact = (Contact)pm.getObjectById(new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/CRX/segment/Standard/account/admin-Standard"));
	    	CheckPermissionsResult result = contact.checkPermissions(params);
	    	System.out.print(result.isHasUpdatePermission());
	    }
	    
	    protected void testCreateUser(
	    ) throws ServiceException {
	    	try {
	    		org.opencrx.kernel.home1.jmi1.Segment userHomeSegment = (org.opencrx.kernel.home1.jmi1.Segment)pm.getObjectById(
	    			new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName, "segment", segmentName)
	    		);
	    		org.opencrx.security.realm1.jmi1.PrincipalGroup primaryGroup = 
	    			(org.opencrx.security.realm1.jmi1.PrincipalGroup)SecureObject.getInstance().findPrincipal(
	    				SecurityKeys.USER_GROUP_USERS, 
	    				SecureObject.getRealmIdentity(providerName, segmentName),
	    				pm
	    			);
	    		ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
	    		contactQuery.thereExistsFullName().equalTo("test99");
	    		List<Contact> contacts = Accounts.getInstance().getAccountSegment(
	    			pm, providerName, segmentName
	    		).getAccount(
	    			contactQuery
	    		);
	    		if(!contacts.isEmpty()) {
	    			Contact contact = contacts.iterator().next();
		    		CreateUserParams params = Structures.create(
		    			CreateUserParams.class,
		    			Datatypes.member(CreateUserParams.Member.contact, contact),
		    			Datatypes.member(CreateUserParams.Member.initialPassword, "changeit"),
		    			Datatypes.member(CreateUserParams.Member.initialPasswordVerification, "changeit"),
		    			Datatypes.member(CreateUserParams.Member.primaryUserGroup, primaryGroup),
		    			Datatypes.member(CreateUserParams.Member.principalName, "test99")
		    		);
		    		try {
		    			pm.currentTransaction().begin();
		    			CreateUserResult result = userHomeSegment.createUser(params);
		    			pm.currentTransaction().commit();
		    			System.out.println("result.status=" + result.getStatus());
		    			System.out.println("result.home=" + result.getCreatedUserHome());
		    		} catch(Exception e) {
		    			System.out.println("change password failed. Message=" + e.getMessage());
		    			new ServiceException(e).log();
		    		}
	    		}
	    	} catch(Exception e) {
	    		new ServiceException(e).log();
	    	}
	    }
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected static PersistenceManagerFactory entityManagerFactory = null;
	protected static String providerName = "CRX";
	protected static String segmentName = "Standard";

}
