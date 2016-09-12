/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestQuery
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2011, CRIXP Corp., Switzerland
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.opencrx.kernel.account1.cci2.AbstractGroupQuery;
import org.opencrx.kernel.account1.cci2.AccountAddressQuery;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.cci2.GroupQuery;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.Group;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.cci2.MeetingQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.Meeting;
import org.opencrx.kernel.base.cci2.IndexEntryQuery;
import org.opencrx.kernel.base.jmi1.IndexEntry;
import org.opencrx.kernel.contract1.cci2.SalesOrderPositionQuery;
import org.opencrx.kernel.contract1.cci2.SalesOrderQuery;
import org.opencrx.kernel.contract1.cci2.SalesVolumeContractQuery;
import org.opencrx.kernel.contract1.jmi1.GenericContract;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.contract1.jmi1.SalesOrderPosition;
import org.opencrx.kernel.contract1.jmi1.SalesVolumeContract;
import org.opencrx.kernel.product1.cci2.AccountAssignmentProductQuery;
import org.opencrx.kernel.product1.cci2.ProductQuery;
import org.opencrx.kernel.product1.jmi1.AccountAssignmentProduct;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.ExtentCapable;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.rest.cci.QueryExtensionRecord;

import test.org.opencrx.generic.AbstractTest;

@RunWith(Suite.class)
@SuiteClasses(
    {
        TestQuery.TestAll.class
    }
)

/**
 * TestQuery
 */
public class TestQuery {

    @BeforeClass
    public static void initialize(
    ) throws NamingException, ServiceException {
//    	{
//          if(!NamingManager.hasInitialContextFactoryBuilder()) {
//                NonManagedInitialContextFactoryBuilder.install(null);
//          }    		
//        	entityManagerFactory = Utils.getPersistenceManagerFactory();
//    	}
    	{
	    	entityManagerFactory = Utils.getPersistenceManagerFactoryProxy(
	    		"http://127.0.0.1:8080/opencrx-rest-CRX/", 
	    		"admin-Standard", 
	    		"admin-Standard", 
	    		"application/vnd.openmdx.wbxml" // text/xml
	    	);
    	}
    }

    public static class TestAll extends AbstractTest {
    	
		public TestAll(
		) {
			super(TestQuery.entityManagerFactory);
		}
	
        @Test
        public void run(
        ) throws ServiceException, IOException, ParseException {
            this.testEmbeddedFeatures();
            this.testQueryExtensions();
            this.testNestedQueries();
            this.testExtent();
            this.testAccountAssignmentsQuery();  
            this.testMultivaluedReferences();
            this.testOwner();
        }
		
	    protected void testEmbeddedFeatures(
	    ) throws ServiceException{
	        try {
	        	org.opencrx.kernel.account1.jmi1.Segment accountSegment =
	        		(org.opencrx.kernel.account1.jmi1.Segment)this.pm.getObjectById(
		        		new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", providerName, "segment", segmentName)
		        	);
	        	GroupQuery groupQuery = (GroupQuery)this.pm.newQuery(Group.class);
	        	int count = 0;
	        	for(Group group: accountSegment.<Group>getAccount(groupQuery)) {
	        		MemberQuery memberQuery = (MemberQuery)this.pm.newQuery(Member.class);
	        		memberQuery.forAllMemberRole().notEqualTo((short)13);
	        		boolean hasMembers = !group.getMember(memberQuery).isEmpty();
	        		System.out.println("Group " + group.getName() + " has " + (hasMembers ? "" : "no ") + "members not having role 13");
	        		count++;
	        		if(count > 100) break;
	        	}
	        	System.out.println("account segment=" + accountSegment);
	        } finally {
	        }
	    }

	    protected void testQueryExtensions(
	    ) throws ServiceException{
	        try {
	        	org.opencrx.kernel.contract1.jmi1.Segment contractSegment =
	        		(org.opencrx.kernel.contract1.jmi1.Segment)this.pm.getObjectById(
		        		new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName)
		        	);
	        	System.out.println("contract segment=" + contractSegment);
	        	org.opencrx.kernel.contract1.cci2.SalesOrderQuery salesOrderQuery = 
	        		(org.opencrx.kernel.contract1.cci2.SalesOrderQuery)pm.newQuery(
	        			org.opencrx.kernel.contract1.jmi1.SalesOrder.class
	        		);
	        	QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(salesOrderQuery);
            	queryExtension.setClause(
            		"EXISTS (" + 
            		"  SELECT 0 FROM " + 
            		"    OOCKE1_CONTRACTPOSITION cp " + 
            		"  INNER JOIN " + 
            		"    OOCKE1_PRODUCTCONFIG pc " + 
            		"  ON " + 
            		"    pc.p$$parent = cp.object_id " + 
            		"  INNER JOIN " + 
            		"    OOCKE1_PROPERTY p " + 
            		"  ON " + 
            		"    p.p$$parent = pc.object_id" + 
            		"  WHERE " + 
            		"    p.name = 'FieldUri' AND " + 
            		"    pc.name = 'CropScan.Default' AND " + 
            		"    cp.p$$parent = v.object_id" + 
            		")"
            	);
	        	List<org.opencrx.kernel.contract1.jmi1.SalesOrder> salesOrders = contractSegment.getSalesOrder(salesOrderQuery);
	        	assertTrue(salesOrderQuery.toString(), !salesOrders.isEmpty());	        	
	        } finally {
	        }
	    }
	    
	    protected void testNestedQueries(
	    ) throws ServiceException{
	        try {
	        	org.opencrx.kernel.contract1.jmi1.Segment contractSegment =
	        		(org.opencrx.kernel.contract1.jmi1.Segment)this.pm.getObjectById(
		        		new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName)
		        	);
	        	System.out.println("contract segment=" + contractSegment);
	        	org.opencrx.kernel.product1.jmi1.Segment productSegment =
	        		(org.opencrx.kernel.product1.jmi1.Segment)this.pm.getObjectById(
			        	new Path("xri://@openmdx*org.opencrx.kernel.product1").getDescendant("provider", providerName, "segment", segmentName)
		        	);
	        	System.out.println("product segment=" + productSegment);
		    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = 
		    		(org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
		        		new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", providerName, "segment", segmentName)
		    		);	        	
		    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = 
		    		(org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
		        		new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName, "segment", segmentName)
		    		);
		    	// Test 0: Get addresses where owner of the address is member of a group
		    	{
			    	GroupQuery groupQuery = (GroupQuery)this.pm.newQuery(Group.class);
			    	groupQuery.orderByName().ascending();
			    	List<Group> groups = accountSegment.getAccount(groupQuery);
			    	int count = 0;
			    	for(Group group: groups) {
			    		AccountAddressQuery addressQuery = (AccountAddressQuery)this.pm.newQuery(AccountAddress.class);
			    		addressQuery.account().thereExistsAccountMembership().thereExistsAccountFrom().equalTo(group);
			    		addressQuery.account().thereExistsAccountMembership().distance().equalTo(-1);
			    		List<AccountAddress> addresses = accountSegment.getAddress(addressQuery);
			    		System.out.println("Group " + group.getName() + " has addresses " + !addresses.isEmpty());
			    		count++;
			    		if(count > 10) break;
			    	}
		    	}
		    	// Test 1: Account groups with new members
		    	{
		    		Date since = new Date(System.currentTimeMillis() - 86400000L);
		    		GroupQuery groupQuery = (GroupQuery)this.pm.newQuery(Group.class);
		    		groupQuery.thereExistsMember().createdAt().greaterThan(since);
		    		groupQuery.orderByCreatedAt().ascending();
			    	List<Group> groups = accountSegment.getAccount(groupQuery);
			    	for(Group group: groups) {
			    		System.out.println("Group " + group.getName() + " has new members");
			    	}
		    	}
		    	// Test 2: Accounts with assigned activities matching name .*Test.*
		    	{
		    		AccountQuery accountQuery = (AccountQuery)pm.newQuery(Account.class);
		    		accountQuery.thereExistsAssignedActivity().name().like(".*Test.*");
		    		List<Account> accounts = accountSegment.getAccount(accountQuery);
		    		int count = 0;
		    		for(Account account: accounts) {
		    			System.out.println("Account " + account.getFullName() + " has assigned activities matching name like .*Test.*");
		    			count++;
		    			if(count > 50) break;		    			
		    		}
		    	}
	        	// Test 3: Get sales orders which have a position which a configuration with name "CropScan.*" and a property "FieldUri"
		    	{
		        	SalesOrderQuery salesOrderQuery = 
		        		(org.opencrx.kernel.contract1.cci2.SalesOrderQuery)pm.newQuery(
		        			org.opencrx.kernel.contract1.jmi1.SalesOrder.class
		        		);
		        	salesOrderQuery.thereExistsPosition().thereExistsConfiguration().name().like("CropScan.*");
		        	salesOrderQuery.thereExistsPosition().thereExistsConfiguration().thereExistsProperty().name().equalTo("FieldUri");
		        	List<SalesOrder> salesOrders = contractSegment.getSalesOrder(salesOrderQuery);
		        	assertTrue(salesOrderQuery.toString(), !salesOrders.isEmpty());
		    	}
	        	// Test 4
		    	{
		        	SalesOrderQuery salesOrderQuery = 
		        		(org.opencrx.kernel.contract1.cci2.SalesOrderQuery)pm.newQuery(
		        			org.opencrx.kernel.contract1.jmi1.SalesOrder.class
		        		);
		        	salesOrderQuery.thereExistsSalesRep().thereExistsFullName().like("F.*");
		        	List<SalesOrder> salesOrders = contractSegment.getSalesOrder(salesOrderQuery);
		        	assertTrue(salesOrderQuery.toString(), !salesOrders.isEmpty());
		    	}
	        	// Test 5
		    	{
		        	org.opencrx.kernel.home1.jmi1.UserHome guest = (org.opencrx.kernel.home1.jmi1.UserHome)this.pm.getObjectById(
		        		new Path("xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName, "segment", segmentName, "userHome", "guest")
		        	);
		        	assertNotNull("UserHome guest", guest);
		        	org.opencrx.kernel.product1.cci2.ProductQuery productQuery = (org.opencrx.kernel.product1.cci2.ProductQuery)this.pm.newQuery(
		        		org.opencrx.kernel.product1.jmi1.Product.class
		        	);
		        	productQuery.thereExistsAssignedAccount().thereExistsAccount().equalTo(guest.getContact());
		        	List<org.opencrx.kernel.product1.jmi1.Product> products = productSegment.getProduct(productQuery);
		        	assertTrue(productQuery.toString(), !products.isEmpty());
		    	}
	        	// Test 6
		    	{
		        	AbstractGroupQuery groupQuery = (AbstractGroupQuery)this.pm.newQuery(AbstractGroup.class);
		        	List<AbstractGroup> groups = accountSegment.getAccount(groupQuery);
		        	int counti = 0;
		        	for(AbstractGroup group: groups) {
			        	MeetingQuery meetingQuery = (MeetingQuery)this.pm.newQuery(Meeting.class);	        	
			        	meetingQuery.thereExistsMeetingParty().thereExistsParty().thereExistsAccountMembership().thereExistsAccountFrom().equalTo(group);
			        	meetingQuery.thereExistsMeetingParty().thereExistsParty().thereExistsAccountMembership().distance().equalTo(-1);
			        	List<Meeting> meetings = activitySegment.getActivity(meetingQuery);
			        	int countj = 0;
			        	for(Meeting meeting: meetings) {
			        		System.out.println("Meeting " + meeting.getActivityNumber() + " has party where account is member of group " + group.getName());
			        		countj++;
			        		if(countj > 50) break;
			        	}
			        	counti++;
			        	if(counti > 500) break;
		        	}
		    	}
	        	// Test 7: Get accounts which have only notes with title "Test"
		    	{
		        	AccountQuery accountQuery = (AccountQuery)this.pm.newQuery(Account.class);
		        	accountQuery.forAllNote().thereExistsTitle().equalTo("Test");
		        	accountQuery.orderByFullName().ascending();
		        	List<Account> accounts = accountSegment.getAccount(accountQuery);
		        	int count = 0;
		        	for(Account account: accounts) {
		        		System.out.println("Account " + account.getFullName() + " (" + account.refMofId() + ")" + " has only notes with Title 'Test'");
		        		count++;
		        		if(count > 100) break;
		        	}
		    	}
		    	// Test 8: Get contacts where email matches
		    	{
		    		ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
		    		EMailAddressQuery emailAddressQuery = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
		    		emailAddressQuery.thereExistsEmailAddress().like(".*com");
		    		contactQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(emailAddressQuery));
		    		int count = 0;
		    		for(Contact contact: accountSegment.<Contact>getAccount(contactQuery)) {
		        		System.out.println("Contact " + contact.getFullName() + " (" + contact.refMofId() + ")" + " has an email address like '.*com'");
		        		count++;
		        		if(count > 100) break;		    			
		    		}
		    	}
		    	// Test 9: Accounts having members with given memberRole
		    	{
		    		ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
		    		contactQuery.thereExistsMember().thereExistsMemberRole().equalTo((short)13);
		    		int count = 0;
		    		for(Contact contact: accountSegment.<Contact>getAccount(contactQuery)) {
		        		System.out.println("Contact " + contact.getFullName() + " (" + contact.refMofId() + ")" + " has member having memberRole 13");
		        		count++;
		        		if(count > 100) break;
		    		}
		    	}
		    	// Test 10: Accounts having addresses
		    	{
		    		AccountQuery query = (AccountQuery)pm.newQuery(Account.class);		    		
		    		query.thereExistsAddress().modifiedAt().greaterThanOrEqualTo(new Date(0));
		    		int count = 0;
		    		for(Account account: accountSegment.<Account>getAccount(query)) {
		    			assertTrue("Account must have at least one address", !account.getAddress().isEmpty());
		        		System.out.println("Account " + account.getFullName() + " (" + account.refMofId() + ")" + " has " + account.getAddress().size() + " addresses");
		        		count++;
		        		if(count > 100) break;
		    		}
		    	}
		    	// Test 11: Accounts having no addresses
		    	{
		    		AccountQuery query = (AccountQuery)pm.newQuery(Account.class);		    		
		    		query.forAllAddress().modifiedAt().lessThan(new Date(0));
		    		int count = 0;
		    		for(Account account: accountSegment.<Account>getAccount(query)) {
		    			//assertTrue("Account must have no address", account.getAddress().isEmpty());
		        		System.out.println("Account " + account.getFullName() + " (" + account.refMofId() + ")" + " has " + account.getAddress().size() + " addresses");
		        		count++;
		        		if(count > 100) break;
		    		}
		    	}
		    	// Test 12: Nested query with reference assignedAccount on abstract root class AbstractContract		    
		    	{
		    		Date salesOrderDate = new Date();
		    		int count = 0;
		    		for(Account account: accountSegment.<Account>getAccount()) {
						SalesVolumeContractQuery salesVolumeContractQuery = (SalesVolumeContractQuery)pm.newQuery(SalesVolumeContract.class);
						salesVolumeContractQuery.forAllDisabled().isFalse();
						salesVolumeContractQuery.thereExistsAssignedAccount().thereExistsAccount().equalTo(account);
						salesVolumeContractQuery.thereExistsAssignedAccount().forAllValidFrom().lessThanOrEqualTo(salesOrderDate);
						salesVolumeContractQuery.thereExistsAssignedAccount().forAllValidTo().greaterThanOrEqualTo(salesOrderDate);
						salesVolumeContractQuery.thereExistsAssignedAccount().accountRole().equalTo((short)100);
						salesVolumeContractQuery.contractState().lessThanOrEqualTo(new Short((short)1000));
						salesVolumeContractQuery.forAllActiveOn().lessThanOrEqualTo(salesOrderDate);
						salesVolumeContractQuery.forAllExpiresOn().greaterThanOrEqualTo(salesOrderDate);
						salesVolumeContractQuery.orderByContractNumber().ascending();
						List<SalesVolumeContract> salesVolumeContracts = contractSegment.getContract(salesVolumeContractQuery);
						System.out.println("Account " + account.getFullName() + " has sales volume contracts: " + !salesVolumeContracts.isEmpty());
						count++;
						if(count > 50) break;
		    		}
		    	}
		    	// Test 13: Nested query with reference to abstract root class ActivityGroup		    
		    	{
		    		int count = 0;
		    		for(ActivityTracker activityTracker: activitySegment.<ActivityTracker>getActivityTracker()) {
		    			ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
		    			activityQuery.thereExistsAssignedGroup().thereExistsActivityGroup().equalTo(activityTracker);
						List<Activity> activities = activityTracker.getFilteredActivity(activityQuery);
						System.out.println("Activity tracker " + activityTracker.getName() + " has activities: " + !activities.isEmpty());
						count++;
						if(count > 50) break;
		    		}
		    	}
	        } finally {
	        }
	    }

	    protected void testExtent(
	    ) {
            org.opencrx.kernel.contract1.jmi1.Segment contractSegment = 
            	(org.opencrx.kernel.contract1.jmi1.Segment)pm.getObjectById(
	        		new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName)
            	);
            {
            	SalesOrderPositionQuery salesOrderPositionQuery = (SalesOrderPositionQuery)PersistenceHelper.newQuery(
            		this.pm.getExtent(SalesOrderPosition.class),
            		contractSegment.refGetPath().getDescendant("salesOrder", ":*", "position", ":*")
                );
	            salesOrderPositionQuery.forAllDisabled().isFalse();
	            salesOrderPositionQuery.forAllExternalLink().startsNotWith("TEST:");
	            salesOrderPositionQuery.thereExistsConfiguration().name().equalTo("CropScan.Default");
	            List<SalesOrderPosition> salesOrderPositions = contractSegment.getExtent(salesOrderPositionQuery);            
	            List<Path> salesOrderPositionIdentities = new ArrayList<Path>();
	            for(SalesOrderPosition salesOrderPosition: salesOrderPositions) {
	            	salesOrderPositionIdentities.add(salesOrderPosition.refGetPath());
	            }
	        	assertTrue(salesOrderPositionQuery.toString(), !salesOrderPositionIdentities.isEmpty());
            }
            {
            	int count = 0;
            	for(GenericContract contract: contractSegment.<GenericContract>getContract()) {
            		// Get index entries where the XRIs of the indexed object matches a pattern
                	IndexEntryQuery query = (IndexEntryQuery)PersistenceHelper.newQuery(
                		pm.getExtent(IndexEntry.class),
                		contractSegment.refGetPath().getDescendant("indexEntry", ":*")
                	);
                	query.thereExistsIndexedObject().elementOf(
            			PersistenceHelper.getCandidates(
    	        			pm.getExtent(ExtentCapable.class, true),
    	        			contract.refMofId() + "/($...)"
            			)
            		);
                	System.out.println("Contract " + contract.refMofId() + " and its composites have " + contractSegment.getExtent(query).size() + " index entries");
                	count++;
                	if(count > 50) break;
            	}
            }
	    }

	    protected void testAccountAssignmentsQuery(
	    ) {	    	
	    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = 
	    		(org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
	        		new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", providerName, "segment", segmentName)
	    		);
	    	org.opencrx.kernel.product1.jmi1.Segment productSegment = 
	    		(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
	        		new Path("xri://@openmdx*org.opencrx.kernel.product1").getDescendant("provider", providerName, "segment", segmentName)
	    		);
	    	Collection<Account> accounts = accountSegment.getAccount();
    		// New style extent query
	    	{
		    	int count = 0;
		    	for(Account account: accounts) {
	            	AccountAssignmentProductQuery query = (AccountAssignmentProductQuery)PersistenceHelper.newQuery(
	            		pm.getExtent(AccountAssignmentProduct.class), 
	            		productSegment.refGetPath().getDescendant("product", ":*", "assignedAccount", "%")
	            	);
		    		Date since = new Date(System.currentTimeMillis() - (10L * 86400L * 1000L));
		    		query.thereExistsAccount().equalTo(account);
		    		query.thereExistsValidFrom().greaterThan(since);
		    		query.accountRole().greaterThanOrEqualTo((short)2);		        
		    		List<AccountAssignmentProduct> assignments = productSegment.getExtent(query);
		    		Set<Date> dates = new TreeSet<Date>();
		    		for(AccountAssignmentProduct assignment: assignments) {
		    			dates.add(assignment.getValidFrom());
		    		}
		    		System.out.println("Dates=" + dates);
		    		count++;
		    		if(count > 50) break;
		    	}
	    	}
	    	// Products with specific account assignment
	    	{
		    	if(!accounts.isEmpty()) {
			    	Account account = accounts.iterator().next();
			    	ProductQuery productQuery = (ProductQuery)this.pm.newQuery(Product.class);
		    		Date since = new Date(System.currentTimeMillis() - (10L * 86400L * 1000L));
		    		productQuery.thereExistsAssignedAccount().thereExistsAccount().equalTo(account);
		    		productQuery.thereExistsAssignedAccount().thereExistsValidFrom().greaterThan(since);
		    		productQuery.thereExistsAssignedAccount().accountRole().greaterThanOrEqualTo((short)2);
		    		List<Product> products = productSegment.getProduct(productQuery);
		    		int count = 0;
		    		for(Product product: products) {
		    			System.out.println("Product=" + product.getProductNumber());
			    		count++;
			    		if(count > 50) break;
		    		}
		    	}
	    	}
	    	// Accounts member of group 
	    	{
	    		GroupQuery groupQuery = (GroupQuery)this.pm.newQuery(Group.class);
	    		int count = 0;
	    		for(Group group: accountSegment.<Group>getAccount(groupQuery)) {
	    			ContactQuery newContactsQuery = (ContactQuery)this.pm.newQuery(Contact.class);
					newContactsQuery.forAllDisabled().isFalse();
	    			newContactsQuery.thereExistsAccountMembership().thereExistsAccountFrom().equalTo(group);
	    			newContactsQuery.thereExistsAccountMembership().distance().equalTo(-1);
	    			newContactsQuery.thereExistsAccountMembership().forAllDisabled().isFalse();
					newContactsQuery.thereExistsAccountMembership().createdAt().greaterThanOrEqualTo(new Date(0L));
					newContactsQuery.thereExistsAccountMembership().orderByModifiedAt().descending();	    			
	    			int count1 = 0;
	    			for(Contact contact: accountSegment.<Contact>getAccount(newContactsQuery)) {
	    				System.out.println("Contact " + contact.getFullName() + " is member of group " + group.getFullName());
	    				count1++;
	    				if(count1 > 50) break;
	    			}
	    			count++;
	    			if(count > 50) break;
	    		}
	    	}
	    }

	    protected void testMultivaluedReferences(
	    ) {
	    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = 
	    		(org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
	        		new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName, "segment", segmentName)
	    		);	    	
	    	ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
	    	query.contract().isEmpty();
	    	List<Activity> activities = activitySegment.getActivity(query);
	    	System.out.println("Exists activities where contract().isEmpty() is " + !activities.isEmpty());
	    	query = (ActivityQuery)pm.newQuery(Activity.class);
	    	query.contract().isNonEmpty();
	    	activities = activitySegment.getActivity(query);
	    	System.out.println("Exists activities where contract().isNonEmpty() is " + !activities.isEmpty());	    	
	    }
	    	
	    protected void testOwner(
	    ) {
	    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = 
	    		(org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
	        		new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", providerName, "segment", segmentName)
	    		);
	    	String groupName = "Users";
	    	ActivityQuery query = (ActivityQuery)pm.newQuery(Activity.class);
	    	query.thereExistsOwner().elementOf(segmentName + ":" + groupName);
	    	List<Activity> activities = activitySegment.getActivity(query);
	    	int count = 0;
	    	for(Activity activity: activities) {
	    		System.out.println("Activity=" + activity.getActivityNumber());
	    		count++;
	    		if(count > 50) break;
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
