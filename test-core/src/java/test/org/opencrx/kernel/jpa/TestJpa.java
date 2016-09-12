/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TestJpa
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2008, CRIXP Corp., Switzerland
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
package test.org.opencrx.kernel.jpa;

import java.util.List;

import javax.jdo.JDOHelper;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.Query;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class TestJpa
    extends TestCase {

    //-----------------------------------------------------------------------
    /**
     * Constructs a test case with the given name.
     */
    public TestJpa(
        String name
    ) {
        super(name);
        if(em == null) {
            EntityManagerFactory emf = Persistence.createEntityManagerFactory(null);            
            em = emf.createEntityManager();        
        }
    }
    
    //-----------------------------------------------------------------------
    /**
     * The batch TestRunner can be given a class to run directly.
     * To start the batch runner from your main you can write: 
     */
    public static void main(
        String[] args
    ) {
        junit.textui.TestRunner.run (suite());
    }
    
    //-----------------------------------------------------------------------
    /**
     * A test runner either expects a static method suite as the
     * entry point to get a test to run or it will extract the 
     * suite automatically. 
     */
    public static Test suite(
    ) {
        return new TestSuite(TestJpa.class);
    }

    //-----------------------------------------------------------------------
    private void printSecureObject(
        org.opencrx.kernel.base.cci2.SecureObject secureObject
    ) {
        //System.out.println("  owners: " + secureObject.getOwner());
        System.out.println("  access level browse: " + secureObject.getAccessLevelBrowse());
        System.out.println("  access level delete: " + secureObject.getAccessLevelDelete());
        System.out.println("  access level update: " + secureObject.getAccessLevelUpdate());        
    }
    
    //-----------------------------------------------------------------------
    private void printAccountAddress(
        org.opencrx.kernel.address1.cci2.Addressable address
    ) {
        System.out.println();
        if(address instanceof org.opencrx.kernel.address1.cci2.PostalAddressable) {
            org.opencrx.kernel.address1.cci2.PostalAddressable postalAddress = (org.opencrx.kernel.address1.cci2.PostalAddressable)address;
            System.out.println("postal address " + postalAddress.getPostalAddressLine());
            System.out.println("  usage: " + address.getUsage());
            System.out.println("  street: " + postalAddress.getPostalStreet());
            System.out.println("  code: " + postalAddress.getPostalCode());
            System.out.println("  city: " + postalAddress.getPostalCode());
        }
        else if(address instanceof org.opencrx.kernel.address1.cci2.PhoneNumberAddressable) {
            org.opencrx.kernel.address1.cci2.PhoneNumberAddressable phoneNumber = (org.opencrx.kernel.address1.cci2.PhoneNumberAddressable)address;
            System.out.println("phone number " + phoneNumber.getPhoneNumberFull());
            System.out.println("  usage: " + address.getUsage());
        }
        else if(address instanceof org.opencrx.kernel.address1.cci2.WebAddressable) {
            org.opencrx.kernel.address1.cci2.WebAddressable webAddress = (org.opencrx.kernel.address1.cci2.WebAddressable)address;
            System.out.println("web address " + webAddress.getWebUrl());
            System.out.println("  usage: " + address.getUsage());
        }
        else if(address instanceof org.opencrx.kernel.address1.cci2.EMailAddressable) {
            org.opencrx.kernel.address1.cci2.EMailAddressable emailAddress = (org.opencrx.kernel.address1.cci2.EMailAddressable)address;
            System.out.println("email address " + emailAddress.getEmailAddress());
            System.out.println("  usage: " + address.getUsage());
        }           
    }

    //-----------------------------------------------------------------------
    public void printCrxObject(
        org.opencrx.kernel.generic.cci2.CrxObject crxObject
    ) {
        crxObject.getCreatedBy().toString();
        this.printSecureObject(crxObject);
        Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.generic.jpa3.Note e WHERE e.crxObject = ?1");
        q.setParameter(1, JDOHelper.getObjectId(crxObject));
        List<org.opencrx.kernel.generic.cci2.Note> notes = q.getResultList();        
        for(org.opencrx.kernel.generic.cci2.Note note : notes) {
            System.out.println();
            System.out.println("  note " + note.getTitle());
        }
        q = em.createQuery("SELECT e FROM org.opencrx.kernel.generic.jpa3.Rating e WHERE e.crxObject = ?1");
        q.setParameter(1, JDOHelper.getObjectId(crxObject));
        List<org.opencrx.kernel.generic.cci2.Rating> ratings = q.getResultList();        
        for(org.opencrx.kernel.generic.cci2.Rating rating : ratings) {
            System.out.println();
            System.out.println("  rating " + rating.getDescription());
        }
        q = em.createQuery("SELECT e FROM org.opencrx.kernel.generic.jpa3.PropertySetEntry e WHERE e.crxObject = ?1");
        q.setParameter(1, JDOHelper.getObjectId(crxObject));
        List<org.opencrx.kernel.generic.cci2.PropertySetEntry> propertySetEntries = q.getResultList();        
        for(org.opencrx.kernel.generic.cci2.PropertySetEntry property : propertySetEntries) {
            System.out.println();
            System.out.println("  property " + property.getPropertyName());
        }
//        q = em.createQuery("SELECT e FROM org.opencrx.kernel.base.jpa3.AuditEntry e WHERE e.auditee = ?1");
//        q.setParameter(1, objectIdBuilder.toPath(JDOHelper.getObjectId(crxObject)).toXri());
//        List<org.opencrx.kernel.base.cci2.AuditEntry> auditEntries = q.getResultList();        
//        for(org.opencrx.kernel.base.cci2.AuditEntry auditEntry : auditEntries) {
//            System.out.println();
//            System.out.println("  audit " + auditEntry.getCreatedAt());
//        }
        q = em.createQuery("SELECT e FROM org.opencrx.kernel.base.jpa3.IndexEntry e WHERE e.indexed = ?1");
        q.setParameter(1, JDOHelper.getObjectId(crxObject));
        List<org.opencrx.kernel.base.cci2.IndexEntry> indexEntries = q.getResultList();        
        for(org.opencrx.kernel.base.cci2.IndexEntry indexEntry : indexEntries) {
            System.out.println();
            System.out.println("  index " + indexEntry.getKeywords());
        }
    }

    //-----------------------------------------------------------------------
    public void testAccounts(
    ) throws Exception {
        if(RUN_TEST_ACCOUNTS) {
            try {
                em.getTransaction().begin();            
                Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.account1.jpa3.Segment e WHERE e.openmdxjdoIdentity = 'accounts/CRX/Standard'");
                List<org.opencrx.kernel.account1.cci2.Segment> segments = q.getResultList();            
                org.opencrx.kernel.account1.cci2.Segment segment = segments.get(0);
                System.out.println("account1 segment");
                System.out.println("  description: " + segment.getDescription());
                this.printSecureObject(segment);
        
                // Accounts
                int ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.account1.jpa3.Account e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.account1.cci2.Account> accounts = q.getResultList();
                for(org.opencrx.kernel.account1.cci2.Account account : accounts) {
                    System.out.println();
                    System.out.println("account " + account.getFullName());
                    System.out.println("  description: " + account.getDescription());
                    this.printCrxObject(account);
                    q = em.createQuery("SELECT e FROM org.opencrx.kernel.account1.jpa3.AccountAddress e WHERE e.account = ?1");
                    q.setParameter(1, JDOHelper.getObjectId(account));
                    List<org.opencrx.kernel.account1.cci2.AccountAddress> addresses = q.getResultList();                
                    for(org.opencrx.kernel.account1.cci2.AccountAddress address : addresses) {
                        this.printAccountAddress(address);
                        this.printCrxObject(address);                        
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;
                }    
                
                // Addresses
                ii = 0;                        
                q = em.createQuery("SELECT OBJECT(a) FROM org.opencrx.kernel.account1.jpa3.AccountAddress a, org.opencrx.kernel.account1.jpa3.Segment s WHERE a MEMBER OF s.address AND s = ?1");
                q.setParameter(1, segment);
                List<org.opencrx.kernel.account1.cci2.AccountAddress> addresses = q.getResultList();            
                for(org.opencrx.kernel.account1.cci2.AccountAddress address : addresses) {
                    this.printAccountAddress(address);
                    this.printCrxObject(address);                        
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Competitors
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.account1.jpa3.Competitor e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.account1.cci2.Competitor> competitors = q.getResultList();        
                for(org.opencrx.kernel.account1.cci2.Competitor competitor : competitors) {
                    System.out.println();
                    System.out.println("  competitor " + competitor.getName());
                    System.out.println("    description: " + competitor.getDescription());
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Account filters
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.account1.jpa3.AccountFilterGlobal e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.account1.cci2.AccountFilterGlobal> accountFilters = q.getResultList();        
                for(org.opencrx.kernel.account1.cci2.AccountFilterGlobal accountFilter : accountFilters) {
                    System.out.println();
                    System.out.println("  account filter " + accountFilter.getName());
                    System.out.println("    description: " + accountFilter.getDescription());
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Address filters
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.account1.jpa3.AddressFilterGlobal e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.account1.cci2.AddressFilterGlobal> addressFilters = q.getResultList();        
                for(org.opencrx.kernel.account1.cci2.AddressFilterGlobal addressFilter : addressFilters) {
                    System.out.println();
                    System.out.println("  address filter " + addressFilter.getName());
                    System.out.println("    description: " + addressFilter.getDescription());
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
            }
            finally {
                em.getTransaction().commit();            
            }
        }
    }

    //-----------------------------------------------------------------------
    private void printActivity(
        org.opencrx.kernel.activity1.cci2.Activity activity
    ) {
        System.out.println();
        System.out.println("activity " + activity.getActivityNumber());
        System.out.println("  name: " + activity.getName());
        System.out.println("  description: " + activity.getDescription());
        this.printCrxObject(activity);         
        Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.ActivityFollowUp e WHERE e.activity = ?1");
        q.setParameter(1, JDOHelper.getObjectId(activity));
        List<org.opencrx.kernel.activity1.cci2.ActivityFollowUp> followUps = q.getResultList();        
        for(org.opencrx.kernel.activity1.cci2.ActivityFollowUp followUp : followUps) {
            System.out.println();
            System.out.println("  follow up " + followUp.getTitle());
            System.out.println("    text: " + followUp.getText());
        }
    }
    
    //-----------------------------------------------------------------------
    public void testActivities(
    ) throws Exception {
        if(RUN_TEST_ACTIVITIES) {
            try {
                em.getTransaction().begin();    
                Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.Segment e WHERE e.openmdxjdoIdentity = 'activities/CRX/Standard'");
                List<org.opencrx.kernel.activity1.cci2.Segment> segments = q.getResultList();            
                org.opencrx.kernel.activity1.cci2.Segment segment = segments.get(0);
                System.out.println("activity1 segment");
                System.out.println("  description: " + segment.getDescription());
                this.printSecureObject(segment);
                
                // Activities
                int ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.Activity e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.Activity> activities = q.getResultList();                    
                for(org.opencrx.kernel.activity1.cci2.Activity activity : activities) {
                    this.printActivity(activity);
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Creators
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.ActivityCreator e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.ActivityCreator> activityCreators = q.getResultList();                    
                for(org.opencrx.kernel.activity1.cci2.ActivityCreator activityCreator : activityCreators) {
                    System.out.println();
                    System.out.println("activity creator " + activityCreator.getName());
                    System.out.println("  description: " + activityCreator.getDescription());
                    this.printCrxObject(activityCreator);                
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Trackers
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.ActivityTracker e WHERE e.segment = ?1 AND e.openmdxjdoIdentity LIKE 'activityTracker/%'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.ActivityTracker> activityTrackers = q.getResultList();                    
                for(org.opencrx.kernel.activity1.cci2.ActivityTracker activityTracker : activityTrackers) {
                    System.out.println();
                    System.out.println("activity tracker " + activityTracker.getName());
                    System.out.println("  description: " + activityTracker.getDescription());
                    this.printCrxObject(activityTracker);
                    q = em.createQuery("SELECT OBJECT(a) FROM org.opencrx.kernel.activity1.jpa3.Activity a, org.opencrx.kernel.activity1.jpa3.ActivityTracker g WHERE a MEMBER OF g.filteredActivity AND g = ?1");
                    q.setParameter(1, activityTracker);
                    List<org.opencrx.kernel.activity1.cci2.Activity> filteredActivities = q.getResultList();                            
                    int jj = 0;
                    for(org.opencrx.kernel.activity1.cci2.Activity activity: filteredActivities) {
                        this.printActivity(activity);
                        jj++;
                        if(jj > MAX_COUNT) break;            
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Milestones
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.ActivityMilestone e WHERE e.segment = ?1 AND e.openmdxjdoIdentity LIKE 'activityMilestone/%'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.ActivityMilestone> activityMilestones = q.getResultList();                                
                for(org.opencrx.kernel.activity1.cci2.ActivityMilestone activityMilestone : activityMilestones) {
                    System.out.println();
                    System.out.println("activity milestone " + activityMilestone.getName());
                    System.out.println("  description: " + activityMilestone.getDescription());
                    this.printCrxObject(activityMilestone);                
                    q = em.createQuery("SELECT OBJECT(a) FROM org.opencrx.kernel.activity1.jpa3.Activity a, org.opencrx.kernel.activity1.jpa3.ActivityMilestone g WHERE a MEMBER OF g.filteredActivity AND g = ?1");
                    q.setParameter(1, activityMilestone);
                    List<org.opencrx.kernel.activity1.cci2.Activity> filteredActivities = q.getResultList();                            
                    int jj = 0;
                    for(org.opencrx.kernel.activity1.cci2.Activity activity: filteredActivities) {
                        this.printActivity(activity);
                        jj++;
                        if(jj > MAX_COUNT) break;            
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Category
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.ActivityCategory e WHERE e.segment = ?1 AND e.openmdxjdoIdentity LIKE 'activityCategory/%'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.ActivityCategory> activityCategories = q.getResultList();                                
                for(org.opencrx.kernel.activity1.cci2.ActivityCategory category : activityCategories) {
                    System.out.println();
                    System.out.println("activity category " + category.getName());
                    System.out.println("  description: " + category.getDescription());
                    this.printCrxObject(category);                
                    q = em.createQuery("SELECT OBJECT(a) FROM org.opencrx.kernel.activity1.jpa3.Activity a, org.opencrx.kernel.activity1.jpa3.ActivityCategory g WHERE a MEMBER OF g.filteredActivity AND g = ?1");
                    q.setParameter(1, category);
                    List<org.opencrx.kernel.activity1.cci2.Activity> filteredActivities = q.getResultList();                            
                    int jj = 0;
                    for(org.opencrx.kernel.activity1.cci2.Activity activity: filteredActivities) {
                        this.printActivity(activity);
                        jj++;
                        if(jj > MAX_COUNT) break;            
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Resources
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.Resource e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.Resource> resources = q.getResultList();                                
                for(org.opencrx.kernel.activity1.cci2.Resource resource : resources) {
                    System.out.println();
                    System.out.println("resource " + resource.getName());
                    System.out.println("  description: " + resource.getDescription());
                    this.printCrxObject(resource);                
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Calendars
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.Calendar e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.Calendar> calendars = q.getResultList();                                            
                for(org.opencrx.kernel.activity1.cci2.Calendar calendar : calendars) {
                    System.out.println();
                    System.out.println("calendar " + calendar.getName());
                    System.out.println("  description: " + calendar.getDescription());
                    this.printCrxObject(calendar);                
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Processes
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.ActivityProcess e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.ActivityProcess> activityProcesses = q.getResultList();                                            
                for(org.opencrx.kernel.activity1.cci2.ActivityProcess activityProcess : activityProcesses) {
                    System.out.println();
                    System.out.println("process " + activityProcess.getName());
                    System.out.println("  description: " + activityProcess.getDescription());
                    this.printCrxObject(activityProcess);                
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Activity types
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.activity1.jpa3.ActivityType e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.activity1.cci2.ActivityType> activityTypes = q.getResultList();                                                        
                for(org.opencrx.kernel.activity1.cci2.ActivityType activityType : activityTypes) {
                    System.out.println();
                    System.out.println("type " + activityType.getName());
                    System.out.println("  description: " + activityType.getDescription());
                    this.printCrxObject(activityType);                
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }            
            }
            finally {
                em.getTransaction().commit();
            }
        }
    }

    //-----------------------------------------------------------------------
    private void printContractPosition(
        org.opencrx.kernel.contract1.cci2.SalesContractPosition position
    ) {
        System.out.println();
        System.out.println("  position " + position.getPositionNumber());
        System.out.println("    name: " + position.getName());
        System.out.println("    description: " + position.getDescription());
        System.out.println("    base amount: " + position.getBaseAmount());
    }
    
    //-----------------------------------------------------------------------
    public void testContracts(
    ) throws Exception {
        if(RUN_TEST_CONTRACTS) {
            try{
                em.getTransaction().begin();         
                Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.Segment e WHERE e.openmdxjdoIdentity = 'contracts/CRX/Standard'");
                List<org.opencrx.kernel.contract1.cci2.Segment> segments = q.getResultList();            
                org.opencrx.kernel.contract1.cci2.Segment segment = segments.get(0);
                System.out.println("contract1 segment");
                System.out.println("  description: " + segment.getDescription());
                this.printSecureObject(segment);
                
                // Leads
                int ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.Lead e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.contract1.cci2.Lead> leads = q.getResultList();            
                for(org.opencrx.kernel.contract1.cci2.Lead lead : leads) {
                    System.out.println();
                    System.out.println("lead " + lead.getContractNumber());
                    System.out.println("  name: " + lead.getName());
                    System.out.println("  description: " + lead.getDescription());
                    this.printCrxObject(lead);
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
    
                // Opportunities
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.Opportunity e WHERE e.segment = ?1 AND e.openmdxjdoIdentity LIKE 'opportunity/%'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.contract1.cci2.Opportunity> opportunities = q.getResultList();            
                for(org.opencrx.kernel.contract1.cci2.Opportunity opportunity : opportunities) {
                    System.out.println();
                    System.out.println("opportunity " + opportunity.getContractNumber());
                    System.out.println("  name: " + opportunity.getName());
                    System.out.println("  description: " + opportunity.getDescription());
                    this.printCrxObject(opportunity);
                    q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.AbstractOpportunityPosition e WHERE e.opportunity = ?1 AND e.openmdxjdoIdentity LIKE 'opportunityPos/%'");
                    q.setParameter(1, JDOHelper.getObjectId(opportunity));
                    List<org.opencrx.kernel.contract1.cci2.AbstractOpportunityPosition> positions = q.getResultList();            
                    for(org.opencrx.kernel.contract1.cci2.AbstractOpportunityPosition position: positions) {
                        this.printContractPosition(position);                            
                        this.printCrxObject(position);                    
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Quotes
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.Quote e WHERE e.segment = ?1 AND e.openmdxjdoIdentity LIKE 'quote/%'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.contract1.cci2.Quote> quotes = q.getResultList();            
                for(org.opencrx.kernel.contract1.cci2.Quote quote : quotes) {
                    System.out.println();
                    System.out.println("quote " + quote.getContractNumber());
                    System.out.println("  name: " + quote.getName());
                    System.out.println("  description: " + quote.getDescription());
                    this.printCrxObject(quote);
                    q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.AbstractQuotePosition e WHERE e.quote = ?1 AND e.openmdxjdoIdentity LIKE 'quotePos/%'");
                    q.setParameter(1, JDOHelper.getObjectId(quote));
                    List<org.opencrx.kernel.contract1.cci2.AbstractQuotePosition> positions = q.getResultList();            
                    for(org.opencrx.kernel.contract1.cci2.AbstractQuotePosition position: positions) {
                        this.printContractPosition(position);                            
                        this.printCrxObject(position);                    
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Sales orders
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.SalesOrder e WHERE e.segment = ?1 AND e.openmdxjdoIdentity LIKE 'salesOrder/%'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.contract1.cci2.SalesOrder> salesOrders = q.getResultList();            
                for(org.opencrx.kernel.contract1.cci2.SalesOrder salesOrder : salesOrders) {
                    System.out.println();
                    System.out.println("sales order " + salesOrder.getContractNumber());
                    System.out.println("  name: " + salesOrder.getName());
                    System.out.println("  description: " + salesOrder.getDescription());
                    this.printCrxObject(salesOrder);
                    q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.AbstractSalesOrderPosition e WHERE e.salesOrder = ?1 AND e.openmdxjdoIdentity LIKE 'salesOrderPos/%'");
                    q.setParameter(1, JDOHelper.getObjectId(salesOrder));
                    List<org.opencrx.kernel.contract1.cci2.AbstractSalesOrderPosition> positions = q.getResultList();            
                    for(org.opencrx.kernel.contract1.cci2.AbstractSalesOrderPosition position: positions) {
                        this.printContractPosition(position);                            
                        this.printCrxObject(position);                    
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
                // Invoices
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.Invoice e WHERE e.segment = ?1 AND e.openmdxjdoIdentity LIKE 'invoice/%'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.contract1.cci2.Invoice> invoices = q.getResultList();            
                for(org.opencrx.kernel.contract1.cci2.Invoice invoice : invoices) {
                    System.out.println();
                    System.out.println("invoice " + invoice.getContractNumber());
                    System.out.println("  name: " + invoice.getName());
                    System.out.println("  description: " + invoice.getDescription());
                    this.printCrxObject(invoice);
                    q = em.createQuery("SELECT e FROM org.opencrx.kernel.contract1.jpa3.AbstractInvoicePosition e WHERE e.invoice = ?1 AND e.openmdxjdoIdentity LIKE 'invoicePos/%'");
                    q.setParameter(1, JDOHelper.getObjectId(invoice));
                    List<org.opencrx.kernel.contract1.cci2.AbstractInvoicePosition> positions = q.getResultList();            
                    for(org.opencrx.kernel.contract1.cci2.AbstractInvoicePosition position: positions) {
                        this.printContractPosition(position);                            
                        this.printCrxObject(position);                    
                    }
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
                
            }
            finally {
                em.getTransaction().commit();
            }
        }
    }

    //-----------------------------------------------------------------------
    private void printProduct(
        org.opencrx.kernel.product1.cci2.AbstractProduct product
    ) {
        System.out.println();
        System.out.println("product " + product.getProductNumber());
        System.out.println("  name: " + product.getName());
        System.out.println("  description: " + product.getDescription());
        this.printCrxObject(product);      
        Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.product1.jpa3.ProductBasePrice e WHERE e.product = ?1");
        q.setParameter(1, JDOHelper.getObjectId(product));
        List<org.opencrx.kernel.product1.cci2.ProductBasePrice> basePrices = q.getResultList(); 
        int ii = 0;
        for(org.opencrx.kernel.product1.cci2.ProductBasePrice basePrice: basePrices) {
            System.out.println();
            System.out.println("  base price " + basePrice.getDescription());
            System.out.println("    price: " + basePrice.getPrice());
            System.out.println("    currency: " + basePrice.getPriceCurrency());
            this.printCrxObject(basePrice);
            ii++;
            if(ii > MAX_COUNT) break;
        }                
    }
    
    //-----------------------------------------------------------------------
    public void testProducts(
    ) throws Exception {
        if(RUN_TEST_PRODUCTS) {
            try {
                em.getTransaction().begin();   
                Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.product1.jpa3.Segment e WHERE e.openmdxjdoIdentity = 'products/CRX/Standard'");
                List<org.opencrx.kernel.product1.cci2.Segment> segments = q.getResultList();            
                org.opencrx.kernel.product1.cci2.Segment segment = segments.get(0);
                System.out.println("product1 segment");
                System.out.println("  description: " + segment.getDescription());
                this.printSecureObject(segment);
                
                // Products
                int ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.product1.jpa3.Product e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.product1.cci2.Product> products = q.getResultList();                        
                for(org.opencrx.kernel.product1.cci2.Product product: products) {
                    this.printProduct(product);
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
            }
            finally {
                em.getTransaction().commit();
            }
        }
    }

    //-----------------------------------------------------------------------
    public void testUserHomes(
    ) throws Exception {
        if(RUN_TEST_USERHOMES) {
            try {
                em.getTransaction().begin();   
                Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.home1.jpa3.Segment e WHERE e.openmdxjdoIdentity = 'homes/CRX/Standard'");
                List<org.opencrx.kernel.home1.cci2.Segment> segments = q.getResultList();            
                org.opencrx.kernel.home1.cci2.Segment segment = segments.get(0);
                System.out.println("home1 segment");
                System.out.println("  description: " + segment.getDescription());
                this.printSecureObject(segment);
                
                // UserHomes
                int ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.home1.jpa3.UserHome e WHERE e.segment = ?1 AND e.openmdxjdoIdentity = 'userHome/CRX/Standard/wfro'");
                q.setParameter(1, JDOHelper.getObjectId(segment));
                List<org.opencrx.kernel.home1.cci2.UserHome> userHomes = q.getResultList();                        
                for(org.opencrx.kernel.home1.cci2.UserHome userHome: userHomes) {
                    System.out.println("user home " + ((org.opencrx.kernel.home1.jpa3.UserHome)userHome).getContact_Id());
                    // Assigned Contracts
                    q = em.createQuery("SELECT OBJECT(c) FROM org.opencrx.kernel.contract1.jpa3.AbstractContract c, org.opencrx.kernel.home1.jpa3.UserHome h WHERE c MEMBER OF h.assignedContract AND h = ?1");
                    q.setParameter(1, userHome);
                    List<org.opencrx.kernel.contract1.cci2.AbstractContract> filteredContracts = q.getResultList();                            
                    int jj = 0;
                    for(org.opencrx.kernel.contract1.cci2.AbstractContract contract: filteredContracts) {
                        System.out.println("contract " + contract.getContractNumber());
                        System.out.println("  name: " + contract.getName());
                        System.out.println("  description: " + contract.getDescription());
                        jj++;
                        if(jj > MAX_COUNT) break;            
                    }                                
                    // Assigned Activities
                    q = em.createQuery("SELECT OBJECT(a) FROM org.opencrx.kernel.activity1.jpa3.Activity a, org.opencrx.kernel.home1.jpa3.UserHome h WHERE a MEMBER OF h.assignedActivity AND h = ?1");
                    q.setParameter(1, userHome);
                    List<org.opencrx.kernel.activity1.cci2.Activity> filteredActivities = q.getResultList();                            
                    jj = 0;
                    for(org.opencrx.kernel.activity1.cci2.Activity activity: filteredActivities) {
                        this.printActivity(activity);
                        jj++;
                        if(jj > MAX_COUNT) break;            
                    }                
                    ii++;
                    if(ii > MAX_COUNT) break;            
                }
            }
            finally {
                em.getTransaction().commit();
            }
        }
    }

    //-----------------------------------------------------------------------
    private void printBooking(
        org.opencrx.kernel.depot1.cci2.SingleBooking booking
    ) {
        System.out.println();
        System.out.println("booking " + booking.getName());
        System.out.println("  description: " + booking.getDescription());
        System.out.println("  booking date: " + booking.getBookingDate());
        this.printCrxObject(booking);        
    }
    
    //-----------------------------------------------------------------------
    public void testDepots(
    ) throws Exception {
        if(RUN_TEST_DEPOTS) {
            try {
                em.getTransaction().begin();   
                Query q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.Segment e WHERE e.openmdxjdoIdentity = 'depots/CRX/Standard'");
                List<org.opencrx.kernel.depot1.cci2.Segment> segments = q.getResultList();
                org.opencrx.kernel.depot1.cci2.Segment segment = segments.get(0);
                System.out.println("depot1 segment");
                System.out.println("  description: " + segment.getDescription());
                this.printSecureObject(segment);
                
                // DepotEntity
                int ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.DepotEntity e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));            
                List<org.opencrx.kernel.depot1.cci2.DepotEntity> depotEntities = q.getResultList();            
                for(org.opencrx.kernel.depot1.cci2.DepotEntity depotEntity : depotEntities) {
                    System.out.println();
                    System.out.println("depot entity " + depotEntity.getName());
                    System.out.println("  description: " + depotEntity.getDescription());
                    this.printCrxObject(depotEntity);
                    
                    // DepotGroup
                    int jj = 0;
                    q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.DepotGroup e WHERE e.entity = ?1");
                    q.setParameter(1, JDOHelper.getObjectId(depotEntity));            
                    List<org.opencrx.kernel.depot1.cci2.DepotGroup> depotGroups = q.getResultList();                            
                    for(org.opencrx.kernel.depot1.cci2.DepotGroup depotGroup : depotGroups) {
                        System.out.println();
                        System.out.println("  depotGroup " + depotGroup.getName());
                        System.out.println("  description: " + depotGroup.getDescription());
                        this.printCrxObject(depotGroup);
                        jj++;
                        if(jj > MAX_COUNT) break;
                    }
                    
                    // DepotHolder
                    jj = 0;
                    q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.DepotHolder e WHERE e.entity = ?1");
                    q.setParameter(1, JDOHelper.getObjectId(depotEntity));            
                    List<org.opencrx.kernel.depot1.cci2.DepotHolder> depotHolders = q.getResultList();                            
                    for(org.opencrx.kernel.depot1.cci2.DepotHolder depotHolder : depotHolders) {
                        System.out.println();
                        System.out.println("  depot holder " + depotHolder.getName());
                        System.out.println("  description: " + depotHolder.getDescription());
                        this.printCrxObject(depotHolder);
                        
                        int kk = 0;
                        q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.Depot e WHERE e.depotHolder = ?1");
                        q.setParameter(1, JDOHelper.getObjectId(depotHolder));            
                        List<org.opencrx.kernel.depot1.cci2.Depot> depots = q.getResultList();                            
                        for(org.opencrx.kernel.depot1.cci2.Depot depot : depots) {
                            System.out.println();
                            System.out.println("  depot " + depot.getDepotNumber());
                            System.out.println("  name: " + depot.getName());
                            System.out.println("  description: " + depot.getDescription());
                            System.out.println("  opening date: " + depot.getOpeningDate());
                            System.out.println("  closing date: " + depot.getClosingDate());
                            this.printCrxObject(depot);
                            kk++;
                            if(kk > MAX_COUNT) break;
                        }
                        
                        jj++;
                        if(jj > MAX_COUNT) break;
                    }
                    
                    ii++;
                    if(ii > MAX_COUNT) break;
                }
                    
                // ComboundBooking
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.CompoundBooking e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));            
                List<org.opencrx.kernel.depot1.cci2.CompoundBooking> compoundBookings = q.getResultList();                            
                for(org.opencrx.kernel.depot1.cci2.CompoundBooking cb : compoundBookings) {
                    System.out.println();
                    System.out.println("cb " + cb.getName());
                    System.out.println("  description: " + cb.getDescription());
                    System.out.println("  booking date: " + cb.getBookingDate());
                    System.out.println("  accepted by: " + cb.getAcceptedBy());
                    this.printCrxObject(cb);                
                    int jj = 0;
                    q = em.createQuery("SELECT OBJECT(b) FROM org.opencrx.kernel.depot1.jpa3.SingleBooking b, org.opencrx.kernel.depot1.jpa3.CompoundBooking cb WHERE b MEMBER OF cb.booking AND cb = ?1");
                    q.setParameter(1, cb);
                    List<org.opencrx.kernel.depot1.cci2.SingleBooking> bookings = q.getResultList();            
                    for(org.opencrx.kernel.depot1.cci2.SingleBooking booking: bookings) {
                        this.printBooking(booking);
                        jj++;
                        if(jj > MAX_COUNT) break;
                    }                
                    ii++;
                    if(ii > MAX_COUNT) break;
                }
                
                // Booking
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.SingleBooking e WHERE e.segment = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));            
                List<org.opencrx.kernel.depot1.cci2.SingleBooking> bookings = q.getResultList();                            
                for(org.opencrx.kernel.depot1.cci2.SingleBooking booking : bookings) {
                    this.printBooking(booking);
                    ii++;
                    if(ii > MAX_COUNT) break;
                }
                
                // DepotType
                ii = 0;
                q = em.createQuery("SELECT e FROM org.opencrx.kernel.depot1.jpa3.DepotType e WHERE e.depotEntity = ?1");
                q.setParameter(1, JDOHelper.getObjectId(segment));            
                List<org.opencrx.kernel.depot1.cci2.DepotType> depotTypes = q.getResultList();                                        
                for(org.opencrx.kernel.depot1.cci2.DepotType depotType : depotTypes) {
                    System.out.println();
                    System.out.println("depot type " + depotType.getName());
                    System.out.println("  description: " + depotType.getDescription());
                    this.printCrxObject(depotType);
                    ii++;
                    if(ii > MAX_COUNT) break;
                }            
                
            }
            finally {
                em.getTransaction().commit();
            }
        }
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final int MAX_COUNT = 5;
    
    private static final boolean RUN_TEST_ACCOUNTS = true;
    private static final boolean RUN_TEST_ACTIVITIES = true;
    private static final boolean RUN_TEST_CONTRACTS = true;
    private static final boolean RUN_TEST_DEPOTS = true;
    private static final boolean RUN_TEST_PRODUCTS = true;
    private static final boolean RUN_TEST_USERHOMES = true;
    
    private static EntityManager em = null;
}
