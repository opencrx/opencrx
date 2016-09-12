/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Accounts
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
package org.opencrx.kernel.backend;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.AccountAddressQuery;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.cci2.PhoneNumberQuery;
import org.opencrx.kernel.account1.cci2.PostalAddressQuery;
import org.opencrx.kernel.account1.cci2.RoomQuery;
import org.opencrx.kernel.account1.cci2.WebAddressQuery;
import org.opencrx.kernel.account1.jmi1.AbstractFilterAccount;
import org.opencrx.kernel.account1.jmi1.AbstractFilterAddress;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.CheckForAutoUpdateResult;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.account1.jmi1.Room;
import org.opencrx.kernel.account1.jmi1.WebAddress;
import org.opencrx.kernel.activity1.cci2.AddressGroupMemberQuery;
import org.opencrx.kernel.activity1.cci2.EMailQuery;
import org.opencrx.kernel.activity1.cci2.EMailRecipientQuery;
import org.opencrx.kernel.activity1.cci2.IncidentPartyQuery;
import org.opencrx.kernel.activity1.cci2.MeetingPartyQuery;
import org.opencrx.kernel.activity1.cci2.TaskPartyQuery;
import org.opencrx.kernel.activity1.jmi1.AddressGroupMember;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.EMailRecipient;
import org.opencrx.kernel.activity1.jmi1.IncidentParty;
import org.opencrx.kernel.activity1.jmi1.MeetingParty;
import org.opencrx.kernel.activity1.jmi1.TaskParty;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.Lead;
import org.opencrx.kernel.contract1.jmi1.Opportunity;
import org.opencrx.kernel.contract1.jmi1.Quote;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * Accounts backend class.
 *
 */
public class Accounts extends AbstractImpl {

	/**
	 * Register Accounts backend instance.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new Accounts());
	}
	
	/**
	 * Get Accounts backend instance.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Accounts getInstance(
	) throws ServiceException {
		return getInstance(Accounts.class);
	}

	/**
	 * Constructor.
	 */
	protected Accounts(
	) {		
	}
	
    /**
     * Touch account. This way jdoPreStore() will be invoked.
     * 
     * @param account
     * @throws ServiceException
     */
    public void markAccountAsDirty(
        Account account
    ) throws ServiceException {
    	account.setAccountRating(account.getAccountRating());
    	Utils.touchObject(account); // new way to touch object
    }

    /**
     * Init contract with given values.
     * 
     * @param contract
     * @param account
     * @param name
     * @param description
     * @throws ServiceException
     * @deprecated use contract creators instead.
     */
    public void initContract(
        SalesContract contract,
        Account account,
        String name,
        String description
    ) throws ServiceException {
        if(name != null) {
            contract.setName(name);
        }
        if(description != null) {
            contract.setDescription(description);
        }
        contract.setCustomer(account);
        contract.setActiveOn(
        	new Date()
        );
        Base.getInstance().assignToMe(
            contract,
            true, // overwrite
            false // useRunAsPrincipal
        );
    }

    /**
     * Create lead for given account.
     * 
     * @param account
     * @param name
     * @param description
     * @param nextStep
     * @param basedOn
     * @return
     * @throws ServiceException
     * @deprecated use contract creators instead
     */
    public Lead createLead(
        Account account,
        String name,
        String description,
        String nextStep,
        Lead basedOn
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	String providerName = account.refGetPath().get(2);
    	String segmentName = account.refGetPath().get(4);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(
        	pm, 
        	providerName, 
        	segmentName
        );
    	Lead contract = (Lead)Contracts.getInstance().createContract(
    		contractSegment, 
    		Contracts.CONTRACT_TYPE_LEAD, 
    		basedOn
    	);
        contract.setNextStep(nextStep);    	
        this.initContract(
            contract,
            account,
            name,
            description
        );
        return contract;
    }
    
    /**
     * Create opportunity for given account.
     * 
     * @param account
     * @param name
     * @param description
     * @param nextStep
     * @param basedOn
     * @return
     * @throws ServiceException
     * @deprecated use contract creators instead
     */
    public Opportunity createOpportunity(
        Account account,
        String name,
        String description,
        String nextStep,
        Opportunity basedOn
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	String providerName = account.refGetPath().get(2);
    	String segmentName = account.refGetPath().get(4);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(
        	pm, 
        	providerName, 
        	segmentName
        );
    	Opportunity contract = (Opportunity)Contracts.getInstance().createContract(
    		contractSegment, 
    		Contracts.CONTRACT_TYPE_OPPORTUNITY, 
    		basedOn
    	);
        contract.setNextStep(nextStep);    	
        this.initContract(
            contract,
            account,
            name,
            description
        );
        return contract;
    }
    
    /**
     * Create quote for given account.
     * 
     * @param account
     * @param name
     * @param description
     * @param basedOn
     * @return
     * @throws ServiceException
     * @deprecated use contract creators instead
     */
    public Quote createQuote(
        Account account,
        String name,
        String description,
        Quote basedOn
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	String providerName = account.refGetPath().get(2);
    	String segmentName = account.refGetPath().get(4);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(
        	pm, 
        	providerName, 
        	segmentName
        );
    	Quote contract = (Quote)Contracts.getInstance().createContract(
    		contractSegment, 
    		Contracts.CONTRACT_TYPE_QUOTE, 
    		basedOn
    	);
        this.initContract(
            contract,
            account,
            name,
            description
        );
        return contract;
    }
    
    /**
     * Create sales order for given account.
     * 
     * @param account
     * @param name
     * @param description
     * @param basedOn
     * @return
     * @throws ServiceException
     * @deprecated use contract creators instead
     */
    public SalesOrder createSalesOrder(
        Account account,
        String name,
        String description,
        SalesOrder basedOn
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	String providerName = account.refGetPath().get(2);
    	String segmentName = account.refGetPath().get(4);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(
        	pm, 
        	providerName, 
        	segmentName
        );
    	SalesOrder contract = (SalesOrder)Contracts.getInstance().createContract(
    		contractSegment, 
    		Contracts.CONTRACT_TYPE_SALES_ORDER, 
    		basedOn
    	);
        this.initContract(
            contract,
            account,
            name,
            description
        );
        return contract;
    }
    
    /**
     * Create invoice for given account.
     * 
     * @param account
     * @param name
     * @param description
     * @param basedOn
     * @return
     * @throws ServiceException
     * @deprecated use contract creators instead
     */
    public Invoice createInvoice(
        Account account,
        String name,
        String description,
        Invoice basedOn
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	String providerName = account.refGetPath().get(2);
    	String segmentName = account.refGetPath().get(4);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(
        	pm, 
        	providerName, 
        	segmentName
        );
    	Invoice contract = (Invoice)Contracts.getInstance().createContract(
    		contractSegment, 
    		Contracts.CONTRACT_TYPE_INVOICE, 
    		basedOn
    	);
        this.initContract(
            contract,
            account,
            name,
            description
        );
        return contract;
    }
    
    /**
     * Update derived attribute full name of given account.
     * 
     * @param account
     * @throws ServiceException
     */
    public void updateAccountFullName(
        Account account
    ) throws ServiceException {
        if(account instanceof Contact) {
        	Contact contact = (Contact)account;
            contact.setFullName(
                ( (contact.getLastName() == null ? "" : contact.getLastName()) + ", "
                  + (contact.getFirstName() == null ? "" : contact.getFirstName() + " ")
                  + (contact.getMiddleName() == null ? "" : contact.getMiddleName() + "") ).trim()
            );
        } else if(account instanceof AbstractGroup) {
        	AbstractGroup group = (AbstractGroup)account;
            group.setFullName(
                group.getName() == null ? "" : group.getName()
            );
        }
    }

    /**
     * Update postal address lines which are derived from account information.
     * Override this method for custom-specific behavior. The default implementation
     * updates address lines of the form [salutation, firstName " " lastName] and
     * [firstName " " lastName].
     * 
     * @param account
     * @throws ServiceException
     */
    public void updatePostalAddressLine(
    	Account account
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	try {
    		if(account instanceof Contact) {
    			Contact contact = (Contact)account;
	    		// Get currently persistent account (old account)
	        	PersistenceManager pmOld = pm.getPersistenceManagerFactory().getPersistenceManager(
	        		SecurityKeys.ROOT_PRINCIPAL,
	        		null
	        	);
	        	Contact contactOld = (Contact)pmOld.getObjectById(contact.refGetPath());
	    		PostalAddressQuery query = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
	    		query.forAllDisabled().isFalse();
	    		for(PostalAddress postalAddress: contact.<PostalAddress>getAddress(query)) {
	    			if(
	    				postalAddress.getPostalAddressLine().size() == 1 &&
	    				Utils.areEqual(postalAddress.getPostalAddressLine().get(1), contactOld.getFirstName() + " " + contactOld.getLastName())
	    			) {
	    				List<String> postalAddressLines = new ArrayList<String>();
	    				postalAddressLines.add(contact.getFirstName() + " " + contact.getLastName());
	    				if(!Utils.areEqual(postalAddress.getPostalAddressLine(), postalAddressLines)) {
		    				postalAddress.getPostalAddressLine().clear();
		    				postalAddress.getPostalAddressLine().addAll(postalAddressLines);
	    				}
	    			} else if(
	    				(postalAddress.getPostalAddressLine().size() == 2 &&
	    				Utils.areEqual(postalAddress.getPostalAddressLine().get(0), contactOld.getSalutation()) &&
	    				Utils.areEqual(postalAddress.getPostalAddressLine().get(1), contactOld.getFirstName() + " " + contactOld.getLastName()))
	    			) {
	    				List<String> postalAddressLines = new ArrayList<String>();
	    				if(contact.getSalutation() != null && !contact.getSalutation().isEmpty()) {
	    					postalAddressLines.add(contact.getSalutation());
	    				}
	    				postalAddressLines.add(contact.getFirstName() + " " + contact.getLastName());
	    				if(!Utils.areEqual(postalAddress.getPostalAddressLine(), postalAddressLines)) {
		    				postalAddress.getPostalAddressLine().clear();
		    				postalAddress.getPostalAddressLine().addAll(postalAddressLines);
	    				}
	    			}
	    		}
    		}
		} catch(Exception ignore) {}        	
    }

    /**
     * Update the addresses assigned to the given address. The default
     * implementation amends matching postal addresses. Override this
     * method for custom-specific behavior.
     * 
     * @param address
     * @throws ServiceException
     */
    public void updateAssignedAddresses(
    	AccountAddress address
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(address);
    	if(address instanceof PostalAddress) {
    		PostalAddress postalAddress = (PostalAddress)address;
        	try {
        		// Get currently persistent address (old address)
            	PersistenceManager pmOld = pm.getPersistenceManagerFactory().getPersistenceManager(
            		SecurityKeys.ROOT_PRINCIPAL,
            		null
            	);
	        	PostalAddress postalAddressOld = (PostalAddress)pmOld.getObjectById(postalAddress.refGetPath());
	        	// Optimize: only amend assigned addresses if address was changed
	        	if(
    				!Utils.areEqual(postalAddressOld.getPostalAddressLine(), postalAddress.getPostalAddressLine()) ||
    				!Utils.areEqual(postalAddressOld.getPostalStreet(), postalAddress.getPostalStreet()) ||
    				!Utils.areEqual(postalAddressOld.getPostalStreetNumber(), postalAddress.getPostalStreetNumber()) ||
    				!Utils.areEqual(postalAddressOld.getPostalCode(), postalAddress.getPostalCode()) ||
    				!Utils.areEqual(postalAddressOld.getPostalCity(), postalAddress.getPostalCity()) ||
    				!Utils.areEqual(postalAddressOld.getPostalState(), postalAddress.getPostalState()) || 
    				(postalAddressOld.getPostalCountry() != postalAddress.getPostalCountry()) 
	        	) {
		    		Account account = (Account)pm.getObjectById(address.refGetPath().getParent().getParent());
		    		PostalAddressQuery query = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
		    		query.forAllDisabled().isFalse();
		    		for(PostalAddress assignedAddress: account.<PostalAddress>getAssignedAddress(query)) {
		    			// Update assigned address only if it matches the old address
		    			if(
		    				Utils.areEqual(postalAddressOld.getPostalStreet(), assignedAddress.getPostalStreet()) &&
		    				Utils.areEqual(postalAddressOld.getPostalStreetNumber(), assignedAddress.getPostalStreetNumber()) &&
		    				Utils.areEqual(postalAddressOld.getPostalCode(), assignedAddress.getPostalCode()) &&
		    				Utils.areEqual(postalAddressOld.getPostalCity(), assignedAddress.getPostalCity()) &&
		    				Utils.areEqual(postalAddressOld.getPostalState(), assignedAddress.getPostalState()) && 
		    				(postalAddressOld.getPostalCountry() == assignedAddress.getPostalCountry()) 
		    			) {
		    				assignedAddress.getPostalStreet().clear();
		    				assignedAddress.getPostalStreet().addAll(postalAddress.getPostalStreet());
		    				assignedAddress.setPostalStreetNumber(postalAddress.getPostalStreetNumber());
		    				assignedAddress.setPostalCode(postalAddress.getPostalCode());
		    				assignedAddress.setPostalCity(postalAddress.getPostalCity());
		    				assignedAddress.setPostalState(postalAddress.getPostalState());
		    				assignedAddress.setPostalCountry(postalAddress.getPostalCountry());	    				
		    			}
		    		}
        		}
        	} catch(Exception ignore) {}
    	} else if(address instanceof EMailAddress) {
    		EMailAddress emailAddress = (EMailAddress)address;
        	try {
        		// Get currently persistent address (old address)
            	PersistenceManager pmOld = pm.getPersistenceManagerFactory().getPersistenceManager(
            		SecurityKeys.ROOT_PRINCIPAL,
            		null
            	);
	        	EMailAddress emailAddressOld = (EMailAddress)pmOld.getObjectById(emailAddress.refGetPath());
	        	// Only update, if old and new are different
	        	String emailAddressDomainOld = emailAddressOld.getEmailAddress() != null && emailAddressOld.getEmailAddress().indexOf("@") > 0
					? emailAddressOld.getEmailAddress().substring(emailAddressOld.getEmailAddress().indexOf("@"))
					: null;	        	
	        	String emailAddressDomain = emailAddress.getEmailAddress() != null && emailAddress.getEmailAddress().indexOf("@") > 0
					? emailAddress.getEmailAddress().substring(emailAddress.getEmailAddress().indexOf("@"))
					: null;        	
	        	if(
	        		!Utils.areEqual(emailAddressDomainOld, emailAddressDomain)
	        	) {
		    		Account account = (Account)pm.getObjectById(address.refGetPath().getParent().getParent());
		    		EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
		    		query.forAllDisabled().isFalse();
		    		for(EMailAddress assignedAddress: account.<EMailAddress>getAssignedAddress(query)) {
		    			// Update assigned address only if it matches the old address
			        	String emailAddressDomainAssigned = assignedAddress.getEmailAddress() != null && assignedAddress.getEmailAddress().indexOf("@") > 0
							? assignedAddress.getEmailAddress().substring(assignedAddress.getEmailAddress().indexOf("@"))
							: null;	        	
		    			if(
		    				Utils.areEqual(emailAddressDomainOld, emailAddressDomainAssigned)
		    			) {
		    				assignedAddress.setEmailAddress(
		    					assignedAddress.getEmailAddress().replace(emailAddressDomainAssigned, emailAddressDomain)
		    				);
		    			}
		    		}
        		}
        	} catch(Exception ignore) {}
    	}
    }

    /**
     * Update derived attributes of given account. E.g. invoked by jdoPreStore().
     * 
     * @param account
     * @throws ServiceException
     */
    protected void updateAccount(
        Account account
    ) throws ServiceException {
        this.updateAccountFullName(account);
        this.updatePostalAddressLine(account);
        List<String> statusMessage = new ArrayList<String>();
        String oldVCardAsString = account.getVcard();
        String newVCardAsString = VCard.getInstance().mergeVcard(
            account,
            oldVCardAsString,
            statusMessage
        );
        if(newVCardAsString == null) {
        	newVCardAsString = "";
        }
        // Compare old and new VCARD. Only update if a field other than REV changes
        try {
	        Map<String,VCard.VCardField> oldFields = VCard.getInstance().parseVCard(
	        	new BufferedReader(new StringReader(oldVCardAsString == null ? "" : oldVCardAsString)), new StringBuilder()
	        );
	        Map<String,VCard.VCardField> newFields = VCard.getInstance().parseVCard(
	        	new BufferedReader(new StringReader(newVCardAsString)), new StringBuilder()
	        );
	        oldFields.remove("REV");
	        newFields.remove("REV");
	        if(!newFields.equals(oldFields)) {
		        account.setVcard(newVCardAsString);
	        }
        } catch(Exception e) {
        	throw new ServiceException(e);
        }
        // Assertion externalLink().contains(vcard uid)
        String uid = VCard.getInstance().getVCardUid(newVCardAsString);
        if(uid == null) {
        	ServiceException e = new ServiceException(
        		BasicException.Code.DEFAULT_DOMAIN,
        		BasicException.Code.ASSERTION_FAILURE,
        		"Missing UID in accounts's vcard",
        		new BasicException.Parameter("activity", account),
        		new BasicException.Parameter("externalLink", account.getExternalLink()),
        		new BasicException.Parameter("vcard", newVCardAsString)
        	);
        	SysLog.warning("Missing UID in accounts's vcard", account.refGetPath());
        	SysLog.detail(e.getMessage(), e.getCause());        	
        }
        boolean vcardLinkMatches = false;
        boolean hasVCardLink = false;
        for(String externalLink: account.getExternalLink()) {
        	if(externalLink.startsWith(VCard.VCARD_SCHEMA)) {
        		hasVCardLink = true;
        		if(externalLink.endsWith(uid)) {
	        		vcardLinkMatches = true;
	        		break;
        		}
        	}
        }
        if(!hasVCardLink) {
        	account.getExternalLink().add(
        		VCard.VCARD_SCHEMA + uid
        	);
        } else if(!vcardLinkMatches) {
        	ServiceException e = new ServiceException(
        		BasicException.Code.DEFAULT_DOMAIN,
        		BasicException.Code.ASSERTION_FAILURE,
        		"Accounts's external link does not contain vcard UID",
        		new BasicException.Parameter("activity", account),
        		new BasicException.Parameter("externalLink", account.getExternalLink()),
        		new BasicException.Parameter("vcard", newVCardAsString)
        	);
        	SysLog.warning("Accounts's external link does not contain vcard UID", account.refGetPath());
        	SysLog.detail(e.getMessage(), e.getCause());
        }
    }

    /**
     * Update derived attributes of given address.
     * 
     * @param address
     * @throws ServiceException
     */
    protected void updateAddress(
    	AccountAddress address
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(address);
    	// Parse phone numbers
    	if(address instanceof PhoneNumber) {
    		Addresses.getInstance().updatePhoneNumber(
    			(PhoneNumber)address 
    		);    		
    	}
    	// Amend assigned addresses
    	this.updateAssignedAddresses(address);
		// Mark account as dirty updates VCard, ...
		this.markAccountAsDirty(
			(Account)pm.getObjectById(
				address.refGetPath().getParent().getParent()
			)
		);
    }

    /**
     * Callback when address is removed, e.g. jdoPreDelete.
     * 
     * @param address
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeAddress(
    	AccountAddress address,
    	boolean preDelete
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(address);
		// Mark account as dirty updates VCard, ...
		this.markAccountAsDirty(
			(Account)pm.getObjectById(
				address.refGetPath().getParent().getParent()
			)
		);
    }
    
    /**
     * Count accounts matching the account filter.
     * 
     * @param accountFilter
     * @return
     * @throws ServiceException
     */
    public int countFilteredAccount(
    	AbstractFilterAccount accountFilter
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountFilter);
        AccountQuery query = (AccountQuery)pm.newQuery(Account.class);
        QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);    	
    	List<Account> accounts = accountFilter.getFilteredAccount(query);
        return accounts.size();
    }

    /**
     * Get account segment.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.opencrx.kernel.account1.jmi1.Segment getAccountSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.account1.jmi1.Segment) pm.getObjectById(
            new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", providerName, "segment", segmentName)
        );
    }

    /**
     * Find email addresses matching the given pattern.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param emailAddress
     * @return
     */
    public List<EMailAddress> lookupEmailAddress(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        String emailAddress
    ) {
    	return this.lookupEmailAddress(
    		pm, 
    		providerName, 
    		segmentName, 
    		emailAddress, 
    		false // exactCaseInsensitiveOnly
    	);
    }

    /**
     * Find email addresses matching the given pattern.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param emailAddress
     * @param exactCaseInsensitiveOnly
     * @return
     */
    public List<EMailAddress> lookupEmailAddress(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        String emailAddress,
        boolean exactCaseInsensitiveOnly
    ) {
        org.opencrx.kernel.account1.jmi1.Segment accountSegment =
            this.getAccountSegment(
                pm,
                providerName,
                segmentName
            );
        emailAddress = emailAddress.trim();
        List<EMailAddress> addresses = Collections.emptyList();
        // Phase 1: exact match, case-sensitive
        {
            EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
            query.orderByModifiedAt().descending();
            query.thereExistsEmailAddress().equalTo(emailAddress);
           	query.forAllDisabled().isFalse();
           	addresses = accountSegment.getAddress(query);
        }
        // Phase 2: exact match, case-insensitive
        if(addresses.isEmpty()) { 
            EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
            query.orderByModifiedAt().descending();
            query.thereExistsEmailAddress().like(
               "(?i)" + emailAddress.replace(".", "\\.") 
            );
           	query.forAllDisabled().isFalse();
           	addresses = accountSegment.getAddress(query);
        }
        // Phase 3: Search email address with wildcard pattern
        if(!exactCaseInsensitiveOnly) {
	        if(addresses.isEmpty()) { 
		        EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
		        query.orderByModifiedAt().descending();
		        query.thereExistsEmailAddress().like(
		           "(?i).*" + emailAddress.replace(".", "\\.") + ".*" 
		        );
		       	query.forAllDisabled().isFalse();
		       	addresses = accountSegment.getAddress(query);
	        }
        }
        // Active addresses ordered by length. The best match should
        // be the first element in the returned list
   		List<EMailAddress> activeAddresses = new ArrayList<EMailAddress>();
   		for(EMailAddress address: addresses) {
   			Account account = address.getAccount();
   			if(account.isDisabled() == null || !account.isDisabled()) {
   				int index = 0;
   				while(index < activeAddresses.size()) {
   					if(address.getEmailAddress().length() < activeAddresses.get(index).getEmailAddress().length()) {
   						break;
   					}
   					index++;
   				}
   				activeAddresses.add(index, address);
   			}
   		}
   		return activeAddresses;
    }

    /**
     * Get holder for new addresses which can not be automatically be assigned to an account.
     * 
     * @param accountSegment
     * @return
     * @throws ServiceException
     */
    public Account getUnassignableAddressesHolder(
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
    	String providerName = accountSegment.refGetPath().get(2);
    	String segmentName = accountSegment.refGetPath().get(4);
    	return (Account)pm.getObjectById(
    		new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", providerName, "segment", segmentName, "account", (SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName))
    	);
    }

    /**
     * Find email addresses matching the given pattern. Create the address if it does not exist.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param email
     * @param exactCaseInsensitiveOnly
     * @param forceCreate
     * @return
     * @throws ServiceException
     */
    public List<EMailAddress> lookupEmailAddress(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        String email,
        boolean exactCaseInsensitiveOnly,
        boolean forceCreate
    ) throws ServiceException {
    	List<EMailAddress> emailAddresses = this.lookupEmailAddress(
    		pm, 
    		providerName, 
    		segmentName, 
    		email,
    		exactCaseInsensitiveOnly
    	);
    	if(forceCreate && emailAddresses.isEmpty()) {
    		PersistenceManager pmAdmin = pm.getPersistenceManagerFactory().getPersistenceManager(
    			SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName,
    			null
    		);
    		org.opencrx.kernel.account1.jmi1.Segment accountSegment = this.getAccountSegment(pmAdmin, providerName, segmentName);
        	Account addressHolder = this.getUnassignableAddressesHolder(accountSegment);
        	if(addressHolder != null) {
            	EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
            	query.thereExistsEmailAddress().equalTo(email);
            	List<EMailAddress> existingAddresses = addressHolder.getAddress(query);
            	if(existingAddresses.isEmpty()) {
	        		pmAdmin.currentTransaction().begin();
	        		EMailAddress emailAddress = pmAdmin.newInstance(EMailAddress.class);
	        		emailAddress.setEmailAddress(email);
	        		emailAddress.setEmailType((short)1);
	        		addressHolder.addAddress(
	        			this.getUidAsString(),
	        			emailAddress
	        		);
	        		pmAdmin.currentTransaction().commit();
	        		emailAddresses = Collections.singletonList(
	        			(EMailAddress)pm.getObjectById(emailAddress.refGetPath())
	        		);
            	} else {
            		emailAddresses = Collections.singletonList(
	        			(EMailAddress)pm.getObjectById(existingAddresses.iterator().next().refGetPath())
	        		);
            	}
        	}
        	pmAdmin.close();
    	}
    	return emailAddresses;
    }

    /**
     * Get addresses of given account.
     * 
     * @param account
     * @param usage
     * @return
     */
    public List<AccountAddress> getAccountAddresses(
        Account account,
        short usage
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
        AccountAddressQuery query = (AccountAddressQuery)pm.newQuery(AccountAddress.class); 
        query.thereExistsUsage().equalTo(usage);
        query.forAllDisabled().isFalse();
        query.orderByCreatedAt().ascending();
        return account.getAddress(query); 
    }

    /**
     * Get primary business email address.
     * 
     * @param account
     * @param hint
     * @return
     * @throws ServiceException
     */
    public String getPrimaryBusinessEMail(
        Account account,
        String hint
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
        String emailAddress = null;
    	for(boolean ignoreDisabled: Arrays.asList(true, false)) {
    		EMailAddressQuery emailAddressQuery = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
    		if(ignoreDisabled) {
    			emailAddressQuery.forAllDisabled().isFalse();
    		}
	        for(EMailAddress address: account.<EMailAddress>getAddress(emailAddressQuery)) {
                String addr = ((EMailAddress)address).getEmailAddress();
                if(emailAddress == null || (hint != null && !hint.isEmpty())) {
                	if(emailAddress == null) {
                		emailAddress = addr;                		
                	} else {
                		if(hint.equals(addr)) {
                			emailAddress = addr;
                		}
                	}
                }
                if(addr != null && address.isMain() && address.getUsage().contains(Addresses.USAGE_BUSINESS)) {
                    emailAddress = addr;
                }
	        }
	        if(emailAddress != null) {
	        	return emailAddress;
	        }
    	}
        return emailAddress;
    }

    /**
     * Get primary business phone of given account.
     * 
     * @param account
     * @return
     * @throws ServiceException
     */
    public String getPrimaryBusinessPhone(
        Account account
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
        String phoneNumber = null;
    	for(boolean ignoreDisabled: Arrays.asList(true, false)) {
    		PhoneNumberQuery phoneNumberQuery = (PhoneNumberQuery)pm.newQuery(PhoneNumber.class);
    		if(ignoreDisabled) {
    			phoneNumberQuery.forAllDisabled().isFalse();
    		}
	        for(PhoneNumber address: account.<PhoneNumber>getAddress(phoneNumberQuery)) {
                String adr = ((PhoneNumber)address).getPhoneNumberFull();
                if((phoneNumber == null) && (adr != null)) {
                    phoneNumber = adr;
                }
                if(adr != null && address.isMain() && address.getUsage().contains(Addresses.USAGE_BUSINESS)) {
                    phoneNumber = adr;
                }
	        }
	        if(phoneNumber != null) {
	        	return phoneNumber;
	        }
    	}
        return phoneNumber;
    }

    /**
     * Same as getMainAddresses(account, false)
     * 
     * @param account
     * @return main addresses of account
     */
    public AccountAddress[] getMainAddresses(
        Account account
    ) {
    	return this.getMainAddresses(account, false);
    }
    
    /**
     * Return main home and business addresses of given account. If strict is
     * true then address.isMain() is true for all returned addresses. If strict
     * is false then non-main addresses are returned in case a main address with
     * the same type and usage does exist.
     * 
     * @param account
     * @param strict
     * @return array with elements {web home, web business, phone home, phone business, 
     *         fax home, fax, business, postal home, postal business,
     *         mail home, mail business, mobile, phone other, mail other} 
     */
    public org.opencrx.kernel.account1.jmi1.AccountAddress[] getMainAddresses(
        Account account,
        boolean strict
    ) {
    	return this.getAccountAddresses(
    		account, 
    		new AddressFilter(){
    			@Override
    			public boolean matches(
    				AccountAddress address
    			) {
    				boolean isMain = false;
    				try {
    					isMain = address.isMain();
    				} catch(Exception e) {}
    				return isMain;
    			}
			},
			strict
    	);
    }

    /**
     * Callback interface for filtering addresses.
     *
     */
    public interface AddressFilter {
    	
    	boolean matches(AccountAddress address);
    	
    }
    
    /**
     * Return home and business addresses of given account matching the specified address filter.
     * If strict is true then address.isMain() is true for all returned addresses. If strict
     * is false then non-main addresses are returned in case a main address with
     * the same type and usage does exist.
     * 
     * @param account
     * @param addressFilter
     * @param strict
     * @return array with elements {web home, web business, phone home, phone business, 
     *         fax home, fax, business, postal home, postal business,
     *         mail home, mail business, mobile, phone other, mail other} 
     */
    public AccountAddress[] getAccountAddresses(
        Account account,
        AddressFilter addressFilter,
        boolean strict
    ) {
        boolean hasBusinessMail = false;
        boolean hasHomeMail = false;
        boolean hasBusinessPhone = false;
        boolean hasHomePhone = false;
        boolean hasBusinessFax = false;
        boolean hasOrderHomeFax = false;
        boolean hasBusinessPostal = false;
        boolean hasBusinessWeb = false;
        boolean hasHomePostal = false;
        boolean hasHomeWeb = false;
        boolean hasMobile = false;
        boolean hasOtherPhone = false;
        boolean hasOtherMail = false;

        AccountAddress[] accountAddresses = new AccountAddress[13];
        // Performance: retrieve and cache addresses
        account.getAddress().isEmpty(); 
        // HOME
        List<AccountAddress> addresses = this.getAccountAddresses(
            account,
            Addresses.USAGE_HOME
        );
        for(AccountAddress address: addresses) {
            if(address instanceof WebAddress) {
                WebAddress webAddress = (WebAddress)address;
                boolean matchesFilter = addressFilter.matches(webAddress);
                if((!strict && !hasHomeWeb) || matchesFilter) {
                    accountAddresses[WEB_HOME] = webAddress;
                    hasHomeWeb = true;
                }
            }
            else if(address instanceof PhoneNumber) {
                PhoneNumber phoneNumber = (PhoneNumber)address;
                boolean matchesFilter = addressFilter.matches(phoneNumber);
                if((!strict && !hasHomePhone) || matchesFilter) {
                    accountAddresses[PHONE_HOME] = phoneNumber;
                    hasHomePhone = true;
                }
            }
            else if (address instanceof PostalAddress) {
                PostalAddress postalAddress = (PostalAddress)address;
                boolean matchesFilter = addressFilter.matches(postalAddress);
                if((!strict && !hasHomePostal) || matchesFilter) {       
                    accountAddresses[POSTAL_HOME] = postalAddress;
                    hasHomePostal = true;
                }
            }
            else if(address instanceof EMailAddress) {
                EMailAddress mailAddress = (EMailAddress)address;
                boolean matchesFilter = addressFilter.matches(mailAddress);
                if((!strict && !hasHomeMail) || matchesFilter) {
                    accountAddresses[MAIL_HOME] = mailAddress;
                    hasHomeMail = true;
                }
            }
        }    
        // BUSINESS
        addresses = this.getAccountAddresses(
            account,
            Addresses.USAGE_BUSINESS
        );
        for(AccountAddress address: addresses) {
            if(address instanceof WebAddress) {
                WebAddress webAddress = (WebAddress)address;
                boolean matchesFilter = addressFilter.matches(webAddress);
                if((!strict && !hasBusinessWeb) || matchesFilter) {
                    accountAddresses[WEB_BUSINESS] = webAddress;
                    hasBusinessWeb = true;
                }
            }
            else if(address instanceof PhoneNumber) {
                PhoneNumber phoneNumber = (PhoneNumber)address;
                boolean matchesFilter = addressFilter.matches(phoneNumber);
                if((!strict && !hasBusinessPhone) || matchesFilter) {
                    accountAddresses[PHONE_BUSINESS] = phoneNumber;
                    hasBusinessPhone = true;
                }
            }
            else if (address instanceof PostalAddress) {
                PostalAddress postalAddress = (PostalAddress)address;
                boolean matchesFilter = addressFilter.matches(postalAddress);
                if((!strict && !hasBusinessPostal) || matchesFilter) {
                    accountAddresses[POSTAL_BUSINESS] = postalAddress;
                    hasBusinessPostal = true;
                }
            }
            else if(address instanceof EMailAddress) {
                EMailAddress mailAddress = (EMailAddress)address;
                boolean matchesFilter = addressFilter.matches(mailAddress);
                if((!strict && !hasBusinessMail) || matchesFilter) {
                    accountAddresses[MAIL_BUSINESS] = mailAddress;
                    hasBusinessMail = true;
                }
            }
        }    
        // OTHER
        addresses = this.getAccountAddresses(
            account,
            Addresses.USAGE_OTHER
        );
        for(AccountAddress address: addresses) {
            if(address instanceof PhoneNumber) {
                PhoneNumber phoneNumber = (PhoneNumber)address;                
                boolean matchesFilter = addressFilter.matches(phoneNumber);
                if((!strict && !hasOtherPhone) || matchesFilter) {
                    accountAddresses[PHONE_OTHER] = phoneNumber;
                    hasOtherPhone = true;
                }
            }
            else if(address instanceof EMailAddress) {
            	EMailAddress mailAddress = (EMailAddress)address;                
            	boolean matchesFilter = addressFilter.matches(mailAddress);
                if((!strict && !hasOtherMail) || matchesFilter) {
                    accountAddresses[MAIL_OTHER] = mailAddress;
                    hasOtherMail = true;
                }
            }
        }    
        // HOME_FAX
        addresses = this.getAccountAddresses(
            account,
            Addresses.USAGE_HOME_FAX
        );
        for(AccountAddress address: addresses) {
            if(address instanceof PhoneNumber) {
                PhoneNumber phoneNumber = (PhoneNumber)address;                
                boolean matchesFilter = addressFilter.matches(phoneNumber);
                if((!strict && !hasOrderHomeFax) || matchesFilter) {           
                    accountAddresses[FAX_HOME] = phoneNumber;
                    hasOrderHomeFax = true;
                }
            }
        }    
        // BUSINESS_FAX
        addresses = this.getAccountAddresses(
            account,
            Addresses.USAGE_BUSINESS_FAX
        );
        for(AccountAddress address: addresses) {
            if(address instanceof PhoneNumber) {
                PhoneNumber phoneNumber = (PhoneNumber)address;                                
                boolean matchesFilter = addressFilter.matches(phoneNumber);
                if((!strict && !hasBusinessFax) || matchesFilter) {
                    accountAddresses[FAX_BUSINESS] = phoneNumber;
                    hasBusinessFax = true;
                }
            }
        }    
        // MOBILE
        addresses = this.getAccountAddresses(
            account,
            Addresses.USAGE_MOBILE
        );
        for(AccountAddress address: addresses) {
            if(address instanceof PhoneNumber) {
                PhoneNumber phoneNumber = (PhoneNumber)address;     
                boolean matchesFilter = addressFilter.matches(phoneNumber);
                if((!strict && !hasMobile) || matchesFilter) { 
                    accountAddresses[MOBILE] = phoneNumber;
                    hasMobile = true;
                }
            }
        }
        return accountAddresses;
    }

    /**
     * Update VCARD of given account.
     * 
     * @param account
     * @throws ServiceException
     */
    public void updateVcard(
        Account account
    ) throws ServiceException {
        List<String> messages = new ArrayList<String>();
        List<String> errors = new ArrayList<String>();
        List<String> report = new ArrayList<String>();
        String vcard = VCard.getInstance().mergeVcard(
            account,
            account.getVcard(), 
            messages
        );        
        byte[] item = null;
        try {
            item = vcard.getBytes("UTF-8");
        } 
        catch(Exception e) {
            item = vcard.getBytes();    
        }
        VCard.getInstance().importItem(
            item, 
            account, 
            (short)0, 
            errors, 
            report
        );
    }
    
    /**
     * Update member callback. E.g. invoked by jdoPreDelete().
     * 
     * @param member
     * @throws ServiceException
     */
    protected void updateMember(
    	Member member
    ) throws ServiceException {    	
    }
    
    /**
     * Count addresses matching the given filter.
     * 
     * @param addressFilter
     * @return
     * @throws ServiceException
     */
    public int countFilteredAddress(
    	AbstractFilterAddress addressFilter
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(addressFilter);
    	AccountAddressQuery query = (AccountAddressQuery)pm.newQuery(AccountAddress.class);
    	QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);    	
    	List<AccountAddress> addresses = addressFilter.getFilteredAddress(query);
        return addresses.size();
    }
        
    /**
     * Move address to target account. The new address is created and cloned
     * on the target account. The existing address is set to disabled. In
     * addition replaces all references to the old addresses with the new address. 
     * 
     * @param source
     * @param targetAccount
     * @param updateRelationshipsSince
     * @param updateRelationshipsBefore
     * @return
     * @throws ServiceException
     */
    public int moveAddressToAccount(
    	AccountAddress source,
        Account targetAccount,
        Date updateRelationshipsSince,
        Date updateRelationshipsBefore        
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(source);    	
    	// Clone address
    	AccountAddress target = null;
    	// Try to find address on target account. Clone it if it does not exist.
    	if(source instanceof EMailAddress) {
    		EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
    		query.thereExistsEmailAddress().equalTo(((EMailAddress)source).getEmailAddress());
    		List<EMailAddress> addresses = targetAccount.getAddress(query);
    		target = addresses.isEmpty() ? null : addresses.iterator().next();
    	} else if(source instanceof PhoneNumber) {
    		PhoneNumberQuery query = (PhoneNumberQuery)pm.newQuery(PhoneNumber.class);
    		query.thereExistsPhoneNumberFull().equalTo(((PhoneNumber)source).getPhoneNumberFull());
    		List<PhoneNumber> addresses = targetAccount.getAddress(query);
    		target = addresses.isEmpty() ? null : addresses.iterator().next();
    	} else if(source instanceof WebAddress) {
    		WebAddressQuery query = (WebAddressQuery)pm.newQuery(WebAddress.class);
    		query.thereExistsWebUrl().equalTo(((WebAddress)source).getWebUrl());
    		List<WebAddress> addresses = targetAccount.getAddress(query);
    		target = addresses.isEmpty() ? null : addresses.iterator().next();    	
    	} else if(source instanceof Room) {
    		RoomQuery query = (RoomQuery)pm.newQuery(Room.class);
    		query.thereExistsRoomNumber().equalTo(((Room)source).getRoomNumber());
    		List<Room> addresses = targetAccount.getAddress(query);
    		target = addresses.isEmpty() ? null : addresses.iterator().next();    		
    	}
    	if(target == null) {
	    	target = (AccountAddress)Cloneable.getInstance().cloneObject(
	    		source, 
	    		targetAccount, 
	    		source.refGetPath().getParent().getBase(), 
	    		null, // objectMarshallers
	    		"", // referenceFilterAsString
	    		targetAccount.getOwningUser(), 
	    		targetAccount.getOwningGroup()
	    	);
    	}
    	return this.moveAddress(
    		source, 
    		target, 
    		updateRelationshipsSince, 
    		updateRelationshipsBefore
    	);
    }
    
    /**
     * Replace all references to the source address with the target address.
     * 
     * @param source
     * @param target
     * @param updateRelationshipsSince
     * @param updateRelationshipsBefore
     * @return
     * @throws ServiceException
     */
    public int moveAddress(
    	AccountAddress source,
        AccountAddress target,
        Date updateRelationshipsSince,
        Date updateRelationshipsBefore        
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(source);
    	String providerName = source.refGetPath().get(2);
    	String segmentName = source.refGetPath().get(4);
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, providerName, segmentName);
    	Account sourceAccount = source.getAccount();
    	Account targetAccount = target.getAccount();
    	int count = 0;
    	// Update activity1::AddressGroupMember::address
    	{
	    	AddressGroupMemberQuery query = (AddressGroupMemberQuery)PersistenceHelper.newQuery(
	    		pm.getExtent(AddressGroupMember.class),
	    		activitySegment.refGetPath().getDescendant("addressGroup", ":*", "member", ":*")
	    	);
	    	query.thereExistsAddress().equalTo(source);
	    	if(updateRelationshipsSince != null) {
	    		query.createdAt().greaterThanOrEqualTo(updateRelationshipsSince);
	    	}
	    	if(updateRelationshipsBefore != null) {
	    		query.createdAt().lessThanOrEqualTo(updateRelationshipsBefore);
	    	}
	    	List<AddressGroupMember> members = new ArrayList<AddressGroupMember>();
	    	List<AddressGroupMember> addressGroupMembers = activitySegment.getExtent(query);
	    	members.addAll(addressGroupMembers);
	    	for(AddressGroupMember member: members) {
	    		member.setAddress(target);
	    		count++;
	    	}
    	}
    	// Update e-mail sender
    	{
	    	EMailQuery query = (EMailQuery)pm.newQuery(EMail.class);
	    	query.thereExistsSender().equalTo(source);
	    	if(updateRelationshipsSince != null) {
	    		query.createdAt().greaterThanOrEqualTo(updateRelationshipsSince);
	    	}
	    	if(updateRelationshipsBefore != null) {
	    		query.createdAt().lessThanOrEqualTo(updateRelationshipsBefore);
	    	}
	    	List<EMail> members = new ArrayList<EMail>();
	    	List<EMail> emails = activitySegment.getActivity(query);
	    	members.addAll(emails);
	    	for(EMail member: members) {
	    		member.setSender(target);
	    		count++;
	    	}
    	}
    	// Update e-mail recipients
    	{
	    	EMailRecipientQuery query = (EMailRecipientQuery)PersistenceHelper.newQuery(
	    		pm.getExtent(EMailRecipient.class),
	    		activitySegment.refGetPath().getDescendant("activity", ":*", "emailRecipient", ":*")
	    	);
	    	query.thereExistsParty().equalTo(source);
	    	if(updateRelationshipsSince != null) {
	    		query.createdAt().greaterThanOrEqualTo(updateRelationshipsSince);
	    	}
	    	if(updateRelationshipsBefore != null) {
	    		query.createdAt().lessThanOrEqualTo(updateRelationshipsBefore);
	    	}
	    	List<EMailRecipient> members = new ArrayList<EMailRecipient>();
	    	List<EMailRecipient> recipients = activitySegment.getExtent(query);
	    	members.addAll(recipients);
	    	for(EMailRecipient member: members) {
	    		member.setParty(target);
	    		count++;
	    	}
    	}
    	// Update incident parties
    	{
    		if(source instanceof EMailAddress) {
		    	IncidentPartyQuery query = (IncidentPartyQuery)PersistenceHelper.newQuery(
		    		pm.getExtent(IncidentParty.class),
		    		activitySegment.refGetPath().getDescendant("activity", ":*", "incidentParty", ":*")
		    	);
		    	query.thereExistsParty().equalTo(sourceAccount);
		    	query.thereExistsEmailHint().equalTo(((EMailAddress)source).getEmailAddress());
		    	if(updateRelationshipsSince != null) {
		    		query.createdAt().greaterThanOrEqualTo(updateRelationshipsSince);
		    	}
		    	if(updateRelationshipsBefore != null) {
		    		query.createdAt().lessThanOrEqualTo(updateRelationshipsBefore);
		    	}
		    	List<IncidentParty> members = new ArrayList<IncidentParty>();
		    	List<IncidentParty> parties = activitySegment.getExtent(query);
		    	members.addAll(parties);
		    	for(IncidentParty member: members) {
		    		member.setParty(targetAccount);		    		
		    		count++;
		    	}
    		}
    	}
    	// Update meeting parties
    	{
    		if(source instanceof EMailAddress) {
		    	MeetingPartyQuery query = (MeetingPartyQuery)PersistenceHelper.newQuery(
		    		pm.getExtent(MeetingParty.class),
		    		activitySegment.refGetPath().getDescendant("activity", ":*", "meetingParty", ":*")
		    	);
		    	query.thereExistsParty().equalTo(sourceAccount);
		    	query.thereExistsEmailHint().equalTo(((EMailAddress)source).getEmailAddress());
		    	if(updateRelationshipsSince != null) {
		    		query.createdAt().greaterThanOrEqualTo(updateRelationshipsSince);
		    	}
		    	if(updateRelationshipsBefore != null) {
		    		query.createdAt().lessThanOrEqualTo(updateRelationshipsBefore);
		    	}
		    	List<MeetingParty> members = new ArrayList<MeetingParty>();
		    	List<MeetingParty> parties = activitySegment.getExtent(query);
		    	members.addAll(parties);
		    	for(MeetingParty member: members) {
		    		member.setParty(targetAccount);		    		
		    		count++;
		    	}
    		}	    	
    	}
    	// Update task parties
    	{
    		if(source instanceof EMailAddress) {
		    	TaskPartyQuery query = (TaskPartyQuery)PersistenceHelper.newQuery(
		    		pm.getExtent(TaskParty.class),
		    		activitySegment.refGetPath().getDescendant("activity", ":*", "taskParty", ":*")
		    	);
		    	query.thereExistsParty().equalTo(sourceAccount);
		    	query.thereExistsEmailHint().equalTo(((EMailAddress)source).getEmailAddress());
		    	if(updateRelationshipsSince != null) {
		    		query.createdAt().greaterThanOrEqualTo(updateRelationshipsSince);
		    	}
		    	if(updateRelationshipsBefore != null) {
		    		query.createdAt().lessThanOrEqualTo(updateRelationshipsBefore);
		    	}
		    	List<TaskParty> members = new ArrayList<TaskParty>();
		    	List<TaskParty> parties = activitySegment.getExtent(query);
		    	members.addAll(parties);
		    	for(TaskParty member: members) {
		    		member.setParty(targetAccount);		    		
		    		count++;
		    	}
    		}
    	}
    	// Disable source address
    	source.setDisabled(true);
    	return count;
    }

    /**
     * Check whether address qualifies for auto-update.
     * 
     * @param address
     * @return
     */
    public CheckForAutoUpdateResult checkForAutoUpdate(
    	AccountAddress address
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(address);
    	List<AccountAddress> lCandidateAddress = new ArrayList<AccountAddress>();
    	List<Boolean> lCandidateAddressQualifiesForAutoUpdate = new ArrayList<Boolean>();
    	List<String> lCandidateMatchingFields = new ArrayList<String>();
    	List<String> lCandidateNonMatchingFields = new ArrayList<String>();
    	if(address.getAuthority() != null) {
    		if(address instanceof PostalAddress) {
    			PostalAddress postalAddress = (PostalAddress)address;
    			PostalAddressQuery query = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
    			query.orderByCreatedAt().ascending();
    			for(PostalAddress candidate: address.getAuthority().<PostalAddress>getAddress(query)) {
    				String matchingFields = "";
    				String nonMatchingFields = "";
    				boolean qualifiesForAutoUpdate = true;
    				// postalStreet
    				boolean postalStreetMatches = Utils.areEqual(candidate.getPostalStreet(), postalAddress.getPostalStreet());
    				if(postalStreetMatches) {
    					matchingFields += "postalStreet"; 
    				} else {
    					nonMatchingFields += "postalStreet";
    					qualifiesForAutoUpdate = false;
    				}
    				// postalStreetNumber
    				boolean postalStreetNumberMatches = Utils.areEqual(candidate.getPostalStreetNumber(), postalAddress.getPostalStreetNumber());
    				if(postalStreetNumberMatches) {
        				matchingFields += matchingFields.isEmpty() ? "" : "<br />";
    					matchingFields += "postalStreetNumber"; 
    				} else {
        				nonMatchingFields += nonMatchingFields.isEmpty() ? "" : "<br />";
    					nonMatchingFields += "postalStreetNumber";
    					qualifiesForAutoUpdate = false;
    				}
    				// postalCode
    				boolean postalCodeMatches = Utils.areEqual(candidate.getPostalCode(), postalAddress.getPostalCode());
    				if(postalCodeMatches) {
        				matchingFields += matchingFields.isEmpty() ? "" : "<br />";
    					matchingFields += "postalCode"; 
    				} else {
        				nonMatchingFields += nonMatchingFields.isEmpty() ? "" : "<br />";
    					nonMatchingFields += "postalCode";
    					qualifiesForAutoUpdate = false;
    				}
    				// postalCity
    				boolean postalCityMatches = Utils.areEqual(candidate.getPostalCity(), postalAddress.getPostalCity());
    				if(postalCityMatches) {
        				matchingFields += matchingFields.isEmpty() ? "" : "<br />";
    					matchingFields += "postalCity"; 
    				} else {
        				nonMatchingFields += nonMatchingFields.isEmpty() ? "" : "<br />";    				
    					nonMatchingFields += "postalCity";
    					qualifiesForAutoUpdate = false;
    				}    				
    				// postalState
    				boolean postalStateMatches = Utils.areEqual(candidate.getPostalState(), postalAddress.getPostalState());
    				if(postalStateMatches) {
        				matchingFields += matchingFields.isEmpty() ? "" : "<br />";
    					matchingFields += "postalState"; 
    				} else {
        				nonMatchingFields += nonMatchingFields.isEmpty() ? "" : "<br />";    				
    					nonMatchingFields += "postalState";
    					qualifiesForAutoUpdate = false;
    				}   				
    				// postalCountry
    				boolean postalCountryMatches = candidate.getPostalCountry() == postalAddress.getPostalCountry();
    				if(postalCountryMatches) {
        				matchingFields += matchingFields.isEmpty() ? "" : "<br />";
    					matchingFields += "postalCountry"; 
    				} else {
        				nonMatchingFields += nonMatchingFields.isEmpty() ? "" : "<br />";    				
    					nonMatchingFields += "postalCountry";
    					qualifiesForAutoUpdate = false;
    				}
    				if(qualifiesForAutoUpdate) {
    					// Show matches first
    					lCandidateAddress.add(0, candidate);
    					lCandidateAddressQualifiesForAutoUpdate.add(0, qualifiesForAutoUpdate);
    					lCandidateMatchingFields.add(0, matchingFields);
    					lCandidateNonMatchingFields.add(0, nonMatchingFields);
    				} else {
    					lCandidateAddress.add(candidate);
    					lCandidateAddressQualifiesForAutoUpdate.add(qualifiesForAutoUpdate);
    					lCandidateMatchingFields.add(matchingFields);
    					lCandidateNonMatchingFields.add(nonMatchingFields);    					
    				}
    			}
    		} else if(address instanceof EMailAddress) {
    			EMailAddress emailAddress = (EMailAddress)address;
    			EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
    			query.orderByCreatedAt().ascending();
    			for(EMailAddress candidateAddress: address.getAuthority().<EMailAddress>getAddress(query)) {
    				String matchingFields = "";
    				String nonMatchingFields = "";
    				boolean qualifiesForAutoUpdate = true;
    				// domain name
    				String emailAddressDomain = emailAddress.getEmailAddress() != null && emailAddress.getEmailAddress().indexOf("@") > 0
    					? emailAddress.getEmailAddress().substring(emailAddress.getEmailAddress().indexOf("@"))
    					: null;
    				String emailAddressDomainCandidate = candidateAddress.getEmailAddress() != null && candidateAddress.getEmailAddress().indexOf("@") > 0
    					? candidateAddress.getEmailAddress().substring(candidateAddress.getEmailAddress().indexOf("@"))
    					: null;
    				boolean domainNameMatches = Utils.areEqual(emailAddressDomainCandidate, emailAddressDomain);
    				if(domainNameMatches) {
    					matchingFields += "emailAddressDomain"; 
    				} else {
    					nonMatchingFields += "emailAddressDomain";
    					qualifiesForAutoUpdate = false;
    				}
    				if(qualifiesForAutoUpdate) {
    					// Show matches first
    					lCandidateAddress.add(0, candidateAddress);
    					lCandidateAddressQualifiesForAutoUpdate.add(0, qualifiesForAutoUpdate);
    					lCandidateMatchingFields.add(0, matchingFields);
    					lCandidateNonMatchingFields.add(0, nonMatchingFields);
    				} else {
    					lCandidateAddress.add(candidateAddress);
    					lCandidateAddressQualifiesForAutoUpdate.add(qualifiesForAutoUpdate);
    					lCandidateMatchingFields.add(matchingFields);
    					lCandidateNonMatchingFields.add(nonMatchingFields);    					
    				}
    			}
     		}
    	}
    	int size = lCandidateAddress.size();
    	return Structures.create(
    		CheckForAutoUpdateResult.class,
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateAddress1, size > 0 ? lCandidateAddress.get(0) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateQualifiesForAutoUpdate1, size > 0 ? lCandidateAddressQualifiesForAutoUpdate.get(0) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateMatchingFields1, size > 0 ? lCandidateMatchingFields.get(0) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateNonMatchingFields1, size > 0 ? lCandidateNonMatchingFields.get(0) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateAddress2, size > 1 ? lCandidateAddress.get(1) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateQualifiesForAutoUpdate2, size > 1 ? lCandidateAddressQualifiesForAutoUpdate.get(1) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateMatchingFields2, size > 1 ? lCandidateMatchingFields.get(1) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateNonMatchingFields2, size > 1 ? lCandidateNonMatchingFields.get(1) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateAddress3, size > 2 ? lCandidateAddress.get(2) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateQualifiesForAutoUpdate3, size > 2 ? lCandidateAddressQualifiesForAutoUpdate.get(2) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateMatchingFields3, size > 2 ? lCandidateMatchingFields.get(2) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateNonMatchingFields3, size > 2 ? lCandidateNonMatchingFields.get(2) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateAddress4, size > 3 ? lCandidateAddress.get(3) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateQualifiesForAutoUpdate4, size > 3 ? lCandidateAddressQualifiesForAutoUpdate.get(3) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateMatchingFields4, size > 3 ? lCandidateMatchingFields.get(3) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateNonMatchingFields4, size > 3 ? lCandidateNonMatchingFields.get(3) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateAddress5, size > 4 ? lCandidateAddress.get(4) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateQualifiesForAutoUpdate5, size > 4 ? lCandidateAddressQualifiesForAutoUpdate.get(4) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateMatchingFields5, size > 4 ? lCandidateMatchingFields.get(4) : null),
        	Datatypes.member(CheckForAutoUpdateResult.Member.candidateNonMatchingFields5, size > 4 ? lCandidateNonMatchingFields.get(4) : null)
        );
    }

    /**
     * Callback for removing an account. E.g. called by jdoPreDelete().
     * 
     * @param account
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeAccount(
        Account account,
        boolean preDelete
    ) throws ServiceException {
    	// Override for custom-specific implementation
    }

	/**
	 * Get matching contacts.
	 * 
	 * @param accountSegment
	 * @param accountQuery
	 * @param firstName
	 * @param lastName
	 * @param aliasName
	 * @param phoneNumberMobile
	 * @param phoneNumberHome
	 * @param phoneNumberBusiness
	 * @param postalCityHome
	 * @param postalCityBusiness
	 * @param postalStreetHome
	 * @param postalStreetBusiness
	 * @param emailHome
	 * @param emailBusiness
	 * @return
	 * @throws ServiceException
	 */
	public List<Account> getMatchingAccounts(
		org.opencrx.kernel.account1.jmi1.Segment accountSegment,
		AccountQuery accountQuery,
		String fullName,
		String firstName,
		String lastName,
		String aliasName,
		String phoneNumberMobile,
		String phoneNumberHome,
		String phoneNumberBusiness,
		String postalCityHome,
		String postalCityBusiness,
		List<String> postalStreetHome,
		List<String> postalStreetBusiness,
		String emailHome,
		String emailBusiness
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
		accountQuery.orderByFullName().ascending();	    
		boolean hasQueryProperty = false;
      	final String wildcard = ".*";
      	if(accountQuery instanceof ContactQuery) {
      		ContactQuery contactQuery = (ContactQuery)accountQuery;
		    if(firstName != null) {
		        hasQueryProperty = true;
		        contactQuery.thereExistsFirstName().like("(?i)" + wildcard + firstName + wildcard);
		    }
		    if(lastName != null) {
		        hasQueryProperty = true;
		        contactQuery.thereExistsLastName().like("(?i)" + wildcard + lastName + wildcard);
		    } else {
		    	// is required field, so use wildcard if no search string is provided
		        hasQueryProperty = true;
		        contactQuery.thereExistsLastName().like(wildcard);
		    }
      	}
	    if(fullName != null) {
	        hasQueryProperty = true;
	        accountQuery.thereExistsFullName().like("(?i)" + wildcard + fullName + wildcard);
	    }
	    if(aliasName != null) {
	        hasQueryProperty = true;
	        accountQuery.thereExistsAliasName().like("(?i)" + wildcard + aliasName + wildcard);
	    }
	    String queryFilterClause = null;
	    List<String> stringParams = new ArrayList<String>();
	    int stringParamIndex = 0;
	    if(phoneNumberMobile != null) {
	    	PhoneNumberQuery query = (PhoneNumberQuery)pm.newQuery(PhoneNumber.class);
	    	query.thereExistsPhoneNumberFull().like(wildcard + phoneNumberMobile + wildcard);
	    	accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(phoneNumberHome != null) {
	    	PhoneNumberQuery query = (PhoneNumberQuery)pm.newQuery(PhoneNumber.class);
	    	query.thereExistsPhoneNumberFull().like(wildcard + phoneNumberHome + wildcard);
	    	accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(phoneNumberBusiness != null) {
	    	PhoneNumberQuery query = (PhoneNumberQuery)pm.newQuery(PhoneNumber.class);
	    	query.thereExistsPhoneNumberFull().like(wildcard + phoneNumberBusiness + wildcard);
	    	accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(postalCityHome != null) {
	    	PostalAddressQuery query = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
	    	query.thereExistsPostalCity().like(wildcard + postalCityHome + wildcard);
	    	accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(postalCityBusiness != null) {
	    	PostalAddressQuery query = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
	    	query.thereExistsPostalCity().like(wildcard + postalCityBusiness + wildcard);
	    	accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(postalStreetHome != null) {
	        for(int i = 0; i < postalStreetHome.size(); i++) {
		        hasQueryProperty = true;
		        if(stringParamIndex > 0) { queryFilterClause += " AND "; } else { queryFilterClause = ""; }
		        queryFilterClause += "v.object_id IN (SELECT act.object_id FROM OOCKE1_ACCOUNT act INNER JOIN OOCKE1_ADDRESS adr ON adr.p$$parent = act.object_id WHERE ((adr.postal_street_0" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_1" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_2" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_3" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_4" + " LIKE ?s" + stringParamIndex + ")))";
		        stringParams.add(wildcard + postalStreetHome.get(i) + wildcard);
		        stringParamIndex++;
	        }
	    }
	    if(postalStreetBusiness != null) {
	        for(int i = 0; i < postalStreetBusiness.size(); i++) {
		        hasQueryProperty = true;
		        if(stringParamIndex > 0) { queryFilterClause += " AND "; } else { queryFilterClause = ""; }
		        queryFilterClause += "v.object_id IN (SELECT act.object_id FROM OOCKE1_ACCOUNT act INNER JOIN OOCKE1_ADDRESS adr ON adr.p$$parent = act.object_id WHERE ((adr.postal_street_0" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_1" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_2" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_3" + " LIKE ?s" + stringParamIndex + ") OR (adr.postal_street_4" + " LIKE ?s" + stringParamIndex + ")))";
		        stringParams.add(wildcard + postalStreetBusiness.get(i) + wildcard);
		        stringParamIndex++;
	        }
	    }
	    if(emailHome != null) {
	    	emailHome = emailHome.trim();
	    	EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
	    	query.thereExistsEmailAddress().like(wildcard + emailHome + wildcard);
	    	accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(emailBusiness != null) {
	    	emailBusiness = emailBusiness.trim();
	    	EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
	    	query.thereExistsEmailAddress().like(wildcard + emailBusiness + wildcard);
	    	accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(queryFilterClause != null) {
	    	QueryExtensionRecord queryFilter = PersistenceHelper.newQueryExtension(accountQuery);
		    queryFilter.setClause(queryFilterClause);
		    queryFilter.getStringParam().addAll(stringParams);
	    }
	   	return hasQueryProperty ?
	        accountSegment.getAccount(accountQuery) :
	        null;
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.backend.AbstractImpl#preDelete(org.opencrx.kernel.generic.jmi1.CrxObject, boolean)
	 */
	@Override
	public void preDelete(
		RefObject_1_0 object, 
		boolean preDelete
	) throws ServiceException {
		super.preDelete(object, preDelete);
		if(object instanceof Account) {
			this.removeAccount((Account)object, preDelete);
		} else if(object instanceof AccountAddress) {
			this.removeAddress((AccountAddress)object, preDelete);
		}
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.backend.AbstractImpl#preStore(org.opencrx.kernel.generic.jmi1.CrxObject)
	 */
	@Override
	public void preStore(
		RefObject_1_0 object
	) throws ServiceException {
		super.preStore(object);
		if(object instanceof Account) {
			this.updateAccount((Account)object);
		} else if(object instanceof AccountAddress) {
			this.updateAddress((AccountAddress)object);
		} else if(object instanceof Member) {
			this.updateMember((Member)object);			
		}
	}

	//-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final int MAIL_BUSINESS = 0;
    public static final int MAIL_HOME = 1;
    public static final int PHONE_BUSINESS = 2;
    public static final int PHONE_HOME = 3;
    public static final int FAX_BUSINESS = 4;
    public static final int FAX_HOME = 5;
    public static final int POSTAL_BUSINESS = 6;
    public static final int WEB_BUSINESS = 7;
    public static final int POSTAL_HOME = 8;
    public static final int WEB_HOME = 9;
    public static final int MOBILE = 10;
    public static final int PHONE_OTHER = 11;
    public static final int MAIL_OTHER = 12;

    public static final short MEMBER_ROLE_EMPLOYEE = 11;
    public static final short MEMBER_ROLE_ASSISTANT = 17;

    public static final short MEMBER_QUALITY_NORMAL = 5;
    
}

//--- End of File -----------------------------------------------------------
