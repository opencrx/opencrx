/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateContractWizardController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.wizard;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.cci2.PostalAddressQuery;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.portal.EmailAddressDataBinding;
import org.opencrx.kernel.portal.PhoneNumberDataBinding;
import org.opencrx.kernel.portal.PostalAddressDataBinding;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.DataBinding;

/**
 * CreateContactWizardController
 *
 */
public class CreateContactWizardController extends CreateAccountWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateContactWizardController(
   	) {
   		super();
   	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.portal.wizard.CreateAccountWizardController#mapToAccount(org.opencrx.kernel.account1.jmi1.Account, java.util.Map)
	 */
	@Override
	protected void mapToAccount(
		Account account,
		Map<String,Object> formFields
	) {
		if(account instanceof Contact) {
			Contact contact = (Contact)account;
		    contact.setSalutationCode(
		    	formFields.get("org:opencrx:kernel:account1:Contact:salutationCode") == null 
		    	? 0
		    	: ((Number)formFields.get("org:opencrx:kernel:account1:Contact:salutationCode")).shortValue()
		    );
		    contact.setAliasName((String)formFields.get("org:opencrx:kernel:account1:Account:aliasName"));
		    contact.setSalutation((String)formFields.get("org:opencrx:kernel:account1:Contact:salutation"));
		    contact.setFirstName((String)formFields.get("org:opencrx:kernel:account1:Contact:firstName"));
		    contact.setMiddleName((String)formFields.get("org:opencrx:kernel:account1:Contact:middleName"));
		    contact.setLastName((String)formFields.get("org:opencrx:kernel:account1:Contact:lastName"));
		    contact.setJobTitle((String)formFields.get("org:opencrx:kernel:account1:Contact:jobTitle"));
		    contact.setJobRole((String)formFields.get("org:opencrx:kernel:account1:Contact:jobRole"));
		    contact.setOrganization((String)formFields.get("org:opencrx:kernel:account1:Contact:organization"));
		    contact.setDepartment((String)formFields.get("org:opencrx:kernel:account1:Contact:department"));
		    contact.setDoNotPhone((Boolean)formFields.get("org:opencrx:kernel:account1:Contact:doNotPhone"));
		    contact.setBirthdate((Date)formFields.get("org:opencrx:kernel:account1:Contact:birthdate"));
		    contact.setDescription((String)formFields.get("org:opencrx:kernel:account1:Account:description"));
		}
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.portal.wizard.CreateAccountWizardController#mapToAddresses(org.opencrx.kernel.account1.jmi1.Account, java.util.Map)
	 */
	@Override
	protected void mapToAddresses(
		Account account,
		Map<String,Object> formFields
	) {
		PersistenceManager pm = this.getPm();		
		super.mapToAddresses(
			account, 
			formFields
		);
	    // Phone Home
	    DataBinding phoneHomeDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)400;automaticParsing=(boolean)true");
	    phoneHomeDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Contact:address!phoneNumberFull",
	        formFields.get("org:opencrx:kernel:account1:Contact:address!phoneNumberFull")
	    );
	    // Mail Home
	    String emailHome = (String)formFields.get("org:opencrx:kernel:account1:Contact:address!emailAddress");
	    if (emailHome != null) {emailHome = emailHome.trim();}
	    DataBinding mailHomeDataBinding = new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)400;[emailType=(short)1]");
	    mailHomeDataBinding.setValue(
	        account,
	        "org:opencrx:kernel:account1:Contact:address!emailAddress",
	        emailHome
	    );
	    // Postal Home
	    DataBinding postalHomeDataBinding = new PostalAddressDataBinding("[isMain=(boolean)true];usage=(short)400?zeroAsNull=true");
	    postalHomeDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Contact:address!postalAddressLine",
	        formFields.get("org:opencrx:kernel:account1:Contact:address!postalAddressLine")
	    );
	    postalHomeDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Contact:address!postalStreet",
	        formFields.get("org:opencrx:kernel:account1:Contact:address!postalStreet")
	    );
	    postalHomeDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Contact:address!postalCity",
	        formFields.get("org:opencrx:kernel:account1:Contact:address!postalCity")
	    );
	    postalHomeDataBinding.setValue(
	    	account,
			"org:opencrx:kernel:account1:Contact:address!postalState",
			formFields.get("org:opencrx:kernel:account1:Contact:address!postalState")
	    );
	    postalHomeDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Contact:address!postalCode",
	        formFields.get("org:opencrx:kernel:account1:Contact:address!postalCode")
	    );
	    postalHomeDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Contact:address!postalCountry",
	        formFields.get("org:opencrx:kernel:account1:Contact:address!postalCountry")
	    );
	    try {
		    postalHomeDataBinding.setValue(
		    	account,
		        "org:opencrx:kernel:account1:Contact:address!authority",
		        formFields.get("org:opencrx:kernel:account1:Contact:address!authority") == null 
		        	? null
		        	: pm.getObjectById(formFields.get("org:opencrx:kernel:account1:Contact:address!authority"))	        	
		    );
	    } catch(Exception ignore) {}
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.portal.wizard.CreateAccountWizardController#newAccountInstance()
	 */
	@Override
    protected Account newAccountInstance(
    ) throws ServiceException {
		PersistenceManager pm = this.getPm();
		return pm.newInstance(Contact.class);
    }

	/**
	 * Get copy-and-link addresses. The key of the map is the address type, the values are a list 
	 * of corresponding addresses. When an address is copied-and-linked by the user, the action
	 * doCopyAndLinkAddress<address type> is invoked.
	 * 
	 * @return
	 */
	public Map<String,List<AccountAddress>> getCopyAndLinkAddresses(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		if(this.getAccount() instanceof Contact) {
			Contact contact = (Contact)this.getAccount();
			org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, this.getProviderName(), this.getSegmentName());
			Map<String,List<AccountAddress>> copyAndLinkAddresses = new HashMap<String,List<AccountAddress>>();
			// Collect addresses of type Business|Postal|Main
			// Collect the addresses of the accounts where the contact is member of
			List<AccountAddress> addressesBusinessPostalMain = new ArrayList<AccountAddress>();
			org.opencrx.kernel.account1.cci2.MemberQuery membershipQuery = (org.opencrx.kernel.account1.cci2.MemberQuery)org.openmdx.base.persistence.cci.PersistenceHelper.newQuery(
	    		pm.getExtent(org.opencrx.kernel.account1.jmi1.Member.class),
	    		accountSegment.refGetPath().getDescendant("account", ":*", "member", ":*")
	    	);
			membershipQuery.thereExistsAccount().equalTo(contact);
			membershipQuery.forAllDisabled().isFalse();
			membershipQuery.orderByCreatedAt().ascending();
			for(Member membership: accountSegment.<Member>getExtent(membershipQuery)) {
				Account accountFrom = (Account)pm.getObjectById(membership.refGetPath().getParent().getParent());
				if(accountFrom instanceof AbstractGroup) {
					PostalAddressQuery postalAddressQuery = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
					for(PostalAddress postalAddress:accountFrom.<PostalAddress>getAddress(postalAddressQuery)) {
						addressesBusinessPostalMain.add(postalAddress);
					}
				}
			}
			copyAndLinkAddresses.put(
				"Business|Postal|Main",
				addressesBusinessPostalMain
			);
			return copyAndLinkAddresses;
		} else {
			return Collections.emptyMap();
		}
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#initFormFields(java.util.Map)
	 */
	@Override 
	protected void initFormFields(
		Map<String,Object> formFields
	) throws ServiceException {
		RefObject_1_0 obj = this.getObject();
		if(obj instanceof Contact) {
		    Contact contact = (Contact)obj;
		    formFields.put("org:opencrx:kernel:account1:Contact:salutationCode", contact.getSalutationCode());
		    formFields.put("org:opencrx:kernel:account1:Contact:salutation", contact.getSalutation());
		    formFields.put("org:opencrx:kernel:account1:Contact:firstName", contact.getFirstName());
		    formFields.put("org:opencrx:kernel:account1:Contact:middleName", contact.getMiddleName());
		    formFields.put("org:opencrx:kernel:account1:Contact:lastName", contact.getLastName());
		    formFields.put("org:opencrx:kernel:account1:Account:aliasName", contact.getAliasName());
		    formFields.put("org:opencrx:kernel:account1:Contact:jobTitle", contact.getJobTitle());
		    formFields.put("org:opencrx:kernel:account1:Contact:jobRole", contact.getJobRole());
		    formFields.put("org:opencrx:kernel:account1:Contact:organization", contact.getOrganization());
		    formFields.put("org:opencrx:kernel:account1:Contact:department", contact.getDepartment());
		    formFields.put("org:opencrx:kernel:account1:Contact:doNotPhone", contact.isDoNotPhone());
		    formFields.put("org:opencrx:kernel:account1:Contact:birthdate", contact.getBirthdate());
		    formFields.put("org:opencrx:kernel:account1:Account:description", contact.getDescription());
		    AccountAddress[] addresses = Accounts.getInstance().getMainAddresses(contact);
		    if(addresses[Accounts.PHONE_BUSINESS] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull", ((PhoneNumber)addresses[Accounts.PHONE_BUSINESS]).getPhoneNumberFull());
		    }
		    if(addresses[Accounts.MOBILE] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull", ((PhoneNumber)addresses[Accounts.MOBILE]).getPhoneNumberFull());
		    }
		    if(addresses[Accounts.PHONE_HOME] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Contact:address!phoneNumberFull", ((PhoneNumber)addresses[Accounts.PHONE_HOME]).getPhoneNumberFull());
		    }
		    if(addresses[Accounts.MAIL_BUSINESS] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Business!emailAddress", ((EMailAddress)addresses[Accounts.MAIL_BUSINESS]).getEmailAddress());
		    }
		    if(addresses[Accounts.MAIL_HOME] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Contact:address!emailAddress", ((EMailAddress)addresses[Accounts.MAIL_HOME]).getEmailAddress());
		    }
		    if(addresses[Accounts.POSTAL_HOME] != null) {
			    formFields.put("org:opencrx:kernel:account1:Contact:address!postalAddressLine", new ArrayList<String>(((PostalAddress)addresses[Accounts.POSTAL_HOME]).getPostalAddressLine()));
			    formFields.put("org:opencrx:kernel:account1:Contact:address!postalStreet", new ArrayList<String>(((PostalAddress)addresses[Accounts.POSTAL_HOME]).getPostalStreet()));
			    formFields.put("org:opencrx:kernel:account1:Contact:address!postalCity", ((PostalAddress)addresses[Accounts.POSTAL_HOME]).getPostalCity());
			    formFields.put("org:opencrx:kernel:account1:Contact:address!postalState", ((PostalAddress)addresses[Accounts.POSTAL_HOME]).getPostalState());
			    formFields.put("org:opencrx:kernel:account1:Contact:address!postalCode", ((PostalAddress)addresses[Accounts.POSTAL_HOME]).getPostalCode());
			    formFields.put("org:opencrx:kernel:account1:Contact:address!postalCountry", ((PostalAddress)addresses[Accounts.POSTAL_HOME]).getPostalCountry());
		    	formFields.put("org:opencrx:kernel:account1:Contact:address!authority", addresses[Accounts.POSTAL_HOME].getAuthority() == null || addresses[Accounts.POSTAL_HOME].getAuthority() == null ? null : addresses[Accounts.POSTAL_HOME].getAuthority().refGetPath());
		    }
		    if(addresses[Accounts.POSTAL_BUSINESS] != null) {
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalAddressLine", new ArrayList<String>(((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalAddressLine()));
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalStreet", new ArrayList<String>(((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalStreet()));
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCity", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalCity());
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalState", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalState());
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCode", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalCode());
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCountry", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalCountry());
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Business!authority", addresses[Accounts.POSTAL_BUSINESS] == null || addresses[Accounts.POSTAL_BUSINESS].getAuthority() == null ? null : addresses[Accounts.POSTAL_BUSINESS].getAuthority().refGetPath());
		    }
   			// Allows rendering to determine the object type (e.g. required for code fields)
   			formFields.put(
   				org.openmdx.base.accessor.cci.SystemAttributes.OBJECT_CLASS, 
   				"org:opencrx:kernel:account1:Contact"
   			);
		}
	}

	/**
	 * OK action.
	 * 
	 * @param accountXri
	 * @param isAddMembershipMode
	 * @param membershipXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
   	   	@RequestParameter(name = "accountXri") String accountXri,
   		@RequestParameter(name = "isAddMembershipMode") Boolean isAddMembershipMode,
   	   	@RequestParameter(name = "membershipXri") String membershipXri,   				
   		@FormParameter(forms = {"ContactForm", "ContactMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		this.doRefresh(
			accountXri,
			formFields
		);
		if(
			this.getAccount() != null &&
			this.isValidForUpdate()
		) {
			this.updateAccount(
				this.getAccount(), 
				formFields,
				null, // isAddMemberMode
				null, // memberXri
				isAddMembershipMode,
				membershipXri
			);
		}
	    formFields.put("org:opencrx:kernel:account1:AccountAssignment:account", null);
	    formFields.put("org:opencrx:kernel:account1:Member:memberRole", null);
		formFields.put("org:opencrx:kernel:account1:Member:name", null);
		formFields.put("org:opencrx:kernel:account1:Member:description", null);	    
	}

	/**
	 * Create action.
	 * 
	 * @param accountXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCreate(
   	   	@RequestParameter(name = "accountXri") String accountXri,
   		@FormParameter(forms = {"ContactForm", "ContactMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		this.doRefresh(
			accountXri,
			formFields
		);
		this.updateAccount(
			this.account = this.newAccountInstance(),
			formFields,
			null, // isAddMemberMode
			null, // memberXri
			null, // isAddMembershipMode
			null // membershipXri
		);
	    formFields.put("org:opencrx:kernel:account1:AccountAssignment:account", null);
	    formFields.put("org:opencrx:kernel:account1:Member:memberRole", null);
		formFields.put("org:opencrx:kernel:account1:Member:name", null);
		formFields.put("org:opencrx:kernel:account1:Member:description", null);	    
	}
	
	/**
	 * Search action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doSearch(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
   		@FormParameter(forms = {"ContactForm", "ContactMembershipForm", "MemberForm"}) Map<String,Object> formFields   				
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			accountXri,
			formFields
		);
		this.errorMsg = "";
	    String firstName = (String)formFields.get("org:opencrx:kernel:account1:Contact:firstName");
	    String lastName = (String)formFields.get("org:opencrx:kernel:account1:Contact:lastName");
	    String aliasName = (String)formFields.get("org:opencrx:kernel:account1:Account:aliasName");
	    String phoneNumberMobile = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull");
	    String phoneNumberHome = (String)formFields.get("org:opencrx:kernel:account1:Contact:address!phoneNumberFull");
	    String phoneNumberBusiness = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull");
	    String postalCityHome = (String)formFields.get("org:opencrx:kernel:account1:Contact:address!postalCity");
	    String postalCityBusiness = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCity");
	    List<String> postalStreetHome = null;
	    try {
	    	if (formFields.get("org:opencrx:kernel:account1:Contact:address!postalStreet") != null) {
				if (formFields.get("org:opencrx:kernel:account1:Contact:address!postalStreet") instanceof List) {
					@SuppressWarnings("unchecked")
                    List<String> values = (List<String>)formFields.get("org:opencrx:kernel:account1:Contact:address!postalStreet");
					postalStreetHome = values;
				} else {
					String postalStreet = ((String)formFields.get("org:opencrx:kernel:account1:Contact:address!postalStreet")).replace("\n\r", "\n");
					postalStreetHome = Arrays.asList(postalStreet.split("\n"));
				}
	    	}
	    } catch (Exception e) {}
	    List<String> postalStreetBusiness = null;
	    try {
	    	if (formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet") != null) {
				if (formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet") instanceof List) {
					@SuppressWarnings("unchecked")
                    List<String> values = (List<String>)formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet");
					postalStreetBusiness = values;
				} else {
					String postalStreet = ((String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet")).replace("\n\r", "\n");
					postalStreetBusiness = Arrays.asList(postalStreet.split("\n"));
				}
	    	}
	    } catch (Exception e) {}
	    String emailHome = (String)formFields.get("org:opencrx:kernel:account1:Contact:address!emailAddress");
	    String emailBusiness = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!emailAddress");
	    org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, this.getProviderName(), this.getSegmentName());
	    this.matchingAccounts = Accounts.getInstance().getMatchingAccounts(
	    	accountSegment,
	    	(ContactQuery)pm.newQuery(Contact.class),
	    	null, // fullName
	    	firstName,
	    	lastName,
	    	aliasName,
	    	phoneNumberMobile,
	    	phoneNumberHome,
	    	phoneNumberBusiness,
	    	postalCityHome,
	    	postalCityBusiness,
	    	postalStreetHome,
	    	postalStreetBusiness,
	    	emailHome,
	    	emailBusiness
	    );
	}

	/**
	 * CopyAndLinkAddressBusinessPostalMain action.
	 * 
	 * @param copyAndLinkAddressXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCopyAndLinkAddressBusinessPostalMain(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
   		@RequestParameter(name = "copyAndLinkAddressXri") String copyAndLinkAddressXri,
		@FormParameter(forms = {"ContactForm", "ContactMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			accountXri,
			formFields
		);
		if(copyAndLinkAddressXri != null && !copyAndLinkAddressXri.isEmpty()) {
			try {
				PostalAddress postalAddress = (PostalAddress)pm.getObjectById(new Path(copyAndLinkAddressXri));
		        formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalStreet", postalAddress.getPostalStreet());
		        formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCity", postalAddress.getPostalCity());
		        formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalState", postalAddress.getPostalState());
		        formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCode", postalAddress.getPostalCode());
		        formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCountry", postalAddress.getPostalCountry());
		        formFields.put("org:opencrx:kernel:account1:Account:address*Business!authority", postalAddress.refGetPath().getParent().getParent());
			} catch(Exception ignore) {}
		}
	}

	/**
	 * Flag to turn on / off create legal entity button.
	 * 
	 * @return
	 */
	public boolean isCreateLegalEntityButtonEnabled(
	) {
		return true;
	}
	
	//-------------------------------------------------------------------
	// Members
	//-------------------------------------------------------------------

}
