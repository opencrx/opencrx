/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateLegalEntityWizardController
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
package org.opencrx.kernel.portal.wizard;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.cci2.LegalEntityQuery;
import org.opencrx.kernel.account1.cci2.PhoneNumberQuery;
import org.opencrx.kernel.account1.cci2.PostalAddressQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.LegalEntity;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.portal.EmailAddressDataBinding;
import org.opencrx.kernel.portal.PhoneNumberDataBinding;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.portal.servlet.DataBinding;
import org.openmdx.portal.servlet.databinding.CompositeObjectDataBinding;

/**
 * CreateLegalEntityWizardController
 *
 */
public class CreateLegalEntityWizardController extends CreateAccountWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateLegalEntityWizardController(
   	) {
   		super();
   	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.portal.wizard.CreateAccountWizardController#newAccountInstance()
	 */
	@Override
    protected Account newAccountInstance(
    ) throws ServiceException {
		PersistenceManager pm = this.getPm();
		return pm.newInstance(LegalEntity.class);
    }

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#initFormFields(java.util.Map)
	 */
	@Override 
	protected void initFormFields(
		Map<String,Object> formFields
	) throws ServiceException {
		RefObject_1_0 obj = this.getObject();
		if(obj instanceof LegalEntity) {
		    LegalEntity legalEntity = (LegalEntity)obj;
		    formFields.put("org:opencrx:kernel:account1:AbstractGroup:name", legalEntity.getName());
		    formFields.put("org:opencrx:kernel:account1:Account:aliasName", legalEntity.getAliasName());
		    formFields.put("org:opencrx:kernel:account1:LegalEntity:industry", legalEntity.getIndustry());
		    formFields.put("org:opencrx:kernel:account1:Account:description", legalEntity.getDescription());
		    AccountAddress[] addresses = Accounts.getInstance().getMainAddresses(legalEntity);
		    if(addresses[Accounts.PHONE_BUSINESS] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull", ((PhoneNumber)addresses[Accounts.PHONE_BUSINESS]).getPhoneNumberFull());
		    }
		    if(addresses[Accounts.MOBILE] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull", ((PhoneNumber)addresses[Accounts.MOBILE]).getPhoneNumberFull());
		    }
		    if(addresses[Accounts.PHONE_OTHER] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Other!phoneNumberFull", ((PhoneNumber)addresses[Accounts.PHONE_OTHER]).getPhoneNumberFull());
		    }
		    if(addresses[Accounts.FAX_BUSINESS] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*BusinessFax!phoneNumberFull", ((PhoneNumber)addresses[Accounts.FAX_BUSINESS]).getPhoneNumberFull());
		    }
		    if(addresses[Accounts.MAIL_BUSINESS] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Business!emailAddress", ((EMailAddress)addresses[Accounts.MAIL_BUSINESS]).getEmailAddress());
		    }
		    if(addresses[Accounts.MAIL_OTHER] != null) {
		    	formFields.put("org:opencrx:kernel:account1:Account:address*Other!emailAddress", ((EMailAddress)addresses[Accounts.MAIL_OTHER]).getEmailAddress());
		    }
		    if(addresses[Accounts.WEB_BUSINESS] != null) {
		    	formFields.put("org:opencrx:kernel:account1:LegalEntity:address!webUrl", ((org.opencrx.kernel.account1.jmi1.WebAddress)addresses[Accounts.WEB_BUSINESS]).getWebUrl());
		    }
		    if(addresses[Accounts.POSTAL_BUSINESS] != null) {
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalAddressLine", new ArrayList<String>(((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalAddressLine()));
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalStreet", new ArrayList<String>(((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalStreet()));
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCity", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalCity());
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalState", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalState());
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCode", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalCode());
			    formFields.put("org:opencrx:kernel:account1:Account:address*Business!postalCountry", ((PostalAddress)addresses[Accounts.POSTAL_BUSINESS]).getPostalCountry());
		    }
		}
		formFields.put(
			org.openmdx.base.accessor.cci.SystemAttributes.OBJECT_CLASS, 
			"org:opencrx:kernel:account1:LegalEntity"
		);		
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.portal.wizard.CreateAccountWizardController#mapToAccount(org.opencrx.kernel.account1.jmi1.Account, java.util.Map)
	 */
	@Override
	protected void mapToAccount(
		Account account,
		Map<String,Object> formFields
	) {
		if(account instanceof LegalEntity) {
			LegalEntity legalEntity = (LegalEntity)account;
			legalEntity.setName((String)formFields.get("org:opencrx:kernel:account1:AbstractGroup:name"));
			legalEntity.setAliasName((String)formFields.get("org:opencrx:kernel:account1:Account:aliasName"));
			legalEntity.setIndustry(
				formFields.get("org:opencrx:kernel:account1:LegalEntity:industry") == null
					? 0
					: ((Number)formFields.get("org:opencrx:kernel:account1:LegalEntity:industry")).shortValue()
			);
			legalEntity.setDescription((String)formFields.get("org:opencrx:kernel:account1:Account:description"));
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
		super.mapToAddresses(
			account, 
			formFields
		);
		// Phone Other
		DataBinding phoneOtherDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)1800;automaticParsing=(boolean)true");
		phoneOtherDataBinding.setValue(
			account,
			"org:opencrx:kernel:account1:Account:address*Other!phoneNumberFull",
			formFields.get("org:opencrx:kernel:account1:Account:address*Other!phoneNumberFull")
		);
		// Fax Business
		DataBinding faxBusinessDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)530;automaticParsing=(boolean)true");
		faxBusinessDataBinding.setValue(
			account,
			"org:opencrx:kernel:account1:Account:address*BusinessFax!phoneNumberFull",
			formFields.get("org:opencrx:kernel:account1:Account:address*BusinessFax!phoneNumberFull")
		);
	    // Mail Other
	    String emailOther = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Other!emailAddress");
	    if (emailOther != null) {emailOther = emailOther.trim();}
	    DataBinding mailOtherDataBinding = new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)1800;[emailType=(short)1]");
	    mailOtherDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Other!emailAddress",
	        emailOther
	    );
	    // Web Business
	    DataBinding webBusinessDataBinding = new CompositeObjectDataBinding("type=org:opencrx:kernel:account1:WebAddress;disabled=(boolean)false;[isMain=(boolean)true];usage=(short)500");
	    webBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:LegalEntity:address!webUrl",
	        formFields.get("org:opencrx:kernel:account1:LegalEntity:address!webUrl")
	    );
	}

	/**
	 * OK action.
	 * 
	 * @param accountXri
	 * @param isAddMemberMode
	 * @param memberXri
	 * @param isAddMembershipMode
	 * @param membershipXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
   	   	@RequestParameter(name = "accountXri") String accountXri,
   		@RequestParameter(name = "isAddMemberMode") Boolean isAddMemberMode,
   	   	@RequestParameter(name = "memberXri") String memberXri,   	   	
   		@RequestParameter(name = "isAddMembershipMode") Boolean isAddMembershipMode,
   	   	@RequestParameter(name = "membershipXri") String membershipXri,   				
   		@FormParameter(forms = {"LegalEntityForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
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
				isAddMemberMode,
				memberXri,
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
   		@FormParameter(forms = {"LegalEntityForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
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
	
	/* (non-Javadoc)
	 * @see org.opencrx.kernel.portal.wizard.CreateAccountWizardController#doSearch(java.lang.String, java.util.Map)
	 */
	@Override
	public void doSearch(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
   		@FormParameter(forms = {"LegalEntityForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields   				
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			accountXri, 
			formFields
		);
		this.errorMsg = "";
	    boolean hasQueryProperty = false;
	    org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, this.getProviderName(), this.getSegmentName());
	    LegalEntityQuery legalEntityQuery = (LegalEntityQuery)pm.newQuery(LegalEntity.class);
	    legalEntityQuery.orderByName().ascending();
	    String name = (String)formFields.get("org:opencrx:kernel:account1:AbstractGroup:name");
	    String aliasName = (String)formFields.get("org:opencrx:kernel:account1:Account:aliasName");
	    String phoneNumberBusiness = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull");
	    String phoneNumberMobile = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull");
	    String postalCityBusiness = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCity");
	    List<String> postalStreetBusiness = null;
	    try {
	    	if (formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet") != null) {
				if (formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet") instanceof java.util.ArrayList) {
					@SuppressWarnings("unchecked")
                    List<String> values = (List<String>)formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet");
					postalStreetBusiness = values;
				} else {
					String postalStreet = ((String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet")).replace("\n\r", "\n");
					postalStreetBusiness = Arrays.asList(postalStreet.split("\n"));
				}
	    	}
	    } catch (Exception e) {}
	    String emailBusiness = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!emailAddress");
		final String wildcard = ".*";
	    if(name != null) {
	        hasQueryProperty = true;
	        legalEntityQuery.thereExistsFullName().like("(?i)" + wildcard + name + wildcard);
	    } else {
	    	// is required field, so use wildcard if no search string is provided
	        hasQueryProperty = true;
	        legalEntityQuery.thereExistsFullName().like(wildcard);
	    }
	    if(aliasName != null) {
	        hasQueryProperty = true;
	        legalEntityQuery.thereExistsAliasName().like("(?i)" + wildcard + aliasName + wildcard);
	    }
	    String queryFilterClause = null;
	    List<String> stringParams = new ArrayList<String>();
	    int stringParamIndex = 0;
	    if(phoneNumberBusiness != null) {
	    	PhoneNumberQuery query = (PhoneNumberQuery)pm.newQuery(PhoneNumber.class);
	    	query.thereExistsPhoneNumberFull().like(wildcard + phoneNumberBusiness + wildcard);
	    	legalEntityQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(phoneNumberMobile != null) {
	    	PhoneNumberQuery query = (PhoneNumberQuery)pm.newQuery(PhoneNumber.class);
	    	query.thereExistsPhoneNumberFull().like(wildcard + phoneNumberMobile + wildcard);
	    	legalEntityQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(postalCityBusiness != null) {
	    	PostalAddressQuery query = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
	    	query.thereExistsPostalCity().like(wildcard + postalCityBusiness + wildcard);
	    	legalEntityQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
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
	    if(emailBusiness != null) {
	    	emailBusiness = emailBusiness.trim();
	    	EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
	    	query.thereExistsEmailAddress().like("(?i)" + wildcard + emailBusiness + wildcard);
	    	legalEntityQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));
	    }
	    if(queryFilterClause != null) {
	    	QueryExtensionRecord queryFilter = PersistenceHelper.newQueryExtension(legalEntityQuery);
		    queryFilter.setClause(queryFilterClause);
		    queryFilter.getStringParam().addAll(stringParams);
	    }
	   	this.matchingAccounts = hasQueryProperty ?
	        accountSegment.getAccount(legalEntityQuery) :
	        null;
	}

    //-------------------------------------------------------------------
	// Members
	//-------------------------------------------------------------------
}
