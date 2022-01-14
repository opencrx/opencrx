/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BpiAdapterExtension
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
package org.opencrx.application.bpi.adapter;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.jdo.FetchGroup;
import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.application.bpi.datatype.BpiAccount;
import org.opencrx.application.bpi.datatype.BpiAccountFilter;
import org.opencrx.application.bpi.datatype.BpiAccountMember;
import org.opencrx.application.bpi.datatype.BpiActivity;
import org.opencrx.application.bpi.datatype.BpiActivityCreator;
import org.opencrx.application.bpi.datatype.BpiActivityFollowUp;
import org.opencrx.application.bpi.datatype.BpiAddress;
import org.opencrx.application.bpi.datatype.BpiAddressGroup;
import org.opencrx.application.bpi.datatype.BpiCodeTable;
import org.opencrx.application.bpi.datatype.BpiCodeTableEntry;
import org.opencrx.application.bpi.datatype.BpiContact;
import org.opencrx.application.bpi.datatype.BpiEMailAddress;
import org.opencrx.application.bpi.datatype.BpiLocalizedField;
import org.opencrx.application.bpi.datatype.BpiObject;
import org.opencrx.application.bpi.datatype.BpiOrganization;
import org.opencrx.application.bpi.datatype.BpiParticipant;
import org.opencrx.application.bpi.datatype.BpiPhoneNumber;
import org.opencrx.application.bpi.datatype.BpiPostalAddress;
import org.opencrx.application.bpi.datatype.BpiWebAddress;
import org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery;
import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.cci2.LegalEntityQuery;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.account1.jmi1.AccountMembership;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.LegalEntity;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.account1.jmi1.WebAddress;
import org.opencrx.kernel.activity1.cci2.ActivityCreatorQuery;
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.cci2.ActivityTrackerQuery;
import org.opencrx.kernel.activity1.cci2.AddressGroupQuery;
import org.opencrx.kernel.activity1.jmi1.AbstractActivityParty;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.AddressGroup;
import org.opencrx.kernel.activity1.jmi1.IncidentParty;
import org.opencrx.kernel.activity1.jmi1.MeetingParty;
import org.opencrx.kernel.activity1.jmi1.TaskParty;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.code1.cci2.CodeValueContainerQuery;
import org.opencrx.kernel.code1.cci2.CodeValueEntryQuery;
import org.opencrx.kernel.code1.jmi1.CodeValueContainer;
import org.opencrx.kernel.code1.jmi1.CodeValueEntry;
import org.opencrx.kernel.generic.jmi1.LocalizedField;
import org.opencrx.kernel.workflow1.cci2.ExporterTaskQuery;
import org.opencrx.kernel.workflow1.jmi1.ExporterTask;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.naming.Path;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;


/**
 * BpiPlugIn
 *
 */
public class BpiPlugIn {

	/**
	 * Create new instance of Gson. Override for custom-specific configuration.
	 * 
	 * @return
	 */
	protected GsonBuilder newGsonBuilder(
	) {
		return new GsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
	}

    /**
     * Stringify value.
     * 
     * @param pw
     * @param value
     */
	public void printObject(
    	PrintWriter pw,
    	Object value
    ) {
    	Gson gson = this.newGsonBuilder().create();
    	pw.println(gson.toJson(value));
    }

    /**
     * Stringify value.
     * 
     * @param value
     */
	public String printObject(
    	Object value
    ) {
		return this.printObject(value, false);
    }

    /**
     * Stringify value.
     * 
     * @param value
     * @param prettyPrinting
     */
	public String printObject(
    	Object value,
    	boolean prettyPrinting
    ) {
		GsonBuilder gsonBuilder = this.newGsonBuilder();
		if(prettyPrinting) {
			gsonBuilder.setPrettyPrinting();
		}
    	Gson gson = gsonBuilder.create();
    	return gson.toJson(value);
    }

    /**
     * Parse object from string.
     * 
     * @param reader
     * @param clazz
     * @return
     */
    public <T> T parseObject(
    	Reader r,
    	Class<T> clazz
    ) {
    	Gson gson = new Gson();
    	return gson.fromJson(r, clazz);
    }

    /**
     * Find code value containers matching the given name.
     * 
     * @param path
     * @param pm
     * @return
     * @throws ServiceException
     */
    public List<CodeValueContainer> findCodeValueContainers(
    	Path path,
    	PersistenceManager pm
    ) throws ServiceException {
    	org.opencrx.kernel.code1.jmi1.Segment codeSegment = (org.opencrx.kernel.code1.jmi1.Segment)pm.getObjectById(
    		new Path("xri://@openmdx*org.opencrx.kernel.code1").getDescendant("provider", path.getSegment(2).toString(), "segment", path.getSegment(4).toString())
    	);
    	CodeValueContainerQuery codeValueContainerQuery = (CodeValueContainerQuery)pm.newQuery(CodeValueContainer.class);
    	codeValueContainerQuery.thereExistsName().equalTo(path.getLastSegment().toString());
    	codeValueContainerQuery.orderByCreatedAt().ascending();
    	return codeSegment.getValueContainer(codeValueContainerQuery);
    }

    /**
     * Get new instance of of BpiCodeTable.
     * 
     * @return
     */
    public BpiCodeTable newBpiCodeTable(
    ) {
    	return new BpiCodeTable();
    }

    /**
     * Get new instance of of BpiCodeTableEntry.
     * 
     * @return
     */
    public BpiCodeTableEntry newBpiCodeTableEntry(
    ) {
    	return new BpiCodeTableEntry();
    }
    
    /**
     * Get new instance of BpiActivityCreator.
     * 
     * @return
     */
    public BpiActivityCreator newBpiActivityCreator(
    ) {
    	return new BpiActivityCreator();
    }

    /**
     * Get new instance of BpiActivity.
     * 
     * @return
     */
    public BpiActivity newBpiActivity(
    ) {
    	return new BpiActivity();
    }

    /**
     * Get new instance of BpiActivityFollowUp.
     * 
     * @return
     */
    public BpiActivityFollowUp newBpiActivityFollowUp(
    ) {
    	return new BpiActivityFollowUp();
    }

    /**
     * Get new instance of BpiEMailAddress.
     * 
     * @return
     */
    public BpiEMailAddress newBpiEMailAddress(
    ) {
    	return new BpiEMailAddress();
    }

    /**
     * Get new instance of BpiPhoneNumber.
     * 
     * @return
     */
    public BpiPhoneNumber newBpiPhoneNumber(
    ) {
    	return new BpiPhoneNumber();
    }

    /**
     * Get new instance of BpiWebAddress.
     * 
     * @return
     */
    public BpiWebAddress newBpiWebAddress(
    ) {
    	return new BpiWebAddress();
    }

    /**
     * Get new instance of BpiAccountMember.
     * 
     * @return
     */
    public BpiAccountMember newBpiAccountMember(
    ) {
    	return new BpiAccountMember();
    }

    /**
     * Get new instance of BpiPostalAddress.
     * 
     * @return
     */
    public BpiPostalAddress newBpiPostalAddress(
    ) {
    	return new BpiPostalAddress();
    }

    /**
     * Get new instance of BpiParticipant.
     * 
     * @return
     */
    public BpiParticipant newBpiParticipant(
    ) {
    	return new BpiParticipant();
    }

    /**
     * Get new instance of BpiContact.
     * 
     * @return
     */
    public BpiContact newBpiContact(
    ) {
    	return new BpiContact();
    }

    /**
     * Get new instance of BpiOrganization.
     * 
     * @return
     */
    public BpiOrganization newBpiOrganization(
    ) {
    	return new BpiOrganization();
    }

    /**
     * Get new instance of BpiLocalizedField.
     * 
     * @return
     */
    public BpiLocalizedField newBpiLocalizedField(
    ) {
    	return new BpiLocalizedField();
    }

    /**
     * Get new instance of BpiAddressGroup.
     * 
     * @return
     */
    public BpiAddressGroup newBpiAddressGroup(
    ) {
    	return new BpiAddressGroup();    	
    }

    /**
     * Get new instance of BpiAccountFilter.
     * 
     * @return
     */
    public BpiAccountFilter newBpiAccountFilter(
    ) {
    	return new BpiAccountFilter();    	
    }

    /**
     * Map code value container to BpiCodeTable.
     * 
     * @param codeValueContainer
     * @param bpiCodeTable
     * @param requestedLocales
     * @param fetchGroup
     * @return
     * @throws ServiceException
     */
    public BpiCodeTable toBpiCodeTable(
    	CodeValueContainer codeValueContainer,
    	BpiCodeTable bpiCodeTable,
    	List<Short> locales,
    	String fetchGroup
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(codeValueContainer);
    	this.toBpiObject(
    		codeValueContainer, 
    		bpiCodeTable,
    		fetchGroup
    	);
    	bpiCodeTable.setName(codeValueContainer.getName());
    	List<BpiCodeTableEntry> bpiEntries = new ArrayList<BpiCodeTableEntry>();
		CodeValueEntryQuery codeValueEntryQuery = (CodeValueEntryQuery)pm.newQuery(CodeValueEntry.class);
		codeValueEntryQuery.orderByCreatedAt().ascending();    	
		for(CodeValueEntry entry: codeValueContainer.<CodeValueEntry>getEntry(codeValueEntryQuery)) {
			BpiCodeTableEntry bpiEntry = newBpiCodeTableEntry();
			this.toBpiObject(
				entry, 
				bpiEntry,
				fetchGroup
			);
			bpiEntry.setValidFrom(entry.getValidFrom());
			bpiEntry.setValidTo(entry.getValidTo());
			if(locales == null || locales.isEmpty()) {
				bpiEntry.setShortText(entry.getShortText());
				bpiEntry.setLongText(entry.getLongText());				
			} else {
				List<String> shortTexts = new ArrayList<String>();
				List<String> longTexts = new ArrayList<String>();
				for(Short locale: locales) {
					if(locale < entry.getShortText().size()) {
						shortTexts.add(entry.getShortText().get(locale));
					}
					if(locale < entry.getLongText().size()) {
						longTexts.add(entry.getLongText().get(locale));
					}
				}
				bpiEntry.setShortText(shortTexts);
				bpiEntry.setLongText(longTexts);
			}
			bpiEntries.add(bpiEntry);
		}
    	bpiCodeTable.setEntry(bpiEntries);
    	return bpiCodeTable;
    }

	/**
	 * Map activity creator to BpiActivityCreator.
	 * 
	 * @param activityCreator
	 * @return
	 * @throws ServiceException
	 */
    public BpiActivityCreator toBpiActivityCreator(
		ActivityCreator activityCreator,
		BpiActivityCreator bpiActivityCreator,
		String fetchGroup
	) throws ServiceException {
		this.toBpiObject(
			activityCreator, 
			bpiActivityCreator,
			fetchGroup
		);
		bpiActivityCreator.setName(activityCreator.getName());
		bpiActivityCreator.setDescription(activityCreator.getDescription());
		return bpiActivityCreator;
	}
	
	/**
	 * Map address to BpiAddress.
	 * 
	 * @param address
	 * @param bpiAddress
	 * @param fetchGroup
	 * @return
	 * @throws ServiceException
	 */
	public BpiAddress toBpiAddress(
		AccountAddress address,
		BpiAddress bpiAddress,
		String fetchGroup		
	) throws ServiceException {
		this.toBpiObject(
			address, 
			bpiAddress,
			fetchGroup
		);
		bpiAddress.setMain(address.isMain());
		bpiAddress.setUsage(address.getUsage());
		return bpiAddress;
	}

    /**
     * Map EMailAddress to BpiEMailAddress.
     * 
     * @param emailAddress
     * @throws ServiceException
     * @throws IOException
     */
	public BpiEMailAddress toBpiEMailAddress(
		EMailAddress emailAddress,
		String fetchGroup
    ) throws ServiceException {
		BpiEMailAddress bpiEMailAddress = newBpiEMailAddress();
		this.toBpiAddress(
			emailAddress, 
			bpiEMailAddress,
			fetchGroup
		);
		bpiEMailAddress.setEmailAddress(emailAddress.getEmailAddress());
		return bpiEMailAddress;
	}

    /**
     * Map PhoneNumber to BpiPhoneNumber.
     * 
     * @param phoneNumber
     * @throws ServiceException
     * @throws IOException
     */
	public BpiPhoneNumber toBpiPhoneNumber(
		PhoneNumber phoneNumber,
		String fetchGroup
    ) throws ServiceException {
		BpiPhoneNumber bpiPhoneNumber = newBpiPhoneNumber();
		this.toBpiAddress(
			phoneNumber, 
			bpiPhoneNumber,
			fetchGroup
		);
		bpiPhoneNumber.setPhoneNumberFull(phoneNumber.getPhoneNumberFull());
		return bpiPhoneNumber;
    }

    /**
     * Map WebAddress to BpiWebAddress.
     * 
     * @param webAddress
     * @throws ServiceException
     * @throws IOException
     */
	public BpiWebAddress toBpiWebAddress(
		WebAddress webAddress,
		String fetchGroup
    ) throws ServiceException {
		BpiWebAddress bpiWebAddress = newBpiWebAddress();
		this.toBpiAddress(
			webAddress, 
			bpiWebAddress,
			fetchGroup
		);
		bpiWebAddress.setWebAddress(webAddress.getWebUrl());
		return bpiWebAddress;
    }

    /**
     * Map PostalAddress to BpiPostalAddress.
     * 
     * @param postalAddress
     * @throws ServiceException
     * @throws IOException
     */
	public BpiPostalAddress toBpiPostalAddress(
		PostalAddress postalAddress,
		String fetchGroup
    ) throws ServiceException {
		BpiPostalAddress bpiPostalAddress = newBpiPostalAddress();
		this.toBpiAddress(
			postalAddress,
			bpiPostalAddress,
			fetchGroup
		);
		bpiPostalAddress.setPostalAddressLine(postalAddress.getPostalAddressLine());
		bpiPostalAddress.setPostalStreet(postalAddress.getPostalStreet());
		bpiPostalAddress.setPostalCode(postalAddress.getPostalCode());
		bpiPostalAddress.setPostalCountry(postalAddress.getPostalCountry());
		bpiPostalAddress.setPostalCity(postalAddress.getPostalCity());
		return bpiPostalAddress;
    }

    /**
     * Map BasicObject to BpiObject.
     * 
     * @param object
     * @param bpiObject
     * @throws ServiceException
     */
    public BpiObject toBpiObject(
    	BasicObject object,
    	BpiObject bpiObject,
    	String fetchGroup
    ) throws ServiceException {
    	if(bpiObject.getId() == null) {
    		bpiObject.setId(object.refGetPath().getLastSegment().toString());
    	}
    	if(!FetchGroup.BASIC.equals(fetchGroup)) {
    		bpiObject.setXri(object.refGetPath().toXRI());
        	bpiObject.setCreatedAt(object.getCreatedAt());
        	bpiObject.setCreatedBy(object.getCreatedBy());
        	bpiObject.setModifiedAt(object.getModifiedAt());
        	bpiObject.setModifiedBy(object.getModifiedBy());
    	}
    	return bpiObject;
    }

    /**
     * Map Account to BpiAccount.
     * 
     * @param account
     * @param bpiAccount
     * @return
     * @throws ServiceException
     */
    public BpiAccount toBpiAccount(
    	Account account,
    	BpiAccount bpiAccount,
    	String fetchGroup
    ) throws ServiceException {
    	this.toBpiObject(
    		account, 
    		bpiAccount,
    		fetchGroup
    	);    	
    	bpiAccount.setExtString0(account.getExtString0());
    	// Get main addresses ...
    	AccountAddress[] mainAddresses = null;
        List<AccountAddress> businessAddresses =  Accounts.getInstance().getAccountAddresses(
            account,
            Addresses.USAGE_BUSINESS
        );
    	if(FetchGroup.BASIC.equals(fetchGroup)) {
    		mainAddresses = new AccountAddress[Accounts.MAIL_OTHER + 1];
    		for(AccountAddress address: businessAddresses) {
    			if(Boolean.TRUE.equals(address.isMain())) {
    				if(address instanceof PostalAddress) {
    					mainAddresses[Accounts.POSTAL_BUSINESS] = address;
    				} else if(address instanceof PhoneNumber) {
    					mainAddresses[Accounts.PHONE_BUSINESS] = address;
    				} else if(address instanceof EMailAddress) {
    					mainAddresses[Accounts.MAIL_BUSINESS] = address;
    				}
    			}
    		}
    	} else {
    		mainAddresses = Accounts.getInstance().getMainAddresses(account);
    	}
    	// ... and all business addresses
        PhoneNumber phoneBusiness2 = null;
        EMailAddress mailBusiness2 = null;
        PhoneNumber faxBusiness2 = null;
        PostalAddress postalBusiness2 = null;
        for(AccountAddress address: businessAddresses) {
        	if(
            	// phoneBusiness2 in case a phoneBusiness exists and are not equal
        		address instanceof PhoneNumber && 
        		mainAddresses[Accounts.PHONE_BUSINESS] != null && 
        		!address.equals(mainAddresses[Accounts.PHONE_BUSINESS])
        	) {
        		phoneBusiness2 = (PhoneNumber)address;
        	} else if(
        		// mailBusiness2 in case a mailBusiness exists and are not equal
        		address instanceof EMailAddress && 
        		mainAddresses[Accounts.MAIL_BUSINESS] != null && 
        		!address.equals(mainAddresses[Accounts.MAIL_BUSINESS])
        	) {
        		mailBusiness2 = (EMailAddress)address;
        	} else if(
	    		// faxBusiness2 in case a faxBusiness exists and are not equal
	    		address instanceof PhoneNumber && 
	    		mainAddresses[Accounts.FAX_BUSINESS] != null &&
	    		!address.equals(mainAddresses[Accounts.FAX_BUSINESS])
	    	) {
	    		faxBusiness2 = (PhoneNumber)address;
	    	} else if(
	    		// postalBusiness2 in case a postalBusiness exists and are not equal
	    		address instanceof PostalAddress &&
	    		mainAddresses[Accounts.POSTAL_BUSINESS] != null &&
	    		!address.equals(mainAddresses[Accounts.POSTAL_BUSINESS])
	    	) {
	    		postalBusiness2 = (PostalAddress)address;
	    	}
        }
    	if(mainAddresses[Accounts.MAIL_BUSINESS] instanceof EMailAddress) {
    		bpiAccount.setMailBusiness(
    			this.toBpiEMailAddress(
    				(EMailAddress)mainAddresses[Accounts.MAIL_BUSINESS], 
    				fetchGroup)
    			);
    	}
    	if(mailBusiness2 != null) {
    		bpiAccount.setMailBusiness2(
    			this.toBpiEMailAddress(
    				mailBusiness2, 
    				fetchGroup
    			)
    		);    		
    	}
    	if(mainAddresses[Accounts.MAIL_HOME] instanceof EMailAddress) {
    		bpiAccount.setMailHome(
    			this.toBpiEMailAddress(
    				(EMailAddress)mainAddresses[Accounts.MAIL_HOME], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.MAIL_OTHER] instanceof EMailAddress) {
    		bpiAccount.setMailOther(
    			this.toBpiEMailAddress(
    				(EMailAddress)mainAddresses[Accounts.MAIL_OTHER], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.PHONE_BUSINESS] instanceof PhoneNumber) {
    		bpiAccount.setPhoneBusiness(
    			this.toBpiPhoneNumber(
    				(PhoneNumber)mainAddresses[Accounts.PHONE_BUSINESS], 
    				fetchGroup
    			)
    		);
    	}
    	if(phoneBusiness2 != null) {
    		bpiAccount.setPhoneBusiness2(
    			this.toBpiPhoneNumber(
    				phoneBusiness2, 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.PHONE_HOME] instanceof PhoneNumber) {
    		bpiAccount.setPhoneHome(
    			this.toBpiPhoneNumber(
    				(PhoneNumber)mainAddresses[Accounts.PHONE_HOME], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.PHONE_OTHER] instanceof PhoneNumber) {
    		bpiAccount.setPhoneOther(
    			this.toBpiPhoneNumber(
    				(PhoneNumber)mainAddresses[Accounts.PHONE_OTHER], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.FAX_BUSINESS] instanceof PhoneNumber) {
    		bpiAccount.setFaxBusiness(
    			this.toBpiPhoneNumber(
    				(PhoneNumber)mainAddresses[Accounts.FAX_BUSINESS], 
    				fetchGroup
    			)
    		);
    	}
    	if(faxBusiness2 != null) {
    		bpiAccount.setFaxBusiness2(
    			this.toBpiPhoneNumber(
    				faxBusiness2, 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.FAX_HOME] instanceof PhoneNumber) {
    		bpiAccount.setFaxHome(
    			this.toBpiPhoneNumber(
    				(PhoneNumber)mainAddresses[Accounts.FAX_HOME], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.POSTAL_BUSINESS] instanceof PostalAddress) {
    		bpiAccount.setPostalBusiness(
    			this.toBpiPostalAddress(
    				(PostalAddress)mainAddresses[Accounts.POSTAL_BUSINESS], 
    				fetchGroup
    			)
    		);
    	}
    	if(postalBusiness2 != null) {
    		bpiAccount.setPostalBusiness2(
    			this.toBpiPostalAddress(
    				postalBusiness2, 
    				fetchGroup
    			)
    		);    		
    	}
    	if(mainAddresses[Accounts.POSTAL_HOME] instanceof PostalAddress) {
    		bpiAccount.setPostalHome(
    			this.toBpiPostalAddress(
    				(PostalAddress)mainAddresses[Accounts.POSTAL_HOME], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.MOBILE] instanceof PhoneNumber) {
    		bpiAccount.setMobile(
    			this.toBpiPhoneNumber(
    				(PhoneNumber)mainAddresses[Accounts.MOBILE], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.WEB_BUSINESS] instanceof WebAddress) {
    		bpiAccount.setWebBusiness(
    			this.toBpiWebAddress(
    				(WebAddress)mainAddresses[Accounts.WEB_BUSINESS], 
    				fetchGroup
    			)
    		);
    	}
    	if(mainAddresses[Accounts.WEB_HOME] instanceof WebAddress) {
    		bpiAccount.setWebBusiness(
    			this.toBpiWebAddress(
    				(WebAddress)mainAddresses[Accounts.WEB_HOME], 
    				fetchGroup
    			)
    		);
    	}
		// Localized fields
    	if(!FetchGroup.BASIC.equals(fetchGroup)) {
			List<BpiLocalizedField> bpiLocalizedFields = new ArrayList<BpiLocalizedField>();
			for(LocalizedField localizedField: account.<LocalizedField>getLocalizedField()) {
				bpiLocalizedFields.add(
					this.toBpiLocalizedField(
						localizedField, 
						this.newBpiLocalizedField(),
						FetchGroup.DEFAULT
					)
				);
			}
			bpiAccount.setLocalizedField(bpiLocalizedFields);    	
    	}
    	return bpiAccount;
    }

    /**
     * Map contact to BpiContact.
     * 
     * @param contact
     * @throws ServiceException
     */
    public BpiContact toBpiContact(
       	Contact contact,
       	BpiContact bpiContact,
       	String fetchGroup
    ) throws ServiceException {
    	this.toBpiAccount(
    		contact, 
    		bpiContact, 
    		fetchGroup
    	);
    	bpiContact.setAliasName(contact.getAliasName());
    	bpiContact.setFullName(contact.getFullName());
    	if(!FetchGroup.BASIC.equals(fetchGroup)) {
    		bpiContact.setVcard(contact.getVcard());
    	}
    	bpiContact.setFirstName(contact.getFirstName());
    	bpiContact.setLastName(contact.getLastName());
    	bpiContact.setSalutationCode(contact.getSalutationCode());
    	bpiContact.setSalutation(contact.getSalutation());
    	bpiContact.setJobTitle(contact.getJobTitle());
		return bpiContact;
    }

    /**
     * Map address group to BpiAddressGroup.
     * 
     * @param addressGroup
     * @param bpiAddressGroup
     * @param fetchGroup
     * @return
     * @throws ServiceException
     */
    public BpiAddressGroup toBpiAddressGroup(
    	AddressGroup addressGroup,
    	BpiAddressGroup bpiAddressGroup,
    	String fetchGroup
    ) throws ServiceException {
    	this.toBpiObject(
    		addressGroup, 
    		bpiAddressGroup,
    		fetchGroup
    	);
    	bpiAddressGroup.setName(addressGroup.getName());
    	bpiAddressGroup.setDescription(addressGroup.getDescription());
    	return bpiAddressGroup;
    }

    /**
     * Map account filter to BpiAccountFilter.
     * 
     * @param accountFilter
     * @param bpiAccountFilter
     * @param fetchGroup
     * @return
     * @throws ServiceException
     */
    public BpiAccountFilter toBpiAccountFilter(
    	AccountFilterGlobal accountFilter,
    	BpiAccountFilter bpiAccountFilter,
    	String fetchGroup
    ) throws ServiceException {
    	this.toBpiObject(
    		accountFilter, 
    		bpiAccountFilter,
    		fetchGroup
    	);
    	bpiAccountFilter.setName(accountFilter.getName());
    	bpiAccountFilter.setDescription(accountFilter.getDescription());
    	return bpiAccountFilter;
    }

    /**
     * Map organization to BpiOrganization.
     * 
     * @param organization
     * @throws ServiceException
     * @throws IOException
     */
    public BpiOrganization toBpiOrganization(
    	LegalEntity organization,
    	BpiOrganization bpiOrganization,
    	String fetchGroup
    ) throws ServiceException {
    	this.toBpiAccount(
    		organization, 
    		bpiOrganization,
    		fetchGroup
    	);
    	bpiOrganization.setName(organization.getName());
    	bpiOrganization.setAliasName(organization.getAliasName());    	
    	bpiOrganization.setFullName(organization.getFullName());
    	if(!FetchGroup.BASIC.equals(fetchGroup)) {    	
    		bpiOrganization.setVcard(organization.getVcard());
    	}
		return bpiOrganization;
    }

    /**
     * Map activity to BpiActivity.
     * 
     * @param activity
     * @param bpiActivity
     * @param includeParticipants
     */
    public BpiActivity toBpiActivity(
    	Activity activity,
    	BpiActivity bpiActivity,
    	String fetchGroup
    ) throws ServiceException {
    	this.toBpiObject(
    		activity, 
    		bpiActivity,
    		fetchGroup
    	);
    	bpiActivity.setActivityNumber(activity.getActivityNumber());
    	bpiActivity.setName(activity.getName());
    	bpiActivity.setDescription(activity.getDescription());
    	bpiActivity.setAdditionalInformation(activity.getDetailedDescription());
    	bpiActivity.setScheduledStart(activity.getScheduledStart());
    	bpiActivity.setScheduledEnd(activity.getScheduledEnd());
    	bpiActivity.setActivityState(activity.getActivityState());
    	bpiActivity.setProcessState(activity.getProcessState() == null ? null : activity.getProcessState().getName());
    	bpiActivity.setLocation(activity.getLocation());
    	bpiActivity.setCategory(activity.getCategory());
    	if(!FetchGroup.BASIC.equals(fetchGroup)) {
	    	if(activity.getReportingContact() instanceof Contact) {
	    		bpiActivity.setReportingContact(
	    			this.toBpiContact(
	    				activity.getReportingContact(), 
	    				this.newBpiContact(),
	    				FetchGroup.DEFAULT
	    			)
	    		);
	    	}
	    	if(activity.getAssignedTo() instanceof Contact) {
	    		bpiActivity.setAssignedTo(
	    			this.toBpiContact(
	    				activity.getAssignedTo(), 
	    				this.newBpiContact(),
	    				FetchGroup.DEFAULT
	    			)
	    		);
	    	}
	    	if(activity.getReportingAccount() instanceof Contact) {
	    		bpiActivity.setReportingContact2(
	    			this.toBpiContact(
	    				(Contact)activity.getReportingAccount(), 
	    				this.newBpiContact(),
	    				FetchGroup.DEFAULT
	    			)
	    		);
	    	}
    	}
    	// Participants
    	if(FetchGroup.ALL.equals(fetchGroup)) {
	    	List<BpiParticipant> bpiParticipants = new ArrayList<BpiParticipant>();
			for(AbstractActivityParty party: Activities.getInstance().getActivityParties(activity)) {
				Account partyAccount = null;
				if(party instanceof TaskParty) {
					partyAccount = (Account)((TaskParty)party).getParty();
				} else if(party instanceof MeetingParty) {
					partyAccount = (Account)((MeetingParty)party).getParty();
				} else if(party instanceof IncidentParty) {
					partyAccount = (Account)((IncidentParty)party).getParty();
				}
				if(partyAccount != null) {
					BpiParticipant bpiParticipant = newBpiParticipant();
					this.toBpiObject(
						party, 
						bpiParticipant, 
						FetchGroup.DEFAULT
					);
					bpiParticipant.setPartyStatus(party.getPartyStatus());
					bpiParticipant.setPartyType(party.getPartyType());
					if(partyAccount instanceof Contact) {
						bpiParticipant.setAccount(
							this.toBpiContact(
								(Contact)partyAccount, 
								this.newBpiContact(),
								FetchGroup.DEFAULT
							)
						);
					} else if(partyAccount instanceof LegalEntity) {
						bpiParticipant.setAccount(
							this.toBpiOrganization(
								(LegalEntity)partyAccount, 
								this.newBpiOrganization(),
								FetchGroup.DEFAULT
							)
						);
					}
					bpiParticipants.add(bpiParticipant);
				}
			}
			bpiActivity.setParticipant(bpiParticipants);
    	}
		// Localized fields
    	if(!FetchGroup.BASIC.equals(fetchGroup)) {    	
			List<BpiLocalizedField> bpiLocalizedFields = new ArrayList<BpiLocalizedField>();
			for(LocalizedField localizedField: activity.<LocalizedField>getLocalizedField()) {			
				bpiLocalizedFields.add(
					this.toBpiLocalizedField(
						localizedField, 
						this.newBpiLocalizedField(),
						FetchGroup.DEFAULT
					)
				);
			}
			bpiActivity.setLocalizedField(bpiLocalizedFields);
    	}
		return bpiActivity;
    }

    /**
     * Map activityFollowUp to BpiActivityFollowUp.
     * 
     * @param activityFollowUp
     * @param bpiActivityFollowUp
     */
    public BpiActivityFollowUp toBpiActivityFollowUp(
    	ActivityFollowUp activityFollowUp,
    	BpiActivityFollowUp bpiActivityFollowUp,
    	String fetchGroup
    ) throws ServiceException {
    	this.toBpiObject(
    		activityFollowUp, 
    		bpiActivityFollowUp,
    		fetchGroup
    	);
    	bpiActivityFollowUp.setTitle(activityFollowUp.getTitle());
    	bpiActivityFollowUp.setText(activityFollowUp.getText());
    	bpiActivityFollowUp.setTransition(activityFollowUp.getTransition().getName());
		return bpiActivityFollowUp;
    }

    /**
     * Map member to BpiAccountMember.
     * 
     * @param member
     * @param bpiAccountMember
     * @return
     * @throws ServiceException
     */
    public BpiAccountMember toBpiAccountMember(
    	Member member,
    	BpiAccountMember bpiAccountMember,
    	String fetchGroup
    ) throws ServiceException {
		bpiAccountMember.setId(member.refGetPath().getLastSegment().toString());
		bpiAccountMember.setXri(member.refGetPath().toXRI());
		bpiAccountMember.setName(member.getAccount().getFullName());
		bpiAccountMember.setMemberRole(member.getMemberRole());
		if(member.getAccount() instanceof Contact) {
			bpiAccountMember.setAccount(
				this.toBpiContact(
					(Contact)member.getAccount(), 
					this.newBpiContact(),
					FetchGroup.BASIC
				)
			);
		} else if(member.getAccount() instanceof LegalEntity) {
			bpiAccountMember.setAccount(
				this.toBpiOrganization(
					(LegalEntity)member.getAccount(), 
					this.newBpiOrganization(),
					FetchGroup.BASIC
				)
			);
		}
		return bpiAccountMember;
    }

    /**
     * Map membership to BpiAccountMember.
     * 
     * @param membership
     * @param bpiAccountMember
     * @return
     * @throws ServiceException
     */
    public BpiAccountMember toBpiAccountMember(
    	AccountMembership membership,
    	BpiAccountMember bpiAccountMember,
    	String fetchGroup
    ) throws ServiceException {
		bpiAccountMember.setId(membership.refGetPath().getLastSegment().toString());
		bpiAccountMember.setXri(membership.refGetPath().toXRI());
		bpiAccountMember.setName(membership.getAccountTo().getFullName());
		bpiAccountMember.setMemberRole(membership.getMemberRole());
		if(membership.getAccountFrom() instanceof Contact) {
			bpiAccountMember.setAccount(
				this.toBpiContact(
					(Contact)membership.getAccountFrom(), 
					this.newBpiContact(),
					FetchGroup.BASIC
				)
			);
		} else if(membership.getAccountFrom() instanceof LegalEntity) {
			bpiAccountMember.setAccount(
				this.toBpiOrganization(
					(LegalEntity)membership.getAccountFrom(), 
					this.newBpiOrganization(),
					FetchGroup.BASIC
				)
			);
		}
		return bpiAccountMember;
    }

    /**
     * Map localized field to BpiLocalizedField.
     * 
     * @param localizedField
     * @param bpiLocalizedField
     * @return
     * @throws ServiceException
     */
    public BpiLocalizedField toBpiLocalizedField(
    	LocalizedField localizedField,
    	BpiLocalizedField bpiLocalizedField,
    	String fetchGroup
    ) throws ServiceException {
    	this.toBpiObject(
    		localizedField, 
    		bpiLocalizedField,
    		fetchGroup
    	);
    	bpiLocalizedField.setName(localizedField.getName());
    	bpiLocalizedField.setDescription(localizedField.getDescription());
    	bpiLocalizedField.setLocale(localizedField.getLocale());
    	bpiLocalizedField.setLocalizedValue(localizedField.getLocalizedValue());
    	return bpiLocalizedField;
    }

    /**
     * Find contacts with the given id.
     * 
     * @param path
     * @param pm
     * @return
     */
    public List<Contact> findContacts(
    	Path path,
    	PersistenceManager pm    	
    ) throws ServiceException {
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
    	contactQuery.thereExistsAliasName().equalTo(path.getLastSegment().toString());
    	contactQuery.orderByCreatedAt().ascending();
    	contactQuery.forAllDisabled().isFalse();
    	return accountSegment.getAccount(contactQuery);
    }

    /**
     * Find exporter tasks matching path component 6.
     * 
     * @param path
     * @param pm
     * @return
     * @throws ServiceException
     */
    public List<ExporterTask> findExporterTasks(
    	Path path,
    	PersistenceManager pm
    ) throws ServiceException {
    	String id = path.getSegment(6).toString();
    	org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = Workflows.getInstance().getWorkflowSegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	ExporterTask exporterTask = (ExporterTask)workflowSegment.getWfProcess(id);
    	if(exporterTask != null) {
    		return Collections.singletonList(exporterTask);
    	} else {
	    	ExporterTaskQuery exporterTaskQuery = (ExporterTaskQuery)pm.newQuery(ExporterTask.class);
	    	exporterTaskQuery.name().equalTo(path.getSegment(6).toString());
	    	return workflowSegment.getWfProcess(exporterTaskQuery);
    	}
    }

    /**
     * Find address groups with the given id.
     * 
     * @param path
     * @param pm
     * @return
     */
    public List<AddressGroup> findAddressGroups(
    	Path path,
    	PersistenceManager pm    	
    ) throws ServiceException {
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	AddressGroupQuery addressGroupQuery = (AddressGroupQuery)pm.newQuery(AddressGroup.class);
    	try {
    		addressGroupQuery.name().equalTo(URLDecoder.decode(path.getSegment(6).toString(), "UTF-8"));
    	} catch(Exception ignore) {}
    	addressGroupQuery.orderByCreatedAt().ascending();
    	addressGroupQuery.forAllDisabled().isFalse();
    	return activitySegment.getAddressGroup(addressGroupQuery);    	
    }

    /**
     * Find account filters with the given id.
     * 
     * @param path
     * @param pm
     * @return
     */
    public List<AccountFilterGlobal> findAccountFilters(
    	Path path,
    	PersistenceManager pm    	
    ) throws ServiceException {
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	AccountFilterGlobalQuery accountFilterQuery = (AccountFilterGlobalQuery)pm.newQuery(AccountFilterGlobal.class);
    	try {
    		accountFilterQuery.name().equalTo(URLDecoder.decode(path.getSegment(6).toString(), "UTF-8"));
    	} catch(Exception ignore) {}
    	accountFilterQuery.orderByCreatedAt().ascending();
    	accountFilterQuery.forAllDisabled().isFalse();
    	return accountSegment.getAccountFilter(accountFilterQuery);
    }

    /**
     * Find account members for given account.
     * 
     * @param account
     * @return
     */
    public List<Member> findAccountMembers(
    	Account account
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
		MemberQuery memberQuery = (MemberQuery)pm.newQuery(Member.class);
		memberQuery.forAllDisabled().isFalse();
		memberQuery.thereExistsAccount().forAllDisabled().isFalse();
		return account.getMember(memberQuery);
    }

    /**
     * Find legal entities with the given id.
     * 
     * @param path
     * @param pm
     * @return
     */
    public List<LegalEntity> findLegalEntities(
    	Path path,
    	PersistenceManager pm    	
    ) throws ServiceException {
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	LegalEntityQuery legalEntityQuery = (LegalEntityQuery)pm.newQuery(LegalEntity.class);
    	legalEntityQuery.name().equalTo(path.getLastSegment().toString());
    	legalEntityQuery.orderByCreatedAt().ascending();
    	legalEntityQuery.forAllDisabled().isFalse();
    	return accountSegment.getAccount(legalEntityQuery);    	
    }
    
    /**
     * Find activity creators with given name.
     * 
     * @param path
     * @param pm
     * @return
     */
    public List<ActivityCreator> findActivityCreators(
    	Path path,
    	PersistenceManager pm    	
    ) throws ServiceException {
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	ActivityCreatorQuery activityCreatorQuery = (ActivityCreatorQuery)pm.newQuery(ActivityCreator.class);
    	activityCreatorQuery.name().equalTo(path.getLastSegment().toString());
    	activityCreatorQuery.orderByCreatedAt().ascending();
    	activityCreatorQuery.forAllDisabled().isFalse();
    	return activitySegment.getActivityCreator(activityCreatorQuery);    	
    }

    /**
     * Find activity trackers with given name.
     * 
     * @param path
     * @param pm
     * @return
     */
    public List<ActivityTracker> findActivityTrackers(
    	Path path,
    	PersistenceManager pm    	
    ) throws ServiceException {
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	ActivityTrackerQuery activityTrackerQuery = (ActivityTrackerQuery)pm.newQuery(ActivityTracker.class);
    	try {
    		activityTrackerQuery.name().equalTo(URLDecoder.decode(path.getLastSegment().toString(), "UTF-8"));
    	} catch(Exception ignore) {}
    	activityTrackerQuery.orderByCreatedAt().ascending();
    	activityTrackerQuery.forAllDisabled().isFalse();
    	return activitySegment.getActivityTracker(activityTrackerQuery);    	
    }

    /**
     * Find activities with given number.
     * 
     * @param path
     * @param pm
     * @return
     */
    public List<Activity> findActivities(
    	Path path,
    	PersistenceManager pm    	
    ) throws ServiceException {
    	String id = path.getLastSegment().toString();
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, path.getSegment(2).toString(), path.getSegment(4).toString());
    	ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
    	activityQuery.thereExistsActivityNumber().equalTo(id);
    	activityQuery.orderByCreatedAt().ascending();
    	activityQuery.forAllDisabled().isFalse();
    	List<Activity> activities = activitySegment.getActivity(activityQuery);
    	if(!activities.isEmpty()) {
    		return activities;
    	}
    	activities = new ArrayList<org.opencrx.kernel.activity1.jmi1.Activity>();
    	try {
    		org.opencrx.kernel.activity1.jmi1.Activity activity = activitySegment.getActivity(id);
    		if(activity != null) {
    			activities.add(activity);
    		}
    	} catch(Exception ignore) {}
    	return activities;
    }

    /**
     * Merge an activity's detailed description with a new detailed description.
     * The default implementation does an append of the form old + '~ ~ ~' + newText
     * 
     * @param oldText
     * @param newText
     * @return
     */
    public String mergeActivityDetailedDescription(
    	String oldText,
    	String newText
    ) {
    	return
    		newText + "\n\n" +
    		"~ ~ ~\n" +
    		oldText;    		
    }
    
}
