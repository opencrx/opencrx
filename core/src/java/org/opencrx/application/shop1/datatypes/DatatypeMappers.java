/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: DatatypeMappers
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2009, CRIXP Corp., Switzerland
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

package org.opencrx.application.shop1.datatypes;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.application.shop1.cci2.ActivityFollowUpT;
import org.opencrx.application.shop1.cci2.ActivityT;
import org.opencrx.application.shop1.cci2.AmountT;
import org.opencrx.application.shop1.cci2.ContactT;
import org.opencrx.application.shop1.cci2.ContractPositionT;
import org.opencrx.application.shop1.cci2.ContractStatusT;
import org.opencrx.application.shop1.cci2.ContractT;
import org.opencrx.application.shop1.cci2.CredentialsT;
import org.opencrx.application.shop1.cci2.CustomerContractT;
import org.opencrx.application.shop1.cci2.CustomerHobbyAndInterestT;
import org.opencrx.application.shop1.cci2.CustomerStatusT;
import org.opencrx.application.shop1.cci2.CustomerT;
import org.opencrx.application.shop1.cci2.DeliveryInformationT;
import org.opencrx.application.shop1.cci2.DocumentT;
import org.opencrx.application.shop1.cci2.EmailAddressT;
import org.opencrx.application.shop1.cci2.InvoiceT;
import org.opencrx.application.shop1.cci2.LegalEntityT;
import org.opencrx.application.shop1.cci2.MessengerAddressT;
import org.opencrx.application.shop1.cci2.PhoneNumberT;
import org.opencrx.application.shop1.cci2.PostalAddressT;
import org.opencrx.application.shop1.cci2.PriceLevelT;
import org.opencrx.application.shop1.cci2.ProductBundleDataT;
import org.opencrx.application.shop1.cci2.ProductConfigurationT;
import org.opencrx.application.shop1.cci2.ProductConfigurationTypeT;
import org.opencrx.application.shop1.cci2.ProductDescriptionT;
import org.opencrx.application.shop1.cci2.ProductPhaseT;
import org.opencrx.application.shop1.cci2.ProductPriceT;
import org.opencrx.application.shop1.cci2.ProductStatusT;
import org.opencrx.application.shop1.cci2.ProductT;
import org.opencrx.application.shop1.cci2.RelatedProductT;
import org.opencrx.application.shop1.cci2.ReturnStatusT;
import org.opencrx.application.shop1.cci2.SalesOrderT;
import org.opencrx.application.shop1.cci2.StringPropertyT;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.LegalEntity;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.account1.jmi1.WebAddress;
import org.opencrx.kernel.activity1.cci2.ActivityFollowUpQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.address1.jmi1.PostalAddressable;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.base.cci2.PropertyQuery;
import org.opencrx.kernel.base.jmi1.PropertySet;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.code1.jmi1.CodeValueContainer;
import org.opencrx.kernel.contract1.jmi1.AbstractContract;
import org.opencrx.kernel.contract1.jmi1.AbstractInvoicePosition;
import org.opencrx.kernel.contract1.jmi1.AbstractSalesOrderPosition;
import org.opencrx.kernel.contract1.jmi1.AccountAssignmentContract;
import org.opencrx.kernel.contract1.jmi1.ContractAddress;
import org.opencrx.kernel.contract1.jmi1.DeliveryInformation;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.InvoicePosition;
import org.opencrx.kernel.contract1.jmi1.Lead;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.contract1.jmi1.SalesOrderPosition;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.Description;
import org.opencrx.kernel.product1.cci2.ProductFilterGlobalQuery;
import org.opencrx.kernel.product1.cci2.ProductQuery;
import org.opencrx.kernel.product1.jmi1.AbstractPriceLevel;
import org.opencrx.kernel.product1.jmi1.AbstractProductConfiguration;
import org.opencrx.kernel.product1.jmi1.ConfiguredProduct;
import org.opencrx.kernel.product1.jmi1.PricingRule;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.opencrx.kernel.product1.jmi1.ProductClassification;
import org.opencrx.kernel.product1.jmi1.ProductClassificationFilterProperty;
import org.opencrx.kernel.product1.jmi1.ProductConfiguration;
import org.opencrx.kernel.product1.jmi1.ProductConfigurationType;
import org.opencrx.kernel.product1.jmi1.ProductConfigurationTypeSet;
import org.opencrx.kernel.product1.jmi1.ProductFilterGlobal;
import org.opencrx.kernel.product1.jmi1.ProductFilterProperty;
import org.opencrx.kernel.product1.jmi1.ProductPhase;
import org.opencrx.kernel.product1.jmi1.RelatedProduct;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.openmdx.base.accessor.spi.IntegerMarshaller;
import org.openmdx.base.accessor.spi.ShortMarshaller;
import org.openmdx.base.collection.MarshallingList;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.spi2.Datatypes;

public class DatatypeMappers {

    //-----------------------------------------------------------------------
	public DatatypeMappers(
	) {
		
	}
	
    //-----------------------------------------------------------------------
    public static String bigDecimalToString(
        BigDecimal value
    ) {
        return value == null ? null : value.toPlainString();
    }
    
    //-----------------------------------------------------------------------
    public static Short toShort(
        Number number
    ) {
        return number == null ? null : new Short(number.shortValue());
    }
    
    //-----------------------------------------------------------------------
    public static Integer toInteger(
        Number number
    ) {
        return number == null ? null : new Integer(number.intValue());
    }
    
    //-----------------------------------------------------------------------
	public static List<Short> toShortList(
        List<?> values
    ) {
        return new MarshallingList<Short>(
            ShortMarshaller.NORMALIZE,
            values
        );
    }
    
    //-----------------------------------------------------------------------
	public static List<Integer> toIntegerList(
        List<?> values
    ) {
        return new MarshallingList<Integer>(
            IntegerMarshaller.NORMALIZE,
            values
        );
    }
    
    //-----------------------------------------------------------------------
    public AbstractContractFieldMapper newAbstractContractFieldMapper(
    ) {
    	return new AbstractContractFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public AbstractContractFieldMapper getAbstractContractFieldMapper(
    ) {
    	if(this.abstractContractFieldMapper == null) {
    		this.abstractContractFieldMapper = this.newAbstractContractFieldMapper();    		
    	}
    	return this.abstractContractFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public AccountFieldMapper newAccountFieldMapper(
    ) {
    	return new AccountFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public AccountFieldMapper getAccountFieldMapper(
    ) {
    	if(this.accountFieldMapper == null) {
    		this.accountFieldMapper = this.newAccountFieldMapper();    		
    	}
    	return this.accountFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public EmailAddressFieldMapper newEmailAddressFieldMapper(
    ) {
    	return new EmailAddressFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public EmailAddressFieldMapper getEmailAddressFieldMapper(
    ) {
    	if(this.emailAddressFieldMapper == null) {
    		this.emailAddressFieldMapper = this.newEmailAddressFieldMapper();    		
    	}
    	return this.emailAddressFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public InvoiceFieldMapper newInvoiceFieldMapper(
    ) {
    	return new InvoiceFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public InvoiceFieldMapper getInvoiceFieldMapper(
    ) {
    	if(this.invoiceFieldMapper == null) {
    		this.invoiceFieldMapper = this.newInvoiceFieldMapper();    		
    	}
    	return this.invoiceFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public LeadFieldMapper newLeadFieldMapper(
    ) {
    	return new LeadFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public LeadFieldMapper getLeadFieldMapper(
    ) {
    	if(this.leadFieldMapper == null) {
    		this.leadFieldMapper = this.newLeadFieldMapper();    		
    	}
    	return this.leadFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public PhoneNumberFieldMapper newPhoneNumberFieldMapper(
    ) {
    	return new PhoneNumberFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public PhoneNumberFieldMapper getPhoneNumberFieldMapper(
    ) {
    	if(this.phoneNumberFieldMapper == null) {
    		this.phoneNumberFieldMapper = this.newPhoneNumberFieldMapper();    		
    	}
    	return this.phoneNumberFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public PostalAddressFieldMapper newPostalAddressFieldMapper(
    ) {
    	return new PostalAddressFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public PostalAddressFieldMapper getPostalAddressFieldMapper(
    ) {
    	if(this.postalAddressFieldMapper == null) {
    		this.postalAddressFieldMapper = this.newPostalAddressFieldMapper();    		
    	}
    	return this.postalAddressFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public ProductFieldMapper newProductFieldMapper(
    ) {
    	return new ProductFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public ProductFieldMapper getProductFieldMapper(
    ) {
    	if(this.productFieldMapper == null) {
    		this.productFieldMapper = this.newProductFieldMapper();    		
    	}
    	return this.productFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public ProductFilterFieldMapper newProductFilterFieldMapper(
    ) {
    	return new ProductFilterFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public ProductFilterFieldMapper getProductFilterFieldMapper(
    ) {
    	if(this.productFilterFieldMapper == null) {
    		this.productFilterFieldMapper = this.newProductFilterFieldMapper();    		
    	}
    	return this.productFilterFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public SalesOrderFieldMapper newSalesOrderFieldMapper(
    ) {
    	return new SalesOrderFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public SalesOrderFieldMapper getSalesOrderFieldMapper(
    ) {
    	if(this.salesOrderFieldMapper == null) {
    		this.salesOrderFieldMapper = this.newSalesOrderFieldMapper();    		
    	}
    	return this.salesOrderFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public DeliveryInformationFieldMapper newDeliveryInformationFieldMapper(
    ) {
    	return new DeliveryInformationFieldMapper();
    }
    
    //-----------------------------------------------------------------------
    public DeliveryInformationFieldMapper getDeliveryInformationFieldMapper(
    ) {
    	if(this.deliveryInformationFieldMapper == null) {
    		this.deliveryInformationFieldMapper = this.newDeliveryInformationFieldMapper();
    	}
    	return this.deliveryInformationFieldMapper;
    }
    
    //-----------------------------------------------------------------------
    public void mapStringProperty(
        String name,
        CodeValueContainer domain,
        String value,
        StringProperty property
    ) {
        property.setName(name);
        property.setDomain(domain);
        property.setStringValue(value);        
    }
    
    //-----------------------------------------------------------------------
    public StringPropertyT mapStringProperty(
        StringProperty p
    ) {
        return Datatypes.create(
            StringPropertyT.class, 
            Datatypes.member(
                StringPropertyT.Member.name, 
                p.getName()
            ),
            Datatypes.member(
                StringPropertyT.Member.stringValue, 
                Arrays.asList(new String[]{p.getStringValue()})
            ),
            Datatypes.member(
                StringPropertyT.Member.domain, 
                p.getDomain() == null ? null : p.getDomain().getName()
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public List<StringPropertyT> mapPropertySet(
        PropertySet propertySet
    ) {
        if(propertySet == null) return null;
        List<StringPropertyT> propertiesT = new ArrayList<StringPropertyT>();
        org.opencrx.kernel.base.jmi1.BasePackage basePackage =
            (org.opencrx.kernel.base.jmi1.BasePackage)propertySet.refOutermostPackage().refPackage(
                "org:opencrx:kernel:base"
            );                        
        PropertyQuery query = basePackage.createPropertyQuery();
        query.orderByName().ascending();
        String currentName = null;
        List<String> currentValues = new ArrayList<String>();
        org.opencrx.kernel.base.cci2.Property currentProperty = null;
        for(org.opencrx.kernel.base.cci2.Property p: propertySet.getProperty(query)) {
            if(p instanceof StringProperty) {
                String propertyName = p.getName();
                if(propertyName.indexOf("[") > 0) {
                    propertyName = propertyName.substring(0, propertyName.indexOf("["));
                }
                if(!propertyName.equals(currentName)) {
                    if(currentName != null) {
                        propertiesT.add(
                            Datatypes.create(
                                StringPropertyT.class, 
                                Datatypes.member(
                                    StringPropertyT.Member.name, 
                                    currentName
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue, 
                                    currentValues
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.domain, 
                                    currentProperty.getDomain() == null || currentProperty.getDomain().getName().isEmpty() 
                                        ? null 
                                        : currentProperty.getDomain().getName().iterator().next()
                                )
                            )
                        );
                    }
                    currentName = propertyName;
                    currentProperty = p;
                    currentValues.clear();
                }
                String stringValue = ((StringProperty)p).getStringValue();
                if(stringValue != null) {
                    currentValues.add(stringValue);
                }
            }
        }
        if(currentProperty != null) {
            propertiesT.add(
                Datatypes.create(
                    StringPropertyT.class, 
                    Datatypes.member(
                        StringPropertyT.Member.name, 
                        currentName
                    ),
                    Datatypes.member(
                        StringPropertyT.Member.stringValue, 
                        currentValues
                    ),
                    Datatypes.member(
                        StringPropertyT.Member.domain, 
                        currentProperty.getDomain() == null || currentProperty.getDomain().getName().isEmpty() 
                            ? null 
                            : currentProperty.getDomain().getName().iterator().next()
                    )
                )
            );
        }
        return propertiesT;
    }
    
    //-----------------------------------------------------------------------
    public CustomerT mapCustomer(
        Account customer,
        List<Lead> customerContracts,
        String shopCategory
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(customer);
    	if(customer instanceof LegalEntity) {
	        String customerNumber = this.getAccountFieldMapper().getAccountNumber(customer);     
	        PropertySet genericData = null;
	        Collection<org.opencrx.kernel.generic.jmi1.PropertySet> propertySets = customer.getPropertySet();
	        for(org.opencrx.kernel.generic.jmi1.PropertySet propertySet: propertySets) {
	            if(propertySet.getName().equals(PropertySetName.GenericData.toString())) {
	                genericData = propertySet;
	            }
	        }
	        // Primary contact
	        org.opencrx.kernel.account1.jmi1.Account primaryContact = null;
        	MemberQuery memberQuery = (MemberQuery)pm.newQuery(Member.class);
        	if(shopCategory != null) {
        		memberQuery.thereExistsCategory().equalTo(shopCategory);
        	}
        	memberQuery.thereExistsMemberRole().equalTo(Accounts.MEMBER_ROLE_EMPLOYEE);
        	List<Member> members = customer.getMember(memberQuery);
        	if(!members.isEmpty()) {
        		primaryContact = members.iterator().next().getAccount();
        	}	        
	        // Get main addresses
	        org.opencrx.kernel.account1.jmi1.AccountAddress[] addresses = org.opencrx.kernel.backend.Accounts.getInstance().getMainAddresses(customer);	        
	        LegalEntity legalEntity = (LegalEntity)customer;
	        // Customer contracts
	        List<CustomerContractT> customerContractsT = new ArrayList<CustomerContractT>();
	        if(customerContracts != null) {
		        for(Lead customerContract: customerContracts) {
		        	customerContractsT.add(
		        		this.mapCustomerContract(customerContract)
		        	);
		        }
	        }
	        return Datatypes.create(
	            CustomerT.class,
	            Datatypes.member(
	                CustomerT.Member.customerNumber,
	                customerNumber
	            ),
	            Datatypes.member(
	                CustomerT.Member.externalId,
	                customer.getExternalLink()
	            ),                    
	            Datatypes.member(
	            	CustomerT.Member.accountRating,
	                toInteger(legalEntity.getAccountRating())
	            ),
	            Datatypes.member(
	            	CustomerT.Member.accountCategory,
	                toIntegerList(legalEntity.getAccountCategory())
	            ),
	            Datatypes.member(
	            	CustomerT.Member.legalEntity,
	            	Datatypes.create(
	            		LegalEntityT.class,	            	
			            Datatypes.member(
			            	LegalEntityT.Member.legalName,
			                legalEntity.getName()
			            ),                    
			            Datatypes.member(
			            	LegalEntityT.Member.postalAddressBusiness,
			                this.mapPostalAddress((PostalAddress)addresses[Accounts.POSTAL_BUSINESS])
			            ),
			            Datatypes.member(
			            	LegalEntityT.Member.emailAddressBusiness,
			                this.mapEmailAddress((EMailAddress)addresses[Accounts.MAIL_BUSINESS])
			            ),
			            Datatypes.member(
			            	LegalEntityT.Member.phoneNumberBusiness,
			                this.mapPhoneNumber((PhoneNumber)addresses[Accounts.PHONE_BUSINESS])
			            ),
			            Datatypes.member(
			            	LegalEntityT.Member.faxNumberBusiness,
			                this.mapPhoneNumber((PhoneNumber)addresses[Accounts.FAX_BUSINESS])
			            ),
			            Datatypes.member(
			            	LegalEntityT.Member.webAddressBusiness,
			                addresses[Accounts.WEB_BUSINESS] == null ? null : ((WebAddress)addresses[Accounts.WEB_BUSINESS]).getWebUrl()
			            ),
			            Datatypes.member(
			            	LegalEntityT.Member.primaryContactNumber,
			            	primaryContact == null ? null : this.getAccountFieldMapper().getAccountNumber(primaryContact)
			            )
			        )
			    ),
	            Datatypes.member(
	                CustomerT.Member.customerStatus,
	                this.mapCustomerStatus(customer)
	            ),
	            Datatypes.member(
	                CustomerT.Member.customerContract,
	                customerContractsT
	            ),
	            Datatypes.member(
	                CustomerT.Member.genericData,
	                this.mapPropertySet(genericData)
	            )
	        );
    	}
    	else {
    		Contact contact = (Contact)customer;
	        // Get main addresses
	        org.opencrx.kernel.account1.jmi1.AccountAddress[] addresses = org.opencrx.kernel.backend.Accounts.getInstance().getMainAddresses(customer);
	        // Get messenger addresses (= PhoneNumbers with usage OTHER
	        List<MessengerAddressT> messengerAddresses = new ArrayList<MessengerAddressT>();
	        Collection<AccountAddress> accountAddresses = customer.getAddress();
	        for(AccountAddress address: accountAddresses) {
	            if(
	                (address instanceof PhoneNumber) &&
	                address.getUsage().contains(Addresses.USAGE_OTHER)
	            ) {
	                messengerAddresses.add(
	                    mapMessengerAddress((PhoneNumber)address)
	                );
	            }
	        }
	        String customerNumber = this.getAccountFieldMapper().getAccountNumber(customer);     
	        PropertySet genericData = null;
	        PropertySet bookmarks = null;
	        Collection<org.opencrx.kernel.generic.jmi1.PropertySet> propertySets = customer.getPropertySet();
	        for(org.opencrx.kernel.generic.jmi1.PropertySet propertySet: propertySets) {
	            if(propertySet.getName().equals(PropertySetName.GenericData.toString())) {
	                genericData = propertySet;
	            }
	            else if(propertySet.getName().equals(PropertySetName.Bookmarks.toString())) {
	                bookmarks = propertySet;
	            }
	        }
	        // Customer contracts
	        List<CustomerContractT> customerContractsT = new ArrayList<CustomerContractT>();
	        if(customerContracts != null) {
		        for(Lead customerContract: customerContracts) {
		        	customerContractsT.add(
		        		this.mapCustomerContract(customerContract)
		        	);
		        }
	        }
	        return Datatypes.create(
	            CustomerT.class,
	            Datatypes.member(
	                CustomerT.Member.customerNumber,
	                customerNumber
	            ),
	            Datatypes.member(
	                CustomerT.Member.externalId,
	                customer.getExternalLink()
	            ),                    
	            Datatypes.member(
	            	CustomerT.Member.accountRating,
	                toInteger(contact.getAccountRating())
	            ),
	            Datatypes.member(
	            	CustomerT.Member.accountCategory,
	                toIntegerList(contact.getAccountCategory())
	            ),
	            Datatypes.member(
	            	CustomerT.Member.contact,
	            	Datatypes.create(
	            		ContactT.class,	            	
			            Datatypes.member(
			                ContactT.Member.organization,
			                contact.getOrganization()
			            ),
			            Datatypes.member(
			                ContactT.Member.salutationCode,
			                toInteger(contact.getSalutationCode())
			            ),                    
			            Datatypes.member(
			                ContactT.Member.salutation,
			                contact.getSalutation()
			            ),                    
			            Datatypes.member(
			            	ContactT.Member.title,
			                this.getAccountFieldMapper().getTitle(customer)
			            ),                    
			            Datatypes.member(
			            	ContactT.Member.firstName,
			                contact.getFirstName()
			            ),                    
			            Datatypes.member(
			            	ContactT.Member.lastName,
			                contact.getLastName()
			            ),               
			            Datatypes.member(
			            	ContactT.Member.middleName,
			                contact.getMiddleName()
			            ),                    
			            Datatypes.member(
			            	ContactT.Member.nickName,
			                contact.getNickName()
			            ),                    
			            Datatypes.member(
			            	ContactT.Member.birthDate,
			                contact.getBirthdate()
			            ),                    
			            Datatypes.member(
			            	ContactT.Member.placeOfBirth,
			                this.getAccountFieldMapper().getPlaceOfBirth(customer)
			            ),                    
			            Datatypes.member(
			            	ContactT.Member.birthDateIsValidated,
			                this.getAccountFieldMapper().getBirthDateIsValidated(customer)
			            ),               
			            Datatypes.member(
			            	ContactT.Member.doNotPostalMail,
			                contact.isDoNotPostalMail()
			            ),               
			            Datatypes.member(
			            	ContactT.Member.doNotEmail,
			                contact.isDoNotEMail()
			            ),               
			            Datatypes.member(
			            	ContactT.Member.doNotPhone,
			                contact.isDoNotPhone()
			            ),               
			            Datatypes.member(
			            	ContactT.Member.doNotFax,
			                contact.isDoNotFax()
			            ),               
			            Datatypes.member(
			            	ContactT.Member.nativeLanguage,
			                this.getAccountFieldMapper().getNativeLanguage(customer)
			            ),               
			            Datatypes.member(
			            	ContactT.Member.preferredSpokenLanguage,
			                toInteger(contact.getPreferredSpokenLanguage())
			            ),               
			            Datatypes.member(
			            	ContactT.Member.preferredWrittenLanguage,
			                toInteger(contact.getPreferredWrittenLanguage())
			            ),               
			            Datatypes.member(
			            	ContactT.Member.gender,
			                toInteger(contact.getGender())
			            ),               
			            Datatypes.member(
			            	ContactT.Member.jobRole,
			                this.getAccountFieldMapper().getJobRole(customer)
			            ),               
			            Datatypes.member(
			            	ContactT.Member.jobTitle,
			                contact.getJobTitle()
			            ),                                   
			            Datatypes.member(
			            	ContactT.Member.citizenship,
			                toIntegerList(contact.getCitizenship())
			            ),
			            Datatypes.member(
			            	ContactT.Member.education,
			                toInteger(contact.getEducation())
			            ),
			            Datatypes.member(
			            	ContactT.Member.annualIncomeAmount,
			                this.getAccountFieldMapper().getAnnualIncomeAmount(customer) 
			            ),               
			            Datatypes.member(
			            	ContactT.Member.annualIncomeCurrency,
			                this.getAccountFieldMapper().getAnnualIncomeCurrency(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.monthlyIncomeAmount,
			                this.getAccountFieldMapper().getMonthlyIncomeAmount(customer) 
			            ),               
			            Datatypes.member(
			            	ContactT.Member.monthlyIncomeCurrency,
			                this.getAccountFieldMapper().getMonthlyIncomeCurrency(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.numberOfChildren,
			                toInteger(contact.getNumberOfChildren())
			            ),
			            Datatypes.member(
			            	ContactT.Member.childrenNames,
			                contact.getChildrenNames()
			            ),
			            Datatypes.member(
			            	ContactT.Member.familyStatus,
			                toInteger(contact.getFamilyStatus())
			            ),
			            Datatypes.member(
			            	ContactT.Member.preferredContactMethod,
			                toInteger(contact.getPreferredContactMethod())
			            ),
			            Datatypes.member(
			            	ContactT.Member.religion,
			                toIntegerList(contact.getReligion())
			            ),
			            Datatypes.member(
			            	ContactT.Member.postalAddressHome,
			                this.mapPostalAddress((PostalAddress)addresses[Accounts.POSTAL_HOME])
			            ),
			            Datatypes.member(
			            	ContactT.Member.emailAddressHome,
			            	this.mapEmailAddress((EMailAddress)addresses[Accounts.MAIL_HOME])
			            ),
			            Datatypes.member(
			            	ContactT.Member.faxNumberHome,
			            	this.mapPhoneNumber((PhoneNumber)addresses[Accounts.FAX_HOME])
			            ),
			            Datatypes.member(
			            	ContactT.Member.phoneNumberHome,
			            	this.mapPhoneNumber((PhoneNumber)addresses[Accounts.PHONE_HOME])
			            ),
			            Datatypes.member(
			            	ContactT.Member.webAddressHome,
			                addresses[Accounts.WEB_HOME] == null ? null : ((WebAddress)addresses[Accounts.WEB_HOME]).getWebUrl()                              
			            ),
			            Datatypes.member(
			            	ContactT.Member.postalAddressBusiness,
			            	this.mapPostalAddress((PostalAddress)addresses[Accounts.POSTAL_BUSINESS])
			            ),
			            Datatypes.member(
			            	ContactT.Member.emailAddressBusiness,
			            	this.mapEmailAddress((EMailAddress)addresses[Accounts.MAIL_BUSINESS])
			            ),
			            Datatypes.member(
			            	ContactT.Member.phoneNumberBusiness,
			            	this.mapPhoneNumber((PhoneNumber)addresses[Accounts.PHONE_BUSINESS])
			            ),
			            Datatypes.member(
			            	ContactT.Member.faxNumberBusiness,
			            	this.mapPhoneNumber((PhoneNumber)addresses[Accounts.FAX_BUSINESS])
			            ),
			            Datatypes.member(
			            	ContactT.Member.phoneNumberMobile,
			            	this.mapPhoneNumber((PhoneNumber)addresses[Accounts.MOBILE])
			            ),
			            Datatypes.member(
			            	ContactT.Member.messengerAddress,
			                messengerAddresses
			            ),
			            Datatypes.member(
			            	ContactT.Member.webAddressBusiness,
			                addresses[Accounts.WEB_BUSINESS] == null ? null : ((WebAddress)addresses[Accounts.WEB_BUSINESS]).getWebUrl()
			            ),
			            Datatypes.member(
			            	ContactT.Member.blogAddress,
			                this.getAccountFieldMapper().getBlogAddress(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.communityStatus,
			                this.getAccountFieldMapper().getCommunityStatus(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.commerceStatus,
			                this.getAccountFieldMapper().getCommerceStatus(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.personsInHousehold,
			                this.getAccountFieldMapper().getPersonsInHousehold(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.professionalSkills,
			                this.getAccountFieldMapper().getProfessionalSkills(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.internetUsage,
			                this.getAccountFieldMapper().getInternetUsage(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.internetProvider,
			                this.getAccountFieldMapper().getInternetProvider(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.pcUsage,
			                this.getAccountFieldMapper().getPcUsage(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.portalRating,
			                this.getAccountFieldMapper().getPortalRating(customer)
			            ),
			            Datatypes.member(
			            	ContactT.Member.hobbyAndInterest,
			            	this.mapCustomerHobbyAndInterst(contact)
			            ),
			            Datatypes.member(
			            	ContactT.Member.bookmarks,
			            	this.mapPropertySet(bookmarks)
			            )
			        )
			    ),
	            Datatypes.member(
	                CustomerT.Member.customerStatus,
	                this.mapCustomerStatus(customer)
	            ),
	            Datatypes.member(
	                CustomerT.Member.customerContract,
	                customerContractsT
	            ),
	            Datatypes.member(
	                CustomerT.Member.genericData,
	                this.mapPropertySet(genericData)
	            )
	        );
    	}
    }
    
    //-----------------------------------------------------------------------
    public CustomerHobbyAndInterestT mapCustomerHobbyAndInterst(
        Contact contact
    ) {       
        return Datatypes.create(
            CustomerHobbyAndInterestT.class,
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.sports,
                this.getAccountFieldMapper().getSports(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.travel,
                this.getAccountFieldMapper().getTravel(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.finance,
                this.getAccountFieldMapper().getFinance(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.computerInternet,
                this.getAccountFieldMapper().getComputerInternet(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.telecommunication,
                this.getAccountFieldMapper().getTelecommunication(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.entertainment,
                this.getAccountFieldMapper().getEntertainment(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.music,
                this.getAccountFieldMapper().getMusic(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.lifestyle,
                this.getAccountFieldMapper().getLifestyle(contact)
            ),
            Datatypes.member(
                CustomerHobbyAndInterestT.Member.other,
                this.getAccountFieldMapper().getOther(contact)
            )
        );        
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT mapOperationStatus(
        Exception e
    ) {
        return Datatypes.create(
            ReturnStatusT.class,
            Datatypes.member(
                ReturnStatusT.Member.returnCode,
                BasicException.Code.GENERIC
            ),
            Datatypes.member(
                ReturnStatusT.Member.returnParams,
                new String[]{e.getClass().getName(), e.getMessage() == null ? "" : e.getMessage()}
            )               
        );
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT mapOperationStatus(
        int exceptionCode,
        String[] parameters
    ) {
    	List<Object> returnParams = new ArrayList<Object>();
    	if(parameters != null) {
    		for(Object param: returnParams) {
    			if(param != null) {
    				returnParams.add(param);
    			}
    		}
    	}
        return Datatypes.create(
            ReturnStatusT.class,
            Datatypes.member(
                ReturnStatusT.Member.returnCode,
                exceptionCode
            ),
            Datatypes.member(
                ReturnStatusT.Member.returnParams,
                returnParams
            )               
        );
    }
    
    //-----------------------------------------------------------------------
    public List<String> mapProductClassifications(
        List<ProductClassification> classifications
    ) {
        List<String> classificationIds = new ArrayList<String>();
        for(ProductClassification classification: classifications) {
            classificationIds.add(classification.getName());
        }
        return classificationIds;
    }
    
    //-----------------------------------------------------------------------
    public ProductConfigurationT mapProductConfiguration(
        AbstractProductConfiguration configuration
    ) {
        return Datatypes.create(
            ProductConfigurationT.class,
            Datatypes.member(
                ProductConfigurationT.Member.propertySetName,
                configuration.getName()
            ),
            Datatypes.member(
                ProductConfigurationT.Member.property,
                mapPropertySet(configuration)
            )            
        );        
    }
    
    //-----------------------------------------------------------------------
    public ProductConfigurationTypeT mapProductConfigurationTypeSet(
         ProductConfigurationTypeSet configurationTypeSet
    ) {
        List<ProductConfigurationT> configurationsT = new ArrayList<ProductConfigurationT>();
        Collection<ProductConfigurationType> configurationTypes = configurationTypeSet.getConfigurationType();
        for(ProductConfigurationType configurationType: configurationTypes) {
            configurationsT.add(
                mapProductConfiguration(configurationType)
            );                     
        }
        return Datatypes.create(
            ProductConfigurationTypeT.class,
            Datatypes.member(
                ProductConfigurationTypeT.Member.name,
                configurationTypeSet.getName()
            ),
            Datatypes.member(
                ProductConfigurationTypeT.Member.configuration,
                configurationsT
            )            
        );        
    }
    
    //-----------------------------------------------------------------------
    public List<String> mapUoms(
        List<Uom> uoms
    ) {
        List<String> uomNames = new ArrayList<String>();
        for(Uom uom: uoms) {
            uomNames.add(uom.getName());
        }
        return uomNames;
    }
    
    //-----------------------------------------------------------------------
    public List<ProductDescriptionT> mapProductDescriptions(
        Collection<Description> descriptions
    ) {
        List<ProductDescriptionT> descriptionsT = new ArrayList<ProductDescriptionT>();
        for(Description description: descriptions) {
            descriptionsT.add(
                Datatypes.create(
                    ProductDescriptionT.class, 
                    Datatypes.member(
                        ProductDescriptionT.Member.language,
                        toInteger(description.getLanguage())
                    ),
                    Datatypes.member(
                        ProductDescriptionT.Member.description,
                        description.getDescription()
                    ),
                    Datatypes.member(
                        ProductDescriptionT.Member.detailedDescription,
                        description.getDetailedDescription()
                    )                    
                )
            );
        }
        return descriptionsT;
    }
    
    //-----------------------------------------------------------------------
    public PostalAddressT mapPostalAddress(
        PostalAddressable address
    ) {
        if(address == null) return null;
        return Datatypes.create(
            PostalAddressT.class, 
            Datatypes.member(
                PostalAddressT.Member.postalAddressLine0,
                address.getPostalAddressLine().size() >= 1 ? address.getPostalAddressLine().get(0) : null
            ),                            
            Datatypes.member(
                PostalAddressT.Member.postalAddressLine1,
                address.getPostalAddressLine().size() >= 2 ? address.getPostalAddressLine().get(1) : null
            ),
            Datatypes.member(
                PostalAddressT.Member.postalAddressLine2,
                address.getPostalAddressLine().size() >= 3 ? address.getPostalAddressLine().get(2) : null
            ),                            
            Datatypes.member(
                PostalAddressT.Member.postalStreet0,
                address.getPostalStreet().size() >= 1 ? address.getPostalStreet().get(0) : null
            ),                           
            Datatypes.member(
                PostalAddressT.Member.postalStreet1,
                address.getPostalStreet().size() >= 2 ? address.getPostalStreet().get(1) : null
            ),                
            Datatypes.member(
                PostalAddressT.Member.postalStreetNumber,
                this.getPostalAddressFieldMapper().getPostalStreetNumber(address)
            ),                 
            Datatypes.member(
                PostalAddressT.Member.postalCity,
                address.getPostalCity()
            ),                  
            Datatypes.member(
                PostalAddressT.Member.postalCountry,
                toInteger(address.getPostalCountry())
            ),              
            Datatypes.member(
                PostalAddressT.Member.postalCode,
                address.getPostalCode()
            )                            
        );                                            
    }
    
    //-----------------------------------------------------------------------
    public ContractPositionT mapContractPosition(            
        SalesContractPosition position,
        String contractNumber,
        Date paymentDate,
        Product product
    ) {
    	List<DeliveryInformationT> deliveryInformationsT = new ArrayList<DeliveryInformationT>();
    	Collection<DeliveryInformation> deliveryInformations = position.getDeliveryInformation();
    	for(DeliveryInformation deliveryInformation: deliveryInformations) {
    		deliveryInformationsT.add(
    			this.mapDeliveryInformation(deliveryInformation)
    		);
    	}
    	ProductConfiguration actualProductConfiguration = null;
    	if(position instanceof ConfiguredProduct) {
    		ConfiguredProduct configuredProduct = (ConfiguredProduct)position;
    		actualProductConfiguration = configuredProduct.getCurrentConfig();
    	}
        return Datatypes.create(
            ContractPositionT.class,
            Datatypes.member(
                ContractPositionT.Member.contractNumber,
                contractNumber
            ),
            Datatypes.member(
                ContractPositionT.Member.externalLink,
                position instanceof CrxObject ? 
                	((CrxObject)position).getExternalLink() : 
                		Collections.emptyList()
            ),
            Datatypes.member(
                ContractPositionT.Member.paymentDate,
                paymentDate
            ),
            Datatypes.member(
                ContractPositionT.Member.positionNumber,
                position.getPositionNumber()
            ),
            Datatypes.member(
                ContractPositionT.Member.productNumber,
                product.getProductNumber()
            ),
            Datatypes.member(
                ContractPositionT.Member.salesTaxType,
                position.getSalesTaxType() == null ? 
                	null : 
                		position.getSalesTaxType().getName()
            ),
            Datatypes.member(
                ContractPositionT.Member.quantity,
                bigDecimalToString(position.getQuantity())
            ),
            Datatypes.member(
                ContractPositionT.Member.pricePerUnit,
                bigDecimalToString(position.getPricePerUnit())
            ),
            Datatypes.member(
                ContractPositionT.Member.baseAmount,
                bigDecimalToString(position.getBaseAmount())
            ),
            Datatypes.member(
                ContractPositionT.Member.taxAmount,
                bigDecimalToString(position.getTaxAmount())
            ),
            Datatypes.member(
                ContractPositionT.Member.amount,
                bigDecimalToString(position.getAmount())
            ),
            Datatypes.member(
                ContractPositionT.Member.pricingDate,
                position.getPricingDate()
            ),
            Datatypes.member(
                ContractPositionT.Member.priceUom,
                position.getPriceUom().getName()
            ),
            Datatypes.member(
                ContractPositionT.Member.positionName,
                position.getName()
            ),
            Datatypes.member(
                ContractPositionT.Member.discount,
                bigDecimalToString(position.getDiscount())
            ),
            Datatypes.member(
                ContractPositionT.Member.discountIsPercentage,
                position.isDiscountIsPercentage()
            ),
            Datatypes.member(
                ContractPositionT.Member.discountAmount,
                bigDecimalToString(position.getDiscountAmount())
            ),
            Datatypes.member(
                ContractPositionT.Member.actualProductConfiguration,
                actualProductConfiguration == null ? 
                	null : 
                		this.mapProductConfiguration(actualProductConfiguration)
            ),
            Datatypes.member(
                ContractPositionT.Member.deliveryInformation,
                deliveryInformationsT
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public ContractT mapContract(
        SalesContract contract,
        List<String> externalLink,
        List<ContractPositionT> positions
    ) {
        PostalAddressT postalAddressDeliveryT = null;
        PostalAddressT postalAddressInvoiceT = null;
        Collection<ContractAddress> addresses = contract.getAddress();
        for(ContractAddress address: addresses) {
            if(
                (address instanceof PostalAddressable) &&
                (!address.getUsage().isEmpty()) && 
                (address.getUsage().get(0).compareTo(Addresses.USAGE_CONTRACT_DELIVERY) == 0)
            ) {
                postalAddressDeliveryT = mapPostalAddress((PostalAddressable)address);
            }
            else if(
                (address instanceof PostalAddressable) &&
                (!address.getUsage().isEmpty()) && 
                (address.getUsage().get(0).compareTo(Addresses.USAGE_CONTRACT_INVOICE) == 0)
            ) {
                postalAddressInvoiceT = mapPostalAddress((PostalAddressable)address);
            }
        }
        return Datatypes.create(
            ContractT.class,
            Datatypes.member(
                ContractT.Member.contractNumber,
                contract.getContractNumber()
            ),
            Datatypes.member(
                ContractT.Member.externalId,
                externalLink
            ),            
            Datatypes.member(
                ContractT.Member.customerNumber,
                this.getAccountFieldMapper().getAccountNumber(contract.getCustomer())
            ),            
            Datatypes.member(
                ContractT.Member.activeOn,
                contract.getActiveOn()
            ),            
            Datatypes.member(
                ContractT.Member.expiresOn,
                contract.getExpiresOn()
            ),            
            Datatypes.member(
                ContractT.Member.cancelOn,
                contract.getCancelOn()
            ),            
            Datatypes.member(
                ContractT.Member.closedOn,
                contract.getClosedOn()
            ),            
            Datatypes.member(
                ContractT.Member.isGift,
                contract.isGift()
            ),            
            Datatypes.member(
                ContractT.Member.giftMessage,
                contract.getGiftMessage()
            ),
            Datatypes.member(
                ContractT.Member.contractCurrency,
                toInteger(contract.getContractCurrency())
            ),            
            Datatypes.member(
                ContractT.Member.totalBaseAmount,
                bigDecimalToString(contract.getTotalBaseAmount())
            ),            
            Datatypes.member(
                ContractT.Member.totalDiscountAmount,
                bigDecimalToString(contract.getTotalDiscountAmount())
            ),            
            Datatypes.member(
                ContractT.Member.totalAmount,
                bigDecimalToString(contract.getTotalAmount())
            ),
            Datatypes.member(
                ContractT.Member.totalTaxAmount,
                bigDecimalToString(contract.getTotalTaxAmount())
            ),            
            Datatypes.member(
                ContractT.Member.totalAmountIncludingTax,
                bigDecimalToString(contract.getTotalAmountIncludingTax())
            ),            
            Datatypes.member(
                ContractT.Member.position,
                positions
            ),
            Datatypes.member(
                ContractT.Member.contractStatus,
                mapContractStatus(contract)
            ),
            Datatypes.member(
                ContractT.Member.postalAddressDelivery,
                postalAddressDeliveryT
            ),
            Datatypes.member(
                ContractT.Member.postalAddressInvoice,
                postalAddressInvoiceT
            ),            
            Datatypes.member(
                ContractT.Member.billingPartner,
                this.getAbstractContractFieldMapper().getBillingPartner(contract)
            ),            
            Datatypes.member(
                ContractT.Member.billingPartnerRegistrationId,
                this.getAbstractContractFieldMapper().getBillingPartnerRegistrationId(contract)
            )            
        );
    }
    
    //-----------------------------------------------------------------------
    public InvoiceT mapInvoice(
        Invoice invoice
    ) {
        Collection<AbstractInvoicePosition> positions = invoice.getPosition();
        return Datatypes.create(
            InvoiceT.class,
            Datatypes.member(
                InvoiceT.Member.contract,
                mapContract(
                    invoice,
                    invoice.getExternalLink(),
                    mapInvoicePositions(
                        invoice.getContractNumber(),
                        this.getInvoiceFieldMapper().getPaymentDate(invoice),
                        positions
                    )
                )
            ),
            Datatypes.member(
                InvoiceT.Member.isVoucher,
                this.getInvoiceFieldMapper().isVoucher(invoice)
            ),
            Datatypes.member(
                InvoiceT.Member.paymentDate,
                this.getInvoiceFieldMapper().getPaymentDate(invoice)
            )            
        );
    }
    
    //-----------------------------------------------------------------------
    public void mapSalesOrder(
        SalesOrder salesOrder,
        SalesContract origin,
        Account customer,
        String invoiceNumber,
        SalesOrderState salesOrderState,
        Integer contractCurrency,
        Date activeOn,
        Date expiresOn,
        Date cancelOn,
        Date closedOn,
        Boolean isGift,
        String giftMessage,
        PricingRule defaultPricingRule
    ) {
        salesOrder.setName(invoiceNumber + " / " + this.getAccountFieldMapper().getAccountNumber(customer));
        salesOrder.setOrigin(origin);
        salesOrder.setCustomer(customer);
        salesOrder.setContractState(toShort(salesOrderState.getValue()));
        salesOrder.setContractCurrency(contractCurrency == null ? origin.getContractCurrency() : contractCurrency.shortValue());
        salesOrder.setActiveOn(activeOn);
        salesOrder.setExpiresOn(expiresOn);
        salesOrder.setCancelOn(cancelOn);
        salesOrder.setClosedOn(closedOn);
        salesOrder.setGift(Boolean.TRUE.equals(isGift));
        salesOrder.setGiftMessage(giftMessage);
        this.getAbstractContractFieldMapper().setBillingPartner(salesOrder, this.getAbstractContractFieldMapper().getBillingPartner(origin));
        this.getAbstractContractFieldMapper().setBillingPartnerRegistrationId(salesOrder, this.getAbstractContractFieldMapper().getBillingPartnerRegistrationId(origin));
        salesOrder.setPricingRule(
            origin.getPricingRule() == null ? 
            	defaultPricingRule : 
            	origin.getPricingRule()
        );
        salesOrder.setCalcRule(origin.getCalcRule());
        salesOrder.setContractLanguage(origin.getContractLanguage());
        salesOrder.setContractNumber(invoiceNumber);
    }
    
    //-----------------------------------------------------------------------
    public void mapInvoice(
        Invoice invoice,
        SalesContract origin,
        Account customer,
        String invoiceNumber,
        Boolean isVoucher,
        InvoiceState invoiceState,
        PricingRule defaultPricingRule
    ) {
        invoice.setName(invoiceNumber + " / " + this.getAccountFieldMapper().getAccountNumber(customer));
        invoice.setOrigin(origin);
        invoice.setCustomer(customer);
        invoice.setContractState(toShort(invoiceState.getValue()));
        invoice.setContractCurrency(origin.getContractCurrency());
        invoice.setActiveOn(origin.getActiveOn());
        invoice.setExpiresOn(origin.getExpiresOn());
        invoice.setCancelOn(origin.getCancelOn());
        invoice.setClosedOn(origin.getClosedOn());        
        this.getAbstractContractFieldMapper().setBillingPartner(invoice, this.getAbstractContractFieldMapper().getBillingPartner(origin));
        this.getAbstractContractFieldMapper().setBillingPartnerRegistrationId(invoice, this.getAbstractContractFieldMapper().getBillingPartnerRegistrationId(origin));
        invoice.setPricingRule(
            origin.getPricingRule() == null ? 
            	defaultPricingRule : 
            	origin.getPricingRule()
        );
        invoice.setContractNumber(invoiceNumber);
        invoice.setCalcRule(origin.getCalcRule());
        invoice.setContractLanguage(origin.getContractLanguage());
        this.getInvoiceFieldMapper().setIsVoucher(invoice, isVoucher);        
    }
    
    //-----------------------------------------------------------------------
    public SalesOrderT mapSalesOrder(
        SalesOrder salesOrder
    ) {
        Collection<AbstractSalesOrderPosition> positions = salesOrder.getPosition();
        return Datatypes.create(
            SalesOrderT.class,
            Datatypes.member(
                SalesOrderT.Member.contract,
                mapContract(
                    salesOrder,
                    salesOrder.getExternalLink(),
                    mapSalesOrderPositions(
                        salesOrder.getContractNumber(),
                        positions
                    )
                )
            )            
        );
    }
    
    //-----------------------------------------------------------------------
    public List<ContractPositionT> mapInvoicePositions(
        String contractNumber,
        Date paymentDate,
        Collection<AbstractInvoicePosition> positions
    ) {
        List<ContractPositionT> positionsT = new ArrayList<ContractPositionT>();
        for(AbstractInvoicePosition position: positions) {      
            if(position instanceof InvoicePosition) {
                InvoicePosition invoicePosition = (InvoicePosition)position;
                positionsT.add(
                    mapContractPosition(
                        invoicePosition,
                        contractNumber,
                        paymentDate,
                        invoicePosition.getProduct()
                    )
                );
            }
        }
        return positionsT;
    }
    
    //-----------------------------------------------------------------------
    public List<ContractPositionT> mapSalesOrderPositions(
        String contractNumber,
        Collection<AbstractSalesOrderPosition> positions
    ) {
        List<ContractPositionT> positionsT = new ArrayList<ContractPositionT>();
        for(AbstractSalesOrderPosition position: positions) {            
            if(position instanceof SalesOrderPosition) {
                SalesOrderPosition salesOrderPosition = (SalesOrderPosition)position;
                positionsT.add(
                    mapContractPosition(
                        salesOrderPosition,
                        contractNumber,
                        null,
                        salesOrderPosition.getProduct()
                    )
                );
            }
        }
        return positionsT;
    }
    
    //-----------------------------------------------------------------------
    public ContractStatusT mapContractStatus(
        AbstractContract contract
    ) {
        Collection<org.opencrx.kernel.generic.jmi1.PropertySet> propertySets = contract.getPropertySet();        
        List<StringPropertyT> tags = new ArrayList<StringPropertyT>();
        for(org.opencrx.kernel.generic.jmi1.PropertySet propertySet: propertySets) {
            if(propertySet.getName().equals(PropertySetName.ContractStatus + "." + contract.getContractState())) {
                tags = mapPropertySet(propertySet);
                break;
            }
        }
        return Datatypes.create(
            ContractStatusT.class,
            Datatypes.member(
                ContractStatusT.Member.status,
                toInteger(contract.getContractState())
            ),
            Datatypes.member(
                ContractStatusT.Member.description,
                contract.getDescription()
            ),
            Datatypes.member(
                ContractStatusT.Member.tag,
                tags
            )            
        );        
    }
        
    //-----------------------------------------------------------------------
    public MessengerAddressT mapMessengerAddress(
        PhoneNumber messengerAddress
    ) {
        if(messengerAddress == null) return null;
        return Datatypes.create(
            MessengerAddressT.class, 
            Datatypes.member(
                MessengerAddressT.Member.messengerId,
                messengerAddress.getPhoneNumberFull()
            ),
            Datatypes.member(
            	MessengerAddressT.Member.providerName,
                this.getPhoneNumberFieldMapper().getProviderName(messengerAddress)
            ),
            Datatypes.member(
            	MessengerAddressT.Member.providerVerified,
                this.getPhoneNumberFieldMapper().isProviderVerified(messengerAddress)
            )
        );        
    }
    
    //-----------------------------------------------------------------------
    public CustomerContractT mapCustomerContract(
        Lead customerContract
    ) {
        if(customerContract == null) return null;
        List<String> assignedCustomers = new ArrayList<String>();
        Collection<AccountAssignmentContract> accountAssignments = customerContract.getAssignedAccount();
        for(AccountAssignmentContract accountAssignment: accountAssignments) {
        	try {
	        	assignedCustomers.add(
	        		this.getAccountFieldMapper().getAccountNumber(accountAssignment.getAccount())
	        	);
        	} catch(Exception e) {
        		ServiceException e0 = new ServiceException(
        			e,
        			BasicException.Code.DEFAULT_DOMAIN,
        			BasicException.Code.GENERIC,
        			"Unable to map assigned account"
        		);
        		e0.log();
        	}
        }
        return Datatypes.create(
            CustomerContractT.class, 
            Datatypes.member(
                CustomerContractT.Member.contractNumber,
                this.getLeadFieldMapper().getContractNumber(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.customerNumber,
                this.getAccountFieldMapper().getAccountNumber(customerContract.getCustomer())
            ),
            Datatypes.member(
                CustomerContractT.Member.contractStatus,
                this.mapContractStatus(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.acceptedLegal,
                this.getLeadFieldMapper().acceptedLegal(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.acceptedMarketing,
                this.getLeadFieldMapper().acceptedMarketing(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.acceptedPrivateDataForwarding,
                this.getLeadFieldMapper().acceptedPrivateDataForwarding(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.referrer,
                this.getLeadFieldMapper().getReferrer(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.contactSource,
                this.getLeadFieldMapper().getContactSource(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.salesTaxType,
                this.getLeadFieldMapper().getSalesTaxType(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.contractCurrency,
                this.getLeadFieldMapper().getContractCurrency(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.noBilling,
                this.getLeadFieldMapper().noBilling(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.billingPartner,
                this.getAbstractContractFieldMapper().getBillingPartner(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.billingPartnerRegistrationId,
                this.getAbstractContractFieldMapper().getBillingPartnerRegistrationId(customerContract)
            ),
            Datatypes.member(
                CustomerContractT.Member.assignedCustomer,
                assignedCustomers
            )
        );                    
    }
   
    //-----------------------------------------------------------------------
    public void mapCustomerContract(
        CustomerContractT customerContractT,
        Lead customerContract
    ) {
    	LeadFieldMapper leadFieldMapper = this.getLeadFieldMapper();
    	leadFieldMapper.setAcceptedLegal(customerContract, customerContractT.isAcceptedLegal());
    	leadFieldMapper.setAcceptedMarketing(customerContract, customerContractT.isAcceptedMarketing());
    	leadFieldMapper.setAcceptedPrivateDataForwarding(customerContract, customerContractT.isAcceptedPrivateDataForwarding());
    	leadFieldMapper.setReferrer(customerContract, customerContractT.getReferrer());
    	leadFieldMapper.setContactSource(customerContract, customerContractT.getContactSource());
    	leadFieldMapper.setSalesTaxType(customerContract, customerContractT.getSalesTaxType());
    	leadFieldMapper.setContractCurrency(customerContract, customerContractT.getContractCurrency());   
    	leadFieldMapper.setNoBilling(customerContract, customerContractT.isNoBilling());
        this.getAbstractContractFieldMapper().setBillingPartner(customerContract, customerContractT.getBillingPartner());
        this.getAbstractContractFieldMapper().setBillingPartnerRegistrationId(customerContract, customerContractT.getBillingPartnerRegistrationId());        
    }
    
    //-----------------------------------------------------------------------
    public CustomerStatusT mapCustomerStatus(
        Account account
    ) {
        return Datatypes.create(
            CustomerStatusT.class, 
            Datatypes.member(
                CustomerStatusT.Member.status,
                toInteger(account.getAccountState())
            ),
            Datatypes.member(
                CustomerStatusT.Member.description,
                account.getDescription()
            )  
        );        
    }
    
    //-----------------------------------------------------------------------
    public ProductPriceT mapProductPrice(
       ProductBasePrice price,
       Date pricingDate,
       BigDecimal salesTaxRate
    ) {
        return Datatypes.create(
            ProductPriceT.class, 
            Datatypes.member(
                ProductPriceT.Member.price, 
                bigDecimalToString(price.getPrice())
            ),
            Datatypes.member(
                ProductPriceT.Member.priceIncludingTax, 
                bigDecimalToString(
                    price.getPrice().multiply(
                        salesTaxRate.divide(new BigDecimal(100)).add(BigDecimal.ONE)
                    )                        
                )
            ),
            Datatypes.member(
                ProductPriceT.Member.priceCurrency, 
                toInteger(price.getPriceCurrency())
            ),
            Datatypes.member(
                ProductPriceT.Member.priceUom, 
                price.getUom().getName()
            ),
            Datatypes.member(
                ProductPriceT.Member.pricingDate, 
                pricingDate
            ),
            Datatypes.member(
                ProductPriceT.Member.priceLevel, 
                price.getPriceLevel().isEmpty() ? null : price.getPriceLevel().iterator().next().getName() 
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public ProductStatusT mapProductStatus(
        Product product
    ) {
        return Datatypes.create(
            ProductStatusT.class, 
            Datatypes.member(
                ProductStatusT.Member.status,
                toInteger(product.getProductState())
            ),
            Datatypes.member(
                ProductStatusT.Member.description,
                product.getDescription()
            )  
        );        
    }
    
    //-----------------------------------------------------------------------
    public EmailAddressT mapEmailAddress(
        EMailAddress emailAddress
    ) {
        if(emailAddress == null) return null;
        return Datatypes.create(
            EmailAddressT.class, 
            Datatypes.member(
                EmailAddressT.Member.emailAddress,
                emailAddress.getEmailAddress()
            ),
            Datatypes.member(
                EmailAddressT.Member.emailValid,
                this.getEmailAddressFieldMapper().emailValid(emailAddress)
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public PhoneNumberT mapPhoneNumber(
        PhoneNumber phoneNumber
    ) {
        if(phoneNumber == null) return null;
        return Datatypes.create(
            PhoneNumberT.class, 
            Datatypes.member(
                PhoneNumberT.Member.phoneNumber,
                phoneNumber.getPhoneNumberFull()
            ),
            Datatypes.member(
                PhoneNumberT.Member.phoneNumberVerified,
                this.getPhoneNumberFieldMapper().isNumberVerified(phoneNumber)
            ),
            Datatypes.member(
                PhoneNumberT.Member.providerName,
                this.getPhoneNumberFieldMapper().getProviderName(phoneNumber)
            ),
            Datatypes.member(
                PhoneNumberT.Member.providerVerified,
                this.getPhoneNumberFieldMapper().isProviderVerified(phoneNumber)
            )
        );        
    }
        
    //-----------------------------------------------------------------------
    public ProductT mapProduct(
        Product product,
        Boolean mapPictureContent,
        org.opencrx.kernel.product1.jmi1.Segment productSegment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(product);
        List<ProductClassification> classifications = product.getClassification();
        Collection<Description> descriptions = product.getAdditionalDescription();
        List<Uom> priceUoms = product.getPriceUom();
        // Get product filters of type PRODUCT_BUNDLE and test whether product is
        // a filtered product of these filters. If yes, product is part of the
        // product bundle defined by the filter
        List<String> productBundles = new ArrayList<String>();
        ProductFilterGlobalQuery productFilterQuery = (ProductFilterGlobalQuery)pm.newQuery(ProductFilterGlobal.class);
        productFilterQuery.thereExistsUserCode0().equalTo(new Short((short)ProductFilterType.PRODUCT_BUNDLE.getValue()));
        Collection<ProductFilterGlobal> bundleFilters = productSegment.getProductFilter(productFilterQuery);
        for(ProductFilterGlobal bundleFilter: bundleFilters) {
            org.opencrx.kernel.product1.cci2.ProductQuery productQuery = (ProductQuery)pm.newQuery(Product.class);
            productQuery.identity().equalTo(
                product.refMofId()
            );
            if(!bundleFilter.getFilteredProduct(productQuery).isEmpty()) {
                productBundles.add(
                    bundleFilter.getName()
                );
            }
        }
        // Get product filters of type PRODUCT_BUNDLE and test whether product is
        // a filtered product of these filters. If yes, product is part of the
        // product bundle defined by the filter
        List<String> contentPartners = new ArrayList<String>();
        productFilterQuery = (ProductFilterGlobalQuery)pm.newQuery(ProductFilterGlobal.class);
        productFilterQuery.thereExistsUserCode0().equalTo(new Short((short)ProductFilterType.CONTENT_PARTNER.getValue()));
        Collection<ProductFilterGlobal> contentPartnerFilters = productSegment.getProductFilter(productFilterQuery);
        for(ProductFilterGlobal contentPartnerFilter: contentPartnerFilters) {
            org.opencrx.kernel.product1.cci2.ProductQuery productQuery = (ProductQuery)pm.newQuery(Product.class);
            productQuery.identity().equalTo(
                product.refMofId()
            );
            if(!contentPartnerFilter.getFilteredProduct(productQuery).isEmpty()) {
                contentPartners.add(
                    contentPartnerFilter.getName()
                );
            }
        }
        // Bundle data
        Boolean isBundle = this.getProductFieldMapper().isBundle(product);
        ProductBundleDataT bundleData = null;        
        if((isBundle != null) && isBundle.booleanValue()) {  
            productFilterQuery = (ProductFilterGlobalQuery)pm.newQuery(ProductFilterGlobal.class);
            productFilterQuery.name().equalTo(product.getProductNumber());
            bundleFilters = productSegment.getProductFilter(productFilterQuery);
            List<String> classificationIdFilter = null;
            List<String> members = new ArrayList<String>();
            if(!bundleFilters.isEmpty()) {
                // Members
                ProductFilterGlobal bundleFilter = bundleFilters.iterator().next();
                Collection<Product> filteredProducts = bundleFilter.getFilteredProduct();
                for(Product p: filteredProducts) {
                    if(p.getProductNumber() != null) {
                        members.add(p.getProductNumber());
                    }
                }
                // ClassificationId
                Collection<ProductFilterProperty> filterProperties = bundleFilter.getProductFilterProperty();
                for(ProductFilterProperty p: filterProperties) {
                    if(p instanceof ProductClassificationFilterProperty) {
                        List<ProductClassification> classificationFilter = ((ProductClassificationFilterProperty)p).getClassification();
                        classificationIdFilter = mapProductClassifications(classificationFilter);
                    }
                }
                bundleData = Datatypes.create(
                    ProductBundleDataT.class, 
                    Datatypes.member(
                        ProductBundleDataT.Member.classificationIdFilter,
                        classificationIdFilter
                    ),            
                    Datatypes.member(
                        ProductBundleDataT.Member.member,
                        members
                    )
                );                
            }
        }
        // Relationships
        List<RelatedProductT> relatedProductsT = new ArrayList<RelatedProductT>();
        Collection<RelatedProduct> relatedProducts = product.getRelatedProduct();
	    for(RelatedProduct relatedProduct: relatedProducts) {
	    	relatedProductsT.add(
		        Datatypes.create(
		            RelatedProductT.class, 
		            Datatypes.member(
		            	RelatedProductT.Member.relationshipType,
		                ((Number)relatedProduct.getRelationshipType()).intValue()
		            ),            
		            Datatypes.member(
		            	RelatedProductT.Member.productNumber,
		                relatedProduct.getProduct().getProductNumber()
		            )
		        )
		    );
	    }
        // Configurations
        List<ProductConfigurationT> configurationsT = new ArrayList<ProductConfigurationT>();
        Collection<ProductConfiguration> configurations = product.getConfiguration();
        for(ProductConfiguration configuration: configurations) {
            configurationsT.add(
                mapProductConfiguration(configuration)
            );
        }
        Collection<ProductPhase> productPhases = product.getProductPhase();
        List<ProductPhaseT> productPhasesT = new ArrayList<ProductPhaseT>();
        for(ProductPhase productPhase: productPhases) {
            productPhasesT.add(
                mapProductPhase(productPhase)
            );
        }     
        boolean mapPicture = mapPictureContent != null && mapPictureContent.booleanValue();
        return Datatypes.create(
            ProductT.class, 
            Datatypes.member(
                ProductT.Member.productNumber,
                product.getProductNumber()
            ),            
            Datatypes.member(
                ProductT.Member.productName,
                product.getName()
            ),
            Datatypes.member(
                ProductT.Member.activeOn,
                product.getActiveOn()
            ),
            Datatypes.member(
                ProductT.Member.expiresOn,
                product.getExpiresOn()
            ),
            Datatypes.member(
                ProductT.Member.description,
                mapProductDescriptions(descriptions)
            ),
            Datatypes.member(
                ProductT.Member.classificationId,
                mapProductClassifications(classifications)
            ),
            Datatypes.member(
                ProductT.Member.isMemberOfBundle,
                productBundles
            ),
            Datatypes.member(
                ProductT.Member.contentPartner,
                contentPartners
            ),
            Datatypes.member(
                ProductT.Member.priceUom,
                mapUoms(priceUoms)
            ),
            Datatypes.member(
                ProductT.Member.configurationType,
                product.getConfigType() == null ? null : product.getConfigType().getName()
            ),
            Datatypes.member(
                ProductT.Member.configuration,
                configurationsT
            ),
            Datatypes.member(
                ProductT.Member.productStatus,
                mapProductStatus(product)
            ),
            Datatypes.member(
                ProductT.Member.pictureContent,
                mapPicture
                    ? product.getPictureContent()
                    : null
            ),
            Datatypes.member(
                ProductT.Member.pictureMimeType,
                mapPicture
                    ? product.getPictureContentMimeType()
                    : null
            ),
            Datatypes.member(
                ProductT.Member.pictureTitle,
                mapPicture
                    ? product.getPicture() == null ? null : product.getPicture().getDescription()
                    : null
            ),    
            Datatypes.member(
                ProductT.Member.productPhase,
                productPhasesT
            ),
            Datatypes.member(
                ProductT.Member.isBundle,
                isBundle
            ),
            Datatypes.member(
                ProductT.Member.bundleData,
                bundleData
            ),
            Datatypes.member(
            	ProductT.Member.relatedProduct,
            	relatedProductsT
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public ActivityFollowUpT mapActivityFollowUp(
    	ActivityFollowUp followUp
    ) {
        return Datatypes.create(
            ActivityFollowUpT.class, 
            Datatypes.member(
                ActivityFollowUpT.Member.name,
                followUp.getTitle()
            ),            
            Datatypes.member(
                ActivityFollowUpT.Member.description,
                followUp.getText()
            ),            
            Datatypes.member(
                ActivityFollowUpT.Member.createdAt,
                followUp.getCreatedAt()
            ),            
            Datatypes.member(
                ActivityFollowUpT.Member.modifiedAt,
                followUp.getModifiedAt()
            ),            
            Datatypes.member(
                ActivityFollowUpT.Member.transitionName,
                followUp.getTransition().getName()
            ),
            Datatypes.member(
                ActivityFollowUpT.Member.reportingCustomerNumber,
                followUp.getAssignedTo() == null 
                	? null :
                		this.getAccountFieldMapper().getAccountNumber(followUp.getAssignedTo())
            ),
            Datatypes.member(
                ActivityFollowUpT.Member.category,
                followUp.getCategory()
            )
       );
    }
    
    //-----------------------------------------------------------------------
    public ActivityT mapActivity(
    	Activity activity
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
    	ActivityFollowUpQuery followUpQuery = (ActivityFollowUpQuery)pm.newQuery(ActivityFollowUp.class);
    	followUpQuery.orderByCreatedAt().ascending();
    	List<ActivityFollowUp> followUps = activity.getFollowUp(followUpQuery);
    	List<ActivityFollowUpT> followUpsT = new ArrayList<ActivityFollowUpT>();
    	for(ActivityFollowUp followUp: followUps) {
    		followUpsT.add(
    			this.mapActivityFollowUp(followUp)
    		);
    	}
    	List<String> activityGroups = new ArrayList<String>();
    	Collection<ActivityGroupAssignment> groupAssignments = activity.getAssignedGroup();
    	for(ActivityGroupAssignment assignment: groupAssignments) {
    		activityGroups.add(
    			assignment.getActivityGroup().getName()
    		);
    	}
        return Datatypes.create(
            ActivityT.class, 
            Datatypes.member(
                ActivityT.Member.activityNumber,
                activity.getActivityNumber()
            ),            
            Datatypes.member(
                ActivityT.Member.activityState,
                activity.getProcessState().getName()
            ),            
            Datatypes.member(
                ActivityT.Member.name,
                activity.getName()
            ),            
            Datatypes.member(
                ActivityT.Member.description,
                activity.getDescription()
            ),            
            Datatypes.member(
                ActivityT.Member.detailedDescription,
                activity.getDetailedDescription()
            ),            
            Datatypes.member(
                ActivityT.Member.reportingCustomerNumber,
                this.getAccountFieldMapper().getAccountNumber(activity.getReportingContact())
            ),
            Datatypes.member(
                ActivityT.Member.scheduledStart,
                activity.getScheduledStart()
            ),            
            Datatypes.member(
                ActivityT.Member.scheduledEnd,
                activity.getScheduledEnd()
            ),            
            Datatypes.member(
                ActivityT.Member.dueBy,
                activity.getDueBy()
            ),            
            Datatypes.member(
                ActivityT.Member.followUp,
                followUpsT
            ),
            Datatypes.member(
                ActivityT.Member.activityGroup,
                activityGroups
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public void mapProduct(
        ProductT productT,
        Product product
    ) {
        product.setProductNumber(productT.getProductNumber());
        product.setName(productT.getProductName());
        product.setActiveOn(productT.getActiveOn());
        product.setExpiresOn(productT.getExpiresOn());
        if(productT.getProductStatus() == null) {
            product.setProductState((short)0);
            product.setDescription("New");
        }
        else {
            product.setProductState((short)productT.getProductStatus().getStatus());
            product.setDescription(productT.getProductStatus().getDescription());                         
        }        
    }
    
    //-----------------------------------------------------------------------
    public AmountT mapAmount(
        BigDecimal quantity,
        Uom uom
    ) {
        return Datatypes.create(
            AmountT.class, 
            Datatypes.member(
                AmountT.Member.count,
                bigDecimalToString(quantity)
            ),
            Datatypes.member(
                AmountT.Member.uom,
                uom.getName()
            )
        );        
    }
    
    //-----------------------------------------------------------------------
    public CredentialsT mapCredentials(
        Account account
    ) {
        return Datatypes.create(
            CredentialsT.class, 
            Datatypes.member(
                CredentialsT.Member.userName, 
                this.getAccountFieldMapper().getUserName(account)
            ),
            Datatypes.member(
                CredentialsT.Member.passwordMd5, 
                this.getAccountFieldMapper().getPasswordMd5(account)
            ),
            Datatypes.member(
                CredentialsT.Member.resetPasswordChallenge, 
                this.getAccountFieldMapper().getResetPasswordChallenge(account)
            ),
            Datatypes.member(
                CredentialsT.Member.resetPasswordResponse, 
                this.getAccountFieldMapper().getResetPasswordResponse(account)
            )
        );        
    }
    
    //-----------------------------------------------------------------------
    public void mapPostalAddress(
        PostalAddressT addressT,
        PostalAddressable address
    ) {
        address.getPostalAddressLine().clear();
        if(addressT.getPostalAddressLine0() != null && addressT.getPostalAddressLine0().length() > 0) {
	        address.getPostalAddressLine().add(
	            addressT.getPostalAddressLine0()
	        );
	        if(addressT.getPostalAddressLine1() != null && addressT.getPostalAddressLine1().length() > 0) {
		        address.getPostalAddressLine().add(
		            addressT.getPostalAddressLine1()
		        );
		        if(addressT.getPostalAddressLine2() != null && addressT.getPostalAddressLine2().length() > 0) {
			        address.getPostalAddressLine().add(
			            addressT.getPostalAddressLine2()
			        );
		        }
	        }
        }
        address.getPostalStreet().clear();
        if(addressT.getPostalStreet0() != null && addressT.getPostalStreet0().length() > 0) {
	        address.getPostalStreet().add(
	            addressT.getPostalStreet0()
	        );
	        if(addressT.getPostalStreet1() != null && addressT.getPostalStreet1().length() > 0) {
		        address.getPostalStreet().add(
		            addressT.getPostalStreet1()
		        );
	        }
        }
        this.getPostalAddressFieldMapper().setPostalStreetNumber(
            address,
            addressT.getPostalStreetNumber()
        );
        address.setPostalCity(addressT.getPostalCity());
        address.setPostalCountry(addressT.getPostalCountry() == null ? null : addressT.getPostalCountry().shortValue());
        address.setPostalCode(addressT.getPostalCode());
    }

    //-----------------------------------------------------------------------
    public void mapEmailAddress(
        EmailAddressT addressT,
        EMailAddress address
    ) {
        address.setEmailAddress(addressT.getEmailAddress());
        this.getEmailAddressFieldMapper().setEmailValid(address, addressT.isEmailValid());
    }
    
    //-----------------------------------------------------------------------
    public void mapPhoneNumber(
        PhoneNumberT addressT,
        PhoneNumber address
    ) {
        address.setPhoneNumberFull(addressT.getPhoneNumber());
        this.getPhoneNumberFieldMapper().setProviderName(address, addressT.getProviderName());
        this.getPhoneNumberFieldMapper().setProviderVerified(address, addressT.isProviderVerified());
        this.getPhoneNumberFieldMapper().setNumberVerified(address, addressT.isPhoneNumberVerified());
    }
    
    //-----------------------------------------------------------------------
    public void mapMessengerAddress(
        MessengerAddressT addressT,
        PhoneNumber address
    ) {
        address.setPhoneNumberFull(addressT.getMessengerId());
        this.getPhoneNumberFieldMapper().setProviderName(address, addressT.getProviderName());
        this.getPhoneNumberFieldMapper().setProviderVerified(address, addressT.isProviderVerified());
    }
    
    //-----------------------------------------------------------------------
    public void mapWebAddress(
        String addressT,
        WebAddress address
    ) {
        address.setWebUrl(addressT);
    }
    
    //-----------------------------------------------------------------------
    public PriceLevelT mapPriceLevel(
        AbstractPriceLevel priceLevel
    ) {
        PropertySet genericData = null;
        Collection<org.opencrx.kernel.generic.jmi1.PropertySet> propertySets = priceLevel.getPropertySet();
        for(org.opencrx.kernel.generic.jmi1.PropertySet propertySet: propertySets) {
            if(propertySet.getName().equals(PropertySetName.GenericData.toString())) {
                genericData = propertySet;
                break;
            }
        }
        return Datatypes.create(
            PriceLevelT.class, 
            Datatypes.member(
                PriceLevelT.Member.name, 
                priceLevel.getName()
            ),
            Datatypes.member(
                PriceLevelT.Member.description, 
                priceLevel.getDescription()
            ),
            Datatypes.member(
                PriceLevelT.Member.priceCurrency, 
                toInteger(priceLevel.getPriceCurrency())
            ),
            Datatypes.member(
                PriceLevelT.Member.validFrom, 
                priceLevel.getValidFrom()
            ),
            Datatypes.member(
                PriceLevelT.Member.validTo, 
                priceLevel.getValidTo()
            ),
            Datatypes.member(
                PriceLevelT.Member.genericData, 
                mapPropertySet(genericData)
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public ProductPhaseT mapProductPhase(
        ProductPhase productPhase
    ) {
        return Datatypes.create(
            ProductPhaseT.class, 
            Datatypes.member(
                ProductPhaseT.Member.name, 
                productPhase.getName()
            ),
            Datatypes.member(
                ProductPhaseT.Member.productPhaseKey, 
                productPhase.getProductPhaseKey()
            ),
            Datatypes.member(
                ProductPhaseT.Member.validFrom, 
                productPhase.getValidFrom()
            ),
            Datatypes.member(
                ProductPhaseT.Member.validTo, 
                productPhase.getValidTo()
            )
        );
    }
    
    //-----------------------------------------------------------------------
    public DeliveryInformationT mapDeliveryInformation(
    	DeliveryInformation deliveryInformation
    ) {
        return Datatypes.create(
            DeliveryInformationT.class, 
            Datatypes.member(
            	DeliveryInformationT.Member.deliveryStatus, 
                this.getDeliveryInformationFieldMapper().getDeliveryStatus(deliveryInformation)
            ),
            Datatypes.member(
            	DeliveryInformationT.Member.deliveryStatusDescription, 
                this.getDeliveryInformationFieldMapper().getDeliveryStatusDescription(deliveryInformation)
            ),
            Datatypes.member(
            	DeliveryInformationT.Member.actualDeliveryOn, 
                deliveryInformation.getActualDeliveryOn()
            ),
            Datatypes.member(
            	DeliveryInformationT.Member.quantityShipped, 
                bigDecimalToString(deliveryInformation.getQuantityShipped())
            ),
            Datatypes.member(
            	DeliveryInformationT.Member.productAssembledAt, 
            	this.getDeliveryInformationFieldMapper().getProductAssembledAt(deliveryInformation)
            )            
        );    	
    }

    //-----------------------------------------------------------------------
    public DocumentT mapDocument(
    	Document document
    ) {
    	if(document.getHeadRevision() == null) {
    		return null;
    	}
    	DocumentRevision headRevision = document.getHeadRevision();
    	if(!(headRevision instanceof MediaContent)) {
    		return null;
    	}
    	MediaContent media = (MediaContent)headRevision;
    	QuotaByteArrayOutputStream content = new QuotaByteArrayOutputStream(DatatypeMappers.class.getName());
    	try {
	    	BinaryLargeObjects.streamCopy(
	    		media.getContent().getContent(), 
	    		0L, 
	    		content
	    	);
	    	content.close();
    	} catch(Exception e) {}
        return Datatypes.create(
            DocumentT.class, 
            Datatypes.member(
            	DocumentT.Member.name, 
            	document.getName()
            ),
            Datatypes.member(
	        	DocumentT.Member.title, 
	            document.getTitle()
	        ),
            Datatypes.member(
	        	DocumentT.Member.contentName, 
	            media.getContentName()
	        ),
            Datatypes.member(
	        	DocumentT.Member.contentLanguage,
	        	document.getContentLanguage().isEmpty() 
	        		? 0
	        		: new Integer(document.getContentLanguage().get(0))
	        ),
            Datatypes.member(
	        	DocumentT.Member.contentMimeType, 
	            media.getContentMimeType()
	        ),
            Datatypes.member(
	        	DocumentT.Member.content, 
	            content.toByteArray()
	        )
	    );    	
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private AbstractContractFieldMapper abstractContractFieldMapper = null;
    private AccountFieldMapper accountFieldMapper = null;
    private EmailAddressFieldMapper emailAddressFieldMapper = null;
    private InvoiceFieldMapper invoiceFieldMapper = null;
    private LeadFieldMapper leadFieldMapper = null;
    private PhoneNumberFieldMapper phoneNumberFieldMapper = null;
    private PostalAddressFieldMapper postalAddressFieldMapper = null;
    private ProductFieldMapper productFieldMapper = null;
    private ProductFilterFieldMapper productFilterFieldMapper = null;
    private SalesOrderFieldMapper salesOrderFieldMapper = null;
    private DeliveryInformationFieldMapper deliveryInformationFieldMapper = null;
    
}
