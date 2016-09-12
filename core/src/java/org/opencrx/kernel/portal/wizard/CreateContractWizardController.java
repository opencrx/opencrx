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

import java.io.Writer;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.contract1.jmi1.CalculationRule;
import org.opencrx.kernel.contract1.jmi1.CreatePositionParams;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.Opportunity;
import org.opencrx.kernel.contract1.jmi1.Quote;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.product1.jmi1.PriceLevel;
import org.opencrx.kernel.product1.jmi1.PricingRule;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * CreateContractWizardController
 *
 */
public class CreateContractWizardController extends org.openmdx.portal.servlet.AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateContractWizardController(
   	) {
   		super();
   	}

	/**
	 * OK action.
	 * 
	 * @param isAddMembershipMode
	 * @param accountMembershipXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields
	) throws ServiceException {
		this.doRefresh(
			contractPositionCount,
			formFields
		);
	}
	
	/**
	 * Create contract and contract positions.
	 * 
	 * @param contract
	 * @throws ServiceException
	 */
	protected void createContract(
		SalesContract contract
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(pm, this.getProviderName(), this.getSegmentName());
	    String name = (String)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:name");
	    String contractNumber = (String)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:contractNumber");
	    Account account = this.formFields.get("org:opencrx:kernel:contract1:SalesContract:customer") != null ?
	        (Account)pm.getObjectById(
	        	this.formFields.get("org:opencrx:kernel:contract1:SalesContract:customer")
	        ) : null;
	    @SuppressWarnings("unchecked")
        List<String> postalAddressLineShipping = (List<String>)this.formFields.get("org:opencrx:kernel:account1:Contact:address!postalAddressLine");
	    @SuppressWarnings("unchecked")
        List<String> postalStreetShipping = (List<String>)this.formFields.get("org:opencrx:kernel:account1:Contact:address!postalStreet");
	    @SuppressWarnings("unchecked")
        List<String> postalAddressLineBilling = (List<String>)this.formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalAddressLine");
	    @SuppressWarnings("unchecked")
        List<String> postalStreetBilling = (List<String>)this.formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet");
	    if(
	        (name != null) && !name.isEmpty() &&
	        (contractNumber != null) && !contractNumber.isEmpty() &&
	        (account != null)
	    ) {
	    	try {
			    PricingRule pricingRule = this.formFields.get("org:opencrx:kernel:contract1:SalesContract:pricingRule") != null 
			    	? (org.opencrx.kernel.product1.jmi1.PricingRule)pm.getObjectById(
			        	this.formFields.get("org:opencrx:kernel:contract1:SalesContract:pricingRule")
			    	  ) 
			        : null;
				contract.setDescription((String)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:description"));
				contract.setCustomer(account);
				contract.setActiveOn((Date)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:activeOn"));
				if(this.formFields.get("org:opencrx:kernel:contract1:SalesContract:contractCurrency") != null) {
					contract.setContractCurrency((Short)this.formFields.get("org:opencrx:kernel:contract1:SalesContract:contractCurrency"));
				}
				if(this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:priority") != null) {
					contract.setPriority((Short)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:priority"));
				}
				contract.setSalesRep(
					this.formFields.get("org:opencrx:kernel:contract1:SalesContract:salesRep") != null 
						? (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(
							this.formFields.get("org:opencrx:kernel:contract1:SalesContract:salesRep")
						  ) 
						: null
				);
				contract.setExpiresOn(
					(Date)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:expiresOn")
				);
				if(this.formFields.get("org:opencrx:kernel:contract1:SalesContract:paymentTerms") != null) {
					contract.setPaymentTerms((Short)this.formFields.get("org:opencrx:kernel:contract1:SalesContract:paymentTerms"));
				}
				contract.setOrigin(
					this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:origin") != null 
						? (org.opencrx.kernel.contract1.jmi1.SalesContract)pm.getObjectById(
							this.formFields.get("org:opencrx:kernel:contract1:SalesContract:origin")
						  ) 
						: null
				);
				contract.setPricingRule(pricingRule);
				contract.setCalcRule(
					this.formFields.get("org:opencrx:kernel:contract1:SalesContract:calcRule") != null 
						? (org.opencrx.kernel.contract1.jmi1.CalculationRule)pm.getObjectById(
							this.formFields.get("org:opencrx:kernel:contract1:SalesContract:calcRule")
						  ) 
						: null
				);
				if(this.formFields.get("org:opencrx:kernel:contract1:ShippingDetail:shippingMethod") != null) {
					contract.setShippingMethod((Short)this.formFields.get("org:opencrx:kernel:contract1:ShippingDetail:shippingMethod"));
				}
				pm.currentTransaction().begin();
				if(contract instanceof Opportunity) {
					contract.setContractNumber(contractNumber.replace("?", "P"));
					contract.setName(name.replace("?", "P"));
					contractSegment.addOpportunity(
					    Base.getInstance().getUidAsString(),
					    (Opportunity)contract
					);
				} else if(contract instanceof Quote) {
					contract.setContractNumber(contractNumber.replace("?", "Q"));
					contract.setName(name.replace("?", "Q"));
					contractSegment.addQuote(
					    Base.getInstance().getUidAsString(),
					    (Quote)contract
					);
				} else if(contract instanceof SalesOrder) {
					contract.setContractNumber(contractNumber.replace("?", "S"));
					contract.setName(name.replace("?", "S"));
					contractSegment.addSalesOrder(
					    Base.getInstance().getUidAsString(),
					    (SalesOrder)contract
					);
				} else if(contract instanceof Invoice) {
					contract.setContractNumber(contractNumber.replace("?", "I"));
					contract.setName(name.replace("?", "I"));
					contractSegment.addInvoice(
					    Base.getInstance().getUidAsString(),
					    (Invoice)contract
					);
				}
				pm.currentTransaction().commit();
				// Shipping address
				org.opencrx.kernel.contract1.jmi1.PostalAddress shippingAddress = pm.newInstance(org.opencrx.kernel.contract1.jmi1.PostalAddress.class);
				shippingAddress.getUsage().add((short)10200); // Delivery
				shippingAddress.setPostalAddressLine(postalAddressLineShipping);
				shippingAddress.setPostalStreet(postalStreetShipping);
				shippingAddress.setPostalCity((String)this.formFields.get("org:opencrx:kernel:account1:Contact:address!postalCity"));
				shippingAddress.setPostalCode((String)this.formFields.get("org:opencrx:kernel:account1:Contact:address!postalCode"));
				shippingAddress.setPostalCountry((Short)this.formFields.get("org:opencrx:kernel:account1:Contact:address!postalCountry"));
				// Billing address
				org.opencrx.kernel.contract1.jmi1.PostalAddress billingAddress = pm.newInstance(org.opencrx.kernel.contract1.jmi1.PostalAddress.class);
				billingAddress.getUsage().add((short)10000); // Invoice
				billingAddress.setPostalAddressLine(postalAddressLineBilling);
				billingAddress.setPostalStreet(postalStreetBilling);
				billingAddress.setPostalCity((String)this.formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCity"));
				billingAddress.setPostalCode((String)this.formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCode"));
				billingAddress.setPostalCountry((Short)this.formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCountry"));
				// Add addresses
				pm.currentTransaction().begin();
				contract.addAddress(
				    Base.getInstance().getUidAsString(),
				    shippingAddress
				);
				contract.addAddress(
				    Base.getInstance().getUidAsString(),
				    billingAddress
				);
				pm.currentTransaction().commit();
				// Create positions
				for(int i = 0; i < 100; i++) {
					String quantity = (String)this.formFields.get("position.quantity." + i);
				    String productXri = (String)this.formFields.get("position.product.xri." + i);
				    String positionName = (String)this.formFields.get("position.name." + i);
				    String pricePerUnit = (String)this.formFields.get("position.pricePerUnit." + i);
				    if(productXri != null) {
					    CreatePositionParams params = Structures.create(
					    	CreatePositionParams.class, 
					    	Datatypes.member(CreatePositionParams.Member.ignoreProductConfiguration, Boolean.TRUE),
					    	Datatypes.member(CreatePositionParams.Member.name, positionName),
					    	Datatypes.member(CreatePositionParams.Member.priceUom, null),
					    	Datatypes.member(CreatePositionParams.Member.pricingDate, null),
					    	Datatypes.member(CreatePositionParams.Member.pricingRule, pricingRule),
					    	Datatypes.member(CreatePositionParams.Member.product, (org.opencrx.kernel.product1.jmi1.Product)pm.getObjectById(new Path(productXri))),
					    	Datatypes.member(CreatePositionParams.Member.quantity, app.parseNumber(quantity)),
					    	Datatypes.member(CreatePositionParams.Member.uom, null)					    	
					    );
					    try {
						    pm.currentTransaction().begin();
						    org.opencrx.kernel.contract1.jmi1.CreatePositionResult result = contract.createPosition(params);
						    pm.currentTransaction().commit();
						    if(pricePerUnit != null) {
								org.opencrx.kernel.contract1.jmi1.SalesContractPosition position = result.getPosition();
								pm.refresh(position);
								pm.currentTransaction().begin();
								position.setPricePerUnit(app.parseNumber(pricePerUnit));
								pm.currentTransaction().commit();
						    }
					    } catch(Exception e) {
					        ServiceException e0 = new ServiceException(e);
					        e0.log();
					        try {
					            pm.currentTransaction().rollback();
					        } catch(Exception e1) {}
					    }
				    }
				}
				pm.currentTransaction().begin();
				contract.reprice();
				pm.currentTransaction().commit();
	    	} catch (Exception e) {
				try {
					pm.currentTransaction().rollback();
				} catch(Exception e1) {}
				new ServiceException(e).log();
	    	}
			// Forward
			if(contract != null) {
				this.setExitAction(
					new ObjectReference(contract, app).getSelectObjectAction()
				);
			}
	    }		
	}

	/**
	 * Refresh action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doRefresh(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields
   	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.formFields = formFields;
		this.contractPositionCount = contractPositionCount == null ? 0 : contractPositionCount;
		RefObject_1_0 obj = this.getObject();
		// Get position values
		for(int i = 0; i < 100; i++) {
		    this.formFields.put(
		        "position.quantity." + i,
		        this.getRequest().getParameter("position.quantity." + i)
		    );
		    this.formFields.put(
		        "position.product." + i,
		        this.getRequest().getParameter("position.product." + i)
		    );
		    this.formFields.put(
		        "position.product.xri." + i,
		        this.getRequest().getParameter("position.product.xri." + i)
		    );
		    this.formFields.put(
		        "position.name." + i,
		        this.getRequest().getParameter("position.name." + i)
		    );
		    this.formFields.put(
		        "position.pricePerUnit." + i,
		        this.getRequest().getParameter("position.pricePerUnit." + i)
		    );
		    this.formFields.put(
		        "position.amount." + i,
		        this.getRequest().getParameter("position.amount." + i)
		    );
		}
		// Set defaults
		if(obj instanceof org.opencrx.kernel.account1.jmi1.Account) {
		    org.opencrx.kernel.account1.jmi1.Account customer = (org.opencrx.kernel.account1.jmi1.Account)obj;
		    this.formFields.put(
		        "org:opencrx:kernel:contract1:SalesContract:customer",
		        customer.refGetPath()
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:contract1:AbstractContract:activeOn",
		        new Date()
		    );
		}
		this.customer = formFields.get("org:opencrx:kernel:contract1:SalesContract:customer") != null 
			? (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(
				this.formFields.get("org:opencrx:kernel:contract1:SalesContract:customer")
			  ) 
			: null;
	    // Contract number
	    if(this.customer != null) {
	        String contractNumber = (String)formFields.get("org:opencrx:kernel:contract1:AbstractContract:contractNumber");
	        if(contractNumber == null || contractNumber.isEmpty()) {
	            contractNumber = 
	            	(this.customer.getAliasName() != null ? this.customer.getAliasName() : this.customer.getFullName()) + 
	            	"-?" + 
	            	(System.currentTimeMillis() / 1000);
	            this.formFields.put(
	                "org:opencrx:kernel:contract1:AbstractContract:contractNumber",
	                contractNumber
	            );
	        }
	        if(this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:name") == null) {
	        	this.formFields.put(
	                "org:opencrx:kernel:contract1:AbstractContract:name",
	                contractNumber
	            );
	        }
	    }
	    // Pricing rule
	    PricingRule pricingRule = this.formFields.get("org:opencrx:kernel:contract1:SalesContract:pricingRule") != null 
	    	? (PricingRule)pm.getObjectById(
	    		this.formFields.get("org:opencrx:kernel:contract1:SalesContract:pricingRule")
	    	  ) 
	        : null;
	    org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(pm, this.getProviderName(), this.getSegmentName());
	    org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(pm, this.getProviderName(), this.getSegmentName());
	    if(pricingRule == null) {
	        pricingRule = Products.getInstance().findPricingRule(Products.PRICING_RULE_NAME_LOWEST_PRICE, productSegment);
	        if(pricingRule != null) {
	        	this.formFields.put(
	                "org:opencrx:kernel:contract1:SalesContract:pricingRule",
	                pricingRule.refGetPath()
	            );
	        }
	    }
	    // Calculation Rule
	    CalculationRule calcRule = this.formFields.get("org:opencrx:kernel:contract1:SalesContract:calcRule") != null 
	    	? (CalculationRule)pm.getObjectById(
	        	this.formFields.get("org:opencrx:kernel:contract1:SalesContract:calcRule")
	    	  ) 
	        : null;
	    if(calcRule == null) {
	        calcRule = Contracts.getInstance().findCalculationRule(Contracts.CALCULATION_RULE_NAME_DEFAULT, contractSegment);
	        if(calcRule != null) {
	        	this.formFields.put(
	                "org:opencrx:kernel:contract1:SalesContract:calcRule",
	                calcRule.refGetPath()
	            );
	        }
	    }
	}

	/**
	 * Cancel action.
	 * 
	 * @throws ServiceException
	 */
	public void doCancel(
	) throws ServiceException {
		this.setExitAction(
			new ObjectReference(getObject(), getApp()).getSelectObjectAction()
		);
	}

	/**
	 * Create opportunity action.
	 * 
	 * @param contractPositionCount
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCreateOpportunity(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
		this.createContract(
			pm.newInstance(Opportunity.class)
		);
	}

	/**
	 * Create quote action.
	 * 
	 * @param contractPositionCount
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCreateQuote(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
		this.createContract(
			pm.newInstance(Quote.class)
		);
	}

	/**
	 * Create sales order action.
	 * 
	 * @param contractPositionCount
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCreateSalesOrder(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
		this.createContract(
			pm.newInstance(SalesOrder.class)
		);
	}

	/**
	 * Create invoice action.
	 * 
	 * @param contractPositionCount
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCreateInvoice(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
		this.createContract(
			pm.newInstance(Invoice.class)
		);
	}

	/**
	 * Add position action.
	 * 
	 * @param contractPositionCount
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doAddPosition(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields		
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
	    org.opencrx.kernel.product1.jmi1.Product product = this.formFields.get("org:opencrx:kernel:product1:ProductDescriptor:product") != null ?
	    	(org.opencrx.kernel.product1.jmi1.Product)pm.getObjectById(
	    		formFields.get("org:opencrx:kernel:product1:ProductDescriptor:product")
	    	) : null;
	    java.math.BigDecimal quantity = (java.math.BigDecimal)this.formFields.get("org:opencrx:kernel:contract1:SalesContractPosition:quantity");
	    String positionName = (String)formFields.get("org:opencrx:kernel:contract1:SalesContractPosition:name");
	    if (product != null && quantity == null) {
	    	quantity = product.getDefaultQuantity();
	    }
	    if(
	        (product != null) &&
	        (quantity != null)
	    ) {
	        String productTitle = new ObjectReference(product, app).getTitle();
	        this.formFields.put(
	            "position.quantity." + this.contractPositionCount,
	            quantity.toString()
	        );
	        this.formFields.put(
	            "position.product." + this.contractPositionCount,
	            productTitle
	        );
	        this.formFields.put(
	            "position.product.xri." + this.contractPositionCount,
	            product.refMofId()
	        );
	        this.formFields.put(
	            "position.name." + this.contractPositionCount,
	            (positionName == null) || positionName.isEmpty() 
	            	? productTitle 
	            	: positionName
	        );
	        ProductBasePrice candidate = null;
            Short contractCurrency = (Short)this.formFields.get("org:opencrx:kernel:contract1:SalesContract:contractCurrency");
	        FindPrice: for(ProductBasePrice price:  product.<ProductBasePrice>getBasePrice()) {
	            // Find a price which matches currency, quantity, uom and which
	            // is assigned to a valid price level
	            if(
	                (contractCurrency.compareTo(price.getPriceCurrency()) == 0) &&
	                ((price.getQuantityFrom() == null) || (price.getQuantityFrom().compareTo(quantity) <= 0)) &&
	                ((price.getQuantityTo() == null) || (price.getQuantityTo().compareTo(quantity) >= 0)) &&
	                (product.getDefaultUom() != null) &&
	                product.getDefaultUom().equals(price.getUom())
	            ) {
	                Date now = new Date();
	                if(this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:activeOn") != null) {
	                   try {
	                       now = (Date)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:activeOn");
	                   } catch (Exception ignore) {}
	                }
	                for(PriceLevel priceLevel: price.<PriceLevel>getPriceLevel()) {
						if(
						    ((priceLevel.isDisabled() == null) || !priceLevel.isDisabled()) &&
						    ((priceLevel.getValidFrom() == null) || (priceLevel.getValidFrom().compareTo(now) <= 0)) &&
						    ((priceLevel.getValidTo() == null) || (priceLevel.getValidTo().compareTo(now) >= 0))
						) {
			                candidate = price;
			                break FindPrice;
						}
	                }
	            }
	        }
	        java.math.BigDecimal pricePerUnit = candidate == null 
	        	? java.math.BigDecimal.ZERO 
	        	: candidate.getPrice();
	        this.formFields.put(
	            "position.pricePerUnit." + this.contractPositionCount,
	            pricePerUnit.toString()
	        );
	        this.formFields.put(
	            "position.amount." + this.contractPositionCount,
	            quantity.multiply(pricePerUnit).toString()
	        );
	        this.contractPositionCount++;
	    }
	}

	/**
	 * Set billing address action.
	 * 
	 * @param contractPositionCount
	 * @param addressXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doSetBillingAddress(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,
		@RequestParameter(name = "AddressXri") String addressXri,
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields		
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
		if(addressXri != null && !addressXri.isEmpty()) {
		    org.opencrx.kernel.account1.jmi1.PostalAddress address = (org.opencrx.kernel.account1.jmi1.PostalAddress)pm.getObjectById(new Path(addressXri));
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Account:address*Business!postalAddressLine",
				new ArrayList<String>(address.getPostalAddressLine())
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Account:address*Business!postalStreet",
				new ArrayList<String>(address.getPostalStreet())
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Account:address*Business!postalCity",
				address.getPostalCity()
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Account:address*Business!postalCode",
				address.getPostalCode()
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Account:address*Business!postalCountry",
				address.getPostalCountry()
		    );
		}
	}

	/**
	 * Set shipping address action.
	 * 
	 * @param contractPositionCount
	 * @param addressXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doSetShippingAddress(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
		@RequestParameter(name = "AddressXri") String addressXri,
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields		
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
		if(addressXri != null && !addressXri.isEmpty()) {
		    org.opencrx.kernel.account1.jmi1.PostalAddress address = (org.opencrx.kernel.account1.jmi1.PostalAddress)pm.getObjectById(new Path(addressXri));
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Contact:address!postalAddressLine",
				new ArrayList<String>(address.getPostalAddressLine())
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Contact:address!postalStreet",
				new ArrayList<String>(address.getPostalStreet())
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Contact:address!postalCity",
				address.getPostalCity()
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Contact:address!postalCode",
				address.getPostalCode()
		    );
		    this.formFields.put(
		        "org:opencrx:kernel:account1:Contact:address!postalCountry",
				address.getPostalCountry()
		    );
		}
	}

	/**
	 * Delete position action.
	 * 
	 * @param contractPositionCount
	 * @param deletePositionIndex
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doDeletePosition(
		@RequestParameter(name = "ContractPositionCount") Integer contractPositionCount,		
		@RequestParameter(name = "DeletePositionIndex") Integer deletePositionIndex,
   		@FormParameter(forms = {"CreateContractForm", "CreateContractPositionForm"}) Map<String,Object> formFields	
	) throws ServiceException {
		this.doRefresh(
			contractPositionCount, 
			formFields
		);
		if(deletePositionIndex != null) {
			this.formFields.remove("position.quantity." + deletePositionIndex);
			this.formFields.remove("position.product." + deletePositionIndex);
			this.formFields.remove("position.product.xri." + deletePositionIndex);
			this.formFields.remove("position.name." + deletePositionIndex);
			this.formFields.remove("position.pricePerUnit." + deletePositionIndex);
			this.formFields.remove("position.amount." + deletePositionIndex);
		}
	}

	/**
	 * 
	 * Get form values.
	 * 
	 * @return
	 */
	public Map<String,Object> getFormFields(
	) {
		return this.formFields;
	}

	/**
	 * @return the contractPositionCount
	 */
	public int getContractPositionCount(
	) {
		return this.contractPositionCount;
	}

	/**
	 * @return the customer
	 */
	public Account getCustomer(
	) {
		return this.customer;
	}

	/**
	 * Get view port.
	 * 
	 * @param out
	 * @return
	 */
	public ViewPort getViewPort(
		Writer out
	) {
		if(this.viewPort == null) {
			TransientObjectView view = new TransientObjectView(
				this.getFormFields(),
				this.getApp(),
				this.getObject(),
				this.getPm()
			);
			this.viewPort = ViewPortFactory.openPage(
				view,
				this.getRequest(),
				out
			);			
		}
		return this.viewPort;
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#close()
	 */
    @Override
    public void close(
    ) throws ServiceException {
	    super.close();
	    if(this.viewPort != null) {
	    	this.viewPort.close(false);
	    }
    }

    //-------------------------------------------------------------------
	// Members
	//-------------------------------------------------------------------
    private Map<String,Object> formFields;
	private int contractPositionCount = 0;
	private Account customer;
	private ViewPort viewPort;

}
