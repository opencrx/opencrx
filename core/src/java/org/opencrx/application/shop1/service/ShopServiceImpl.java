/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: ShopServiceImpl
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
package org.opencrx.application.shop1.service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jdo.PersistenceManager;

import org.opencrx.application.shop1.cci2.*;
import org.opencrx.application.shop1.datatypes.DatatypeMappers;
import org.opencrx.application.shop1.datatypes.InvoiceState;
import org.opencrx.application.shop1.datatypes.ProductFilterType;
import org.opencrx.application.shop1.datatypes.PropertySetName;
import org.opencrx.application.shop1.datatypes.SalesOrderState;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.cci2.LegalEntityQuery;
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
import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams;
import org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.NewActivityParams;
import org.opencrx.kernel.activity1.jmi1.NewActivityResult;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Activities.Priority;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.base.jmi1.Property;
import org.opencrx.kernel.base.jmi1.SecureObject;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.code1.cci2.CodeValueContainerQuery;
import org.opencrx.kernel.code1.jmi1.AbstractEntry;
import org.opencrx.kernel.code1.jmi1.CodeValueContainer;
import org.opencrx.kernel.code1.jmi1.CodeValueEntry;
import org.opencrx.kernel.code1.jmi1.SimpleEntry;
import org.opencrx.kernel.contract1.cci2.InvoicePositionQuery;
import org.opencrx.kernel.contract1.cci2.InvoiceQuery;
import org.opencrx.kernel.contract1.cci2.LeadQuery;
import org.opencrx.kernel.contract1.cci2.SalesOrderQuery;
import org.opencrx.kernel.contract1.jmi1.AbstractContract;
import org.opencrx.kernel.contract1.jmi1.AbstractInvoicePosition;
import org.opencrx.kernel.contract1.jmi1.AbstractSalesOrderPosition;
import org.opencrx.kernel.contract1.jmi1.AccountAssignmentContract;
import org.opencrx.kernel.contract1.jmi1.CreatePositionParams;
import org.opencrx.kernel.contract1.jmi1.DeliveryInformation;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.InvoicePosition;
import org.opencrx.kernel.contract1.jmi1.Lead;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.contract1.jmi1.SalesOrderCreateInvoiceResult;
import org.opencrx.kernel.contract1.jmi1.SalesOrderPosition;
import org.opencrx.kernel.document1.cci2.DocumentFolderQuery;
import org.opencrx.kernel.document1.cci2.DocumentQuery;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.generic.cci2.PropertySetQuery;
import org.opencrx.kernel.generic.jmi1.Description;
import org.opencrx.kernel.generic.jmi1.Media;
import org.opencrx.kernel.generic.jmi1.PropertySet;
import org.opencrx.kernel.product1.cci2.AbstractPriceLevelQuery;
import org.opencrx.kernel.product1.cci2.PricingRuleQuery;
import org.opencrx.kernel.product1.cci2.ProductBasePriceQuery;
import org.opencrx.kernel.product1.cci2.ProductClassificationQuery;
import org.opencrx.kernel.product1.cci2.ProductConfigurationTypeSetQuery;
import org.opencrx.kernel.product1.cci2.ProductQuery;
import org.opencrx.kernel.product1.cci2.SalesTaxTypeQuery;
import org.opencrx.kernel.product1.jmi1.AbstractPriceLevel;
import org.opencrx.kernel.product1.jmi1.ConfiguredProduct;
import org.opencrx.kernel.product1.jmi1.CreateInitialPricesParams;
import org.opencrx.kernel.product1.jmi1.PriceUomFilterProperty;
import org.opencrx.kernel.product1.jmi1.PricingRule;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.opencrx.kernel.product1.jmi1.ProductClassification;
import org.opencrx.kernel.product1.jmi1.ProductClassificationFilterProperty;
import org.opencrx.kernel.product1.jmi1.ProductConfiguration;
import org.opencrx.kernel.product1.jmi1.ProductConfigurationTypeSet;
import org.opencrx.kernel.product1.jmi1.ProductFilterGlobal;
import org.opencrx.kernel.product1.jmi1.ProductFilterProperty;
import org.opencrx.kernel.product1.jmi1.ProductPhase;
import org.opencrx.kernel.product1.jmi1.SalesTaxType;
import org.opencrx.kernel.product1.jmi1.SetConfigurationTypeParams;
import org.opencrx.kernel.uom1.cci2.UomQuery;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class ShopServiceImpl
	implements org.opencrx.application.shop1.cci2.ShopService {

    //-----------------------------------------------------------------------
    public ShopServiceImpl(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        String shopName,
        boolean emailAddressMustBeUnique,
        boolean noCopyOfProductConfiguration,
        DatatypeMappers datatypeMappers
    ) {
        this.pm = pm;
        this.providerName = providerName;
        this.segmentName = segmentName;
        this.shopName = shopName;
        this.emailAddressMustBeUnique = emailAddressMustBeUnique;
        this.noCopyOfProductConfiguration = noCopyOfProductConfiguration;
        this.datatypeMappers = datatypeMappers;
    }

    //-----------------------------------------------------------------------
    /**
     * Currently the following objects are shop-local:
     * <ul>
     *   <li>Contracts
     *   <li>Account membership
     * </ul>
     * Objects which are shop-local are marked by setting their category field.  
     * Functions which create and query shop-local objects apply a category filter. 
     * Shop-local objects allow multiple shop instances to share the same data 
     * segment. By default the shop category is equal to the shop name.
     */
    protected String getShopCategory(
    ) {
    	return this.shopName;
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.account1.jmi1.Segment getAccountSegment(
    ) {
        return 
            (org.opencrx.kernel.account1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/" + this.providerName + "/segment/" + this.segmentName)
            );
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.code1.jmi1.Segment getCodeSegment(
        String segmentName
    ) {
        return 
            (org.opencrx.kernel.code1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.code1/provider/" + this.providerName + "/segment/" + segmentName)
            );
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.contract1.jmi1.Segment getContractSegment(
    ) {
        return 
            (org.opencrx.kernel.contract1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.contract1/provider/" + this.providerName + "/segment/" + this.segmentName)
            );
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.depot1.jmi1.Segment getDepotSegment(
    ) {
        return 
            (org.opencrx.kernel.depot1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.depot1/provider/" + this.providerName + "/segment/" + this.segmentName)
            );
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.product1.jmi1.Segment getProductSegment(
    ) {
        return 
            (org.opencrx.kernel.product1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.product1/provider/" + this.providerName + "/segment/" + this.segmentName)
            );
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.Segment getActivitySegment(
    ) {
        return 
            (org.opencrx.kernel.activity1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.activity1/provider/" + this.providerName + "/segment/" + this.segmentName)
            );
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.document1.jmi1.Segment getDocumentSegment(
    ) {
        return 
            (org.opencrx.kernel.document1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.document1/provider/" + this.providerName + "/segment/" + this.segmentName)
            );
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.uom1.jmi1.Segment getUomSegment(
    ) {
        return 
            (org.opencrx.kernel.uom1.jmi1.Segment)this.pm.getObjectById(
                new Path("xri://@openmdx*org.opencrx.kernel.uom1/provider/" + this.providerName + "/segment/" + this.segmentName)
            );
    }

    //-----------------------------------------------------------------------
    public String uuidAsString(
    ) {
        return UUIDConversion.toUID(UUIDs.newUUID());
    }
    
    //-----------------------------------------------------------------------
    protected Account findAccount(
        String customerNumber
    ) {
    	if(customerNumber == null) return null;
        AccountQuery accountQuery = (AccountQuery)this.pm.newQuery(Account.class);
        accountQuery.thereExistsAliasName().equalTo(customerNumber);
        List<Account> accounts = this.getAccountSegment().getAccount(accountQuery);
        if(!accounts.isEmpty()) {
            return accounts.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    protected Activity findActivity(
    	String activityNumber,
    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment
    ) {
    	ActivityQuery activityQuery = (ActivityQuery)this.pm.newQuery(Activity.class);
    	activityQuery.thereExistsActivityNumber().equalTo(activityNumber);
    	List<Activity> activities = activitySegment.getActivity(activityQuery);
    	if(!activities.isEmpty()) {
    		return activities.iterator().next();
    	}
    	return null;
    }

    //-----------------------------------------------------------------------
    protected PricingRule findPricingRule(
    ) {
        PricingRuleQuery pricingRuleQuery = (PricingRuleQuery)this.pm.newQuery(PricingRule.class);
        pricingRuleQuery.name().equalTo(this.shopName);
        List<PricingRule> pricingRules = this.getProductSegment().getPricingRule(pricingRuleQuery);
        if(!pricingRules.isEmpty()) {
            return pricingRules.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    protected Collection<ContractPositionT> getInvoicePositions(
        Account customer,
        Integer invoiceStatusThreshold,
        boolean includeVouchers,
        Product restrictToProduct
    ) {
        String contractCategory = this.getShopCategory();
        List<ContractPositionT> positionsT = new ArrayList<ContractPositionT>();
    	if(restrictToProduct == null) {    		
	        InvoiceQuery invoiceQuery = (InvoiceQuery)this.pm.newQuery(Invoice.class);
	        invoiceQuery.thereExistsCustomer().equalTo(customer);
	        if(invoiceStatusThreshold != null) {
	            invoiceQuery.contractState().greaterThanOrEqualTo(invoiceStatusThreshold.shortValue());
	        }	        
	        if(contractCategory != null) {
	        	invoiceQuery.thereExistsCategory().equalTo(contractCategory);
	        }
	        for(Invoice invoice: this.getContractSegment().getInvoice(invoiceQuery)) {
	            if(!this.datatypeMappers.getInvoiceFieldMapper().isVoucher(invoice) || (this.datatypeMappers.getInvoiceFieldMapper().isVoucher(invoice) && includeVouchers)) {
	                Collection<AbstractInvoicePosition> positions = invoice.getPosition();
	                for(AbstractInvoicePosition position: positions) {
	                    if(position instanceof InvoicePosition) {
	                        positionsT.add(
	                            this.datatypeMappers.mapContractPosition(
	                                position, 
	                                invoice.getContractNumber(),    
	                                this.datatypeMappers.getInvoiceFieldMapper().getPaymentDate(invoice),
	                                ((InvoicePosition)position).getProduct()
	                            )
	                        );                        
	                    }
	                }
	            }
	        }
    	}
    	// CR10008427: Optimize in case restrictToProduct != null. In this case do
    	// not iterate over invoices. Instead get all invoice positions matching
    	// restrictToProduct.
    	else {
    		org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment();
            InvoicePositionQuery positionQuery = (InvoicePositionQuery)this.pm.newQuery(InvoicePosition.class);
            // Invoice positions
            positionQuery.identity().like(
          		contractSegment.refGetPath().getDescendant(new String[]{"invoice", ":*", "position", ":*"}).toString()
            );
            // Match restrictToProduct
            positionQuery.thereExistsProduct().equalTo(restrictToProduct);
            // Must be part of invoice with matching customer 
            QueryExtensionRecord queryFilter = PersistenceHelper.newQueryExtension(positionQuery);
            String clause = "EXISTS (SELECT 0 FROM OOCKE1_CONTRACT c INNER JOIN OOCKE1_ACCOUNT a ON c.customer = a.object_id WHERE v.p$$parent = c.object_id AND a.alias_name = ?s0";
            queryFilter.setStringParam(this.datatypeMappers.getAccountFieldMapper().getAccountNumber(customer));
            clause += " AND c.is_gift = ?b0";
            queryFilter.setBooleanParam(new Boolean[]{includeVouchers});
            if(invoiceStatusThreshold != null) {
            	clause += " AND c.contract_state >= ?i0";
            }
            queryFilter.setIntegerParam(new Integer[]{invoiceStatusThreshold});
            clause += ")";
            queryFilter.setClause(clause);
            Collection<AbstractInvoicePosition> positions = contractSegment.getExtent(positionQuery);
            for(AbstractInvoicePosition position: positions) {
                if(position instanceof InvoicePosition) {
                	Invoice invoice = (Invoice)this.pm.getObjectById(position.refGetPath().getParent().getParent());
                	if(contractCategory == null || invoice.getCategory().contains(contractCategory)) {
	                    positionsT.add(
	                    	 this.datatypeMappers.mapContractPosition(
	                            position, 
	                            invoice.getContractNumber(),    
	                            this.datatypeMappers.getInvoiceFieldMapper().getPaymentDate(invoice),
	                            ((InvoicePosition)position).getProduct()
	                        )
	                    );
                	}
                }
            }    		
    	}
        return positionsT;
    }
    
    //-----------------------------------------------------------------------
    public Invoice findInvoice(
        String invoiceNumber
    ) {
        InvoiceQuery query = (InvoiceQuery)this.pm.newQuery(Invoice.class);
        query.thereExistsContractNumber().equalTo(invoiceNumber);
        Collection<Invoice> invoices = this.getContractSegment().getInvoice(query);
        if(!invoices.isEmpty()) {
            return invoices.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    public SalesOrder findSalesOrder(
        String salesOrderNumber
    ) {
        SalesOrderQuery query = (SalesOrderQuery)this.pm.newQuery(SalesOrder.class);
        query.thereExistsContractNumber().equalTo(salesOrderNumber);
        Collection<SalesOrder> salesOrder = this.getContractSegment().getSalesOrder(query);
        if(!salesOrder.isEmpty()) {
            return salesOrder.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    public List<Lead> findCustomerContracts(
        Account account
    ) {    	
        LeadQuery leadQuery = (LeadQuery)this.pm.newQuery(Lead.class);
        String contractCategory = this.getShopCategory();
        if(contractCategory != null) {
        	leadQuery.thereExistsCategory().equalTo(contractCategory);
        }
        List<Lead> leads = account.getAssignedContract(leadQuery);
        if(!leads.isEmpty()) {
            return leads;
        }
        // If not customer contract is found for this shop try to find a customer 
        // contract which's contract number is equal to the customer number
        else {
        	Lead customerContract = this.findCustomerContractByContractNumber(
    			this.datatypeMappers.getAccountFieldMapper().getAccountNumber(account)
    		);
        	return customerContract == null ? null : Collections.singletonList(customerContract);
        }
    }

    //-----------------------------------------------------------------------
    public Lead findMainCustomerContract(
        Account account
    ) {    	
        LeadQuery leadQuery = (LeadQuery)this.pm.newQuery(Lead.class);
        String contractCategory = this.getShopCategory();
        if(contractCategory != null) {
        	leadQuery.thereExistsCategory().equalTo(contractCategory);
        }
        leadQuery.thereExistsCustomer().equalTo(account);
        List<Lead> leads = account.getAssignedContract(leadQuery);
        if(!leads.isEmpty()) {
            return leads.iterator().next();
        }
        // If not customer contract is found for this shop try to find a customer 
        // contract which's contract number is equal to the customer number
        else {
        	return this.findCustomerContractByContractNumber(
    			this.datatypeMappers.getAccountFieldMapper().getAccountNumber(account)
    		);
        }
    }
    
    //-----------------------------------------------------------------------
    public Lead findCustomerContract(
    	AbstractContract contract
    ) {
    	if(contract instanceof Lead) {
    		return (Lead)contract;
    	}
    	else if(contract.getOrigin() != null) {
    		return this.findCustomerContract(contract.getOrigin());
    	}
    	else {
    		return null;
    	}
    }
    
    //-----------------------------------------------------------------------
    public Lead findCustomerContractByContractNumber(
        String customerContractNumber
    ) {
        LeadQuery query = (LeadQuery)this.pm.newQuery(Lead.class);
        query.thereExistsContractNumber().equalTo(customerContractNumber);
        List<Lead> leads = this.getContractSegment().getLead(query);
        if(!leads.isEmpty()) {
            return leads.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    public ProductConfigurationTypeSet findProductConfigurationType(
        String name
    ) {
        ProductConfigurationTypeSetQuery query = (ProductConfigurationTypeSetQuery)this.pm.newQuery(ProductConfigurationTypeSet.class);
        query.name().equalTo(name);
        List<ProductConfigurationTypeSet> configurationTypes = this.getProductSegment().getConfigurationTypeSet(query);
        if(!configurationTypes.isEmpty()) {
            return configurationTypes.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    public Product findProduct(
        String productNumber
    ) {
        ProductQuery query = (ProductQuery)this.pm.newQuery(Product.class);
        query.productNumber().equalTo(productNumber);
        Collection<Product> products = this.getProductSegment().getProduct(query);
        if(!products.isEmpty()) {
            return products.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    public ProductClassification findProductClassification(
        String classificationName
    ) {
        ProductClassificationQuery query = (ProductClassificationQuery)this.pm.newQuery(ProductClassification.class);
        query.name().equalTo(classificationName);
        Collection<ProductClassification> productClassifications = this.getProductSegment().getProductClassification(query);
        if(!productClassifications.isEmpty()) {
            return productClassifications.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    public Uom findUom(
        String uomName
    ) {
        UomQuery query = (UomQuery)this.pm.newQuery(Uom.class);
        query.name().equalTo(uomName);
        Collection<Uom> uoms = this.getUomSegment().getUom(query);
        if(!uoms.isEmpty()) {
            return uoms.iterator().next();
        }
        else {
            return null;
        }
    }
    
    //-----------------------------------------------------------------------
    public SalesTaxType findSalesTaxType(
        String salesTaxTypeName
    ) {
        SalesTaxTypeQuery query = (SalesTaxTypeQuery)this.pm.newQuery(SalesTaxType.class);
        query.name().equalTo(salesTaxTypeName);
        Collection<SalesTaxType> salesTaxTypes = this.getProductSegment().getSalesTaxType(query);
        if(!salesTaxTypes.isEmpty()) {
            return salesTaxTypes.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    public CodeValueContainer findCodeValueContainer(
        String name,
        boolean isRootContainers
    ) {
        if(name == null) return null;
        CodeValueContainerQuery query = (CodeValueContainerQuery)this.pm.newQuery(CodeValueContainer.class);
        query.thereExistsName().equalTo(name);
        Collection<CodeValueContainer> containers = this.getCodeSegment(
            isRootContainers ? "Root" : this.segmentName
        ).getValueContainer(query);            
        if(!containers.isEmpty()) {
            return containers.iterator().next();
        }
        return null;
    }
    
    //-----------------------------------------------------------------------
    /**
     * Generate next sales order number.
     * @return Next sales order number formatted as {customer number}-{7-digit sales order count}S-{4-digit millis}.
     */
    private String getNextSalesOrderNumber(
        Account customer
    ) {
        SalesOrderQuery salesOrderQuery = (SalesOrderQuery)this.pm.newQuery(SalesOrder.class);
        salesOrderQuery.orderByContractNumber().descending();
        salesOrderQuery.thereExistsCustomer().equalTo(customer);
        List<SalesOrder> salesOrders = this.getContractSegment().getSalesOrder(salesOrderQuery);
        int lastSalesOrderNumber = 1000000;
        if(!salesOrders.isEmpty()) {
            String salesOrderNumber = salesOrders.iterator().next().getContractNumber();
            String[] ids = salesOrderNumber.split("-");
            // We have a sales order number
            if(ids.length == 3 && ids[1].endsWith("S")) {
                lastSalesOrderNumber = Integer.valueOf(ids[1].substring(0, ids[1].length() - 1));
            }            
        }
        long suffix = System.currentTimeMillis() % 10000L;
        return 
            this.datatypeMappers.getAccountFieldMapper().getAccountNumber(customer) + "-" + 
            (lastSalesOrderNumber + 1) + "S-" +
            (suffix < 10 ? ("000" + suffix) : suffix < 100 ? ("00" + suffix) : suffix < 1000 ? ("0" + suffix) : ("" + suffix));
    }
    
    //-----------------------------------------------------------------------
    /**
     * Generate next invoice number.
     * @return Next invoice number formatted as {customer number}-{7-digit invoice count}I-{4-digit millis}.
     */
    protected String getNextInvoiceNumber(
        Account customer
    ) {
        InvoiceQuery invoiceQuery = (InvoiceQuery)this.pm.newQuery(Invoice.class);
        invoiceQuery.orderByContractNumber().descending();
        invoiceQuery.thereExistsCustomer().equalTo(customer);        
        List<Invoice> invoices = this.getContractSegment().getInvoice(invoiceQuery);
        int lastInvoiceNumber = 100000000;
        if(!invoices.isEmpty()) {
            String invoiceNumber = invoices.iterator().next().getContractNumber();
            if(invoiceNumber != null) {
	            String[] ids = invoiceNumber.split("-");
	            // We have an invoice number
	            if(ids.length == 3 && ids[1].endsWith("I")) {
	                lastInvoiceNumber = Integer.valueOf(ids[1].substring(0, ids[1].length() - 1));
	            }
            }
        }        
        long suffix = System.currentTimeMillis() % 10000L;
        return 
            this.datatypeMappers.getAccountFieldMapper().getAccountNumber(customer) + "-" + 
            (lastInvoiceNumber + 1) + "I-" +
            (suffix < 10 ? ("000" + suffix) : suffix < 100 ? ("00" + suffix) : suffix < 1000 ? ("0" + suffix) : ("" + suffix));
    }
    
    //-----------------------------------------------------------------------
    protected SalesContractPosition createContractPosition(
        SalesContract contract,
        String productNumber,
        BigDecimal quantity,
        Boolean ignoreProductConfiguration,
        ProductConfigurationT actualProductConfigurationT,
        String priceUomName,
        Date pricingDate,
        SalesTaxType salesTaxType,
        Boolean discountIsPercentage,
        BigDecimal discount,
        BigDecimal pricePerUnit
    ) {
        Product product = this.findProduct(productNumber);
        Uom priceUom = this.findUom(priceUomName);
        String positionName = product.getName() + " (" + priceUom.getName() + ")";
        CreatePositionParams createPositionParams = Structures.create(
        	CreatePositionParams.class,
        	Datatypes.member(CreatePositionParams.Member.ignoreProductConfiguration, ignoreProductConfiguration),
        	Datatypes.member(CreatePositionParams.Member.name, positionName),
        	Datatypes.member(CreatePositionParams.Member.priceUom, priceUom),
        	Datatypes.member(CreatePositionParams.Member.pricingDate, pricingDate),
        	Datatypes.member(CreatePositionParams.Member.pricingRule, null),
        	Datatypes.member(CreatePositionParams.Member.product, product),
        	Datatypes.member(CreatePositionParams.Member.quantity, quantity),
        	Datatypes.member(CreatePositionParams.Member.priceUom, priceUom)
        );
        PersistenceHelper.currentUnitOfWork(this.pm).begin();
        org.opencrx.kernel.contract1.jmi1.CreatePositionResult createPositionResult = 
        	contract.createPosition(createPositionParams);
        PersistenceHelper.currentUnitOfWork(this.pm).commit();;
        SalesContractPosition contractPosition = (SalesContractPosition)this.pm.getObjectById(createPositionResult.getPosition().refGetPath());
        PersistenceHelper.currentUnitOfWork(this.pm).begin();
        contractPosition.setSalesTaxType(salesTaxType);
        contractPosition.setDiscountIsPercentage(
            discountIsPercentage == null ? Boolean.TRUE : discountIsPercentage
        );
        contractPosition.setDiscount(
            discount == null ? BigDecimal.ZERO : discount
        );
        if(contractPosition instanceof SecureObject) {
            ((SecureObject)contractPosition).getOwningGroup().addAll(
                contract.getOwningGroup()
            );
        }
        // Update actual product configuration
        if(
        	(actualProductConfigurationT != null) &&
        	(contractPosition instanceof ConfiguredProduct)
        ) {
	        ProductConfiguration configuration = ((ConfiguredProduct)contractPosition).getCurrentConfig();
            if(configuration.getName().equals(actualProductConfigurationT.getPropertySetName())) {
                this.updatePropertySet(
                	actualProductConfigurationT.getProperty(),
                    configuration,
                    false
                );
            }
        }        
        PersistenceHelper.currentUnitOfWork(this.pm).commit();;
        this.pm.refresh(contractPosition);
        // Override original price defined by price list
        if(
        	pricePerUnit != null && 
        	pricePerUnit.compareTo(contractPosition.getPricePerUnit()) != 0
        ) {
        	PersistenceHelper.currentUnitOfWork(this.pm).begin();
        	contractPosition.setPricePerUnit(pricePerUnit);
        	PersistenceHelper.currentUnitOfWork(this.pm).commit();;
        }
        return contractPosition;
    }
    
    //-----------------------------------------------------------------------
    protected void updatePropertySet(
        List<StringPropertyT> propertiesT,
        org.opencrx.kernel.base.jmi1.PropertySet propertySet,
        boolean removeNonExisting
    ) {
        Map<String,Object[]> newProperties = new HashMap<String,Object[]>();
        for(StringPropertyT propertyT: propertiesT) {
            for(int i = 0; i < propertyT.getStringValue().size(); i++) {
                newProperties.put(
                    propertyT.getName() + "[" + i + "]",
                    new Object[]{propertyT, i}
                );
            }
        }
        Collection<Property> properties = propertySet.getProperty();
        // Update existing values and remove unused
        for(Property property: properties) {
            if(property instanceof StringProperty) {
                String propertyName = property.getName();
                newProperties.remove(propertyName);
                int propertyIndex = 0;
                if(propertyName.indexOf("[") > 0) {
                    propertyIndex = Integer.valueOf(
                        propertyName.substring(
                            propertyName.indexOf("[") + 1,
                            propertyName.indexOf("]")
                        )
                    );
                    propertyName = propertyName.substring(0, propertyName.indexOf("["));
                }
                StringPropertyT matchingPropertyT = null;
                for(StringPropertyT propertyT: propertiesT) {
                    if(propertyT.getName().equals(propertyName)) {
                        matchingPropertyT = propertyT;
                        break;
                    }
                }
                if(matchingPropertyT != null) {
                    if(propertyIndex < matchingPropertyT.getStringValue().size()) {
                        ((StringProperty)property).setStringValue(
                            matchingPropertyT.getStringValue().get(propertyIndex)
                        );
                    }
                    else {
                        property.refDelete();
                    }
                }
                else if(removeNonExisting) {
                    property.refDelete();
                }
            }
        }
        // Add new values
        for(Map.Entry<String,Object[]> newProperty: newProperties.entrySet()) {
            StringProperty property = this.pm.newInstance(StringProperty.class);
            StringPropertyT propertyT = (StringPropertyT)newProperty.getValue()[0];
            Integer index = (Integer)newProperty.getValue()[1];
            this.datatypeMappers.mapStringProperty(
               propertyT.getName() + "[" + index + "]",
               this.findCodeValueContainer(propertyT.getDomain(), false),
               propertyT.getStringValue().get(index),
               property
            );
            if(propertySet instanceof SecureObject) {
                property.getOwningGroup().addAll(
                    ((SecureObject)propertySet).getOwningGroup()
                );
            }
            propertySet.addProperty(
                false, 
                this.uuidAsString(), 
                property
            );
        }
    }
    
    //-----------------------------------------------------------------------    
    protected void addContractAddresses(
        AbstractContract contract,
        PostalAddressT postalAddressInvoiceT,
        PostalAddressT postalAddressDeliveryT
    ) {
        // postalAddressInvoice
        if(postalAddressInvoiceT != null) {
            org.opencrx.kernel.contract1.jmi1.PostalAddress postalAddressInvoice = pm.newInstance(org.opencrx.kernel.contract1.jmi1.PostalAddress.class);
            this.datatypeMappers.mapPostalAddress(
                postalAddressInvoiceT, 
                postalAddressInvoice
            );
            postalAddressInvoice.getUsage().add(Addresses.USAGE_CONTRACT_INVOICE);
            postalAddressInvoice.getOwningGroup().addAll(
                contract.getOwningGroup()
            );
            contract.addAddress(
                false, 
                this.uuidAsString(),
                postalAddressInvoice
            );
        }
        // postalAddressDelivery
        if(postalAddressDeliveryT != null) {
            org.opencrx.kernel.contract1.jmi1.PostalAddress postalAddressDelivery =  pm.newInstance(org.opencrx.kernel.contract1.jmi1.PostalAddress.class);
            this.datatypeMappers.mapPostalAddress(
                postalAddressDeliveryT,
                postalAddressDelivery
            );
            postalAddressDelivery.getUsage().add(Addresses.USAGE_CONTRACT_DELIVERY);
            postalAddressDelivery.getOwningGroup().addAll(
                contract.getOwningGroup()
            );
            contract.addAddress(
                false, 
                this.uuidAsString(), 
                postalAddressDelivery
            );
        }
    }
    
    //-----------------------------------------------------------------------    
    protected void addContractPositions(
        SalesContract contract,
        List<ContractPositionT> contractPositions,
        String defaultSalesTaxType,
        Boolean discountIsPercentage,
        BigDecimal discount
    ) {
        for(ContractPositionT positionT: contractPositions) {
            String salesTaxTypeName = positionT.getSalesTaxType() == null ? 
            	defaultSalesTaxType : 
            	positionT.getSalesTaxType();
            SalesTaxType salesTaxType = this.findSalesTaxType(salesTaxTypeName);
            this.createContractPosition(
                contract, 
                positionT.getProductNumber(), 
                new BigDecimal(positionT.getQuantity()), 
                this.noCopyOfProductConfiguration,
                positionT.getActualProductConfiguration(),
                positionT.getPriceUom(), 
                positionT.getPricingDate(), 
                salesTaxType,
                discountIsPercentage == null ? 
                    positionT.isDiscountIsPercentage() : 
                    discountIsPercentage,
                discount == null ? 
                    positionT.getDiscount() == null ?
                        null : 
                        new BigDecimal(positionT.getDiscount()) :
                    discount,
                positionT.getPricePerUnit() == null ?
            		null :
            			new BigDecimal(positionT.getPricePerUnit())
            );
        }
    }

    //-----------------------------------------------------------------------    
    protected String validateContractPosition(
    	String productNumber,
    	String priceUomName
    ) {
    	// Check existence of product
        Product product = this.findProduct(productNumber);
        if(product == null) {
    		return "product '" + productNumber + "' not found";
        }            
        // Check existence of priceUom
        Uom priceUom = this.findUom(priceUomName);
        if(priceUom == null) {
    		return "priceUom '" + priceUom + "' not found";            	
        }
        // Check whether priceUom is supported by product
        if(!product.getPriceUom().contains(priceUom)) {
        	Set<String> validPriceUoms = new HashSet<String>();
        	Collection<Uom> priceUoms = product.getPriceUom();
        	for(Uom uom: priceUoms) {
        		validPriceUoms.add(uom.getName());
        	}
    		return "priceUom '" + priceUom + "' not valid. Must be one of " + validPriceUoms;            	
        }    	
        return null;
    }
    
    //-----------------------------------------------------------------------    
    protected String validateContractPositions(
        List<ContractPositionT> contractPositions
    ) {
    	int ii = 0;
        for(ContractPositionT position: contractPositions) {
        	String validationResult = this.validateContractPosition(
        		position.getProductNumber(),
        		position.getPriceUom()
        	);
        	if(validationResult != null) {
        		return "ContractPosition " + ii + ": " + validationResult;
        	}
        	ii++;
        }
        return null;
    }
    
    //-----------------------------------------------------------------------    
    protected Collection<Object> toNonNullElementsCollection(
        Collection<?> source,
        Collection<Object> target,
        Object nullValue
    ) {        
        for(Object e: source) {
            if(e == null) {
                target.add(nullValue);
            }
            else {
                target.add(e);
            }
        }
        return target;
    }
    
    //-----------------------------------------------------------------------    
    protected void updateProductConfiguration(
        ProductT productT,
        Product product,
        ProductConfigurationTypeSet configurationType
    ) {
        SetConfigurationTypeParams setConfigurationTypeParams = Structures.create(
        	SetConfigurationTypeParams.class, 
        	Datatypes.member(SetConfigurationTypeParams.Member.configurationType, configurationType)
        );
        product.setConfigurationType(setConfigurationTypeParams);
        // Configuration properties
        Collection<ProductConfiguration> configurations = product.getConfiguration();
        Collection<ProductConfiguration> configurationsToBeDeleted = new ArrayList<ProductConfiguration>();
        for(ProductConfiguration configuration: configurations) {
            if(productT.getPriceUom().contains(configuration.getName())) {
                // Overwrite the configuration properties defined by productT
                for(ProductConfigurationT configurationT: productT.getConfiguration()) {
                    if(configuration.getName().equals(configurationT.getPropertySetName())) {
                        this.updatePropertySet(
                            configurationT.getProperty(),
                            configuration,
                            false
                        );
                    }
                }
            }
            // A configuration is only valid if it has a matching uom. 
            // Remove configurations set by setConfigurationType() but 
            // do not have a corresponding uom.
            else {
            	configurationsToBeDeleted.add(configuration);
            }
        }
        for(ProductConfiguration configuration: configurationsToBeDeleted) {
        	configuration.refDelete();
        }
    }
    
    //-----------------------------------------------------------------------    
    protected void updateProductDescriptions(
        ProductT productT,
        Product product
    ) {
        for(ProductDescriptionT descriptionT: productT.getDescription()) {
            Description description = null;
            Collection<Description> descriptions = product.getAdditionalDescription();
            for(Description e: descriptions) {
                if(e.getLanguage() == descriptionT.getLanguage()) {
                    description = e;
                    break;
                }
            }
            if(description == null) {
                description = pm.newInstance(Description.class);
                description.getOwningGroup().addAll(
                    product.getOwningGroup()
                );
                product.addAdditionalDescription(
                    false, 
                    this.uuidAsString(), 
                    description
                );
            }
            description.setDescription(descriptionT.getDescription());
            description.setDetailedDescription(descriptionT.getDetailedDescription());
            description.setLanguage((short)descriptionT.getLanguage());                        
        }
    }
    
    //-----------------------------------------------------------------------    
    protected void updateProductPicture(
        ProductT productT,
        Product product
    ) {
        if(productT.getPictureTitle() != null) {
            Media picture = null;
            Collection<Media> pictures = product.getMedia();
            for(Media e: pictures) {
                if(
                    (e.getContentName() != null) && 
                    e.getContentName().equals(productT.getPictureTitle())
                ) {
                    picture = e;
                    break;                
                }
            }
            if(picture == null) {
                picture = pm.newInstance(Media.class);
                picture.getOwningGroup().addAll(
                    product.getOwningGroup()
                );
                product.addMedia(
                    this.uuidAsString(), 
                    picture
                );
            }
            picture.setDescription(productT.getPictureTitle());
            picture.setContentName(productT.getPictureTitle());
            picture.setContentMimeType(productT.getPictureMimeType());
            if(productT.getPictureContent() != null) {
                picture.setContent(BinaryLargeObjects.valueOf(productT.getPictureContent()));
            }
            product.setPicture(picture);
        }
        else {
            product.setPicture(null);
        }
    }
    
    //-----------------------------------------------------------------------    
    protected void updateProductPhase(
        ProductT productT,
        Product product
    ) {
        Collection<ProductPhase> productPhases = product.getProductPhase();
        for(ProductPhase productPhase: productPhases) {
            productPhase.refDelete();
        }
        for(ProductPhaseT productPhaseT: productT.getProductPhase()) {
            ProductPhase productPhase = pm.newInstance(ProductPhase.class);
            productPhase.setName(productPhaseT.getName());
            productPhase.setProductPhaseKey(productPhaseT.getProductPhaseKey());
            productPhase.setValidFrom(productPhaseT.getValidFrom());
            productPhase.setValidTo(productPhaseT.getValidTo());
            productPhase.getOwningGroup().addAll(
                product.getOwningGroup()
            );
            product.addProductPhase(
                this.uuidAsString(), 
                productPhase
            );
        }
    }

    //-----------------------------------------------------------------------
    protected Collection<ContractPositionT> getSalesOrderPositions(
        Account customer,
        Integer salesOrderStatusThreshold
    ) {
        String contractCategory = this.getShopCategory();
        List<ContractPositionT> positionsT = new ArrayList<ContractPositionT>();
        SalesOrderQuery salesOrderQuery = (SalesOrderQuery)this.pm.newQuery(SalesOrder.class);
        salesOrderQuery.thereExistsCustomer().equalTo(customer);
        if(salesOrderStatusThreshold != null) {
            salesOrderQuery.contractState().greaterThanOrEqualTo(salesOrderStatusThreshold.shortValue());
        }	        
        if(contractCategory != null) {
        	salesOrderQuery.thereExistsCategory().equalTo(contractCategory);
        }
        for(SalesOrder salesOrder: this.getContractSegment().getSalesOrder(salesOrderQuery)) {
            Collection<AbstractSalesOrderPosition> positions = salesOrder.getPosition();
            for(AbstractSalesOrderPosition position: positions) {
                if(position instanceof SalesOrderPosition) {
                    positionsT.add(
                        this.datatypeMappers.mapContractPosition(
                            position, 
                            salesOrder.getContractNumber(),    
                            null, // paymentDate
                            ((SalesOrderPosition)position).getProduct()
                        )
                    );                        
                }
            }
        }
        return positionsT;
    }
    
    //-----------------------------------------------------------------------
    public void setContractStatus(
        AbstractContract contract,
        ContractStatusT contractStatusT
    ) {
	    // Update state history
	    String shortDescription = contract.getDescription();
	    shortDescription = shortDescription == null ? 
	    	"" : 
	    		shortDescription.substring(0, Math.min(shortDescription.length(), 50));
	    if(contract.getModifiedAt() != null) {
		    this.datatypeMappers.getAbstractContractFieldMapper().getStateHistory(contract).add(
		    	contract.getContractState() + "@" +
		        DateTimeFormat.BASIC_UTC_FORMAT.format(contract.getModifiedAt()) + " // " +
		        shortDescription
		    );
	    }
	    contract.setContractState(DatatypeMappers.toShort(contractStatusT.getStatus()));
	    contract.setDescription(contractStatusT.getDescription());
	    if(contractStatusT.getTag() != null) {
	        Collection<PropertySet> propertySets = contract.getPropertySet();
	        String contractStatusPropertySetName = PropertySetName.ContractStatus + "." + contract.getContractState();
	        PropertySet contractStatusProperties = null;
	        for(PropertySet propertySet: propertySets) {
	            if(propertySet.getName().equals(contractStatusPropertySetName)) {
	                contractStatusProperties = propertySet;
	                break;
	            }
	        }
	        if(contractStatusProperties == null) {
	            contractStatusProperties = pm.newInstance(PropertySet.class);
	            contractStatusProperties.setName(contractStatusPropertySetName);
	            contractStatusProperties.getOwningGroup().addAll(
	            	contract.getOwningGroup()
	            );
	            contract.addPropertySet(
	                this.uuidAsString(), 
	                contractStatusProperties
	            );
	        }
	        this.updatePropertySet(
	            contractStatusT.getTag(), 
	            contractStatusProperties, 
	            true
	        );
	    }
    }                
    
    //-----------------------------------------------------------------------    
    public void updateProductBundle(
        ProductT productT,
        Product product
    ) {
        Boolean isBundle = productT.isBundle();
        this.datatypeMappers.getProductFieldMapper().setIsBundle(product, productT.isBundle());
        if((isBundle != null) && isBundle.booleanValue()) {
        	org.opencrx.kernel.product1.jmi1.Segment productSegment = this.getProductSegment();
        	ProductFilterGlobal bundleFilter = null;
        	// Find product filter
        	Collection<ProductFilterGlobal> productFilters = productSegment.getProductFilter();
        	for(ProductFilterGlobal f: productFilters) {
        		if(f.getName().equals(productT.getProductNumber())) {
        			bundleFilter = f;
        			break;
        		}
        	}
        	// Create if it does not exist
        	if(bundleFilter == null) {
	            bundleFilter = this.pm.newInstance(ProductFilterGlobal.class);
	            bundleFilter.setName(productT.getProductNumber());
	            bundleFilter.setDescription("Filter for product bundle " + productT.getProductNumber());
	            this.datatypeMappers.getProductFilterFieldMapper().setProductFilterType(
	                bundleFilter, 
	                ProductFilterType.PRODUCT_BUNDLE.getValue()
	            );
	            bundleFilter.getOwningGroup().addAll(
	                productSegment.getOwningGroup()
	            );
	            productSegment.addProductFilter(
	                this.uuidAsString(), 
	                bundleFilter
	            );
        	}
        	// Find filter property
        	ProductClassificationFilterProperty filterProperty = null;
        	Collection<ProductClassificationFilterProperty> filterProperties = bundleFilter.getProductFilterProperty();
        	for(ProductClassificationFilterProperty p: filterProperties) {
        		if("Classifications".equals(p.getName())) {
        			filterProperty = p;
        			break;
        		}
        	}
        	// Create if it does not exist
        	if(filterProperty == null) {
        		filterProperty = pm.newInstance(ProductClassificationFilterProperty.class);
        		filterProperty.setName("Classifications");
        		filterProperty.setActive(Boolean.TRUE);
        		filterProperty.setFilterOperator(ConditionType.IS_IN.code());
        		filterProperty.setFilterQuantor(Quantifier.THERE_EXISTS.code());
                filterProperty.getOwningGroup().addAll(
                    bundleFilter.getOwningGroup()
                );
                bundleFilter.addProductFilterProperty(
                    this.uuidAsString(), 
                    filterProperty
                );
        	}
        	// Update filter values
        	filterProperty.getClassification().clear();
            if((productT.getBundleData() != null) && (productT.getBundleData().getClassificationIdFilter() != null)) {
                for(String classificationId: productT.getBundleData().getClassificationIdFilter()) {
                    filterProperty.getClassification().add(
                        this.findProductClassification(classificationId)
                    );
                }
            }
        }
    }
    
	//-----------------------------------------------------------------------
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#addActivityFollowUp(org.opencrx.application.shop1.cci2.AddActivityFollowUpParams)
     */
    @Override
	public AddActivityFollowUpResult addActivityFollowUp(
		AddActivityFollowUpParams params
	) {
        List<Structures.Member<AddActivityFollowUpResult.Member>> result = 
            new ArrayList<Structures.Member<AddActivityFollowUpResult.Member>>();
        try {
        	String activityNumber = params.getActivityNumber(); 
        	if(activityNumber != null && activityNumber.length() > 0) {
        		org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment();
        		Activity activity = this.findActivity(
        			activityNumber, 
        			activitySegment 
        		);
        		if(activity != null) {
        			String transitionName = params.getTransitionName();
        			ActivityProcessTransition transition = Activities.getInstance().findActivityProcessTransition(
        				activity, 
        				transitionName
        			);
        			if(transition != null) {
	        			Account reportingCustomer = null;
	        			if(params.getReportingCustomerNumber() != null) {
	        				reportingCustomer = this.findAccount(params.getReportingCustomerNumber());
	        			}
						ActivityDoFollowUpParams doFollowUpParams = Structures.create(
							ActivityDoFollowUpParams.class, 
							Datatypes.member(ActivityDoFollowUpParams.Member.assignTo, reportingCustomer instanceof Contact ? (Contact)reportingCustomer : null),
							Datatypes.member(ActivityDoFollowUpParams.Member.followUpText, params.getDescription()),
							Datatypes.member(ActivityDoFollowUpParams.Member.followUpTitle, params.getName()),
							Datatypes.member(ActivityDoFollowUpParams.Member.transition, transition)
						);
	        			PersistenceHelper.currentUnitOfWork(this.pm).begin();
	        			ActivityDoFollowUpResult doFollowUpResult = activity.doFollowUp(doFollowUpParams);
	        			PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	        			ActivityFollowUp followUp = doFollowUpResult.getFollowUp();
	        			this.pm.refresh(followUp);
	        			if(params.getCategory() != null && !params.getCategory().isEmpty()) {
		        			PersistenceHelper.currentUnitOfWork(this.pm).begin();
		        			followUp.getCategory().addAll(params.getCategory());
		        			PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	        			}
	                    result.add(
	                        Datatypes.member(
	                        	AddActivityFollowUpResult.Member.followUp,
	                            this.datatypeMappers.mapActivityFollowUp(
	                                followUp
	                            )
	                        )                            
	                    );                        
	                    result.add(
	                        Datatypes.member(
	                        	AddActivityFollowUpResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NONE,
	                                null
	                            )
	                        )                            
	                    );
        			}
        			else {
                        result.add(
                            Datatypes.member(
                            	AddActivityFollowUpResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.NOT_FOUND,
                                    new String[]{"ActivityProcessTransition", transitionName}
                                )
                            )                            
                        );                                                                        		        			        				
        			}
        		}
        		else {
                    result.add(
                        Datatypes.member(
                        	AddActivityFollowUpResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"Activity", activityNumber}
                            )
                        )                            
                    );                                                                        		        			
        		}
        	}
        	else {
                result.add(
                    Datatypes.member(
                    	AddActivityFollowUpResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.ASSERTION_FAILURE,
                            new String[]{"ActivityNumber is null or empty", activityNumber}
                        )
                    )                            
                );                                                                        		
        	}
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	AddActivityFollowUpResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	AddActivityFollowUpResult.class,
            result
        );
    }

	//-----------------------------------------------------------------------
	/* (non-Javadoc)
	 * @see org.opencrx.application.shop1.cci2.ShopService#addCustomerToCustomerContract(org.opencrx.application.shop1.cci2.AddCustomerToCustomerContractParams)
	 */
    @Override
	public AddCustomerToCustomerContractResult addCustomerToCustomerContract(
		AddCustomerToCustomerContractParams params
	) {
        List<Structures.Member<AddCustomerToCustomerContractResult.Member>> result = 
            new ArrayList<Structures.Member<AddCustomerToCustomerContractResult.Member>>();
        try {
        	String customerNumber = params.getCustomerNumber();
        	Account customer = this.findAccount(customerNumber);
        	if(customer != null) {
        		String customerContractNumber = params.getCustomerContractNumber();
    			List<Lead> customerContracts = this.findCustomerContracts(customer);
        		Lead customerContract = this.findCustomerContractByContractNumber(customerContractNumber);
    			if(customerContract != null) {
            		if(customerContracts == null || !customerContracts.contains(customerContract)) {
        				PersistenceHelper.currentUnitOfWork(this.pm).begin();
        				AccountAssignmentContract accountAssignment = this.pm.newInstance(AccountAssignmentContract.class);
        				accountAssignment.setAccount(customer);
        				customerContract.addAssignedAccount(
        					this.uuidAsString(), 
        					accountAssignment
        				);
        				PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	                    result.add(
	                        Datatypes.member(
	                        	AddCustomerToCustomerContractResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NONE,
	                                new String[]{"CustomerContract", customerContractNumber}
	                            )
	                        )                            
	                    );        				
        			}
        			else {
                        result.add(
                            Datatypes.member(
                            	AddCustomerToCustomerContractResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.DUPLICATE,
                                    new String[]{"Duplicate assignment of customer contract", customerNumber, customerContractNumber}
                                )
                            )                            
                        );                                                                        		        			        		        			
        			}
        		}
        		else {
                    result.add(
                        Datatypes.member(
                        	AddCustomerToCustomerContractResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"Customer contract not found", customerContractNumber}
                            )
                        )                            
                    );                                                                        		        			        		        			        				
        		}
        	}
        	else {
                result.add(
                    Datatypes.member(
                    	AddCustomerToCustomerContractResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Customer", customerNumber}
                        )
                    )                            
                );                                                                        		        			        		
        	}        	
        }
	    catch(Exception e) {
	        try {
	            PersistenceHelper.currentUnitOfWork(this.pm).rollback();
	        } catch(Exception e0) {}            
	        new ServiceException(e).log();
	        result.add(
	            Datatypes.member(
	            	AddCustomerToCustomerContractResult.Member.status,
	                this.datatypeMappers.mapOperationStatus(e)
	            )                            
	        );
	    }
	    return Structures.create(
	    	AddCustomerToCustomerContractResult.class,
	        result
	    );
    }

	//-----------------------------------------------------------------------
	/* (non-Javadoc)
	 * @see org.opencrx.application.shop1.cci2.ShopService#addDeliveryInformation(org.opencrx.application.shop1.cci2.AddDeliveryInformationParams)
	 */
    @Override
	public AddDeliveryInformationResult addDeliveryInformation(
		AddDeliveryInformationParams params
	) {
        List<Structures.Member<AddDeliveryInformationResult.Member>> result = 
            new ArrayList<Structures.Member<AddDeliveryInformationResult.Member>>();
        try {
        	String contractNumber = params.getContractNumber();
        	String positionNumber = params.getPositionNumber();
        	DeliveryInformationT deliveryInformationT = params.getDeliveryInformation();
        	if(deliveryInformationT != null && contractNumber != null && contractNumber.length() > 0) {
        		AbstractContract contract = this.findSalesOrder(contractNumber);
        		if(contract == null) {
        			contract = this.findInvoice(contractNumber);
        		}
        		if(contract != null) {
	        		SalesContractPosition position = null;
	        		if(contract instanceof SalesOrder) {
	        			Collection<AbstractSalesOrderPosition> positions = ((SalesOrder)contract).getPosition();
	        			for(AbstractSalesOrderPosition p: positions) {
	        				if(p.getPositionNumber().equals(positionNumber)) {
	        					position = p;
	        					break;
	        				}
	        			}
	        		}
	        		else if(contract instanceof Invoice) {
	        			Collection<AbstractInvoicePosition> positions = ((Invoice)contract).getPosition();
	        			for(AbstractInvoicePosition p: positions) {
	        				if(p.getPositionNumber().equals(positionNumber)) {
	        					position = p;
	        					break;
	        				}
	        			}        			
	        		}
	        		if(position != null) {
	        			PersistenceHelper.currentUnitOfWork(this.pm).begin();
	        			DeliveryInformation deliveryInformation = this.pm.newInstance(DeliveryInformation.class);
	        			deliveryInformation.setActualDeliveryOn(deliveryInformationT.getActualDeliveryOn());
	        			deliveryInformation.setQuantityShipped(
	        				deliveryInformationT.getQuantityShipped() == null ?
	        					BigDecimal.ZERO :
	        						new BigDecimal(deliveryInformationT.getQuantityShipped())
	        			);
	        			this.datatypeMappers.getDeliveryInformationFieldMapper().setDeliveryStatus(
	        				deliveryInformation, 
	        				deliveryInformationT.getDeliveryStatus()
	        			);
	        			this.datatypeMappers.getDeliveryInformationFieldMapper().setDeliveryStatusDescription(
	        				deliveryInformation, 
	        				deliveryInformationT.getDeliveryStatusDescription()
	        			);
	        			this.datatypeMappers.getDeliveryInformationFieldMapper().setProductAssembledAt(
	        				deliveryInformation, 
	        				deliveryInformationT.getProductAssembledAt()
	        			);
	        			position.addDeliveryInformation(
	        				this.uuidAsString(), 
	        				deliveryInformation
	        			);
	        			PersistenceHelper.currentUnitOfWork(this.pm).commit();;	      
	                    result.add(
	                        Datatypes.member(
	                        	AddDeliveryInformationResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NONE,
	                                null
	                            )
	                        )                            
	                    );	
	        		}
	        		else {
	                    result.add(
	                        Datatypes.member(
	                        	AddDeliveryInformationResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NOT_FOUND,
	                                new String[]{"Position", contractNumber, positionNumber}
	                            )
	                        )                            
	                    );                                                                        		        			        				        			
	        		}
        		}
        		else {
                    result.add(
                        Datatypes.member(
                        	AddDeliveryInformationResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"(SalesOrder|Invoice)", contractNumber}
                            )
                        )                            
                    );                                                                        		        			        			
        		}
        	}
        	else {
                result.add(
                    Datatypes.member(
                    	AddDeliveryInformationResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.ASSERTION_FAILURE,
                            new String[]{"ContractNumber or DeliveryInformation is null or empty", contractNumber}
                        )
                    )                            
                );                                                                        		        		
        	}
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	AddDeliveryInformationResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	AddDeliveryInformationResult.class,
            result
        );
    }

    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#addSalesOrderPosition(org.opencrx.application.shop1.cci2.AddSalesOrderPositionParams)
     */
    @Override
    public AddSalesOrderPositionResult addSalesOrderPosition(
        AddSalesOrderPositionParams params
    ) {
        List<Structures.Member<AddSalesOrderPositionResult.Member>> result = 
            new ArrayList<Structures.Member<AddSalesOrderPositionResult.Member>>();
        try {
            SalesOrder salesOrder = this.findSalesOrder(params.getSalesOrderNumber());
            if(salesOrder != null) {
                String customerNumber = this.datatypeMappers.getAccountFieldMapper().getAccountNumber(salesOrder.getCustomer());
                Lead customerContract = this.findCustomerContract(salesOrder);
                if(customerContract != null) {
                    String salesTaxTypeName = params.getSalesTaxType() == null || params.getSalesTaxType().isEmpty() ?
                    	this.datatypeMappers.getLeadFieldMapper().getSalesTaxType(customerContract) :
                    		params.getSalesTaxType();
                    SalesTaxType salesTaxType = this.findSalesTaxType(salesTaxTypeName);
                    if(salesTaxType != null || salesTaxTypeName == null) {
                    	String validationResult = this.validateContractPosition(
                       		params.getProductNumber(),
                    		params.getPriceUom()
                    	);
                    	if(validationResult == null) {
	                        this.createContractPosition(
	                            salesOrder, 
	                            params.getProductNumber(),
	                            new BigDecimal(params.getQuantity()), 
	                            this.noCopyOfProductConfiguration,
	                            params.getActualProductConfiguration(),
	                            params.getPriceUom(), 
	                            params.getPricingDate(),
	                            salesTaxType,
	                            params.isDiscountIsPercentage() == null ? Boolean.TRUE : params.isDiscountIsPercentage(),
	                            params.getDiscount() == null ? BigDecimal.ZERO : new BigDecimal(params.getDiscount()),
	                            params.getPricePerUnit() == null ? null : new BigDecimal(params.getPricePerUnit())
	                        );
	                        result.add(
	                            Datatypes.member(
	                                AddSalesOrderPositionResult.Member.status,
	                                this.datatypeMappers.mapOperationStatus(
	                                    BasicException.Code.NONE,
	                                    null
	                                )
	                            )                            
	                        );
	                    }
                    	else {
                            result.add(
                                Datatypes.member(
                                    AddSalesOrderPositionResult.Member.status,
                                    this.datatypeMappers.mapOperationStatus(
                                        BasicException.Code.ASSERTION_FAILURE,
                                        new String[]{"SalesOrderPosition", validationResult}
                                    )                       
                                )                            
                            );                                                                                                                            		
                    	}
                    }
                    else {
                        result.add(
                            Datatypes.member(
                                AddSalesOrderPositionResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.NOT_FOUND,
                                    new String[]{"SalesTaxType", salesTaxTypeName}
                                )                       
                            )                            
                        );                                                                                                        
                    }
                }
                else {
                    result.add(
                        Datatypes.member(
                            AddSalesOrderPositionResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"CustomerContract", customerNumber}
                            )                       
                        )                            
                    );                                                                                
                }
            }
            else {
                result.add(
                    Datatypes.member(
                        AddSalesOrderPositionResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"SalesOrder", params.getSalesOrderNumber()}
                        )                       
                    )                            
                );                                                            
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    AddSalesOrderPositionResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            AddSalesOrderPositionResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setInvoiceStatus(org.opencrx.application.shop1.cci2.SetInvoiceStatusParams)
     */
    @Override
    public CancelInvoiceResult cancelInvoice(
        CancelInvoiceParams params
    ) {
        List<Structures.Member<CancelInvoiceResult.Member>> result = 
            new ArrayList<Structures.Member<CancelInvoiceResult.Member>>();
        try {
            Invoice invoice = this.findInvoice(params.getInvoiceNumber());
            if(invoice != null) {
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                invoice.setCancelOn(
                	params.getCancelOn() == null ?
                		new Date() :
                		params.getCancelOn()
                );
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;      
                result.add(
                    Datatypes.member(
                        CancelInvoiceResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                                                                    
            }
            else {
                result.add(
                    Datatypes.member(
                        CancelInvoiceResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Invoice", params.getInvoiceNumber()}
                        )
                    )                            
                );                                                                    
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CancelInvoiceResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CancelInvoiceResult.class,
            result
        );
    }
                  
	//-----------------------------------------------------------------------
	/* (non-Javadoc)
	 * @see org.opencrx.application.shop1.cci2.ShopService#cancelSalesOrder(org.opencrx.application.shop1.cci2.CancelSalesOrderParams)
	 */
    @Override
	public CancelSalesOrderResult cancelSalesOrder(
		CancelSalesOrderParams params
	) {
        List<Structures.Member<CancelSalesOrderResult.Member>> result = 
            new ArrayList<Structures.Member<CancelSalesOrderResult.Member>>();
        try {
            SalesOrder salesOrder = this.findSalesOrder(params.getSalesOrderNumber());
            if(salesOrder != null) {
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                salesOrder.setCancelOn(
                	params.getCancelOn() == null ?
                		new Date() :
                		params.getCancelOn()
                );
                // Disable all positions
                Collection<SalesOrderPosition> positions = salesOrder.getPosition();
                for(SalesOrderPosition position: positions) {
                	position.setDisabled(true);
                }
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;      
                result.add(
                    Datatypes.member(
                        CancelSalesOrderResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                                                                    
            }
            else {
                result.add(
                    Datatypes.member(
                    	CancelSalesOrderResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"SalesOrder", params.getSalesOrderNumber()}
                        )
                    )                            
                );                                                                    
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	CancelSalesOrderResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	CancelSalesOrderResult.class,
            result
        );
    }
    
	//-----------------------------------------------------------------------
	/* (non-Javadoc)
	 * @see org.opencrx.application.shop1.cci2.ShopService#createActivity(org.opencrx.application.shop1.cci2.CreateActivityParams)
	 */
    @Override
	public CreateActivityResult createActivity(
		CreateActivityParams params
	) {
        List<Structures.Member<CreateActivityResult.Member>> result = 
            new ArrayList<Structures.Member<CreateActivityResult.Member>>();
        try {
        	String activityCreatorName = params.getActivityCreatorName(); 
        	if(activityCreatorName != null && activityCreatorName.length() > 0) {
        		org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment();
        		ActivityCreator activityCreator = Activities.getInstance().findActivityCreator(
        			activityCreatorName, 
        			activitySegment 
        		);
        		if(activityCreator != null) {
        			Account reportingCustomer = null;
        			if(params.getReportingCustomerNumber() != null) {
        				reportingCustomer = this.findAccount(params.getReportingCustomerNumber());
        			}
        			NewActivityParams newActivityParams = Structures.create(
        				NewActivityParams.class, 
        				Datatypes.member(NewActivityParams.Member.creationContext, null),
        				Datatypes.member(NewActivityParams.Member.description, params.getDescription()),
        				Datatypes.member(NewActivityParams.Member.detailedDescription, params.getDetailedDescription()),
        				Datatypes.member(NewActivityParams.Member.dueBy, params.getDueBy()),
        				Datatypes.member(NewActivityParams.Member.icalType, ICalendar.ICAL_TYPE_NA),
        				Datatypes.member(NewActivityParams.Member.name, params.getName()),
        				Datatypes.member(NewActivityParams.Member.priority, params.getPriority() == null ? (short)0 : params.getPriority().shortValue()),
        				Datatypes.member(NewActivityParams.Member.reportingContact, reportingCustomer instanceof Contact ? (Contact)reportingCustomer : null),
        				Datatypes.member(NewActivityParams.Member.scheduledEnd, params.getScheduledEnd()),
        				Datatypes.member(NewActivityParams.Member.scheduledStart, params.getScheduledStart())
        			);
        			PersistenceHelper.currentUnitOfWork(this.pm).begin();
        			NewActivityResult newActivityResult = activityCreator.newActivity(newActivityParams);
        			PersistenceHelper.currentUnitOfWork(this.pm).commit();;
        			Activity activity = newActivityResult.getActivity();
        			this.pm.refresh(activity);
                    result.add(
                        Datatypes.member(
                        	CreateActivityResult.Member.activity,
                            this.datatypeMappers.mapActivity(
                                activity
                            )
                        )                            
                    );                                
                    result.add(
                        Datatypes.member(
                        	CreateActivityResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NONE,
                                null
                            )
                        )                            
                    );                                        			
        		}
        		else {
                    result.add(
                        Datatypes.member(
                        	CreateActivityResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"ActivityCreator", activityCreatorName}
                            )
                        )                            
                    );                                                                        		        			
        		}
        	}
        	else {
                result.add(
                    Datatypes.member(
                    	CreateActivityResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.ASSERTION_FAILURE,
                            new String[]{"ActivityCreator is null or empty", activityCreatorName}
                        )
                    )                            
                );                                                                        		
        	}
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	CreateActivityResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	CreateActivityResult.class,
            result
        );
    }

    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createCustomerAsContact(org.opencrx.application.shop1.cci2.CreateCustomerAsContactParams)
     */
    @Override
    public CreateCustomerAsContactResult createCustomerAsContact(
        CreateCustomerAsContactParams params
    ) {
        List<Structures.Member<CreateCustomerAsContactResult.Member>> result = 
            new ArrayList<Structures.Member<CreateCustomerAsContactResult.Member>>();
        try {
            AccountQuery accountQuery = (AccountQuery)this.pm.newQuery(Account.class);
            accountQuery.thereExistsUserString0().equalTo(params.getUserName());
            List<Account> accounts = this.getAccountSegment().getAccount(accountQuery);
            if(accounts.isEmpty()) {
                boolean isDuplicateEMailAddressHome = false;
                boolean isDuplicateEMailAddressBusiness = false;
                if(this.emailAddressMustBeUnique) {
                	if(params.getEmailAddressHome() != null) {
	                    EMailAddressQuery addressQuery = (EMailAddressQuery)this.pm.newQuery(EMailAddress.class);
	                    addressQuery.thereExistsEmailAddress().equalTo(params.getEmailAddressHome());
	                    List<AccountAddress> addresses = this.getAccountSegment().getAddress(addressQuery);
	                    isDuplicateEMailAddressHome = !addresses.isEmpty();
                	}
                	if(params.getEmailAddressBusiness() != null) {
	                    EMailAddressQuery addressQuery = (EMailAddressQuery)this.pm.newQuery(EMailAddress.class);
	                    addressQuery.thereExistsEmailAddress().equalTo(params.getEmailAddressBusiness());
	                    List<AccountAddress> addresses = this.getAccountSegment().getAddress(addressQuery);
	                    isDuplicateEMailAddressBusiness = !addresses.isEmpty();                		
                	}
                }
                if(
                	!this.emailAddressMustBeUnique || 
                	(!isDuplicateEMailAddressHome && !isDuplicateEMailAddressBusiness)
                ) {
                    // Create Customer
                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
                    Contact customer = this.pm.newInstance(Contact.class);
                    customer.setLastName(params.getLastName());
                    customer.setFirstName(params.getFirstName());
                    this.datatypeMappers.getAccountFieldMapper().setUserName(customer, params.getUserName());
                    customer.getOwningGroup().addAll(
                        this.getAccountSegment().getOwningGroup()
                    );
                    this.getAccountSegment().addAccount(
                        false, 
                        this.uuidAsString(), 
                        customer
                    );
                    // Create Email address home
                    if(params.getEmailAddressHome() != null && params.getEmailAddressHome().length() > 0) {
	                    EMailAddress emailAddressHome = this.pm.newInstance(EMailAddress.class);
	                    emailAddressHome.setEmailAddress(params.getEmailAddressHome());
	                    emailAddressHome.getUsage().add(Addresses.USAGE_HOME);
	                    this.datatypeMappers.getEmailAddressFieldMapper().setEmailValid(emailAddressHome, false); // emailValid
	                    emailAddressHome.getOwningGroup().addAll(
	                        customer.getOwningGroup()
	                    );
	                    customer.addAddress(
	                        false, 
	                        this.uuidAsString(), 
	                        emailAddressHome
	                    );
                    }
                    // Create Email address business
                    if(params.getEmailAddressBusiness() != null && params.getEmailAddressBusiness().length() > 0) {
	                    EMailAddress emailAddressBusiness = this.pm.newInstance(EMailAddress.class);
	                    emailAddressBusiness.setEmailAddress(params.getEmailAddressBusiness());
	                    emailAddressBusiness.getUsage().add(Addresses.USAGE_BUSINESS);
	                    this.datatypeMappers.getEmailAddressFieldMapper().setEmailValid(emailAddressBusiness, false); // emailValid
	                    emailAddressBusiness.getOwningGroup().addAll(
	                        customer.getOwningGroup()
	                    );
	                    customer.addAddress(
	                        false, 
	                        this.uuidAsString(), 
	                        emailAddressBusiness
	                    );
                    }
                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                    customer = (Contact)this.pm.getObjectById(customer.refGetPath());
                    this.pm.refresh(customer);
                    result.add(
                        Datatypes.member(
                            CreateCustomerAsContactResult.Member.customer,
                            this.datatypeMappers.mapCustomer(
                                customer, 
                                null,
                                this.getShopCategory()
                            )
                        )                                                  
                    );                        
                    result.add(
                        Datatypes.member(
                            CreateCustomerAsContactResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NONE,
                                null
                            )
                        )                            
                    );
                }
                else {
                    result.add(
                        Datatypes.member(
                            CreateCustomerAsContactResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.DUPLICATE,
                                new String[]{"EmailAddress", params.getEmailAddressHome()}
                            )
                        )                            
                    );                                                    
                }
            }
            else {
                result.add(
                    Datatypes.member(
                        CreateCustomerAsContactResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.DUPLICATE,
                            new String[]{"Account", params.getUserName()}
                        )
                    )                            
                );                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateCustomerAsContactResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateCustomerAsContactResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createCustomerAsLegalEntity(org.opencrx.application.shop1.cci2.CreateCustomerAsContactParams)
     */
    @Override
    public CreateCustomerAsLegalEntityResult createCustomerAsLegalEntity(
        CreateCustomerAsLegalEntityParams params
    ) {
        List<Structures.Member<CreateCustomerAsLegalEntityResult.Member>> result = 
            new ArrayList<Structures.Member<CreateCustomerAsLegalEntityResult.Member>>();
        try {
            LegalEntityQuery legalEntityQuery = (LegalEntityQuery)this.pm.newQuery(LegalEntity.class);
            legalEntityQuery.name().equalTo(params.getLegalName());
            List<Account> legalEntities = this.getAccountSegment().getAccount(legalEntityQuery);
            if(legalEntities.isEmpty()) {
                // Create Customer
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                LegalEntity customer = this.pm.newInstance(LegalEntity.class);
                customer.setName(params.getLegalName());
                customer.getOwningGroup().addAll(
                    this.getAccountSegment().getOwningGroup()
                );
                this.getAccountSegment().addAccount(
                    false, 
                    this.uuidAsString(), 
                    customer
                );
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                customer = (LegalEntity)this.pm.getObjectById(customer.refGetPath());
                this.pm.refresh(customer);
                result.add(
                    Datatypes.member(
                        CreateCustomerAsLegalEntityResult.Member.customer,
                        this.datatypeMappers.mapCustomer(
                            customer,
                            null,
                            this.getShopCategory()
                        )
                    )
                );
                result.add(
                    Datatypes.member(
                    	CreateCustomerAsLegalEntityResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        CreateCustomerAsLegalEntityResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.DUPLICATE,
                            new String[]{"legalName", params.getLegalName()}
                        )
                    )                            
                );                                                    
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateCustomerAsLegalEntityResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateCustomerAsLegalEntityResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createCustomerContract(org.opencrx.application.shop1.cci2.CreateCustomerContractParams)
     */
    @Override
    public CreateCustomerContractResult createCustomerContract(
        CreateCustomerContractParams params
    ) {
        List<Structures.Member<CreateCustomerContractResult.Member>> result = 
            new ArrayList<Structures.Member<CreateCustomerContractResult.Member>>();
        try {
            String customerNumber = params.getCustomerContract().getCustomerNumber();
            Account customer = this.findAccount(customerNumber);
            if(customer != null) {
	            Lead customerContract = this.findMainCustomerContract(customer);
	            if(customerContract == null) {
                    // Create customer contract
	            	CustomerContractT customerContractT = params.getCustomerContract();
                    customerContract = this.pm.newInstance(Lead.class);
                    customerContract.setCustomer(customer);
                    customerContract.setPricingRule(this.findPricingRule());
                    String contractCategory = this.getShopCategory();
                    if(contractCategory != null) {
                    	customerContract.getCategory().add(contractCategory);
                    }
                    this.datatypeMappers.getLeadFieldMapper().setContractNumber(customerContract, customerNumber);
                    this.datatypeMappers.getLeadFieldMapper().setAcceptedLegal(customerContract, customerContractT.isAcceptedLegal());
                    this.datatypeMappers.getLeadFieldMapper().setAcceptedMarketing(customerContract, customerContractT.isAcceptedMarketing());
                    this.datatypeMappers.getLeadFieldMapper().setAcceptedPrivateDataForwarding(customerContract, customerContractT.isAcceptedPrivateDataForwarding());
                    this.datatypeMappers.getLeadFieldMapper().setReferrer(customerContract, customerContractT.getReferrer());
                    this.datatypeMappers.getLeadFieldMapper().setContactSource(customerContract, customerContractT.getContactSource());
                    this.datatypeMappers.getLeadFieldMapper().setSalesTaxType(customerContract, customerContractT.getSalesTaxType());
                    this.datatypeMappers.getLeadFieldMapper().setContractCurrency(customerContract, customerContractT.getContractCurrency());
                    this.datatypeMappers.getLeadFieldMapper().setNoBilling(customerContract, customerContractT.isNoBilling());
                    this.datatypeMappers.getAbstractContractFieldMapper().setBillingPartner(customerContract, customerContractT.getBillingPartner());
                    this.datatypeMappers.getAbstractContractFieldMapper().setBillingPartnerRegistrationId(customerContract, customerContractT.getBillingPartnerRegistrationId());
                    if(customerContractT.getContractStatus() != null) {
	                    this.setContractStatus(
	                    	customerContract, 
	                    	customerContractT.getContractStatus()
	                    );
                    }
                    customerContract.getOwningGroup().addAll(
                        this.getContractSegment().getOwningGroup()
                    );
                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
                    this.getContractSegment().addLead(
                        this.uuidAsString(), 
                        customerContract
                    );
                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                    customerContract = (Lead)this.pm.getObjectById(customerContract.refGetPath());
                    this.pm.refresh(customerContract);                     	
                    result.add(
                        Datatypes.member(
                            CreateCustomerContractResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NONE,
                                null
                            )
                        )                        
                    );
                    result.add(
                        Datatypes.member(
                            CreateCustomerContractResult.Member.customerContract,
                            this.datatypeMappers.mapCustomerContract(customerContract)
                        )                                                    
                    );                    
	            }
                else {
                    result.add(
                        Datatypes.member(
                            CreateCustomerContractResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.DUPLICATE,
                                new String[]{"CustomerContract", customerNumber}
                            )
                        )                            
                    );                                                
                }                    
            }
            else {
                result.add(
                    Datatypes.member(
                        CreateCustomerContractResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Customer", customerNumber}
                        )                       
                    )                            
                );                    
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateCustomerContractResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateCustomerContractResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createInvoice(org.opencrx.application.shop1.cci2.CreateInvoiceParams)
     */
    @Override
    public CreateInvoiceResult createInvoice(
        CreateInvoiceParams params
    ) {
        List<Structures.Member<CreateInvoiceResult.Member>> result = 
            new ArrayList<Structures.Member<CreateInvoiceResult.Member>>();
        try {
            String customerNumber = params.getInvoice().getContract().getCustomerNumber();
            Account customer = this.findAccount(customerNumber);            
            if(customer != null) {            	
                Lead customerContract = null;
                if(params.getCustomerContractNumber() != null) {
                	customerContract = this.findCustomerContractByContractNumber(params.getCustomerContractNumber());
                }
                else {
                	customerContract = this.findMainCustomerContract(customer);
                	if(customerContract == null) {
                		List<Lead> customerContracts = this.findCustomerContracts(customer);
                		if(customerContracts != null && customerContracts.size() == 1) {
                			customerContract = customerContracts.iterator().next();
                		}                		
                	}
                }
                if(customerContract != null) {
                	String validationResult = this.validateContractPositions(
                		params.getInvoice().getContract().getPosition()
                	);
                	if(validationResult == null) {
	                    String invoiceNumber = this.getNextInvoiceNumber(customer);
	                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
	                    @SuppressWarnings("deprecation")
                        Invoice invoice = Accounts.getInstance().createInvoice(
	                    	customer, 
	                    	invoiceNumber + " /" + customerNumber, // name
	                    	null, // description 
	                    	null // basedOn
	                    );
	                    this.datatypeMappers.mapInvoice(                        
	                        invoice,
	                        customerContract,
	                        customer,
	                        invoiceNumber,
	                        params.getInvoice().isVoucher(),
	                        InvoiceState.DRAFT,
	                        this.findPricingRule()
	                    );
	                    String contractCategory = this.getShopCategory();
	                    if(contractCategory != null) {
	                    	invoice.getCategory().add(contractCategory);
	                    }
	                    invoice.getOwningGroup().addAll(
	                        this.getContractSegment().getOwningGroup()
	                    );
	                    this.addContractAddresses(
	                        invoice, 
	                        params.getInvoice().getContract().getPostalAddressInvoice(), 
	                        params.getInvoice().getContract().getPostalAddressDelivery()
	                    );
	                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	                    this.addContractPositions(
	                        invoice,
	                        params.getInvoice().getContract().getPosition(),
	                        this.datatypeMappers.getLeadFieldMapper().getSalesTaxType(customerContract),
	                        null, // discountIsPercentage
	                        null // discount
	                    );
	                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
	                    invoice.reprice();
	                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;                        
	                    this.pm.refresh(invoice);
	                    result.add(
	                        Datatypes.member(
	                            CreateInvoiceResult.Member.invoice,
	                            this.datatypeMappers.mapInvoice(invoice)
	                        )                            
	                    );
	                    result.add(
	                        Datatypes.member(
	                            CreateInvoiceResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NONE,
	                                null
	                            )
	                        )                            
	                    );
	                }
                	else {
                        result.add(
                            Datatypes.member(
                                CreateInvoiceResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.ASSERTION_FAILURE,
                                    new String[]{"CustomerContract", customerNumber, validationResult}
                                )
                            )                            
                        );                                                                                                                    		
                	}
                }
                else {
                    result.add(
                        Datatypes.member(
                            CreateInvoiceResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"No CustomerContract not found for customer. Check existence of main contract of customer or specify customerContractNumber as input.", customerNumber}
                            )
                        )                            
                    );                                                                                                    
                }
            }
            else {
                result.add(
                    Datatypes.member(
                        CreateInvoiceResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", customerNumber}
                        )
                    )                            
                );                                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateInvoiceResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateInvoiceResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createInvoiceFromInvoice(org.opencrx.application.shop1.cci2.CreateInvoiceFromInvoiceParams)
     */
    @Override
    public CreateInvoiceFromInvoiceResult createInvoiceFromInvoice(
        CreateInvoiceFromInvoiceParams params
    ) {
        List<Structures.Member<CreateInvoiceFromInvoiceResult.Member>> result = 
            new ArrayList<Structures.Member<CreateInvoiceFromInvoiceResult.Member>>();
        try {
            Invoice originalInvoice = this.findInvoice(params.getInvoiceNumber());
            if(originalInvoice != null) {
                Account customer = originalInvoice.getCustomer();                        	
            	String customerNumber = this.datatypeMappers.getAccountFieldMapper().getAccountNumber(customer);
                String newInvoiceNumber = this.getNextInvoiceNumber(originalInvoice.getCustomer());
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                @SuppressWarnings("deprecation")
                Invoice newInvoice = Accounts.getInstance().createInvoice(
                	customer, 
                	newInvoiceNumber + " /" + customerNumber, // name 
                	null, // description 
                	originalInvoice // basedOn
                );
                this.datatypeMappers.mapInvoice(
                    newInvoice, 
                    originalInvoice, 
                    originalInvoice.getCustomer(), 
                    newInvoiceNumber, 
                    Boolean.FALSE, 
                    InvoiceState.DRAFT,
                    originalInvoice.getPricingRule()
                );
                this.datatypeMappers.getAbstractContractFieldMapper().getStateHistory(newInvoice).clear(); // reset state history
                if(params.getExpiresOn() != null) {
                	newInvoice.setExpiresOn(params.getExpiresOn());
                }
                String contractCategory = this.getShopCategory();
                if(contractCategory != null) {
                	newInvoice.getCategory().add(contractCategory);
                }                
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;     
                InvoiceT originalInvoiceT =  this.datatypeMappers.mapInvoice(originalInvoice);
                Lead customerContract = this.findCustomerContract(originalInvoice);                
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                this.addContractAddresses(
                    newInvoice, 
                    originalInvoiceT.getContract().getPostalAddressInvoice(), 
                    originalInvoiceT.getContract().getPostalAddressDelivery()
                );
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                SysLog.detail("Adding positions to newly created invoice", originalInvoiceT.getContract().getPosition().size());
                this.addContractPositions(
                    newInvoice,
                    originalInvoiceT.getContract().getPosition(),
                    this.datatypeMappers.getLeadFieldMapper().getSalesTaxType(customerContract),
                    null, // discountIsPercentage (inherit from original position) 
                    null // discount (inherit from original position)
                );
                this.pm.refresh(newInvoice);
                result.add(
                    Datatypes.member(
                        CreateInvoiceFromInvoiceResult.Member.invoice,
                        this.datatypeMappers.mapInvoice(newInvoice)
                    )                            
                );
                result.add(
                    Datatypes.member(
                        CreateInvoiceFromInvoiceResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        CreateInvoiceFromInvoiceResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Invoice", params.getInvoiceNumber()}
                        )
                    )                            
                );                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateInvoiceFromInvoiceResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateInvoiceFromInvoiceResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createInvoiceFromSalesOrder(org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderParams)
     */
    @Override
    public CreateInvoiceFromSalesOrderResult createInvoiceFromSalesOrder(
        CreateInvoiceFromSalesOrderParams params
    ) {
        List<Structures.Member<CreateInvoiceFromSalesOrderResult.Member>> result = 
            new ArrayList<Structures.Member<CreateInvoiceFromSalesOrderResult.Member>>();
        try {
            SalesOrder salesOrder = this.findSalesOrder(params.getSalesOrderNumber());
            if(salesOrder != null) {
                String invoiceNumber = this.getNextInvoiceNumber(salesOrder.getCustomer());
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                SalesOrderCreateInvoiceResult createInvoiceResult = salesOrder.createInvoice();
                this.pm.flush();
                Invoice invoice = (Invoice)this.pm.getObjectById(createInvoiceResult.getInvoice().refGetPath());
                this.datatypeMappers.mapInvoice(
                    invoice,
                    salesOrder,
                    salesOrder.getCustomer(),
                    invoiceNumber,
                    Boolean.FALSE,
                    InvoiceState.DRAFT,
                    salesOrder.getPricingRule()
                );
                this.datatypeMappers.getAbstractContractFieldMapper().getStateHistory(invoice).clear(); // reset state history
                String contractCategory = this.getShopCategory();
                if(contractCategory != null) {
                	invoice.getCategory().add(contractCategory);
                }
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                this.pm.refresh(invoice);
                result.add(
                    Datatypes.member(
                        CreateInvoiceFromSalesOrderResult.Member.invoice,
                        this.datatypeMappers.mapInvoice(invoice)
                    )                            
                );
                result.add(
                    Datatypes.member(
                        CreateInvoiceFromSalesOrderResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        CreateInvoiceFromSalesOrderResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"SalesOrder", params.getSalesOrderNumber()}
                        )
                    )                            
                );                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateInvoiceFromSalesOrderResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateInvoiceFromSalesOrderResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createProductClassification(org.opencrx.application.shop1.cci2.CreateProductClassificationParams)
     */
    @Override
    public CreateProductClassificationResult createProductClassification(
        CreateProductClassificationParams params
    ) {
        List<Structures.Member<CreateProductClassificationResult.Member>> result = 
            new ArrayList<Structures.Member<CreateProductClassificationResult.Member>>();
        try {
            boolean hasErrors = false;
            for(ProductClassificationT classificationT: params.getClassification()) {
                ProductClassification classification = this.findProductClassification(classificationT.getClassificationId());
                if(classification == null) {
                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
                    classification = pm.newInstance(ProductClassification.class);
                    classification.setName(classificationT.getClassificationId());
                    classification.setDescription(classificationT.getDescription());
                    classification.getOwningGroup().addAll(
                        this.getProductSegment().getOwningGroup()
                    );
                    this.getProductSegment().addProductClassification(
                        this.uuidAsString(), 
                        classification
                    );
                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;                    
                }
                else {
                    result.add(
                        Datatypes.member(
                            CreateProductClassificationResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.DUPLICATE,
                                new String[]{"ProductClassification", classificationT.getClassificationId()}
                            )
                        )                            
                    );                                    
                    hasErrors = true;
                    break;
                }
            }
            if(!hasErrors) {
                result.add(
                    Datatypes.member(
                        CreateProductClassificationResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateProductClassificationResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateProductClassificationResult.class,
            result
        );
    }
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createProducts(org.opencrx.application.shop1.cci2.CreateProductsParams)
     */
    @Override
    public CreateProductsResult createProducts(
        CreateProductsParams params
    ) {
        List<Structures.Member<CreateProductsResult.Member>> result = 
            new ArrayList<Structures.Member<CreateProductsResult.Member>>();
        try {
            boolean hasErrors = false;
            Date creationStartedAt = new Date();
            for(ProductT productT: params.getProduct()) {
                Product product = this.findProduct(productT.getProductNumber());
                if(product == null) {
                     product = pm.newInstance(Product.class);
                     // Product classification
                     List<ProductClassification> productClassifications = new ArrayList<ProductClassification>();
                     for(String classificationId: productT.getClassificationId()) {
                         ProductClassification classification = this.findProductClassification(classificationId);
                         if(classification != null) {
                             productClassifications.add(classification);
                         }
                         else {
                             result.add(
                                 Datatypes.member(
                                     CreateProductsResult.Member.status,
                                     this.datatypeMappers.mapOperationStatus(
                                         BasicException.Code.NOT_FOUND,
                                         new String[]{"Product", productT.getProductNumber(), "ProductClassification", classificationId}
                                     )
                                 )                            
                             );                                    
                             hasErrors = true;
                             break;                                                 
                         }
                     }
                     if(hasErrors) break;
                     // Price uoms
                     List<Uom> priceUoms = new ArrayList<Uom>();
                     for(String priceUomName: productT.getPriceUom()) {
                         Uom priceUom = this.findUom(priceUomName);
                         if(priceUom != null) {
                             priceUoms.add(priceUom);
                         }
                         else {
                             result.add(
                                 Datatypes.member(
                                     CreateProductsResult.Member.status,
                                     this.datatypeMappers.mapOperationStatus(
                                         BasicException.Code.NOT_FOUND,
                                         new String[]{"Product", productT.getProductNumber(), "PriceUom", priceUomName}
                                     )
                                 )                            
                             );                                    
                             hasErrors = true;
                             break;                                                                              
                         }
                     }
                     if(hasErrors) break;                     
                     this.datatypeMappers.mapProduct(
                         productT,
                         product
                     );
                     product.getClassification().addAll(productClassifications);
                     product.getPriceUom().addAll(priceUoms);
                     product.getOwningGroup().addAll(
                         this.getProductSegment().getOwningGroup()
                     );
                     PersistenceHelper.currentUnitOfWork(this.pm).begin();
                     this.getProductSegment().addProduct(
                         productT.getProductNumber(), 
                         product
                     );
                     PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                     // Configuration type
                     ProductConfigurationTypeSet configurationType = null;
                     if(productT.getConfigurationType() != null) {
	                     configurationType = this.findProductConfigurationType(productT.getConfigurationType());
	                     if(configurationType == null) {
	                         result.add(
	                             Datatypes.member(
	                                 CreateProductsResult.Member.status,
	                                 this.datatypeMappers.mapOperationStatus(
	                                     BasicException.Code.NOT_FOUND,
	                                     new String[]{"ProductConfigurationType", productT.getConfigurationType()}
	                                 )
	                             )                            
	                         );                                    
	                         hasErrors = true;
	                         break;                                                                          
	                     }
                     }
                     PersistenceHelper.currentUnitOfWork(this.pm).begin();
                     if(configurationType != null) {
	                     this.updateProductConfiguration(
	                         productT, 
	                         product, 
	                         configurationType
	                     );
                     }
                     this.updateProductDescriptions(
                         productT,
                         product
                     );
                     this.updateProductPicture(
                         productT,
                         product
                     );
                     this.updateProductPhase(
                         productT,
                         product
                     );
                     this.updateProductBundle(
                         productT,
                         product
                     );
                     PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                     result.add(
                         Datatypes.member(
                             CreateProductsResult.Member.status,
                             this.datatypeMappers.mapOperationStatus(
                                 BasicException.Code.NONE,
                                 null
                             )
                         )                            
                     );                                     
                }
                else {
                    result.add(
                        Datatypes.member(
                            CreateProductsResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.DUPLICATE,
                                new String[]{"Product", productT.getProductNumber()}
                            )
                        )                            
                    );                                    
                    hasErrors = true;
                    break;                    
                }                    
            }
            // Create initial prices for the newly created products
            Collection<AbstractPriceLevel> priceLevels = this.getProductSegment().getPriceLevel();
            for(AbstractPriceLevel priceLevel: priceLevels) {
                if(priceLevel.isFinal() == null || !priceLevel.isFinal()) {
                    // The price level must have a PriceUomFilter. Initial prices are
                    // calculated for this priceUom. If no priceUom is found the prices
                    // must be calculated manually.
                    Uom priceUom = null;
                    Collection<ProductFilterProperty> filterProperties = priceLevel.getProductFilterProperty();
                    for(ProductFilterProperty filterProperty: filterProperties) {
                        if(filterProperty instanceof PriceUomFilterProperty) {
                            Collection<Uom> filteredPriceUoms = ((PriceUomFilterProperty)filterProperty).getPriceUom();
                            if(!filteredPriceUoms.isEmpty()) {
                                priceUom = filteredPriceUoms.iterator().next();
                                break;
                            }
                        }
                    }
                    if(priceUom != null) {
                        PersistenceHelper.currentUnitOfWork(this.pm).begin();
                        CreateInitialPricesParams createInitialPricesParams = Structures.create(
                        	CreateInitialPricesParams.class, 
                        	Datatypes.member(CreateInitialPricesParams.Member.includeProductsModifiedSince, creationStartedAt),
                        	Datatypes.member(CreateInitialPricesParams.Member.priceUom, priceUom),
                        	Datatypes.member(CreateInitialPricesParams.Member.processingMode, Products.PROCESSING_MODE_PROCESS)
                        );
                        priceLevel.createInitialPrices(createInitialPricesParams);
                        PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                    }
                }
            }
            if(!hasErrors) {
                result.add(
                    Datatypes.member(
                        CreateProductsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateProductsResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateProductsResult.class,
            result
        );
    }
          
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#createSalesOrder(org.opencrx.application.shop1.cci2.CreateSalesOrderParams)
     */
    @Override
    public CreateSalesOrderResult createSalesOrder(
        CreateSalesOrderParams params
    ) {
        List<Structures.Member<CreateSalesOrderResult.Member>> result = 
            new ArrayList<Structures.Member<CreateSalesOrderResult.Member>>();
        try {
            String customerNumber = params.getSalesOrder().getContract().getCustomerNumber();
            Account customer = this.findAccount(customerNumber);            
            if(customer != null) {
                Lead customerContract = null;
                if(params.getCustomerContractNumber() != null) {
                	customerContract = this.findCustomerContractByContractNumber(params.getCustomerContractNumber());
                }
                else {
                	customerContract = this.findMainCustomerContract(customer);
                	if(customerContract == null) {
                		List<Lead> customerContracts = this.findCustomerContracts(customer);
                		if(customerContracts != null && customerContracts.size() == 1) {
                			customerContract = customerContracts.iterator().next();                			
                		}
                	}
                }
                if(customerContract != null) {
                	String validationResult = this.validateContractPositions(
                		params.getSalesOrder().getContract().getPosition()
                	);
                	if(validationResult == null) {
	                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
	                    String contractNumber = params.getSalesOrder().getContract().getContractNumber();
	                    String salesOrderNumber = contractNumber == null || contractNumber.isEmpty() ?
	                    	this.getNextSalesOrderNumber(customer) :
	                    		contractNumber;
	                    SalesOrder salesOrder = pm.newInstance(SalesOrder.class);
	                    this.getContractSegment().addSalesOrder(
	                        this.uuidAsString(),
	                        salesOrder
	                    );
	                    ContractT contractT = params.getSalesOrder().getContract();
	                    this.datatypeMappers.mapSalesOrder(
	                        salesOrder, 
	                        customerContract, 
	                        customer, 
	                        salesOrderNumber, 
	                        SalesOrderState.DRAFT,	                        
	                        contractT.getContractCurrency(),
	                        contractT.getActiveOn(),
	                        contractT.getExpiresOn(),
	                        contractT.getCancelOn(),
	                        contractT.getClosedOn(),
	                        contractT.isGift(),
	                        contractT.getGiftMessage(),
	                        this.findPricingRule()
	                    );
	                    salesOrder.getOwningGroup().addAll(
	                        this.getContractSegment().getOwningGroup()
	                    );
	                    String contractCategory = this.getShopCategory();
	                    if(contractCategory != null) {
	                    	salesOrder.getCategory().add(contractCategory);
	                    }	                    
	                    this.addContractAddresses(
	                        salesOrder,
	                        params.getSalesOrder().getContract().getPostalAddressDelivery(), 
	                        params.getSalesOrder().getContract().getPostalAddressInvoice() 
	                    );
	                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	                    this.pm.refresh(salesOrder);
	                    this.addContractPositions(
	                        salesOrder,
	                        params.getSalesOrder().getContract().getPosition(), 
	                        this.datatypeMappers.getLeadFieldMapper().getSalesTaxType(customerContract),
	                        null, // discountIsPercentage
	                        null // discount
	                    );
	                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
	                    salesOrder.reprice();
	                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;                        
	                    this.pm.refresh(salesOrder);
	                    result.add(
	                        Datatypes.member(
	                            CreateSalesOrderResult.Member.salesOrder,
	                            this.datatypeMappers.mapSalesOrder(salesOrder)
	                        )                            
	                    );
	                    result.add(
	                        Datatypes.member(
	                            CreateSalesOrderResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NONE,
	                                null
	                            )
	                        )                            
	                    );
	                }
                	else {
                        result.add(
                            Datatypes.member(
                                CreateSalesOrderResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.ASSERTION_FAILURE,
                                    new String[]{"CustomerContract", customerNumber, validationResult}
                                )
                            )                        
                        );                                                                                                                    		
                	}
                }
                else {
                    result.add(
                        Datatypes.member(
                            CreateSalesOrderResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"No CustomerContract not found for customer. Check existence of main contract of customer or specify customerContractNumber as input.", customerNumber}
                            )
                        )                            
                    );                                                                                                    
                }
            }
            else {
                result.add(
                    Datatypes.member(
                        CreateSalesOrderResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", customerNumber}
                        )
                    )                            
                );                                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    CreateSalesOrderResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            CreateSalesOrderResult.class,
            result
        );
    }
        
	//-----------------------------------------------------------------------
	/* (non-Javadoc)
	 * @see org.opencrx.application.shop1.cci2.ShopService#getActivitiesByQuery(org.opencrx.application.shop1.cci2.GetActivitiesByQueryParams)
	 */
    @Override
	public GetActivitiesByQueryResult getActivitiesByQuery(
		GetActivitiesByQueryParams params
	) {
        List<Structures.Member<GetActivitiesByQueryResult.Member>> result = 
            new ArrayList<Structures.Member<GetActivitiesByQueryResult.Member>>();
        try {
        	String customerNumber = params.getCustomerNumber();
        	Account customer = this.findAccount(customerNumber);
        	if(customer != null) {
        		ActivityQuery activityQuery = (ActivityQuery)this.pm.newQuery(Activity.class);
        		if(params.getPercentCompleteThreshold() != null) {
        			activityQuery.thereExistsPercentComplete().greaterThanOrEqualTo(
        				params.getPercentCompleteThreshold().shortValue()
        			);
        		}
        		activityQuery.orderByActivityNumber().descending();
        		List<Activity> activities = customer.getAssignedActivity(activityQuery);
        		List<ActivityT> activitiesT = new ArrayList<ActivityT>();
        		for(Activity activity: activities) {
        			activitiesT.add(
        				this.datatypeMappers.mapActivity(activity)
        			);
        		}
                result.add(
                    Datatypes.member(
                    	GetActivitiesByQueryResult.Member.activity,
                    	activitiesT
                    )                            
                );                                
                result.add(
                    Datatypes.member(
                    	GetActivitiesByQueryResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );        		
        	}
    		else {
                result.add(
                    Datatypes.member(
                    	GetActivitiesByQueryResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Customer", customerNumber}
                        )
                    )                            
                );                                                                        		        			
    		}
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	GetActivitiesByQueryResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	GetActivitiesByQueryResult.class,
            result
        );
    }

	//-----------------------------------------------------------------------
	/* (non-Javadoc)
	 * @see org.opencrx.application.shop1.cci2.ShopService#getActivity(org.opencrx.application.shop1.cci2.GetActivityParams)
	 */
    @Override
	public GetActivityResult getActivity(
		GetActivityParams params
	) {
        List<Structures.Member<GetActivityResult.Member>> result = 
            new ArrayList<Structures.Member<GetActivityResult.Member>>();
        try {
        	String activityNumber = params.getActivityNumber();
        	Activity activity = this.findActivity(
        		activityNumber, 
        		this.getActivitySegment()
        	);        	
        	if(activity != null) {
                result.add(
                    Datatypes.member(
                    	GetActivityResult.Member.activity,
                    	this.datatypeMappers.mapActivity(activity)
                    )                            
                );                                
                result.add(
                    Datatypes.member(
                    	GetActivityResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );        		
        	}
    		else {
                result.add(
                    Datatypes.member(
                    	GetActivityResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Activity", activityNumber}
                        )
                    )                            
                );                                                                        		        			
    		}
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	GetActivityResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	GetActivityResult.class,
            result
        );
    }

    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getCodeValueContainer(org.opencrx.application.shop1.cci2.GetCodeValueContainerParams)
     */
    @Override
    public GetCodeValueContainerResult getCodeValueContainer(
        GetCodeValueContainerParams params
    ) {
        List<Structures.Member<GetCodeValueContainerResult.Member>> result = 
            new ArrayList<Structures.Member<GetCodeValueContainerResult.Member>>();
        try {
            List<CodeValueContainerT> containersT = new ArrayList<CodeValueContainerT>();
            boolean hasErrors = false;
            for(String name: params.getContainerName()) {
                CodeValueContainer container = this.findCodeValueContainer(
                    name, 
                    params.isRootContainers()
                );
                if(container != null) {
                    List<CodeValueEntryT> entriesT = new ArrayList<CodeValueEntryT>();
                    Collection<CodeValueEntry> entries = container.getEntry();
                    for(AbstractEntry entry: entries) {
                        if(entry instanceof CodeValueEntry) {
                            CodeValueEntry codeValueEntry = (CodeValueEntry)entry;
                            entriesT.add(
                                Datatypes.create(
                                    CodeValueEntryT.class, 
                                    Datatypes.member(
                                        CodeValueEntryT.Member.code, 
                                        codeValueEntry.getEntryValue() == null ? 
                                        	codeValueEntry.refGetPath().getBase() : 
                                        		codeValueEntry.getEntryValue()
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.shortText, 
                                        codeValueEntry.getShortText() == null ? 
                                        	Collections.emptyList() : 
                                        		this.toNonNullElementsCollection(codeValueEntry.getShortText(), new ArrayList<Object>(), "")
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.longText, 
                                        codeValueEntry.getLongText() == null ? 
                                        	Collections.emptyList() : 
                                        		this.toNonNullElementsCollection(codeValueEntry.getLongText(), new ArrayList<Object>(), "")
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.validFrom, 
                                        codeValueEntry.getValidFrom()
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.validTo, 
                                        codeValueEntry.getValidTo()
                                    )
                                )
                            );                     
                        }
                        else if(entry instanceof SimpleEntry) {
                            SimpleEntry simpleEntry = (SimpleEntry)entry;
                            entriesT.add(
                                Datatypes.create(
                                    CodeValueEntryT.class, 
                                    Datatypes.member(
                                        CodeValueEntryT.Member.code, 
                                        simpleEntry.getEntryValue() == null ? 
                                        	simpleEntry.refGetPath().getBase() : 
                                        		simpleEntry.getEntryValue()
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.shortText, 
                                        Collections.emptyList()
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.longText, 
                                        Collections.emptyList()
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.validFrom, 
                                        simpleEntry.getValidFrom()
                                    ),
                                    Datatypes.member(
                                        CodeValueEntryT.Member.validTo, 
                                        simpleEntry.getValidTo()
                                    )
                                )
                            );                            
                        }
                    }
                    containersT.add(
                        Datatypes.create(
                            CodeValueContainerT.class, 
                            Datatypes.member(
                                CodeValueContainerT.Member.name, 
                                name
                            ),
                            Datatypes.member(
                                CodeValueContainerT.Member.entry, 
                                entriesT
                            )
                        )
                    );
                }
                else {
                    result.add(
                        Datatypes.member(
                            GetCodeValueContainerResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"CodeValueContainer", name}
                            )                       
                        )                            
                    );
                    hasErrors = true;
                    break;
                }
            }
            if(!hasErrors) {
                result.add(
                    Datatypes.member(
                        GetCodeValueContainerResult.Member.container,
                        containersT
                    )                            
                );
                result.add(
                    Datatypes.member(
                        GetCodeValueContainerResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )                       
                    )                            
                );
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetCodeValueContainerResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetCodeValueContainerResult.class,
            result
        );
    }
        
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getCredentials(org.opencrx.application.shop1.cci2.GetCredentialsParams)
     */
    @Override
    public GetCredentialsResult getCredentials(
        GetCredentialsParams params
    ) {
        List<Structures.Member<GetCredentialsResult.Member>> result = 
            new ArrayList<Structures.Member<GetCredentialsResult.Member>>();
        try {
            AccountQuery query = (AccountQuery)this.pm.newQuery(Account.class);
            query.thereExistsUserString0().equalTo(params.getUserName());
            Collection<Account> accounts = this.getAccountSegment().getAccount(query);
            if(!accounts.isEmpty()) {
                Account customer = accounts.iterator().next();
                result.add(
                    Datatypes.member(
                        GetCredentialsResult.Member.customerNumber,
                        customer.getAliasName()
                    )
                );
                result.add(
                    Datatypes.member(
                        GetCredentialsResult.Member.credentials,
                        this.datatypeMappers.mapCredentials(customer)
                    )
                );
                result.add(
                    Datatypes.member(
                        GetCredentialsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                
            }
            else {
                result.add(
                    Datatypes.member(
                        GetCredentialsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getUserName()}
                        )
                    )                            
                );                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetCredentialsResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetCredentialsResult.class,
            result
        );
    }
                
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getCredentials(org.opencrx.application.shop1.cci2.GetCredentialsParams)
     */
    @Override
    public GetCredentialsByEmailAddressResult getCredentialsByEmailAddress(
        GetCredentialsByEmailAddressParams params
    ) {
        List<Structures.Member<GetCredentialsByEmailAddressResult.Member>> result = 
            new ArrayList<Structures.Member<GetCredentialsByEmailAddressResult.Member>>();
        try {
            // Query by email address
            if(params.getEmailAddress() != null && params.getEmailAddress().length() > 0) {            	
                EMailAddressQuery addressQuery = (EMailAddressQuery)this.pm.newQuery(EMailAddress.class);
                addressQuery.thereExistsEmailAddress().equalTo(params.getEmailAddress());
                List<AccountAddress> addresses = this.getAccountSegment().getAddress(addressQuery);
                List<Account> accounts = new ArrayList<Account>();
                List<String> customerNumbers = new ArrayList<String>();
                for(AccountAddress address: addresses) {
                    Account account = (Account)this.pm.getObjectById(address.refGetPath().getParent().getParent());
                    if(!accounts.contains(account)) {
                    	accounts.add(account);
                    	customerNumbers.add(
                    		this.datatypeMappers.getAccountFieldMapper().getAccountNumber(account)
                    	);
                    }
                }
                if(!accounts.isEmpty()) {
                	if(accounts.size() == 1) {
		                Account customer = accounts.iterator().next();
		                result.add(
		                    Datatypes.member(
		                    	GetCredentialsByEmailAddressResult.Member.customerNumber,
		                        customer.getAliasName()
		                    )
		                );
		                result.add(
		                    Datatypes.member(
		                    	GetCredentialsByEmailAddressResult.Member.credentials,
		                        this.datatypeMappers.mapCredentials(customer)
		                    )
		                );
		                result.add(
		                    Datatypes.member(
		                    	GetCredentialsByEmailAddressResult.Member.status,
		                        this.datatypeMappers.mapOperationStatus(
		                            BasicException.Code.NONE,
		                            null
		                        )
		                    )                            
		                );
                	}
                	else {
    	                result.add(
    	                    Datatypes.member(
    	                    	GetCredentialsByEmailAddressResult.Member.status,
    	                        this.datatypeMappers.mapOperationStatus(
    	                            BasicException.Code.ASSERTION_FAILURE,
    	                            new String[]{"Email address is not unique. Multiple customers found.", params.getEmailAddress(), customerNumbers.toString()}
    	                        )
    	                    )                            
    	                );                		
                	}
	            }
	            else {
	                result.add(
	                    Datatypes.member(
	                    	GetCredentialsByEmailAddressResult.Member.status,
	                        this.datatypeMappers.mapOperationStatus(
	                            BasicException.Code.NOT_FOUND,
	                            new String[]{"Account", params.getEmailAddress()}
	                        )
	                    )                            
	                );                
	            }
            }
            else {
                result.add(
                    Datatypes.member(
                    	GetCredentialsByEmailAddressResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.ASSERTION_FAILURE,
                            new String[]{"emailAddress is null or empty"}
                        )
                    )                            
                );            	
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	GetCredentialsByEmailAddressResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	GetCredentialsByEmailAddressResult.class,
            result
        );
    }
                
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getCustomer(org.opencrx.application.shop1.cci2.GetCustomerParams)
     */
    @Override
    public GetCustomerResult getCustomer(
        GetCustomerParams params
    ) {
        List<Structures.Member<GetCustomerResult.Member>> result = 
            new ArrayList<Structures.Member<GetCustomerResult.Member>>();
        try {
            Account customer = this.findAccount(params.getCustomerNumber());
            if(customer != null) {
                List<Lead> customerContracts = this.findCustomerContracts(customer);                        
                result.add(
                    Datatypes.member(
                        GetCustomerResult.Member.customer,
                        this.datatypeMappers.mapCustomer(
                            customer,
                            customerContracts,
                            this.getShopCategory()
                        )
                    )
                );
                result.add(
                    Datatypes.member(
                        GetCustomerResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        GetCustomerResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getCustomerNumber()}
                        )
                    )                            
                );                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetCustomerResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetCustomerResult.class,
            result
        );
    }
                     
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getCustomersByQuery(org.opencrx.application.shop1.cci2.GetCustomersByQueryParams)
     */
    @Override
    public GetCustomersByQueryResult getCustomersByQuery(
        GetCustomersByQueryParams params
    ) {
        List<Structures.Member<GetCustomersByQueryResult.Member>> result = 
            new ArrayList<Structures.Member<GetCustomersByQueryResult.Member>>();
        try {
            List<String> customerNumbers = new ArrayList<String>();
            if(params.getLegalName() != null) {
            	LegalEntityQuery legalEntityQuery = (LegalEntityQuery)pm.newQuery(LegalEntity.class);
            	legalEntityQuery.name().equalTo(params.getLegalName());
            	List<LegalEntity> legalEntities = this.getAccountSegment().getAccount(legalEntityQuery);
            	for(Account legalEntity: legalEntities) {
            		String customerNumber = this.datatypeMappers.getAccountFieldMapper().getAccountNumber(legalEntity);
                    if(customerNumber == null) {
                    	SysLog.warning("Customer number is null. Skipping customer", legalEntity.refGetPath());
                    } else {
	            		customerNumbers.add(customerNumber);
            		}
            	}
            }
            // Query by email address
            else if(params.getEmailAddress() != null) {
                EMailAddressQuery addressQuery = (EMailAddressQuery)this.pm.newQuery(EMailAddress.class);
                addressQuery.thereExistsEmailAddress().equalTo(params.getEmailAddress());
                List<AccountAddress> addresses = this.getAccountSegment().getAddress(addressQuery);
                for(AccountAddress address: addresses) {
                    Account account = (Account)this.pm.getObjectById(address.refGetPath().getParent().getParent());
                    String customerNumber = this.datatypeMappers.getAccountFieldMapper().getAccountNumber(account);
                    if(customerNumber == null) {
                    	SysLog.warning("Customer number is null. Skipping customer", account.refGetPath());
                    }
                    else if(!customerNumbers.contains(customerNumber)) {
                        customerNumbers.add(customerNumber);
                    }
                }
            }
            result.add(
                Datatypes.member(
                    GetCustomersByQueryResult.Member.customerNumber,
                    customerNumbers
                )
            );
            result.add(
                Datatypes.member(
                    GetCustomersByQueryResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(
                        BasicException.Code.NONE,
                        null
                    )
                )                            
            );
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetCustomersByQueryResult.Member.customerNumber,
                    Collections.emptyList()
                )
            );
            result.add(
                Datatypes.member(
                    GetCustomersByQueryResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetCustomersByQueryResult.class,
            result
        );
    }
                     
    //-----------------------------------------------------------------------
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getDocuments(org.opencrx.application.shop1.cci2.GetDocumentsParams)
     */
    @Override
    public GetDocumentsResult getDocuments(
    	GetDocumentsParams params
    ) {
        List<Structures.Member<GetDocumentsResult.Member>> result = 
            new ArrayList<Structures.Member<GetDocumentsResult.Member>>();
    	try {
    		String folderName = params.getFolderName();
    		if(folderName != null && folderName.length() > 0) {
    			DocumentFolderQuery documentFolderQuery = (DocumentFolderQuery)this.pm.newQuery(DocumentFolder.class);
    			documentFolderQuery.name().equalTo(folderName);
    			List<DocumentFolder> documentFolders = this.getDocumentSegment().getFolder(documentFolderQuery);
    			if(!documentFolders.isEmpty()) {
    				DocumentFolder documentFolder = documentFolders.iterator().next();
    				DocumentQuery documentQuery = (DocumentQuery)this.pm.newQuery(Document.class);
    				documentQuery.thereExistsFolder().equalTo(documentFolder);
    				List<Document> documents = this.getDocumentSegment().getDocument(documentQuery);
    				List<DocumentT> documentsT = new ArrayList<DocumentT>();
    				for(Document document: documents) {
						documentsT.add(
							this.datatypeMappers.mapDocument(document)
						);
    				}
                    result.add(
                        Datatypes.member(
                        	GetDocumentsResult.Member.document,
                        	documentsT
                        )
                    );
                    result.add(
                        Datatypes.member(
                        	GetDocumentsResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NONE,
                                null
                            )
                        )
                    );
    			}
    			else {
                    result.add(
                        Datatypes.member(
                        	GetDocumentsResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"DocumentFolder", folderName}
                            )
                        )                            
                    );                    				
    			}
    		}
    		else {
                result.add(
                    Datatypes.member(
                    	GetDocumentsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Folder name is null or empty", folderName}
                        )
                    )                            
                );                    				
			}    			
    	}
	    catch(Exception e) {
	        new ServiceException(e).log();
	        result.add(
	            Datatypes.member(
	            	GetDocumentsResult.Member.status,
	                this.datatypeMappers.mapOperationStatus(e)
	            )                            
	        );
	    }
	    return Structures.create(
	    	GetDocumentsResult.class,
	        result
	    );    	
    }
    
    //-----------------------------------------------------------------------
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getInvoice(org.opencrx.application.shop1.cci2.GetInvoiceParams)
     */
    @Override
    public GetInvoiceResult getInvoice(
        GetInvoiceParams params
    ) {
        List<Structures.Member<GetInvoiceResult.Member>> result = 
            new ArrayList<Structures.Member<GetInvoiceResult.Member>>();
        try {
            Invoice invoice = this.findInvoice(params.getInvoiceNumber());
            if(invoice != null) {
                result.add(
                    Datatypes.member(
                        GetInvoiceResult.Member.invoice,
                        this.datatypeMappers.mapInvoice(invoice)
                    )                            
                );
                result.add(
                    Datatypes.member(
                        GetInvoiceResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                
            }
            else {
                result.add(
                    Datatypes.member(
                        GetInvoiceResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Invoice", params.getInvoiceNumber()}
                        )
                    )                            
                );                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetInvoiceResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetInvoiceResult.class,
            result
        );
    }
     
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getInvoicePositions(org.opencrx.application.shop1.cci2.GetInvoicePositionsParams)
     */
    @Override
    public GetInvoicePositionsResult getInvoicePositions(
        GetInvoicePositionsParams params
    ) {
        List<Structures.Member<GetInvoicePositionsResult.Member>> result = 
            new ArrayList<Structures.Member<GetInvoicePositionsResult.Member>>();
        try {
            Account customer = this.findAccount(params.getCustomerNumber());
            if(customer != null) {
                Collection<ContractPositionT> positions = 
                    this.getInvoicePositions(
                        customer, 
                        params.getInvoiceStatusThreshold(),
                        true,
                        null
                    );
                result.add(
                    Datatypes.member(
                        GetInvoicePositionsResult.Member.position,
                        positions
                    )
                );          
                result.add(
                    Datatypes.member(
                        GetInvoicePositionsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        GetInvoicePositionsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getCustomerNumber()}
                        )
                    )                            
                );                                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetInvoicePositionsResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetInvoicePositionsResult.class,
            result
        );
    }
                                
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getInvoices(org.opencrx.application.shop1.cci2.GetInvoicesParams)
     */
    @Override
    public GetInvoicesResult getInvoices(
        GetInvoicesParams params
    ) {
        List<Structures.Member<GetInvoicesResult.Member>> result = 
            new ArrayList<Structures.Member<GetInvoicesResult.Member>>();
        try {
            Account customer = this.findAccount(params.getCustomerNumber());
            if(customer != null) {
                InvoiceQuery invoiceQuery = (InvoiceQuery)this.pm.newQuery(Invoice.class);
                invoiceQuery.thereExistsCustomer().equalTo(customer);
                String contractCategory = this.getShopCategory();
                if(contractCategory != null) {
                	invoiceQuery.thereExistsCategory().equalTo(contractCategory);
                }
                List<InvoiceT> invoicesT = new ArrayList<InvoiceT>();
                for(Invoice invoice: this.getContractSegment().getInvoice(invoiceQuery)) {
                    invoicesT.add(
                    	 this.datatypeMappers.mapInvoice(invoice)
                    );
                }
                result.add(
                    Datatypes.member(
                        GetInvoicesResult.Member.invoice,
                        invoicesT
                    )
                );          
                result.add(
                    Datatypes.member(
                        GetInvoicesResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                
            }
            else {
                result.add(
                    Datatypes.member(
                        GetInvoicesResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getCustomerNumber()}
                        )
                    )                            
                );                                
            }            
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetInvoicesResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetInvoicesResult.class,
            result
        );
    }
                           
    //-----------------------------------------------------------------------
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getPriceLevel(org.opencrx.application.shop1.cci2.GetPriceLevelParams)
     */
    @Override
    public GetPriceLevelResult getPriceLevel(
        GetPriceLevelParams params
    ) {
        List<Structures.Member<GetPriceLevelResult.Member>> result = 
            new ArrayList<Structures.Member<GetPriceLevelResult.Member>>();
        try {
            AbstractPriceLevelQuery query = (AbstractPriceLevelQuery)this.pm.newQuery(AbstractPriceLevel.class);
            query.name().equalTo(params.getPriceLevel());
            Collection<AbstractPriceLevel> priceLevels = this.getProductSegment().getPriceLevel(query);
            if(!priceLevels.isEmpty()) {
                AbstractPriceLevel priceLevel = priceLevels.iterator().next();
                result.add(
                    Datatypes.member(
                        GetPriceLevelResult.Member.priceLevel,
                        this.datatypeMappers.mapPriceLevel(priceLevel)
                    )
                );
                result.add(
                    Datatypes.member(
                        GetPriceLevelResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        GetPriceLevelResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"PriceLevel", params.getPriceLevel()}
                        )
                    )                            
                );                                                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetPriceLevelResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }        
        return Structures.create(
            GetPriceLevelResult.class,
            result
        );
    }
        
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getProducts(org.opencrx.application.shop1.cci2.GetProductsParams)
     */
    @Override
    public GetProductsResult getProducts(
        GetProductsParams params
    ) {
        List<Structures.Member<GetProductsResult.Member>> result = 
            new ArrayList<Structures.Member<GetProductsResult.Member>>();
        try {
            boolean hasErrors = false;
            List<ProductT> productsT = new ArrayList<ProductT>();
            for(String productNumber: params.getProductNumber()) {
                Product product = this.findProduct(productNumber);
                if(product != null) {
                    productsT.add(
                    	 this.datatypeMappers.mapProduct(
                            product,
                            params.isReturnPictureContent(),
                            this.getProductSegment()
                        )
                    );
                }
                else {
                    result.add(
                        Datatypes.member(
                            GetProductsResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"Product", productNumber}
                            )
                        )                            
                    );
                    hasErrors = true;
                    break;
                }
            }
            if(!hasErrors) {
                result.add(
                    Datatypes.member(
                        GetProductsResult.Member.product,
                        productsT
                    )                            
                );
                result.add(
                    Datatypes.member(
                        GetProductsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )                       
                    )                            
                );                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetProductsResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetProductsResult.class,
            result
        );
    }
                                        
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getProductsByQuery(org.opencrx.application.shop1.cci2.GetProductsByQueryParams)
     */
    @Override
    public GetProductsByQueryResult getProductsByQuery(
        GetProductsByQueryParams params
    ) {
        List<Structures.Member<GetProductsByQueryResult.Member>> result = 
            new ArrayList<Structures.Member<GetProductsByQueryResult.Member>>();
        try {
            List<ProductClassification> productClassifications = new ArrayList<ProductClassification>();
            for(String classificationId: params.getClassificationId()) {
                productClassifications.add(
                    this.findProductClassification(classificationId)
                );
            }
            ProductQuery query = (ProductQuery)this.pm.newQuery(Product.class);
            query.thereExistsClassification().elementOf(productClassifications);
            if(params.getProductStatus() != null) {
                query.productState().equalTo(params.getProductStatus());
            }
            List<String> productNumbers = new ArrayList<String>();
            for(Product product: this.getProductSegment().getProduct(query)) {
                productNumbers.add(
                    product.getProductNumber()
                );
            }
            result.add(
                Datatypes.member(
                    GetProductsByQueryResult.Member.productNumber,
                    productNumbers
                )                            
            );
            result.add(
                Datatypes.member(
                    GetProductsByQueryResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(
                        BasicException.Code.NONE,
                        null
                    )                       
                )                            
            );                
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetProductsByQueryResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetProductsByQueryResult.class,
            result
        );
    }
                                        
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getProductConfigurationTypes(org.opencrx.application.shop1.cci2.GetProductConfigurationTypesParams)
     */
    @Override
    public GetProductConfigurationTypesResult getProductConfigurationTypes(
        GetProductConfigurationTypesParams params
    ) {
        List<Structures.Member<GetProductConfigurationTypesResult.Member>> result = 
            new ArrayList<Structures.Member<GetProductConfigurationTypesResult.Member>>();
        try {
            List<ProductConfigurationTypeT> configurationTypesT = new ArrayList<ProductConfigurationTypeT>();
            Collection<ProductConfigurationTypeSet> configurationTypeSets = this.getProductSegment().getConfigurationTypeSet();
            for(ProductConfigurationTypeSet configurationTypeSet: configurationTypeSets) {
                configurationTypesT.add(
                	 this.datatypeMappers.mapProductConfigurationTypeSet(configurationTypeSet)
                );
            }
            result.add(
                Datatypes.member(
                    GetProductConfigurationTypesResult.Member.configurationType,
                    configurationTypesT
                )                            
            );
            result.add(
                Datatypes.member(
                    GetProductConfigurationTypesResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(
                        BasicException.Code.NONE,
                        null
                    )                       
                )                            
            );                
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetProductConfigurationTypesResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetProductConfigurationTypesResult.class,
            result
        );
    }
                                        
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getProductPrices(org.opencrx.application.shop1.cci2.GetProductPricesParams)
     */
    @Override
    public GetProductPricesResult getProductPrices(
        GetProductPricesParams params
    ) {
        List<Structures.Member<GetProductPricesResult.Member>> result = 
            new ArrayList<Structures.Member<GetProductPricesResult.Member>>();
        try {
            // Customer
		    String customerNumber = params.getCustomerNumber();
		    Lead customerContract = null;		    
		    if(customerNumber == null) {
		        customerNumber = this.shopName + "." + params.getPriceCurrency();
		        customerContract = this.findCustomerContractByContractNumber(customerNumber);
		        if(customerContract == null) {
		            customerContract = pm.newInstance(Lead.class);
		            this.datatypeMappers.getLeadFieldMapper().setContractNumber(customerContract, customerNumber);
		            this.datatypeMappers.getLeadFieldMapper().setContractCurrency(customerContract, params.getPriceCurrency());
		            customerContract.getOwningGroup().addAll(
		                this.getContractSegment().getOwningGroup()
		            );
		            PersistenceHelper.currentUnitOfWork(this.pm).begin();
		            this.getContractSegment().addLead(
		                this.uuidAsString(), 
		                customerContract
		            );
		            PersistenceHelper.currentUnitOfWork(this.pm).commit();;                    
		        }
            }
		    else {
	            customerContract = this.findMainCustomerContract(
	            	this.findAccount(params.getCustomerNumber())
	            );
		    }
            // Sales Tax
            String salesTaxTypeName = params.getSalesTaxType();
            BigDecimal salesTaxRate = null;
            if(salesTaxTypeName != null) {
                SalesTaxType salesTaxType = this.findSalesTaxType(salesTaxTypeName);
                salesTaxRate = salesTaxType.getRate();
            }
            else {
                salesTaxRate = BigDecimal.ZERO;
            }
            if(salesTaxRate != null) {
                PricingRuleQuery ruleQuery = (PricingRuleQuery)this.pm.newQuery(PricingRule.class);
                ruleQuery.name().equalTo(this.shopName);
                List<PricingRule> rules = this.getProductSegment().getPricingRule(ruleQuery);
                if(!rules.isEmpty()) {
                    PricingRule rule = rules.iterator().next();
                    boolean hasErrors = false;
                    List<ProductPriceListT> productPriceListsT = new ArrayList<ProductPriceListT>();
                    for(String productNumber: params.getProductNumber()) {
                        Product product = this.findProduct(productNumber);
                        if(product != null) {
                            List<ProductPriceT> productPricesT = new ArrayList<ProductPriceT>();
                            Collection<Uom> priceUoms = product.getPriceUom();
                            for(Uom priceUom: priceUoms) {
                                for(Date pricingDate: params.getPricingDate()) {
                                	org.opencrx.kernel.product1.jmi1.GetPriceLevelParams getPriceLevelParams = Structures.create(
                                		org.opencrx.kernel.product1.jmi1.GetPriceLevelParams.class, 
                                		Datatypes.member(org.opencrx.kernel.product1.jmi1.GetPriceLevelParams.Member.contract, customerContract),
                                		Datatypes.member(org.opencrx.kernel.product1.jmi1.GetPriceLevelParams.Member.priceUom, priceUom),
                                		Datatypes.member(org.opencrx.kernel.product1.jmi1.GetPriceLevelParams.Member.pricingDate, pricingDate),
                                		Datatypes.member(org.opencrx.kernel.product1.jmi1.GetPriceLevelParams.Member.product, product),
                                		Datatypes.member(org.opencrx.kernel.product1.jmi1.GetPriceLevelParams.Member.quantity, new BigDecimal(params.getQuantity() == null ? "1.0" : params.getQuantity()))
                                	);
                                    org.opencrx.kernel.product1.jmi1.GetPriceLevelResult getPriceLevelResult = 
                                        rule.getPriceLevel(getPriceLevelParams);
                                    if(getPriceLevelResult.getPriceLevel() != null) {
                                        AbstractPriceLevel priceLevel = (AbstractPriceLevel)this.pm.getObjectById(getPriceLevelResult.getPriceLevel().refGetPath());
                                        ProductBasePriceQuery priceQuery = (ProductBasePriceQuery)this.pm.newQuery(ProductBasePrice.class);
                                        priceQuery.thereExistsPriceLevel().equalTo(priceLevel);
                                        priceQuery.uom().equalTo(priceUom);
                                        List<ProductBasePrice> prices = product.getBasePrice(priceQuery);
                                        if(!prices.isEmpty()) {
                                            ProductBasePrice price = prices.iterator().next();
                                            productPricesT.add(
                                            	 this.datatypeMappers.mapProductPrice(
                                                    price,
                                                    pricingDate,
                                                    salesTaxRate
                                                )
                                            );
                                        }
                                    }
                                }
                            }
                            productPriceListsT.add(
                                Datatypes.create(
                                    ProductPriceListT.class, 
                                    Datatypes.member(
                                        ProductPriceListT.Member.productNumber,
                                        productNumber
                                    ),
                                    Datatypes.member(
                                        ProductPriceListT.Member.salesTaxType,
                                        salesTaxTypeName
                                    ),
                                    Datatypes.member(
                                        ProductPriceListT.Member.productPrice,
                                        productPricesT
                                    )
                                )
                            );
                        }
                        else {
                            result.add(
                                Datatypes.member(
                                    GetProductPricesResult.Member.status,
                                    this.datatypeMappers.mapOperationStatus(
                                        BasicException.Code.NOT_FOUND,
                                        new String[]{"Product", productNumber}
                                    )
                                )                            
                            );
                            hasErrors = true;
                            break;
                        }
                    }
                    if(!hasErrors) {
                        result.add(
                            Datatypes.member(
                                GetProductPricesResult.Member.productPriceList,
                                productPriceListsT
                            )                            
                        );
                        result.add(
                            Datatypes.member(
                                GetProductPricesResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.NONE,
                                    null
                                )
                            )                            
                        );                                                  
                    }
                }
                else {
                    result.add(
                        Datatypes.member(
                            GetProductPricesResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"PricingRule", this.shopName}
                            )
                        )                            
                    );                                
                }
            }
            else {
                result.add(
                    Datatypes.member(
                        GetProductPricesResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"SalesTaxType", params.getSalesTaxType()}
                        )
                    )                            
                );                                                    
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetProductPricesResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetProductPricesResult.class,
            result
        );
    }
                                                
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getSalesOrder(org.opencrx.application.shop1.cci2.GetSalesOrderParams)
     */
    @Override
    public GetSalesOrderResult getSalesOrder(
        GetSalesOrderParams params
    ) {
        List<Structures.Member<GetSalesOrderResult.Member>> result = 
            new ArrayList<Structures.Member<GetSalesOrderResult.Member>>();
        try {
            SalesOrderQuery query = (SalesOrderQuery)this.pm.newQuery(SalesOrder.class);
            query.thereExistsContractNumber().equalTo(params.getSalesOrderNumber());
            Collection<SalesOrder> salesOrders = this.getContractSegment().getSalesOrder(query);
            if(!salesOrders.isEmpty()) {
                SalesOrder salesOrder = salesOrders.iterator().next();
                result.add(
                    Datatypes.member(
                        GetSalesOrderResult.Member.salesOrder,
                        this.datatypeMappers.mapSalesOrder(salesOrder)
                    )                            
                );
                result.add(
                    Datatypes.member(
                        GetSalesOrderResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                
            }
            else {
                result.add(
                    Datatypes.member(
                        GetSalesOrderResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"SalesOrder", params.getSalesOrderNumber()}
                        )
                    )                            
                );                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetSalesOrderResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetSalesOrderResult.class,
            result
        );
    }
                                                    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getSalesOrders(org.opencrx.application.shop1.cci2.GetSalesOrdersParams)
     */
    @Override
    public GetSalesOrdersResult getSalesOrders(
        GetSalesOrdersParams params
    ) {
        List<Structures.Member<GetSalesOrdersResult.Member>> result = 
            new ArrayList<Structures.Member<GetSalesOrdersResult.Member>>();
        try {
            Account customer = this.findAccount(params.getCustomerNumber());
            if(customer != null) {
                SalesOrderQuery salesOrderQuery = (SalesOrderQuery)this.pm.newQuery(SalesOrder.class);
                salesOrderQuery.thereExistsCustomer().equalTo(customer);
                String contractCategory = this.getShopCategory();
                if(contractCategory != null) {
                	salesOrderQuery.thereExistsCategory().equalTo(contractCategory);
                }
                List<SalesOrderT> salesOrdersT = new ArrayList<SalesOrderT>();
                for(SalesOrder salesOrder: this.getContractSegment().getSalesOrder(salesOrderQuery)) {
                    salesOrdersT.add(
                    	 this.datatypeMappers.mapSalesOrder(salesOrder)
                    );
                }
                result.add(
                    Datatypes.member(
                        GetSalesOrdersResult.Member.salesOrder,
                        salesOrdersT
                    )
                );          
                result.add(
                    Datatypes.member(
                        GetSalesOrdersResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                
            }
            else {
                result.add(
                    Datatypes.member(
                        GetSalesOrdersResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getCustomerNumber()}
                        )
                    )                            
                );                                
            }            
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetSalesOrdersResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetSalesOrdersResult.class,
            result
        );
    }
             
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#getSalesOrderPositions(org.opencrx.application.shop1.cci2.GetSalesOrderPositionsParams)
     */
    @Override
    public GetSalesOrderPositionsResult getSalesOrderPositions(
        GetSalesOrderPositionsParams params
    ) {
        List<Structures.Member<GetSalesOrderPositionsResult.Member>> result = 
            new ArrayList<Structures.Member<GetSalesOrderPositionsResult.Member>>();
        try {
            Account customer = this.findAccount(params.getCustomerNumber());
            if(customer != null) {
                Collection<ContractPositionT> positions = 
                    this.getSalesOrderPositions(
                        customer, 
                        params.getSalesOrderStatusThreshold()
                    );
                result.add(
                    Datatypes.member(
                        GetSalesOrderPositionsResult.Member.position,
                        positions
                    )
                );          
                result.add(
                    Datatypes.member(
                        GetSalesOrderPositionsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        GetSalesOrderPositionsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getCustomerNumber()}
                        )
                    )                            
                );                                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    GetSalesOrderPositionsResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            GetSalesOrderPositionsResult.class,
            result
        );
    }
                                    
	//-----------------------------------------------------------------------
	/* (non-Javadoc)
	 * @see org.opencrx.application.shop1.cci2.ShopService#sendEMail(org.opencrx.application.shop1.cci2.SendEMailParams)
	 */
    @Override
	public SendEMailResult sendEMail(
		SendEMailParams params
	) {
        List<Structures.Member<SendEMailResult.Member>> result = 
            new ArrayList<Structures.Member<SendEMailResult.Member>>();
        try {
        	String activityCreatorName = params.getEmailCreatorName(); 
        	if(activityCreatorName != null && activityCreatorName.length() > 0) {
        		org.opencrx.kernel.activity1.jmi1.Segment activitySegment = this.getActivitySegment();
        		ActivityCreator activityCreator = Activities.getInstance().findActivityCreator(
        			activityCreatorName, 
        			activitySegment 
        		);
        		if(activityCreator != null) {
        			Account reportingCustomer = null;
        			if(params.getOnBehalfOfCustomerNumber() != null) {
        				reportingCustomer = this.findAccount(params.getOnBehalfOfCustomerNumber());
        			}
        			NewActivityParams newActivityParams = Structures.create(
        				NewActivityParams.class, 
        				Datatypes.member(NewActivityParams.Member.creationContext, null),
        				Datatypes.member(NewActivityParams.Member.description, params.getSubject()),
        				Datatypes.member(NewActivityParams.Member.detailedDescription, null),
        				Datatypes.member(NewActivityParams.Member.dueBy, null),
        				Datatypes.member(NewActivityParams.Member.icalType, ICalendar.ICAL_TYPE_NA),
        				Datatypes.member(NewActivityParams.Member.name, params.getSubject()),
        				Datatypes.member(NewActivityParams.Member.priority, Priority.NORMAL.getValue()),
        				Datatypes.member(NewActivityParams.Member.reportingContact, reportingCustomer instanceof Contact ? (Contact)reportingCustomer : null),
        				Datatypes.member(NewActivityParams.Member.scheduledStart, null),
        				Datatypes.member(NewActivityParams.Member.scheduledEnd, null)
        			);
        			PersistenceHelper.currentUnitOfWork(this.pm).begin();
        			NewActivityResult newActivityResult = activityCreator.newActivity(newActivityParams);
        			PersistenceHelper.currentUnitOfWork(this.pm).commit();;
        			Activity activity = newActivityResult.getActivity();
        			this.pm.refresh(activity);
        			if(activity instanceof EMail) {
        				PersistenceHelper.currentUnitOfWork(this.pm).begin();
        				EMail email = (EMail)activity;
        				email.setMessageSubject(
        					params.getSubject()
        				);
        				email.setMessageBody(
        					params.getBody()
        				);
        				Activities.getInstance().addEMailRecipients(
        					email, 
        					params.getSender(), 
        					params.getRecipientTo(), 
        					params.getRecipientCc(), 
        					params.getRecipientBcc()
        				);
         				PersistenceHelper.currentUnitOfWork(this.pm).commit();;
         				// Send EMail
         				Activities.getInstance().sendEMail(
         					email
         				);
	                    result.add(
	                        Datatypes.member(
	                        	SendEMailResult.Member.activity,
	                            this.datatypeMappers.mapActivity(
	                                activity
	                            )
	                        )                            
	                    );                                
	                    result.add(
	                        Datatypes.member(
	                        	SendEMailResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NONE,
	                                null
	                            )
	                        )                            
	                    );
        			}
        			else {
                        result.add(
                            Datatypes.member(
                            	SendEMailResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.ASSERTION_FAILURE,
                                    new String[]{"Activity is not of type EMail", activity.getClass().getName()}
                                )
                            )                            
                        );                                                                        		        			        				
        			}
        		}
        		else {
                    result.add(
                        Datatypes.member(
                        	SendEMailResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NOT_FOUND,
                                new String[]{"ActivityCreator", activityCreatorName}
                            )
                        )                            
                    );                                                                        		        			
        		}
        	}
        	else {
                result.add(
                    Datatypes.member(
                    	SendEMailResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.ASSERTION_FAILURE,
                            new String[]{"ActivityCreator is null or empty", activityCreatorName}
                        )
                    )                            
                );                                                                        		
        	}
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	SendEMailResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	SendEMailResult.class,
            result
        );
    }

    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setCredentials(org.opencrx.application.shop1.cci2.SetCredentialsParams)
     */
    @Override
    public SetCredentialsResult setCredentials(
        SetCredentialsParams params
    ) {
        List<Structures.Member<SetCredentialsResult.Member>> result = 
            new ArrayList<Structures.Member<SetCredentialsResult.Member>>();
        try {
            Account customer = this.findAccount(params.getCustomerNumber());
            if(customer != null) {
                boolean duplicateUserName = false;
                String customerUserName = this.datatypeMappers.getAccountFieldMapper().getUserName(customer);
                if(customerUserName != null && !customerUserName.equals(params.getCredentials().getUserName())) {
                    AccountQuery accountQuery = (AccountQuery)this.pm.newQuery(Account.class);
                    accountQuery.thereExistsUserString0().equalTo(params.getCredentials().getUserName());
                    List<Account> accounts = this.getAccountSegment().getAccount(accountQuery);
                    duplicateUserName = !accounts.isEmpty();
                }
                if(!duplicateUserName) {
                    PersistenceHelper.currentUnitOfWork(this.pm).begin();
                    this.datatypeMappers.getAccountFieldMapper().setUserName(customer, params.getCredentials().getUserName()); 
                    this.datatypeMappers.getAccountFieldMapper().setPasswordMd5(customer, params.getCredentials().getPasswordMd5());
                    this.datatypeMappers.getAccountFieldMapper().setResetPasswordChallenge(customer, params.getCredentials().getResetPasswordChallenge());
                    this.datatypeMappers.getAccountFieldMapper().setResetPasswordResponse(customer, params.getCredentials().getResetPasswordResponse());
                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                    result.add(
                        Datatypes.member(
                            SetCredentialsResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NONE,
                                null
                            )
                        )                            
                    );
                }
                else {
                    result.add(
                        Datatypes.member(
                            SetCredentialsResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.DUPLICATE,
                                new String[]{"Account", params.getCredentials().getUserName()}
                            )
                        )                            
                    );                                
                }
            }
            else {
                result.add(
                    Datatypes.member(
                        SetCredentialsResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getCustomerNumber()}
                        )
                    )                            
                );                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    SetCredentialsResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            SetCredentialsResult.class,
            result
        );
    }
                                                                                                    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setCustomerContractStatus(org.opencrx.application.shop1.cci2.SetCustomerContractStatusParams)
     */
    @Override
    public SetCustomerContractStatusResult setCustomerContractStatus(
    	SetCustomerContractStatusParams params
    ) {
        List<Structures.Member<SetCustomerContractStatusResult.Member>> result = 
            new ArrayList<Structures.Member<SetCustomerContractStatusResult.Member>>();
        try {
            String customerContractNumber = params.getCustomerContractNumber();
            if(customerContractNumber != null) {
	            Lead customerContract = this.findCustomerContractByContractNumber(customerContractNumber);
	            if(customerContract != null) {
	                PersistenceHelper.currentUnitOfWork(this.pm).begin();	       
	                this.setContractStatus(
	                	customerContract, 
	                	params.getContractStatus()
	                );
	                PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	                result.add(
	                    Datatypes.member(
	                    	SetCustomerContractStatusResult.Member.status,
	                        this.datatypeMappers.mapOperationStatus(
	                            BasicException.Code.NONE,
	                            null
	                        )
	                    )
	                );                                                                                    	                
	            }
	            else {
	                result.add(
	                    Datatypes.member(
	                    	SetCustomerContractStatusResult.Member.status,
	                        this.datatypeMappers.mapOperationStatus(
	                            BasicException.Code.NOT_FOUND,
	                            new String[]{"CustomerContract", customerContractNumber}
	                        )
	                    )                            
	                );                                                                                
	            }
            }
            else {
                result.add(
                    Datatypes.member(
                    	SetCustomerContractStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"CustomerContract", customerContractNumber}
                        )
                    )                            
                );                                                                                            	
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                	SetCustomerContractStatusResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
        	SetCustomerContractStatusResult.class,
            result
        );
    }           
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setCustomerStatus(org.opencrx.application.shop1.cci2.SetCustomerStatusParams)
     */
    @Override
    public SetCustomerStatusResult setCustomerStatus(
        SetCustomerStatusParams params
    ) {
        List<Structures.Member<SetCustomerStatusResult.Member>> result = 
            new ArrayList<Structures.Member<SetCustomerStatusResult.Member>>();
        try {
            Account customer = this.findAccount(params.getCustomerNumber());
            if(customer != null) {
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                // Update state history
                String shortDescription = customer.getDescription();
                shortDescription = shortDescription == null
                    ? ""
                    : shortDescription.substring(0, Math.min(shortDescription.length(), 50));
                this.datatypeMappers.getAccountFieldMapper().getStateHistory(customer).add(
                    customer.getAccountState() + "@" +                    
                    DateTimeFormat.BASIC_UTC_FORMAT.format(customer.getModifiedAt()) + " // " +
                    shortDescription
                );
                customer.setAccountState(DatatypeMappers.toShort(params.getCustomerStatus().getStatus()));
                customer.setDescription(params.getCustomerStatus().getDescription());
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;    
                result.add(
                    Datatypes.member(
                        SetCustomerStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                                                                                        
            }
            else {
                result.add(
                    Datatypes.member(
                        SetCustomerStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Account", params.getCustomerNumber()}
                        )
                    )                            
                );                                                                    
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    SetCustomerStatusResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            SetCustomerStatusResult.class,
            result
        );
    }
                                                                        
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setInvoiceStatus(org.opencrx.application.shop1.cci2.SetInvoiceStatusParams)
     */
    @Override
    public SetInvoiceStatusResult setInvoiceStatus(
        SetInvoiceStatusParams params
    ) {
        List<Structures.Member<SetInvoiceStatusResult.Member>> result = 
            new ArrayList<Structures.Member<SetInvoiceStatusResult.Member>>();
        try {
            Invoice invoice = this.findInvoice(params.getInvoiceNumber());
            if(invoice != null) {
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                this.setContractStatus(
                	invoice, 
                	params.getInvoiceStatus()
                );
                if(params.getInvoiceStatus().getStatus() == InvoiceState.PAID.getValue()) {
                    this.datatypeMappers.getInvoiceFieldMapper().setPaymentDate(invoice, new Date());
                }
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;      
                result.add(
                    Datatypes.member(
                        SetInvoiceStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                                                                    
            }
            else {
                result.add(
                    Datatypes.member(
                        SetInvoiceStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Invoice", params.getInvoiceNumber()}
                        )
                    )                            
                );                                                                    
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    SetInvoiceStatusResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            SetInvoiceStatusResult.class,
            result
        );
    }
                                                                            
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setProductStatus(org.opencrx.application.shop1.cci2.SetProductStatusParams)
     */
    @Override
    public SetProductStatusResult setProductStatus(
        SetProductStatusParams params
    ) {
        List<Structures.Member<SetProductStatusResult.Member>> result = 
            new ArrayList<Structures.Member<SetProductStatusResult.Member>>();
        try {
            Product product = this.findProduct(params.getProductNumber());
            if(product != null) {
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                // Update state history
                String shortDescription = product.getDescription();
                shortDescription = shortDescription == null
                    ? ""
                    : shortDescription.substring(0, Math.min(shortDescription.length(), 50));
                this.datatypeMappers.getProductFieldMapper().getStateHistory(product).add(
                    product.getProductState() + "@" +
                    DateTimeFormat.BASIC_UTC_FORMAT.format(product.getModifiedAt()) + " // " +
                    shortDescription
                );
                product.setProductState((short)params.getProductStatus().getStatus());
                product.setDescription(params.getProductStatus().getDescription());
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;        
                result.add(
                    Datatypes.member(
                        SetProductStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                                                
            }
            else {
                result.add(
                    Datatypes.member(
                        SetProductStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Product", params.getProductNumber()}
                        )
                    )                            
                );                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    SetProductStatusResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            SetProductStatusResult.class,
            result
        );
    }
                
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setSalesOrderPositionQuantity(org.opencrx.application.shop1.cci2.SetSalesOrderPositionQuantityParams)
     */
    @Override
    public SetSalesOrderPositionQuantityResult setSalesOrderPositionQuantity(
        SetSalesOrderPositionQuantityParams params
    ) {
        List<Structures.Member<SetSalesOrderPositionQuantityResult.Member>> result = 
            new ArrayList<Structures.Member<SetSalesOrderPositionQuantityResult.Member>>();
        try {
            String contractNumber = params.getContractNumber();
            SalesOrder salesOrder = this.findSalesOrder(contractNumber);
            if(salesOrder != null) {
                Collection<SalesOrderPosition> positions = salesOrder.getPosition();
                for(SalesOrderPosition position: positions) {
                    if(params.getPositionNumber().equals(position.getPositionNumber())) {
                        BigDecimal quantity = new BigDecimal(params.getQuantity());
                        if(quantity.compareTo(BigDecimal.ZERO) == 0) {
                            PersistenceHelper.currentUnitOfWork(this.pm).begin();
                            position.refDelete();
                            PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                        }
                        else {
                            PersistenceHelper.currentUnitOfWork(this.pm).begin();
                            position.setQuantity(quantity);
                            PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                        }
                        PersistenceHelper.currentUnitOfWork(this.pm).begin();
                        salesOrder.reprice();
                        PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                    }
                }
                result.add(
                    Datatypes.member(
                        SetSalesOrderPositionQuantityResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );
            }
            else {
                result.add(
                    Datatypes.member(
                        SetSalesOrderPositionQuantityResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"SalesOrder", contractNumber}
                        )
                    )                            
                );                                                                                                
            }
        }
        catch(Exception e) {
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    SetSalesOrderPositionQuantityResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            SetSalesOrderPositionQuantityResult.class,
            result
        );
    }
                                                                                
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#setSalesOrderStatus(org.opencrx.application.shop1.cci2.SetSalesOrderStatusParams)
     */
    @Override
    public SetSalesOrderStatusResult setSalesOrderStatus(
        SetSalesOrderStatusParams params
    ) {
        List<Structures.Member<SetSalesOrderStatusResult.Member>> result = 
            new ArrayList<Structures.Member<SetSalesOrderStatusResult.Member>>();
        try {
            SalesOrder salesOrder = this.findSalesOrder(params.getSalesOrderNumber());
            if(salesOrder != null) {
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                this.setContractStatus(
                	salesOrder, 
                	params.getSalesOrderStatus()
                );
                PersistenceHelper.currentUnitOfWork(this.pm).commit();;    
                result.add(
                    Datatypes.member(
                        SetSalesOrderStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NONE,
                            null
                        )
                    )                            
                );                                                                                    
            }
            else {
                result.add(
                    Datatypes.member(
                        SetSalesOrderStatusResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"SalesOrder", params.getSalesOrderNumber()}
                        )
                    )                            
                );                                                                    
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    SetSalesOrderStatusResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            SetSalesOrderStatusResult.class,
            result
        );
    }
                             
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#updateCustomer(org.opencrx.application.shop1.cci2.UpdateCustomerParams)
     */
    @Override
    public UpdateCustomerResult updateCustomer(
        UpdateCustomerParams params
    ) {
        List<Structures.Member<UpdateCustomerResult.Member>> result = 
            new ArrayList<Structures.Member<UpdateCustomerResult.Member>>();
        try {
            String customerNumber = params.getCustomer().getCustomerNumber();            
            Account customer = this.findAccount(customerNumber);
            CustomerT customerT = params.getCustomer();
            if(customer != null && customerT != null) {            	            	
            	// Customer is a legal entity
            	if(customerT.getLegalEntity() != null) {
            		LegalEntity legalEntity = (LegalEntity)customer;
            		LegalEntityT legalEntityT = customerT.getLegalEntity();      
            		boolean isDuplicateEmailAddressBusiness = false;
	                if(this.emailAddressMustBeUnique && Boolean.TRUE.equals(params.isUpdateAddressData())) {
	                	if(legalEntityT.getEmailAddressBusiness() != null) {
		                    EMailAddressQuery addressQuery = (EMailAddressQuery)this.pm.newQuery(EMailAddress.class);
		                    addressQuery.thereExistsEmailAddress().equalTo(legalEntityT.getEmailAddressBusiness().getEmailAddress());
		                    List<AccountAddress> addresses = this.getAccountSegment().getAddress(addressQuery);
		                    // Ignore addresses which are composite to account to be updated
		                    for(AccountAddress address: addresses) {
		                    	if(!address.refGetPath().startsWith(legalEntity.refGetPath())) {
		                    		isDuplicateEmailAddressBusiness = true;
		                    		break;
		                    	}
		                    }
		                }
	                }
	                if(!this.emailAddressMustBeUnique || !isDuplicateEmailAddressBusiness) {
	            		String primaryContactNumber = legalEntityT.getPrimaryContactNumber();
	            		org.opencrx.kernel.account1.jmi1.Account primaryContact = null;
	            		if(primaryContactNumber != null) {
	            			primaryContact = this.findAccount(primaryContactNumber);            			
	            		}
	            		if(primaryContact != null || primaryContactNumber == null) {
	                        PersistenceHelper.currentUnitOfWork(this.pm).begin();
		                    // Update main data
		                    if(params.isUpdateMainData()) {
		                    	// Account
		                        customer.getExternalLink().clear();
		                        customer.getExternalLink().addAll(customerT.getExternalId());
		                        if(customerT.getAccountRating() != null) {
		                        	customer.setAccountRating(DatatypeMappers.toShort(customerT.getAccountRating()));
		                        }
		                        customer.getAccountCategory().clear();
		                        customer.getAccountCategory().addAll(DatatypeMappers.toShortList(customerT.getAccountCategory()));
		                        // Legal entity
		                        legalEntity.setName(legalEntityT.getLegalName());
		                        if(primaryContact != null) {
		                        	MemberQuery memberQuery = (MemberQuery)this.pm.newQuery(Member.class);
		                        	if(this.getShopCategory() != null) {
		                        		memberQuery.thereExistsCategory().equalTo(this.getShopCategory());
		                        	}
		                        	memberQuery.thereExistsMemberRole().equalTo(Accounts.MEMBER_ROLE_EMPLOYEE);
		                        	List<Member> members = customer.getMember(memberQuery);
		                        	if(members.isEmpty()) {
		                        		Member member = pm.newInstance(Member.class);
		                        		member.setName(primaryContact.getFullName());
		                        		member.setAccount(primaryContact);
		                        		member.getMemberRole().add(Accounts.MEMBER_ROLE_EMPLOYEE);
		                        		if(this.getShopCategory() != null) {
		                        			member.getCategory().add(this.getShopCategory());
		                        		}
		                        		customer.addMember(
		                        			this.uuidAsString(),
		                        			member
		                        		);
		                        	}
		                        	else {
		                        		Member member = members.iterator().next();
		                        		member.setAccount(primaryContact);
		                        	}
		                        }
		                    }
		                    // Update address data
		                    if(Boolean.TRUE.equals(params.isUpdateAddressData())) {                    
		                        AccountAddress[] addresses = org.opencrx.kernel.backend.Accounts.getInstance().getMainAddresses(legalEntity);
		                        // postalBusiness
		                        if(legalEntityT.getPostalAddressBusiness() == null) {
		                            if(addresses[Accounts.POSTAL_BUSINESS] != null) {
		                                addresses[Accounts.POSTAL_BUSINESS].refDelete();
		                            }
		                        }
		                        else {                        
		                            PostalAddress address = (PostalAddress)addresses[Accounts.POSTAL_BUSINESS];
		                            if(address == null) {
		                                address = this.pm.newInstance(PostalAddress.class);
		                                address.getUsage().add(Addresses.USAGE_BUSINESS);
		                                address.getOwningGroup().addAll(
		                                	legalEntity.getOwningGroup()
		                                );                                
		                                legalEntity.addAddress(
		                                    false, 
		                                    this.uuidAsString(), 
		                                    address
		                                );
		                            }
		                            this.datatypeMappers.mapPostalAddress(
		                            	legalEntityT.getPostalAddressBusiness(),
		                                address
		                            );
		                        }
		                        // emailBusiness
		                        if(legalEntityT.getEmailAddressBusiness() == null) {
		                            if(addresses[Accounts.MAIL_BUSINESS] != null) {
		                                addresses[Accounts.MAIL_BUSINESS].refDelete();
		                            }
		                        }
		                        else {                        
		                            EMailAddress address = (EMailAddress)addresses[Accounts.MAIL_BUSINESS];
		                            if(address == null) {
		                                address = pm.newInstance(EMailAddress.class);
		                                address.getUsage().add(Addresses.USAGE_BUSINESS);
		                                address.getOwningGroup().addAll(
		                                	legalEntity.getOwningGroup()
		                                );                                
		                                legalEntity.addAddress(
		                                    this.uuidAsString(), 
		                                    address
		                                );
		                            }
		                            this.datatypeMappers.mapEmailAddress(
		                            	legalEntityT.getEmailAddressBusiness(),
		                                address
		                            );
		                        }
		                        // faxBusiness
		                        if(legalEntityT.getFaxNumberBusiness() == null) {
		                            if(addresses[Accounts.FAX_BUSINESS] != null) {
		                                addresses[Accounts.FAX_BUSINESS].refDelete();
		                            }
		                        }
		                        else {                        
		                            PhoneNumber address = (PhoneNumber)addresses[Accounts.FAX_BUSINESS];
		                            if(address == null) {
		                                address = pm.newInstance(PhoneNumber.class);
		                                address.getUsage().add(Addresses.USAGE_BUSINESS_FAX);
		                                address.getOwningGroup().addAll(
		                                	legalEntity.getOwningGroup()
		                                );                                
		                                legalEntity.addAddress(
		                                    this.uuidAsString(), 
		                                    address
		                                );
		                            }
		                            this.datatypeMappers.mapPhoneNumber(
		                            	legalEntityT.getFaxNumberBusiness(),
		                                address
		                            );
		                        }
		                        // phoneBusiness
		                        if(legalEntityT.getPhoneNumberBusiness() == null) {
		                            if(addresses[Accounts.PHONE_BUSINESS] != null) {
		                                addresses[Accounts.PHONE_BUSINESS].refDelete();
		                            }
		                        }
		                        else {                        
		                            PhoneNumber address = (PhoneNumber)addresses[Accounts.PHONE_BUSINESS];
		                            if(address == null) {
		                                address = pm.newInstance(PhoneNumber.class);
		                                address.getUsage().add(Addresses.USAGE_BUSINESS);
		                                address.getOwningGroup().addAll(
		                                	legalEntity.getOwningGroup()
		                                );
		                                legalEntity.addAddress(
		                                    this.uuidAsString(), 
		                                    address
		                                );
		                            }
		                            this.datatypeMappers.mapPhoneNumber(
		                            	legalEntityT.getPhoneNumberBusiness(),
		                                address
		                            );
		                        }
		                        // webBusiness
		                        if(legalEntityT.getWebAddressBusiness() == null) {
		                            if(addresses[Accounts.WEB_BUSINESS] != null) {
		                                addresses[Accounts.WEB_BUSINESS].refDelete();
		                            }
		                        }
		                        else {                        
		                            WebAddress address = (WebAddress)addresses[Accounts.WEB_BUSINESS];
		                            if(address == null) {
		                                address = pm.newInstance(WebAddress.class);
		                                address.getUsage().add(Addresses.USAGE_BUSINESS);
		                                address.getOwningGroup().addAll(
		                                	legalEntity.getOwningGroup()
		                                );                                
		                                legalEntity.addAddress(
		                                    this.uuidAsString(), 
		                                    address
		                                );
		                            }
		                            this.datatypeMappers.mapWebAddress(
		                            	legalEntityT.getWebAddressBusiness(),
		                                address
		                            );
		                        }
		                    }
		                    if(Boolean.TRUE.equals(params.isUpdateGenericData())) {
		                        Collection<PropertySet> propertySets = legalEntity.getPropertySet();
		                        List<StringPropertyT> genericData = params.getCustomer().getGenericData();
		                        if(genericData != null) {
		                            boolean isNew = true;
		                            for(PropertySet propertySet: propertySets) {
		                                if(propertySet.getName().equals(PropertySetName.GenericData.toString())) {
		                                    this.updatePropertySet(
		                                        genericData,
		                                        propertySet,
		                                        true
		                                    );
		                                    isNew = false;
		                                    break;
		                                }
		                            }
		                            if(isNew) {
		                                PropertySet propertySet = pm.newInstance(PropertySet.class);
		                                propertySet.setName(PropertySetName.GenericData.toString());
		                                propertySet.getOwningGroup().addAll(
		                                	legalEntity.getOwningGroup()
		                                );
		                                legalEntity.addPropertySet(
		                                    this.uuidAsString(), 
		                                    propertySet
		                                );
		                                this.updatePropertySet(
		                                    genericData,
		                                    propertySet,
		                                    true
		                                );
		                            }
		                        }
		                    }
		                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
		                    result.add(
		                        Datatypes.member(
		                            UpdateCustomerResult.Member.customer,
		                            this.datatypeMappers.mapCustomer(
		                                customer,
		                                this.findCustomerContracts(customer),
		                                this.getShopCategory()
		                            )
		                        )                            
		                    );                                                                                    
		                    result.add(
		                        Datatypes.member(
		                            UpdateCustomerResult.Member.status,
		                            this.datatypeMappers.mapOperationStatus(
		                                BasicException.Code.NONE,
		                                null
		                            )
		                        )
		                    );                                                                                    
	            		}
	            		else {
	                        result.add(
	                            Datatypes.member(
	                                UpdateCustomerResult.Member.status,
	                                this.datatypeMappers.mapOperationStatus(
	                                    BasicException.Code.NOT_FOUND,
	                                    new String[]{"PrimaryContact", primaryContactNumber}
	                                )
	                            )                            
	                        );                                                                                            			
	            		}
	                }
            		else {
            			List<String> addresses = new ArrayList<String>();
            			if( legalEntityT.getEmailAddressBusiness() != null) {
            				addresses.add(legalEntityT.getEmailAddressBusiness().getEmailAddress());
            			}
                        result.add(
                            Datatypes.member(
                            	UpdateCustomerResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.DUPLICATE,
                                    new String[]{"EmailAddress", addresses.toString()}
                                )
                            )
                        );
            		}
            	}
            	// Customer is a contact
            	else if(customerT.getContact() != null) {
            		Contact contact = (Contact)customer;
	                ContactT contactT = customerT.getContact();
	                boolean isDuplicateEmailAddressHome = false;
	                boolean isDuplicateEmailAddressBusiness = false;
	                if(this.emailAddressMustBeUnique && Boolean.TRUE.equals(params.isUpdateAddressData())) {
	                	if(contactT.getEmailAddressHome() != null) {
		                    EMailAddressQuery addressQuery = (EMailAddressQuery)this.pm.newQuery(EMailAddress.class);
		                    addressQuery.thereExistsEmailAddress().equalTo(contactT.getEmailAddressHome().getEmailAddress());
		                    List<AccountAddress> addresses = this.getAccountSegment().getAddress(addressQuery);
		                    // Ignore addresses which are composite to account to be updated
		                    for(AccountAddress address: addresses) {
		                    	if(!address.refGetPath().startsWith(contact.refGetPath())) {
		                    		isDuplicateEmailAddressHome = true;
		                    		break;
		                    	}
		                    }		                    
	                	}
	                	if(contactT.getEmailAddressBusiness() != null) {
		                    EMailAddressQuery addressQuery = (EMailAddressQuery)this.pm.newQuery(EMailAddress.class);
		                    addressQuery.thereExistsEmailAddress().equalTo(contactT.getEmailAddressBusiness().getEmailAddress());
		                    List<AccountAddress> addresses = this.getAccountSegment().getAddress(addressQuery);
		                    // Ignore addresses which are composite to account to be updated
		                    for(AccountAddress address: addresses) {
		                    	if(!address.refGetPath().startsWith(contact.refGetPath())) {
		                    		isDuplicateEmailAddressBusiness = true;
		                    		break;
		                    	}
		                    }
	                	}            	
	                }
	                if(
	                	!this.emailAddressMustBeUnique || 
	                	(!isDuplicateEmailAddressHome && !isDuplicateEmailAddressBusiness)
	                ) {
	                	PersistenceHelper.currentUnitOfWork(this.pm).begin();
	                    if(Boolean.TRUE.equals(params.isUpdateMainData())) {
	                    	// Account
	                        customer.getExternalLink().clear();
	                        customer.getExternalLink().addAll(customerT.getExternalId());
	                        if(customerT.getAccountRating() != null) {
	                        	customer.setAccountRating(DatatypeMappers.toShort(customerT.getAccountRating()));
	                        }
	                        customer.getAccountCategory().clear();
	                        if(customerT.getAccountCategory() != null) {
	                        	customer.getAccountCategory().addAll(DatatypeMappers.toShortList(customerT.getAccountCategory()));
	                        }
	                        // Contact
	                        contact.setOrganization(contactT.getOrganization());
	                        contact.setSalutationCode(DatatypeMappers.toShort(contactT.getSalutationCode()));
	                        contact.setSalutation(contactT.getSalutation());
	                        this.datatypeMappers.getAccountFieldMapper().setTitle(contact, contactT.getTitle());
	                        contact.setFirstName(contactT.getFirstName());
	                        contact.setLastName(contactT.getLastName());
	                        contact.setMiddleName(contactT.getMiddleName());
	                        contact.setNickName(contactT.getNickName());
	                        contact.setBirthdate(contactT.getBirthDate());
	                        this.datatypeMappers.getAccountFieldMapper().setPlaceOfBirth(contact, contactT.getPlaceOfBirth());
	                        this.datatypeMappers.getAccountFieldMapper().setBirthDateIsValidated(contact, contactT.getBirthDateIsValidated());
	                        contact.setDoNotPostalMail(contactT.isDoNotPostalMail());
	                        contact.setDoNotEMail(contactT.isDoNotEmail());
	                        contact.setDoNotPhone(contactT.isDoNotPhone());
	                        contact.setDoNotFax(contactT.isDoNotFax());
	                        this.datatypeMappers.getAccountFieldMapper().setNativeLanguage(contact, contactT.getNativeLanguage());
	                        if(contactT.getPreferredSpokenLanguage() != null) {
	                        	contact.setPreferredSpokenLanguage(DatatypeMappers.toShort(contactT.getPreferredSpokenLanguage()));
	                        }
	                        if(contactT.getPreferredWrittenLanguage() != null) {
	                        	contact.setPreferredWrittenLanguage(DatatypeMappers.toShort(contactT.getPreferredWrittenLanguage()));
	                        }
	                        contact.setGender(DatatypeMappers.toShort(contactT.getGender()));
	                        this.datatypeMappers.getAccountFieldMapper().setJobRole(contact, contactT.getJobRole());
	                        contact.setJobTitle(contactT.getJobTitle());
	                        contact.getCitizenship().clear();
	                        contact.getCitizenship().addAll(DatatypeMappers.toShortList(contactT.getCitizenship()));
	                        if(contactT.getEducation() != null) {
	                        	contact.setEducation(DatatypeMappers.toShort(contactT.getEducation()));
	                        }
	                        this.datatypeMappers.getAccountFieldMapper().setAnnualIncomeAmount(contact, contactT.getAnnualIncomeAmount());
	                        this.datatypeMappers.getAccountFieldMapper().setAnnualIncomeCurrency(contact, contactT.getAnnualIncomeCurrency());
	                        this.datatypeMappers.getAccountFieldMapper().setMonthlyIncomeAmount(contact, contactT.getMonthlyIncomeAmount());
	                        this.datatypeMappers.getAccountFieldMapper().setMonthlyIncomeCurrency(contact, contactT.getMonthlyIncomeCurrency());
	                        if(contactT.getAnnualIncomeCurrency() != null) {
	                        	contact.setAnnualIncomeCurrency(DatatypeMappers.toShort(contactT.getAnnualIncomeCurrency()));
	                        }
	                        if(contactT.getNumberOfChildren() != null) {
	                        	contact.setNumberOfChildren(DatatypeMappers.toShort(contactT.getNumberOfChildren()));
	                        }
	                        contact.getChildrenNames().clear();
	                        contact.getChildrenNames().addAll(contactT.getChildrenNames());
                        	contact.setFamilyStatus(DatatypeMappers.toShort(contactT.getFamilyStatus()));
	                        if(contactT.getPreferredContactMethod() != null) {
	                        	contact.setPreferredContactMethod(DatatypeMappers.toShort(contactT.getPreferredContactMethod()));
	                        }
	                        contact.getReligion().clear();
	                        contact.getReligion().addAll(DatatypeMappers.toShortList(contactT.getReligion()));
	                        this.datatypeMappers.getAccountFieldMapper().setCommunityStatus(contact, contactT.getCommunityStatus());
	                        this.datatypeMappers.getAccountFieldMapper().setCommerceStatus(contact, contactT.getCommerceStatus());
	                        this.datatypeMappers.getAccountFieldMapper().setPersonsInHousehold(contact, contactT.getPersonsInHousehold());
	                        this.datatypeMappers.getAccountFieldMapper().setProfessionalSkills(contact, contactT.getProfessionalSkills());
	                        this.datatypeMappers.getAccountFieldMapper().setInternetUsage(contact, contactT.getInternetUsage());
	                        this.datatypeMappers.getAccountFieldMapper().setInternetProvider(contact, contactT.getInternetProvider());
	                        this.datatypeMappers.getAccountFieldMapper().setPcUsage(contact, contactT.getPcUsage());
	                        this.datatypeMappers.getAccountFieldMapper().setPortalRating(contact, contactT.getPortalRating());
	                    }
	                    if(Boolean.TRUE.equals(params.isUpdateAddressData())) {                    
	                        AccountAddress[] addresses = org.opencrx.kernel.backend.Accounts.getInstance().getMainAddresses(contact);
	                        // postalHome
	                        if(contactT.getPostalAddressHome() == null) {
	                            if(addresses[Accounts.POSTAL_HOME] != null) {
	                                addresses[Accounts.POSTAL_HOME].refDelete();
	                            }
	                        }
	                        else {                        
	                            PostalAddress address = (PostalAddress)addresses[Accounts.POSTAL_HOME];
	                            if(address == null) {
	                                address = pm.newInstance(PostalAddress.class);
	                                address.getUsage().add(Addresses.USAGE_HOME);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapPostalAddress(
	                            	contactT.getPostalAddressHome(),
	                                address
	                            );
	                        }
	                        // postalBusiness
	                        if(contactT.getPostalAddressBusiness() == null) {
	                            if(addresses[Accounts.POSTAL_BUSINESS] != null) {
	                                addresses[Accounts.POSTAL_BUSINESS].refDelete();
	                            }
	                        }
	                        else {                        
	                            PostalAddress address = (PostalAddress)addresses[Accounts.POSTAL_BUSINESS];
	                            if(address == null) {
	                                address = pm.newInstance(PostalAddress.class);
	                                address.getUsage().add(Addresses.USAGE_BUSINESS);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapPostalAddress(
	                            	contactT.getPostalAddressBusiness(),
	                                address
	                            );
	                        }
	                        // emailHome
	                        if(contactT.getEmailAddressHome() == null) {
	                            if(addresses[Accounts.MAIL_HOME] != null) {
	                                addresses[Accounts.MAIL_HOME].refDelete();
	                            }
	                        }
	                        else {                        
	                            EMailAddress address = (EMailAddress)addresses[Accounts.MAIL_HOME];
	                            if(address == null) {
	                                address = pm.newInstance(EMailAddress.class);
	                                address.getUsage().add(Addresses.USAGE_HOME);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapEmailAddress(
	                            	contactT.getEmailAddressHome(),
	                                address
	                            );
	                        }
	                        // emailBusiness
	                        if(contactT.getEmailAddressBusiness() == null) {
	                            if(addresses[Accounts.MAIL_BUSINESS] != null) {
	                                addresses[Accounts.MAIL_BUSINESS].refDelete();
	                            }
	                        }
	                        else {                        
	                            EMailAddress address = (EMailAddress)addresses[Accounts.MAIL_BUSINESS];
	                            if(address == null) {
	                                address = pm.newInstance(EMailAddress.class);
	                                address.getUsage().add(Addresses.USAGE_BUSINESS);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapEmailAddress(
	                            	contactT.getEmailAddressBusiness(),
	                                address
	                            );
	                        }
	                        // faxHome
	                        if(contactT.getFaxNumberHome() == null) {
	                            if(addresses[Accounts.FAX_HOME] != null) {
	                                addresses[Accounts.FAX_HOME].refDelete();
	                            }
	                        }
	                        else {                        
	                            PhoneNumber address = (PhoneNumber)addresses[Accounts.FAX_HOME];
	                            if(address == null) {
	                                address = pm.newInstance(PhoneNumber.class);
	                                address.getUsage().add(Addresses.USAGE_HOME_FAX);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapPhoneNumber(
	                            	contactT.getFaxNumberHome(),
	                                address
	                            );
	                        }
	                        // faxBusiness
	                        if(contactT.getFaxNumberBusiness() == null) {
	                            if(addresses[Accounts.FAX_BUSINESS] != null) {
	                                addresses[Accounts.FAX_BUSINESS].refDelete();
	                            }
	                        }
	                        else {                        
	                            PhoneNumber address = (PhoneNumber)addresses[Accounts.FAX_BUSINESS];
	                            if(address == null) {
	                                address = pm.newInstance(PhoneNumber.class);
	                                address.getUsage().add(Addresses.USAGE_BUSINESS_FAX);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapPhoneNumber(
	                            	contactT.getFaxNumberBusiness(),
	                                address
	                            );
	                        }
	                        // phoneHome
	                        if(contactT.getPhoneNumberHome() == null) {
	                            if(addresses[Accounts.PHONE_HOME] != null) {
	                                addresses[Accounts.PHONE_HOME].refDelete();
	                            }
	                        }
	                        else {                        
	                            PhoneNumber address = (PhoneNumber)addresses[Accounts.PHONE_HOME];
	                            if(address == null) {
	                                address = pm.newInstance(PhoneNumber.class);
	                                address.getUsage().add(Addresses.USAGE_HOME);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapPhoneNumber(
	                            	contactT.getPhoneNumberHome(),
	                                address
	                            );
	                        }
	                        // phoneBusiness
	                        if(contactT.getPhoneNumberBusiness() == null) {
	                            if(addresses[Accounts.PHONE_BUSINESS] != null) {
	                                addresses[Accounts.PHONE_BUSINESS].refDelete();
	                            }
	                        }
	                        else {                        
	                            PhoneNumber address = (PhoneNumber)addresses[Accounts.PHONE_BUSINESS];
	                            if(address == null) {
	                                address = pm.newInstance(PhoneNumber.class);
	                                address.getUsage().add(Addresses.USAGE_BUSINESS);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapPhoneNumber(
	                            	contactT.getPhoneNumberBusiness(),
	                                address
	                            );
	                        }
	                        // phoneMobile
	                        if(contactT.getPhoneNumberMobile() == null) {
	                            if(addresses[Accounts.MOBILE] != null) {
	                                addresses[Accounts.MOBILE].refDelete();
	                            }
	                        }
	                        else {                        
	                            PhoneNumber address = (PhoneNumber)addresses[Accounts.MOBILE];
	                            if(address == null) {
	                                address = pm.newInstance(PhoneNumber.class);
	                                address.getUsage().add(Addresses.USAGE_MOBILE);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapPhoneNumber(
	                            	contactT.getPhoneNumberMobile(),
	                                address
	                            );
	                        }
	                        // webHome
	                        if(contactT.getWebAddressHome() == null) {
	                            if(addresses[Accounts.WEB_HOME] != null) {
	                                addresses[Accounts.WEB_HOME].refDelete();
	                            }
	                        }
	                        else {                        
	                            WebAddress address = (WebAddress)addresses[Accounts.WEB_HOME];
	                            if(address == null) {
	                                address = pm.newInstance(WebAddress.class);
	                                address.getUsage().add(Addresses.USAGE_HOME);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );      
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapWebAddress(
	                            	contactT.getWebAddressHome(),
	                                address
	                            );
	                        }
	                        // webBusiness
	                        if(contactT.getWebAddressBusiness() == null) {
	                            if(addresses[Accounts.WEB_BUSINESS] != null) {
	                                addresses[Accounts.WEB_BUSINESS].refDelete();
	                            }
	                        }
	                        else {                        
	                            WebAddress address = (WebAddress)addresses[Accounts.WEB_BUSINESS];
	                            if(address == null) {
	                                address = pm.newInstance(WebAddress.class);
	                                address.getUsage().add(Addresses.USAGE_BUSINESS);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                            }
	                            this.datatypeMappers.mapWebAddress(
	                            	contactT.getWebAddressBusiness(),
	                                address
	                            );
	                        }
	                        // messengerAddress
	                        if(contactT.getMessengerAddress() != null) {
	                            Collection<AccountAddress> accountAddresses = contact.getAddress();
	                            int i = 0;
	                            for(AccountAddress address: accountAddresses) {
	                                if(address instanceof PhoneNumber) {
	                                    boolean containsUsageOther = false;
	                                    for(Short usage: address.getUsage()) {
	                                        if(usage.compareTo(Addresses.USAGE_OTHER) == 0) {
	                                            containsUsageOther = true;
	                                            break;
	                                        }
	                                    }
	                                    if(containsUsageOther) {
	                                        if(i >= contactT.getMessengerAddress().size()) {
	                                            address.refDelete();
	                                        }
	                                        else {
	                                        	 this.datatypeMappers.mapMessengerAddress(
	                                            	contactT.getMessengerAddress().get(i),
	                                                (PhoneNumber)address
	                                            );
	                                        }
	                                        i++;
	                                    }
	                                }
	                            }
	                            while(i < contactT.getMessengerAddress().size()) {
	                                PhoneNumber address = pm.newInstance(PhoneNumber.class);
	                                address.getUsage().add(Addresses.USAGE_OTHER);
	                                address.getOwningGroup().addAll(
	                                	contact.getOwningGroup()
	                                );                                
	                                contact.addAddress(
	                                    this.uuidAsString(), 
	                                    address
	                                );
	                                this.datatypeMappers.mapMessengerAddress(
	                                	contactT.getMessengerAddress().get(i),
	                                    address
	                                );
	                                i++;
	                            }
	                        }    
	                        // blogAddress
	                        this.datatypeMappers.getAccountFieldMapper().setBlogAddress(
	                        	contact, 
	                            contactT.getBlogAddress()
	                        );
	                    }
	                    if(Boolean.TRUE.equals(params.isUpdateGenericData())) {
	                        List<StringPropertyT> genericData = params.getCustomer().getGenericData();
	                        if(genericData != null) {
	                        	PropertySetQuery propertySetQuery = (PropertySetQuery)pm.newQuery(PropertySet.class);
	                        	propertySetQuery.name().equalTo(PropertySetName.GenericData.toString());
	                        	List<PropertySet> propertySets = contact.getPropertySet(propertySetQuery);
	                        	PropertySet propertySet = null;
	                        	if(propertySets.isEmpty()) {
	                                propertySet = pm.newInstance(PropertySet.class);
	                                propertySet.setName(PropertySetName.GenericData.toString());
	                                propertySet.getOwningGroup().addAll(
	                                    customer.getOwningGroup()
	                                );
	                                contact.addPropertySet(
	                                    this.uuidAsString(), 
	                                    propertySet
	                                );                        		
	                        	} else {
	                        		propertySet = propertySets.iterator().next();
	                        	}
	                            this.updatePropertySet(
	                                genericData,
	                                propertySet,
	                                true
	                            );
	                        }
	                    }
	                    if(Boolean.TRUE.equals(params.isUpdateHobbyAndInterestData())) {
	                        CustomerHobbyAndInterestT hobbyAndInterestT = contactT.getHobbyAndInterest();
	                        if(hobbyAndInterestT != null) {
	                            this.datatypeMappers.getAccountFieldMapper().setSports(contact, hobbyAndInterestT.getSports());
	                            this.datatypeMappers.getAccountFieldMapper().setTravel(contact, hobbyAndInterestT.getTravel());
	                            this.datatypeMappers.getAccountFieldMapper().setFinance(contact, hobbyAndInterestT.getFinance());
	                            this.datatypeMappers.getAccountFieldMapper().setComputerInternet(contact, hobbyAndInterestT.getComputerInternet());
	                            this.datatypeMappers.getAccountFieldMapper().setTelecommunication(contact, hobbyAndInterestT.getTelecommunication());
	                            this.datatypeMappers.getAccountFieldMapper().setEntertainment(contact, hobbyAndInterestT.getEntertainment());
	                            this.datatypeMappers.getAccountFieldMapper().setMusic(contact, hobbyAndInterestT.getMusic());
	                            this.datatypeMappers.getAccountFieldMapper().setLifestyle(contact, hobbyAndInterestT.getLifestyle());
	                            this.datatypeMappers.getAccountFieldMapper().setOther(contact, hobbyAndInterestT.getOther());                        
	                        }
	                    }
	                    if(Boolean.TRUE.equals(params.isUpdateBookmarks())) {
	                        List<StringPropertyT> bookmarks = contactT.getBookmarks();
	                        if(bookmarks != null) {
	                        	PropertySetQuery propertySetQuery = (PropertySetQuery)pm.newQuery(PropertySet.class);
	                        	propertySetQuery.name().equalTo(PropertySetName.Bookmarks.toString());
	                        	List<PropertySet> propertySets = contact.getPropertySet(propertySetQuery);
	                        	PropertySet propertySet = null;
	                        	if(propertySets.isEmpty()) {
	                                propertySet = pm.newInstance(PropertySet.class);
	                                propertySet.setName(PropertySetName.Bookmarks.toString());
	                                propertySet.getOwningGroup().addAll(
	                                    customer.getOwningGroup()
	                                );
	                                customer.addPropertySet(
	                                    this.uuidAsString(), 
	                                    propertySet
	                                );                        		
	                        	} else {
	                        		propertySet = propertySets.iterator().next();
	                        	}
	                            this.updatePropertySet(
	                                bookmarks,
	                                propertySet,
	                                true
	                            );
	                        }
	                    }
	                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	                    result.add(
	                        Datatypes.member(
	                            UpdateCustomerResult.Member.customer,
	                            this.datatypeMappers.mapCustomer(
	                                customer,
	                                this.findCustomerContracts(customer),
	                                this.getShopCategory()
	                            )
	                        )                            
	                    );                                                                                    
	                    result.add(
	                        Datatypes.member(
	                            UpdateCustomerResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.NONE,
	                                null
	                            )
	                        )
	                    );                                                                                    
	                }
	                else {
            			List<String> addresses = new ArrayList<String>();
            			if(contactT.getEmailAddressHome() != null) {
            				addresses.add(contactT.getEmailAddressHome().getEmailAddress());
            			}
            			if(contactT.getEmailAddressBusiness() != null) {
            				addresses.add(contactT.getEmailAddressBusiness().getEmailAddress());
            			}
	                    result.add(
	                        Datatypes.member(
	                            UpdateCustomerResult.Member.status,
	                            this.datatypeMappers.mapOperationStatus(
	                                BasicException.Code.DUPLICATE,
	                                new String[]{"EmailAddress", addresses.toString()}
	                            )
	                        )                            
	                    );
	                }
            	}
            }
            else {
                result.add(
                    Datatypes.member(
                        UpdateCustomerResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Customer", customerNumber}
                        )
                    )                            
                );                                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    UpdateCustomerResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            UpdateCustomerResult.class,
            result
        );
    }
            
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#updateCustomerContract(org.opencrx.application.shop1.cci2.UpdateCustomerContractParams)
     */
    @Override
    public UpdateCustomerContractResult updateCustomerContract(
        UpdateCustomerContractParams params
    ) {
        List<Structures.Member<UpdateCustomerContractResult.Member>> result = 
            new ArrayList<Structures.Member<UpdateCustomerContractResult.Member>>();
        try {
            String customerContractNumber = params.getCustomerContract().getContractNumber();
            if(customerContractNumber != null) {
	            Lead customerContract = this.findCustomerContractByContractNumber(customerContractNumber);
	            CustomerContractT customerContractT = params.getCustomerContract();
	            if(customerContract != null && customerContractT != null) {
	                PersistenceHelper.currentUnitOfWork(this.pm).begin();
		           	this.datatypeMappers.mapCustomerContract(
		                customerContractT,
		                customerContract
		            );	                    			                
	                PersistenceHelper.currentUnitOfWork(this.pm).commit();;
	                result.add(
	                    Datatypes.member(
	                        UpdateCustomerContractResult.Member.customerContract,
	                        this.datatypeMappers.mapCustomerContract(
	                            customerContract
	                        )
	                    )                            
	                );                                                                                    
	                result.add(
	                    Datatypes.member(
	                        UpdateCustomerContractResult.Member.status,
	                        this.datatypeMappers.mapOperationStatus(
	                            BasicException.Code.NONE,
	                            null
	                        )
	                    )
	                );                                                                                    	                
	            }
	            else {
	                result.add(
	                    Datatypes.member(
	                        UpdateCustomerContractResult.Member.status,
	                        this.datatypeMappers.mapOperationStatus(
	                            BasicException.Code.NOT_FOUND,
	                            new String[]{"CustomerContract", customerContractNumber}
	                        )
	                    )                            
	                );                                                                                
	            }
            }
            else {
                result.add(
                    Datatypes.member(
                        UpdateCustomerContractResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"CustomerContract", customerContractNumber}
                        )
                    )                            
                );                                                                                            	
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}                        
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    UpdateCustomerContractResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            UpdateCustomerContractResult.class,
            result
        );
    }           
    
    //-----------------------------------------------------------------------    
    /* (non-Javadoc)
     * @see org.opencrx.application.shop1.cci2.ShopService#updateProduct(org.opencrx.application.shop1.cci2.UpdateProductParams)
     */
    @Override
    public UpdateProductResult updateProduct(
        UpdateProductParams params
    ) {
        List<Structures.Member<UpdateProductResult.Member>> result = 
            new ArrayList<Structures.Member<UpdateProductResult.Member>>();
        try {
            String productNumber = params.getProduct().getProductNumber();
            Product product = this.findProduct(productNumber);
            if(product != null) {
                ProductT productT = params.getProduct();
                boolean hasErrors = false;
                PersistenceHelper.currentUnitOfWork(this.pm).begin();
                // Main data
                if(!hasErrors && params.isUpdateMainData()) {
                	 this.datatypeMappers.mapProduct(
                        productT,
                        product
                	 );
                	 this.updateProductDescriptions(
                		 productT, 
                		 product
                	 );
                }
                // Classification
                if(!hasErrors && Boolean.TRUE.equals(params.isUpdateClassification())) {
                    List<ProductClassification> newClassifications = new ArrayList<ProductClassification>();
                    for(String classificationId: params.getProduct().getClassificationId()) {
                        ProductClassification classification = this.findProductClassification(classificationId);
                        if(classification != null) {
                            newClassifications.add(classification);
                        }
                        else {
                            result.add(
                                Datatypes.member(
                                    UpdateProductResult.Member.status,
                                    this.datatypeMappers.mapOperationStatus(
                                        BasicException.Code.NOT_FOUND,
                                        new String[]{"ProductClassification", classificationId}
                                    )
                                )                            
                            );
                            hasErrors = true;
                            break;
                        }                    
                    }
                    if(!hasErrors) {
                        product.getClassification().clear();
                        product.getClassification().addAll(newClassifications);
                    }
                }
                // Uom
                if(!hasErrors && Boolean.TRUE.equals(params.isUpdateUom())) {
                    List<Uom> newUoms = new ArrayList<Uom>();
                    for(String uomName: params.getProduct().getPriceUom()) {
                        Uom uom = this.findUom(uomName);
                        if(uom != null) {
                            newUoms.add(uom);
                        }
                        else {
                            result.add(
                                Datatypes.member(
                                    UpdateProductResult.Member.status,
                                    this.datatypeMappers.mapOperationStatus(
                                        BasicException.Code.NOT_FOUND,
                                        new String[]{"Uom", uomName}
                                    )
                                )                            
                            );
                            hasErrors = true;
                            break;
                        }                    
                    }
                    if(!hasErrors) {
                        product.getPriceUom().clear();
                        product.getPriceUom().addAll(newUoms);
                    }         
                }
                // Configuration
                if(!hasErrors && Boolean.TRUE.equals(params.isUpdateConfiguration())) {
                    ProductConfigurationTypeSet configurationType = this.findProductConfigurationType(productT.getConfigurationType());
                    if(configurationType == null) {
                        result.add(
                            Datatypes.member(
                                UpdateProductResult.Member.status,
                                this.datatypeMappers.mapOperationStatus(
                                    BasicException.Code.NOT_FOUND,
                                    new String[]{"ProductConfigurationType", productT.getConfigurationType()}
                                )
                            )                            
                        );                                    
                        hasErrors = true;
                    }
                    if(!hasErrors) {
                        this.updateProductConfiguration(
                            productT, 
                            product, 
                            configurationType
                        );
                    }
                }
                // Picture
                if(!hasErrors && Boolean.TRUE.equals(params.isUpdatePicture())) {
                    this.updateProductPicture(
                        productT, 
                        product
                    );
                }
                // Online date
                if(!hasErrors && Boolean.TRUE.equals(params.isUpdateProductPhase())) {
                    this.updateProductPhase(
                        productT,
                        product
                    );
                }
                // Bundle data
                if(!hasErrors && Boolean.TRUE.equals(params.isUpdateBundleData())) {
                	this.updateProductBundle(
                		productT, 
                		product
                	);
                }
                if(!hasErrors) {
                    PersistenceHelper.currentUnitOfWork(this.pm).commit();;
                    result.add(
                        Datatypes.member(
                            UpdateProductResult.Member.product,
                            this.datatypeMappers.mapProduct(
                                product,
                                params.isUpdatePicture(),
                                this.getProductSegment()
                            )
                        )                            
                    );                                
                    result.add(
                        Datatypes.member(
                            UpdateProductResult.Member.status,
                            this.datatypeMappers.mapOperationStatus(
                                BasicException.Code.NONE,
                                null
                            )
                        )                            
                    );
                }
                else {
                    PersistenceHelper.currentUnitOfWork(this.pm).rollback();
                }
            }
            else {
                result.add(
                    Datatypes.member(
                        UpdateProductResult.Member.status,
                        this.datatypeMappers.mapOperationStatus(
                            BasicException.Code.NOT_FOUND,
                            new String[]{"Product", productNumber}
                        )
                    )                            
                );                                                                
            }
        }
        catch(Exception e) {
            try {
                PersistenceHelper.currentUnitOfWork(this.pm).rollback();
            } catch(Exception e0) {}            
            new ServiceException(e).log();
            result.add(
                Datatypes.member(
                    UpdateProductResult.Member.status,
                    this.datatypeMappers.mapOperationStatus(e)
                )                            
            );
        }
        return Structures.create(
            UpdateProductResult.class,
            result
        );
    }

	//-----------------------------------------------------------------------
    public void close(
    ) {
    	if(this.pm != null) {
    		this.pm.close();
    	}
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    protected final PersistenceManager pm;
    protected final String providerName;
    protected final String segmentName;
    protected final boolean emailAddressMustBeUnique;
    protected final boolean noCopyOfProductConfiguration;
    protected final String shopName;
    protected final DatatypeMappers datatypeMappers;
    
}

//--- End of File -----------------------------------------------------------
