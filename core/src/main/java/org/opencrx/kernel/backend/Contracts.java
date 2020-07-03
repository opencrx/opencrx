/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: Contracts
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

package org.opencrx.kernel.backend;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.contract1.cci2.AbstractContractQuery;
import org.opencrx.kernel.contract1.cci2.AbstractInvoicePositionQuery;
import org.opencrx.kernel.contract1.cci2.AbstractOpportunityPositionQuery;
import org.opencrx.kernel.contract1.cci2.AbstractQuotePositionQuery;
import org.opencrx.kernel.contract1.cci2.AbstractSalesOrderPositionQuery;
import org.opencrx.kernel.contract1.cci2.CalculationRuleQuery;
import org.opencrx.kernel.contract1.cci2.ContractCreatorQuery;
import org.opencrx.kernel.contract1.cci2.ContractTypeQuery;
import org.opencrx.kernel.contract1.jmi1.AbstractContract;
import org.opencrx.kernel.contract1.jmi1.AbstractFilterContract;
import org.opencrx.kernel.contract1.jmi1.AbstractInvoicePosition;
import org.opencrx.kernel.contract1.jmi1.AbstractOpportunityPosition;
import org.opencrx.kernel.contract1.jmi1.AbstractQuotePosition;
import org.opencrx.kernel.contract1.jmi1.AbstractSalesOrderPosition;
import org.opencrx.kernel.contract1.jmi1.AccountAssignmentContract;
import org.opencrx.kernel.contract1.jmi1.CalculationRule;
import org.opencrx.kernel.contract1.jmi1.Contract1Package;
import org.opencrx.kernel.contract1.jmi1.ContractCreator;
import org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal;
import org.opencrx.kernel.contract1.jmi1.ContractGroup;
import org.opencrx.kernel.contract1.jmi1.ContractGroupAssignment;
import org.opencrx.kernel.contract1.jmi1.ContractType;
import org.opencrx.kernel.contract1.jmi1.DeliveryInformation;
import org.opencrx.kernel.contract1.jmi1.EMailAddress;
import org.opencrx.kernel.contract1.jmi1.GenericContract;
import org.opencrx.kernel.contract1.jmi1.GetContractAmountsResult;
import org.opencrx.kernel.contract1.jmi1.GetPositionAmountsResult;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.InvoicePosition;
import org.opencrx.kernel.contract1.jmi1.Lead;
import org.opencrx.kernel.contract1.jmi1.Opportunity;
import org.opencrx.kernel.contract1.jmi1.OpportunityPosition;
import org.opencrx.kernel.contract1.jmi1.PhoneNumber;
import org.opencrx.kernel.contract1.jmi1.PositionCreation;
import org.opencrx.kernel.contract1.jmi1.PositionModification;
import org.opencrx.kernel.contract1.jmi1.PositionRemoval;
import org.opencrx.kernel.contract1.jmi1.PreviewRepriceResult;
import org.opencrx.kernel.contract1.jmi1.QuantityModification;
import org.opencrx.kernel.contract1.jmi1.Quote;
import org.opencrx.kernel.contract1.jmi1.QuotePosition;
import org.opencrx.kernel.contract1.jmi1.RemovedPosition;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesContractCreator;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.contract1.jmi1.SalesOrderPosition;
import org.opencrx.kernel.contract1.jmi1.SalesVolumeContract;
import org.opencrx.kernel.depot1.cci2.DepotPositionReferenceQuery;
import org.opencrx.kernel.depot1.cci2.DepotReferenceQuery;
import org.opencrx.kernel.depot1.jmi1.Depot;
import org.opencrx.kernel.depot1.jmi1.DepotPosition;
import org.opencrx.kernel.depot1.jmi1.DepotPositionReference;
import org.opencrx.kernel.depot1.jmi1.DepotReference;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.generic.SecurityKeys.Action;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.Description;
import org.opencrx.kernel.generic.jmi1.DescriptionContainer;
import org.opencrx.kernel.generic.jmi1.PropertySet;
import org.opencrx.kernel.product1.cci2.PricingRuleQuery;
import org.opencrx.kernel.product1.cci2.SalesTaxTypeAssignmentQuery;
import org.opencrx.kernel.product1.cci2.SalesTaxTypeQuery;
import org.opencrx.kernel.product1.jmi1.AbstractPriceLevel;
import org.opencrx.kernel.product1.jmi1.AbstractProduct;
import org.opencrx.kernel.product1.jmi1.ConfiguredProduct;
import org.opencrx.kernel.product1.jmi1.DiscountOrigin;
import org.opencrx.kernel.product1.jmi1.GetPriceLevelResult;
import org.opencrx.kernel.product1.jmi1.PricingRule;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.opencrx.kernel.product1.jmi1.SalesTaxType;
import org.opencrx.kernel.product1.jmi1.SalesTaxTypeAssignment;
import org.opencrx.kernel.product1.jmi1.SalesTaxTypeGroup;
import org.opencrx.kernel.product1.jmi1.SalesTransactionType;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.opencrx.kernel.utils.ScriptUtils;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.accessor.jmi.cci.RefPackage_1_0;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.RuntimeServiceException;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.Authority;
import org.openmdx.base.marshalling.Marshaller;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * Default Contracts backend.
 *
 */
public class Contracts extends AbstractImpl {

	/**
	 * Register Contracts backend.
	 */
	public static void register(
	) {
		registerImpl(new Contracts());
	}
	
	/**
	 * Get registered Contracts backend.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Contracts getInstance(
	) throws ServiceException {
		return getInstance(Contracts.class);
	}

	/**
	 * Constructor
	 */
	protected Contracts(
	) {
		
	}
	
    /**
     * Find calculation rule with given name.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    public CalculationRule findCalculationRule(
        String name,
        org.opencrx.kernel.contract1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        CalculationRuleQuery calculationRuleQuery = (CalculationRuleQuery)pm.newQuery(CalculationRule.class);
        calculationRuleQuery.name().equalTo(name);
        List<CalculationRule> calculationRules = segment.getCalculationRule(calculationRuleQuery);
        return calculationRules.isEmpty() ? 
            null : 
            calculationRules.iterator().next();
    }
    
    /**
     * Find contract creator with given name.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    public ContractCreator findContractCreator(
        String name,
        org.opencrx.kernel.contract1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        ContractCreatorQuery contractCreatorQuery = (ContractCreatorQuery)pm.newQuery(ContractCreator.class);
        contractCreatorQuery.name().equalTo(name);
        List<ContractCreator> contractCreators = segment.getContractCreator(contractCreatorQuery);
        return contractCreators.isEmpty() ? null : 
            contractCreators.iterator().next();
    }

    /**
     * Get contracts segment.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.opencrx.kernel.contract1.jmi1.Segment getContractSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.contract1.jmi1.Segment)pm.getObjectById(
            new Path("xri:@openmdx:org.opencrx.kernel.contract1/provider/" + providerName + "/segment/" + segmentName)
        );
    }

    /**
     * Create / update calculation rule.
     * 
     * @param calculationRuleName
     * @param description
     * @param getPositionAmountsScript
     * @param getContractAmountsScript
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public CalculationRule initCalculationRule(
        String calculationRuleName,
        String description,
        String getPositionAmountsScript,
        String getContractAmountsScript,
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
            pm, 
            providerName, 
            segmentName
        );
        CalculationRule calculationRule = null;
        if((calculationRule = this.findCalculationRule(calculationRuleName, contractSegment)) != null) {
            return calculationRule;            
        }                
        pm.currentTransaction().begin();
        calculationRule = pm.newInstance(CalculationRule.class);
        calculationRule.setName(calculationRuleName);
        calculationRule.setDescription(description);
        calculationRule.setGetPositionAmountsScript(getPositionAmountsScript);
        calculationRule.setGetContractAmountsScript(getContractAmountsScript);
        calculationRule.setDefault(true);
        calculationRule.getOwningGroup().addAll(
            contractSegment.getOwningGroup()
        );
        contractSegment.addCalculationRule(
            UUIDConversion.toUID(UUIDs.newUUID()),
            calculationRule
        );                        
        pm.currentTransaction().commit();        
        return calculationRule;
    }
       
	/**
	 * Find contract filter.
	 * 
	 * @param contractFilterName
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal findContractFilter(
		String contractFilterName,
		org.opencrx.kernel.contract1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);		
		org.opencrx.kernel.contract1.cci2.ContractFilterGlobalQuery query =
		    (org.opencrx.kernel.contract1.cci2.ContractFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal.class);
		query.name().equalTo(contractFilterName);
		Collection<ContractFilterGlobal> contractFilters = segment.getContractFilter(query);
		if(!contractFilters.isEmpty()) {
			return (org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal)contractFilters.iterator().next();
		}
		return null;
	}
    
	/**
	 * Init contract filter.
	 * 
	 * @param filterName
	 * @param filterProperties
	 * @param pm
	 * @param segment
	 * @param allUsers
	 * @return
	 */
	public org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal initContractFilter(
		String filterName,
		org.opencrx.kernel.contract1.jmi1.ContractFilterProperty[] filterProperties,
		org.opencrx.kernel.contract1.jmi1.Segment segment,
		List<PrincipalGroup> allUsers
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal contractFilter = this.findContractFilter(
			filterName,
			segment
		);
		if(contractFilter != null) return contractFilter;
		try {
			pm.currentTransaction().begin();
			contractFilter = pm.newInstance(org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal.class);
			contractFilter.setName(filterName);
			contractFilter.getOwningGroup().addAll(allUsers);
			segment.addContractFilter(
				false,
				Contracts.getInstance().getUidAsString(),
				contractFilter
			);
			for(int i = 0; i < filterProperties.length; i++) {
				filterProperties[i].getOwningGroup().addAll(allUsers);
				contractFilter.addFilterProperty(
					false,
					Contracts.getInstance().getUidAsString(),
					filterProperties[i]
				);
			}
			pm.currentTransaction().commit();
		} catch(Exception e) {
			new ServiceException(e).log();
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return contractFilter;
	}
    
    /**
     * Copy contract specific attributes from source to target.
     * 
     * @param source
     * @param target
     */
    protected void copyCrxObject(
    	CrxObject source,
    	CrxObject target
    ) {
        // Index 0
        target.setUserBoolean0(source.isUserBoolean0());
        target.setUserNumber0(source.getUserNumber0());
        target.setUserString0(source.getUserString0());
        target.setUserDateTime0(source.getUserDateTime0());
        target.setUserDate0(source.getUserDate0());
        target.setUserCode0(source.getUserCode0());            
        // Index 1
        target.setUserBoolean1(source.isUserBoolean1());
        target.setUserNumber1(source.getUserNumber1());
        target.setUserString1(source.getUserString1());
        target.setUserDateTime1(source.getUserDateTime1());
        target.setUserDate1(source.getUserDate1());
        target.setUserCode1(source.getUserCode1());            
        // Index 2
        target.setUserBoolean2(source.isUserBoolean2());
        target.setUserNumber2(source.getUserNumber2());
        target.setUserString2(source.getUserString2());
        target.setUserDateTime2(source.getUserDateTime2());
        target.setUserDate2(source.getUserDate2());
        target.setUserCode2(source.getUserCode2());            
        // Index 3
        target.setUserBoolean3(source.isUserBoolean3());
        target.setUserNumber3(source.getUserNumber3());
        target.setUserString3(source.getUserString3());
        target.setUserDateTime3(source.getUserDateTime3());
        target.setUserDate3(source.getUserDate3());
        target.setUserCode3(source.getUserCode3());            
        // Index 4
        target.setUserBoolean4(source.getUserBoolean4());
        target.setUserNumber4(source.getUserNumber4());
        target.setUserString4(source.getUserString4());
        target.setUserDateTime4(source.getUserDateTime4());
        target.setUserDate4(source.getUserDate4());
        target.setUserCode4(source.getUserCode4());                	
    }
    
    /**
     * Copy sales contract specific attributes from source to target.
     * 
     * @param source
     * @param target
     * @throws ServiceException
     */
    protected void copySalesContract(
        SalesContract source,
        SalesContract target
    ) throws ServiceException {
        target.setName(source.getName());
        target.setDescription(source.getDescription());
        target.setPriority(source.getPriority());
        target.setActiveOn(source.getActiveOn());
        target.setExpiresOn(source.getExpiresOn());
        target.setCancelOn(source.getCancelOn());
        target.setClosedOn(source.getClosedOn());
        target.setContractNumber(source.getContractNumber());
        target.setContractCurrency(source.getContractCurrency());
        target.setPaymentTerms(source.getPaymentTerms());
        target.setContractLanguage(source.getContractLanguage());
        target.setContractState(source.getContractState());
        target.setPricingDate(source.getPricingDate());
        target.getCompetitor().clear();
        target.getCompetitor().addAll(source.getCompetitor());
        target.getContact().clear();
        target.getContact().addAll(source.getContact());
        target.setBroker(source.getBroker());
        target.setCustomer(source.getCustomer());
        target.setSalesRep(source.getSalesRep());
        target.setSupplier(source.getSupplier());
        target.getActivity().clear();
        target.getActivity().addAll(source.getActivity());
        target.setOrigin(source.getOrigin());
        target.getInventoryCb().clear();
        target.getInventoryCb().addAll(source.getInventoryCb());
        target.setPricingRule(source.getPricingRule());
        target.setCalcRule(source.getCalcRule());
        target.setShippingMethod(source.getShippingMethod());
        target.setShippingTrackingNumber(source.getShippingTrackingNumber());
        target.setShippingInstructions(source.getShippingInstructions());
        target.setGift(source.isGift());
        target.setGiftMessage(source.getGiftMessage());
        target.getOwningGroup().clear();
        target.getOwningGroup().addAll(source.getOwningGroup());
        target.setOwningUser(source.getOwningUser());
        target.setDisabled(source.isDisabled());
        target.setDisabledReason(source.getDisabledReason());
        target.getExternalLink().clear();
        target.getExternalLink().addAll(source.getExternalLink());
        target.getCategory().clear();
        target.getCategory().addAll(source.getCategory());
        target.setCarrier(source.getCarrier());
        target.setFreightTerms(source.getFreightTerms());
        target.setSalesTaxTypeGroup(source.getSalesTaxTypeGroup());
    	copyCrxObject(
    		source,
    		target
    	);
    }

    /**
     * Copy sales contract position specific attributes from source to target.
     * 
     * @param source
     * @param target
     * @throws ServiceException
     */
    protected void copySalesContractPosition(
        SalesContractPosition source,
        SalesContractPosition target
    ) throws ServiceException {
    	target.setLineItemNumber(source.getLineItemNumber());
    	target.setName(source.getName());
    	target.setDescription(source.getDescription());
    	target.setPositionNumber(source.getPositionNumber());
    	target.setContractPositionState(source.getContractPositionState());
    	target.setQuantity(source.getQuantity());
    	target.setMinQuantity(source.getMinQuantity());
    	target.setMaxQuantity(source.getMaxQuantity());
    	target.setOffsetQuantity(source.getOffsetQuantity());    	
    	try { target.setMinMaxQuantityHandling(source.getMinMaxQuantityHandling()); } catch(Exception e) {}
    	target.setPricePerUnit(source.getPricePerUnit());
    	try { target.setPricingState(source.getPricingState()); } catch(Exception e) {}
    	target.setDiscount(source.getDiscount());
    	target.setDiscountDescription(source.getDiscountDescription());
    	target.setDiscountIsPercentage(source.isDiscountIsPercentage());
    	target.setSalesCommission(source.getSalesCommission());
    	target.setSalesCommissionIsPercentage(source.isSalesCommissionIsPercentage());
    	target.getContact().clear();
    	target.getContact().addAll(source.getContact());
    	target.setPriceUom(source.getPriceUom());
    	target.setUom(source.getUom());
    	target.setSalesTaxType(source.getSalesTaxType());    	
    	try { target.setListPrice(source.getListPrice()); } catch(Exception e) {}
    	target.setPricingRule(source.getPricingRule());
    	target.setPriceLevel(source.getPriceLevel());
    	target.setCalcRule(source.getCalcRule());
    	try { target.setShippingMethod(source.getShippingMethod()); } catch(Exception e) {}
    	target.setShippingTrackingNumber(source.getShippingTrackingNumber());
    	target.setShippingInstructions(source.getShippingInstructions());
    	try { target.setGift(source.isGift()); } catch(Exception e) {}
    	target.setGiftMessage(source.getGiftMessage());
    	target.setCarrier(source.getCarrier());
    	target.setSalesTransactionType(source.getSalesTransactionType());
    	if(source instanceof ConfiguredProduct && target instanceof ConfiguredProduct) {
    		((ConfiguredProduct)target).setProduct(((ConfiguredProduct)source).getProduct());
    		((ConfiguredProduct)target).setProductSerialNumber(((ConfiguredProduct)source).getProductSerialNumber());
    		((ConfiguredProduct)target).setConfigType(((ConfiguredProduct)source).getConfigType());
    	}
    	if(source instanceof CrxObject && target instanceof CrxObject) {
	    	copyCrxObject(
	    		(CrxObject)source,
	    		(CrxObject)target
	    	);
    	}
    	target.setOrigin(source);
    }

    /**
     * Get additional description matching the given language.
     * 
     * @param container
     * @param language
     * @return
     * @throws ServiceException
     */
    protected Description getAdditionalDescription(
        DescriptionContainer container,
        short language
    ) throws ServiceException {
        Collection<Description> descriptions = container.getAdditionalDescription();
        for(Description description: descriptions) {
        	if(description.getLanguage() == language) {
        		return description;
        	}
        }
        return null;
    }
    
    /**
     * Get default calculation rule.
     * 
     * @param contractSegment
     * @return
     * @throws ServiceException
     */
    public CalculationRule getDefaultCalculationRule(
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contractSegment);
    	CalculationRuleQuery query = (CalculationRuleQuery)pm.newQuery(CalculationRule.class);
    	query.thereExistsIsDefault().isTrue();
    	List<CalculationRule> calculationRules = contractSegment.getCalculationRule(query);
    	return calculationRules.isEmpty() ?
    		null :
    		calculationRules.iterator().next();
    }
    
    /**
     * Mark contract as closed.
     * 
     * @param contract
     * @param newContractState
     * @throws ServiceException
     */
    public void markAsClosed(
        AbstractContract contract,
        short newContractState
    ) throws ServiceException {
        contract.setClosedOn(
            new Date()
        );        
        contract.setContractState(
            newContractState
        );
    }
        
    /**
     * Get the uom scale factor for given position.
     * 
     * @param position
     * @return
     */
    public BigDecimal getUomScaleFactor(
        SalesContractPosition position
    ) {
        return (position.getUom() != null) && (position.getPriceUom() != null) ? 
        	Utils.getUomScaleFactor(position.getUom(), position.getPriceUom()) : 
        	BigDecimal.ONE;
    }

    /**
     * Get the price level for given position.
     * 
     * @param position
     * @return
     */
    protected AbstractPriceLevel getPriceLevel(
        SalesContractPosition position
    ) {
    	return position.getPriceLevel();
    }

    /**
     * Get the sales tax rate for the given position.
     * 
     * @param position
     * @return
     */
    public BigDecimal getSalesTaxRate(
        SalesContractPosition position
    ) {
        BigDecimal salesTaxRate = BigDecimal.ZERO;
        try {
            if(position.getSalesTaxType() != null) {
                if(position.getSalesTaxType().getRate() != null) {
                  salesTaxRate = position.getSalesTaxType().getRate();
                }
            }
        }
        catch(Exception e) {}
        return salesTaxRate;        
    }    

    /**
     * Get price per unit for given position.
     * 
     * @param position
     * @return
     */
    public BigDecimal getPricePerUnit(
    	SalesContractPosition position
    ) {
    	return position.getPricePerUnit() == null ?
            BigDecimal.ZERO :
            	position.getPricePerUnit();    	
    }

    protected javax.jmi.reflect.RefPackage getJmiPackage(
        PersistenceManager pm,
        String authorityXri
    ) {
    	Authority obj = pm.getObjectById(
            Authority.class,
            authorityXri
        );  
    	return obj.refOutermostPackage().refPackage(obj.refGetPath().getLastSegment().toString());
    }

    //-----------------------------------------------------------------------
    protected Contract1Package getContractPackage(
        PersistenceManager pm
    ) {
    	return (Contract1Package)getJmiPackage(
    		pm,
    		Contract1Package.AUTHORITY_XRI
    	);            
    }
    
    /**
     * Get adjusted quantity for given position.
     * 
     * @param position
     * @return
     */
    protected BigDecimal getMinMaxAdjustedQuantity(
        SalesContractPosition position
    ) {
        BigDecimal quantity = position.getQuantity() != null ? 
        	position.getQuantity() : 
        		BigDecimal.ZERO;
        BigDecimal minMaxAdjustedQuantity = quantity;
        BigDecimal minQuantity = position.getMinQuantity() != null ? 
        	position.getMinQuantity() : 
        		BigDecimal.ZERO;
        BigDecimal maxQuantity = position.getMaxQuantity() != null ? 
        	position.getMaxQuantity() : 
        		new BigDecimal(Double.MAX_VALUE);
        BigDecimal offsetQuantity = position.getOffsetQuantity() != null ?  
        	position.getOffsetQuantity() : 
        		BigDecimal.ZERO;        		
        short minMaxQuantityHandling = MIN_MAX_QUANTITY_HANDLING_NA;
        try {
            minMaxQuantityHandling = position.getMinMaxQuantityHandling(); 
        }  catch(Exception e) {}
        if(minMaxQuantityHandling == MIN_MAX_QUANTITY_HANDLING_LIMIT) {
            // Adjust min/max handling when quantity is negative
            if(quantity.compareTo(BigDecimal.ZERO) < 0) {
                minMaxAdjustedQuantity = minMaxAdjustedQuantity.add(offsetQuantity).max(maxQuantity.negate()).min(minQuantity.negate());              
            } else {
                minMaxAdjustedQuantity = minMaxAdjustedQuantity.subtract(offsetQuantity).max(minQuantity).min(maxQuantity);
            }
        }
        return minMaxAdjustedQuantity;
    }
    
    /**
     * Get position amounts. This method is called from the getPositionAmounts script
     * and delegates to getPositionAmountsInternal.
     * 
     * @param rootPkg
     * @param calculationRule
     * @param position
     * @param quantity
     * @param uomScaleFactor
     * @param salesTaxRate
     * @return
     */
    public static GetPositionAmountsResult getPositionAmounts(
        org.openmdx.base.accessor.jmi.cci.RefPackage_1_0 rootPkg,
        CalculationRule calculationRule,
        SalesContractPosition position,
        BigDecimal quantity,
        BigDecimal uomScaleFactor,
        BigDecimal salesTaxRate
    ) {
    	try {
	    	return Contracts.getInstance().getPositionAmountsInternal(
	    		calculationRule,
	    		position,
	    		Contracts.getInstance().getPricePerUnit(position),
	    		quantity,
	    		uomScaleFactor,
	    		position.getDiscount() != null ? position.getDiscount() : BigDecimal.ZERO,
	    		position.isDiscountIsPercentage(),
	    		position.getDiscountCalculationType(),
	    		salesTaxRate
	    	);
    	} catch(ServiceException e) {
    		throw new RuntimeServiceException(e);
    	}
    }

    /**
     * Get position amounts. This method is called from the getPositionAmounts script
     * and delegates to getPositionAmountsInternal.
     * 
     * @param rootPkg
     * @param calculationRule
     * @param position
     * @param pricePerUnit
     * @param quantity
     * @param uomScaleFactor
     * @param salesTaxRate
     * @return
     */
    public static GetPositionAmountsResult getPositionAmounts(
        org.openmdx.base.accessor.jmi.cci.RefPackage_1_0 rootPkg,
        CalculationRule calculationRule,
        SalesContractPosition position,
        BigDecimal pricePerUnit,
        BigDecimal quantity,
        BigDecimal uomScaleFactor,
        BigDecimal discount,
        Boolean discountIsPercentage,
        Short discountCalculationType,
        BigDecimal salesTaxRate
    ) {
    	try {
	    	return Contracts.getInstance().getPositionAmountsInternal(
	    		calculationRule,
	    		position,
	    		pricePerUnit,
	    		quantity,
	    		uomScaleFactor,
	    		discount,
	    		discountIsPercentage,
	    		discountCalculationType,
	    		salesTaxRate
	    	);
    	} catch(ServiceException e) {
    		throw new RuntimeServiceException(e);
    	}
    }

    /**
     * Get position amounts. Delegate to CalculationRule::getPositionAmounts script.
     * 
     * @param calculationRule
     * @param position
     * @param overridePricePerUnit
     * @param overrideQuantity
     * @param overrideUomScaleFactor
     * @param overrideDiscount
     * @param overrideDiscountIsPercentage
     * @param overrideSalesTaxRate
     * @return
     * @throws ServiceException
     */
    public GetPositionAmountsResult getPositionAmounts(
        CalculationRule calculationRule,
        SalesContractPosition position,
        BigDecimal overridePricePerUnit,
        BigDecimal overrideQuantity,
        BigDecimal overrideUomScaleFactor,
        BigDecimal overrideDiscount,
        Boolean overrideDiscountIsPercentage,
        Short overrideDiscountCalculationType,
        BigDecimal overrideSalesTaxRate
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(calculationRule);
        String script = (calculationRule.getGetPositionAmountsScript() == null) || (calculationRule.getGetPositionAmountsScript().length() == 0) ? 
            DEFAULT_GET_POSITION_AMOUNTS_SCRIPT : 
            calculationRule.getGetPositionAmountsScript();
        org.opencrx.kernel.contract1.jmi1.Contract1Package contractPkg = this.getContractPackage(pm); 
        try {
        	Class<?> clazz = ScriptUtils.getClass(script);
        	Method getPositionAmountMethod1 = null;
        	Method getPositionAmountMethod2 = null;
        	// Signature 1
        	try {
	            getPositionAmountMethod1 = clazz.getMethod(
	                "getPositionAmounts", 
	                new Class[] {
	                    org.openmdx.base.accessor.jmi.cci.RefPackage_1_0.class,
	                    CalculationRule.class, 
	                    SalesContractPosition.class,
	                    BigDecimal.class, // quantity
	                    BigDecimal.class, // uomScaleFactor
	                    BigDecimal.class // salesTaxRate
	                }
	            );
        	} catch(NoSuchMethodException ignore) {}
        	// Signature 2
        	try {
	            getPositionAmountMethod2 = clazz.getMethod(
	                "getPositionAmounts", 
	                new Class[] {
	                    org.openmdx.base.accessor.jmi.cci.RefPackage_1_0.class,
	                    CalculationRule.class, 
	                    SalesContractPosition.class,
	                    BigDecimal.class, // pricePerUnit
	                    BigDecimal.class, // quantity
	                    BigDecimal.class, // uomScaleFactor
	                    BigDecimal.class, // discount
	                    Boolean.class, // discountIsPercentage
	                    Short.class, // discountCalculationType
	                    BigDecimal.class // salesTaxRate
	                }
	            );
        	} catch(NoSuchMethodException ignore) {}
        	if(getPositionAmountMethod1 != null) {
	            org.opencrx.kernel.contract1.jmi1.GetPositionAmountsResult result = 
	                (org.opencrx.kernel.contract1.jmi1.GetPositionAmountsResult)getPositionAmountMethod1.invoke(
	                    null, 
	                    new Object[] {
	                        contractPkg.refOutermostPackage(),
	                        calculationRule,
	                        position,
	                        overrideQuantity == null ? this.getMinMaxAdjustedQuantity(position) : overrideQuantity,
	                        overrideUomScaleFactor == null ? this.getUomScaleFactor(position) : overrideUomScaleFactor,
	                        overrideSalesTaxRate == null ? this.getSalesTaxRate(position) : overrideSalesTaxRate
	                    }
	                );
	            return result;
        	} else if(getPositionAmountMethod2 != null) {
	            org.opencrx.kernel.contract1.jmi1.GetPositionAmountsResult result = 
	                (org.opencrx.kernel.contract1.jmi1.GetPositionAmountsResult)getPositionAmountMethod2.invoke(
	                    null,
	                    new Object[] {
	                        contractPkg.refOutermostPackage(),
	                        calculationRule,
	                        position,
	                        overridePricePerUnit == null ? this.getPricePerUnit(position) : overridePricePerUnit,
	                        overrideQuantity == null ? this.getMinMaxAdjustedQuantity(position) : overrideQuantity,
	                        overrideUomScaleFactor == null ? this.getUomScaleFactor(position) : overrideUomScaleFactor,
		                    overrideDiscount == null ? position.getDiscount() : overrideDiscount,
				            overrideDiscount == null ? position.isDiscountIsPercentage() : overrideDiscountIsPercentage,
				            overrideDiscountCalculationType == null ? position.getDiscountCalculationType() : overrideDiscountCalculationType,
	                        overrideSalesTaxRate == null ? this.getSalesTaxRate(position) : overrideSalesTaxRate                       	
	                    }
	                );
	            return result;
        	} else {
                return Structures.create(
                	GetPositionAmountsResult.class,
                	Datatypes.member(GetPositionAmountsResult.Member.statusCode, STATUS_CODE_ERROR),
                	Datatypes.member(GetPositionAmountsResult.Member.statusMessage, "getPositionAmounts does not define a method with the following signature:\n" + DEFAULT_GET_POSITION_AMOUNTS_SCRIPT)
                );
        	}
        } catch(InvocationTargetException e) {
            return Structures.create(
            	GetPositionAmountsResult.class,
            	Datatypes.member(GetPositionAmountsResult.Member.statusCode, STATUS_CODE_ERROR),
            	Datatypes.member(GetPositionAmountsResult.Member.statusMessage, "Can not invoke getPositionAmounts:\n" + e.getTargetException().getMessage())
            );
        } catch(IllegalAccessException e) {
            return Structures.create(
            	GetPositionAmountsResult.class,
            	Datatypes.member(GetPositionAmountsResult.Member.statusCode, STATUS_CODE_ERROR),
            	Datatypes.member(GetPositionAmountsResult.Member.statusMessage, "Illegal access when invoking getPositionAmounts:\n" + e.getMessage())
            );
        } catch(Exception e) {
            return Structures.create(
            	GetPositionAmountsResult.class,
            	Datatypes.member(GetPositionAmountsResult.Member.statusCode, STATUS_CODE_ERROR),
            	Datatypes.member(GetPositionAmountsResult.Member.statusMessage, "Error parsing getPositionAmountsScript:\n" + e.getMessage())
            );
        }
    }

    /**
     * Get contract amounts.
     * 
     * @param calculationRule
     * @param contract
     * @param lineItemNumbers
     * @param positionBaseAmounts
     * @param positionDiscountAmounts
     * @param positionTaxAmounts
     * @param positionAmounts
     * @param salesCommissions
     * @param salesCommissionIsPercentages
     * @return
     * @throws ServiceException
     */
    public GetContractAmountsResult getContractAmounts(
        CalculationRule calculationRule,
        SalesContract contract,
        List<?> lineItemNumbers,
        List<?> positionBaseAmounts,
        List<?> positionDiscountAmounts,
        List<?> positionTaxAmounts,
        List<?> positionAmounts,
        List<?> salesCommissions,
        List<?> salesCommissionIsPercentages
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(calculationRule);
        String script = (calculationRule.getGetContractAmountsScript() == null) || (calculationRule.getGetContractAmountsScript().length() == 0) ? 
            DEFAULT_GET_CONTRACT_AMOUNTS_SCRIPT : 
            calculationRule.getGetContractAmountsScript();
        org.opencrx.kernel.contract1.jmi1.Contract1Package contractPkg = this.getContractPackage(pm); 
        try {
        	Class<?> clazz = ScriptUtils.getClass(script);
            Method getContractAmountMethod = clazz.getMethod(
                "getContractAmounts", 
                new Class[] {
                    org.openmdx.base.accessor.jmi.cci.RefPackage_1_0.class,
                    CalculationRule.class, 
                    SalesContract.class,
                    Integer[].class,
                    BigDecimal[].class,
                    BigDecimal[].class,
                    BigDecimal[].class,
                    BigDecimal[].class,
                    BigDecimal[].class,
                    Boolean[].class
                }
            );
            org.opencrx.kernel.contract1.jmi1.GetContractAmountsResult result = 
                (org.opencrx.kernel.contract1.jmi1.GetContractAmountsResult)getContractAmountMethod.invoke(
                    null, 
                    new Object[] {
                        contractPkg.refOutermostPackage(),
                        calculationRule,
                        contract,
                        lineItemNumbers.toArray(new Integer[lineItemNumbers.size()]),
                        positionBaseAmounts.toArray(new BigDecimal[positionBaseAmounts.size()]),
                        positionDiscountAmounts.toArray(new BigDecimal[positionDiscountAmounts.size()]),
                        positionTaxAmounts.toArray(new BigDecimal[positionTaxAmounts.size()]),
                        positionAmounts.toArray(new BigDecimal[positionAmounts.size()]),
                        salesCommissions.toArray(new BigDecimal[salesCommissions.size()]),
                        salesCommissionIsPercentages.toArray(new Boolean[salesCommissionIsPercentages.size()])
                    }
                );
            return result;
        } catch(NoSuchMethodException e) {
            return contractPkg.createGetContractAmountsResult(
                STATUS_CODE_ERROR,
                "getContractAmounts does not define a method with the following signature:\n" +
                DEFAULT_GET_CONTRACT_AMOUNTS_SCRIPT,
                null, null, null, null, null, null
            );
        } catch(InvocationTargetException e) {
            return contractPkg.createGetContractAmountsResult(
                STATUS_CODE_ERROR,
                "Can not invoke getContractAmounts:\n" + e.getTargetException().getMessage(),
                null, null, null, null, null, null
            );
        } catch(IllegalAccessException e) {
            return contractPkg.createGetContractAmountsResult(
                STATUS_CODE_ERROR,
                "Illegal access when invoking getContractAmounts:\n" + e.getMessage(),
                null, null, null, null, null, null
            );
        } catch(Exception e) {
            return contractPkg.createGetContractAmountsResult(
                STATUS_CODE_ERROR,
                "Error parsing getContractAmountsScript:\n" + e.getMessage(),
                null, null, null, null, null, null
            );
        }
    }

    /**
     * Calculate amounts for given position.
     * 
     * @param position
     * @return
     * @throws ServiceException
     */
    public BigDecimal[] calculateAmounts(
    	SalesContractPosition position
    ) throws ServiceException {      
        // baseAmount, discountAmount         
        BigDecimal baseAmount = null;
        BigDecimal discountAmount = null;
        BigDecimal amount = null;
        BigDecimal taxAmount = null;
        BigDecimal pricePerUnit = position.getPricePerUnit() == null ? 
            BigDecimal.ZERO :
            position.getPricePerUnit();
        PersistenceManager pm = JDOHelper.getPersistenceManager(position);   
        String providerName = position.refGetPath().getSegment(2).toString();
        String segmentName = position.refGetPath().getSegment(4).toString();
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
        	pm,
        	providerName, 
        	segmentName
        );
        SalesContract contract = (SalesContract)pm.getObjectById(
    		position.refGetPath().getParent().getParent()
    	);
        // Get all positions in one roundtrip. Read up to the last element
        BigDecimal minMaxAdjustedQuantity = this.getMinMaxAdjustedQuantity(position);
        // Calculation rule with first prio from position, then contract, then default
        CalculationRule calculationRule = position.getCalcRule() == null ?
        	contract.getCalcRule() :
        	position.getCalcRule();
        if(calculationRule == null) {
        	calculationRule = this.getDefaultCalculationRule(contractSegment);
        }
        if(calculationRule != null) {
            GetPositionAmountsResult result = this.getPositionAmounts(
                calculationRule,
                position,
                null, // overridePricePerUnit
                null, // overrideQuantity
                null, // overrideUomScaleFactor
                null, // overrideDiscount
                null, // overrideDiscountIsPercentage
                null, // overrideDiscountCalculationType
                null // overrideSalesTaxRate
            );
            if(result.getStatusCode() != STATUS_CODE_OK) {
                throw new ServiceException(
                    BasicException.Code.DEFAULT_DOMAIN,
                    BasicException.Code.PROCESSING_FAILURE,
                    "Unable to calculate position amounts",
                    new BasicException.Parameter("result", result)
                );
            }
            baseAmount = result.getBaseAmount();
            discountAmount = result.getDiscountAmount();   
            taxAmount = result.getTaxAmount();
            amount = result.getAmount();   
        }
        // No calculation rule. Fall back to default amount calculation
        else {
            BigDecimal uomScaleFactor = this.getUomScaleFactor(position);
            BigDecimal salesTaxRate = this.getSalesTaxRate(position);              
            baseAmount = minMaxAdjustedQuantity.multiply(pricePerUnit.multiply(uomScaleFactor));
            // discount
            Boolean discountIsPercentage = position.isDiscountIsPercentage() == null ? 
                Boolean.FALSE : 
                position.isDiscountIsPercentage();
            BigDecimal discount = position.getDiscount() == null ? 
                BigDecimal.ZERO : 
               position.getDiscount();
            // Discount is per piece in case of !discountIsPercentage
            discountAmount = discountIsPercentage.booleanValue() ? 
                baseAmount.multiply(discount.divide(HUNDRED, BigDecimal.ROUND_UP)) : 
                minMaxAdjustedQuantity.multiply(discount.multiply(uomScaleFactor));
            // taxAmount
            taxAmount = baseAmount.subtract(discountAmount).multiply(
                salesTaxRate.divide(HUNDRED, BigDecimal.ROUND_UP)
            );    
            // amount
            amount = baseAmount.subtract(discountAmount).add(taxAmount);      
        }        
        return new BigDecimal[]{
        	baseAmount, 
        	discountAmount, 
        	amount, 
        	taxAmount
        };
    }
    
    //-------------------------------------------------------------------------    
    public BigDecimal[] calculateQuantities(
    	SalesContractPosition position
    ) {
        BigDecimal minMaxAdjustedQuantity = this.getMinMaxAdjustedQuantity(position);
        Collection<DeliveryInformation> deliveryInformations = position.getDeliveryInformation();
        BigDecimal quantityShipped = BigDecimal.ZERO;
        for(DeliveryInformation deliveryInformation: deliveryInformations) {
        	if(deliveryInformation.getQuantityShipped() != null) {
        		quantityShipped = quantityShipped.add(deliveryInformation.getQuantityShipped());
        	}
        }
        return new BigDecimal[]{
        	quantityShipped, 
        	minMaxAdjustedQuantity.subtract(quantityShipped)
        };        
    }

    //-------------------------------------------------------------------------    
    public String[] calculateUomDescriptions(
    	SalesContractPosition position
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
       AbstractContract contract = (AbstractContract)pm.getObjectById(
    		position.refGetPath().getParent().getParent()
    	);    	
    	short contractLanguage = contract.getContractLanguage();
    	String description = "N/A";
    	String detailedDescription = "N/A";
        if(position.getUom() != null) {
            try {
                description = position.getUom().getDescription();
                detailedDescription = position.getUom().getDetailedDescription();
                Description additionalDescription = this.getAdditionalDescription(
                    position.getUom(),
                    contractLanguage
                );
                if(additionalDescription != null) {
                    description = additionalDescription.getDescription();
                    detailedDescription = additionalDescription.getDetailedDescription();
                }
            }
            catch(Exception e) {
                new ServiceException(e).log();
                description = "#ERR";
                detailedDescription = "#ERR";         
            }
        }
        return new String[]{
        	description,
        	detailedDescription
        };
    }

    //-------------------------------------------------------------------------    
    public String[] calculatePriceUomDescriptions(
    	SalesContractPosition position
    ) {    	
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
        AbstractContract contract = (AbstractContract)pm.getObjectById(
    		position.refGetPath().getParent().getParent()
    	);    	
    	short contractLanguage = contract.getContractLanguage();
    	String description = "N/A";
    	String detailedDescription = "N/A";
        if(position.getPriceUom() != null) {
            try {
                description = position.getPriceUom().getDescription();
                detailedDescription = position.getPriceUom().getDetailedDescription();
                Description additionalDescription = this.getAdditionalDescription(
                    position.getPriceUom(),
                    contractLanguage
                );
                if(additionalDescription != null) {
                    description = additionalDescription.getDescription();
                    detailedDescription = additionalDescription.getDetailedDescription();
                }
            }
            catch(Exception e) {
                new ServiceException(e).log();
                description = "#ERR";
                detailedDescription = "#ERR";         
            }
        }
        return new String[]{
        	description,
        	detailedDescription
        };
    }

    /**
     * Calculate product descriptions.
     * 
     * @param position
     * @return
     */
    public String[] calculateProductDescriptions(
    	SalesContractPosition position
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
        AbstractContract contract = (AbstractContract)pm.getObjectById(
    		position.refGetPath().getParent().getParent()
    	);    	
    	short contractLanguage = contract.getContractLanguage();
    	String description = "N/A";
    	String detailedDescription = "N/A";
        if(
        	(position instanceof ConfiguredProduct) &&
        	(((ConfiguredProduct)position).getProduct() != null)
        ) {
            try {
            	Product product = ((ConfiguredProduct)position).getProduct();
                description = product.getDescription();
                detailedDescription = product.getDetailedDescription();
                Description additionalDescription = this.getAdditionalDescription(
                	product,
                    contractLanguage
                );
                if(additionalDescription != null) {
                    description = additionalDescription.getDescription();
                    detailedDescription = additionalDescription.getDetailedDescription();
                }
            } catch(Exception e) {
                new ServiceException(e).log();
                description = "#ERR";
                detailedDescription = "#ERR";         
            }
        }
        return new String[]{
        	description,
        	detailedDescription
        };
    }
    
    /**
     * Calculate descriptions for sales tax types.
     * 
     * @param position
     * @return
     */
    public String[] calculateSalesTaxTypeDescriptions(
    	SalesContractPosition position
    ) {    	
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
        AbstractContract contract = (AbstractContract)pm.getObjectById(
    		position.refGetPath().getParent().getParent()
    	);    	
    	short contractLanguage = contract.getContractLanguage();
    	String description = "N/A";
    	String detailedDescription = "N/A";
        if(position.getSalesTaxType() != null) {
            try {
                description = position.getSalesTaxType().getDescription();
                detailedDescription = position.getSalesTaxType().getDetailedDescription();
                Description additionalDescription = this.getAdditionalDescription(
                    position.getSalesTaxType(),
                    contractLanguage
                );
                if(additionalDescription != null) {
                    description = additionalDescription.getDescription();
                    detailedDescription = additionalDescription.getDetailedDescription();
                }
            } catch(Exception e) {
                new ServiceException(e).log();
                description = "#ERR";
                detailedDescription = "#ERR";         
            }
        }
        return new String[]{
        	description,
        	detailedDescription
        };
    }
    
    /**
     * Mark contract as dirty, i.e. touch contract.
     * 
     * @param contract
     * @throws ServiceException
     */
    public void markContractAsDirty(
        AbstractContract contract
    ) throws ServiceException {
    	Utils.touchObject(contract);
    }
    
    /**
     * Get sales contract positions for given sales contract.
     * 
     * @param contract
     * @return
     */
    public List<SalesContractPosition> getSalesContractPositions(
    	SalesContract contract
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
        List<SalesContractPosition> positions = new ArrayList<SalesContractPosition>();
        if(contract instanceof Opportunity) {
        	AbstractOpportunityPositionQuery query = (AbstractOpportunityPositionQuery)pm.newQuery(AbstractOpportunityPosition.class);
        	query.orderByLineItemNumber().ascending();
        	Collection<AbstractOpportunityPosition> p = ((Opportunity)contract).getPosition(query);
        	positions.addAll(p);
        } else if(contract instanceof Quote) {
        	AbstractQuotePositionQuery query = (AbstractQuotePositionQuery)pm.newQuery(AbstractQuotePosition.class);
        	query.orderByLineItemNumber().ascending();
        	Collection<AbstractQuotePosition> p = ((Quote)contract).getPosition(query);
        	positions.addAll(p);
        } else if(contract instanceof SalesOrder) {
        	AbstractSalesOrderPositionQuery query = (AbstractSalesOrderPositionQuery)pm.newQuery(AbstractSalesOrderPosition.class);
        	query.orderByLineItemNumber().ascending();
        	Collection<AbstractSalesOrderPosition> p = ((SalesOrder)contract).getPosition(query);
        	positions.addAll(p);
        } else if(contract instanceof Invoice) {
        	AbstractInvoicePositionQuery query = (AbstractInvoicePositionQuery)pm.newQuery(AbstractInvoicePosition.class);
        	query.orderByLineItemNumber().ascending();
        	Collection<AbstractInvoicePosition> p = ((Invoice)contract).getPosition(query);
        	positions.addAll(p);
        }
        return positions;
    }
    
    /**
     * Calculate amounts for given contract.
     * 
     * @param contract
     * @return
     * @throws ServiceException
     */
    public BigDecimal[] calculateAmounts(
        SalesContract contract
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
    	String providerName = contract.refGetPath().getSegment(2).toString();
    	String segmentName = contract.refGetPath().getSegment(4).toString();    	
    	org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
    		pm,
    		providerName, 
    		segmentName
    	);
        BigDecimal totalBaseAmount = null;
        BigDecimal totalDiscountAmount = null;
        BigDecimal totalTaxAmount = null;                
        BigDecimal totalAmount = null;
        BigDecimal totalAmountIncludingTax = null;
        BigDecimal totalSalesCommission = null;
        if(contract instanceof Lead) {
        	Lead lead = (Lead)contract;
        	totalBaseAmount = lead.getEstimatedValue();
        	totalBaseAmount = totalBaseAmount == null ? BigDecimal.ZERO : totalBaseAmount;
        	totalDiscountAmount = BigDecimal.ZERO;
        	totalTaxAmount = BigDecimal.ZERO;
        	totalAmount = totalBaseAmount;        	
        	totalAmountIncludingTax = totalBaseAmount;
        	totalSalesCommission = BigDecimal.ZERO;
        } else {
        	List<SalesContractPosition> positions = this.getSalesContractPositions(contract);
	        List<Integer> lineItemNumbers = new ArrayList<Integer>();
	        List<BigDecimal> positionBaseAmounts = new ArrayList<BigDecimal>();
	        List<BigDecimal> positionDiscountAmounts = new ArrayList<BigDecimal>();
	        List<BigDecimal> positionTaxAmounts = new ArrayList<BigDecimal>();
	        List<BigDecimal> positionAmounts = new ArrayList<BigDecimal>();
	        List<BigDecimal> salesCommissions = new ArrayList<BigDecimal>();
	        List<Boolean> salesCommissionIsPercentages = new ArrayList<Boolean>();
	        for(SalesContractPosition position: positions) {
	            BigDecimal[] amounts = this.calculateAmounts(
	                position
	            );
	            lineItemNumbers.add(
	                (int)position.getLineItemNumber()
	            );
	            positionBaseAmounts.add(
	            	amounts[0]
	            );
	            positionDiscountAmounts.add(
	            	amounts[1]
	            );
	            positionAmounts.add(
	            	amounts[2]
	            );
	            positionTaxAmounts.add(
	            	amounts[3]
	            );
	            salesCommissions.add(
	                position.getSalesCommission() == null ? 
	                    BigDecimal.ZERO : 
	                    position.getSalesCommission()
	            );
	            salesCommissionIsPercentages.add(
	                position.isDiscountIsPercentage() == null ?
	                    Boolean.FALSE : 
	                    position.isSalesCommissionIsPercentage()
	            );
	        }
	        // To amount calculation using calculation rule if defined        
	        CalculationRule calculationRule = contract.getCalcRule() == null ?
	        	this.getDefaultCalculationRule(contractSegment) :
	        	contract.getCalcRule();
	        if(calculationRule != null) {
	            GetContractAmountsResult result = this.getContractAmounts(
	                calculationRule,
	                contract,
	                lineItemNumbers,
	                positionBaseAmounts,
	                positionDiscountAmounts,
	                positionTaxAmounts,
	                positionAmounts,
	                salesCommissions,
	                salesCommissionIsPercentages
	            );
	            if(result.getStatusCode() != STATUS_CODE_OK) {
	                throw new ServiceException(
	                    BasicException.Code.DEFAULT_DOMAIN,
	                    BasicException.Code.PROCESSING_FAILURE,
	                    "Unable to calculate contract amounts",
	                    new BasicException.Parameter("result", result)
	                );
	            }
	            totalBaseAmount = result.getTotalBaseAmount();
	            totalDiscountAmount = result.getTotalDiscountAmount();   
	            totalTaxAmount = result.getTotalTaxAmount();
	            totalAmount = result.getTotalAmount();   
	            totalAmountIncludingTax = result.getTotalAmountIncludingTax();
	            totalSalesCommission = result.getTotalSalesCommission();   
	        } else {
		        // To default amount calculation if no calculation rule is defined
	            totalBaseAmount = BigDecimal.ZERO;
	            totalDiscountAmount = BigDecimal.ZERO;
	            totalTaxAmount = BigDecimal.ZERO;
	            totalSalesCommission = BigDecimal.ZERO;
	            for(int i = 0; i < positionBaseAmounts.size(); i++) {
	                totalBaseAmount = totalBaseAmount.add(
	                  positionBaseAmounts.get(i)
	                );
	                BigDecimal discountAmount = positionDiscountAmounts.get(i);
	                totalDiscountAmount = totalDiscountAmount.add(discountAmount);
	                totalTaxAmount = totalTaxAmount.add(
	                  positionTaxAmounts.get(i)
	                );
	                BigDecimal salesCommission = salesCommissions.get(i) != null ? 
	                    (BigDecimal) salesCommissions.get(i) : 
	                    BigDecimal.ZERO;
	                BigDecimal baseAmount = positionBaseAmounts.get(i);
	                totalSalesCommission = totalSalesCommission.add(
	                  (salesCommissionIsPercentages.get(i) != null) &&
	                  ((salesCommissionIsPercentages.get(i)).booleanValue()) ? 
	                	  baseAmount.subtract(discountAmount).multiply(salesCommission.divide(HUNDRED, BigDecimal.ROUND_UP)) : 
	                	  salesCommission
	                );
	            }
	            totalAmount = totalBaseAmount.subtract(totalDiscountAmount);
	            totalAmountIncludingTax = totalAmount.add(totalTaxAmount);
	        }
        }
        return new BigDecimal[]{
        	totalBaseAmount,
        	totalDiscountAmount,
        	totalAmount,
        	totalTaxAmount,
        	totalAmountIncludingTax,
        	totalSalesCommission
        };
    }

    /**
     * Update contract callback. Override for custom-specific behaviour.
     * 
     * @param contract
     * @throws ServiceException
     */
    protected void updateContract(
    	AbstractContract contract
    ) throws ServiceException {
    	if(JDOHelper.isNew(contract)) {
    		Base.getInstance().assignToMe(
    			contract, 
    			false, // overwrite
    			false // useRunAsPrincipal
    		);
    	}
    }

    /**
     * Update sales contract callback.
     * 
     * @param contract
     * @throws ServiceException
     */
    protected void updateSalesContract(
    	SalesContract contract
    ) throws ServiceException {
    	if(JDOHelper.isNew(contract)) {
    		contract.setPricingState(PRICING_STATE_NA);
    	}
    	if(!Boolean.TRUE.equals(contract.isNoAutoRecalc())) {
    		this.recalcSalesContract(contract);
    	}
    }

    /**
     * Recalc sales contract.
     * 
     * @param contract
     * @throws ServiceException
     */
    public void recalcSalesContract(
    	SalesContract contract
    ) throws ServiceException {
		BigDecimal[] amounts = Contracts.getInstance().calculateAmounts(contract);
		if(!Utils.areEqual(contract.getTotalBaseAmount(), amounts[0])) {
			contract.setTotalBaseAmount(amounts[0]);
		}
		if(!Utils.areEqual(contract.getTotalDiscountAmount(), amounts[1])) {
			contract.setTotalDiscountAmount(amounts[1]);
		}
		if(!Utils.areEqual(contract.getTotalAmount(), amounts[2])) {
			contract.setTotalAmount(amounts[2]);
		}
		if(!Utils.areEqual(contract.getTotalTaxAmount(), amounts[3])) {		
			contract.setTotalTaxAmount(amounts[3]);
		}
		if(!Utils.areEqual(contract.getTotalAmountIncludingTax(), amounts[4])) {		
			contract.setTotalAmountIncludingTax(amounts[4]);
		}
		if(!Utils.areEqual(contract.getTotalSalesCommission(), amounts[5])) {		
			contract.setTotalSalesCommission(amounts[5]);
		}
    }

    /**
     * Create a new invoice based on the given sales order.
     * 
     * @param salesOrder
     * @return
     * @throws ServiceException
     */
    public Invoice createInvoice(
        SalesOrder salesOrder
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(salesOrder);
    	String providerName = salesOrder.refGetPath().getSegment(2).toString();
    	String segmentName = salesOrder.refGetPath().getSegment(4).toString();
    	Map<String,Marshaller> objectMarshallers = new HashMap<String,Marshaller>();
    	objectMarshallers.put(
    		"org:opencrx:kernel:contract1:SalesOrder",
    		new Marshaller() {
    			public Object marshal(
    				Object s
    			) throws ServiceException {
    				SalesOrder salesOrder = (SalesOrder)s;
    				PersistenceManager pm = JDOHelper.getPersistenceManager(salesOrder);
    				Invoice invoice = pm.newInstance(Invoice.class);
    				Contracts.getInstance().copySalesContract(
    					salesOrder,
    					invoice
    				);
    				return invoice;                       
    			}
    			public Object unmarshal(Object s) {
    				throw new UnsupportedOperationException();
    			}
    		}
    	);
    	objectMarshallers.put(
    		"org:opencrx:kernel:contract1:SalesOrderPosition",
    		new Marshaller() {
    			public Object marshal(
    				Object s
    			) throws ServiceException {
    				SalesOrderPosition salesOrderPosition = (SalesOrderPosition)s;
    				PersistenceManager pm = JDOHelper.getPersistenceManager(salesOrderPosition);
    				InvoicePosition invoicePosition = pm.newInstance(InvoicePosition.class);
    				Contracts.getInstance().copySalesContractPosition(
    					salesOrderPosition,
    					invoicePosition
    				);
    				return invoicePosition;
    			}
    			public Object unmarshal(Object s) {
    				throw new UnsupportedOperationException();
    			}
    		}
    	);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
            pm, 
            providerName, 
            segmentName
        );
        Invoice invoice = (Invoice)Cloneable.getInstance().cloneObject(
        	salesOrder, 
        	contractSegment, 
        	"invoice", 
        	objectMarshallers, 
        	null, // reference filter
        	null, // owning user
        	null // owning group
        );
        invoice.setOrigin(salesOrder);
        if(invoice.getSalesRep() == null) {
            Base.getInstance().assignToMe(
                invoice,
                true, // overwrite
                false // useRunAsPrincipal
            );
        }
        return invoice;
    }
    
    /**
     * Create a new sales order based on the given quote.
     * 
     * @param quote
     * @return
     * @throws ServiceException
     */
    public SalesOrder createSalesOrder(
        Quote quote
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(quote);
    	String providerName = quote.refGetPath().getSegment(2).toString();
    	String segmentName = quote.refGetPath().getSegment(4).toString();
    	Map<String,Marshaller> objectMarshallers = new HashMap<String,Marshaller>();
    	objectMarshallers.put(
    		"org:opencrx:kernel:contract1:Quote",
    		new Marshaller() {
    			public Object marshal(
    				Object s
    			) throws ServiceException {
    				Quote quote = (Quote)s;
    				PersistenceManager pm = JDOHelper.getPersistenceManager(quote);
    				SalesOrder salesOrder = pm.newInstance(SalesOrder.class);
    				Contracts.getInstance().copySalesContract(
    					quote,
    					salesOrder
    				);
    				return salesOrder;                       
    			}
    			public Object unmarshal(Object s) {
    				throw new UnsupportedOperationException();
    			}
    		}
    	);
    	objectMarshallers.put(
    		"org:opencrx:kernel:contract1:QuotePosition",
    		new Marshaller() {
    			public Object marshal(
    				Object s
    			) throws ServiceException {
    				QuotePosition quotePosition = (QuotePosition)s;
    				PersistenceManager pm = JDOHelper.getPersistenceManager(quotePosition);
    				SalesOrderPosition salesOrderPosition = pm.newInstance(SalesOrderPosition.class);
    				Contracts.getInstance().copySalesContractPosition(
    					quotePosition,
    					salesOrderPosition
    				);
    				return salesOrderPosition;
    			}
    			public Object unmarshal(Object s) {
    				throw new UnsupportedOperationException();
    			}
    		}
    	);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
            pm, 
            providerName, 
            segmentName
        );
        SalesOrder salesOrder = (SalesOrder)Cloneable.getInstance().cloneObject(
        	quote, 
        	contractSegment, 
        	"salesOrder", 
        	objectMarshallers, 
        	null, // reference filter
        	null, // owning user
        	null // owning group
        );
        salesOrder.setOrigin(quote);
        if(salesOrder.getSalesRep() == null) {
            Base.getInstance().assignToMe(
                salesOrder,
                true, // overwrite
                false // useRunAsPrincipal
            );
        }
        return salesOrder;
    }

    /**
     * Create a new quote based on the given opportunity.
     * 
     * @param opportunity
     * @return
     * @throws ServiceException
     */
    public Quote createQuote(
        Opportunity opportunity
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(opportunity);
    	String providerName = opportunity.refGetPath().getSegment(2).toString();
    	String segmentName = opportunity.refGetPath().getSegment(4).toString();
    	Map<String,Marshaller> objectMarshallers = new HashMap<String,Marshaller>();
    	objectMarshallers.put(
    		"org:opencrx:kernel:contract1:Opportunity",
    		new Marshaller() {
    			public Object marshal(
    				Object s
    			) throws ServiceException {
    				Opportunity opportunity = (Opportunity)s;
    				PersistenceManager pm = JDOHelper.getPersistenceManager(opportunity);
    				Quote quote = pm.newInstance(Quote.class);
    				Contracts.getInstance().copySalesContract(
    					opportunity,
    					quote
    				);
    				return quote;                       
    			}
    			public Object unmarshal(Object s) {
    				throw new UnsupportedOperationException();
    			}
    		}
    	);
    	objectMarshallers.put(
    		"org:opencrx:kernel:contract1:OpportunityPosition",
    		new Marshaller() {
    			public Object marshal(
    				Object s
    			) throws ServiceException {
    				OpportunityPosition opportunityPosition = (OpportunityPosition)s;
    				PersistenceManager pm = JDOHelper.getPersistenceManager(opportunityPosition);
    				QuotePosition quotePosition = pm.newInstance(QuotePosition.class);
    				Contracts.getInstance().copySalesContractPosition(
    					opportunityPosition,
    					quotePosition
    				);
    				return quotePosition;
    			}
    			public Object unmarshal(Object s) {
    				throw new UnsupportedOperationException();
    			}
    		}
    	);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
            pm, 
            providerName, 
            segmentName
        );
        Quote quote = (Quote)Cloneable.getInstance().cloneObject(
        	opportunity, 
        	contractSegment, 
        	"quote", 
        	objectMarshallers, 
        	null, // reference filter
        	null, // owning user
        	null // owning group
        );
        quote.setOrigin(opportunity);
        if(quote.getSalesRep() == null) {
            Base.getInstance().assignToMe(
            	quote,
                true, // overwrite
                false // useRunAsPrincipal
            );
        }
        return quote;
    }
    
    /**
     * Create a new opportunity based on the given lead.
     * 
     * @param lead
     * @return
     * @throws ServiceException
     */
    public Opportunity createOpportunity(
        Lead lead
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(lead);
    	String providerName = lead.refGetPath().getSegment(2).toString();
    	String segmentName = lead.refGetPath().getSegment(4).toString();
    	Map<String,Marshaller> objectMarshallers = new HashMap<String,Marshaller>();
    	objectMarshallers.put(
    		"org:opencrx:kernel:contract1:Lead",
    		new Marshaller() {
    			public Object marshal(
    				Object s
    			) throws ServiceException {
    				Lead lead = (Lead)s;
    				PersistenceManager pm = JDOHelper.getPersistenceManager(lead);
    				Opportunity opportunity = pm.newInstance(Opportunity.class);
    				Contracts.getInstance().copySalesContract(
    					lead,
    					opportunity
    				);
    				return opportunity;                       
    			}
    			public Object unmarshal(Object s) {
    				throw new UnsupportedOperationException();
    			}
    		}
    	);
        org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
            pm, 
            providerName, 
            segmentName
        );
        Opportunity opportunity = (Opportunity)Cloneable.getInstance().cloneObject(
        	lead, 
        	contractSegment, 
        	"opportunity", 
        	objectMarshallers, 
        	null, // reference filter
        	null, // owning user
        	null // owning group
        );
        opportunity.setOrigin(lead);
        if(opportunity.getSalesRep() == null) {
            Base.getInstance().assignToMe(
            	opportunity,
                true, // overwrite
                false // useRunAsPrincipal
            );
        }
        return opportunity;
    }

    /**
     * Reprice given sales contract position.
     * 
     * @param position
     * @param contract
     * @param product
     * @throws ServiceException
     */
    public void repriceSalesContractPosition(
    	SalesContractPosition position,
        SalesContract contract,
        Product product
    ) throws ServiceException {
        if(contract == null) {
        	return;
        }
        PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
        if(position == null) {
        	return;
        }
        if(product == null) {
        	return;        
        }
        PricingRule pricingRule = position.getPricingRule();
        if(pricingRule == null) {
        	return;       
        }
        BigDecimal quantity = position.getQuantity();
        if(quantity == null) {
        	return;
        }
        Date pricingDate = position.getPricingDate() != null 
        	? position.getPricingDate() 
        	: contract.getPricingDate() != null 
        		? contract.getPricingDate() 
        		: contract.getActiveOn() != null 
        			? contract.getActiveOn() 
        			: new Date();
        Uom priceUom = position.getPriceUom() != null 
        	? position.getPriceUom() 
        	: position.getUom() != null 
        		? position.getUom() 
        		: null;
        short contractPositionState = position.getContractPositionState();
    	GetPriceLevelResult result =
            Products.getInstance().getPriceLevel(
                pricingRule,
                contract,
                position,
                product,
                priceUom,
                quantity,
                contractPositionState,
                pricingDate,
                null // no price level override
            );
        if(result.getStatusCode() != STATUS_CODE_OK) {
            throw new ServiceException(
                BasicException.Code.DEFAULT_DOMAIN,
                BasicException.Code.PROCESSING_FAILURE,
                "Unable to get price level",
                new BasicException.Parameter("code", result.getStatusCode()),
                new BasicException.Parameter("message", result.getStatusMessage())
            );
        }
        AbstractPriceLevel priceLevel = result.getPriceLevel() == null
        	? null
        	: (AbstractPriceLevel)pm.getObjectById(result.getPriceLevel().refGetPath());
        ProductBasePrice basePrice = result.getBasePrice() == null
        	? null
        	: (ProductBasePrice)pm.getObjectById(result.getBasePrice().refGetPath());
        BigDecimal discount = result.getDiscount();
        Boolean discountIsPercentage = result.isDiscountIsPercentage();
        DiscountOrigin discountOrigin = result.getDiscountOrigin() == null
        	? null
        	: (DiscountOrigin)pm.getObjectById(result.getDiscountOrigin().refGetPath());
        if(!Boolean.TRUE.equals(position.isManualPricing())) {
            if(!Utils.areEqual(priceLevel, position.getPriceLevel())) {
            	position.setPriceLevel(priceLevel);
            }
            if(!Utils.areEqual(basePrice, position.getListPrice())) {
            	position.setListPrice(basePrice);
            }
            // Do not amend pricePerUnit, discount and priceUom if basePrice is null
            if(basePrice != null) {
	        	if(!Utils.areEqual(basePrice.getPrice(), position.getPricePerUnit())) {
	        		position.setPricePerUnit(basePrice.getPrice());
	        	}
	        	if(!Utils.areEqual(discount, position.getDiscount())) {
	        		position.setDiscount(discount);
	        	}
	        	if(!Utils.areEqual(discountIsPercentage, position.isDiscountIsPercentage())) {
	                position.setDiscountIsPercentage(discountIsPercentage);
	        	}
	        	if(!Utils.areEqual(discountOrigin, position.getDiscountOrigin())) {
	                position.setDiscountOrigin(discountOrigin);
	        	}
	        	if(!Utils.areEqual(basePrice.getUom(), position.getPriceUom())) {
	        		position.setPriceUom(basePrice.getUom());
	        	}
            }
        }
        if(!Utils.areEqual(PRICING_STATE_OK, position.getPricingState())) {
        	position.setPricingState(PRICING_STATE_OK);
        }
    }

    /**
     * Controls whether the creation of contract position modifications is enabled / disabled.
     * Disabled by default. Override for custom-specific behavior.
     * 
     * @return
     */
    protected boolean isEnabledContractPositionModifications(
    ) {
    	return false;	
    }

    /**
     * Update sales contract position.
     * 
     * @param contract
     * @param position
     * @param product
     * @param reprice
     */
    public void updateSalesContractPosition(
        SalesContract contract,
        SalesContractPosition position,
        Product product,
        boolean reprice
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
        // Create
        if(JDOHelper.isNew(position)) {
        	if(position.getUom() == null) {
        		position.setUom(product.getDefaultUom());
        	}
        	if(position.getSalesTaxType() == null) {
        		position.setSalesTaxType(product.getSalesTaxType());
        	}
        	PricingRule pricingRule = position.getPricingRule() == null ?
        		contract.getPricingRule() :
        		position.getPricingRule();
        	if(pricingRule == null) {
            	org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(pm, position.refGetPath().getSegment(2).toString(), position.refGetPath().getSegment(4).toString());
        		PricingRuleQuery pricingRuleQuery = (PricingRuleQuery)pm.newQuery(PricingRule.class);
        		pricingRuleQuery.thereExistsIsDefault().isTrue();
        		List<PricingRule> pricingRules = productSegment.getPricingRule(pricingRuleQuery);
        		if(!pricingRules.isEmpty()) {
        			pricingRule = pricingRules.iterator().next();
        		}
        	}
        	position.setPricingRule(pricingRule);
        	if(this.isEnabledContractPositionModifications()) {
    	    	PositionCreation positionCreation = pm.newInstance(PositionCreation.class);
    	        positionCreation.setInvolved(position);
    	        contract.addPositionModification(
    	        	this.getUidAsString(),
    	        	positionCreation
    	        );
        	}
        } else {
        	if(this.isEnabledContractPositionModifications()) {
        		PersistenceManager pmOld = null;
        		try {
	            	pmOld = pm.getPersistenceManagerFactory().getPersistenceManager(
	            		SecurityKeys.ROOT_PRINCIPAL,
	            		null
	            	);
	                BigDecimal quantityOld = ((SalesContractPosition)pmOld.getObjectById(
	                	position.refGetPath())
	                ).getQuantity();
	                BigDecimal quantityNew = position.getQuantity();
	                if(!Utils.areEqual(quantityOld, quantityNew)) {
		            	QuantityModification quantityModification = pm.newInstance(QuantityModification.class);
		            	quantityModification.setInvolved(position);
		            	quantityModification.setQuantity(quantityOld);
		                contract.addPositionModification(
		                	this.getUidAsString(),
		                	quantityModification
		                );
	                }
        		} finally {
        			if(pmOld != null) {
                        pmOld.close();        				
        			}
        		}
        	}
        }
        if(reprice) {
    		this.repriceSalesContractPosition(
    			position,
    			contract,
    			product
            );
        }
    }

    /**
     * Get sales tax type for given sales contract position.
     * 
     * @param contract
     * @param position
     * @param product
     * @return
     */
    public SalesTaxType getSalesTaxType(
    	SalesContract contract,
    	SalesContractPosition position,
    	Product product
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
    	String providerName = contract.refGetPath().getSegment(2).toString();
    	String segmentName = contract.refGetPath().getSegment(4).toString();
    	org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(pm, providerName, segmentName);
    	SalesTaxType salesTaxType = null;
    	SalesTransactionType salesTransactionType = position.getSalesTransactionType();
    	Uom priceUom = position.getPriceUom();
    	// SalesTaxType defined by Product
    	if(product.getSalesTaxType() != null) {
    		salesTaxType = product.getSalesTaxType();
    	} else {
    		SalesTaxTypeAssignmentQuery salesTaxTypeAssignmentQuery = (SalesTaxTypeAssignmentQuery)pm.newQuery(SalesTaxTypeAssignment.class);
    		salesTaxTypeAssignmentQuery.forAllDisabled().isFalse();
    		salesTaxTypeAssignmentQuery.thereExistsSalesTaxType().forAllDisabled().isFalse();
    		if(priceUom != null) {
    			salesTaxTypeAssignmentQuery.thereExistsUom().equalTo(priceUom);
    		}
    		if(contract.getSalesTaxTypeGroup() != null) {
    			salesTaxTypeAssignmentQuery.thereExistsSalesTaxType().thereExistsSalesTaxTypeGroup().equalTo(contract.getSalesTaxTypeGroup());
    		}
    		if(salesTransactionType != null) {
    			salesTaxTypeAssignmentQuery.thereExistsSalesTaxType().thereExistsSalesTransactionType().equalTo(salesTransactionType);        			
    		}
    		List<SalesTaxTypeAssignment> salesTaxTypeAssignments = product.getSalesTaxTypeAssignment(salesTaxTypeAssignmentQuery);
    		if(!salesTaxTypeAssignments.isEmpty()) {
    			// SalesTaxType defined by SalesTaxTypeAssignment
    			salesTaxType = salesTaxTypeAssignments.iterator().next().getSalesTaxType();
    		} else {
    			SalesTaxTypeQuery salesTaxTypeQuery = (SalesTaxTypeQuery)pm.newQuery(SalesTaxType.class);
    			salesTaxTypeQuery.forAllDisabled().isFalse();
    			salesTaxTypeQuery.orderByValidFrom().descending();
        		if(contract.getSalesTaxTypeGroup() != null) {
        			salesTaxTypeQuery.thereExistsSalesTaxTypeGroup().equalTo(contract.getSalesTaxTypeGroup());
        		}
        		if(salesTransactionType != null) {
        			salesTaxTypeQuery.thereExistsSalesTransactionType().equalTo(salesTransactionType);        			
        		}
        		List<SalesTaxType> salesTaxTypes = productSegment.getSalesTaxType(salesTaxTypeQuery);
        		if(!salesTaxTypes.isEmpty()) {
        			// SalesTaxType defined globally (by segment)
        			salesTaxType = salesTaxTypes.iterator().next();
        		}
    		}
    	}
    	if(salesTaxType != null) {
    		position.setSalesTaxType(salesTaxType);
    		
    	}
    	return salesTaxType;
    }

    /**
     * Create sales contract position.
     * 
     * @param contract
     * @param isIgnoreProductConfiguration
     * @param name
     * @param quantity
     * @param pricingDate
     * @param product
     * @param uom
     * @param priceUom
     * @param pricingRule
     * @return
     */
    public SalesContractPosition createSalesContractPosition(
        SalesContract contract,
        Boolean isIgnoreProductConfiguration,
        String name,
        BigDecimal quantity,
        Date pricingDate,
        Product product,
        Uom uom,
        Uom priceUom,
        SalesTransactionType salesTransactionType,
        PricingRule pricingRule,
        AbstractPriceLevel priceLevel
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
    	SalesContractPosition position = null;
    	long maxLineItemNumber = 0;
    	if(contract instanceof Opportunity) {
    		AbstractOpportunityPositionQuery positionQuery = (AbstractOpportunityPositionQuery)pm.newQuery(AbstractOpportunityPosition.class);
    		positionQuery.orderByLineItemNumber().descending();
    		List<AbstractOpportunityPosition> positions = ((Opportunity)contract).getPosition(positionQuery);
    		if(!positions.isEmpty()) {
    			maxLineItemNumber = positions.iterator().next().getLineItemNumber();
    		}
    		position = pm.newInstance(OpportunityPosition.class);
    		((Opportunity)contract).addPosition(
    			this.getUidAsString(),
    			(OpportunityPosition)position
    		);
    	} else if(contract instanceof Quote) {
    		AbstractQuotePositionQuery positionQuery = (AbstractQuotePositionQuery)pm.newQuery(AbstractQuotePosition.class);
    		positionQuery.orderByLineItemNumber().descending();
    		List<AbstractQuotePosition> positions = ((Quote)contract).getPosition(positionQuery);
    		if(!positions.isEmpty()) {
    			maxLineItemNumber = positions.iterator().next().getLineItemNumber();
    		}
    		position = pm.newInstance(QuotePosition.class);
    		((Quote)contract).addPosition(
    			this.getUidAsString(),
    			(QuotePosition)position
    		);
    	} else if(contract instanceof SalesOrder) {
    		AbstractSalesOrderPositionQuery positionQuery = (AbstractSalesOrderPositionQuery)pm.newQuery(AbstractSalesOrderPosition.class);
    		positionQuery.orderByLineItemNumber().descending();
    		List<AbstractSalesOrderPosition> positions = ((SalesOrder)contract).getPosition(positionQuery);
    		if(!positions.isEmpty()) {
    			maxLineItemNumber = positions.iterator().next().getLineItemNumber();
    		}
    		position = pm.newInstance(SalesOrderPosition.class);
    		((SalesOrder)contract).addPosition(
    			this.getUidAsString(),
    			(SalesOrderPosition)position
    		);
    	} else if(contract instanceof Invoice) {
    		AbstractInvoicePositionQuery positionQuery = (AbstractInvoicePositionQuery)pm.newQuery(AbstractInvoicePosition.class);
    		positionQuery.orderByLineItemNumber().descending();
    		List<AbstractInvoicePosition> positions = ((Invoice)contract).getPosition(positionQuery);
    		if(!positions.isEmpty()) {
    			maxLineItemNumber = positions.iterator().next().getLineItemNumber();
    		}
    		position = pm.newInstance(InvoicePosition.class);
    		((Invoice)contract).addPosition(
    			this.getUidAsString(),
    			(InvoicePosition)position
    		);
    	} else {
            return null;
        }
        // lineItemNumber
    	Long positionNumber = new Long(100000 * (maxLineItemNumber / 100000 + 1));
        position.setLineItemNumber(
        	positionNumber
        );
        position.setPositionNumber(
        	positionNumber.toString()
        );
        // name
        if(name != null) {
            position.setName(name);
        } else {
            position.setName("Position " + position.getLineItemNumber());
        }
        // quantity
        if(quantity != null) {
            position.setQuantity(quantity);
        } else {
            position.setQuantity(BigDecimal.ONE);
        }
        // pricingDate
        if(pricingDate != null) {
            position.setPricingDate(pricingDate);
        } else {
            position.setPricingDate(
                contract.getPricingDate()
            );
        }
        // uom (only touch if specified as param
        position.setUom(uom);
        // priceUom (only touch if specified as param
        if(priceUom != null) {
            position.setPriceUom(priceUom);
        }
        // salesTransactionType
        if(salesTransactionType != null) {
        	position.setSalesTransactionType(salesTransactionType);
        }
        // pricingRule
        if(pricingRule != null) {
            position.setPricingRule(pricingRule);
        }
        // pricingLevel
        if(priceLevel != null) {
            position.setPriceLevel(priceLevel);
        }
        // calcRule
        position.setCalcRule(
            contract.getCalcRule()
        );
        // Update position
        if(product != null) {
        	SalesTaxType salesTaxType = this.getSalesTaxType(
        		contract, 
        		position, 
        		product
        	);
        	if(salesTaxType != null) {
        		position.setSalesTaxType(salesTaxType);
        		
        	}            	
            this.updateSalesContractPosition(
                contract,
                position,
                product,
                true
            );
            if(position instanceof ConfiguredProduct) {
            	((ConfiguredProduct)position).setProduct(product);
            }
            // Clone configurations
            if((isIgnoreProductConfiguration == null) || !isIgnoreProductConfiguration.booleanValue()) {
                Products.getInstance().cloneProductConfigurationSet(
                    product,
                    position,
                    false,
                    true,
                    JDOHelper.isNew(contract) ? null : contract.getOwningUser(),
                    JDOHelper.isNew(contract) ? null : contract.getOwningGroup()
                );
            }
	        // Update DepotReferences
            // Collect from product and salesTaxType
	        Collection<DepotReference> existingDepotReferences = position.getDepotReference();
	        List<Depot> excludeDepotReferences = new ArrayList<Depot>();
	        for(DepotReference existingDepotReference: existingDepotReferences) {
	        	excludeDepotReferences.add(existingDepotReference.getDepot());
	        }
	        Collection<DepotReference> depotReferences = new ArrayList<DepotReference>(product.<DepotReference>getDepotReference());
	        if(salesTaxType != null) {
	        	depotReferences.addAll(salesTaxType.<DepotReference>getDepotReference());
	        }
	        for(DepotReference depotReference: depotReferences) {
	        	if(!excludeDepotReferences.contains(depotReference.getDepot())) {
	                Cloneable.getInstance().cloneObject(
	                    depotReference,
	                    position,
	                    "depotReference",
	                    null,
	                    "property",
	                    contract.getOwningUser(),
	                    contract.getOwningGroup()
	                );		
	        	}
	        }   
        }
        // Touch contract --> jdoPreStore will be invoked
        this.markContractAsDirty(
        	contract
        );
        return position;
    }

    /**
     * Remove sales contract position callback. Override for custom-specific behavour.
     * 
     * @param position
     * @param checkForMinPositions
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeSalesContractPosition(
    	SalesContractPosition position,
        boolean checkForMinPositions,
        boolean preDelete
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
    	SalesContract contract = (SalesContract)pm.getObjectById(
			position.refGetPath().getParent().getParent()
		);
    	if(this.isEnabledContractPositionModifications()) {
	        // Make a copy of the removed position
	        Marshaller positionMarshaller = new Marshaller() {
	            public Object marshal(
	            	Object s
	            ) throws ServiceException {
	            	if(s instanceof SalesContractPosition) {
	            		SalesContractPosition position = (SalesContractPosition)s;
	            		PersistenceManager pm = JDOHelper.getPersistenceManager(position);
	            		RemovedPosition removedPosition = pm.newInstance(RemovedPosition.class);
	            		Contracts.getInstance().copySalesContractPosition(
	            			position, 
	            			removedPosition
	            		);
	            		return removedPosition;
	            	} else {
	            		return s;
	            	}
	            }
	            public Object unmarshal(Object s) {
	                throw new UnsupportedOperationException();
	            }
	        };
	        Map<String,Marshaller> objectMarshallers = new HashMap<String,Marshaller>();
	        objectMarshallers.put("org:opencrx:kernel:contract1:OpportunityPosition", positionMarshaller);
	        objectMarshallers.put("org:opencrx:kernel:contract1:QuotePosition", positionMarshaller);
	        objectMarshallers.put("org:opencrx:kernel:contract1:SalesOrderPosition", positionMarshaller);
	        objectMarshallers.put("org:opencrx:kernel:contract1:InvoicePosition", positionMarshaller);
	        RemovedPosition removedPosition = (RemovedPosition)Cloneable.getInstance().cloneObject(
	        	position, 
	        	contract, 
	        	"removedPosition", 
	        	objectMarshallers, 
	        	null,
	        	contract.getOwningUser(),
	        	contract.getOwningGroup()
	        );
	        // Update position modifications
	        // Replace involved with removed position
	        Collection<PositionModification> positionModifications = contract.getPositionModification();
	        for(PositionModification positionModification: positionModifications) {
	        	try {
		        	if(positionModification.getInvolved().equals(position)) {
		        		positionModification.setInvolved(removedPosition);
		        	}
	        	} catch(Exception e) {}
	        }
	        PositionRemoval positionRemoval = pm.newInstance(PositionRemoval.class);
	        positionRemoval.setInvolved(removedPosition);
	        contract.addPositionModification(
	        	this.getUidAsString(),
	        	positionRemoval
	        );
    	}
        if(!preDelete) {
        	position.refDelete();        	
        }
        this.markContractAsDirty(
        	contract
        );
    }

    /**
     * Remove contract callback. Override for custom-specific behaviour.
     * 
     * @param contract
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeContract(
        AbstractContract contract,
        boolean preDelete
    ) throws ServiceException {
    }
    
    /**
     * Set pricing state for given contract position. At the same
     * time amend the pricing state of the contract.
     * 
     * @param position
     * @param pricingState
     */
    /**
     * @param position
     * @param pricingState
     */
    public void updatePricingState(
    	SalesContractPosition position,
    	short pricingState
    ) {
    	if(JDOHelper.isPersistent(position)) {
	    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
	        SalesContract contract = 
	        	(SalesContract)pm.getObjectById(
	            position.refGetPath().getParent().getParent()
	        );
	        this.updatePricingState(
	        	contract, 
	        	pricingState
	        );
    	}
        position.setPricingState(pricingState);
    }
    
    /**
     * Set pricing state for given contract.
     * 
     * @param contract
     * @param pricingState
     */
    public void updatePricingState(
    	SalesContract contract,
    	short pricingState
    ) {
        contract.setPricingState(pricingState);
    }

    /**
     * Update contract state.
     * 
     * @param contract
     * @param contractState
     */
    public void updateContractState(
    	AbstractContract contract,
    	short contractState
    ) {
    }
    
    /**
     * Update sales contract position callback. Override for custom-specific behaviour.
     * 
     * @param position
     * @throws ServiceException
     */
    protected void updateSalesContractPosition(
    	SalesContractPosition position
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
    	SalesContract contract = (SalesContract)pm.getObjectById(
            position.refGetPath().getParent().getParent()
        );
        if(position instanceof ConfiguredProduct) {
        	Product product = ((ConfiguredProduct)position).getProduct();
	        if(product != null) {
	            this.updateSalesContractPosition(
	                contract,
	            	position,
	                product,
	                false
	            );
	        }
        }
        this.markContractAsDirty(
            contract
        );
    }

    /**
     * Re-price sales contract position.
     * 
     * @param position
     * @return
     * @throws ServiceException
     */
    public short repriceSalesContractPosition(
        SalesContractPosition position
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
    	SalesContract contract = (SalesContract)pm.getObjectById(
			position.refGetPath().getParent().getParent()
		);
    	short pricingState = 0;
        if(position instanceof ConfiguredProduct) {
        	Product product = ((ConfiguredProduct)position).getProduct();
        	if(product != null) {
	            this.updateSalesContractPosition(
	                contract,
	                position,
	                product,
	                true
	            );
        	}
        	pricingState = position.getPricingState();
        }
        return pricingState;
    }
    
    /**
     * Preview re-price sales contract position.
     * 
     * @param position
     * @param overrideCalculationRule
     * @param overridePriceLevel
     * @param overrideProduct
     * @param overridePriceUom
     * @param overrideManualPricing
     * @param overridePricePerUnit
     * @param overrideQuantity
     * @param overrideUomScaleFactor
     * @param overrideSalesTaxRate
     * @param overrideDiscount
     * @param overrideDiscountIsPercentage
     * @param overrideDiscountCalculationType
     * @param overridePricingDate
     * @return
     * @throws ServiceException
     */
    public PreviewRepriceResult previewRepriceSalesContractPosition(
        SalesContractPosition position,
        CalculationRule overrideCalculationRule,
        AbstractPriceLevel overridePriceLevel,
        AbstractProduct overrideProduct,
        Uom overridePriceUom,
        Boolean overrideManualPricing,
        BigDecimal overridePricePerUnit,
        BigDecimal overrideQuantity,
        BigDecimal overrideUomScaleFactor,
        BigDecimal overrideSalesTaxRate,
        BigDecimal overrideDiscount,
        Boolean overrideDiscountIsPercentage,
        Short overrideDiscountCalculationType,
        Short overrideContractPositionState,
        Date overridePricingDate
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(position);
    	String providerName = position.refGetPath().getSegment(2).toString();
    	String segmentName = position.refGetPath().getSegment(4).toString();
    	org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(pm, providerName, segmentName);
    	SalesContract contract = (SalesContract)pm.getObjectById(position.refGetPath().getParent().getParent());
        PricingRule pricingRule = position.getPricingRule();
    	AbstractProduct product = overrideProduct != null
    		? overrideProduct
    		: ((ConfiguredProduct)position).getProduct();
        BigDecimal quantity = overrideQuantity != null ?
        	overrideQuantity
            : this.getMinMaxAdjustedQuantity(position);    	
        Uom priceUom = overridePriceUom != null
        	? overridePriceUom
        	: position.getPriceUom() != null 
	        	? position.getPriceUom() 
	        	: position.getUom() != null 
	        		? position.getUom() 
	        		: null;
        Date pricingDate = overridePricingDate != null
        	? overridePricingDate
        	: position.getPricingDate() != null 
	        	? position.getPricingDate() 
	        	: contract.getPricingDate() != null 
	        		? contract.getPricingDate() 
	        		: contract.getActiveOn() != null 
	        			? contract.getActiveOn() 
	        			: new Date();
	    short contractPositionState = overrideContractPositionState != null
	    	? overrideContractPositionState
	    	: position.getContractPositionState();
    	GetPriceLevelResult getPriceLevelResult =
            Products.getInstance().getPriceLevel(
                pricingRule,
                contract,
                position,
                product,
                priceUom,
                quantity,
                contractPositionState,
                pricingDate,
                overridePriceLevel
            );
        if(getPriceLevelResult.getStatusCode() != STATUS_CODE_OK) {
            return Structures.create(
            	PreviewRepriceResult.class,
            	Datatypes.member(PreviewRepriceResult.Member.statusCode, getPriceLevelResult.getStatusCode()),
            	Datatypes.member(PreviewRepriceResult.Member.statusMessage, getPriceLevelResult.getStatusMessage())
            );
        }
        if(getPriceLevelResult.getBasePrice() == null) {
            return Structures.create(
            	PreviewRepriceResult.class,
            	Datatypes.member(PreviewRepriceResult.Member.statusCode, (short)-1),
            	Datatypes.member(PreviewRepriceResult.Member.statusMessage, "getPriceLevelResult::basePrice is null")
            );
        }
    	CalculationRule calculationRule = overrideCalculationRule != null
    		? overrideCalculationRule
    		: this.getDefaultCalculationRule(contractSegment);
        BigDecimal uomScaleFactor = overrideUomScaleFactor != null
        	? overrideUomScaleFactor
        	: this.getUomScaleFactor(position);
        BigDecimal salesTaxRate = overrideSalesTaxRate != null
        	? overrideSalesTaxRate
        	: this.getSalesTaxRate(position);
        BigDecimal discount = null;
        Boolean discountIsPercentage = null;
        if(overrideDiscount != null) {
        	discount = overrideDiscount;
        	discountIsPercentage = overrideDiscountIsPercentage;
        } else {
        	discount = getPriceLevelResult.getDiscount();
        	discountIsPercentage = getPriceLevelResult.isDiscountIsPercentage();
        }
        Short discountCalculationType = overrideDiscountCalculationType != null
        	? overrideDiscountCalculationType
        	: position.getDiscountCalculationType();
        Boolean manualPricing = overrideManualPricing != null
        	? overrideManualPricing
        	: position.isManualPricing();
        BigDecimal pricePerUnit = overridePricePerUnit != null
        	? overridePricePerUnit
        	: Boolean.TRUE.equals(manualPricing)
        		? position.getPricePerUnit()
        		: getPriceLevelResult.getBasePrice().getPrice();
        GetPositionAmountsResult getPositionAmountsResult = this.getPositionAmounts(
            calculationRule,
            position,
            pricePerUnit,
            quantity,
            uomScaleFactor,
            discount,
            discountIsPercentage,
            discountCalculationType,
            salesTaxRate
        );
        if(getPositionAmountsResult.getStatusCode() != STATUS_CODE_OK) {
            return Structures.create(
            	PreviewRepriceResult.class,
            	Datatypes.member(PreviewRepriceResult.Member.statusCode, getPositionAmountsResult.getStatusCode()),
            	Datatypes.member(PreviewRepriceResult.Member.statusMessage, getPositionAmountsResult.getStatusMessage())
            );
        }
        return Structures.create(
        	PreviewRepriceResult.class,
        	Datatypes.member(PreviewRepriceResult.Member.statusCode, (short)0),
        	Datatypes.member(PreviewRepriceResult.Member.amount, getPositionAmountsResult.getAmount()),
        	Datatypes.member(PreviewRepriceResult.Member.baseAmount, getPositionAmountsResult.getBaseAmount()),
        	Datatypes.member(PreviewRepriceResult.Member.baseAmountIncludingTax, getPositionAmountsResult.getBaseAmountIncludingTax()),
        	Datatypes.member(PreviewRepriceResult.Member.basePrice, getPriceLevelResult.getBasePrice()),
        	Datatypes.member(PreviewRepriceResult.Member.discount, discount),
        	Datatypes.member(PreviewRepriceResult.Member.discountAmount, getPositionAmountsResult.getDiscountAmount()),
        	Datatypes.member(PreviewRepriceResult.Member.discountAmountIncludingTax, getPositionAmountsResult.getDiscountAmountIncludingTax()),
        	Datatypes.member(PreviewRepriceResult.Member.discountIsPercentage, discountIsPercentage),
        	Datatypes.member(PreviewRepriceResult.Member.discountOrigin, getPriceLevelResult.getDiscountOrigin()),
        	Datatypes.member(PreviewRepriceResult.Member.priceLevel, getPriceLevelResult.getPriceLevel()),
        	Datatypes.member(PreviewRepriceResult.Member.pricePerUnit, getPositionAmountsResult.getPricePerUnit()),
        	Datatypes.member(PreviewRepriceResult.Member.pricePerUnitIncludingTax, getPositionAmountsResult.getPricePerUnitIncludingTax()),
        	Datatypes.member(PreviewRepriceResult.Member.taxAmount, getPositionAmountsResult.getTaxAmount())
        );
    }

    /**
     * Re-price the given sales contract.
     * 
     * @param contract
     * @throws ServiceException
     */
    public void repriceSalesContract(
    	SalesContract contract
    ) throws ServiceException {
        List<SalesContractPosition> positions = new ArrayList<SalesContractPosition>();
        if(contract instanceof Opportunity) {
        	Collection<AbstractOpportunityPosition> p = ((Opportunity)contract).getPosition();
        	positions.addAll(p);
        } else if(contract instanceof Quote) {
        	Collection<AbstractQuotePosition> p = ((Quote)contract).getPosition();
        	positions.addAll(p);
        } else if(contract instanceof SalesOrder) {
        	Collection<AbstractSalesOrderPosition> p = ((SalesOrder)contract).getPosition();
        	positions.addAll(p);
        } else if(contract instanceof Invoice) {
        	Collection<AbstractInvoicePosition> p = ((Invoice)contract).getPosition();
        	positions.addAll(p);
        }
        short pricingState = PRICING_STATE_OK;
        for(SalesContractPosition position: positions) {
            short pricingStatePosition = this.repriceSalesContractPosition(
                position
            );
            if(pricingStatePosition == PRICING_STATE_DIRTY) {
                pricingState = PRICING_STATE_DIRTY;
            }
        }
        contract.setPricingState(
            pricingState
        );
    }

    /**
     * Count contracts matching the given contract filter.
     * 
     * @param contractFilter
     * @return
     * @throws ServiceException
     */
    public int countFilteredContract(
        AbstractFilterContract contractFilter
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contractFilter);
        AbstractContractQuery query = (AbstractContractQuery)pm.newQuery(AbstractContract.class);
        QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);    	
        List<AbstractContract> contracts = contractFilter.getFilteredContract(query);
        return contracts.size();
    }

    /**
     * Get rounding factor to round total amounts for given sales contract position.
     * 
     * @param position
     * @return
     */
    public BigDecimal getRoundingFactorTotal(
    	SalesContractPosition position
    ) {
    	return new BigDecimal(0.01);
    }

    /**
     * Get rounding factor to round total amounts for given sales contract.
     * 
     * @param contract
     * @return
     */
    public BigDecimal getRoundingFactorTotal(
    	SalesContract contract
    ) {
    	return new BigDecimal(0.01);
    }

    /**
     * Get discount amount per unit.
     * 
     * @param position
     * @return
     */
    public BigDecimal getDiscountAmountPerUnit(
    	SalesContractPosition position,
    	BigDecimal quantity,
    	BigDecimal discount,
    	Boolean discountIsPercentage,
    	Short discountCalculationType,
    	BigDecimal pricePerUnit,
    	BigDecimal salesTaxRate
    ) {
    	return Boolean.TRUE.equals(discountIsPercentage)
    		? pricePerUnit.multiply(discount.divide(HUNDRED))
    		: discount == null
    			? BigDecimal.ZERO
    			: discount;
    }

    /**
     * Get amounts for contract positions.
     * 
     * @param calculationRule
     * @param position
     * @param pricePerUnit
     * @param quantity
     * @param uomScaleFactor
     * @param discount
     * @param discountIsPercentage
     * @param salesTaxRate
     * @return
     */
    public GetPositionAmountsResult getPositionAmountsInternal(
        CalculationRule calculationRule,
        SalesContractPosition position,
        BigDecimal pricePerUnit,
        BigDecimal quantity,
        BigDecimal uomScaleFactor,
        BigDecimal discount,
        Boolean discountIsPercentage,
        Short discountCalculationType,
        BigDecimal salesTaxRate
    ) {
    	MathContext mathCtx = new MathContext(100, RoundingMode.HALF_UP);
        BigDecimal salesTaxRateFactor = salesTaxRate.divide(HUNDRED);
    	BigDecimal roundingFactorTotal = this.getRoundingFactorTotal(position);
        BigDecimal discountAmountPerUnit = this.getDiscountAmountPerUnit(
        	position,
        	quantity,
        	discount,
        	discountIsPercentage,
        	discountCalculationType,
        	pricePerUnit,
        	salesTaxRate
        );
        BigDecimal pricePerUnitIncludingTax = pricePerUnit.multiply(BigDecimal.ONE.add(salesTaxRateFactor));
        BigDecimal discountAmountUnrounded = discountAmountPerUnit == null
        	? BigDecimal.ZERO
        	: quantity.multiply(discountAmountPerUnit.multiply(uomScaleFactor, mathCtx), mathCtx);
        BigDecimal baseAmountUnrounded = quantity.multiply(pricePerUnit.multiply(uomScaleFactor, mathCtx), mathCtx);
        BigDecimal grossAmountUnrounded = baseAmountUnrounded.subtract(discountAmountUnrounded);
        BigDecimal taxAmountUnrounded = grossAmountUnrounded.multiply(salesTaxRateFactor, mathCtx);
        BigDecimal amount = Utils.round(grossAmountUnrounded.add(taxAmountUnrounded), roundingFactorTotal);
        BigDecimal baseAmount = Utils.round(baseAmountUnrounded, roundingFactorTotal);
        BigDecimal baseAmountIncludingTax = Utils.round(
        	baseAmount.multiply(BigDecimal.ONE.add(salesTaxRateFactor), mathCtx),
        	roundingFactorTotal
        );
        // Derive rounded taxAmount from rounded amount
        // IMPORTANT:
        // taxAmount = round(taxAmountUnrounded, roundingFactor)
        // is NOT always correct depending on roundingFactor
        BigDecimal taxAmount = Utils.round(
        	amount.multiply(salesTaxRateFactor, mathCtx).divide(BigDecimal.ONE.add(salesTaxRateFactor), mathCtx),
        	roundingFactorTotal
        );
        // Derive rounded discountAmount from baseAmount, taxAmount and amount
        // IMPORTANT:
        // discountAmount = round(discountAmountUnrounded, roundingFactor)
        // is NOT always correct depending on roundingFactor
        BigDecimal discountAmount = baseAmount.add(taxAmount).subtract(amount);
        BigDecimal discountAmountIncludingTax = Utils.round(
        	discountAmount.multiply(BigDecimal.ONE.add(salesTaxRateFactor), mathCtx),
        	roundingFactorTotal
        );
        GetPositionAmountsResult result = Structures.create(
        	GetPositionAmountsResult.class,
        	Datatypes.member(GetPositionAmountsResult.Member.amount, amount),
        	Datatypes.member(GetPositionAmountsResult.Member.baseAmount, baseAmount),
        	Datatypes.member(GetPositionAmountsResult.Member.baseAmountIncludingTax, baseAmountIncludingTax),
        	Datatypes.member(GetPositionAmountsResult.Member.discountAmount, discountAmount),
        	Datatypes.member(GetPositionAmountsResult.Member.discountAmountIncludingTax, discountAmountIncludingTax),
        	Datatypes.member(GetPositionAmountsResult.Member.pricePerUnit, pricePerUnit),
        	Datatypes.member(GetPositionAmountsResult.Member.pricePerUnitIncludingTax, pricePerUnitIncludingTax),
        	Datatypes.member(GetPositionAmountsResult.Member.statusCode, (short)0),
        	Datatypes.member(GetPositionAmountsResult.Member.statusMessage, null),
        	Datatypes.member(GetPositionAmountsResult.Member.taxAmount, taxAmount)
        );
        return result;
    }

    /**
     * Get contract amounts.
     * 
     * @param rootPkg
     * @param calculationRule
     * @param contract
     * @param lineItemNumbers
     * @param positionBaseAmounts
     * @param positionDiscountAmounts
     * @param positionTaxAmounts
     * @param positionAmounts
     * @param salesCommissions
     * @param salesCommissionIsPercentages
     * @return
     */
    public static GetContractAmountsResult getContractAmounts(
        RefPackage_1_0 rootPkg,
        CalculationRule calculationRule,
        SalesContract contract,
        java.lang.Integer[] lineItemNumbers,
        java.math.BigDecimal[] positionBaseAmounts,
        java.math.BigDecimal[] positionDiscountAmounts,
        java.math.BigDecimal[] positionTaxAmounts,
        java.math.BigDecimal[] positionAmounts,
        java.math.BigDecimal[] salesCommissions,
        java.lang.Boolean[] salesCommissionIsPercentages
    ) {
    	try {
	    	return Contracts.getInstance().getContractAmounts(
	            calculationRule,
	            contract,
	            lineItemNumbers,
	            positionBaseAmounts,
	            positionDiscountAmounts,
	            positionTaxAmounts,
	            positionAmounts,
	            salesCommissions,
	            salesCommissionIsPercentages
	    	);
    	} catch(ServiceException e) {
    		throw new RuntimeServiceException(e);
    	}
    }

    /**
     * Get contract amounts.
     * 
     * @param calculationRule
     * @param contract
     * @param lineItemNumbers
     * @param positionBaseAmounts
     * @param positionDiscountAmounts
     * @param positionTaxAmounts
     * @param positionAmounts
     * @param salesCommissions
     * @param salesCommissionIsPercentages
     * @return
     */
    public GetContractAmountsResult getContractAmounts(
        CalculationRule calculationRule,
        SalesContract contract,
        Integer[] lineItemNumbers,
        BigDecimal[] positionBaseAmounts,
        BigDecimal[] positionDiscountAmounts,
        BigDecimal[] positionTaxAmounts,
        BigDecimal[] positionAmounts,
        BigDecimal[] salesCommissions,
        Boolean[] salesCommissionIsPercentages
    ) {
    	BigDecimal roundingFactor = this.getRoundingFactorTotal(contract);    	
        BigDecimal totalBaseAmount = BigDecimal.ZERO;
        BigDecimal totalDiscountAmount = BigDecimal.ZERO;
        BigDecimal totalTaxAmount = BigDecimal.ZERO;
        BigDecimal totalSalesCommission = BigDecimal.ZERO;
        for(int i = 0; i < positionBaseAmounts.length; i++) {
            BigDecimal baseAmount = positionBaseAmounts[i] != null ? positionBaseAmounts[i] : BigDecimal.ZERO; 
            totalBaseAmount = totalBaseAmount.add(baseAmount);
            BigDecimal discountAmount = positionDiscountAmounts[i] != null ? positionDiscountAmounts[i] : BigDecimal.ZERO;
            totalDiscountAmount = totalDiscountAmount.add(discountAmount);
            BigDecimal taxAmount = positionTaxAmounts[i] != null ? positionTaxAmounts[i] : BigDecimal.ZERO;
            totalTaxAmount = totalTaxAmount.add(taxAmount);
            BigDecimal salesCommission = salesCommissions[i] != null ? salesCommissions[i] : BigDecimal.ZERO;
            totalSalesCommission = totalSalesCommission.add(
            	(salesCommissionIsPercentages[i] != null) && (salesCommissionIsPercentages[i].booleanValue())
	              	? baseAmount.subtract(discountAmount).multiply(salesCommission.divide(new BigDecimal(100), BigDecimal.ROUND_UP))
	              	: salesCommission
            );
        }
        totalBaseAmount = Utils.round(totalBaseAmount, roundingFactor);
        totalTaxAmount = Utils.round(totalTaxAmount, roundingFactor);
        totalDiscountAmount = Utils.round(totalDiscountAmount, roundingFactor);
        totalSalesCommission = Utils.round(totalSalesCommission, roundingFactor);
        BigDecimal totalAmount = totalBaseAmount.subtract(totalDiscountAmount);
        BigDecimal totalAmountIncludingTax = totalAmount.add(totalTaxAmount);
        GetContractAmountsResult result = Structures.create(
        	GetContractAmountsResult.class,
        	Datatypes.member(GetContractAmountsResult.Member.statusCode, (short)0),
        	Datatypes.member(GetContractAmountsResult.Member.statusMessage, null),
        	Datatypes.member(GetContractAmountsResult.Member.totalAmount, totalAmount),
        	Datatypes.member(GetContractAmountsResult.Member.totalAmountIncludingTax, totalAmountIncludingTax),
        	Datatypes.member(GetContractAmountsResult.Member.totalBaseAmount, totalBaseAmount),
        	Datatypes.member(GetContractAmountsResult.Member.totalDiscountAmount, totalDiscountAmount),
        	Datatypes.member(GetContractAmountsResult.Member.totalSalesCommission, totalSalesCommission),
        	Datatypes.member(GetContractAmountsResult.Member.totalTaxAmount, totalTaxAmount)
        );
        return result;
    }

    /**
     * Creates a contract for the given contractType. Override this method to 
     * support custom-specific contract types.
     * 
     * @param contractSegment
     * @param contractType
     * @param basedOn
     * @return
     * @throws ServiceException
     */
    public AbstractContract createContract(
    	org.opencrx.kernel.contract1.jmi1.Segment contractSegment,
    	short contractType,
    	AbstractContract basedOn
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contractSegment);
    	AbstractContract contract = null;
    	ContractTypeQuery contractTypeQuery = (ContractTypeQuery)pm.newQuery(ContractType.class);
    	contractTypeQuery.contractType().equalTo(contractType);
    	List<ContractType> contractTypes = contractSegment.getContractType(contractTypeQuery);
    	ContractType defaultContractType = contractTypes.isEmpty() ? null :
    		contractTypes.iterator().next();
    	// Lead
    	if(contractType == CONTRACT_TYPE_LEAD) {
    		contract = pm.newInstance(Lead.class);
	        contractSegment.addLead(
	        	this.getUidAsString(),
	        	(Lead)contract
	        );  	     
	        contract.setOrigin(basedOn);
    	} else if(contractType == CONTRACT_TYPE_OPPORTUNITY) {
        	// Opportunity
    		if(basedOn instanceof Lead) {
    			contract = this.createOpportunity((Lead)basedOn);
    		} else if(basedOn instanceof Opportunity) {
	    		contract = (Opportunity)Cloneable.getInstance().cloneObject(
	    			basedOn, 
	    			contractSegment, 
	    			"opportunity", 
	    			null, // object marshallers
	    			null, // reference filter
	    			null, // owning user
	    			null // owningGroup
	    		);
    		} else {
    	        contract = pm.newInstance(Opportunity.class);
    	        contractSegment.addOpportunity(
    	        	this.getUidAsString(),
    	        	(Opportunity)contract
    	        );    			
    	        contract.setOrigin(basedOn);
    		}
    	} else if(contractType == CONTRACT_TYPE_QUOTE) {
        	// Quote
    		if(basedOn instanceof Opportunity) {
    			contract = this.createQuote((Opportunity)basedOn);
    		} else if(basedOn instanceof Quote) {
	    		contract = (Quote)Cloneable.getInstance().cloneObject(
	    			basedOn, 
	    			contractSegment, 
	    			"quote", 
	    			null, // object marshallers
	    			null, // reference filter
	    			null, // owning user
	    			null // owningGroup
	    		);
    		} else {
    	        contract = pm.newInstance(Quote.class);
    	        contractSegment.addQuote(
    	        	this.getUidAsString(),
    	        	(Quote)contract
    	        );
    	        contract.setOrigin(basedOn);
    		}
    	} else if(contractType == CONTRACT_TYPE_SALES_ORDER) {
        	// SalesOrder
    		if(basedOn instanceof Quote) {
    			contract = this.createSalesOrder((Quote)basedOn);
    		} else if(basedOn instanceof SalesOrder) {
	    		contract = (SalesOrder)Cloneable.getInstance().cloneObject(
	    			basedOn, 
	    			contractSegment, 
	    			"salesOrder", 
	    			null, // object marshallers
	    			null, // reference filter
	    			null, // owning user
	    			null // owningGroup
	    		);
    		} else {
    	        contract = pm.newInstance(SalesOrder.class);
    	        contractSegment.addSalesOrder(
    	        	this.getUidAsString(),
    	        	(SalesOrder)contract
    	        );
    	        contract.setOrigin(basedOn);
    		}
    	} else if(contractType == CONTRACT_TYPE_INVOICE) {
        	// Invoice
    		if(basedOn instanceof SalesOrder) {
    			contract = this.createInvoice((SalesOrder)basedOn);
    		} else if(basedOn instanceof Invoice) {
	    		contract = (Invoice)Cloneable.getInstance().cloneObject(
	    			basedOn, 
	    			contractSegment, 
	    			"invoice", 
	    			null, // object marshallers
	    			null, // reference filter
	    			null, // owning user
	    			null // owningGroup
	    		);
    		} else {
    	        contract = pm.newInstance(Invoice.class);
    	        contractSegment.addInvoice(
    	        	this.getUidAsString(),
    	        	(Invoice)contract
    	        );
    	        contract.setOrigin(basedOn);
    		}
    	} else if(contractType == CONTRACT_TYPE_SALESVOLUME_CONTRACT) {
        	// SalesVolumeContract
    		if(basedOn instanceof SalesVolumeContract) {
	    		contract = (SalesVolumeContract)Cloneable.getInstance().cloneObject(
	    			basedOn, 
	    			contractSegment, 
	    			"contract", 
	    			null, // object marshallers
	    			null, // reference filter
	    			null, // owning user
	    			null // owningGroup
	    		);
    		} else {
    	        contract = pm.newInstance(SalesVolumeContract.class);
    	        contractSegment.addContract(
    	        	this.getUidAsString(),
    	        	(SalesVolumeContract)contract
    	        );
    	        contract.setOrigin(basedOn);
    		}
    	} else if(contractType == CONTRACT_TYPE_GENERIC_CONTRACT) {
        	// GenericContract
    		if(basedOn instanceof GenericContract) {
	    		contract = (GenericContract)Cloneable.getInstance().cloneObject(
	    			basedOn, 
	    			contractSegment, 
	    			"contract", 
	    			null, // object marshallers
	    			null, // reference filter
	    			null, // owning user
	    			null // owningGroup
	    		);
    		} else {
    	        contract = pm.newInstance(GenericContract.class);
    	        contractSegment.addContract(
    	        	this.getUidAsString(),
    	        	(GenericContract)contract
    	        );
    	        contract.setOrigin(basedOn);
    		}
    	}
    	if(contract != null) {
    		contract.setContractType(defaultContractType);
    	}
    	return contract;
    }

    /**
     * Create contract and re-apply contract creator.
     * 
     * @param contractCreator
     * @param name
     * @param description
     * @param contractType
     * @param activeOn
     * @param priority
     * @param basedOn
     * @return
     * @throws ServiceException
     */
    public AbstractContract createContract(
        ContractCreator contractCreator,
        String name,
        String description,
        ContractType contractType,
        Date activeOn,
        Short priority,
        AbstractContract basedOn
    ) throws ServiceException {
    	return this.createContract(
    		contractCreator,
    		name,
    		description,
    		contractType,
    		activeOn,
    		priority,
    		basedOn,
    		true // reapplyContractCreator
    	);
    }

    /**
     * Create contract based on given contract creator.
     * 
     * @param contractCreator
     * @param name
     * @param description
     * @param contractType
     * @param activeOn
     * @param priority
     * @param basedOn
     * @param reapplyContractCreator
     * @return
     * @throws ServiceException
     */
    public AbstractContract createContract(
        ContractCreator contractCreator,
        String name,
        String description,
        ContractType contractType,
        Date activeOn,
        Short priority,
        AbstractContract basedOn,
        boolean reapplyContractCreator
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contractCreator);
    	String providerName = contractCreator.refGetPath().getSegment(2).toString();
    	String segmentName = contractCreator.refGetPath().getSegment(4).toString();
    	boolean useRunAsPrincipal = Utils.hasObjectRunAsPermission(
    		contractCreator.refGetPath(), 
    		Utils.getPermissions(
    			Utils.getRequestingPrincipal(pm, providerName, segmentName), 
    			Action.RUN_AS.getName()
    		)
    	);
    	org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
    		pm, 
    		providerName, 
    		segmentName
    	);
    	AbstractContract contract = null;
    	if(contractType == null) {
    		contractType = contractCreator.getDefaultType();
    	}
    	if(contractType != null) {
    		contract = this.createContract(
    			contractSegment,
    			contractType.getContractType(),
    			basedOn
    		);
    		if(contract != null) {
    			contract.setName(
    				name == null ? "@" + new Date() : name
    			);
    			if(description != null) {
    				contract.setDescription(description);
    			}
    			activeOn = activeOn == null ? contractCreator.getActiveOn() : activeOn;
    			if(activeOn != null) {
    				contract.setActiveOn(activeOn);
    			}
    			priority = priority == null ? contractCreator.getPriority() : priority;
    			if(priority != null) {
    				contract.setPriority(priority);
    			}
    			contract.setContractType(contractType);
    			Base.getInstance().assignToMe(
    				contract, 
    				true, // overwrite 
    				useRunAsPrincipal
    			);
    			if(reapplyContractCreator) {
    				this.reapplyContractCreator(
    					contract,
    					contractCreator
    				);
    			}
    		}
    	}
    	return contract;
    }

    /**
     * Get sales tax type group for given contract.
     * 
     * @param contract
     * @param contractCreator
     * @return
     * @throws ServiceException
     */
    public SalesTaxTypeGroup getSalesTaxTypeGroup(
    	AbstractContract contract,
    	SalesContractCreator contractCreator
    ) throws ServiceException {
    	return contractCreator.getSalesTaxTypeGroup();
    }

    /**
     * Create sales contract based on given contract creator.
     * 
     * @param contractCreator
     * @param name
     * @param description
     * @param contractType
     * @param activeOn
     * @param priority
     * @param pricingDate
     * @param contractCurrency
     * @param customer
     * @param salesRep
     * @param broker
     * @param supplier
     * @param basedOn
     * @return
     * @throws ServiceException
     */
    public AbstractContract createSalesContract(
    	SalesContractCreator contractCreator,
        String name,
        String description,
        ContractType contractType,
        Date activeOn,
        Short priority,
        Date pricingDate,
        Short contractCurrency,
        Short paymentTerms,
        Account customer,
        Account salesRep,
        Account broker,
        Account supplier,
        SalesTaxTypeGroup salesTaxTypeGroup,
        AbstractContract basedOn
    ) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(contractCreator);
		String providerName = contractCreator.refGetPath().getSegment(2).toString();
		String segmentName = contractCreator.refGetPath().getSegment(4).toString();
    	AbstractContract contract = this.createContract(
    		contractCreator, 
    		name, 
    		description, 
    		contractType, 
    		activeOn, 
    		priority,
    		basedOn,
    		false // reapplyContractCreator
    	);
    	if(contract instanceof SalesContract) {
            org.opencrx.kernel.contract1.jmi1.Segment contractSegment = this.getContractSegment(
                pm, 
                providerName, 
                segmentName
            );
            org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(
            	pm, 
            	providerName, 
            	segmentName
            );
    		SalesContract salesContract = (SalesContract)contract;
    		pricingDate = pricingDate == null ? contractCreator.getPricingDate() : pricingDate;
    		if(pricingDate != null) {
    			salesContract.setPricingDate(pricingDate);
    		}
    		salesContract.setPricingRule(
    			contractCreator.getPricingRule() == null ?
        			((SalesContract)contract).getPricingRule() == null ?
        				Products.getInstance().getDefaultPricingRule(productSegment) :
        					((SalesContract)contract).getPricingRule() :
        						contractCreator.getPricingRule()    			
    		);
    		salesContract.setCalcRule(    			
    			contractCreator.getCalcRule() == null ? 
    				((SalesContract)contract).getCalcRule() == null ?
    					this.getDefaultCalculationRule(contractSegment) : 
    						((SalesContract) contract).getCalcRule() :
    							contractCreator.getCalcRule()
    		);
    		contractCurrency = contractCurrency == null ? contractCreator.getContractCurrency() : contractCurrency;
    		if(contractCurrency != null) {
    			salesContract.setContractCurrency(contractCurrency);
    		}
    		paymentTerms = paymentTerms == null ? contractCreator.getPaymentTerms() : paymentTerms;
    		if(paymentTerms != null) {
    			salesContract.setPaymentTerms(paymentTerms);
    		}
    		customer = customer == null ? contractCreator.getCustomer() : customer;
    		if(customer != null) {
	    		salesContract.setCustomer(customer);
    		}
    		salesRep = salesRep == null ? contractCreator.getSalesRep() : salesRep;
    		if(salesRep != null) {
    			salesContract.setSalesRep(salesRep);
    		}
    		broker = broker == null ? contractCreator.getBroker() : broker;
    		if(broker != null) {
    			salesContract.setBroker(broker);
    		}
    		supplier = supplier == null ? contractCreator.getSupplier() : supplier;
    		if(supplier != null) {
    			salesContract.setSupplier(supplier);
    		}
    		salesTaxTypeGroup = salesTaxTypeGroup == null 
    			? this.getSalesTaxTypeGroup(contract, contractCreator)
    			: salesTaxTypeGroup;
    		if(salesTaxTypeGroup != null) {
    			salesContract.setSalesTaxTypeGroup(salesTaxTypeGroup);
    		}
    	}
    	this.reapplyContractCreator(
    		contract,
    		contractCreator
    	);
    	return contract;
    }

    /**
     * Create / update depot references.
     * 
     * @param contract
     * @param contractCreator
     * @throws ServiceException
     */
    public void createOrUpdateDepotReferences(
    	AbstractContract contract,
    	ContractCreator contractCreator
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
    	// Update DepotReferences
    	{
	        List<Depot> depots = new ArrayList<Depot>();
	        DepotReferenceQuery depotReferenceQuery = (DepotReferenceQuery)pm.newQuery(DepotReference.class);
	        for(DepotReference depotReference: contract.<DepotReference>getDepotReference(depotReferenceQuery)) {
	        	depots.add(depotReference.getDepot());
	        }
	        for(DepotReference depotReference: contractCreator.<DepotReference>getDepotReference(depotReferenceQuery)) {
	        	if(!depots.contains(depotReference.getDepot())) {
	                Cloneable.getInstance().cloneObject(
	                    depotReference,
	                    contract,
	                    "depotReference",
	                    null,
	                    "property",
	                    contract.getOwningUser(),
	                    contract.getOwningGroup()
	                );
	        	}
	        }
    	}
    	// Update DepotPositionReferences
    	{
	        List<DepotPosition> depotPositions = new ArrayList<DepotPosition>();
	        DepotPositionReferenceQuery depotPositionReferenceQuery = (DepotPositionReferenceQuery)pm.newQuery(DepotPositionReference.class);
	        for(DepotPositionReference depotPositionReference: contract.<DepotPositionReference>getDepotReference(depotPositionReferenceQuery)) {
	        	depotPositions.add(depotPositionReference.getDepotPosition());
	        }
	        for(DepotPositionReference depotPositionReference: contractCreator.<DepotPositionReference>getDepotReference(depotPositionReferenceQuery)) {
	        	if(!depotPositions.contains(depotPositionReference.getDepotPosition())) {
	                Cloneable.getInstance().cloneObject(
	                    depotPositionReference,
	                    contract,
	                    "depotReference",
	                    null,
	                    "property",
	                    contract.getOwningUser(),
	                    contract.getOwningGroup()
	                );
	        	}
	        }
    	}
    }

    /**Get principal groups which are assigned by default to given newly created contract.
     * 
     * @param contract
     * @param contractGroups
     * @return
     * @throws ServiceException
     */
    public Set<PrincipalGroup> getDefaultOwningGroups(
    	AbstractContract contract,
    	List<ContractGroup> contractGroups
    ) throws ServiceException {
        Set<PrincipalGroup> owningGroups = new HashSet<PrincipalGroup>();
        for(ContractGroup contractGroup: contractGroups) {
            List<PrincipalGroup> groups = contractGroup.getOwningGroup();
            for(PrincipalGroup group: groups) {
                if(contractGroup.getName().endsWith(PRIVATE_GROUP_SUFFIX)) {
                	String contractGroupName = contractGroup.getName().substring(0, contractGroup.getName().indexOf("~"));
                	String principalGroupName = group.getName().substring(0, group.getName().indexOf("."));
                	if(contractGroupName.equals(principalGroupName)) {
                		owningGroups.add(group);
                	}
                } else {                         	
                	owningGroups.add(group);
                }
            }
        }
        return owningGroups;
    }

    /**
     * Re-apply contract creator on given contract.
     * 
     * @param contract
     * @param contractCreator
     * @throws ServiceException
     */
    public void reapplyContractCreator(
    	AbstractContract contract,
    	ContractCreator contractCreator
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(contract);
    	if(contractCreator == null) {
    		contractCreator = contract.getLastAppliedCreator();
    	}
    	if(contractCreator != null) {
    		List<ContractGroup> contractGroups = contractCreator.<ContractGroup>getContractGroup(); 
			Set<PrincipalGroup> owningGroups = this.getDefaultOwningGroups(
				contract,
				contractGroups
			);
        	if(!owningGroups.containsAll(contract.getOwningGroup()) || !contract.getOwningGroup().containsAll(owningGroups)) {
                contract.getOwningGroup().clear();
                contract.getOwningGroup().addAll(owningGroups);
        	}
			// Assign contract to groups
	        Collection<ContractGroupAssignment> existingGroupAssignments = contract.getGroupAssignment();
	        List<ContractGroup> excludeGroups = new ArrayList<ContractGroup>();
	        for(ContractGroupAssignment existingGroupAssignment: existingGroupAssignments) {
	            if(existingGroupAssignment.getContractGroup() != null) {
	                excludeGroups.add(
	                    existingGroupAssignment.getContractGroup()
	                );
	            }
	        }
	        // Add new group assignments
	        for(ContractGroup contractGroup: contractGroups) {
	            if(!excludeGroups.contains(contractGroup)) {
	    			ContractGroupAssignment assignment = pm.newInstance(ContractGroupAssignment.class);
	    			assignment.setName(contractGroup.getName());
	    			assignment.setContractGroup(contractGroup);
	    			assignment.getOwningGroup().addAll(owningGroups);
	    			contract.addGroupAssignment(
	    				this.getUidAsString(),
	    				assignment
	    			);
	            }
	        }
	        // Update DepotReferences
	        this.createOrUpdateDepotReferences(
	        	contract,
	        	contractCreator
	        );
	        // Update PropertySet
	        Collection<PropertySet> existingPropertySets = contract.getPropertySet();
	        List<String> excludePropertySets = new ArrayList<String>();
	        for(PropertySet existingPropertySet: existingPropertySets) {
	            excludePropertySets.add(
	                existingPropertySet.getName()
	            );
	        }                    
	        Collection<PropertySet> propertySets = contractCreator.getPropertySet();
	        for(PropertySet propertySet: propertySets) {
	            if(!excludePropertySets.contains(propertySet.getName())) {
	                Cloneable.getInstance().cloneObject(
	                    propertySet,
	                    contract,
	                    "propertySet",
	                    null,
	                    "property",
	                    contract.getOwningUser(),
	                    contract.getOwningGroup()
	                );
	            }
	        }
	        contract.setLastAppliedCreator(contractCreator);
    	}
    }
   
    /**
     * Store callback for account assignments.
     * 
     * @param contract
     * @throws ServiceException
     */
    protected void updateAccountAssignmentContract(
    	AccountAssignmentContract accountAssignment
    ) throws ServiceException {
    }

    /**
     * Delete callback for account assignments.
     * 
     * @param contract
     * @throws ServiceException
     */
    protected void removeAccountAssignmentContract(
    	AccountAssignmentContract accountAssignment,
    	boolean preDelete
    ) throws ServiceException {
        if(!preDelete) {
        	accountAssignment.refDelete();
        }    	
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
		if(object instanceof AbstractContract) {
			this.removeContract((AbstractContract)object, preDelete);
		} else if(object instanceof AccountAssignmentContract) {
			this.removeAccountAssignmentContract((AccountAssignmentContract)object, preDelete);
		} else if(object instanceof SalesContractPosition) {
			this.removeSalesContractPosition((SalesContractPosition)object, true, preDelete);
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
		if(object instanceof SalesContract) {
			this.updateContract((AbstractContract)object);
			this.updateSalesContract((SalesContract)object);
		} else if(object instanceof AbstractContract) {
			this.updateContract((AbstractContract)object);
		} else if(object instanceof SalesContractPosition) {
			this.updateSalesContractPosition((SalesContractPosition)object);
		} else if(object instanceof AccountAssignmentContract) {
			this.updateAccountAssignmentContract((AccountAssignmentContract)object);
		} else if(object instanceof PhoneNumber) {
			Addresses.getInstance().updatePhoneNumber((PhoneNumber)object);
		} else if(object instanceof EMailAddress) {
			Addresses.getInstance().updateEMailAddress((EMailAddress)object);			
		}
	}

    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public final static short STATUS_CODE_OK = 0;
    public final static short STATUS_CODE_ERROR = 1;
    
    // Min/Max Quantity Handling
    public static final int MIN_MAX_QUANTITY_HANDLING_NA = 0;
    public static final int MIN_MAX_QUANTITY_HANDLING_LIMIT = 3;
    
    // Pricing Status
    public static final short PRICING_STATE_NA = 0;
    public static final short PRICING_STATE_DIRTY = 10;
    public static final short PRICING_STATE_OK = 20;
    
    // Booking texts
    public static final String BOOKING_TEXT_NAME_RETURN_GOODS = "return goods";
    public static final String BOOKING_TEXT_NAME_DELIVER_GOODS = "deliver goods";
            
    public static final String CALCULATION_RULE_NAME_DEFAULT = "Default";
        
    // Standard contract types
    public static final short CONTRACT_TYPE_LEAD = 1;
    public static final short CONTRACT_TYPE_OPPORTUNITY = 2;
    public static final short CONTRACT_TYPE_QUOTE = 3;
    public static final short CONTRACT_TYPE_SALES_ORDER = 4;
    public static final short CONTRACT_TYPE_INVOICE = 5;
    public static final short CONTRACT_TYPE_SALESVOLUME_CONTRACT = 6;
    public static final short CONTRACT_TYPE_GENERIC_CONTRACT = 7;

    public static final BigDecimal HUNDRED = new BigDecimal("100.0");
    
	public static final String PRIVATE_GROUP_SUFFIX = "~Private";
    
    public static final String DEFAULT_GET_POSITION_AMOUNTS_SCRIPT = 
        "//<pre>\n" + 
        "public static org.opencrx.kernel.contract1.jmi1.GetPositionAmountsResult getPositionAmounts(\n" + 
        "    org.openmdx.base.accessor.jmi.cci.RefPackage_1_0 rootPkg,\n" +
        "    org.opencrx.kernel.contract1.jmi1.CalculationRule calculationRule,\n" +  
        "    org.opencrx.kernel.contract1.jmi1.SalesContractPosition position,\n" +
        "    java.math.BigDecimal pricePerUnit,\n" +
        "    java.math.BigDecimal quantity,\n" +
        "    java.math.BigDecimal uomScaleFactor,\n" +
        "    java.math.BigDecimal discount,\n" +
        "    java.lang.Boolean discountIsPercentage,\n" +
        "    java.lang.Short discountCalculationType,\n" +
        "    java.math.BigDecimal salesTaxRate\n" +
        ") {\n" +
        "    return org.opencrx.kernel.backend.Contracts.getPositionAmounts(\n" + 
        "        rootPkg,\n" +
        "        calculationRule,\n" +
        "        position,\n" +
        "        pricePerUnit,\n" +
        "        quantity,\n" +
        "        uomScaleFactor,\n" +
        "        discount,\n" +
        "        discountIsPercentage,\n" +
        "        discountCalculationType,\n" +        
        "        salesTaxRate\n" +
        "   );\n" +
        "}//</pre>";
        
    public static final String DEFAULT_GET_CONTRACT_AMOUNTS_SCRIPT = 
        "//<pre>\n" + 
        "public static org.opencrx.kernel.contract1.jmi1.GetContractAmountsResult getContractAmounts(\n" + 
        "    org.openmdx.base.accessor.jmi.cci.RefPackage_1_0 rootPkg,\n" +
        "    org.opencrx.kernel.contract1.jmi1.CalculationRule calculationRule,\n" +  
        "    org.opencrx.kernel.contract1.jmi1.SalesContract contract,\n" +
        "    Integer[] lineItemNumbers,\n" +
        "    java.math.BigDecimal[] positionBaseAmounts,\n" +
        "    java.math.BigDecimal[] positionDiscountAmounts,\n" +
        "    java.math.BigDecimal[] positionTaxAmounts,\n" +
        "    java.math.BigDecimal[] positionAmounts,\n" +
        "    java.math.BigDecimal[] salesCommissions,\n" +
        "    Boolean[] salesCommissionIsPercentages\n" +
        ") {\n" +
        "    return org.opencrx.kernel.backend.Contracts.getContractAmounts(\n" +
        "        rootPkg,\n" +
        "        calculationRule,\n" +
        "        contract,\n" +
        "        lineItemNumbers,\n" +
        "        positionBaseAmounts,\n" +
        "        positionDiscountAmounts,\n" +
        "        positionTaxAmounts,\n" +
        "        positionAmounts,\n" +
        "        salesCommissions,\n" +
        "        salesCommissionIsPercentages\n" +
        "    );\n" +
        "}//</pre>";
        
}
