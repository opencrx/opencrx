/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: Products
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
package org.opencrx.kernel.backend;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.base.jmi1.AttributeFilterProperty;
import org.opencrx.kernel.code1.jmi1.ValidateObjectResult;
import org.opencrx.kernel.contract1.jmi1.AbstractContract;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.opencrx.kernel.generic.OpenCrxException;
import org.opencrx.kernel.product1.cci2.AbstractPriceLevelQuery;
import org.opencrx.kernel.product1.cci2.DiscountRuleQuery;
import org.opencrx.kernel.product1.cci2.PriceListEntryQuery;
import org.opencrx.kernel.product1.cci2.PriceModifierQuery;
import org.opencrx.kernel.product1.cci2.PricingRuleQuery;
import org.opencrx.kernel.product1.cci2.ProductBasePriceQuery;
import org.opencrx.kernel.product1.cci2.ProductConfigurationQuery;
import org.opencrx.kernel.product1.cci2.ProductQuery;
import org.opencrx.kernel.product1.jmi1.AbstractFilterProduct;
import org.opencrx.kernel.product1.jmi1.AbstractPriceLevel;
import org.opencrx.kernel.product1.jmi1.AbstractProduct;
import org.opencrx.kernel.product1.jmi1.AccountAssignment;
import org.opencrx.kernel.product1.jmi1.CategoryFilterProperty;
import org.opencrx.kernel.product1.jmi1.ConfiguredProduct;
import org.opencrx.kernel.product1.jmi1.DefaultSalesTaxTypeFilterProperty;
import org.opencrx.kernel.product1.jmi1.DisabledFilterProperty;
import org.opencrx.kernel.product1.jmi1.DiscountOrigin;
import org.opencrx.kernel.product1.jmi1.DiscountPriceModifier;
import org.opencrx.kernel.product1.jmi1.DiscountRule;
import org.opencrx.kernel.product1.jmi1.DiscountSchedule;
import org.opencrx.kernel.product1.jmi1.EMailAddress;
import org.opencrx.kernel.product1.jmi1.GetPriceLevelResult;
import org.opencrx.kernel.product1.jmi1.LinearPriceModifier;
import org.opencrx.kernel.product1.jmi1.PhoneNumber;
import org.opencrx.kernel.product1.jmi1.PriceListEntry;
import org.opencrx.kernel.product1.jmi1.PriceModifier;
import org.opencrx.kernel.product1.jmi1.PriceModifierReference;
import org.opencrx.kernel.product1.jmi1.PriceUomFilterProperty;
import org.opencrx.kernel.product1.jmi1.PricingRule;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.opencrx.kernel.product1.jmi1.ProductClassificationFilterProperty;
import org.opencrx.kernel.product1.jmi1.ProductConfiguration;
import org.opencrx.kernel.product1.jmi1.ProductConfigurationType;
import org.opencrx.kernel.product1.jmi1.ProductFilterProperty;
import org.opencrx.kernel.product1.jmi1.ProductPhasePriceLevel;
import org.opencrx.kernel.product1.jmi1.ProductQueryFilterProperty;
import org.opencrx.kernel.product1.jmi1.RelatedProduct;
import org.opencrx.kernel.product1.jmi1.SalesTaxType;
import org.opencrx.kernel.product1.jmi1.UriAddress;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.opencrx.kernel.utils.ScriptUtils;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.accessor.jmi.cci.RefPackage_1_0;
import org.openmdx.base.dataprovider.layer.persistence.jdbc.spi.Database_1_Attributes;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.marshalling.Marshaller;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Quantifier;
import org.openmdx.base.rest.cci.QueryExtensionRecord;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class Products extends AbstractImpl {

    //-------------------------------------------------------------------------
	public static void register(
	) {
		registerImpl(new Products());
	}
	
    //-------------------------------------------------------------------------
	public static Products getInstance(
	) throws ServiceException {
		return getInstance(Products.class);
	}

	//-------------------------------------------------------------------------
	protected Products(
	) {
		
	}
	
    //-------------------------------------------------------------------------
    public class PriceLevelMarshaller
        implements Marshaller {

        public PriceLevelMarshaller(
            String nameReplacementRegex,
            String nameReplacementValue,
            Date validFrom,
            Date validTo
        ) {
           this.nameReplacementRegex = nameReplacementRegex;
           this.nameReplacementValue = nameReplacementValue;
           this.validFrom = validFrom;
           this.validTo = validTo;           
        }
        
        public Object marshal(
        	Object s
        ) throws ServiceException {
            AbstractPriceLevel priceLevel = PersistenceHelper.clone((AbstractPriceLevel)s);
            if(
                (priceLevel.getName() != null) && 
                (this.nameReplacementRegex != null) && 
                (this.nameReplacementValue != null)
            ) {
                String name = priceLevel.getName();
                if(name != null) {
                    priceLevel.setName(
                        name.replaceAll(
                            this.nameReplacementRegex, 
                            this.nameReplacementValue
                        )
                    );
                }
            }
            if(this.validFrom != null) {
                priceLevel.setValidFrom(
                    this.validFrom
                );
            }
            priceLevel.setValidTo(null);
            if(this.validTo != null) {
                priceLevel.setValidTo(
                     this.validTo
                );
            }
            priceLevel.setFinal(
                Boolean.FALSE
            );
            return priceLevel;                       
        }
        
        public Object unmarshal(Object s) {
            throw new UnsupportedOperationException();
        }
                
        private final String nameReplacementRegex;
        private final String nameReplacementValue;
        private final Date validFrom;
        private final Date validTo;
        
    }
    
    //-------------------------------------------------------------------------
    public class ProductPhasePriceLevelMarshaller
        implements Marshaller {

        public ProductPhasePriceLevelMarshaller(
            String nameReplacementRegex,
            String nameReplacementValue,
            String productPhaseKey
        ) {
           this.nameReplacementRegex = nameReplacementRegex;
           this.nameReplacementValue = nameReplacementValue;
           this.productPhaseKey = productPhaseKey;
        }
        
        public Object marshal(
        	Object s
        ) throws ServiceException {
            ProductPhasePriceLevel priceLevel = PersistenceHelper.clone((ProductPhasePriceLevel)s);
            if(
                (priceLevel.getName() != null) && 
                (this.nameReplacementRegex != null) && 
                (this.nameReplacementValue != null)
            ) {
                String name = priceLevel.getName();
                if(name != null) {
                    priceLevel.setName(
                        name.replaceAll(
                            this.nameReplacementRegex, 
                            this.nameReplacementValue
                        )
                    );
                }
            }
            if(this.productPhaseKey != null) {
                priceLevel.setProductPhaseKey(
                    this.productPhaseKey
                );
            }
            priceLevel.setFinal(
                Boolean.FALSE
            );
            return priceLevel;                       
        }
        
        public Object unmarshal(Object s) {
            throw new UnsupportedOperationException();
        }
                
        private final String nameReplacementRegex;
        private final String nameReplacementValue;
        private final String productPhaseKey;
        
    }
    
    /**
     * Find pricing rule.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     * 
     */
    @Deprecated
    public org.opencrx.kernel.product1.jmi1.PricingRule findPricingRule(
        String name,
        org.opencrx.kernel.product1.jmi1.Segment segment,
        javax.jdo.PersistenceManager pm
    ) {
    	return this.findPricingRule(name, segment);
    }

    /**
     * Find pricing rule.
     * 
     * @param name
     * @param segment
     * @return
     */
    public org.opencrx.kernel.product1.jmi1.PricingRule findPricingRule(
        String name,
        org.opencrx.kernel.product1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        PricingRuleQuery pricingRuleQuery = (PricingRuleQuery)pm.newQuery(PricingRule.class);
        pricingRuleQuery.name().equalTo(name);
        List<org.opencrx.kernel.product1.jmi1.PricingRule> pricingRules = segment.getPricingRule(pricingRuleQuery);
        return pricingRules.isEmpty()
            ? null
            : pricingRules.iterator().next();
    }

    public PricingRule getDefaultPricingRule(
        org.opencrx.kernel.product1.jmi1.Segment productSegment
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(productSegment);
    	PricingRuleQuery query = (PricingRuleQuery)pm.newQuery(PricingRule.class);
    	query.thereExistsIsDefault().isTrue();
    	List<PricingRule> pricingRules = productSegment.getPricingRule(query);
    	return pricingRules.isEmpty() ?
    		null :
    			pricingRules.iterator().next();
    }

	/**
	 * Init sales tax type.
	 * 
	 * @param name
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.product1.jmi1.SalesTaxType findSalesTaxType(
		String name,
		org.opencrx.kernel.product1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
		org.opencrx.kernel.product1.cci2.SalesTaxTypeQuery query =
		    (org.opencrx.kernel.product1.cci2.SalesTaxTypeQuery)pm.newQuery(org.opencrx.kernel.product1.jmi1.SalesTaxType.class);
		query.name().equalTo(name);
		Collection<SalesTaxType> salesTaxTypes = segment.getSalesTaxType(query);
		if(!salesTaxTypes.isEmpty()) {
			return (org.opencrx.kernel.product1.jmi1.SalesTaxType)salesTaxTypes.iterator().next();
		}
		return null;
	}
    
	/**
	 * Init sales tax type.
	 * 
	 * @param name
	 * @param rate
	 * @param segment
	 * @param pm
	 * @return
	 */
	public org.opencrx.kernel.product1.jmi1.SalesTaxType initSalesTaxType(
		String name,
		java.math.BigDecimal rate,
		org.opencrx.kernel.product1.jmi1.Segment segment
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(segment);		
		org.opencrx.kernel.product1.jmi1.SalesTaxType salesTaxType = this.findSalesTaxType(
		    name,
			segment
		);
		if(salesTaxType != null) return salesTaxType;
		try {
			pm.currentTransaction().begin();
			salesTaxType = pm.newInstance(org.opencrx.kernel.product1.jmi1.SalesTaxType.class);
			salesTaxType.setName(name);
			salesTaxType.setRate(rate);
			salesTaxType.getOwningGroup().addAll(
				segment.getOwningGroup()
			);
			segment.addSalesTaxType(
				Contracts.getInstance().getUidAsString(),
				salesTaxType
			);
			pm.currentTransaction().commit();
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
		}
		return salesTaxType;
	}

    /**
     * @return Returns the product segment.
     */
    public org.opencrx.kernel.product1.jmi1.Segment getProductSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
            new Path("xri:@openmdx:org.opencrx.kernel.product1/provider/" + providerName + "/segment/" + segmentName)
        );
    }

    //-----------------------------------------------------------------------
    public PricingRule initPricingRule(
        String pricingRuleName,
        String description,
        String getPriceLevelScript,
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        org.opencrx.kernel.product1.jmi1.Segment productSegment = this.getProductSegment(
            pm, 
            providerName, 
            segmentName
        );
        PricingRule pricingRule = null;
        if((pricingRule = this.findPricingRule(pricingRuleName, productSegment, pm)) != null) {
            return pricingRule;            
        }                
        pm.currentTransaction().begin();
        pricingRule = pm.newInstance(PricingRule.class);
        pricingRule.setName(pricingRuleName);
        pricingRule.setDescription(description);
        pricingRule.setGetPriceLevelScript(getPriceLevelScript);
        pricingRule.setDefault(true);
        pricingRule.getOwningGroup().addAll(
            productSegment.getOwningGroup()
        );
        productSegment.addPricingRule(
            false,
            UUIDConversion.toUID(UUIDs.newUUID()),
            pricingRule
        );                        
        pm.currentTransaction().commit();        
        return pricingRule;
    }
    
    /**
     * Clone ProductConfigurations stored under fromIdentity to toIdentity.
     * 
     * @return number of cloned configurations
     */
    public int cloneProductConfigurationSet(
    	org.opencrx.kernel.product1.jmi1.ProductConfigurationSet from,
    	SalesContractPosition to,
        boolean cloneDefaultOnly,
        boolean updateCurrentConfig,
        org.opencrx.security.realm1.jmi1.User owningUser,
        List<org.opencrx.security.realm1.cci2.PrincipalGroup> owningGroup        
    ) throws ServiceException {
    	Collection<ProductConfiguration> configurations = from.getConfiguration();
    	for(ProductConfiguration configuration: configurations) {
            if(            	
                !cloneDefaultOnly ||                    
                ((configuration.isDefault() != null) && configuration.isDefault().booleanValue())
            ) {
            	Cloneable.getInstance().cloneObject(
            		configuration, 
            		to, 
            		"configuration", 
            		null, 
            		null,
            		owningUser,
            		owningGroup
            	);
            }
        }
        // configType
    	if(to instanceof ConfiguredProduct) {
	        ((ConfiguredProduct)to).setConfigType(
	            from.getConfigType()
	        );
	        // currentConfig
	        if(updateCurrentConfig) {
	        	configurations = ((ConfiguredProduct)to).getConfiguration();
	            ProductConfiguration defaultConfiguration = null;   
	            for(ProductConfiguration configuration: configurations) {
	                if((configuration.isDefault() != null) && configuration.isDefault().booleanValue()) {
	                    defaultConfiguration = configuration;
	                    break;
	                }
	            }
	            if(defaultConfiguration != null) {
	                ((ConfiguredProduct)to).setCurrentConfig(
	                	defaultConfiguration
	                );
	            }
	        }
        }
        return configurations.size();
    }
    
    /**
     * Set configuration type for given product. This updates / amends all
     * missing configurations and configuration properties of the product.
     * Existing configuration properties are not modified.
     * 
     * @param product
     * @param configurationTypeSet
     * @throws ServiceException
     */
    public void setConfigurationType(
        org.opencrx.kernel.product1.jmi1.Product product,
        org.opencrx.kernel.product1.jmi1.ProductConfigurationTypeSet configurationTypeSet
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(product);
    	Collection<ProductConfigurationType> configurationTypes = configurationTypeSet.getConfigurationType();
    	for(ProductConfigurationType configurationType: configurationTypes) {
    		// Lookup product configuration matching name and configuration type
    		ProductConfigurationQuery configurationQuery = (ProductConfigurationQuery)pm.newQuery(ProductConfiguration.class);
    		configurationQuery.name().equalTo(configurationType.getName());
    		configurationQuery.thereExistsConfigType().equalTo(configurationType);
    		configurationQuery.orderByCreatedAt().descending();
    		List<ProductConfiguration> configurations = product.getConfiguration(configurationQuery);
    		ProductConfiguration configuration = null;
    		if(configurations.isEmpty()) {
    			// Add configuration if it does not exist
    			configuration = pm.newInstance(ProductConfiguration.class);
        		product.addConfiguration(
        			this.getUidAsString(),
        			configuration
        		);
    		} else {
    			configuration = configurations.iterator().next();
    		}
    		configuration.setDefault(configurationType.isDefault());
    		configuration.setName(configurationType.getName());
    		configuration.setDescription(configurationType.getDescription());
    		configuration.setValidFrom(configurationType.getValidFrom());
    		configuration.setValidTo(configurationType.getValidTo());
    		configuration.setConfigType(configurationType);
    		// Enable if disabled
    		if(Boolean.TRUE.equals(configuration.isDisabled())) {
    			configuration.setDisabled(false);
    		}
    		Collection<org.opencrx.kernel.base.jmi1.Property> properties = configurationType.getProperty();
    		for(org.opencrx.kernel.base.jmi1.Property property: properties) {
    			org.opencrx.kernel.base.cci2.PropertyQuery propertyQuery = (org.opencrx.kernel.base.cci2.PropertyQuery)pm.newQuery(org.opencrx.kernel.base.jmi1.Property.class);
    			propertyQuery.name().equalTo(property.getName());
    			// Add configuration property if it does not exist by name
    			if(configuration.getProperty(propertyQuery).isEmpty()) {
	    			Cloneable.getInstance().cloneObject(
	    				property, 
	    				configuration, 
	    				"property", 
	    				null, 
	    				"",
	            		null, // owning user
	               		null // owning group    				
	    			);
    			}
    		}    		
    	}
        product.setConfigType(configurationTypeSet);
    }
    
    /**
     * Set ProductConfigurationSet::configType to null and disable all configurations with
     * ::configurationType element of the currently set configuration type set. No configurations
     * and configuration properties are removed. Override this method for custom-specific behavior.
     * 
     * @param productConfigurationSet
     * @throws ServiceException
     */
    public void unsetConfigurationType(
        org.opencrx.kernel.product1.jmi1.ProductConfigurationSet productConfigurationSet
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(productConfigurationSet);
    	if(productConfigurationSet.getConfigType() != null) {
    		org.opencrx.kernel.product1.jmi1.ProductConfigurationTypeSet configurationTypeSet = productConfigurationSet.getConfigType();
    		for(ProductConfigurationType configurationType: configurationTypeSet.<ProductConfigurationType>getConfigurationType()) {
    			ProductConfigurationQuery configurationQuery = (ProductConfigurationQuery)pm.newQuery(ProductConfiguration.class);
    			configurationQuery.thereExistsConfigType().equalTo(configurationType);
    			for(ProductConfiguration configuration: productConfigurationSet.getConfiguration(configurationQuery)) {
    				configuration.setDisabled(true);
    			}
    		}
	    	productConfigurationSet.setConfigType(null);
	    	if(productConfigurationSet instanceof ConfiguredProduct) {
	    		((ConfiguredProduct)productConfigurationSet).setCurrentConfig(null);
	    	}
    	}
    }

    /**
     * Set ProductConfiguration::configType to null and disable configuration. 
     * By default existing configuration properties are not removed. Override this
     * method for custom-specific behavior.
     * 
     * @param productConfiguration
     * @throws ServiceException
     */
    public void unsetConfigurationType(
        org.opencrx.kernel.product1.jmi1.ProductConfiguration productConfiguration
    ) throws ServiceException {
    	productConfiguration.setConfigType(null);
    	productConfiguration.setDisabled(true);
    }

    //-------------------------------------------------------------------------
    public ProductQuery getFilteredProductQuery(
        AbstractFilterProduct productFilter,
        ProductQuery query,
        boolean forCounting
    ) throws ServiceException {
        Collection<ProductFilterProperty> filterProperties = productFilter.getProductFilterProperty();
        boolean hasQueryFilterClause = false;
        for(ProductFilterProperty filterProperty: filterProperties) {
            Boolean isActive = filterProperty.isActive();            
            if((isActive != null) && isActive.booleanValue()) {
                // Query filter
                if(filterProperty instanceof ProductQueryFilterProperty) {
                	ProductQueryFilterProperty p = (ProductQueryFilterProperty)filterProperty;
                	QueryExtensionRecord queryFilter = PersistenceHelper.newQueryExtension(query);
                	queryFilter.setClause(
                		(forCounting ? Database_1_Attributes.HINT_COUNT : "") + p.getClause()
                	);
                    queryFilter.getStringParam().addAll(
                    	p.getStringParam()
                    );
                    queryFilter.getIntegerParam().addAll(
                    	p.getIntegerParam()
                    );
                    queryFilter.getDecimalParam().addAll(
                    	p.getDecimalParam()
                    );
                    queryFilter.setBooleanParam(
                    	new Boolean[]{p.getBooleanParam().isEmpty()
                        	? Boolean.FALSE : 
                            p.getBooleanParam().iterator().next()
                    	}
                    );
                    queryFilter.getDateParam().addAll(
                    	p.getDateParam()
                    );
                    queryFilter.getDateTimeParam().addAll(
                    	p.getDateTimeParam()
                    );
                    hasQueryFilterClause = true;
                }
                // Attribute filter
                else if(filterProperty instanceof AttributeFilterProperty) {
                	AttributeFilterProperty attributeFilterProperty = (AttributeFilterProperty)filterProperty;
                    // Get filterOperator, filterQuantor
                    short operator = attributeFilterProperty.getFilterOperator();
                    operator = operator == 0 ? 
                    	ConditionType.IS_IN.code() : 
                    	operator;
                    short quantor = attributeFilterProperty.getFilterQuantor();
                    quantor = quantor == 0 ? 
                    	Quantifier.THERE_EXISTS.code() : 
                    	quantor;   
                    
                    if(filterProperty instanceof PriceUomFilterProperty) {
                    	PriceUomFilterProperty p = (PriceUomFilterProperty)filterProperty;
                    	switch(Quantifier.valueOf(quantor)) {
	                		case FOR_ALL:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.forAllPriceUom().elementOf(p.getPriceUom()); 
	                					break;
	                				case IS_NOT_IN:
	                					query.forAllPriceUom().notAnElementOf(p.getPriceUom()); 
	                					break;
	                				default:
	                					query.forAllPriceUom().elementOf(p.getPriceUom()); 
	                					break;
	                			}
	                			break;
	                		default:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.thereExistsPriceUom().elementOf(p.getPriceUom()); 
	                					break;
	                				case IS_NOT_IN:
	                					query.thereExistsPriceUom().notAnElementOf(p.getPriceUom()); 
	                					break;
	                				default:
	                					query.thereExistsPriceUom().elementOf(p.getPriceUom()); 
	                					break;
	                			}
	                			break;
                    	}
                	}
                    else if(filterProperty instanceof CategoryFilterProperty) {
                    	CategoryFilterProperty p = (CategoryFilterProperty)filterProperty;
                    	switch(Quantifier.valueOf(quantor)) {
	                		case FOR_ALL:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.forAllCategory().elementOf(p.getCategory()); 
	                					break;
	                				case IS_LIKE:
	                					query.forAllCategory().like(p.getCategory()); 
	                					break;
	                				case IS_GREATER:
	                					query.forAllCategory().greaterThan(p.getCategory().get(0)); 
	                					break;
	                				case IS_GREATER_OR_EQUAL:
	                					query.forAllCategory().greaterThanOrEqualTo(p.getCategory().get(0)); 
	                					break;
	                				case IS_LESS:
	                					query.forAllCategory().lessThan(p.getCategory().get(0)); 
	                					break;
	                				case IS_LESS_OR_EQUAL:
	                					query.forAllCategory().lessThanOrEqualTo(p.getCategory().get(0)); 
	                					break;
	                				case IS_NOT_IN:
	                					query.forAllCategory().notAnElementOf(p.getCategory()); 
	                					break;
	                				case IS_UNLIKE:
	                					query.forAllCategory().unlike(p.getCategory()); 
	                					break;
	                				default:
	                					query.forAllCategory().elementOf(p.getCategory()); 
	                					break;
	                			}
	                			break;
                    		default:
                    			switch(ConditionType.valueOf(operator)) {
                    				case IS_IN: 
                    					query.thereExistsCategory().elementOf(p.getCategory()); 
                    					break;
                    				case IS_LIKE: 
                    					query.thereExistsCategory().like(p.getCategory()); 
                    					break;
                    				case IS_GREATER:
                    					query.thereExistsCategory().greaterThan(p.getCategory().get(0)); 
                    					break;
                    				case IS_GREATER_OR_EQUAL:
                    					query.thereExistsCategory().greaterThanOrEqualTo(p.getCategory().get(0)); 
                    					break;
                    				case IS_LESS:
                    					query.thereExistsCategory().lessThan(p.getCategory().get(0)); 
                    					break;
                    				case IS_LESS_OR_EQUAL:
                    					query.thereExistsCategory().lessThanOrEqualTo(p.getCategory().get(0)); 
                    					break;
                    				case IS_NOT_IN:
                    					query.thereExistsCategory().notAnElementOf(p.getCategory()); 
                    					break;
                    				case IS_UNLIKE: 
                    					query.thereExistsCategory().unlike(p.getCategory()); 
                    					break;
                    				default:
                    					query.thereExistsCategory().elementOf(p.getCategory()); 
                    					break;
                    			}
                    			break;
                    	}
                    }
                    else if(filterProperty instanceof ProductClassificationFilterProperty) {
                    	ProductClassificationFilterProperty p = (ProductClassificationFilterProperty)filterProperty;
                    	switch(Quantifier.valueOf(quantor)) {
	                		case FOR_ALL:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.forAllClassification().elementOf(p.getClassification()); 
	                					break;
	                				case IS_NOT_IN:
	                					query.forAllClassification().notAnElementOf(p.getClassification()); 
	                					break;
	                				default:
	                					query.forAllClassification().elementOf(p.getClassification()); 
	                					break;
	                			}
	                			break;
	                		default:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.thereExistsClassification().elementOf(p.getClassification()); 
	                					break;
	                				case IS_NOT_IN:
	                					query.thereExistsClassification().notAnElementOf(p.getClassification()); 
	                					break;
	                				default:
	                					query.thereExistsClassification().elementOf(p.getClassification()); 
	                					break;
	                			}
	                			break;
                    	}
                    }
                    else if(filterProperty instanceof DefaultSalesTaxTypeFilterProperty) {
                    	DefaultSalesTaxTypeFilterProperty p = (DefaultSalesTaxTypeFilterProperty)filterProperty;
                    	switch(Quantifier.valueOf(quantor)) {
	                		case FOR_ALL:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.forAllSalesTaxType().elementOf(p.getSalesTaxType()); 
	                					break;
	                				case IS_NOT_IN:
	                					query.forAllSalesTaxType().notAnElementOf(p.getSalesTaxType()); 
	                					break;
	                				default:
	                					query.forAllSalesTaxType().elementOf(p.getSalesTaxType()); 
	                					break;
	                			}
	                			break;
	                		default:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.thereExistsSalesTaxType().elementOf(p.getSalesTaxType()); 
	                					break;
	                				case IS_NOT_IN:
	                					query.thereExistsSalesTaxType().notAnElementOf(p.getSalesTaxType()); 
	                					break;
	                				default:
	                					query.thereExistsSalesTaxType().elementOf(p.getSalesTaxType()); 
	                					break;
	                			}
	                			break;
                    	}                    	
                    }
                    else if(filterProperty instanceof DisabledFilterProperty) {
                    	DisabledFilterProperty p = (DisabledFilterProperty)filterProperty;                    	
                    	switch(Quantifier.valueOf(quantor)) {
	                		case FOR_ALL:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.forAllDisabled().equalTo(p.isDisabled()); 
	                					break;
	                				case IS_NOT_IN: 
	                					query.forAllDisabled().equalTo(!p.isDisabled());	                						
	                					break;
	                				default:
	                					break;
	                			}
	                			break;
	                		default:
	                			switch(ConditionType.valueOf(operator)) {
	                				case IS_IN: 
	                					query.thereExistsDisabled().equalTo(p.isDisabled()); 
	                					break;
	                				case IS_NOT_IN: 
	                					query.thereExistsDisabled().equalTo(!p.isDisabled()); 
	                					break;
	                				default:
	                					break;
	                			}
	                			break;
                    	}
                	}
                }
            }
        }
        if(!hasQueryFilterClause && forCounting) {
        	QueryExtensionRecord queryFilter = PersistenceHelper.newQueryExtension(query);
        	queryFilter.setClause(
        		Database_1_Attributes.HINT_COUNT + "(1=1)"
        	);
        }
        return query;
    }
    
    //-------------------------------------------------------------------------    
    public List<PriceListEntry> getPriceListEntries(
    	AbstractPriceLevel priceLevel,
        boolean applyPriceFilter
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(priceLevel);
    	PriceListEntryQuery priceListEntryQuery = (PriceListEntryQuery)pm.newQuery(PriceListEntry.class);
    	priceListEntryQuery.thereExistsPriceLevel().equalTo(priceLevel);
        if(applyPriceFilter) {
        	priceListEntryQuery.priceCurrency().equalTo(priceLevel.getPriceCurrency());        	
            if(!priceLevel.getPriceUsage().isEmpty()) {
                priceListEntryQuery.thereExistsUsage().elementOf(
                	priceLevel.getPriceUsage()
                );
            }
        }
        org.opencrx.kernel.product1.jmi1.Segment productSegment =
        	(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
        		priceLevel.refGetPath().getPrefix(5)
        	);
        return productSegment.getPriceListEntry(priceListEntryQuery);
    }
    
    //-------------------------------------------------------------------------    
    public List<ProductBasePrice> findPrices(
        Product product,
        AbstractPriceLevel priceLevel,
        org.opencrx.kernel.uom1.jmi1.Uom uom,
        boolean useBasedOnPriceLevel
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(product);
    	ProductBasePriceQuery priceQuery = (ProductBasePriceQuery)pm.newQuery(ProductBasePrice.class);
        // Get basedOn price level if useBasedOnPriceLevel==true
        priceLevel = useBasedOnPriceLevel && (priceLevel.getBasedOn() != null) ? 
        	priceLevel.getBasedOn() : 
        	priceLevel;                    
        // filter on priceCurrency if defined
        priceQuery.priceCurrency().equalTo(
        	priceLevel.getPriceCurrency()
        );
        // filter on priceUsage if defined
        if((priceLevel != null) && !priceLevel.getPriceUsage().isEmpty()) {
        	priceQuery.thereExistsUsage().elementOf(
        		priceLevel.getPriceUsage()
        	);
        }
        if(priceLevel != null) {
        	priceQuery.thereExistsPriceLevel().equalTo(
        		priceLevel
        	);
        }
        if(uom != null) {
        	priceQuery.uom().equalTo(uom);
        }
        return product.getBasePrice(priceQuery);
    }

    //-------------------------------------------------------------------------
    public int clonePriceLevel(
        AbstractPriceLevel priceLevel,
        Short processingMode,
        Map<String,Marshaller> priceLevelMarshallers
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(priceLevel);
    	org.opencrx.kernel.product1.jmi1.Segment productSegment =
    		(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
    			priceLevel.refGetPath().getParent().getParent()
    		);
        List<AbstractPriceLevel> dependentPriceLevels = this.getDependentPriceLevels(
            priceLevel, 
            true
        );
        if(
            (processingMode == PROCESSING_MODE_CLONE_PRICELEVEL_NO_PRICES) ||
            (processingMode == PROCESSING_MODE_CLONE_PRICELEVEL_INCLUDE_PRICES)
        ) {
            // Mapping <source price level identity, cloned price level identity>
            Map<Path,AbstractPriceLevel> clonedLevels = new HashMap<Path,AbstractPriceLevel>();
            // Clone price level
            AbstractPriceLevel clonedPriceLevel = (AbstractPriceLevel)Cloneable.getInstance().cloneObject(
            	priceLevel, 
            	productSegment, 
            	"priceLevel", 
            	priceLevelMarshallers, 
            	"priceModifier, accountFilterProperty, assignedAccount, productFilterProperty",
            	priceLevel.getOwningUser(),
            	priceLevel.getOwningGroup()
            );
            clonedLevels.put(
                priceLevel.refGetPath(), 
                clonedPriceLevel
            );            
            // Clone dependent price levels
            for(AbstractPriceLevel dependentPriceLevel: dependentPriceLevels) {
            	AbstractPriceLevel clonedDependentPriceLevel = (AbstractPriceLevel)Cloneable.getInstance().cloneObject(
                	dependentPriceLevel, 
                	productSegment, 
                	"priceLevel", 
                	priceLevelMarshallers, 
                	"priceModifier, accountFilterProperty, assignedAccount, productFilterProperty",
                	dependentPriceLevel.getOwningUser(),
                	dependentPriceLevel.getOwningGroup()
                );
                clonedLevels.put(
                    dependentPriceLevel.refGetPath(), 
                    clonedDependentPriceLevel
                );
            }
            // Update priceLevel.basedOn references
            for(AbstractPriceLevel level: clonedLevels.values()) {
                if(
                    (level.getBasedOn() != null) &&
                    clonedLevels.keySet().contains(level.getBasedOn().refGetPath())
                ) {
                	AbstractPriceLevel clonedLevel = clonedLevels.get(level.getBasedOn().refGetPath());
                    level.setBasedOn(clonedLevel);
                }                
            }
            // Clone prices if price level is basic price level
            if(
                (processingMode == PROCESSING_MODE_CLONE_PRICELEVEL_INCLUDE_PRICES) &&
                (priceLevel.getBasedOn() == null) 
            ) {
                List<PriceListEntry> priceListEntries = this.getPriceListEntries(
                    priceLevel,
                    true
                );
                for(PriceListEntry priceListEntry: priceListEntries) {
                	org.opencrx.kernel.product1.jmi1.ProductBasePrice price = priceListEntry.getBasePrice();
                	// Do not clone disabled prices
                	if((price.isDisabled() == null) || !price.isDisabled().booleanValue()) {
	                	org.opencrx.kernel.product1.jmi1.Product product = 
	                		(org.opencrx.kernel.product1.jmi1.Product)pm.getObjectById(
	                			price.refGetPath().getParent().getParent()
	                		);
	                	// Do not clone prices for disabled products 
	                	if((product.isDisabled() == null) || !product.isDisabled().booleanValue()) {
		                	org.opencrx.kernel.product1.jmi1.ProductBasePrice clonedPrice = PersistenceHelper.clone(price);
		                	clonedPrice.getPriceLevel().clear();
		                	clonedPrice.getPriceLevel().add(
		                		clonedPriceLevel
		                	);
		                	product.addBasePrice(
		                		false,
		                		this.getUidAsString(),
		                		clonedPrice
		                	);
	                	}
                	}
                }
            }
        }
        return dependentPriceLevels.size() + 1;        
    }

    //-------------------------------------------------------------------------
    public int clonePriceLevel(
        org.opencrx.kernel.product1.jmi1.PriceLevel priceLevel,
        Short processingMode,
        String nameReplacementRegex,
        String nameReplacementValue,
        Date validFrom,
        Date validTo
    ) throws ServiceException {
        Map<String,Marshaller> priceLevelMarshallers = new HashMap<String,Marshaller>();
        priceLevelMarshallers.put(
            "org:opencrx:kernel:product1:PriceLevel",
            new PriceLevelMarshaller(
                nameReplacementRegex,
                nameReplacementValue,
                validFrom,
                validTo                    
            )
        );
        return this.clonePriceLevel(
            priceLevel, 
            processingMode, 
            priceLevelMarshallers
        );
    }
    
    //-------------------------------------------------------------------------
    public int cloneProductPhasePriceLevel(
        org.opencrx.kernel.product1.jmi1.ProductPhasePriceLevel priceLevel,
        Short processingMode,
        String nameReplacementRegex,
        String nameReplacementValue,
        String productPhaseKey
    ) throws ServiceException {
        Map<String,Marshaller> priceLevelMarshallers = new HashMap<String,Marshaller>();
        priceLevelMarshallers.put(
            "org:opencrx:kernel:product1:ProductPhasePriceLevel",
            new ProductPhasePriceLevelMarshaller(
                nameReplacementRegex,
                nameReplacementValue,
                productPhaseKey
            )
        );
        return this.clonePriceLevel(
            priceLevel, 
            processingMode, 
            priceLevelMarshallers
        );
    }
    
    //-------------------------------------------------------------------------
    public List<AbstractPriceLevel> getDependentPriceLevels(
        AbstractPriceLevel priceLevel,
        boolean recursive
    ) throws ServiceException {
    	Set<AbstractPriceLevel> dependentPriceLevels = new HashSet<AbstractPriceLevel>();
    	PersistenceManager pm = JDOHelper.getPersistenceManager(priceLevel);
        org.opencrx.kernel.product1.jmi1.Segment productSegment =
        	(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
        		priceLevel.refGetPath().getPrefix(5)
        	);
        AbstractPriceLevelQuery priceLevelQuery = (AbstractPriceLevelQuery)pm.newQuery(AbstractPriceLevel.class);
        priceLevelQuery.thereExistsBasedOn().equalTo(priceLevel);        
        List<AbstractPriceLevel> levels = productSegment.getPriceLevel(priceLevelQuery);
        dependentPriceLevels.addAll(levels);
        if(recursive) {
	        for(AbstractPriceLevel level: levels) {
	        	dependentPriceLevels.addAll(
	        		this.getDependentPriceLevels(
	        			level,
	        			true
	        		)
	        	);
	        }
        }
        return new ArrayList<AbstractPriceLevel> (dependentPriceLevels);
    }
    
    //-------------------------------------------------------------------------
    public int calculatePrices(
        org.opencrx.kernel.product1.jmi1.AbstractPriceLevel priceLevel,
        Short processingMode,
        Date includeProductsModifiedSince
    ) throws ServiceException {
        if(processingMode == PROCESSING_MODE_TEST) {
            return this.calculatePrices(
                priceLevel, 
                true,
                includeProductsModifiedSince
            );
        }
        else if(processingMode == PROCESSING_MODE_PROCESS) {
            return this.calculatePrices(
                priceLevel, 
                false,
                includeProductsModifiedSince
            );            
        }
        else if(processingMode == PROCESSING_MODE_PROCESS_INCLUDE_DEPENDENT) {
            int numberProcessed = this.calculatePrices(
                priceLevel, 
                false,
                includeProductsModifiedSince
            );                                        	            
        	List<AbstractPriceLevel> dependentPriceLevels = this.getDependentPriceLevels(
                priceLevel,
                false
            );        	
            for(AbstractPriceLevel dependentPriceLevel: dependentPriceLevels) {
                numberProcessed += this.calculatePrices(
                    dependentPriceLevel, 
                    PROCESSING_MODE_PROCESS_INCLUDE_DEPENDENT,
                    includeProductsModifiedSince
                );                                        	
            }
            return numberProcessed;
        }
        return 0;
    }
    
    /**
     * Apply price modifier.
     * 
     * @param modifiedPrice
     * @param priceModifier
     */
    public void applyPriceModifier(
        ProductBasePrice modifiedPrice,
        PriceModifier priceModifier
    ) {
        BigDecimal quantityFromPrice = modifiedPrice.getQuantityFrom();
        BigDecimal quantityToPrice = modifiedPrice.getQuantityTo();
        BigDecimal quantityFromModifier = priceModifier.getQuantityFrom();
        quantityFromModifier = quantityFromModifier == null ? quantityFromPrice : quantityFromModifier;
        BigDecimal quantityToModifier = priceModifier.getQuantityTo();
        quantityToModifier = quantityToModifier == null ? quantityToPrice : quantityToModifier;
        // Apply modifier if quantity range is larger than the price quantity range
        if(
            ((quantityFromModifier == null) || (quantityFromPrice == null) || (quantityFromModifier.compareTo(quantityFromPrice) <= 0)) &&
            ((quantityToModifier == null) || (quantityToPrice == null) || (quantityToModifier.compareTo(quantityToPrice) >= 0))
        ) {
            if(priceModifier instanceof DiscountPriceModifier) {
            	DiscountPriceModifier discountPriceModifier = (DiscountPriceModifier)priceModifier;
                boolean discountIsPercentageModifier = 
                	(discountPriceModifier.isDiscountIsPercentage() != null) && 
                	discountPriceModifier.isDiscountIsPercentage().booleanValue();
                boolean discountIsPercentagePrice =
                    (modifiedPrice.isDiscountIsPercentage() != null) && 
                    modifiedPrice.isDiscountIsPercentage().booleanValue();
                if(discountIsPercentageModifier == discountIsPercentagePrice) {
                    modifiedPrice.setDiscount(
                        discountPriceModifier.getDiscount()
                    );
                }
            } else if(priceModifier instanceof LinearPriceModifier) {
            	LinearPriceModifier linearPriceModifier = (LinearPriceModifier)priceModifier;
                BigDecimal priceMultiplier = linearPriceModifier.getPriceMultiplier();
                priceMultiplier = priceMultiplier == null ? BigDecimal.ZERO : priceMultiplier;
                BigDecimal priceOffset = linearPriceModifier.getPriceOffset();
                priceOffset = priceOffset == null ? BigDecimal.ZERO : priceOffset;
                BigDecimal roundingFactor = linearPriceModifier.getRoundingFactor();
                roundingFactor = roundingFactor == null ? BigDecimal.ONE : roundingFactor;
                BigDecimal price = modifiedPrice.getPrice();
                price = price == null ? BigDecimal.ZERO : price;
                price = 
                    new BigDecimal(
                        price.multiply(priceMultiplier).multiply(roundingFactor).add(
                            new BigDecimal("0.5")
                        ).toBigInteger()
                    ).divide(
                        roundingFactor, price.scale(), BigDecimal.ROUND_FLOOR 
                    ).add(
                        priceOffset
                    );
                modifiedPrice.setPrice(price);
            } else if(priceModifier instanceof PriceModifierReference) {
            	this.applyPriceModifier(
            		modifiedPrice,
            		((PriceModifierReference)priceModifier).getPriceModifier()
            	);
            }
        }    	
    }

    /**
     * Apply price modifiers.
     * 
     * @param modifiedPrice
     * @param priceModifiers
     */
    public void applyPriceModifiers(
        ProductBasePrice modifiedPrice,
        Collection<PriceModifier> priceModifiers
    ) {
        for(PriceModifier priceModifier: priceModifiers) {
        	this.applyPriceModifier(
        		modifiedPrice,
        		priceModifier
        	);
        }        
    }
    
    //-------------------------------------------------------------------------
    public int calculatePrices(
        AbstractPriceLevel priceLevel,
        boolean testOnly,
        Date includeProductsModifiedSince
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(priceLevel);
        if(priceLevel.isFinal()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.PRODUCT_OPERATION_NOT_ALLOWED_FOR_FINAL_PRICE_LEVEL,
                "Operation is not allowed for final price level.",
                new BasicException.Parameter("param0", priceLevel)
            );                                                                
        }
        // No calculation required for base price levels
        boolean isBasePriceLevel = priceLevel.getBasedOn() == null;
        PriceModifierQuery priceModifierQuery = (PriceModifierQuery)pm.newQuery(PriceModifier.class);
        priceModifierQuery.orderByOrderIndex().ascending();
        Collection<PriceModifier> priceModifiers = priceLevel.getPriceModifier(priceModifierQuery);
        // Get all products matching the product filter
        ProductQuery productQuery = (ProductQuery)pm.newQuery(Product.class);
        if(includeProductsModifiedSince != null) {
        	productQuery.modifiedAt().greaterThanOrEqualTo(includeProductsModifiedSince);
        }
        productQuery = this.getFilteredProductQuery(
        	priceLevel,
        	productQuery,
        	false	
        );
        org.opencrx.kernel.product1.jmi1.Segment productSegment =
        	(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
        		priceLevel.refGetPath().getParent().getParent()
        	);        		
        List<Product> filteredProducts = productSegment.getProduct(productQuery);
        int numberProcessed = 0;
        // Iterate all matching products and calculate prices
        for(Product product: filteredProducts) {
            List<ProductBasePrice> basedOnPrices = this.findPrices(
                product,
                priceLevel,
                null, // uom
                !isBasePriceLevel // useBasedOnPriceLevel
            );
            List<ProductBasePrice> existingPrices = this.findPrices(
                product,
                priceLevel, 
                null, // uom
                false
            );
            // Collect new prices and add after iteration to avoid ConcurrentModificationException on iterator
            List<ProductBasePrice> newPrices = new ArrayList<ProductBasePrice>();
            for(ProductBasePrice basePrice: basedOnPrices) {
                if(!testOnly) {
                    // If no price modifiers are defined do not create new price objects. 
                    // Only add the price level to the existing price. A copy can be enforced
                    // by adding a 1:1 price modifier, e.g. a LinearPriceModifier with priceMultiplier=1.0 and
                    // priceOffset=0.0.
                    if(priceModifiers.isEmpty()) {
                        List<AbstractPriceLevel> priceLevels = basePrice.getPriceLevel();
                        if(!priceLevels.contains(priceLevel)) {
                            priceLevels.add(priceLevel);
                        }
                    }
                    // Create new price and apply price modifiers
                    else {
                        ProductBasePrice newPrice = PersistenceHelper.clone(basePrice);
                        this.applyPriceModifiers(
                            newPrice, 
                            priceModifiers
                        );
                        newPrice.getPriceLevel().clear();
                        newPrice.getPriceLevel().add(priceLevel);
                        newPrice.setPriceCurrency(priceLevel.getPriceCurrency());
                        newPrice.getUsage().clear();
                        newPrice.getUsage().addAll(priceLevel.getPriceUsage());
                        // Create new price if no price for price level exists
                        if(existingPrices.isEmpty()) {
                        	newPrices.add(
                        		newPrice
                        	);
                        }
                        // Update existing prices
                        else {
                            for(ProductBasePrice existingPrice: existingPrices) {
                                // Compare price, discount, discountIsPercentage, quantityFrom, quantityTo
                                boolean priceIsEqual = true;
                                priceIsEqual &= Utils.areEqual(existingPrice.getPrice(), newPrice.getPrice());
                                priceIsEqual &= Utils.areEqual(existingPrice.getDiscount(), newPrice.getDiscount());
                                priceIsEqual &= Utils.areEqual(existingPrice.getQuantityFrom(), newPrice.getQuantityFrom());
                                priceIsEqual &= Utils.areEqual(existingPrice.getQuantityTo(), newPrice.getQuantityTo());
                                priceIsEqual &= Utils.areEqual(existingPrice.isDiscountIsPercentage(), newPrice.isDiscountIsPercentage());
                                if(!priceIsEqual) {
                                	// Update price if it is not shared with another price level
                                	if(existingPrice.getPriceLevel().size() == 1) {
	                                	existingPrice.setPrice(newPrice.getPrice());
	                                	existingPrice.setDiscount(newPrice.getDiscount());
	                                	existingPrice.setQuantityFrom(newPrice.getQuantityFrom());
	                                	existingPrice.setQuantityTo(newPrice.getQuantityTo());
	                                	existingPrice.setDiscountIsPercentage(newPrice.isDiscountIsPercentage());
                                	}
                                	// Remove price level from existing price and create new price
                                	else {
                                		existingPrice.getPriceLevel().remove(priceLevel);
                                		newPrices.add(
                                			newPrice
                                		);
                                	}
                                }
                            }
                        }
                    }
                }
            }
            for(ProductBasePrice newPrice: newPrices) {
            	product.addBasePrice(
            		false,
            		this.getUidAsString(),
            		newPrice
            	);                                		
            }
            numberProcessed++;
        }
        return numberProcessed;
    }
    
    //-------------------------------------------------------------------------
    public int removePrices(
        AbstractPriceLevel priceLevel
    ) throws ServiceException {
        if(priceLevel.isFinal()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.PRODUCT_OPERATION_NOT_ALLOWED_FOR_FINAL_PRICE_LEVEL,
                "Operation is not allowed for final price level.",
                new BasicException.Parameter("param0", priceLevel)
            );                                                                
        }
        List<PriceListEntry> priceListEntries = this.getPriceListEntries(
            priceLevel,
            false
        );
        int numberProcessed = 0;
        for(PriceListEntry priceListEntry: priceListEntries) {
        	if(priceListEntry.getBasePrice() != null) {
        		ProductBasePrice basePrice = priceListEntry.getBasePrice();
        		basePrice.getPriceLevel().remove(priceLevel);
        		if(basePrice.getPriceLevel().isEmpty()) {
        			basePrice.refDelete();
        		}
        		numberProcessed++;
        	}
        }
        return numberProcessed;
    }
    
    //-------------------------------------------------------------------------
    public int removePrices(
        org.opencrx.kernel.product1.jmi1.AbstractPriceLevel priceLevel,
        Short processingMode        
    ) throws ServiceException {
        int numberProcessed = 0;
        if(processingMode == PROCESSING_MODE_PROCESS) {
            numberProcessed = this.removePrices(
                priceLevel 
            );
        }
        else if(processingMode == PROCESSING_MODE_PROCESS_INCLUDE_DEPENDENT) {
        	this.removePrices(
                priceLevel 
            );
            List<AbstractPriceLevel> dependentPriceLevels = this.getDependentPriceLevels(
                priceLevel, 
                true
            );
            for(AbstractPriceLevel dependentPriceLevel: dependentPriceLevels) {
            	this.removePrices(
                    dependentPriceLevel 
                );
            }
            numberProcessed = 1 + dependentPriceLevels.size();
        }
        return numberProcessed;
    }
    
    /**
     * Remove price levels.
     * 
     * @param priceLevel
     * @param processingMode
     * @param preDelete
     * @return
     * @throws ServiceException
     */
    public int removePriceLevels(
        AbstractPriceLevel priceLevel,
        Short processingMode,
        boolean preDelete
    ) throws ServiceException {
        int numberProcessed = 0;
        if(processingMode == PROCESSING_MODE_PROCESS) {
            // Remove prices of price level (no dependent)
        	this.removePrices(
                priceLevel, 
                processingMode
            );
        	this.removePriceLevel(
                priceLevel,
                preDelete
            );
            numberProcessed = 1;
        } else if(processingMode == PROCESSING_MODE_PROCESS_INCLUDE_DEPENDENT) {
            List<AbstractPriceLevel> dependentPriceLevels = this.getDependentPriceLevels(
                priceLevel, 
                false
            );
            for(AbstractPriceLevel dependentPriceLevel: dependentPriceLevels) {
            	this.removePriceLevels(
                    dependentPriceLevel,
                    processingMode,
                    false
                );                
            }
        	this.removePrices(
                priceLevel, 
                PROCESSING_MODE_PROCESS
            );
        	this.removePriceLevel(
                priceLevel,
                preDelete
            );
            numberProcessed = 1 + dependentPriceLevels.size();
        }
        return numberProcessed;
    }

    /**
     * Propagate validity to price level and dependent price levels, recursively.
     * 
     * @param priceLevel
     * @param validFrom
     * @param validTo
     * @throws ServiceException
     */
    public void propagateValidity(
        AbstractPriceLevel priceLevel,
        Date validFrom,
        Date validTo
    ) throws ServiceException {
    	if(validFrom != null) {
    		priceLevel.setValidFrom(validFrom);
    	}
    	if(validTo != null) {
    		priceLevel.setValidTo(validTo);
    	}
        List<AbstractPriceLevel> dependentPriceLevels = this.getDependentPriceLevels(
            priceLevel, 
            true // recursive
        );
        for(AbstractPriceLevel dependentPriceLevel: dependentPriceLevels) {
        	if(validFrom != null) {
        		dependentPriceLevel.setValidFrom(validFrom);
        	}
        	if(validTo != null) {
        		dependentPriceLevel.setValidTo(validTo);
        	}
        }
    }

    /**
     * Create initial prices for price level.
     * 
     * @param priceLevel
     * @param processingMode
     * @param uom
     * @param includeProductsCreatedSince
     * @return
     * @throws ServiceException
     */
    public int createInitialPrices(
        AbstractPriceLevel priceLevel,
        Short processingMode,
        org.opencrx.kernel.uom1.jmi1.Uom uom,
        Date includeProductsCreatedSince
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(priceLevel);    	
        // Price level must not be final
        if(priceLevel.isFinal() != null && priceLevel.isFinal()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.PRODUCT_OPERATION_NOT_ALLOWED_FOR_FINAL_PRICE_LEVEL,
                "Operation is not allowed for final price level.",
                new BasicException.Parameter("param0", priceLevel.getName())
            );                                                                
        }
        // Get price modifiers
        PriceModifierQuery priceModifierQuery = (PriceModifierQuery)pm.newQuery(PriceModifier.class);
        priceModifierQuery.orderByOrderIndex().ascending();
        Collection<PriceModifier> priceModifiers = priceLevel.getPriceModifier(priceModifierQuery);
        // Get all products matching the product filter
        ProductQuery productQuery = (ProductQuery)pm.newQuery(Product.class);
        if(includeProductsCreatedSince != null) {
        	productQuery.modifiedAt().greaterThanOrEqualTo(
        		includeProductsCreatedSince
        	);
        }
        productQuery = this.getFilteredProductQuery(
        	priceLevel,
        	productQuery,
        	false
        );
        org.opencrx.kernel.product1.jmi1.Segment productSegment =
        	(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
        		priceLevel.refGetPath().getPrefix(5)
        	);
        List<Product> products = productSegment.getProduct(productQuery);
        int numberProcessed = 0;
        for(Product product: products) {
            List<ProductBasePrice> basePrices = this.findPrices(
                product,
                priceLevel,
                uom,
                false
            );
            if(basePrices.isEmpty()) {
                ProductBasePrice basePrice = pm.newInstance(ProductBasePrice.class);
                basePrice.getPriceLevel().add(
                    priceLevel
                );
                basePrice.getUsage().addAll(
                    priceLevel.getPriceUsage()
                );
                basePrice.setPrice(BigDecimal.ZERO);                
                basePrice.setPriceCurrency(
                    priceLevel.getPriceCurrency()
                );
                basePrice.setDiscount(BigDecimal.ZERO);
                basePrice.setDiscountIsPercentage(Boolean.FALSE);
                basePrice.setUom(uom);
                this.applyPriceModifiers(
                    basePrice, 
                    priceModifiers
                );
                if(processingMode == PROCESSING_MODE_PROCESS) {
                	product.addBasePrice(
                		this.getUidAsString(),
                		basePrice
                	);
                }
                numberProcessed++;
            }
        }
        return numberProcessed;
    }
    
    /**
     * Get price level.
     * 
     * @param clazz
     * @param signature
     * @param params
     * @return
     * @throws ServiceException
     * @throws NoSuchMethodException
     */
    protected GetPriceLevelResult getPriceLevel(
    	Class<?> clazz,
    	Class<?>[] signature,
    	Object[] params
    ) throws ServiceException, NoSuchMethodException {
        try {
            Method m = clazz.getMethod(
                "getPriceLevel",
                signature
            );
            org.opencrx.kernel.product1.jmi1.GetPriceLevelResult result = 
                (org.opencrx.kernel.product1.jmi1.GetPriceLevelResult)m.invoke(
                    null,
                    params
                );
            return result;
        } catch(InvocationTargetException e) {
        	ServiceException e0 = new ServiceException(
            	BasicException.toStackedException(
            		e.getTargetException(), 
            		e,
	                OpenCrxException.DOMAIN,
	                OpenCrxException.PRODUCT_GET_PRICELEVEL_SCRIPT_ERROR,
	                "Error invoking method getPriceLevel",
	                new BasicException.Parameter("param0", e.getMessage())
            	));
            e0.log();
            throw e0;
        } catch(IllegalAccessException e) {
            ServiceException e0 = new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.PRODUCT_GET_PRICELEVEL_SCRIPT_ERROR,
                "Illegal access when invoking method getPriceLevel",
                new BasicException.Parameter("param0", e.getMessage())
            );
            e0.log();
            throw e0;
        }    	
    }

    /**
     * Get price level for given product and context.
     * 
     * @param pricingRule
     * @param overridePriceLevel
     * @param contract
     * @param product
     * @param priceUom
     * @param quantity
     * @param pricingDate
     * @return
     * @throws ServiceException
     */
    public GetPriceLevelResult getPriceLevel(
        PricingRule pricingRule,
        SalesContract contract,
        SalesContractPosition position,
        AbstractProduct product,
        Uom priceUom,
        BigDecimal quantity,
        Short contractPositionState,
        Date pricingDate,
        AbstractPriceLevel overridePriceLevel
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(pricingRule);
        String script = pricingRule.getGetPriceLevelScript() == null || pricingRule.getGetPriceLevelScript().isEmpty() 
        	? PRICING_RULE_GET_PRICE_LEVEL_SCRIPT_LOWEST_PRICE 
        	: pricingRule.getGetPriceLevelScript().intern();
        Class<?> clazz = ScriptUtils.getClass(script);
        try {
        	return this.getPriceLevel(
        		clazz,
        		new Class[]{
                    RefPackage_1_0.class,
                    PricingRule.class,
                    SalesContract.class,
                    SalesContractPosition.class,
                    AbstractProduct.class,
                    Uom.class,
                    java.math.BigDecimal.class,
                    java.lang.Short.class,
                    java.util.Date.class,
                    AbstractPriceLevel.class
                },
        		new Object[]{
                    Utils.getProductPackage(pm).refOutermostPackage(),
                    pricingRule,
                    contract,
                    position,
                    product,
                    priceUom,
                    quantity,
                    contractPositionState,
                    pricingDate,
                    overridePriceLevel
                }
        	);
        } catch(NoSuchMethodException ignore) {
        	try {
	        	return this.getPriceLevel(
	        		clazz,
	        		new Class[] {
	                    RefPackage_1_0.class,
	                    PricingRule.class,
	                    AbstractContract.class,
	                    AbstractProduct.class,
	                    Uom.class,
	                    java.math.BigDecimal.class,
	                    java.util.Date.class
	                },
	        		new Object[] {
	                    Utils.getProductPackage(pm).refOutermostPackage(),
	                    pricingRule,
	                    contract,
	                    product,
	                    priceUom,
	                    quantity,
	                    pricingDate
	                }
	        	);
	        } catch(NoSuchMethodException e) {
	            ServiceException e0 = new ServiceException(
	                OpenCrxException.DOMAIN,
	                OpenCrxException.PRODUCT_GET_PRICELEVEL_SCRIPT_ERROR,
	                "Missing method getPriceLevel",
	                new BasicException.Parameter("param0", e.getMessage())
	            );
	            e0.log();
	            throw e0;
	        }
        }
    }

    //-------------------------------------------------------------------------
    public boolean hasDependentPriceLevels(
        AbstractPriceLevel priceLevel
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(priceLevel);
    	org.opencrx.kernel.product1.jmi1.Segment productSegment =
    		(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
    			priceLevel.refGetPath().getPrefix(5)
    		);
    	AbstractPriceLevelQuery priceLevelQuery = (AbstractPriceLevelQuery)pm.newQuery(AbstractPriceLevel.class);
    	priceLevelQuery.thereExistsBasedOn().equalTo(
    		priceLevel
    	);
    	List<AbstractPriceLevel> levels = productSegment.getPriceLevel(priceLevelQuery);
    	for(AbstractPriceLevel level: levels) {
    		if(!JDOHelper.isDeleted(level)) {
    			return true;
    		}
    	}
    	return false;
    }
    
    //-------------------------------------------------------------------------
    public void removePriceLevel(
        AbstractPriceLevel priceLevel,
        boolean preDelete
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(priceLevel);
    	org.opencrx.kernel.product1.jmi1.Segment productSegment =
    		(org.opencrx.kernel.product1.jmi1.Segment)pm.getObjectById(
    			priceLevel.refGetPath().getPrefix(5)
    		);    	
        if(this.hasDependentPriceLevels(priceLevel)) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.PRODUCT_OPERATION_NOT_ALLOWED_FOR_BASEDON_PRICE_LEVEL,
                "Can not delete price level. Other price levels are based on this price level.",
                new BasicException.Parameter("param0", priceLevel.getName())
            );                                                                            
        }
        // Check for prices which are assigned to price level
        PriceListEntryQuery priceListEntryQuery = (PriceListEntryQuery)pm.newQuery(PriceListEntry.class);
        priceListEntryQuery.thereExistsPriceLevel().equalTo(priceLevel);
        List<PriceListEntry> priceListEntries = productSegment.getPriceListEntry(priceListEntryQuery);
        if(!priceListEntries.isEmpty()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.PRODUCT_OPERATION_NOT_ALLOWED_FOR_PRICE_LEVEL_HAVING_PRICES,
                "Can not delete price level. Price level has assigned prices.",
                new BasicException.Parameter("param0", priceLevel.getName())
            );                                                                                        
        }
        if(!preDelete) {
        	priceLevel.refDelete();
        }
    }
    
    //-------------------------------------------------------------------------
    public int countFilteredProduct(
        org.opencrx.kernel.product1.jmi1.AbstractFilterProduct productFilter
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(productFilter);
    	ProductQuery query = (ProductQuery)pm.newQuery(Product.class);
    	QueryExtensionRecord queryExtension = PersistenceHelper.newQueryExtension(query);
    	queryExtension.setClause(
    		Database_1_Attributes.HINT_COUNT + "(1=1)"
    	);    	
        List<Product> products = productFilter.getFilteredProduct(query);
        return products.size();
    }
       
    /**
     * Find best matching base price for given product and price level. If specified,
     * quantity must match [quantityFrom,quantityTo].
     * 
     * @param priceLevel
     * @param product
     * @param priceUom
     * @param quantity
     * @return
     * @throws ServiceException
     */
    public ProductBasePrice findProductBasePrice(
    	AbstractPriceLevel priceLevel,
    	AbstractProduct product,
    	Uom priceUom,
    	BigDecimal quantity
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(product);
    	ProductBasePrice listPrice = null;
    	ProductBasePriceQuery basePriceQuery = (ProductBasePriceQuery)pm.newQuery(ProductBasePrice.class);
    	basePriceQuery.forAllDisabled().isFalse();
    	basePriceQuery.thereExistsPriceLevel().equalTo(priceLevel);
    	basePriceQuery.uom().equalTo(priceUom);
    	SysLog.trace("Lookup of prices with filter", Arrays.asList(basePriceQuery));
    	List<ProductBasePrice> prices = product.getBasePrice(basePriceQuery);
        SysLog.trace("Matching prices found", prices);
        // Check quantity
        for(ProductBasePrice current: prices) {
        	SysLog.trace("Testing price", current);
            boolean quantityFromMatches =
                (current.getQuantityFrom() == null) || 
                (quantity == null) || 
                (current.getQuantityFrom().compareTo(quantity) <= 0);
            SysLog.trace("Quantity from matches", quantityFromMatches);
            boolean quantityToMatches = 
                (current.getQuantityTo() == null) || 
                (quantity == null) || 
                (current.getQuantityTo().compareTo(quantity) >= 0);
            SysLog.trace("Quantity to matches", quantityToMatches);
            if(quantityFromMatches && quantityToMatches) {
                listPrice = current;
                break;
            }
        }
        return listPrice;
    }
    
    /**
     * Get lowest price level.
     * 
     * @param rootPkg
     * @param pricingRule
     * @param contract
     * @param position
     * @param product
     * @param priceUom
     * @param quantity
     * @param pricingDate
     * @return
     */
	public static GetPriceLevelResult getLowestPricePriceLevel(
	    RefPackage_1_0 rootPkg,
	    PricingRule pricingRule,
	    SalesContract contract,
	    SalesContractPosition position,
	    AbstractProduct product,
	    Uom priceUom,
	    BigDecimal quantity,
	    Short contractPositionState,
	    Date pricingDate,
	    AbstractPriceLevel overridePriceLevel
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(pricingRule);
		String providerName = pricingRule.refGetPath().getSegment(2).toString();
		String segmentName = pricingRule.refGetPath().getSegment(4).toString();
		boolean loggingActivated = false;
		if(loggingActivated) {
			System.out.println("Pricing rule LowestPriceRule invoked: get lowest price");
		}
		short statusCode = (short) 0;
		String statusMessage = null;
		AbstractPriceLevel lowestPriceLevel = null;
		DiscountOrigin lowestPriceDiscountOrigin = null;
		ProductBasePrice lowestPrice = null;
		BigDecimal lowestPriceAfterDiscount = null;
		if(
			(pricingRule != null) &&
			(priceUom != null)
		) {
			SalesContract salesContract = (SalesContract) contract;
			try {
				List<AbstractPriceLevel> priceLevels = null;
				if(overridePriceLevel != null) {
					priceLevels = Collections.singletonList(overridePriceLevel);
				} else {
					org.opencrx.kernel.product1.jmi1.Segment productSegment = Products.getInstance().getProductSegment(pm, providerName, segmentName);
					AbstractPriceLevelQuery priceLevelFilter = (AbstractPriceLevelQuery)pm.newQuery(AbstractPriceLevel.class);
					priceLevelFilter.forAllDisabled().isFalse();
					priceLevels = productSegment.getPriceLevel(priceLevelFilter);
				}
				for(AbstractPriceLevel priceLevelCandidate : priceLevels) {
					boolean candidateMatches = false;
					boolean validFromMatches = (pricingDate == null) || (priceLevelCandidate.getValidFrom() == null) || priceLevelCandidate.getValidFrom().compareTo(pricingDate) <= 0;
					boolean validToMatches = (pricingDate == null) || (priceLevelCandidate.getValidTo() == null) || priceLevelCandidate.getValidTo().compareTo(pricingDate) >= 0;
					if(loggingActivated) {
						System.out.println("Testing price level=" + priceLevelCandidate.getName() + "; validFromMatches=" + validFromMatches + "; validToMatches=" + validToMatches);
					}
					candidateMatches = validFromMatches && validToMatches;
					if(
						candidateMatches &&
						(salesContract.getContractCurrency() == priceLevelCandidate.getPriceCurrency())
					) {
						if(loggingActivated) {
							System.out.println("Price level is candidate: " + priceLevelCandidate.getName());
						}
						candidateMatches = false;
						if(product != null) {
							List<DiscountOrigin> discountCandidates = new ArrayList<DiscountOrigin>();
							// A candidate price level must either have a customer matching the filter or a direct assignment
							boolean customerMatchesFilter = false;
							boolean customerIsAssigned = false;
							Account customer = salesContract.getCustomer();
							if(customer != null) {
								AccountQuery accountQuery = (AccountQuery) pm.newQuery(Account.class);
								accountQuery.forAllDisabled().isFalse();
								accountQuery.identity().equalTo(new String(customer.refMofId()));
								customerMatchesFilter = !priceLevelCandidate.getFilteredAccount(accountQuery).isEmpty();
								{
									// Check whether there exists an AccountAssignment
									Collection<AccountAssignment> accountAssignments = priceLevelCandidate.getAssignedAccount();
									for(AccountAssignment accountAssignment : accountAssignments) {
										try {
											boolean assignmentValidFromMatches = (accountAssignment.getValidFrom() == null) || (accountAssignment.getValidFrom().compareTo(pricingDate) <= 0);
											boolean assignmentValidToMatches = (accountAssignment.getValidTo() == null) || (accountAssignment.getValidTo().compareTo(pricingDate) >= 0);
											if(
												(accountAssignment.getAccount() == customer) &&
												assignmentValidFromMatches &&
												assignmentValidToMatches
											) {
												customerIsAssigned = true;
												// AccountAssignment is also a discount candidate
												discountCandidates.add(accountAssignment);
												break;
											}
										} catch (Exception ignore) {}
									}
								}
							}
							// The price level is a candidate
							if((customer == null) || customerMatchesFilter || customerIsAssigned) {
								// if customer is set it must either be assigned or filtered
								if(loggingActivated) {
									System.out.println("Customer is " + (customer == null ? "undefined" : "") + (customerMatchesFilter ? "filtered " : "") + (customerIsAssigned ? "assigned" : ""));
								}
								// test whether candidate price level defines lower price
								ProductBasePrice basePrice = Products.getInstance().findProductBasePrice(
									priceLevelCandidate,
									product,
									priceUom,
									quantity
								);
								if(basePrice != null && basePrice.getPrice() != null) {
									// Collect discount candidates from DiscountSchedule
									if(priceLevelCandidate.getDiscountSchedule() != null) {
										DiscountSchedule discountSchedule = priceLevelCandidate.getDiscountSchedule();
										DiscountRuleQuery discountRuleQuery = (DiscountRuleQuery) pm.newQuery(DiscountRule.class);
										discountRuleQuery.forAllDisabled().isFalse();
										for(DiscountRule discountRule: discountSchedule.getDiscountRule(discountRuleQuery)) {
											if(discountRule.getCondition() != null) {
												ValidateObjectResult result = Codes.getInstance().validateObject(
													discountRule.getCondition(),
													(ContextCapable) position,
													new Date(),
												    Collections.<String> emptyList(),
												    null, // default anchor
												    Collections.<org.opencrx.kernel.code1.cci2.ValidatorCondition> emptyList(),
												    Collections.<org.opencrx.kernel.code1.cci2.ValidatorCondition> emptyList()
												);
												if(Boolean.TRUE.equals(result.isValid())) {
													discountCandidates.add(discountRule);
												}
											}
										}
									}
									// The base price is always a discount candidate
									discountCandidates.add(basePrice);
									// Find discount candidate giving lowest priceAfterDiscount
									for(DiscountOrigin discountCandidate : discountCandidates) {
										java.math.BigDecimal priceAfterDiscount = basePrice.getPrice();
										if(discountCandidate.getDiscount() != null) {
											if(Boolean.TRUE.equals(discountCandidate.isDiscountIsPercentage())) {
												priceAfterDiscount = priceAfterDiscount.multiply(BigDecimal.ONE.subtract(discountCandidate.getDiscount().divide(new BigDecimal(100.0))));
											} else {
												priceAfterDiscount = priceAfterDiscount.subtract(discountCandidate.getDiscount());
											}
										}
										if(
											(lowestPriceAfterDiscount == null) ||
											(priceAfterDiscount.compareTo(lowestPriceAfterDiscount) < 0)
										) {
											lowestPriceLevel = priceLevelCandidate;
											lowestPrice = basePrice;
											lowestPriceAfterDiscount = priceAfterDiscount;
											if(discountCandidate.getDiscount() != null) {
												lowestPriceDiscountOrigin = discountCandidate;
											}
											if(loggingActivated) {
												System.out.println("New lowest price {priceLevel: \"" + lowestPriceLevel.refGetPath() + "\", lowestPrice: \"" + lowestPrice.refGetPath() + "\", lowestPriceAfterDiscount: \"" + lowestPriceAfterDiscount + "\", lowestPriceDiscountOrigin: \"" + lowestPriceDiscountOrigin.refGetPath() + "\"}");
											}
										}
									}
								}
							} else {
								if(loggingActivated) {
									System.out.println("Customer is neither filtered nor assigned");
								}
							}
						} else {
							if(loggingActivated) {
								System.out.println("Product is null");
							}
							break;
						}
					}
				}
			} catch (Exception e) {
				statusCode = 1;
				ServiceException e0 = new org.openmdx.base.exception.ServiceException(e);
				statusMessage = e0.getMessage();
				e0.log();
			}
		}
		GetPriceLevelResult result = Structures.create(
			GetPriceLevelResult.class,
			Datatypes.member(GetPriceLevelResult.Member.basePrice, lowestPrice),
			Datatypes.member(GetPriceLevelResult.Member.discountOrigin, lowestPriceDiscountOrigin),
		    Datatypes.member(GetPriceLevelResult.Member.discount, lowestPriceDiscountOrigin == null ? null : lowestPriceDiscountOrigin.getDiscount()),
		    Datatypes.member(GetPriceLevelResult.Member.discountIsPercentage, lowestPriceDiscountOrigin == null ? null : lowestPriceDiscountOrigin.isDiscountIsPercentage()),
		    Datatypes.member(GetPriceLevelResult.Member.priceLevel, lowestPriceLevel),
		    Datatypes.member(GetPriceLevelResult.Member.statusCode, statusCode),
		    Datatypes.member(GetPriceLevelResult.Member.statusMessage, statusMessage)
		);
		return result;
	}

    /**
     * Update product callback.
     * 
     * @param product
     * @throws ServiceException
     */
    protected void updateProduct(
        Product product
    ) throws ServiceException {
    }
    
    /**
     * Remove product callback.
     * 
     * @param product
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeProduct(
        Product product,
        boolean preDelete
    ) throws ServiceException {
    }
    
    /**
     * Update product base price callback.
     * 
     * @param price
     * @throws ServiceException
     */
    protected void updateProductBasePrice(
        ProductBasePrice price
    ) throws ServiceException {
    }

    /**
     * Remove product base price callback.
     * 
     * @param price
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeProductBasePrice(
        ProductBasePrice price,
        boolean preDelete
    ) throws ServiceException {
    }

    /**
     * Update related product callback.
     * 
     * @param relatedProduct
     * @throws ServiceException
     */
    protected void updateRelatedProduct(
    	RelatedProduct relatedProduct
    ) throws ServiceException {
    }

    /**
     * Remove related product callback.
     * 
     * @param relatedProduct
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeRelatedProduct(
        RelatedProduct relatedProduct,
        boolean preDelete
    ) throws ServiceException {
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
		if(object instanceof Product) {
			this.removeProduct((Product)object, preDelete);
		} else if(object instanceof AbstractPriceLevel) {
			this.removePriceLevel((AbstractPriceLevel)object, preDelete);
		} else if(object instanceof ProductBasePrice) {
			this.removeProductBasePrice((ProductBasePrice)object, preDelete);
		} else if(object instanceof RelatedProduct) {
			this.removeRelatedProduct((RelatedProduct)object, preDelete);
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
		if(object instanceof Product) {
			this.updateProduct((Product)object);
		} else if(object instanceof RelatedProduct) {
			this.updateRelatedProduct((RelatedProduct)object);
		} else if(object instanceof ProductBasePrice) {
			this.updateProductBasePrice((ProductBasePrice)object);
		} else if(object instanceof PhoneNumber) {
    		Addresses.getInstance().updatePhoneNumber((PhoneNumber)object);
		} else if(object instanceof EMailAddress) {
    		Addresses.getInstance().updateEMailAddress((EMailAddress)object);			
		} else if(object instanceof UriAddress) {
    		Addresses.getInstance().updateUriAddress((UriAddress)object);
		}
	}

    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public final static int BATCHING_MODE_SIZE = 1000;
    
    public static final short PROCESSING_MODE_NA = 0;
    public static final short PROCESSING_MODE_TEST = 1;
    public static final short PROCESSING_MODE_PROCESS = 2;
    public static final short PROCESSING_MODE_PROCESS_INCLUDE_DEPENDENT = 3;
    public static final short PROCESSING_MODE_CLONE_PRICELEVEL_INCLUDE_PRICES = 4;
    public static final short PROCESSING_MODE_CLONE_PRICELEVEL_NO_PRICES = 5;
    
    public static final short STATUS_CODE_OK = 0;
    public static final short STATUS_CODE_ERROR = 1;
    
    public static final String PRICING_RULE_NAME_LOWEST_PRICE = "Lowest Price";
    public static final String PRICING_RULE_DESCRIPTION_LOWEST_PRICE = "Get price level which returns the lowest price of the given product, contract currency, pricing date and quantity. If the product is not defined return the price level which matches the contract currency, price uom and pricing date.";
    public static final String PRICING_RULE_GET_PRICE_LEVEL_SCRIPT_LOWEST_PRICE = 
    "//<pre>\n" + 
    "    public static org.opencrx.kernel.product1.jmi1.GetPriceLevelResult getPriceLevel(\n" +
    "    org.openmdx.base.accessor.jmi.cci.RefPackage_1_0 rootPkg,\n" +
    "    org.opencrx.kernel.product1.jmi1.PricingRule pricingRule,\n" +
    "    org.opencrx.kernel.contract1.jmi1.SalesContract contract,\n" +
    "    org.opencrx.kernel.contract1.jmi1.SalesContractPosition position,\n" +
    "    org.opencrx.kernel.product1.jmi1.AbstractProduct product,\n" +
    "    org.opencrx.kernel.uom1.jmi1.Uom priceUom,\n" +
    "    java.math.BigDecimal quantity,\n" +
    "    java.lang.Short contractPositionState,\n" +
    "    java.util.Date pricingDate,\n" +
    "    org.opencrx.kernel.product1.jmi1.AbstractPriceLevel overridePriceLevel\n" +
    ") {\n" +
    "    return org.opencrx.kernel.backend.Products.getLowestPricePriceLevel(\n" +
    "        rootPkg,\n" +
    "        pricingRule,\n" +
    "        contract,\n" +
    "        position,\n" +
    "        product,\n" +
    "        priceUom,\n" +
    "        quantity,\n" +
    "        contractPositionState,\n" +
    "        pricingDate,\n" +
    "        overridePriceLevel\n" +
    "    );\n" +
    "}//</pre>";
    
}
