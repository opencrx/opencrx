/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: ShopServiceRequestProcessor
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
package org.opencrx.application.shop1.service;

import java.util.HashMap;
import java.util.Map;

import javax.resource.cci.MappedRecord;

import org.opencrx.application.shop1.cci2.AddActivityFollowUpParams;
import org.opencrx.application.shop1.cci2.AddCustomerToCustomerContractParams;
import org.opencrx.application.shop1.cci2.AddDeliveryInformationParams;
import org.opencrx.application.shop1.cci2.AddSalesOrderPositionParams;
import org.opencrx.application.shop1.cci2.CancelInvoiceParams;
import org.opencrx.application.shop1.cci2.CancelSalesOrderParams;
import org.opencrx.application.shop1.cci2.CreateActivityParams;
import org.opencrx.application.shop1.cci2.CreateCustomerAsContactParams;
import org.opencrx.application.shop1.cci2.CreateCustomerAsLegalEntityParams;
import org.opencrx.application.shop1.cci2.CreateCustomerContractParams;
import org.opencrx.application.shop1.cci2.CreateInvoiceFromInvoiceParams;
import org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderParams;
import org.opencrx.application.shop1.cci2.CreateInvoiceParams;
import org.opencrx.application.shop1.cci2.CreateProductClassificationParams;
import org.opencrx.application.shop1.cci2.CreateProductsParams;
import org.opencrx.application.shop1.cci2.CreateSalesOrderParams;
import org.opencrx.application.shop1.cci2.GetActivitiesByQueryParams;
import org.opencrx.application.shop1.cci2.GetActivityParams;
import org.opencrx.application.shop1.cci2.GetCodeValueContainerParams;
import org.opencrx.application.shop1.cci2.GetCredentialsByEmailAddressParams;
import org.opencrx.application.shop1.cci2.GetCredentialsParams;
import org.opencrx.application.shop1.cci2.GetCustomerParams;
import org.opencrx.application.shop1.cci2.GetCustomersByQueryParams;
import org.opencrx.application.shop1.cci2.GetDocumentsParams;
import org.opencrx.application.shop1.cci2.GetInvoiceParams;
import org.opencrx.application.shop1.cci2.GetInvoicePositionsParams;
import org.opencrx.application.shop1.cci2.GetInvoicesParams;
import org.opencrx.application.shop1.cci2.GetPriceLevelParams;
import org.opencrx.application.shop1.cci2.GetProductConfigurationTypesParams;
import org.opencrx.application.shop1.cci2.GetProductPricesParams;
import org.opencrx.application.shop1.cci2.GetProductsByQueryParams;
import org.opencrx.application.shop1.cci2.GetProductsParams;
import org.opencrx.application.shop1.cci2.GetSalesOrderParams;
import org.opencrx.application.shop1.cci2.GetSalesOrderPositionsParams;
import org.opencrx.application.shop1.cci2.GetSalesOrdersParams;
import org.opencrx.application.shop1.cci2.SendEMailParams;
import org.opencrx.application.shop1.cci2.SetCredentialsParams;
import org.opencrx.application.shop1.cci2.SetCustomerContractStatusParams;
import org.opencrx.application.shop1.cci2.SetCustomerStatusParams;
import org.opencrx.application.shop1.cci2.SetInvoiceStatusParams;
import org.opencrx.application.shop1.cci2.SetProductStatusParams;
import org.opencrx.application.shop1.cci2.SetSalesOrderPositionQuantityParams;
import org.opencrx.application.shop1.cci2.SetSalesOrderStatusParams;
import org.opencrx.application.shop1.cci2.UpdateCustomerContractParams;
import org.opencrx.application.shop1.cci2.UpdateCustomerParams;
import org.opencrx.application.shop1.cci2.UpdateProductParams;
import org.w3c.spi2.Structures;

public class ShopServiceRequestProcessor {

    //-----------------------------------------------------------------------
    public void init(
    	org.opencrx.application.shop1.cci2.ShopService delegate,
        boolean mapNullFields
    ) {
        this.delegate = delegate;
        this.mapNullValues = mapNullFields;
    }

    //-----------------------------------------------------------------------
    public MappedRecord addActivityFollowUp(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.addActivityFollowUp(
                Structures.create(
                    null,
                    AddActivityFollowUpParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord addCustomerToCustomerContract(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.addCustomerToCustomerContract(
                Structures.create(
                    null,
                    AddCustomerToCustomerContractParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord addDeliveryInformation(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.addDeliveryInformation(
                Structures.create(
                    null,
                    AddDeliveryInformationParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord addSalesOrderPosition(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.addSalesOrderPosition(
                Structures.create(
                    null,
                    AddSalesOrderPositionParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord cancelInvoice(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.cancelInvoice(
                Structures.create(                
                    null,
                    CancelInvoiceParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord cancelSalesOrder(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.cancelSalesOrder(
                Structures.create(                
                    null,
                    CancelSalesOrderParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createActivity(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createActivity(
                Structures.create(                
                    null,
                    CreateActivityParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createCustomerAsContact(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createCustomerAsContact(
                Structures.create(                
                    null,
                    CreateCustomerAsContactParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createCustomerAsLegalEntity(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createCustomerAsLegalEntity(
                Structures.create(                
                    null,
                    CreateCustomerAsLegalEntityParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createCustomerContract(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createCustomerContract(
                Structures.create(
                    null,
                    CreateCustomerContractParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createInvoice(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createInvoice(
                Structures.create(
                    null,
                    CreateInvoiceParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createInvoiceFromInvoice(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createInvoiceFromInvoice(
                Structures.create(
                    null,
                    CreateInvoiceFromInvoiceParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createInvoiceFromSalesOrder(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createInvoiceFromSalesOrder(
                Structures.create(
                    null,
                    CreateInvoiceFromSalesOrderParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createProductClassification(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createProductClassification(
                Structures.create(
                    null,
                    CreateProductClassificationParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
    
    //-----------------------------------------------------------------------
    public MappedRecord createProducts(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createProducts(
                Structures.create(
                    null,
                    CreateProductsParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
            
    //-----------------------------------------------------------------------
    public MappedRecord createSalesOrder(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.createSalesOrder(
                Structures.create(
                    null,
                    CreateSalesOrderParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
        
    //-----------------------------------------------------------------------
    public MappedRecord getActivity(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getActivity(
                Structures.create(
                    null,
                    GetActivityParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
        
    //-----------------------------------------------------------------------
    public MappedRecord getActivitiesByQuery(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getActivitiesByQuery(
                Structures.create(
                    null,
                    GetActivitiesByQueryParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
        
    //-----------------------------------------------------------------------
    public MappedRecord getCodeValueContainer(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getCodeValueContainer(
                Structures.create(
                    null,
                    GetCodeValueContainerParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
        
    //-----------------------------------------------------------------------
    public MappedRecord getCredentials(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getCredentials(
                Structures.create(
                    null,
                    GetCredentialsParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                
    //-----------------------------------------------------------------------
    public MappedRecord getCredentialsByEmailAddress(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getCredentialsByEmailAddress(
                Structures.create(
                    null,
                    GetCredentialsByEmailAddressParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                
    //-----------------------------------------------------------------------
    public MappedRecord getCustomer(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getCustomer(
                Structures.create(
                    null,
                    GetCustomerParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                     
    //-----------------------------------------------------------------------
    public MappedRecord getCustomersByQuery(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getCustomersByQuery(
                Structures.create(
                    null,
                    GetCustomersByQueryParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                     
    //-----------------------------------------------------------------------
    public MappedRecord getDocuments(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getDocuments(
                Structures.create(
                    null,
                    GetDocumentsParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                     
    //-----------------------------------------------------------------------
    public MappedRecord getInvoice(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getInvoice(
                Structures.create(
                    null,
                    GetInvoiceParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
     
    //-----------------------------------------------------------------------
    public MappedRecord getInvoicePositions(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getInvoicePositions(
                Structures.create(
                    null,
                    GetInvoicePositionsParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                
    //-----------------------------------------------------------------------
    public MappedRecord getInvoices(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getInvoices(
                Structures.create(
                    null,
                    GetInvoicesParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                           
    //-----------------------------------------------------------------------
    public MappedRecord getPriceLevel(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getPriceLevel(
                Structures.create(
                    null,
                    GetPriceLevelParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                        
    //-----------------------------------------------------------------------
    public MappedRecord getProducts(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getProducts(
                Structures.create(
                    null,
                    GetProductsParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                        
    //-----------------------------------------------------------------------
    public MappedRecord getProductsByQuery(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getProductsByQuery(
                Structures.create(
                    null,
                    GetProductsByQueryParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                        
    //-----------------------------------------------------------------------
    public MappedRecord getProductConfigurationTypes(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getProductConfigurationTypes(
                Structures.create(
                    null,
                    GetProductConfigurationTypesParams.class,
                    in == null ? new HashMap<Object,Object>() : in
                )
            ),
            this.mapNullValues
        );
    }
                                        
    //-----------------------------------------------------------------------
    public MappedRecord getProductPrices(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getProductPrices(
                Structures.create(
                    null,
                    GetProductPricesParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                
    //-----------------------------------------------------------------------
    public MappedRecord getSalesOrder(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getSalesOrder(
                Structures.create(
                    null,
                    GetSalesOrderParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                    
    //-----------------------------------------------------------------------
    public MappedRecord getSalesOrders(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getSalesOrders(
                Structures.create(
                    null,
                    GetSalesOrdersParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                  
    //-----------------------------------------------------------------------
    public MappedRecord getSalesOrderPositions(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.getSalesOrderPositions(
                Structures.create(
                    null,
                    GetSalesOrderPositionsParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                    
    //-----------------------------------------------------------------------
    public MappedRecord setCredentials(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.setCredentials(
                Structures.create(
                    null,
                    SetCredentialsParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                                                
    //-----------------------------------------------------------------------
    public MappedRecord setCustomerContractStatus(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.setCustomerContractStatus(
                Structures.create(
                    null,
                    SetCustomerContractStatusParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                        
    //-----------------------------------------------------------------------
    public MappedRecord setCustomerStatus(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.setCustomerStatus(
                Structures.create(
                    null,
                    SetCustomerStatusParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                        
    //-----------------------------------------------------------------------
    public MappedRecord sendEMail(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.sendEMail(
                Structures.create(
                    null,
                    SendEMailParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                            
    //-----------------------------------------------------------------------
    public MappedRecord setInvoiceStatus(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.setInvoiceStatus(
                Structures.create(
                    null,
                    SetInvoiceStatusParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                            
    //-----------------------------------------------------------------------
    public MappedRecord setProductStatus(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.setProductStatus(
                Structures.create(
                    null,
                    SetProductStatusParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                
    //-----------------------------------------------------------------------
    public MappedRecord setSalesOrderPositionQuantity(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.setSalesOrderPositionQuantity(
                Structures.create(
                    null,
                    SetSalesOrderPositionQuantityParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                                
    //-----------------------------------------------------------------------
    public MappedRecord setSalesOrderStatus(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.setSalesOrderStatus(
                Structures.create(
                    null,
                    SetSalesOrderStatusParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                      
    //-----------------------------------------------------------------------
    public MappedRecord updateCustomer(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.updateCustomer(
                Structures.create(
                    null,
                    UpdateCustomerParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
              
    //-----------------------------------------------------------------------
    public MappedRecord updateCustomerContract(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.updateCustomerContract(
                Structures.create(
                    null,
                    UpdateCustomerContractParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                                                        
    //-----------------------------------------------------------------------
    public MappedRecord updateProduct(
        Map<String,Object> in
    ) {
        return Structures.toRecord(
            this.delegate.updateProduct(
                Structures.create(
                    null,
                    UpdateProductParams.class,
                    in
                )
            ),
            this.mapNullValues
        );
    }
                                                                                                                
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private boolean mapNullValues;
    private org.opencrx.application.shop1.cci2.ShopService delegate;
    
}

//--- End of File -----------------------------------------------------------
