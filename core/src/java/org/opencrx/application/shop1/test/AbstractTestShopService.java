/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: AbstractTestShopService
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
package org.opencrx.application.shop1.test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.opencrx.application.shop1.cci2.*;
import org.opencrx.application.shop1.datatypes.InvoiceState;
import org.opencrx.application.shop1.datatypes.LeadState;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public abstract class AbstractTestShopService {

    //-----------------------------------------------------------------------
    // ShopService
    //-----------------------------------------------------------------------
    protected abstract org.opencrx.application.shop1.cci2.AddActivityFollowUpResult addActivityFollowUp(
    	org.opencrx.application.shop1.cci2.AddActivityFollowUpParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.AddCustomerToCustomerContractResult addCustomerToCustomerContract(
    	org.opencrx.application.shop1.cci2.AddCustomerToCustomerContractParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.AddDeliveryInformationResult addDeliveryInformation(
    	org.opencrx.application.shop1.cci2.AddDeliveryInformationParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.AddSalesOrderPositionResult addSalesOrderPosition(
    	org.opencrx.application.shop1.cci2.AddSalesOrderPositionParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CancelInvoiceResult cancelInvoice(
    	org.opencrx.application.shop1.cci2.CancelInvoiceParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CancelSalesOrderResult cancelSalesOrder(
    	org.opencrx.application.shop1.cci2.CancelSalesOrderParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateActivityResult createActivity(
    	org.opencrx.application.shop1.cci2.CreateActivityParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateCustomerAsContactResult createCustomerAsContact(
    	org.opencrx.application.shop1.cci2.CreateCustomerAsContactParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateCustomerAsLegalEntityResult createCustomerAsLegalEntity(
    	org.opencrx.application.shop1.cci2.CreateCustomerAsLegalEntityParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateCustomerContractResult createCustomerContract(
    	org.opencrx.application.shop1.cci2.CreateCustomerContractParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateInvoiceResult createInvoice(
    	org.opencrx.application.shop1.cci2.CreateInvoiceParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateInvoiceFromInvoiceResult createInvoiceFromInvoice(
    	org.opencrx.application.shop1.cci2.CreateInvoiceFromInvoiceParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderResult createInvoiceFromSalesOrder(
    	org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateProductClassificationResult createProductClassification(
    	org.opencrx.application.shop1.cci2.CreateProductClassificationParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateProductsResult createProducts(
    	org.opencrx.application.shop1.cci2.CreateProductsParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.CreateSalesOrderResult createSalesOrder(
    	org.opencrx.application.shop1.cci2.CreateSalesOrderParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetActivitiesByQueryResult getActivitiesByQuery(
    	org.opencrx.application.shop1.cci2.GetActivitiesByQueryParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetActivityResult getActivity(
    	org.opencrx.application.shop1.cci2.GetActivityParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetCodeValueContainerResult getCodeValueContainer(
    	org.opencrx.application.shop1.cci2.GetCodeValueContainerParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetCredentialsResult getCredentials(
    	org.opencrx.application.shop1.cci2.GetCredentialsParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetCredentialsByEmailAddressResult getCredentialsByEmailAddress(
    	org.opencrx.application.shop1.cci2.GetCredentialsByEmailAddressParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetCustomerResult getCustomer(
    	org.opencrx.application.shop1.cci2.GetCustomerParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetCustomersByQueryResult getCustomersByQuery(
    	org.opencrx.application.shop1.cci2.GetCustomersByQueryParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetDocumentsResult getDocuments(
    	org.opencrx.application.shop1.cci2.GetDocumentsParams in
    );
    
    protected abstract org.opencrx.application.shop1.cci2.GetInvoiceResult getInvoice(
    	org.opencrx.application.shop1.cci2.GetInvoiceParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetInvoicePositionsResult getInvoicePositions(
    	org.opencrx.application.shop1.cci2.GetInvoicePositionsParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetInvoicesResult getInvoices(
    	org.opencrx.application.shop1.cci2.GetInvoicesParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetPriceLevelResult getPriceLevel(
    	org.opencrx.application.shop1.cci2.GetPriceLevelParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetProductConfigurationTypesResult getProductConfigurationTypes(
    	org.opencrx.application.shop1.cci2.GetProductConfigurationTypesParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetProductPricesResult getProductPrices(
    	org.opencrx.application.shop1.cci2.GetProductPricesParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetProductsResult getProducts(
    	org.opencrx.application.shop1.cci2.GetProductsParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetProductsByQueryResult getProductsByQuery(
    	org.opencrx.application.shop1.cci2.GetProductsByQueryParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetSalesOrderResult getSalesOrder(
    	org.opencrx.application.shop1.cci2.GetSalesOrderParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetSalesOrdersResult getSalesOrders(
    	org.opencrx.application.shop1.cci2.GetSalesOrdersParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.GetSalesOrderPositionsResult getSalesOrderPositions(
    	org.opencrx.application.shop1.cci2.GetSalesOrderPositionsParams in
    );
    
    protected abstract org.opencrx.application.shop1.cci2.SendEMailResult sendEMail(
    	org.opencrx.application.shop1.cci2.SendEMailParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.SetCredentialsResult setCredentials(
    	org.opencrx.application.shop1.cci2.SetCredentialsParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.SetCustomerStatusResult setCustomerStatus(
    	org.opencrx.application.shop1.cci2.SetCustomerStatusParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.SetCustomerContractStatusResult setCustomerContractStatus(
    	org.opencrx.application.shop1.cci2.SetCustomerContractStatusParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.SetInvoiceStatusResult setInvoiceStatus(
    	org.opencrx.application.shop1.cci2.SetInvoiceStatusParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.SetProductStatusResult setProductStatus(
    	org.opencrx.application.shop1.cci2.SetProductStatusParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.SetSalesOrderPositionQuantityResult setSalesOrderPositionQuantity(
    	org.opencrx.application.shop1.cci2.SetSalesOrderPositionQuantityParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.SetSalesOrderStatusResult setSalesOrderStatus(
    	org.opencrx.application.shop1.cci2.SetSalesOrderStatusParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.UpdateCustomerResult updateCustomer(
    	org.opencrx.application.shop1.cci2.UpdateCustomerParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.UpdateCustomerContractResult updateCustomerContract(
    	org.opencrx.application.shop1.cci2.UpdateCustomerContractParams in
    );

    protected abstract org.opencrx.application.shop1.cci2.UpdateProductResult updateProduct(
    	org.opencrx.application.shop1.cci2.UpdateProductParams in
    );
    
    //-----------------------------------------------------------------------
    // 
    // TestShopService
    //-----------------------------------------------------------------------
    
    //-----------------------------------------------------------------------
    protected static ReturnStatusT newOperationStatus(
        int exceptionCode,
        String[] parameters
    ) {
        return Datatypes.create(
            ReturnStatusT.class,
            new Structures.Member<ReturnStatusT.Member>(
                ReturnStatusT.Member.returnCode,
                exceptionCode
            ),
            new Structures.Member<ReturnStatusT.Member>(
                ReturnStatusT.Member.returnParams,
                parameters
            )               
        );
    }
        
    //-----------------------------------------------------------------------
    protected void logResult(
       String operationName,
       ReturnStatusT returnStatusT
    ) {
        System.out.println(new Date() + "   " + operationName + " " + returnStatusT.getReturnCode() + " " + returnStatusT.getReturnParams());
    }

    //-----------------------------------------------------------------------
    protected ProductT newProduct(
        int id,
        String productNumber
    ) {
        String productName = "Clip-" + id;
        System.out.println(new Date() + "   productNumber " + productNumber);
        boolean isBundle = Integer.valueOf(productNumber) % 10 == 0;
        return Datatypes.create(
            ProductT.class,
            Datatypes.member(
                ProductT.Member.productNumber,
                productNumber
            ),
            Datatypes.member(
                ProductT.Member.productName,
                productName
            ),
            Datatypes.member(
                ProductT.Member.description,
                new ProductDescriptionT[]{
                    Datatypes.create(
                        ProductDescriptionT.class,
                        Datatypes.member(
                            ProductDescriptionT.Member.language,
                            138 // German
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.description,
                            "description description description description description description description"
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.detailedDescription,
                            "detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed"
                        )
                    ),
                    Datatypes.create(
                        ProductDescriptionT.class,
                        Datatypes.member(
                            ProductDescriptionT.Member.language,
                            110 // English
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.description,
                            "description description description description description description description"
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.detailedDescription,
                            "detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed"
                        )
                    ),
                    Datatypes.create(
                        ProductDescriptionT.class,
                        Datatypes.member(
                            ProductDescriptionT.Member.language,
                            126 // French
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.description,
                            "description description description description description description description"
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.detailedDescription,
                            "detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed detailed"
                        )
                    )
                }
            ),
            Datatypes.member(
                ProductT.Member.classificationId,
                new String[]{"country=DE", "project=MyShop", "service=Universal", "category=123_action", "partner=Universal", "genre=Action", "genre=Thriller"}
            ),
            Datatypes.member(
                ProductT.Member.priceUom,
                new String[]{"DVD", "Download-to-own", "24h Rental", "FreeArchiv"}
            ),                        
            Datatypes.member(
                ProductT.Member.configurationType,
                "MMClip"
            ),                      
            Datatypes.member(
                ProductT.Member.configuration,
                new ProductConfigurationT[]{
                    Datatypes.create(
                        ProductConfigurationT.class,
                        Datatypes.member(
                            ProductConfigurationT.Member.propertySetName,
                            "DVD"
                        ),
                        Datatypes.member(
                            ProductConfigurationT.Member.property,
                            new StringPropertyT[]{
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "audioType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "AUDIOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"EN Dolby 5.1", "EN DolbyDigital", "DE Dolby 5.1", "DE DolbyDigital"}
                                    )                                
                                ),
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "videoType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "VIDEOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"2MBit;HD"}
                                    )
                                )
                            }
                        )                                    
                    ),                    
                    Datatypes.create(
                        ProductConfigurationT.class,
                        Datatypes.member(
                            ProductConfigurationT.Member.propertySetName,
                            "Download-to-own"
                        ),
                        Datatypes.member(
                            ProductConfigurationT.Member.property,
                            new StringPropertyT[]{
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "audioType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "AUDIOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"EN Dolby 5.1", "EN DolbyDigital", "DE Dolby 5.1", "DE DolbyDigital"}
                                    )                                
                                ),
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "videoType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "VIDEOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"2MBit HD"}
                                    )
                                )
                            }
                        )                                    
                    ),                                    
                    Datatypes.create(
                        ProductConfigurationT.class,
                        Datatypes.member(
                            ProductConfigurationT.Member.propertySetName,
                            "24h Rental"
                        ),
                        Datatypes.member(
                            ProductConfigurationT.Member.property,
                            new StringPropertyT[]{
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "audioType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "AUDIOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"EN Dolby 5.1", "EN DolbyDigital", "DE Dolby 5.1", "DE DolbyDigital"}
                                    )                                
                                ),
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "videoType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "VIDEOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"2MBit HD"}
                                    )
                                )
                            }
                        )                                                                        
                    ),                                    
                    Datatypes.create(
                        ProductConfigurationT.class,
                        Datatypes.member(
                            ProductConfigurationT.Member.propertySetName,
                            "FreeArchiv"
                        ),
                        Datatypes.member(
                            ProductConfigurationT.Member.property,
                            new StringPropertyT[]{
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "audioType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "AUDIOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"EN Dolby 5.1", "EN DolbyDigital", "DE Dolby 5.1", "DE DolbyDigital"}
                                    )                                
                                ),
                                Datatypes.create(
                                    StringPropertyT.class,
                                    Datatypes.member(
                                        StringPropertyT.Member.name,
                                        "videoType"
                                    ),
                                    Datatypes.member(
                                        StringPropertyT.Member.domain,
                                        "VIDEOTYPE"
                                    ),                                                
                                    Datatypes.member(
                                        StringPropertyT.Member.stringValue,
                                        new String[]{"2MBit HD"}
                                    )
                                )
                            }
                        )                                                                        
                    )                                    
                }
            ),
            Datatypes.member(
                ProductT.Member.pictureContent,
                new byte[]{
                    (byte)0x47,(byte)0x49,(byte)0x46,(byte)0x38,(byte)0x39,(byte)0x61,(byte)0x0E,(byte)0x00,(byte)0x0E,(byte)0x00,(byte)0xF7,(byte)0x00,(byte)0x00,(byte)0xFF,(byte)0xFF,(byte)0xFF,
                    (byte)0xFF,(byte)0xFF,(byte)0xCC,(byte)0xFF,(byte)0xFF,(byte)0x99,(byte)0xFF,(byte)0xFF,(byte)0x66,(byte)0xFF,(byte)0xFF,(byte)0x33,(byte)0xFF,(byte)0xFF,(byte)0x00,(byte)0xFF,
                    (byte)0xCC,(byte)0xFF,(byte)0xFF,(byte)0xCC,(byte)0xCC,(byte)0xFF,(byte)0xCC,(byte)0x99,(byte)0xFF,(byte)0xCC,(byte)0x66,(byte)0xFF,(byte)0xCC,(byte)0x33,(byte)0xFF,(byte)0xCC,
                    (byte)0x00,(byte)0xFF,(byte)0x99,(byte)0xFF,(byte)0xFF,(byte)0x99,(byte)0xCC,(byte)0xFF,(byte)0x99,(byte)0x99,(byte)0xFF,(byte)0x99,(byte)0x66,(byte)0xFF,(byte)0x99,(byte)0x33,
                    (byte)0xFF,(byte)0x99,(byte)0x00,(byte)0xFF,(byte)0x66,(byte)0xFF,(byte)0xFF,(byte)0x66,(byte)0xCC,(byte)0xFF,(byte)0x66,(byte)0x99,(byte)0xFF,(byte)0x66,(byte)0x66,(byte)0xFF,
                    (byte)0x66,(byte)0x33,(byte)0xFF,(byte)0x66,(byte)0x00,(byte)0xFF,(byte)0x33,(byte)0xFF,(byte)0xFF,(byte)0x33,(byte)0xCC,(byte)0xFF,(byte)0x33,(byte)0x99,(byte)0xFF,(byte)0x33,
                    (byte)0x66,(byte)0xFF,(byte)0x33,(byte)0x33,(byte)0xFF,(byte)0x33,(byte)0x00,(byte)0xFF,(byte)0x00,(byte)0xFF,(byte)0xFF,(byte)0x00,(byte)0xCC,(byte)0xFF,(byte)0x00,(byte)0x99,
                    (byte)0xFF,(byte)0x00,(byte)0x66,(byte)0xFF,(byte)0x00,(byte)0x33,(byte)0xFF,(byte)0x00,(byte)0x00,(byte)0xCC,(byte)0xFF,(byte)0xFF,(byte)0xCC,(byte)0xFF,(byte)0xCC,(byte)0xCC,
                    (byte)0xFF,(byte)0x99,(byte)0xCC,(byte)0xFF,(byte)0x66,(byte)0xCC,(byte)0xFF,(byte)0x33,(byte)0xCC,(byte)0xFF,(byte)0x00,(byte)0xCC,(byte)0xCC,(byte)0xFF,(byte)0xCC,(byte)0xCC,
                    (byte)0xCC,(byte)0xCC,(byte)0xCC,(byte)0x99,(byte)0xCC,(byte)0xCC,(byte)0x66,(byte)0xCC,(byte)0xCC,(byte)0x33,(byte)0xCC,(byte)0xCC,(byte)0x00,(byte)0xCC,(byte)0x99,(byte)0xFF,
                    (byte)0xCC,(byte)0x99,(byte)0xCC,(byte)0xCC,(byte)0x99,(byte)0x99,(byte)0xCC,(byte)0x99,(byte)0x66,(byte)0xCC,(byte)0x99,(byte)0x33,(byte)0xCC,(byte)0x99,(byte)0x00,(byte)0xCC,
                    (byte)0x66,(byte)0xFF,(byte)0xCC,(byte)0x66,(byte)0xCC,(byte)0xCC,(byte)0x66,(byte)0x99,(byte)0xCC,(byte)0x66,(byte)0x66,(byte)0xCC,(byte)0x66,(byte)0x33,(byte)0xCC,(byte)0x66,
                    (byte)0x00,(byte)0xCC,(byte)0x33,(byte)0xFF,(byte)0xCC,(byte)0x33,(byte)0xCC,(byte)0xCC,(byte)0x33,(byte)0x99,(byte)0xCC,(byte)0x33,(byte)0x66,(byte)0xCC,(byte)0x33,(byte)0x33,
                    (byte)0xCC,(byte)0x33,(byte)0x00,(byte)0xCC,(byte)0x00,(byte)0xFF,(byte)0xCC,(byte)0x00,(byte)0xCC,(byte)0xCC,(byte)0x00,(byte)0x99,(byte)0xCC,(byte)0x00,(byte)0x66,(byte)0xCC,
                    (byte)0x00,(byte)0x33,(byte)0xCC,(byte)0x00,(byte)0x00,(byte)0x99,(byte)0xFF,(byte)0xFF,(byte)0x99,(byte)0xFF,(byte)0xCC,(byte)0x99,(byte)0xFF,(byte)0x99,(byte)0x99,(byte)0xFF,
                    (byte)0x66,(byte)0x99,(byte)0xFF,(byte)0x33,(byte)0x99,(byte)0xFF,(byte)0x00,(byte)0x99,(byte)0xCC,(byte)0xFF,(byte)0x99,(byte)0xCC,(byte)0xCC,(byte)0x99,(byte)0xCC,(byte)0x99,
                    (byte)0x99,(byte)0xCC,(byte)0x66,(byte)0x99,(byte)0xCC,(byte)0x33,(byte)0x99,(byte)0xCC,(byte)0x00,(byte)0x99,(byte)0x99,(byte)0xFF,(byte)0x99,(byte)0x99,(byte)0xCC,(byte)0x99,
                    (byte)0x99,(byte)0x99,(byte)0x99,(byte)0x99,(byte)0x66,(byte)0x99,(byte)0x99,(byte)0x33,(byte)0x99,(byte)0x99,(byte)0x00,(byte)0x99,(byte)0x66,(byte)0xFF,(byte)0x99,(byte)0x66,
                    (byte)0xCC,(byte)0x99,(byte)0x66,(byte)0x99,(byte)0x99,(byte)0x66,(byte)0x66,(byte)0x99,(byte)0x66,(byte)0x33,(byte)0x99,(byte)0x66,(byte)0x00,(byte)0x99,(byte)0x33,(byte)0xFF,
                    (byte)0x99,(byte)0x33,(byte)0xCC,(byte)0x99,(byte)0x33,(byte)0x99,(byte)0x99,(byte)0x33,(byte)0x66,(byte)0x99,(byte)0x33,(byte)0x33,(byte)0x99,(byte)0x33,(byte)0x00,(byte)0x99,
                    (byte)0x00,(byte)0xFF,(byte)0x99,(byte)0x00,(byte)0xCC,(byte)0x99,(byte)0x00,(byte)0x99,(byte)0x99,(byte)0x00,(byte)0x66,(byte)0x99,(byte)0x00,(byte)0x33,(byte)0x99,(byte)0x00,
                    (byte)0x00,(byte)0x66,(byte)0xFF,(byte)0xFF,(byte)0x66,(byte)0xFF,(byte)0xCC,(byte)0x66,(byte)0xFF,(byte)0x99,(byte)0x66,(byte)0xFF,(byte)0x66,(byte)0x66,(byte)0xFF,(byte)0x33,
                    (byte)0x66,(byte)0xFF,(byte)0x00,(byte)0x66,(byte)0xCC,(byte)0xFF,(byte)0x66,(byte)0xCC,(byte)0xCC,(byte)0x66,(byte)0xCC,(byte)0x99,(byte)0x66,(byte)0xCC,(byte)0x66,(byte)0x66,
                    (byte)0xCC,(byte)0x33,(byte)0x66,(byte)0xCC,(byte)0x00,(byte)0x66,(byte)0x99,(byte)0xFF,(byte)0x66,(byte)0x99,(byte)0xCC,(byte)0x66,(byte)0x99,(byte)0x99,(byte)0x66,(byte)0x99,
                    (byte)0x66,(byte)0x66,(byte)0x99,(byte)0x33,(byte)0x66,(byte)0x99,(byte)0x00,(byte)0x66,(byte)0x66,(byte)0xFF,(byte)0x66,(byte)0x66,(byte)0xCC,(byte)0x66,(byte)0x66,(byte)0x99,
                    (byte)0x66,(byte)0x66,(byte)0x66,(byte)0x66,(byte)0x66,(byte)0x33,(byte)0x66,(byte)0x66,(byte)0x00,(byte)0x66,(byte)0x33,(byte)0xFF,(byte)0x66,(byte)0x33,(byte)0xCC,(byte)0x66,
                    (byte)0x33,(byte)0x99,(byte)0x66,(byte)0x33,(byte)0x66,(byte)0x66,(byte)0x33,(byte)0x33,(byte)0x66,(byte)0x33,(byte)0x00,(byte)0x66,(byte)0x00,(byte)0xFF,(byte)0x66,(byte)0x00,
                    (byte)0xCC,(byte)0x66,(byte)0x00,(byte)0x99,(byte)0x66,(byte)0x00,(byte)0x66,(byte)0x66,(byte)0x00,(byte)0x33,(byte)0x66,(byte)0x00,(byte)0x00,(byte)0x33,(byte)0xFF,(byte)0xFF,
                    (byte)0x33,(byte)0xFF,(byte)0xCC,(byte)0x33,(byte)0xFF,(byte)0x99,(byte)0x33,(byte)0xFF,(byte)0x66,(byte)0x33,(byte)0xFF,(byte)0x33,(byte)0x33,(byte)0xFF,(byte)0x00,(byte)0x33,
                    (byte)0xCC,(byte)0xFF,(byte)0x33,(byte)0xCC,(byte)0xCC,(byte)0x33,(byte)0xCC,(byte)0x99,(byte)0x33,(byte)0xCC,(byte)0x66,(byte)0x33,(byte)0xCC,(byte)0x33,(byte)0x33,(byte)0xCC,
                    (byte)0x00,(byte)0x33,(byte)0x99,(byte)0xFF,(byte)0x33,(byte)0x99,(byte)0xCC,(byte)0x33,(byte)0x99,(byte)0x99,(byte)0x33,(byte)0x99,(byte)0x66,(byte)0x33,(byte)0x99,(byte)0x33,
                    (byte)0x33,(byte)0x99,(byte)0x00,(byte)0x33,(byte)0x66,(byte)0xFF,(byte)0x33,(byte)0x66,(byte)0xCC,(byte)0x33,(byte)0x66,(byte)0x99,(byte)0x33,(byte)0x66,(byte)0x66,(byte)0x33,
                    (byte)0x66,(byte)0x33,(byte)0x33,(byte)0x66,(byte)0x00,(byte)0x33,(byte)0x33,(byte)0xFF,(byte)0x33,(byte)0x33,(byte)0xCC,(byte)0x33,(byte)0x33,(byte)0x99,(byte)0x33,(byte)0x33,
                    (byte)0x66,(byte)0x33,(byte)0x33,(byte)0x33,(byte)0x33,(byte)0x33,(byte)0x00,(byte)0x33,(byte)0x00,(byte)0xFF,(byte)0x33,(byte)0x00,(byte)0xCC,(byte)0x33,(byte)0x00,(byte)0x99,
                    (byte)0x33,(byte)0x00,(byte)0x66,(byte)0x33,(byte)0x00,(byte)0x33,(byte)0x33,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0xFF,(byte)0xFF,(byte)0x00,(byte)0xFF,(byte)0xCC,(byte)0xFF,
                    (byte)0xFF,(byte)0xFF,(byte)0x00,(byte)0xFF,(byte)0x66,(byte)0x00,(byte)0xFF,(byte)0x33,(byte)0x00,(byte)0xFF,(byte)0x00,(byte)0x00,(byte)0xCC,(byte)0xFF,(byte)0x00,(byte)0xCC,
                    (byte)0xCC,(byte)0x00,(byte)0xCC,(byte)0x99,(byte)0x00,(byte)0xCC,(byte)0x66,(byte)0x00,(byte)0xCC,(byte)0x33,(byte)0x00,(byte)0xCC,(byte)0x00,(byte)0x00,(byte)0x99,(byte)0xFF,
                    (byte)0x00,(byte)0x99,(byte)0xCC,(byte)0x00,(byte)0x99,(byte)0x99,(byte)0x00,(byte)0x99,(byte)0x66,(byte)0x00,(byte)0x99,(byte)0x33,(byte)0x00,(byte)0x99,(byte)0x00,(byte)0x00,
                    (byte)0x66,(byte)0xFF,(byte)0x00,(byte)0x66,(byte)0xCC,(byte)0x00,(byte)0x66,(byte)0x99,(byte)0x00,(byte)0x66,(byte)0x66,(byte)0x00,(byte)0x66,(byte)0x33,(byte)0x00,(byte)0x66,
                    (byte)0x00,(byte)0x00,(byte)0x33,(byte)0xFF,(byte)0x00,(byte)0x33,(byte)0xCC,(byte)0x00,(byte)0x33,(byte)0x99,(byte)0x00,(byte)0x33,(byte)0x66,(byte)0x00,(byte)0x33,(byte)0x33,
                    (byte)0x00,(byte)0x33,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0xFF,(byte)0x00,(byte)0x00,(byte)0xCC,(byte)0x00,(byte)0x00,(byte)0x99,(byte)0x00,(byte)0x00,(byte)0x66,(byte)0x00,
                    (byte)0x00,(byte)0x33,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0xFF,(byte)0xFF,(byte)0xFF,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,
                    (byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,
                    (byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,
                    (byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,
                    (byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,
                    (byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,
                    (byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,
                    (byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x21,(byte)0xF9,(byte)0x04,
                    (byte)0x01,(byte)0x00,(byte)0x00,(byte)0xB6,(byte)0x00,(byte)0x2C,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x00,(byte)0x0E,(byte)0x00,(byte)0x0E,(byte)0x00,(byte)0x40,(byte)0x08,
                    (byte)0x54,(byte)0x00,(byte)0x6D,(byte)0x09,(byte)0x1C,(byte)0x78,(byte)0xED,(byte)0x9A,(byte)0xC0,(byte)0x35,(byte)0x06,(byte)0x05,(byte)0x5E,(byte)0x03,(byte)0xC0,(byte)0xB0,
                    (byte)0x60,(byte)0x41,(byte)0x86,(byte)0x00,(byte)0x12,(byte)0xDA,(byte)0xBA,(byte)0x56,(byte)0x60,(byte)0x21,(byte)0x44,(byte)0x8A,(byte)0x12,(byte)0x2D,(byte)0x46,(byte)0x2C,
                    (byte)0x50,(byte)0xF1,(byte)0xE2,(byte)0x40,(byte)0x85,(byte)0x15,(byte)0x31,(byte)0x22,(byte)0x04,(byte)0xA9,(byte)0x11,(byte)0x62,(byte)0xC3,(byte)0x8A,(byte)0x13,(byte)0x2F,
                    (byte)0x72,(byte)0xD4,(byte)0x28,(byte)0x91,(byte)0xE0,(byte)0x4A,(byte)0x5B,(byte)0x23,(byte)0x3F,(byte)0x62,(byte)0x3C,(byte)0x59,(byte)0xD0,(byte)0x65,(byte)0x49,(byte)0x9A,
                    (byte)0x0A,(byte)0x4D,(byte)0xEA,(byte)0x8C,(byte)0x98,(byte)0xF3,(byte)0x64,(byte)0xC7,(byte)0x86,(byte)0x3D,(byte)0x37,(byte)0xFE,(byte)0xE4,(byte)0x39,(byte)0xD1,(byte)0xA1,
                    (byte)0x51,(byte)0xA3,(byte)0xB6,(byte)0x02,(byte)0x02,(byte)0x00,(byte)0x3B
                }                        
            ),
            Datatypes.member(
                ProductT.Member.pictureMimeType,
                "image/gif"                        
            ),
            Datatypes.member(
                ProductT.Member.pictureTitle,
                "Clip-" + id                        
            ),
            Datatypes.member(
                ProductT.Member.productPhase,
                new ProductPhaseT[]{
                    Datatypes.create(
                        ProductPhaseT.class, 
                        Datatypes.member(
                            ProductPhaseT.Member.name, 
                            "Online"
                        ),
                        Datatypes.member(
                            ProductPhaseT.Member.productPhaseKey, 
                            "Online"
                        ),
                        Datatypes.member(
                            ProductPhaseT.Member.validFrom, 
                            new Date()
                        ),
                        Datatypes.member(
                            ProductPhaseT.Member.validTo, 
                            new Date(System.currentTimeMillis() + 12345678900L)
                        )
                    )
                }
            ),
            Datatypes.member(
                ProductT.Member.isBundle,
                isBundle
            ),
            Datatypes.member(
                ProductT.Member.bundleData,
                Datatypes.create(
                    ProductBundleDataT.class, 
                    Datatypes.member(
                        ProductBundleDataT.Member.classificationIdFilter, 
                        new String[]{"category=123_action"}
                    )
                )
            )
        );
    }

    //-----------------------------------------------------------------------
    /**
     * Returned products are used to create sales orders. The default
     * implementation looks up products with product number 1..100 and
     * returns at most three products.
     */
    protected List<ProductT> getProductsForSalesOrder(
    ) {
    	List<ProductT> productsT = new ArrayList<ProductT>();
        for(int i = 0; i < 100; i++) {
            int id = new Double(Math.random() * 100.0).intValue();
            String productNumber = Integer.toString(id);
            GetProductsParams getProductsParams = Datatypes.create(
                GetProductsParams.class,
                Datatypes.member(
                    GetProductsParams.Member.productNumber,
                    new String[]{productNumber}
                ),
                Datatypes.member(
                    GetProductsParams.Member.returnPictureContent,
                    Boolean.FALSE
                )
            );
            GetProductsResult getProductsResult = null;
            getProductsResult = this.getProducts(
                getProductsParams
            );
            this.logResult("getProducts", getProductsResult.getStatus());            
            if(getProductsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                productsT.addAll(getProductsResult.getProduct());
                if(productsT.size() > 3) break;
            }
        }
        return productsT;
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT testCreate100Products(
    ) {
        System.out.println(new Date() + "   ---------- testCreate100Products");
        // Get existing products - required to create product relationships
        GetProductsByQueryParams getProductsByQueryParams = Datatypes.create(
            GetProductsByQueryParams.class,
            Datatypes.member(
                GetProductsByQueryParams.Member.classificationId,
                Arrays.asList(new String[]{"country=DE", "project=MyShop"})
            )
        );
        GetProductsByQueryResult getProductsByQueryResult = null;
        getProductsByQueryResult = this.getProductsByQuery(
            getProductsByQueryParams
        );
        this.logResult("getProductsByQuery", getProductsByQueryResult.getStatus());
        // Find starting product number
        int id = -1;
        for(int i = 0; i < 5000; i+=100) {
            // Get product
            GetProductsParams getProductsParams = Datatypes.create(
                GetProductsParams.class,
                Datatypes.member(
                    GetProductsParams.Member.productNumber,
                    Arrays.asList(new String[]{"" + i})
                ),
                Datatypes.member(
                    GetProductsParams.Member.returnPictureContent,
                    Boolean.FALSE
                )                
            );
            GetProductsResult getProductsResult = null;
            getProductsResult = this.getProducts(
                getProductsParams
            );
            this.logResult("getProducts", getProductsResult.getStatus());
            if(getProductsResult.getStatus().getReturnCode() == BasicException.Code.NOT_FOUND) {
                id = i;
                break;
            }            
        }
        if(id < 0) {
            return newOperationStatus(
                BasicException.Code.NONE,
                new String[]{"No products created in range [0,5000]"}
            );
        }
        CreateProductsResult createProductsResult = null;
        for(int i = 0; i < 10; i++) {
            ProductT[] products = new ProductT[10];
            for(int j = 0; j < 10; j++) {
                int idx = 10*i + j; 
                String productNumber = Integer.toString(id + idx);            
                products[j] = this.newProduct(
                    id + idx, 
                    productNumber 
                );                
            }
            CreateProductsParams createProductsParams = Datatypes.create(
                CreateProductsParams.class,
                Datatypes.member(
                    CreateProductsParams.Member.product,
                    products
                )
            );
            createProductsResult = this.createProducts(
                createProductsParams
            );
            this.logResult("createProducts", createProductsResult.getStatus());
        }
        return createProductsResult.getStatus();
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT testDocuments(
    ) {
        System.out.println(new Date() + "   ---------- testDocuments");
        GetDocumentsParams getDocumentsParams = Datatypes.create(
            GetDocumentsParams.class,
            Datatypes.member(
                GetDocumentsParams.Member.folderName,
                "Report Templates"
            )
        );
        GetDocumentsResult getDocumentsResult = this.getDocuments(
            getDocumentsParams
        );
        this.logResult("getDocuments", getDocumentsResult.getStatus());
        return getDocumentsResult.getStatus();
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT testProducts(
    ) {
        System.out.println(new Date() + "   ---------- testProducts");        
        // Create classifications. Don't care in case of duplicate error
        String[] classificationIds = new String[]{
            "country=DE",
            "country=CH",
            "country=FR",
            "country=IT",
            "project=MyShop", 
            "service=Standard", 
            "service=Premium",
            "category=123_action", 
            "partner=MyPartner", 
            "genre=Action", 
            "genre=Thriller"
        };
        for(String classificationId: classificationIds) {
            CreateProductClassificationParams createProductClassificationParams = Datatypes.create(
                CreateProductClassificationParams.class,
                Datatypes.member(
                    CreateProductClassificationParams.Member.classification,
                    new ProductClassificationT[]{
                        Datatypes.create(
                            ProductClassificationT.class,
                            Datatypes.member(
                                ProductClassificationT.Member.classificationId,
                                classificationId
                            ),
                            Datatypes.member(
                                ProductClassificationT.Member.description,
                                classificationId
                            )
                         )
                    }
                )
            );
            CreateProductClassificationResult createdProductClassificationResult = null;
            createdProductClassificationResult = this.createProductClassification(
                createProductClassificationParams
            );
            this.logResult("createProductClassification", createdProductClassificationResult.getStatus());
        }
        // Get configuration types
        GetProductConfigurationTypesParams getProductConfigurationTypesParams = Datatypes.create(
            GetProductConfigurationTypesParams.class
        );
        GetProductConfigurationTypesResult getProductConfigurationTypesResult = null;
        getProductConfigurationTypesResult = this.getProductConfigurationTypes(
            getProductConfigurationTypesParams
        );
        this.logResult("getProductConfigurationTypes", getProductConfigurationTypesResult.getStatus());
        // Get existing products - required to create product relationships
        GetProductsByQueryParams getProductsByQueryParams = Datatypes.create(
            GetProductsByQueryParams.class,
            Datatypes.member(
                GetProductsByQueryParams.Member.classificationId,
                Arrays.asList(new String[]{"country=DE", "project=MyShop"})
            )
        );
        GetProductsByQueryResult getProductsByQueryResult = null;
        getProductsByQueryResult = this.getProductsByQuery(
            getProductsByQueryParams
        );
        this.logResult("getProductsByQuery", getProductsByQueryResult.getStatus());
        // Get / Create / Update product
        int id = new Double(Math.random() * 100.0).intValue();
        String productNumber = Integer.toString(id);
        System.out.println(new Date() + "   productNumber " + productNumber);
        ProductT productT = this.newProduct(
            id, 
            productNumber 
        );
        // Get product
        GetProductsParams getProductsParams = Datatypes.create(
            GetProductsParams.class,
            Datatypes.member(
                GetProductsParams.Member.productNumber,
                Arrays.asList(new String[]{productNumber})
            ),
            Datatypes.member(
                GetProductsParams.Member.returnPictureContent,
                Boolean.FALSE                
            )
        );
        GetProductsResult getProductsResult = null;
        getProductsResult = this.getProducts(
            getProductsParams
        );
        this.logResult("getProducts", getProductsResult.getStatus());
        // Create product if not found
        if(getProductsResult.getStatus().getReturnCode() == BasicException.Code.NOT_FOUND) {
            CreateProductsParams createProductsParams = Datatypes.create(
                CreateProductsParams.class,
                Datatypes.member(
                    CreateProductsParams.Member.product,
                    new ProductT[]{productT}
                )
            );
            CreateProductsResult createProductsResult = null;
            createProductsResult = this.createProducts(
                createProductsParams
            );
            this.logResult("createProducts", createProductsResult.getStatus());
        }
        // Update product
        UpdateProductParams updateProductParams = Datatypes.create(
            UpdateProductParams.class,
            Datatypes.member(
                UpdateProductParams.Member.product,
                productT
            ),
            Datatypes.member(
                UpdateProductParams.Member.updateMainData,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateProductParams.Member.updateClassification,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateProductParams.Member.updateUom,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateProductParams.Member.updateConfiguration,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateProductParams.Member.updateProductPhase,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateProductParams.Member.updatePicture,
                Boolean.TRUE
            )            
        );         
        UpdateProductResult updateProductResult = null;
        updateProductResult = this.updateProduct(
            updateProductParams
        );
        this.logResult("updateProduct", updateProductResult.getStatus());
        // Get products by query
        getProductsByQueryParams = Datatypes.create(
            GetProductsByQueryParams.class,
            Datatypes.member(
                GetProductsByQueryParams.Member.classificationId,
                Arrays.asList(new String[]{"country=DE", "project=MyShop"})
            )
        );
        getProductsByQueryResult = null;
        getProductsByQueryResult = this.getProductsByQuery(
            getProductsByQueryParams
        );
        this.logResult("getProductsByQuery", getProductsByQueryResult.getStatus());
        if(getProductsByQueryResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
            List<String> productNumbers = new ArrayList<String>();
            for(String pn: getProductsByQueryResult.getProductNumber()) {
                productNumbers.add(pn);
                if(productNumbers.size() > 0) break;
            }
            // Get product prices for at most 10 products
            List<Date> pricingDates = Arrays.asList(
                new Date(System.currentTimeMillis() - 3600000L),
                new Date(System.currentTimeMillis()),
                new Date(System.currentTimeMillis() + 3600000L)
            );
            GetProductPricesParams getProductPricesParams = Datatypes.create(
                GetProductPricesParams.class,
                Datatypes.member(
                    GetProductPricesParams.Member.productNumber,
                    productNumbers
                ),
                Datatypes.member(
                    GetProductPricesParams.Member.pricingDate,
                    pricingDates
                ),
                Datatypes.member(
                    GetProductPricesParams.Member.quantity,
                    "1.0"
                ),
                Datatypes.member(
                    GetProductPricesParams.Member.priceCurrency,
                    978
                ),
                Datatypes.member(
                    GetProductPricesParams.Member.salesTaxType,
                    "Sales Tax 19%"
                )                                
            );
            GetProductPricesResult getProductPricesResult = null;
            getProductPricesResult = this.getProductPrices(
                getProductPricesParams
            );
            this.logResult("getProductPrices", getProductPricesResult.getStatus());            
            if(getProductPricesResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                for(ProductPriceListT priceListT: getProductPricesResult.getProductPriceList()) {
                    for(ProductPriceT priceT: priceListT.getProductPrice()) {
                        GetPriceLevelParams getPriceLevelParams = Datatypes.create(
                            GetPriceLevelParams.class,
                            Datatypes.member(
                                GetPriceLevelParams.Member.priceLevel,
                                priceT.getPriceLevel()
                            )
                        );
                        GetPriceLevelResult getPriceLevelResult = null;
                        getPriceLevelResult = this.getPriceLevel(
                            getPriceLevelParams
                        );
                        this.logResult("getPriceLevel", getPriceLevelResult.getStatus());            
                    }
                }
            }
        }
        // Set status
        int productStatus = new Double((Math.random() * 5.0)).intValue();
        SetProductStatusParams setProductStatusParams = Datatypes.create(
            SetProductStatusParams.class,
            Datatypes.member(
                SetProductStatusParams.Member.productNumber,
                productNumber
            ),
            Datatypes.member(
                SetProductStatusParams.Member.productStatus,
                Datatypes.create(
                    ProductStatusT.class,
                    Datatypes.member(
                        ProductStatusT.Member.status,
                        productStatus
                    ),
                    Datatypes.member(
                        ProductStatusT.Member.description,
                        "Set status to " + productStatus
                    )
                )
            )
        );
        SetProductStatusResult setProductStatusResult = null;
        setProductStatusResult = this.setProductStatus(
            setProductStatusParams
        );
        this.logResult("setProductStatus", setProductStatusResult.getStatus());
        return setProductStatusResult.getStatus();
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT testSalesOrders(
    ) {
        System.out.println(new Date() + "   ---------- testSalesOrders");        
        // Get an existing customer
        String customerNumber = null;
        for(int i = 0; i < 100; i++) {
            int id = new Double(Math.random() * 100.0).intValue();
            String userName = Integer.toString(id);
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            this.logResult("getCredentials", getCredentialsResult.getStatus());            
            if(getCredentialsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                customerNumber = getCredentialsResult.getCustomerNumber();
                break;
            }
        }
        if(customerNumber == null) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{});
        }
        GetCustomerParams getCustomerParams = Datatypes.create(
            GetCustomerParams.class,
            Datatypes.member(
                GetCustomerParams.Member.customerNumber,
                customerNumber
            )
        );
        GetCustomerResult getCustomerResult = null;
        getCustomerResult = this.getCustomer(
            getCustomerParams
        );
        this.logResult("getCustomer", getCustomerResult.getStatus());
        if(getCustomerResult.getStatus().getReturnCode() != BasicException.Code.NONE) {
            return getCustomerResult.getStatus(); 
        }        
        CustomerT customerT = getCustomerResult.getCustomer();
        // Get at most existing products
        List<ProductT> productsT = this.getProductsForSalesOrder();
        // Prepare sales order positions
        List<ContractPositionT> contractPositionsT = new ArrayList<ContractPositionT>();
        for(ProductT productT: productsT) {
            ContractPositionT contractPositionT = Datatypes.create(
                ContractPositionT.class,
                Datatypes.member(
                    ContractPositionT.Member.productNumber,
                    productT.getProductNumber()
                ),
                Datatypes.member(
                    ContractPositionT.Member.quantity,
                    "1.0"
                ),
                Datatypes.member(
                    ContractPositionT.Member.pricingDate,
                    new Date()
                ),
                Datatypes.member(
                    ContractPositionT.Member.priceUom,
                    productT.getPriceUom().get(new Double(Math.random() * productT.getPriceUom().size()).intValue())
                )
            );
            contractPositionsT.add(contractPositionT);
        }
        // Prepare sales order
        SalesOrderT salesOrderT = Datatypes.create(
            SalesOrderT.class,
            Datatypes.member(
                SalesOrderT.Member.contract,
                Datatypes.create(
                    ContractT.class,
                    Datatypes.member(
                        ContractT.Member.customerNumber,
                        customerT.getCustomerNumber()
                    ),
                    Datatypes.member(
                        ContractT.Member.activeOn,
                        new Date()
                    ),
                    Datatypes.member(
                        ContractT.Member.position,
                        contractPositionsT
                    ),             
                    Datatypes.member(
                        ContractT.Member.postalAddressDelivery,
                        customerT.getContact().getPostalAddressHome()
                    ),                    
                    Datatypes.member(
                        ContractT.Member.postalAddressInvoice,
                        customerT.getContact().getPostalAddressHome()
                    )                    
                )
            )
        );
        // Create sales order
        CreateSalesOrderParams createSalesOrderParams = Datatypes.create(
            CreateSalesOrderParams.class,
            Datatypes.member(
                CreateSalesOrderParams.Member.salesOrder,
                salesOrderT
            )
        );            
        CreateSalesOrderResult createSalesOrderResult = null;
        createSalesOrderResult = this.createSalesOrder(
            createSalesOrderParams
        );
        this.logResult("createSalesOrder", createSalesOrderResult.getStatus());
        if(createSalesOrderResult.getStatus().getReturnCode() != BasicException.Code.NONE) {
            return createSalesOrderResult.getStatus(); 
        }
        // Get sales order
        GetSalesOrderParams getSalesOrderParams = Datatypes.create(
            GetSalesOrderParams.class,
            Datatypes.member(
                GetSalesOrderParams.Member.salesOrderNumber,
                createSalesOrderResult.getSalesOrder().getContract().getContractNumber()
            )
        );
        GetSalesOrderResult getSalesOrderResult = null;
        getSalesOrderResult = this.getSalesOrder(
            getSalesOrderParams
        );
        this.logResult("getSalesOrder", getSalesOrderResult.getStatus());        
        // Get sales orders
        GetSalesOrdersParams getSalesOrdersParams = Datatypes.create(
            GetSalesOrdersParams.class,
            Datatypes.member(
                GetSalesOrdersParams.Member.customerNumber,
                customerNumber
            )
        );
        GetSalesOrdersResult getSalesOrdersResult = null;
        getSalesOrdersResult = this.getSalesOrders(
            getSalesOrdersParams
        );
        this.logResult("getSalesOrders", getSalesOrdersResult.getStatus());                
        // Set sales order position quantity and add delivery information
        for(SalesOrderT contractT: getSalesOrdersResult.getSalesOrder()) {
            for(ContractPositionT positionT: contractT.getContract().getPosition()) {
                SetSalesOrderPositionQuantityParams setSalesOrderPositionQuantityParams = Datatypes.create(
                    SetSalesOrderPositionQuantityParams.class,
                    Datatypes.member(
                        SetSalesOrderPositionQuantityParams.Member.contractNumber,
                        positionT.getContractNumber()
                    ),
                    Datatypes.member(
                        SetSalesOrderPositionQuantityParams.Member.positionNumber,
                        positionT.getPositionNumber()
                    ),
                    Datatypes.member(
                        SetSalesOrderPositionQuantityParams.Member.quantity,
                        Integer.toString(new BigDecimal(Math.random() * 5.0 + 1.0).intValue())
                    )
                );
                SetSalesOrderPositionQuantityResult setSalesOrderPositionQuantityResult = null;
                setSalesOrderPositionQuantityResult = this.setSalesOrderPositionQuantity(
                    setSalesOrderPositionQuantityParams
                );
                this.logResult("setSalesOrderPositionQuantity", setSalesOrderPositionQuantityResult.getStatus());  
                // Add delivery information
                AddDeliveryInformationParams addDeliveryInformationParams = Datatypes.create(
                    AddDeliveryInformationParams.class,
                    Datatypes.member(
                    	AddDeliveryInformationParams.Member.contractNumber,
                        positionT.getContractNumber()
                    ),
                    Datatypes.member(
                    	AddDeliveryInformationParams.Member.positionNumber,
                    	positionT.getPositionNumber()
                    ),
                    Datatypes.member(
                    	AddDeliveryInformationParams.Member.deliveryInformation,
                    	Datatypes.create(
                            DeliveryInformationT.class,
                            Datatypes.member(
                            	DeliveryInformationT.Member.deliveryStatus,
                                0
                            ),
                            Datatypes.member(
                            	DeliveryInformationT.Member.deliveryStatusDescription,
                                "Delivered at " + new Date()
                            ),
                            Datatypes.member(
                            	DeliveryInformationT.Member.actualDeliveryOn,
                                new Date()
                            ),
                            Datatypes.member(
                            	DeliveryInformationT.Member.quantityShipped,
                                "1.0"
                            ),
                            Datatypes.member(
                            	DeliveryInformationT.Member.productAssembledAt,
                                new Date()
                            )
                        )
                    )                    
                );
                AddDeliveryInformationResult addDeliveryInformationResult = null;
                addDeliveryInformationResult = this.addDeliveryInformation(
                	addDeliveryInformationParams
                );
                this.logResult("addDeliveryInformation", addDeliveryInformationResult.getStatus());  
            }
            // Add sales order position
            productsT = this.getProductsForSalesOrder();
            if(!productsT.isEmpty()) {
                ProductT productT = productsT.get(0);
                AddSalesOrderPositionParams addSalesOrderPositionParams = Datatypes.create(
                    AddSalesOrderPositionParams.class,
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.salesOrderNumber,
                        contractT.getContract().getContractNumber()
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.quantity,
                        "1.0"
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.productNumber,
                        productT.getProductNumber()
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.pricingDate,
                        new Date()
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.priceUom,
                        productT.getPriceUom().get(new Double(Math.random() * productT.getPriceUom().size()).intValue())
                    )                    
                );
                AddSalesOrderPositionResult addSalesOrderPositionResult = null;
                addSalesOrderPositionResult = this.addSalesOrderPosition(
                    addSalesOrderPositionParams
                );
                this.logResult("addSalesOrderPosition", addSalesOrderPositionResult.getStatus());     
            }
        }
        // Set sales order status
        int salesOrderStatus = new Double((Math.random() * 5.0)).intValue();
        SetSalesOrderStatusParams setSalesOrderStatusParams = Datatypes.create(
            SetSalesOrderStatusParams.class,
            Datatypes.member(
                SetSalesOrderStatusParams.Member.salesOrderNumber,
                getSalesOrderResult.getSalesOrder().getContract().getContractNumber()
            ),
            Datatypes.member(
                SetSalesOrderStatusParams.Member.salesOrderStatus,
                Datatypes.create(
                    ContractStatusT.class,
                    Datatypes.member(
                        ContractStatusT.Member.status,
                        salesOrderStatus
                    ),
                    Datatypes.member(
                         ContractStatusT.Member.description,
                        "Set status to " + salesOrderStatus
                    ),
                    Datatypes.member(
                        ContractStatusT.Member.tag,
                        new StringPropertyT[]{
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "tag1"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 1.1"}
                                ),                                
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 1.2"}
                                )                                
                            ),
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "tag2"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 2.1"}
                                ),                                
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 2.2"}
                                )                                
                            )
                        }
                    )
                )
            )
        );
        SetSalesOrderStatusResult setSalesOrderStatusResult = null;
        setSalesOrderStatusResult = this.setSalesOrderStatus(
            setSalesOrderStatusParams
        );
        this.logResult("setSalesOrderStatus", setSalesOrderStatusResult.getStatus());
        return setSalesOrderStatusResult.getStatus();        
    }

    //-----------------------------------------------------------------------
    public ReturnStatusT testActivities(
    ) {
        System.out.println(new Date() + "   ---------- testActivities");        
        // Get an existing customer
        String customerNumber = null;
        int id = 0;
        for(int i = 0; i < 100; i++) {
            id = new Double(Math.random() * 100.0).intValue();
            String userName = Integer.toString(id);
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            this.logResult("getCredentials", getCredentialsResult.getStatus());            
            if(getCredentialsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                customerNumber = getCredentialsResult.getCustomerNumber();
                break;
            }
        }
        if(customerNumber == null) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{});
        }
        // Create 2 activities 
        for(int i = 0; i < 2; i++) {
            CreateActivityParams createActivityParams = Datatypes.create(
                CreateActivityParams.class,
                Datatypes.member(
                	CreateActivityParams.Member.name,
                    "Test for " + customerNumber + " # " + i + " @ " + System.currentTimeMillis()
                ),
                Datatypes.member(
                	CreateActivityParams.Member.description,
                    "Description for " + customerNumber + " # " + i + " @ " + System.currentTimeMillis()
                ),                
                Datatypes.member(
                	CreateActivityParams.Member.detailedDescription,
                    "Description for " + customerNumber + " # " + i + " @ " + System.currentTimeMillis()
                ),                
                Datatypes.member(
                	CreateActivityParams.Member.reportingCustomerNumber,
                    customerNumber
                ),                
                Datatypes.member(
                	CreateActivityParams.Member.scheduledStart,
                    new Date()
                ),                
                Datatypes.member(
                	CreateActivityParams.Member.scheduledEnd,
                    new Date(System.currentTimeMillis() + 10000L)
                ),                
                Datatypes.member(
                	CreateActivityParams.Member.dueBy,
                    new Date(System.currentTimeMillis() + 10000L)
                ),                                
                Datatypes.member(
                	CreateActivityParams.Member.activityCreatorName,
                    "TestShop - Incidents"
                ),                                
                Datatypes.member(
                	CreateActivityParams.Member.priority,
                    0
                )                                             
            );        	
            CreateActivityResult createActivityResult = null;
            createActivityResult = this.createActivity(
            	createActivityParams
            );
            this.logResult("createActivity", createActivityResult.getStatus());     
            if(createActivityResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
            	String activityNumber = createActivityResult.getActivity().getActivityNumber();
            	List<String> transitionNames = Arrays.asList("Assign", "Add Note", "Complete");
            	for(String transitionName: transitionNames) {
	                AddActivityFollowUpParams addActivityFollowUpParams = Datatypes.create(
	                	AddActivityFollowUpParams.class,
	                    Datatypes.member(
	                    	AddActivityFollowUpParams.Member.activityNumber,
	                        activityNumber
	                    ),
	                    Datatypes.member(
	                    	AddActivityFollowUpParams.Member.name,
	                        "Description for " + customerNumber + " # " + i + " @ " + System.currentTimeMillis()
	                    ),
	                    Datatypes.member(
	                    	AddActivityFollowUpParams.Member.description,
	                        "Description for " + customerNumber + " # " + i + " @ " + System.currentTimeMillis()
	                    ),
	                    Datatypes.member(
	                    	AddActivityFollowUpParams.Member.transitionName,
	                        transitionName
	                    ),
	                    Datatypes.member(
	                    	AddActivityFollowUpParams.Member.reportingCustomerNumber,
	                        customerNumber
	                    ),                    
	                    Datatypes.member(
	                    	AddActivityFollowUpParams.Member.category,
	                        Arrays.asList("cat1", "cat2")
	                    )                    
	                );
	                AddActivityFollowUpResult addActivityFollowUpResult = null;
	                addActivityFollowUpResult = this.addActivityFollowUp(
	                	addActivityFollowUpParams
	                );
	                this.logResult("addActivityFollowUp", addActivityFollowUpResult.getStatus());
            	}
            }            
        }
        // Test sendEMail
        SendEMailParams sendEMailParams = Datatypes.create(
        	SendEMailParams.class,
            Datatypes.member(
            	SendEMailParams.Member.onBehalfOfCustomerNumber,
                customerNumber
            ),                
            Datatypes.member(
            	SendEMailParams.Member.subject,
                "Test for " + customerNumber + " @ " + System.currentTimeMillis()
            ),
            Datatypes.member(
            	SendEMailParams.Member.body,
                "Description for " + customerNumber + " @ " + System.currentTimeMillis()
            ),                
            Datatypes.member(
            	SendEMailParams.Member.sender,
                id + "@gmx.de"
            ),                
            Datatypes.member(
            	SendEMailParams.Member.recipientTo,
                new String[]{id + "@gmx.de"}
            ),                
            Datatypes.member(
            	SendEMailParams.Member.recipientCc,
                new String[]{id + "@gmx.de"}
            ),                
            Datatypes.member(
            	SendEMailParams.Member.recipientBcc,
                new String[]{id + "@gmx.de"}
            ),                                
            Datatypes.member(
            	SendEMailParams.Member.emailCreatorName,
                "TestShop - E-Mails"
            )                                
        );        	
        SendEMailResult sendEMailResult = null;
        sendEMailResult = this.sendEMail(
        	sendEMailParams
        );
        this.logResult("sendEMail", sendEMailResult.getStatus());             
        // Get customer's activities
        GetActivitiesByQueryParams getActivitiesByQueryParams = Datatypes.create(
        	GetActivitiesByQueryParams.class,
            Datatypes.member(
            	GetActivitiesByQueryParams.Member.customerNumber,
                customerNumber
            )
        );        	
        GetActivitiesByQueryResult getActivitiesByQueryResult = null;
        getActivitiesByQueryResult = this.getActivitiesByQuery(
        	getActivitiesByQueryParams
        );
        this.logResult("getActivitiesByQueryResult", getActivitiesByQueryResult.getStatus());
        
        return getActivitiesByQueryResult.getStatus();        
    }

    //-----------------------------------------------------------------------
    public ReturnStatusT testCustomers(
    ) {        
        System.out.println(new Date() + "   ---------- testCustomers");        
        int id = new Double(Math.random() * 100.0).intValue();
        String firstName = "First-" + id;
        String lastName = "Last-" + id;
        String middleName = "Middle-" + id;
        String userName = Integer.toString(id);
        String nickName = "Nick-" + id;
        String emailAddressHome = id + "@gmx.de";
        String emailAddressBusiness = id + "@mycompany.net";
        // Create customer
        CreateCustomerAsContactParams createCustomerParams = Datatypes.create(
            CreateCustomerAsContactParams.class,
            Datatypes.member(
            	CreateCustomerAsContactParams.Member.firstName,
                firstName
            ),
            Datatypes.member(
            	CreateCustomerAsContactParams.Member.lastName,
                lastName
            ),
            Datatypes.member(
            	CreateCustomerAsContactParams.Member.middleName,
                middleName
            ),
            Datatypes.member(
            	CreateCustomerAsContactParams.Member.userName,
                userName
            ),
            Datatypes.member(
            	CreateCustomerAsContactParams.Member.emailAddressHome,
                emailAddressHome
            ),
            Datatypes.member(
            	CreateCustomerAsContactParams.Member.emailAddressBusiness,
                emailAddressBusiness
            )
        );
        CreateCustomerAsContactResult createCustomerAsContactResult = null;
        createCustomerAsContactResult = this.createCustomerAsContact(
            createCustomerParams
        );
        this.logResult("createCustomerAsContact", createCustomerAsContactResult.getStatus());
        String customerNumber = null;
        if(createCustomerAsContactResult.getStatus().getReturnCode() == BasicException.Code.DUPLICATE) {
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            customerNumber = getCredentialsResult.getCustomerNumber();
        }
        else {
            customerNumber = createCustomerAsContactResult.getCustomer().getCustomerNumber();            
        }
        // Get customer
        GetCustomerParams getCustomerParams = Datatypes.create(
            GetCustomerParams.class,
            Datatypes.member(
                GetCustomerParams.Member.customerNumber,
                customerNumber
            )            
        );
        GetCustomerResult getCustomerResult = null;
        getCustomerResult = this.getCustomer(
            getCustomerParams
        );
        this.logResult("getCustomer", getCustomerResult.getStatus());
        // Get customers by query
        GetCustomersByQueryParams getCustomersByQueryParams = Datatypes.create(
            GetCustomersByQueryParams.class,
            Datatypes.member(
                GetCustomersByQueryParams.Member.emailAddress,
                emailAddressHome
            )            
        );
        GetCustomersByQueryResult getCustomersByQueryResult = null;
        getCustomersByQueryResult = this.getCustomersByQuery(
            getCustomersByQueryParams
        );
        this.logResult("getCustomersByQuery", getCustomersByQueryResult.getStatus());
        // Create customer contract
        CreateCustomerContractParams createCustomerContractParams = Datatypes.create(
            CreateCustomerContractParams.class,
            Datatypes.member(
                CreateCustomerContractParams.Member.customerContract,
                Datatypes.create(
                    CustomerContractT.class,
                    Datatypes.member(
                    	CustomerContractT.Member.customerNumber,
                        customerNumber
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.acceptedLegal,
                        Boolean.TRUE
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.acceptedMarketing,
                        Boolean.TRUE
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.acceptedPrivateDataForwarding,
                        Boolean.TRUE
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.referrer,
                        "Referrer"
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.contactSource,
                        33
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.salesTaxType,
                        "Sales Tax 0%"
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.contractCurrency,
                        978
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.noBilling,
                        Boolean.FALSE
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.billingPartner,
                        "Billing Partner"
                    ),
                    Datatypes.member(
                        CustomerContractT.Member.billingPartnerRegistrationId,
                        Arrays.asList(new String[]{"RegId1", "RegId2"})
                    )
                )
            )
        );
        CreateCustomerContractResult createCustomerContractResult = null;
        createCustomerContractResult = this.createCustomerContract(
            createCustomerContractParams
        );
        this.logResult("createCustomerContract", createCustomerContractResult.getStatus());
        // Update customer
        UpdateCustomerParams updateCustomerParams = Datatypes.create(
            UpdateCustomerParams.class,
            Datatypes.member(
                UpdateCustomerParams.Member.updateMainData,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateCustomerParams.Member.updateAddressData,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateCustomerParams.Member.updateHobbyAndInterestData,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateCustomerParams.Member.updateGenericData,
                Boolean.TRUE
            ),            
            Datatypes.member(
                UpdateCustomerParams.Member.updateBookmarks,
                Boolean.TRUE
            ),            
            Datatypes.member(
                UpdateCustomerParams.Member.customer,
                Datatypes.create(
                    CustomerT.class,
                    Datatypes.member(
                        CustomerT.Member.customerNumber,
                        customerNumber
                    ),
                    Datatypes.member(
                        CustomerT.Member.externalId,
                        new String[]{"ID" + id}
                    ),
                    Datatypes.member(
                        CustomerT.Member.accountCategory,
                        new Integer[]{1}
                    ),
                    Datatypes.member(
                        CustomerT.Member.accountRating,
                        1
                    ),      
                    Datatypes.member(
                        CustomerT.Member.genericData,
                        new StringPropertyT[]{
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "genericField1"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"genericValue0", "genericValue1"}
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.domain,
                                    "GenericDomain0"
                                )
                            )
                        }
                    ),
                    Datatypes.member(
                    	CustomerT.Member.contact,
                    	Datatypes.create(
                    		ContactT.class,
		                    Datatypes.member(
		                    	ContactT.Member.organization,
		                        "CRM Corp."
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.salutationCode,
		                        1
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.salutation,
		                        "Dr."
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.title,
		                        5
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.firstName,
		                        firstName
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.lastName,
		                        lastName
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.middleName,
		                        middleName
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.nickName,
		                        nickName
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.birthDate,
		                        new Date()
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.placeOfBirth,
		                        "Gotham"
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.birthDateIsValidated,
		                        1
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.doNotPostalMail,
		                        Boolean.TRUE
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.doNotEmail,
		                        Boolean.TRUE
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.doNotPhone,
		                        Boolean.TRUE
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.doNotFax,
		                        Boolean.TRUE
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.nativeLanguage,
		                        138 // German
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.preferredSpokenLanguage,
		                        138 // German
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.preferredWrittenLanguage,
		                        138 // German
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.gender,
		                        1 // Male
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.jobRole,
		                        13
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.jobTitle,
		                        "Job Title"
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.citizenship,
		                        new Integer[]{276} // Germany
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.education,
		                        1
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.annualIncomeAmount,
		                        5
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.annualIncomeCurrency,
		                        978
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.monthlyIncomeAmount,
		                        5
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.monthlyIncomeCurrency,
		                        978
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.numberOfChildren,
		                        2
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.childrenNames,
		                        new String[]{"Lanzelot", "Larisa"}
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.familyStatus,
		                        1
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.preferredContactMethod,
		                        3 // E-Mail
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.religion,
		                        new Integer[]{1000, 2000}
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.postalAddressHome,
		                        Datatypes.create(
		                            PostalAddressT.class,
		                            Datatypes.member(
		                                PostalAddressT.Member.postalAddressLine0,
		                                "Herr"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalAddressLine1,
		                                firstName + " " + lastName
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalStreet0,
		                                "Privat-Strasse " + id
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalStreetNumber,
		                                Integer.toString(id)
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCity,
		                                "Gotham"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCode,
		                                "12345"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCountry,
		                                276 // Germany
		                            )
		                        )
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.faxNumberHome,
		                        Datatypes.create(
		                            PhoneNumberT.class,
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumber,
		                                "+41(22)99999-" + id
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumberVerified,
		                                Boolean.TRUE
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerName,
		                                37
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerVerified,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.phoneNumberHome,
		                        Datatypes.create(
		                            PhoneNumberT.class,
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumber,
		                                "+41(22)88888-" + id
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumberVerified,
		                                Boolean.TRUE
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerName,
		                                37
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerVerified,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.webAddressHome,
		                        "http://www.myhome-" + id + ".net"
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.postalAddressBusiness,
		                        Datatypes.create(
		                            PostalAddressT.class,
		                            Datatypes.member(
		                                PostalAddressT.Member.postalAddressLine0,
		                                "MyCompany " + id + " Corp"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalAddressLine1,
		                                firstName + " " + lastName
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalStreet0,
		                                "Business Street " + id
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalStreetNumber,
		                                Integer.toString(id)
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCity,
		                                "Gotham"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCode,
		                                "12345"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCountry,
		                                276 // Germany
		                            )
		                        )
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.emailAddressHome,
		                        Datatypes.create(
		                            EmailAddressT.class,
		                            Datatypes.member(
		                                EmailAddressT.Member.emailAddress,
		                                emailAddressHome
		                            ),
		                            Datatypes.member(
		                                EmailAddressT.Member.emailValid,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.emailAddressBusiness,
		                        Datatypes.create(
		                            EmailAddressT.class,
		                            Datatypes.member(
		                                EmailAddressT.Member.emailAddress,
		                                emailAddressBusiness
		                            ),
		                            Datatypes.member(
		                                EmailAddressT.Member.emailValid,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.phoneNumberBusiness,
		                        Datatypes.create(
		                            PhoneNumberT.class,
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumber,
		                                "+41(22)77777-" + id
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumberVerified,
		                                Boolean.TRUE
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerName,
		                                37
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerVerified,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.faxNumberBusiness,
		                        Datatypes.create(
		                            PhoneNumberT.class,
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumber,
		                                "+41(22)66666-" + id
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumberVerified,
		                                Boolean.TRUE
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerName,
		                                37
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerVerified,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.webAddressHome,
		                        "http://www.myhome-" + id + ".de"
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.webAddressBusiness,
		                        "http://www.mycompany-" + id + ".de"
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.blogAddress,
		                        "http://www.google.com/blog/" + id
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.phoneNumberMobile,
		                        Datatypes.create(
		                            PhoneNumberT.class,
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumber,
		                                "+41(78)55555-" + id
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumberVerified,
		                                Boolean.TRUE
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerName,
		                                37
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerVerified,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.messengerAddress,
		                        new MessengerAddressT[]{
		                            Datatypes.create(
		                                MessengerAddressT.class,
		                                Datatypes.member(
		                                    MessengerAddressT.Member.messengerId,
		                                    "msgid-" + id
		                                ),
		                                Datatypes.member(
		                                    MessengerAddressT.Member.providerName,
		                                    37
		                                ),
		                                Datatypes.member(
		                                    MessengerAddressT.Member.providerVerified,
		                                    Boolean.TRUE
		                                )
		                            )
		                        }
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.communityStatus,
		                        1
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.commerceStatus,
		                        1
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.personsInHousehold,
		                        3
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.professionalSkills,
		                        3
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.internetUsage,
		                        3
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.internetProvider,
		                        77
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.pcUsage,
		                        new Integer[]{3, 7}
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.portalRating,
		                        3
		                    ),                    
		                    Datatypes.member(
		                    	ContactT.Member.hobbyAndInterest,
		                        Datatypes.create(
		                            CustomerHobbyAndInterestT.class,
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.sports,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.travel,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.finance,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.computerInternet,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.telecommunication,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.entertainment,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.music,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.lifestyle,
		                                new Integer[]{1, 2, 3}
		                            ),
		                            Datatypes.member(
		                                CustomerHobbyAndInterestT.Member.other,
		                                new Integer[]{1, 2, 3}
		                            )
		                        )
		                    ),
		                    Datatypes.member(
		                    	ContactT.Member.bookmarks,
		                        new StringPropertyT[]{
		                            Datatypes.create(
		                                StringPropertyT.class,
		                                Datatypes.member(
		                                    StringPropertyT.Member.name,
		                                    "MyFavorites"
		                                ),
		                                Datatypes.member(
		                                    StringPropertyT.Member.stringValue,
		                                    Arrays.asList(new String[]{"DVD-1", "DVD-2", "DVD-3"})
		                                )
		                            )
		                        }
		                    )       
		                )
		            )
                )
            )
        );
        UpdateCustomerResult updateCustomerResult = null;
        updateCustomerResult = this.updateCustomer(
            updateCustomerParams
        );
        this.logResult("updateCustomer", updateCustomerResult.getStatus());
        // Update customer contract
        CustomerContractT customerContract = Datatypes.create(
            CustomerContractT.class,
            Datatypes.member(
                CustomerContractT.Member.contractNumber,
                customerNumber
            ),
            Datatypes.member(
                CustomerContractT.Member.acceptedLegal,
                Boolean.TRUE
            ),
            Datatypes.member(
                CustomerContractT.Member.acceptedMarketing,
                Boolean.TRUE
            ),
            Datatypes.member(
                CustomerContractT.Member.acceptedPrivateDataForwarding,
                Boolean.TRUE
            ),
            Datatypes.member(
                CustomerContractT.Member.referrer,
                "Referrer"
            ),
            Datatypes.member(
                CustomerContractT.Member.contactSource,
                id
            ),
            Datatypes.member(
                CustomerContractT.Member.contractCurrency,
                978
            ),                
            Datatypes.member(
                CustomerContractT.Member.salesTaxType,
                "Sales Tax 19%"
            ),                
            Datatypes.member(
                CustomerContractT.Member.noBilling,
                Boolean.FALSE
            ),
            Datatypes.member(
                CustomerContractT.Member.billingPartner,
                "Billing Partner"
            ),
            Datatypes.member(
                CustomerContractT.Member.billingPartnerRegistrationId,
                Arrays.asList(new String[]{"RegId1", "RegId2"})
            )
        );        
        // Update customer contract
        UpdateCustomerContractParams updateCustomerContractParams = Datatypes.create(
            UpdateCustomerContractParams.class,
            Datatypes.member(
                UpdateCustomerContractParams.Member.customerContract,
                customerContract
            )                    
        );
        UpdateCustomerContractResult updateCustomerContractResult = null;
        updateCustomerContractResult = this.updateCustomerContract(
            updateCustomerContractParams
        );
        this.logResult("updateCustomerContract", updateCustomerContractResult.getStatus());
        // Set customer contract status
        SetCustomerContractStatusParams setCustomerContractStatusParams = Datatypes.create(
            SetCustomerContractStatusParams.class,
            Datatypes.member(
                SetCustomerContractStatusParams.Member.customerContractNumber,
                customerNumber
            ),
            Datatypes.member(
            	SetCustomerContractStatusParams.Member.contractStatus,
                Datatypes.create(
                    ContractStatusT.class,
                    Datatypes.member(
                        ContractStatusT.Member.status,
                        LeadState.OPEN_NEW.getValue()
                    ),
                    Datatypes.member(
                         ContractStatusT.Member.description,
                        "Set status to " + LeadState.OPEN_NEW
                    ),
                    Datatypes.member(
                        ContractStatusT.Member.tag,
                        new StringPropertyT[]{
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "tag1"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 1.1"}
                                ),                                
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 1.2"}
                                )                                
                            ),
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "tag2"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 2.1"}
                                ),                                
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 2.2"}
                                )                                
                            )
                        }
                    )
                )
            )
        );
        SetCustomerContractStatusResult setCustomerContractStatusResult = null;
        setCustomerContractStatusResult = this.setCustomerContractStatus(
            setCustomerContractStatusParams
        );
        this.logResult("setCustomerContractStatus", setCustomerContractStatusResult.getStatus());
        // Get credentials
        GetCredentialsParams getCredentialsParams = Datatypes.create(
            GetCredentialsParams.class,
            Datatypes.member(
                GetCredentialsParams.Member.userName,
                userName
            )
        );
        GetCredentialsResult getCredentialsResult = null;
        getCredentialsResult = this.getCredentials(
            getCredentialsParams
        );
        this.logResult("getCredentials", getCredentialsResult.getStatus());
        // Set credentials
        SetCredentialsParams setCredentialsParams = Datatypes.create(
            SetCredentialsParams.class,
            Datatypes.member(
                SetCredentialsParams.Member.customerNumber,
                customerNumber
            ),
            Datatypes.member(
                SetCredentialsParams.Member.credentials,
                Datatypes.create(
                    CredentialsT.class,
                    Datatypes.member(
                        CredentialsT.Member.userName,
                        userName
                    ),
                    Datatypes.member(
                        CredentialsT.Member.passwordMd5,
                        "passwordMd5-" + id
                    ),
                    Datatypes.member(
                        CredentialsT.Member.resetPasswordChallenge,
                        "resetPasswordChallenge-" + id
                    ),
                    Datatypes.member(
                        CredentialsT.Member.resetPasswordResponse,
                        "resetPasswordResponse-" + id
                    )
                )
            )
        );
        SetCredentialsResult setCredentialsResult = null;
        setCredentialsResult = this.setCredentials(
            setCredentialsParams
        );
        this.logResult("setCredentials", setCredentialsResult.getStatus());
        // Get credentials by Email address
        GetCredentialsByEmailAddressParams getCredentialsByEmailAddressParams = Datatypes.create(
            GetCredentialsByEmailAddressParams.class,
            Datatypes.member(
                GetCredentialsByEmailAddressParams.Member.emailAddress,
                emailAddressBusiness
            )
        );
        GetCredentialsByEmailAddressResult getCredentialsByEmailAddressResult = null;
        getCredentialsByEmailAddressResult = this.getCredentialsByEmailAddress(
        	getCredentialsByEmailAddressParams
        );
        this.logResult("getCredentialsByEmailAddress", getCredentialsByEmailAddressResult.getStatus());        
        // Set status
        int customerStatus = new Double((Math.random() * 5.0)).intValue();
        SetCustomerStatusParams setCustomerStatusParams = Datatypes.create(
            SetCustomerStatusParams.class,
            Datatypes.member(
                SetCustomerStatusParams.Member.customerNumber,
                customerNumber
            ),
            Datatypes.member(
                SetCustomerStatusParams.Member.customerStatus,
                Datatypes.create(
                    CustomerStatusT.class,
                    Datatypes.member(
                        CustomerStatusT.Member.status,
                        customerStatus
                    ),
                    Datatypes.member(
                        CustomerStatusT.Member.description,
                        "Set status to " + customerStatus
                    )
                )
            )
        );
        SetCustomerStatusResult setCustomerStatusResult = null;
        setCustomerStatusResult = this.setCustomerStatus(
            setCustomerStatusParams
        );
        this.logResult("setCustomerStatus", setCustomerStatusResult.getStatus());
        return setCredentialsResult.getStatus();
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT testLegalEntities(
    ) {
        System.out.println(new Date() + "   ---------- testLegalEntities");    
        int id = new Double(Math.random() * 100.0).intValue();
        // Find legal entity
        String legalName = "Company-" + Integer.toString(id);
        GetCustomersByQueryParams getCustomersByQueryParams = Datatypes.create(
        	GetCustomersByQueryParams.class,
            Datatypes.member(
            	GetCustomersByQueryParams.Member.legalName,
                legalName
            )
        );
        GetCustomersByQueryResult getCustomersByQueryResult = null;
        getCustomersByQueryResult = this.getCustomersByQuery(
        	getCustomersByQueryParams
        );
        // Create legal entity if none is found
        String customerNumber = null;
        if(getCustomersByQueryResult.getCustomerNumber().isEmpty()) {
        	// Get primary contact        	
            String userName = Integer.toString(id);
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            this.logResult("getCredentials", getCredentialsResult.getStatus());            
            if(getCredentialsResult.getStatus().getReturnCode() == BasicException.Code.NOT_FOUND) {
            	return getCredentialsResult.getStatus();
            }
            String primaryContactNumber = getCredentialsResult.getCustomerNumber();
            // Create customer as legal entity
            CreateCustomerAsLegalEntityParams createCustomerParams = Datatypes.create(
                CreateCustomerAsLegalEntityParams.class,
                Datatypes.member(
                	CreateCustomerAsLegalEntityParams.Member.legalName,
                    legalName
                )
            );
            CreateCustomerAsLegalEntityResult createCustomerAsLegalEntityResult = null;
            createCustomerAsLegalEntityResult = this.createCustomerAsLegalEntity(
                createCustomerParams
            );
            this.logResult("createCustomerAsLegalEntity", createCustomerAsLegalEntityResult.getStatus());
            if(createCustomerAsLegalEntityResult.getCustomer() != null) {
	            customerNumber = createCustomerAsLegalEntityResult.getCustomer().getCustomerNumber();
	            // Update customer
	            UpdateCustomerParams updateCustomerParams =  Datatypes.create(
	                UpdateCustomerParams.class,
	                Datatypes.member(
	                	UpdateCustomerParams.Member.updateMainData,
	                    Boolean.TRUE
	                ),
	                Datatypes.member(
	                	UpdateCustomerParams.Member.customer,
	                	Datatypes.create(
	                		CustomerT.class,
	                        Datatypes.member(
	                        	CustomerT.Member.customerNumber,
	                            customerNumber
	                        ),
	                        Datatypes.member(
	                        	CustomerT.Member.legalEntity,
	                        	Datatypes.create(
	                                LegalEntityT.class,
	                                Datatypes.member(
	                                	LegalEntityT.Member.legalName,
	                                    legalName
	                                ),   
	                                Datatypes.member(
	                                	LegalEntityT.Member.primaryContactNumber,
	                                    primaryContactNumber
	                                )   
	                            )
	                        )                     
	                    )
	                )                
	            );
	            UpdateCustomerResult updateCustomerResult = this.updateCustomer(
	            	updateCustomerParams
	            );
	            this.logResult("updateCustomer", updateCustomerResult.getStatus());
            }
        }
        else {
        	customerNumber = getCustomersByQueryResult.getCustomerNumber().iterator().next();
        }        
        if(customerNumber == null) {
        	return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{"Customer"});
        }
        // Get customer
        GetCustomerParams getCustomerParams = Datatypes.create(
            GetCustomerParams.class,
            Datatypes.member(
                GetCustomerParams.Member.customerNumber,
                customerNumber
            )            
        );
        GetCustomerResult getCustomerResult = null;
        getCustomerResult = this.getCustomer(
            getCustomerParams
        );
        this.logResult("getCustomer", getCustomerResult.getStatus());
        // Update customer
        UpdateCustomerParams updateCustomerParams = Datatypes.create(
            UpdateCustomerParams.class,
            Datatypes.member(
                UpdateCustomerParams.Member.updateMainData,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateCustomerParams.Member.updateAddressData,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateCustomerParams.Member.updateHobbyAndInterestData,
                Boolean.TRUE
            ),
            Datatypes.member(
                UpdateCustomerParams.Member.updateGenericData,
                Boolean.TRUE
            ),            
            Datatypes.member(
                UpdateCustomerParams.Member.updateBookmarks,
                Boolean.TRUE
            ),            
            Datatypes.member(
                UpdateCustomerParams.Member.customer,
                Datatypes.create(
                    CustomerT.class,
                    Datatypes.member(
                        CustomerT.Member.customerNumber,
                        customerNumber
                    ),
                    Datatypes.member(
                        CustomerT.Member.externalId,
                        new String[]{"ID" + id}
                    ),
                    Datatypes.member(
                        CustomerT.Member.accountCategory,
                        new Integer[]{1}
                    ),
                    Datatypes.member(
                        CustomerT.Member.accountRating,
                        1
                    ),      
                    Datatypes.member(
                        CustomerT.Member.genericData,
                        new StringPropertyT[]{
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "genericField1"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"genericValue0", "genericValue1"}
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.domain,
                                    "GenericDomain0"
                                )
                            )
                        }
                    ),
                    Datatypes.member(
                    	CustomerT.Member.legalEntity,
                    	Datatypes.create(
                    		LegalEntityT.class,
		                    Datatypes.member(
		                    	LegalEntityT.Member.legalName,
		                        legalName
		                    ),
		                    Datatypes.member(
		                    	LegalEntityT.Member.postalAddressBusiness,
		                        Datatypes.create(
		                            PostalAddressT.class,
		                            Datatypes.member(
		                                PostalAddressT.Member.postalAddressLine0,
		                                "MyCompany " + id + " Corp"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalAddressLine1,
		                                "c/o CEO"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalStreet0,
		                                "Business Street " + id
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalStreetNumber,
		                                Integer.toString(id)
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCity,
		                                "Gotham"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCode,
		                                "12345"
		                            ),
		                            Datatypes.member(
		                                PostalAddressT.Member.postalCountry,
		                                276 // Germany
		                            )
		                        )
		                    ),
		                    Datatypes.member(
		                    	LegalEntityT.Member.emailAddressBusiness,
		                        Datatypes.create(
		                            EmailAddressT.class,
		                            Datatypes.member(
		                                EmailAddressT.Member.emailAddress,
		                                "info@" + id + ".com"
		                            ),
		                            Datatypes.member(
		                                EmailAddressT.Member.emailValid,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),
		                    Datatypes.member(
		                    	LegalEntityT.Member.phoneNumberBusiness,
		                        Datatypes.create(
		                            PhoneNumberT.class,
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumber,
		                                "+41(22)77777-" + id
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumberVerified,
		                                Boolean.TRUE
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerName,
		                                37
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerVerified,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),                    
		                    Datatypes.member(
		                    	LegalEntityT.Member.faxNumberBusiness,
		                        Datatypes.create(
		                            PhoneNumberT.class,
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumber,
		                                "+41(22)66666-" + id
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.phoneNumberVerified,
		                                Boolean.TRUE
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerName,
		                                37
		                            ),
		                            Datatypes.member(
		                                PhoneNumberT.Member.providerVerified,
		                                Boolean.TRUE
		                            )
		                        )
		                    ),                    
		                    Datatypes.member(
		                    	LegalEntityT.Member.webAddressBusiness,
		                        "http://www.mycompany-" + id + ".de"
		                    )
		                )
		            )
                )
            )
        );
        UpdateCustomerResult updateCustomerResult = null;
        updateCustomerResult = this.updateCustomer(
            updateCustomerParams
        );        
        this.logResult("updateCustomer", updateCustomerResult.getStatus());
        // Create customer contract if customer does not already have one
        if(
        	getCustomerResult.getCustomer() != null && 
        	getCustomerResult.getCustomer().getCustomerContract() == null
        ) {
	        // Create customer contract
	        CreateCustomerContractParams createCustomerContractParams = Datatypes.create(
	            CreateCustomerContractParams.class,
	            Datatypes.member(
	                CreateCustomerContractParams.Member.customerContract,
	                Datatypes.create(
	                    CustomerContractT.class,
	                    Datatypes.member(
	                    	CustomerContractT.Member.customerNumber,
	                        customerNumber
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.acceptedLegal,
	                        Boolean.TRUE
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.acceptedMarketing,
	                        Boolean.TRUE
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.acceptedPrivateDataForwarding,
	                        Boolean.TRUE
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.referrer,
	                        "Referrer"
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.contactSource,
	                        33
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.salesTaxType,
	                        "Sales Tax 0%"
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.contractCurrency,
	                        978
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.noBilling,
	                        Boolean.FALSE
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.billingPartner,
	                        "Billing Partner"
	                    ),
	                    Datatypes.member(
	                        CustomerContractT.Member.billingPartnerRegistrationId,
	                        Arrays.asList(new String[]{"RegId1", "RegId2"})
	                    )
	                )
	            )
	        );
	        CreateCustomerContractResult createCustomerContractResult = null;
	        createCustomerContractResult = this.createCustomerContract(
	            createCustomerContractParams
	        );
	        this.logResult("createCustomerContract", createCustomerContractResult.getStatus());
        }
        return getCustomerResult.getStatus();
    }
        
    //-----------------------------------------------------------------------
    public ReturnStatusT testInvoices(
    ) {        
        System.out.println(new Date() + "   ---------- testInvoices");        
        // Get an existing customer
        String customerNumber = null;
        for(int i = 0; i < 100; i++) {
            int id = new Double(Math.random() * 100.0).intValue();
            String userName = Integer.toString(id);
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            this.logResult("getCredentials", getCredentialsResult.getStatus());            
            if(getCredentialsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                customerNumber = getCredentialsResult.getCustomerNumber();
                break;
            }
        }
        if(customerNumber == null) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{});
        }
        // Get sales orders
        GetSalesOrdersParams getSalesOrdersParams = Datatypes.create(
            GetSalesOrdersParams.class,
            Datatypes.member(
                GetSalesOrdersParams.Member.customerNumber,
                customerNumber
            )
        );
        GetSalesOrdersResult getSalesOrdersResult = null;
        getSalesOrdersResult = this.getSalesOrders(
            getSalesOrdersParams
        );
        this.logResult("getSalesOrders", getSalesOrdersResult.getStatus());
        if(
            (getSalesOrdersResult.getStatus().getReturnCode() != BasicException.Code.NONE) || 
            (getSalesOrdersResult.getSalesOrder().size() < 1)
        ) {
            return getSalesOrdersResult.getStatus();
        }
        // Create invoice based on a sales order
        CreateInvoiceFromSalesOrderParams createInvoiceFromSalesOrderParams = Datatypes.create(
            CreateInvoiceFromSalesOrderParams.class,
            Datatypes.member(
                CreateInvoiceFromSalesOrderParams.Member.salesOrderNumber,
                getSalesOrdersResult.getSalesOrder().get(new Double(Math.random() * getSalesOrdersResult.getSalesOrder().size()).intValue()).getContract().getContractNumber()
            )
        );
        CreateInvoiceFromSalesOrderResult createInvoiceFromSalesOrderResult = null;
        createInvoiceFromSalesOrderResult = this.createInvoiceFromSalesOrder(
            createInvoiceFromSalesOrderParams
        );
        this.logResult("createInvoiceFromSalesOrder", createInvoiceFromSalesOrderResult.getStatus());
        if(createInvoiceFromSalesOrderResult.getStatus().getReturnCode() != BasicException.Code.NONE) {
            return createInvoiceFromSalesOrderResult.getStatus();
        }
        // Get invoice
        GetInvoiceParams getInvoiceParams = Datatypes.create(
            GetInvoiceParams.class,
            Datatypes.member(
                GetInvoiceParams.Member.invoiceNumber,
                createInvoiceFromSalesOrderResult.getInvoice().getContract().getContractNumber()
            )
        );
        GetInvoiceResult getInvoiceResult = null;
        getInvoiceResult = this.getInvoice(
            getInvoiceParams
        );
        this.logResult("getInvoice", getInvoiceResult.getStatus());        
        // Get invoices
        GetInvoicesParams getInvoicesParams = Datatypes.create(
            GetInvoicesParams.class,
            Datatypes.member(
                GetInvoicesParams.Member.customerNumber,
                customerNumber
            )
        );
        GetInvoicesResult getInvoicesResult = null;
        getInvoicesResult = this.getInvoices(
            getInvoicesParams
        );
        this.logResult("getInvoices", getInvoicesResult.getStatus());                        
        // Set invoice status
        SetInvoiceStatusParams setInvoiceStatusParams = Datatypes.create(
            SetInvoiceStatusParams.class,
            Datatypes.member(
                SetInvoiceStatusParams.Member.invoiceNumber,
                createInvoiceFromSalesOrderResult.getInvoice().getContract().getContractNumber()
            ),
            Datatypes.member(
                SetInvoiceStatusParams.Member.invoiceStatus,
                Datatypes.create(
                    ContractStatusT.class,
                    Datatypes.member(
                        ContractStatusT.Member.status,
                        InvoiceState.PAID.getValue()
                    ),
                    Datatypes.member(
                         ContractStatusT.Member.description,
                        "Set status to " + InvoiceState.PAID
                    ),
                    Datatypes.member(
                        ContractStatusT.Member.tag,
                        new StringPropertyT[]{
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "tag1"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 1.1"}
                                ),                                
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 1.2"}
                                )                                
                            ),
                            Datatypes.create(
                                StringPropertyT.class,
                                Datatypes.member(
                                    StringPropertyT.Member.name,
                                    "tag2"
                                ),
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 2.1"}
                                ),                                
                                Datatypes.member(
                                    StringPropertyT.Member.stringValue,
                                    new String[]{"tag value 2.2"}
                                )                                
                            )
                        }
                    )                    
                )
            )
        );
        SetInvoiceStatusResult setInvoiceStatusResult = null;
        setInvoiceStatusResult = this.setInvoiceStatus(
            setInvoiceStatusParams
        );
        this.logResult("setInvoiceStatus", setInvoiceStatusResult.getStatus());        
        return newOperationStatus(BasicException.Code.NONE, new String[]{});
    }

    //-----------------------------------------------------------------------
    public ReturnStatusT testVouchers(
    ) {   
        System.out.println(new Date() + "   ---------- testVouchers");        
        // Get voucher customer
        String voucherCustomerNumber = null;
        for(int i = 0; i < 100; i++) {
            int id = new Double(Math.random() * 100.0).intValue();
            String userName = Integer.toString(id);
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            this.logResult("getCredentials", getCredentialsResult.getStatus());            
            if(getCredentialsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                voucherCustomerNumber = getCredentialsResult.getCustomerNumber();
                break;
            }
        }
        if(voucherCustomerNumber == null) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{});
        }
        GetCustomerParams getCustomerParams = Datatypes.create(
            GetCustomerParams.class,
            Datatypes.member(
                GetCustomerParams.Member.customerNumber,
                voucherCustomerNumber
            )
        );
        GetCustomerResult getCustomerResult = null;
        getCustomerResult = this.getCustomer(
            getCustomerParams
        );
        this.logResult("getCustomer", getCustomerResult.getStatus());            
        CustomerT voucherCustomerT = getCustomerResult.getCustomer();   
        // Get receiving customer
        String receivingCustomerNumber = null;
        for(int i = 0; i < 100; i++) {
            int id = new Double(Math.random() * 100.0).intValue();
            String userName = Integer.toString(id);
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            this.logResult("getCredentials", getCredentialsResult.getStatus());            
            if(getCredentialsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                receivingCustomerNumber = getCredentialsResult.getCustomerNumber();
                break;
            }
        }
        if(receivingCustomerNumber == null) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{});
        }
        getCustomerParams = Datatypes.create(
            GetCustomerParams.class,
            Datatypes.member(
                GetCustomerParams.Member.customerNumber,
                receivingCustomerNumber
            )
        );
        getCustomerResult = null;
        getCustomerResult = this.getCustomer(
            getCustomerParams
        );
        this.logResult("getCustomer", getCustomerResult.getStatus());            
        // Get at most three existing products
        List<ProductT> productsT = new ArrayList<ProductT>();
        for(int i = 0; i < 100; i++) {
            int id = new Double(Math.random() * 100.0).intValue();
            String productNumber = Integer.toString(id);
            GetProductsParams getProductsParams = Datatypes.create(
                GetProductsParams.class,
                Datatypes.member(
                    GetProductsParams.Member.productNumber,
                    new String[]{productNumber}
                ),
                Datatypes.member(
                    GetProductsParams.Member.returnPictureContent,
                    Boolean.FALSE
                )
            );
            GetProductsResult getProductsResult = null;
            getProductsResult = this.getProducts(
                getProductsParams
            );
            this.logResult("getProducts", getProductsResult.getStatus());            
            if(getProductsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                productsT.addAll(getProductsResult.getProduct());
                if(productsT.size() > 3) break;
            }
        }        
        // Prepare voucher positions
        List<ContractPositionT> contractPositionsT = new ArrayList<ContractPositionT>();
        for(ProductT productT: productsT) {
            ContractPositionT contractPositionT = Datatypes.create(
                ContractPositionT.class,
                Datatypes.member(
                    ContractPositionT.Member.productNumber,
                    productT.getProductNumber()
                ),
                Datatypes.member(
                    ContractPositionT.Member.quantity,
                    "1.0"
                ),
                Datatypes.member(
                    ContractPositionT.Member.pricingDate,
                    new Date()
                ),
                Datatypes.member(
                    ContractPositionT.Member.priceUom,
                    productT.getPriceUom().get(new Double(Math.random() * productT.getPriceUom().size()).intValue())
                )
            );
            contractPositionsT.add(contractPositionT);
        }
        // Prepare voucher
        InvoiceT voucherT = Datatypes.create(
            InvoiceT.class,
            Datatypes.member(
                InvoiceT.Member.contract,
                Datatypes.create(
                    ContractT.class,
                    Datatypes.member(
                        ContractT.Member.customerNumber,
                        voucherCustomerT.getCustomerNumber()
                    ),
                    Datatypes.member(
                        ContractT.Member.activeOn,
                        new Date()
                    ),
                    Datatypes.member(
                        ContractT.Member.position,
                        contractPositionsT
                    ),             
                    Datatypes.member(
                        ContractT.Member.postalAddressDelivery,
                        voucherCustomerT.getContact().getPostalAddressHome()
                    ),                    
                    Datatypes.member(
                        ContractT.Member.postalAddressInvoice,
                        voucherCustomerT.getContact().getPostalAddressHome()
                    )                    
                )
            ),
            Datatypes.member(
                InvoiceT.Member.isVoucher,
                Boolean.TRUE
            )
        );
        // Create voucher
        CreateInvoiceParams createVoucherParams = Datatypes.create(
            CreateInvoiceParams.class,
            Datatypes.member(
                CreateInvoiceParams.Member.invoice,
                voucherT
            )
        );            
        CreateInvoiceResult createVoucherResult = null;
        createVoucherResult = this.createInvoice(
            createVoucherParams
        );
        this.logResult("createInvoice", createVoucherResult.getStatus());
        if(createVoucherResult.getStatus().getReturnCode() != BasicException.Code.NONE) {
            return createVoucherResult.getStatus(); 
        }
        // Get voucher
        GetInvoiceParams getVoucherParams = Datatypes.create(
            GetInvoiceParams.class,
            Datatypes.member(
                GetInvoiceParams.Member.invoiceNumber,
                createVoucherResult.getInvoice().getContract().getContractNumber()
            )
        );
        GetInvoiceResult getVoucherResult = null;
        getVoucherResult = this.getInvoice(
            getVoucherParams
        );
        this.logResult("getInvoice", getVoucherResult.getStatus());        
        return getVoucherResult.getStatus();
    }
        
    //-----------------------------------------------------------------------
    public ReturnStatusT testCodeValues(
    ) {
        System.out.println(new Date() + "   ---------- testCodeValues");        
        // Get root containers
        GetCodeValueContainerParams getCodeValueContainerParams = Datatypes.create(
            GetCodeValueContainerParams.class,
            Datatypes.member(
                GetCodeValueContainerParams.Member.containerName,
                Arrays.asList(new String[]{
                    "org:opencrx:kernel:account1:Account:accountCategory", 
                    "org:opencrx:kernel:account1:Account:accountState", 
                    "org:opencrx:kernel:account1:Contact:annualIncomeCurrency" 
                })
            ),
            Datatypes.member(
                GetCodeValueContainerParams.Member.rootContainers,
                Boolean.TRUE
            )
        );
        GetCodeValueContainerResult getCodeValueContainerResult = null;
        getCodeValueContainerResult = this.getCodeValueContainer(
            getCodeValueContainerParams
        );
        this.logResult("getCodeValueContainer", getCodeValueContainerResult.getStatus());                    
        // Get non-root containers
        getCodeValueContainerParams = Datatypes.create(
            GetCodeValueContainerParams.class,
            Datatypes.member(
                GetCodeValueContainerParams.Member.containerName,
                Arrays.asList(new String[]{"AUDIOTYPE", "DOWNLOADS"})
            ),
            Datatypes.member(
                GetCodeValueContainerParams.Member.rootContainers,
                Boolean.FALSE
            )
        );
        getCodeValueContainerResult = null;
        getCodeValueContainerResult = this.getCodeValueContainer(
            getCodeValueContainerParams
        );
        this.logResult("getCodeValueContainer", getCodeValueContainerResult.getStatus());                            
        return getCodeValueContainerResult.getStatus();
    }
    
    //-----------------------------------------------------------------------
    public ReturnStatusT testRegisterCustomer(
    ) {    	
        System.out.println(new Date() + "   ---------- testRegisterCustomer");   
        // Get an existing customer
        String customerNumber = null;
        int id = -1;
        for(int i = 0; i < 100; i++) {
            id = new Double(Math.random() * 100.0).intValue();
            String userName = Integer.toString(id);
            GetCredentialsParams getCredentialsParams = Datatypes.create(
                GetCredentialsParams.class,
                Datatypes.member(
                    GetCredentialsParams.Member.userName,
                    userName
                )
            );
            GetCredentialsResult getCredentialsResult = null;
            getCredentialsResult = this.getCredentials(
                getCredentialsParams
            );
            this.logResult("getCredentials", getCredentialsResult.getStatus());            
            if(getCredentialsResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
                customerNumber = getCredentialsResult.getCustomerNumber();
                break;
            }
        }
        if(customerNumber == null) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{"Customer", Integer.toString(id)});
        }
        // Find legal entity
        String legalName = "Company-" + Integer.toString(id);
        GetCustomersByQueryParams getCustomersByQueryParams = Datatypes.create(
        	GetCustomersByQueryParams.class,
            Datatypes.member(
            	GetCustomersByQueryParams.Member.legalName,
                legalName
            )
        );
        GetCustomersByQueryResult getCustomersByQueryResult = null;
        getCustomersByQueryResult = this.getCustomersByQuery(
        	getCustomersByQueryParams
        );
        // Create legal entity if none is found
        if(getCustomersByQueryResult.getCustomerNumber().isEmpty()) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{"Company", Integer.toString(id)});
        }
        String legalEntityNumber = getCustomersByQueryResult.getCustomerNumber().iterator().next();
        // Get legal entity
        GetCustomerParams getCustomerParams = Datatypes.create(
            GetCustomerParams.class,
            Datatypes.member(
                GetCustomerParams.Member.customerNumber,
                legalEntityNumber
            )            
        );
        GetCustomerResult getCustomerResult = null;
        getCustomerResult = this.getCustomer(
            getCustomerParams
        );
        this.logResult("getCustomer", getCustomerResult.getStatus());
        // Assert that legal entity has a customer contract
        if(getCustomerResult.getCustomer() == null || getCustomerResult.getCustomer().getCustomerContract() == null || getCustomerResult.getCustomer().getCustomerContract().isEmpty()) {
            return newOperationStatus(BasicException.Code.NOT_FOUND, new String[]{"CustomerContract", Integer.toString(id)});        	
        }
        // Add customer to customer contract
        AddCustomerToCustomerContractParams addCustomerToCustomerContractParams = Datatypes.create(
        	AddCustomerToCustomerContractParams.class,
            Datatypes.member(
            	AddCustomerToCustomerContractParams.Member.customerNumber,
                customerNumber
            ),            
            Datatypes.member(
            	AddCustomerToCustomerContractParams.Member.customerContractNumber,
            	getCustomerResult.getCustomer().getCustomerContract().get(0).getContractNumber()
            )            
        );
        AddCustomerToCustomerContractResult addCustomerToCustomerContractResult = this.addCustomerToCustomerContract(
        	addCustomerToCustomerContractParams
        );
        return addCustomerToCustomerContractResult.getStatus();
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    
}

//--- End of File -----------------------------------------------------------
