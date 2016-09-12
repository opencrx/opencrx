/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: TestShopService
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

public class TestShopService extends AbstractTestShopService {

    //-----------------------------------------------------------------------
    public TestShopService(
    	org.opencrx.application.shop1.cci2.ShopService shopService
    ) {
        this.shopService = shopService;
    }

    //-----------------------------------------------------------------------
    @Override
    public org.opencrx.application.shop1.cci2.AddActivityFollowUpResult addActivityFollowUp(
    	org.opencrx.application.shop1.cci2.AddActivityFollowUpParams in
    ) {
    	return this.shopService.addActivityFollowUp(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.AddCustomerToCustomerContractResult addCustomerToCustomerContract(
    	org.opencrx.application.shop1.cci2.AddCustomerToCustomerContractParams in
    ) {
    	return this.shopService.addCustomerToCustomerContract(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.AddDeliveryInformationResult addDeliveryInformation(
    	org.opencrx.application.shop1.cci2.AddDeliveryInformationParams in
    ) {
    	return this.shopService.addDeliveryInformation(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.AddSalesOrderPositionResult addSalesOrderPosition(
    	org.opencrx.application.shop1.cci2.AddSalesOrderPositionParams in
    ) {
    	return this.shopService.addSalesOrderPosition(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CancelInvoiceResult cancelInvoice(
    	org.opencrx.application.shop1.cci2.CancelInvoiceParams in
    ) {
    	return this.shopService.cancelInvoice(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CancelSalesOrderResult cancelSalesOrder(
    	org.opencrx.application.shop1.cci2.CancelSalesOrderParams in
    ) {
    	return this.shopService.cancelSalesOrder(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateActivityResult createActivity(
    	org.opencrx.application.shop1.cci2.CreateActivityParams in
    ) {
    	return this.shopService.createActivity(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateCustomerAsContactResult createCustomerAsContact(
    	org.opencrx.application.shop1.cci2.CreateCustomerAsContactParams in
    ) {
    	return this.shopService.createCustomerAsContact(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateCustomerAsLegalEntityResult createCustomerAsLegalEntity(
    	org.opencrx.application.shop1.cci2.CreateCustomerAsLegalEntityParams in
    ) {
    	return this.shopService.createCustomerAsLegalEntity(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateCustomerContractResult createCustomerContract(
    	org.opencrx.application.shop1.cci2.CreateCustomerContractParams in
    ) {
    	return this.shopService.createCustomerContract(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateInvoiceResult createInvoice(
    	org.opencrx.application.shop1.cci2.CreateInvoiceParams in
    ) {
    	return this.shopService.createInvoice(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateInvoiceFromInvoiceResult createInvoiceFromInvoice(
    	org.opencrx.application.shop1.cci2.CreateInvoiceFromInvoiceParams in
    ) {
    	return this.shopService.createInvoiceFromInvoice(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderResult createInvoiceFromSalesOrder(
    	org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderParams in
    ) {
    	return this.shopService.createInvoiceFromSalesOrder(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateProductClassificationResult createProductClassification(
    	org.opencrx.application.shop1.cci2.CreateProductClassificationParams in
    ) {
    	return this.shopService.createProductClassification(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateProductsResult createProducts(
    	org.opencrx.application.shop1.cci2.CreateProductsParams in
    ) {
    	return this.shopService.createProducts(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.CreateSalesOrderResult createSalesOrder(
    	org.opencrx.application.shop1.cci2.CreateSalesOrderParams in
    ) {
    	return this.shopService.createSalesOrder(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetActivitiesByQueryResult getActivitiesByQuery(
    	org.opencrx.application.shop1.cci2.GetActivitiesByQueryParams in
    ) {
    	return this.shopService.getActivitiesByQuery(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetActivityResult getActivity(
    	org.opencrx.application.shop1.cci2.GetActivityParams in
    ) {
    	return this.shopService.getActivity(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetCodeValueContainerResult getCodeValueContainer(
    	org.opencrx.application.shop1.cci2.GetCodeValueContainerParams in
    ) {
    	return this.shopService.getCodeValueContainer(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetCredentialsResult getCredentials(
    	org.opencrx.application.shop1.cci2.GetCredentialsParams in
    ) {
    	return this.shopService.getCredentials(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetCredentialsByEmailAddressResult getCredentialsByEmailAddress(
    	org.opencrx.application.shop1.cci2.GetCredentialsByEmailAddressParams in
    ) {
    	return this.shopService.getCredentialsByEmailAddress(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetCustomerResult getCustomer(
    	org.opencrx.application.shop1.cci2.GetCustomerParams in
    ) {
    	return this.shopService.getCustomer(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetCustomersByQueryResult getCustomersByQuery(
    	org.opencrx.application.shop1.cci2.GetCustomersByQueryParams in
    ) {
    	return this.shopService.getCustomersByQuery(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetDocumentsResult getDocuments(
    	org.opencrx.application.shop1.cci2.GetDocumentsParams in
    ) {
    	return this.shopService.getDocuments(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetInvoiceResult getInvoice(
    	org.opencrx.application.shop1.cci2.GetInvoiceParams in
    ) {
    	return this.shopService.getInvoice(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetInvoicePositionsResult getInvoicePositions(
    	org.opencrx.application.shop1.cci2.GetInvoicePositionsParams in
    ) {
    	return this.shopService.getInvoicePositions(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetInvoicesResult getInvoices(
    	org.opencrx.application.shop1.cci2.GetInvoicesParams in
    ) {
    	return this.shopService.getInvoices(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetPriceLevelResult getPriceLevel(
    	org.opencrx.application.shop1.cci2.GetPriceLevelParams in
    ) {
    	return this.shopService.getPriceLevel(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetProductConfigurationTypesResult getProductConfigurationTypes(
    	org.opencrx.application.shop1.cci2.GetProductConfigurationTypesParams in
    ) {
    	return this.shopService.getProductConfigurationTypes(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetProductPricesResult getProductPrices(
    	org.opencrx.application.shop1.cci2.GetProductPricesParams in
    ) {
    	return this.shopService.getProductPrices(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetProductsResult getProducts(
    	org.opencrx.application.shop1.cci2.GetProductsParams in
    ) {
    	return this.shopService.getProducts(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetProductsByQueryResult getProductsByQuery(
    	org.opencrx.application.shop1.cci2.GetProductsByQueryParams in
    ) {
    	return this.shopService.getProductsByQuery(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetSalesOrderResult getSalesOrder(
    	org.opencrx.application.shop1.cci2.GetSalesOrderParams in
    ) {
    	return this.shopService.getSalesOrder(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetSalesOrdersResult getSalesOrders(
    	org.opencrx.application.shop1.cci2.GetSalesOrdersParams in
    ) {
    	return this.shopService.getSalesOrders(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.GetSalesOrderPositionsResult getSalesOrderPositions(
    	org.opencrx.application.shop1.cci2.GetSalesOrderPositionsParams in
    ) {
    	return this.shopService.getSalesOrderPositions(in);
    }
    
    @Override
    public org.opencrx.application.shop1.cci2.SendEMailResult sendEMail(
    	org.opencrx.application.shop1.cci2.SendEMailParams in
    ) {
    	return this.shopService.sendEMail(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.SetCredentialsResult setCredentials(
    	org.opencrx.application.shop1.cci2.SetCredentialsParams in
    ) {
    	return this.shopService.setCredentials(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.SetCustomerStatusResult setCustomerStatus(
    	org.opencrx.application.shop1.cci2.SetCustomerStatusParams in
    ) {
    	return this.shopService.setCustomerStatus(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.SetCustomerContractStatusResult setCustomerContractStatus(
    	org.opencrx.application.shop1.cci2.SetCustomerContractStatusParams in
    ) {
    	return this.shopService.setCustomerContractStatus(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.SetInvoiceStatusResult setInvoiceStatus(
    	org.opencrx.application.shop1.cci2.SetInvoiceStatusParams in
    ) {
    	return this.shopService.setInvoiceStatus(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.SetProductStatusResult setProductStatus(
    	org.opencrx.application.shop1.cci2.SetProductStatusParams in
    ) {
    	return this.shopService.setProductStatus(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.SetSalesOrderPositionQuantityResult setSalesOrderPositionQuantity(
    	org.opencrx.application.shop1.cci2.SetSalesOrderPositionQuantityParams in
    ) {
    	return this.shopService.setSalesOrderPositionQuantity(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.SetSalesOrderStatusResult setSalesOrderStatus(
    	org.opencrx.application.shop1.cci2.SetSalesOrderStatusParams in
    ) {
    	return this.shopService.setSalesOrderStatus(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.UpdateCustomerResult updateCustomer(
    	org.opencrx.application.shop1.cci2.UpdateCustomerParams in
    ) {
    	return this.shopService.updateCustomer(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.UpdateCustomerContractResult updateCustomerContract(
    	org.opencrx.application.shop1.cci2.UpdateCustomerContractParams in
    ) {
    	return this.shopService.updateCustomerContract(in);
    }

    @Override
    public org.opencrx.application.shop1.cci2.UpdateProductResult updateProduct(
    	org.opencrx.application.shop1.cci2.UpdateProductParams in
    ) {
    	return this.shopService.updateProduct(in);
    }
    
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private final org.opencrx.application.shop1.cci2.ShopService shopService;
    
}

//--- End of File -----------------------------------------------------------
