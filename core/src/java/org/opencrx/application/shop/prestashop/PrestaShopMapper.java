/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: PrestaShopMapper
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011, CRIXP Corp., Switzerland
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
package org.opencrx.application.shop.prestashop;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.jdo.PersistenceManager;

import org.opencrx.application.shop1.cci2.AddDeliveryInformationParams;
import org.opencrx.application.shop1.cci2.AddSalesOrderPositionParams;
import org.opencrx.application.shop1.cci2.ContactT;
import org.opencrx.application.shop1.cci2.ContractPositionT;
import org.opencrx.application.shop1.cci2.ContractStatusT;
import org.opencrx.application.shop1.cci2.ContractT;
import org.opencrx.application.shop1.cci2.CreateCustomerAsContactParams;
import org.opencrx.application.shop1.cci2.CreateCustomerAsContactResult;
import org.opencrx.application.shop1.cci2.CreateCustomerContractParams;
import org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderParams;
import org.opencrx.application.shop1.cci2.CreateInvoiceFromSalesOrderResult;
import org.opencrx.application.shop1.cci2.CreateProductsParams;
import org.opencrx.application.shop1.cci2.CreateSalesOrderParams;
import org.opencrx.application.shop1.cci2.CreateSalesOrderResult;
import org.opencrx.application.shop1.cci2.CredentialsT;
import org.opencrx.application.shop1.cci2.CustomerContractT;
import org.opencrx.application.shop1.cci2.CustomerT;
import org.opencrx.application.shop1.cci2.DeliveryInformationT;
import org.opencrx.application.shop1.cci2.EmailAddressT;
import org.opencrx.application.shop1.cci2.GetCredentialsParams;
import org.opencrx.application.shop1.cci2.GetCredentialsResult;
import org.opencrx.application.shop1.cci2.GetInvoiceParams;
import org.opencrx.application.shop1.cci2.GetInvoiceResult;
import org.opencrx.application.shop1.cci2.GetProductsParams;
import org.opencrx.application.shop1.cci2.GetProductsResult;
import org.opencrx.application.shop1.cci2.GetSalesOrderParams;
import org.opencrx.application.shop1.cci2.GetSalesOrderResult;
import org.opencrx.application.shop1.cci2.PhoneNumberT;
import org.opencrx.application.shop1.cci2.PostalAddressT;
import org.opencrx.application.shop1.cci2.ProductDescriptionT;
import org.opencrx.application.shop1.cci2.ProductT;
import org.opencrx.application.shop1.cci2.SalesOrderT;
import org.opencrx.application.shop1.cci2.SetCredentialsParams;
import org.opencrx.application.shop1.cci2.SetInvoiceStatusParams;
import org.opencrx.application.shop1.cci2.SetSalesOrderPositionQuantityParams;
import org.opencrx.application.shop1.cci2.SetSalesOrderStatusParams;
import org.opencrx.application.shop1.cci2.UpdateCustomerParams;
import org.opencrx.application.shop1.cci2.UpdateCustomerResult;
import org.opencrx.application.shop1.cci2.UpdateProductParams;
import org.opencrx.application.shop1.cci2.UpdateProductResult;
import org.opencrx.application.shop1.datatypes.InvoiceState;
import org.opencrx.application.shop1.datatypes.SalesOrderState;
import org.opencrx.application.shop1.service.ShopServiceImpl;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.LegalEntity;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.opencrx.kernel.contract1.jmi1.SalesOrder;
import org.opencrx.kernel.product1.cci2.PriceLevelQuery;
import org.opencrx.kernel.product1.cci2.ProductBasePriceQuery;
import org.opencrx.kernel.product1.jmi1.PriceLevel;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.product1.jmi1.ProductBasePrice;
import org.opencrx.kernel.product1.jmi1.SalesTaxType;
import org.opencrx.kernel.uom1.cci2.UomScheduleQuery;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.opencrx.kernel.uom1.jmi1.UomSchedule;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.w3c.spi2.Datatypes;

public class PrestaShopMapper {

    //-----------------------------------------------------------------------    
	public PrestaShopMapper(
		PersistenceManager pm,
		String providerName,
		String segmentName,
		String shopName,
		boolean emailAddressMustBeUnique,
		Connection conn,
		String tablePrefix
	) {
		this.pm = pm;
		this.shopName = shopName;
		this.conn = conn;
		this.tablePrefix = tablePrefix;
		this.shopService = new ShopServiceImpl(
			pm,
			providerName,
			segmentName,
			shopName,
			emailAddressMustBeUnique, 
			true, // noCopyOfProductConfiguration
			new org.opencrx.application.shop1.datatypes.DatatypeMappers()
		);
		// UOM_NAME_PIECE
		Uom uomPiece = this.shopService.findUom(UOM_NAME_PIECE);
		if(uomPiece == null) {
			org.opencrx.kernel.uom1.jmi1.Segment uomSegment = this.shopService.getUomSegment();
			this.pm.currentTransaction().begin();
			UomSchedule uomSchedule = null;
			UomScheduleQuery uomScheduleQuery = (UomScheduleQuery)this.pm.newQuery(UomSchedule.class);
			uomScheduleQuery.name().equalTo("Default");
			List<UomSchedule> uomSchedules = uomSegment.getUomSchedule(uomScheduleQuery);
			if(uomSchedules.isEmpty()) {
				uomSchedule = this.pm.newInstance(UomSchedule.class);
				uomSchedule.setName("Default");
				uomSegment.addUomSchedule(
					this.shopService.uuidAsString(),
					uomSchedule
				);
			} else {
				uomSchedule = uomSchedules.iterator().next();
			}
			uomPiece = this.pm.newInstance(Uom.class);
			uomPiece.setName(UOM_NAME_PIECE);
			uomPiece.setUomSchedule(uomSchedule);
			uomSegment.addUom(
				this.shopService.uuidAsString(),
				uomPiece
			);
			this.pm.currentTransaction().commit();
		}
	}
	
    //-----------------------------------------------------------------------    
    private PreparedStatement getPreparedStatement(
    	Connection conn,
    	String s
    ) throws SQLException {
    	SQLException lastException = null;
    	long delay = 2000;
    	prepareStatement: for(int i = 0; i < 10; i++) {
	    	try {
	    		return conn.prepareStatement(s);
	    	}
	    	catch(SQLException e) {
	    		lastException = e;
	    		if(e.getMessage().indexOf("java.net.SocketException: Connection reset") > 0) {
	    			try {
	    				Thread.sleep(delay);
	    			} catch(Exception ex) {}
	    			delay += 2000;
	    			continue prepareStatement;
	    		}
	    		throw e;
	    	}
    	}
    	throw lastException;
    }
    
    //-----------------------------------------------------------------------    
    protected void closePreparedStatement(
       PreparedStatement ps
    ) {
    	if(ps != null) {
    		try {
    			ps.close();
    		} catch(Exception e) {}
    	}
    }
    	
    //-----------------------------------------------------------------------    
    protected void closeResultSet(
       ResultSet rs
    ) {
    	if(rs != null) {
    		try {
    			rs.close();
    		} catch(Exception e) {}
    	}
    }

    //-----------------------------------------------------------------------
    protected String getProductNumber(
    	int id_product,
    	int id_product_attribute
    ) {
    	return this.shopName + "-" + PRODUCT_NUMBER_FORMAT.format(id_product) + "-" + PRODUCT_ATTRIBUTE_FORMAT.format(id_product_attribute);
    }

    //-----------------------------------------------------------------------
    protected String getUserName(
    	int id_customer
    ) {
    	return USER_NUMBER_FORMAT.format(id_customer) + "@" + this.shopName;
    }

    //-----------------------------------------------------------------------
    protected String getSalesOrderNumber(
    	String customerNumber,
    	int id_order
    ) {
    	return customerNumber + "-" + ORDER_NUMBER_FORMAT.format(id_order) + "S-0000";    	
    }
    
    //-----------------------------------------------------------------------
    protected String bigDecimalToString(
        BigDecimal value
    ) {
        return value == null ? null : value.toPlainString();
    }

    //-----------------------------------------------------------------------
    protected void createOrUpdateSalesTax(
    	String name,
    	BigDecimal rate
    ) throws ServiceException {
    	// Map sales tax
    	SalesTaxType salesTaxType = this.shopService.findSalesTaxType(name);
    	if(salesTaxType == null) {
    		org.opencrx.kernel.product1.jmi1.Segment productSegment = this.shopService.getProductSegment();
    		salesTaxType = this.pm.newInstance(SalesTaxType.class);
    		salesTaxType.setName(name);
    		salesTaxType.setRate(rate);
    		this.pm.currentTransaction().begin();
    		productSegment.addSalesTaxType(
    			this.shopService.uuidAsString(),
    			salesTaxType
    		);
    		this.pm.currentTransaction().commit();
    	}    	
    }
    
    //-----------------------------------------------------------------------
	public void mapOrder(
		ResultSet orders
	) throws ServiceException {
		PreparedStatement ps = null;
		try {
			int id_order = orders.getInt("id_order");
			int id_carrier = orders.getInt("id_carrier");
			int id_lang = orders.getInt("id_lang");
			int id_customer = orders.getInt("id_customer");
			int id_cart = orders.getInt("id_cart");
			int id_currency = orders.getInt("id_currency");
			int id_address_delivery = orders.getInt("id_address_delivery");
			int id_address_invoice = orders.getInt("id_address_invoice");
			String secure_key = orders.getString("secure_key");
			String payment = orders.getString("payment");
			BigDecimal conversion_rate = orders.getBigDecimal("conversion_rate");
			String module = orders.getString("module");
			int recyclable = orders.getInt("recyclable");
			int gift = orders.getInt("gift");
			String gift_message = orders.getString("gift_message");
			String shipping_number = orders.getString("shipping_number");
			BigDecimal total_discounts = orders.getBigDecimal("total_discounts");
			BigDecimal total_paid = orders.getBigDecimal("total_paid");
			BigDecimal total_paid_real = orders.getBigDecimal("total_paid_real");
			BigDecimal total_products = orders.getBigDecimal("total_products");
			BigDecimal total_products_wt = orders.getBigDecimal("total_products_wt");
			BigDecimal total_shipping = orders.getBigDecimal("total_shipping");
			BigDecimal carrier_tax_rate = orders.getBigDecimal("carrier_tax_rate");
			BigDecimal total_wrapping = orders.getBigDecimal("total_wrapping");
			int invoice_number = orders.getInt("invoice_number");
			int delivery_number = orders.getInt("delivery_number");
			Timestamp invoice_date = null;
			try {
				invoice_date = orders.getTimestamp("invoice_date");
			} catch(Exception e) {}
			Timestamp delivery_date = null;
			try {
				delivery_date = orders.getTimestamp("delivery_date");
			} catch(Exception e) {}
			int valid = orders.getInt("valid");
			Timestamp date_add = orders.getTimestamp("date_add");
			Timestamp date_upd = orders.getTimestamp("date_upd");

            // Map currency
            int contractCurrency = 0;
            ps = this.getPreparedStatement(
            	this.conn,
            	"SELECT * FROM " + this.tablePrefix + "currency WHERE id_currency = ?"
            );
            ps.setInt(1, id_currency);
            ResultSet currencies = ps.executeQuery();
            while(currencies.next()) {
            	contractCurrency = currencies.getShort("iso_code_num");
            }
            this.closeResultSet(currencies);
            this.closePreparedStatement(ps);
            
            // Get sales order
            String customerNumber = this.getCustomerNumber(id_customer);
            String salesOrderNumber = this.getSalesOrderNumber(customerNumber, id_order);
            GetSalesOrderParams getSalesOrderParams = Datatypes.create(
                GetSalesOrderParams.class,
                Datatypes.member(
                    GetSalesOrderParams.Member.salesOrderNumber,
                    salesOrderNumber
                )
            );
            GetSalesOrderResult getSalesOrderResult =
            	this.shopService.getSalesOrder(
            		getSalesOrderParams
            	);
            SalesOrderT salesOrderT = getSalesOrderResult.getSalesOrder();
            if(salesOrderT == null) {
            	CreateSalesOrderParams createSalesOrderParams = Datatypes.create(
                    CreateSalesOrderParams.class,
                    Datatypes.member(
                        CreateSalesOrderParams.Member.salesOrder,
                        Datatypes.create(
                            SalesOrderT.class,
                            Datatypes.member(
                                SalesOrderT.Member.contract,
                                Datatypes.create(
                                    ContractT.class,
                                    Datatypes.member(
                                        ContractT.Member.customerNumber,
                                        customerNumber
                                    ),
                                    Datatypes.member(
                                    	ContractT.Member.contractNumber,
                                    	salesOrderNumber
                                    ),
                                    Datatypes.member(
                                        ContractT.Member.activeOn,
                                        new Date()
                                    ),
                                    Datatypes.member(
                                        ContractT.Member.position,
                                        Collections.emptyList()
                                    ),     
                                    Datatypes.member(
                                    	ContractT.Member.contractCurrency,
                                    	contractCurrency
                                    ),
                                    Datatypes.member(
                                        ContractT.Member.isGift,
                                        gift == 1
                                    ),     
                                    Datatypes.member(
                                    	ContractT.Member.giftMessage,
                                    	gift_message
                                    ),
                                    Datatypes.member(
                                        ContractT.Member.postalAddressDelivery,
                                        this.newPostalAddressT(id_address_delivery)
                                    ),
                                    Datatypes.member(
                                        ContractT.Member.postalAddressInvoice,
                                        this.newPostalAddressT(id_address_invoice)
                                    )
                                )
                            )
                        )
                    )
                );
                CreateSalesOrderResult createSalesOrderResult =
                	this.shopService.createSalesOrder(
                		createSalesOrderParams
                	);
                salesOrderT = createSalesOrderResult.getSalesOrder();
            }
            // Update positions
            ps = this.getPreparedStatement(
            	this.conn,
            	"SELECT * FROM " + this.tablePrefix + "order_detail WHERE id_order = ?"
            );
            ps.setInt(1, id_order);
            ResultSet orderDetails = ps.executeQuery();
            List<String> orderedProductIds = new ArrayList<String>();
            while(orderDetails.next()) {
            	int id_order_detail = orderDetails.getInt("id_order_detail");
            	int product_id = orderDetails.getInt("product_id");
            	int product_attribute_id = orderDetails.getInt("product_attribute_id");
            	String product_name = orderDetails.getString("product_name");
            	int product_quantity = orderDetails.getInt("product_quantity");
            	int product_quantity_in_stock = orderDetails.getInt("product_quantity_in_stock");
            	int product_quantity_refunded = orderDetails.getInt("product_quantity_refunded");
            	int product_quantity_return = orderDetails.getInt("product_quantity_return");
            	int product_quantity_reinjected = orderDetails.getInt("product_quantity_reinjected");
            	BigDecimal product_price = orderDetails.getBigDecimal("product_price");
            	BigDecimal reduction_percent = orderDetails.getBigDecimal("reduction_percent");
            	BigDecimal reduction_amount = orderDetails.getBigDecimal("reduction_amount");
            	BigDecimal group_reduction = orderDetails.getBigDecimal("group_reduction");
            	BigDecimal product_quantity_discount = orderDetails.getBigDecimal("product_quantity_discount");
            	String product_ean13 =  orderDetails.getString("product_ean13");
            	String product_upc = orderDetails.getString("product_upc");
            	String product_reference = orderDetails.getString("product_reference");
            	String product_supplier_reference = orderDetails.getString("product_supplier_reference");
            	BigDecimal product_weight = orderDetails.getBigDecimal("product_weight");
            	String tax_name = orderDetails.getString("tax_name");
            	BigDecimal tax_rate = orderDetails.getBigDecimal("tax_rate");
            	BigDecimal ecotax = orderDetails.getBigDecimal("ecotax");
            	BigDecimal ecotax_tax_rate = orderDetails.getBigDecimal("ecotax_tax_rate");
            	int discount_quantity_applied = orderDetails.getInt("discount_quantity_applied");
            	String download_hash = orderDetails.getString("download_hash");
            	int download_nb = orderDetails.getInt("download_nb");
            	Timestamp download_deadline = null;
            	try {
            		download_deadline = orderDetails.getTimestamp("download_deadline");
            	} catch(Exception e) {}            	
            	this.createOrUpdateSalesTax(
            		tax_name,
            		tax_rate
            	);            	
            	String productNumber = this.getProductNumber(product_id, product_attribute_id);
            	ContractPositionT positionT = null;
            	for(ContractPositionT posT: salesOrderT.getContract().getPosition()) {
            		if(posT.getProductNumber().equals(productNumber)) {
            			positionT = posT;
            			break;
            		}
            	}
            	if(positionT == null) {
                    AddSalesOrderPositionParams addSalesOrderPositionParams = Datatypes.create(
                        AddSalesOrderPositionParams.class,
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.salesOrderNumber,
                            salesOrderNumber
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.quantity,
                            Integer.toString(product_quantity)
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.productNumber,
                            productNumber
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.pricingDate,
                            new Date()
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.priceUom,
                            UOM_NAME_PIECE
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.salesTaxType,
                            tax_name == null || tax_name.isEmpty() ? 
                            	SALES_TAX_0 :
                            		tax_name
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.pricePerUnit,
                            bigDecimalToString(product_price)
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.discountIsPercentage,
                            Boolean.TRUE
                        ),
                        Datatypes.member(
                            AddSalesOrderPositionParams.Member.discount,
                            bigDecimalToString(reduction_percent)
                        )
                    );
                    this.shopService.addSalesOrderPosition(
                    	addSalesOrderPositionParams
                    );
            	} else if(Double.valueOf(positionT.getQuantity()) != product_quantity) {
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
                            Integer.toString(product_quantity)
                        )
                    );
                    this.shopService.setSalesOrderPositionQuantity(
                    	setSalesOrderPositionQuantityParams
                    );
            	}
            	orderedProductIds.add(productNumber);
            }
            this.closeResultSet(orderDetails);
            this.closePreparedStatement(ps);
            
            // Shipping
            String productNumberShipping = this.shopName + "-" + PRODUCT_NAME_SHIPPING;
        	ContractPositionT shippingPositionT = null;
        	for(ContractPositionT posT: salesOrderT.getContract().getPosition()) {
        		if(posT.getProductNumber().equals(productNumberShipping)) {
        			shippingPositionT = posT;
        			break;
        		}
        	}
        	if(
        		shippingPositionT == null && 
        		total_shipping.compareTo(BigDecimal.ZERO) > 0
        	) {
        		try {
        			this.createOrUpdateProduct(
        				this.shopName + "-" + PRODUCT_NAME_SHIPPING, 
        				PRODUCT_NAME_SHIPPING, 
        				Collections.<ProductDescriptionT>emptyList(), 
        				"", 
        				BigDecimal.ZERO, 
        				Collections.<String>emptyList(), 
        				BigDecimal.ZERO
        			);
        		} catch(Exception e) {}            
        		this.createOrUpdateSalesTax(
        			SALES_TAX_0,
        			BigDecimal.ZERO
        		);
                AddSalesOrderPositionParams addSalesOrderPositionParams = Datatypes.create(
                    AddSalesOrderPositionParams.class,
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.salesOrderNumber,
                        salesOrderNumber
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.quantity,
                        "1.0"
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.productNumber,
                        productNumberShipping
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.pricingDate,
                        new Date()
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.priceUom,
                        UOM_NAME_PIECE
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.salesTaxType,
                        SALES_TAX_0
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.pricePerUnit,
                        bigDecimalToString(total_shipping)
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.discountIsPercentage,
                        Boolean.TRUE
                    ),
                    Datatypes.member(
                        AddSalesOrderPositionParams.Member.discount,
                        "0.0"
                    )
                );
                this.shopService.addSalesOrderPosition(
                	addSalesOrderPositionParams
                );
        	}
        	orderedProductIds.add(productNumberShipping);
            // Set quantity to 0 for removed products
        	for(ContractPositionT positionT: salesOrderT.getContract().getPosition()) {
        		String productNumber = positionT.getProductNumber();
        		if(!orderedProductIds.contains(productNumber)) {
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
                            "0.0"
                        )
                    );
                    this.shopService.setSalesOrderPositionQuantity(
                    	setSalesOrderPositionQuantityParams
                    );
        		}
        	}        	
        	// Create invoice (if not already created)
        	if(invoice_number > 0) {
        		SalesOrder salesOrder = this.shopService.findSalesOrder(salesOrderNumber);
        		String invoiceNumber = null;
        		if(salesOrder.getInvoice().isEmpty()) {
        	        CreateInvoiceFromSalesOrderParams createInvoiceFromSalesOrderParams = Datatypes.create(
        	            CreateInvoiceFromSalesOrderParams.class,
        	            Datatypes.member(
        	                CreateInvoiceFromSalesOrderParams.Member.salesOrderNumber,
        	                salesOrderNumber
        	            )
        	        );
        	        CreateInvoiceFromSalesOrderResult createInvoiceFromSalesOrderResult =
        	        	this.shopService.createInvoiceFromSalesOrder(
        	        		createInvoiceFromSalesOrderParams
        	        	);
        	        invoiceNumber = createInvoiceFromSalesOrderResult.getInvoice().getContract().getContractNumber();
        		} else {
        			invoiceNumber = salesOrder.getInvoice().get(0).getContractNumber();
        		}        		
            	if(invoiceNumber != null && delivery_number > 0) {            		
                    GetInvoiceParams getInvoiceParams = Datatypes.create(
                        GetInvoiceParams.class,
                        Datatypes.member(
                            GetInvoiceParams.Member.invoiceNumber,
                            invoiceNumber
                        )
                    );
            		GetInvoiceResult getInvoiceResult =
            			this.shopService.getInvoice(
            				getInvoiceParams
            			);
            		if(getInvoiceResult.getInvoice() != null) {            			
                		Invoice invoice = this.shopService.findInvoice(invoiceNumber);
                		if(invoice != null) {                			                	
                    		// Set carrier
                			PreparedStatement psCarrier = this.getPreparedStatement(
                				this.conn,
                				"SELECT * FROM " + this.tablePrefix + "carrier WHERE id_carrier = ?"
                			);
                			psCarrier.setInt(1, id_carrier);
                			ResultSet carriers = psCarrier.executeQuery();
                			String carrierName = null;
                			if(carriers.next()) {
                				carrierName = carriers.getString("name");
                			}
                			this.closeResultSet(carriers);
                			this.closePreparedStatement(psCarrier);
                			if(carrierName != null) {
                				AccountQuery accountQuery = (AccountQuery)this.pm.newQuery(Account.class);
                				accountQuery.thereExistsFullName().equalTo(carrierName);
                				List<Account> accounts = this.shopService.getAccountSegment().getAccount(accountQuery);
                				Account carrier = null;
                				if(accounts.isEmpty()) {
                					carrier = this.pm.newInstance(LegalEntity.class);
                					((LegalEntity)carrier).setName(carrierName);
                					this.pm.currentTransaction().begin();
                					this.shopService.getAccountSegment().addAccount(
                						this.shopService.uuidAsString(),
                						carrier
                					);
                					this.pm.currentTransaction().commit();
                				} else {
                					carrier = accounts.iterator().next();
                				}
                    			this.pm.currentTransaction().begin();
                    			invoice.setCarrier(carrier);
                    			this.pm.currentTransaction().commit();
                			}                			
                			// Shipping number
                			this.pm.currentTransaction().begin();
                			invoice.setShippingTrackingNumber(shipping_number);
                			this.pm.currentTransaction().commit();
                		}
                    	// Create delivery info at position-level                		
            			for(ContractPositionT positionT: getInvoiceResult.getInvoice().getContract().getPosition()) {
            				if(positionT.getDeliveryInformation().isEmpty()) {
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
			                                    "Delivered at " + new Date(delivery_date.getTime())
			                                ),
			                                Datatypes.member(
			                                	DeliveryInformationT.Member.actualDeliveryOn,
			                                    new Date(delivery_date.getTime())
			                                ),
			                                Datatypes.member(
			                                	DeliveryInformationT.Member.quantityShipped,
			                                    positionT.getQuantity()
			                                )
			                            )
			                        )
			                    );
			                    this.shopService.addDeliveryInformation(
			                    	addDeliveryInformationParams
			                    );
            				}
            			}
	            	}
            	}
        	}
		} catch(SQLException e) {
			throw new ServiceException(e);
		}
		finally {
			this.closePreparedStatement(ps);
		}
	}
	
	//-----------------------------------------------------------------------
	protected PostalAddressT newPostalAddressT(
		int id_address
	) throws ServiceException {
		PreparedStatement ps = null;
		try {
			ps = this.getPreparedStatement(
				this.conn, 
				"SELECT * FROM " + this.tablePrefix + "address WHERE id_address = ?"
			);
			ps.setInt(1, id_address);
			ResultSet addresses = ps.executeQuery();
			int a_id_address = -1;
			int a_id_country = -1;
			int a_aid_state = -1;
			String a_company = null;
			String a_lastname = null;
			String a_firstname = null;
			String a_address1 = null;
			String a_address2 = null;
			String a_postcode = null;
			String a_city = null;
			if(addresses.next()) {
				a_id_address = addresses.getInt("id_address");
				a_id_country = addresses.getInt("id_country");
				a_aid_state = addresses.getInt("id_state");
				a_company = addresses.getString("company");
				a_lastname = addresses.getString("lastname");
				a_firstname = addresses.getString("firstname");
				a_address1 = addresses.getString("address1");
				a_address2 = addresses.getString("address2");
				a_postcode = addresses.getString("postcode");
				a_city = addresses.getString("city");
				this.closeResultSet(addresses);
				this.closePreparedStatement(ps);

				int postalCountryCode = 0;
				if(a_id_country >= 0) {
					ps = this.getPreparedStatement(
						this.conn, 
						"SELECT * FROM " + this.tablePrefix + "country WHERE id_country = ?"
					);
					ps.setInt(1, a_id_country);
					ResultSet countries = ps.executeQuery();
					if(countries.next()) {
						String isoCode = countries.getString("iso_code");
						postalCountryCode = Addresses.getInstance().mapToPostalCountryCode(
							"[" + isoCode + "]", 
							null // codeSegment
						);
					}
					this.closeResultSet(countries);
					this.closePreparedStatement(ps);
				}				
				return Datatypes.create(
                    PostalAddressT.class,
                    Datatypes.member(
                        PostalAddressT.Member.postalAddressLine0,
                        a_company == null || a_company.isEmpty() ? a_firstname + " " + a_lastname : a_company
                    ),
                    Datatypes.member(
                        PostalAddressT.Member.postalAddressLine1,
                        a_company == null || a_company.isEmpty() ? "" : a_firstname + " " + a_lastname
                    ),
                    Datatypes.member(
                        PostalAddressT.Member.postalStreet0,
                        a_address1
                    ),
                    Datatypes.member(
                        PostalAddressT.Member.postalStreet1,
                        a_address2
                    ),
                    Datatypes.member(
                        PostalAddressT.Member.postalCity,
                        a_city
                    ),
                    Datatypes.member(
                        PostalAddressT.Member.postalCode,
                        a_postcode
                    ),
                    Datatypes.member(
                        PostalAddressT.Member.postalCountry,
                        postalCountryCode
                    )
                );
			}
			this.closeResultSet(addresses);
			this.closePreparedStatement(ps);
		}
		catch(Exception e) {
			throw new ServiceException(e);
		}
		finally {
			this.closePreparedStatement(ps);
		}
		return null;
	}
	
	//-----------------------------------------------------------------------
	public int getLanguageCode(
		int id_lang
	) throws ServiceException {
		PreparedStatement ps = null;
		short languageCode = 0;
		try {
			ps = this.getPreparedStatement(
				this.conn,
				"SELECT * FROM " + this.tablePrefix + "lang WHERE id_lang = ?"
			);
			ps.setInt(1, id_lang);
			ResultSet rs = ps.executeQuery();
			if(rs.next()) {
				String iso_code = rs.getString("iso_code");
				if("ar".equalsIgnoreCase(iso_code)) {
					return 18;
				} else if("zh".equalsIgnoreCase(iso_code)) {
					languageCode = 75;
				} else if("cz".equalsIgnoreCase(iso_code)) {
					languageCode = 92;
				} else if("nl".equalsIgnoreCase(iso_code)) {
					languageCode = 102;
				} else if("en".equalsIgnoreCase(iso_code)) {
					languageCode = 110;
				} else if("fr".equalsIgnoreCase(iso_code)) {
					languageCode = 126;
				} else if("de".equalsIgnoreCase(iso_code)) {
					languageCode = 138;
				} else if("he".equalsIgnoreCase(iso_code)) {
					languageCode = 156;
				} else if("it".equalsIgnoreCase(iso_code)) {
					languageCode = 183;
				} else if("ja".equalsIgnoreCase(iso_code)) {
					languageCode = 184;
				} else if("fa".equalsIgnoreCase(iso_code)) {
					languageCode = 311;
				} else if("pl".equalsIgnoreCase(iso_code)) {
					languageCode = 314;
				} else if("pt".equalsIgnoreCase(iso_code)) {
					languageCode = 316;
				} else if("ro".equalsIgnoreCase(iso_code)) {
					languageCode = 325;
				} else if("ru".equalsIgnoreCase(iso_code)) {
					languageCode = 328;
				} else if("sk".equalsIgnoreCase(iso_code)) {
					languageCode = 352;
				} else if("sk".equalsIgnoreCase(iso_code)) {
					languageCode = 352;
				} else if("es".equalsIgnoreCase(iso_code)) {
					return 361;
				} else if("sv".equalsIgnoreCase(iso_code)) {
					languageCode = 368;
				} else if("tr".equalsIgnoreCase(iso_code)) {
					languageCode = 392;
				} else if("tr".equalsIgnoreCase(iso_code)) {
					languageCode = 392;
				}
			}
			this.closeResultSet(rs);
			this.closePreparedStatement(ps);
		}
		catch(Exception e) {
			throw new ServiceException(e);
		}
		finally {
			this.closePreparedStatement(ps);
		}
		return languageCode;
	}
	
	//-----------------------------------------------------------------------
	protected void createOrUpdateProduct(
		String productNumber,
		String productName,
		List<ProductDescriptionT> productDescriptionsT,
		String dimension,
		BigDecimal weight,
		List<String> alternateProductNumbers,
		BigDecimal price
	) throws ServiceException {
		SysLog.info("Update product", Arrays.asList(productNumber, productName));
		try {
			// Get product
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
	        GetProductsResult getProductsResult = 
	        	this.shopService.getProducts(
	        		getProductsParams
	        	);	            
	        // Create product
	        if(getProductsResult.getProduct().isEmpty()) {
	            ProductT productT = Datatypes.create(
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
	                    ProductT.Member.priceUom,
	                    new String[]{UOM_NAME_PIECE}
	                ),
	                Datatypes.member(
	                    ProductT.Member.isBundle,
	                    Boolean.FALSE
	                )
	            );            	
	            CreateProductsParams createProductsParams = Datatypes.create(
	                CreateProductsParams.class,
	                Datatypes.member(
	                    CreateProductsParams.Member.product,
	                    new ProductT[]{productT}
	                )
	            );
	        	this.shopService.createProducts(
	        		createProductsParams
	        	);
	        }	        
	        // Update product
	        ProductT productT = Datatypes.create(
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
	                productDescriptionsT
	            ),
	            Datatypes.member(
	                ProductT.Member.priceUom,
	                new String[]{UOM_NAME_PIECE}
	            ),
	            Datatypes.member(
	                ProductT.Member.isBundle,
	                Boolean.FALSE
	            )
	        );
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
	                Boolean.FALSE
	            ),
	            Datatypes.member(
	                UpdateProductParams.Member.updateUom,
	                Boolean.TRUE
	            ),
	            Datatypes.member(
	                UpdateProductParams.Member.updateConfiguration,
	                Boolean.FALSE
	            ),
	            Datatypes.member(
	                UpdateProductParams.Member.updateProductPhase,
	                Boolean.FALSE
	            ),
	            Datatypes.member(
	                UpdateProductParams.Member.updatePicture,
	                Boolean.FALSE
	            )           
	        );
	        UpdateProductResult updateProductResult = 
	        	this.shopService.updateProduct(
	        		updateProductParams
	        	);            
	        Product product = this.shopService.findProduct(productNumber);
	        if(product != null) {
	            // Product details
	        	this.pm.currentTransaction().begin();
	        	product.setProductDimension(dimension);
	        	product.setGrossWeightKilogram(weight);
	        	product.getAlternateProductNumber().clear();
	        	product.getAlternateProductNumber().addAll(alternateProductNumbers);
	        	this.pm.currentTransaction().commit();            	
	            // Create / update standard price for each currency
	        	PreparedStatement psCurrencies = this.getPreparedStatement(
	        		this.conn,
	        		"SELECT * FROM " + this.tablePrefix + "currency"
	        	);
	        	ResultSet currencies = psCurrencies.executeQuery();
	        	while(currencies.next()) {
	        		int c_id_currency = currencies.getInt("id_currency");
	        		String c_name = currencies.getString("name");
	        		String c_iso_code = currencies.getString("iso_code");
	        		String c_iso_code_num = currencies.getString("iso_code_num");
	        		String c_sign = currencies.getString("sign");
	        		int c_blank = currencies.getInt("blank");
	        		int c_format = currencies.getInt("format");
	        		int c_decimals = currencies.getInt("decimals");
	        		BigDecimal c_conversion_rate = currencies.getBigDecimal("conversion_rate");
	        		int c_deleted = currencies.getInt("deleted");
	        		int c_active = currencies.getInt("active");
	        		
	        		short priceCurrency = Short.valueOf(c_iso_code_num);
	        		String priceLevelName = this.shopName + " [" + c_iso_code + "]";
	        		PriceLevel priceLevel = null;
	        		PriceLevelQuery priceLevelQuery = (PriceLevelQuery)this.pm.newQuery(PriceLevel.class);
	        		priceLevelQuery.name().equalTo(priceLevelName);
	        		List<PriceLevel> priceLevels = this.shopService.getProductSegment().getPriceLevel(priceLevelQuery);
	        		if(priceLevels.isEmpty()) {
	        			priceLevel = this.pm.newInstance(PriceLevel.class);
	        			priceLevel.setName(priceLevelName);
	        			priceLevel.setPriceCurrency(priceCurrency);
	        			this.pm.currentTransaction().begin();
	        			this.shopService.getProductSegment().addPriceLevel(
	        				this.shopService.uuidAsString(),
	        				priceLevel
	        			);
	        			this.pm.currentTransaction().commit();
	        		} else {
	        			priceLevel = priceLevels.iterator().next();
	        		}
	        		ProductBasePrice productPrice = null;
	        		ProductBasePriceQuery priceQuery = (ProductBasePriceQuery)this.pm.newQuery(ProductBasePrice.class);
	        		priceQuery.thereExistsPriceLevel().equalTo(priceLevel);
	        		priceQuery.priceCurrency().equalTo(priceCurrency);
	        		List<ProductBasePrice> productPrices = product.getBasePrice(priceQuery);
	        		if(productPrices.isEmpty()) {
	        			productPrice = pm.newInstance(ProductBasePrice.class);
	        			productPrice.setPrice(BigDecimal.ZERO);
	        			productPrice.setPriceCurrency(priceCurrency);
	        			productPrice.getPriceLevel().add(priceLevel);
	        			productPrice.setUom(this.shopService.findUom(UOM_NAME_PIECE));
	        			this.pm.currentTransaction().begin();
	        			product.addBasePrice(
	        				this.shopService.uuidAsString(),
	        				productPrice
	        			);
	        			this.pm.currentTransaction().commit();
	        		} else {
	        			productPrice = productPrices.iterator().next();
	        		}
	        		this.pm.currentTransaction().begin();
	        		productPrice.setPrice(price.multiply(c_conversion_rate));
	        		this.pm.currentTransaction().commit();     		
	        	}
	        	this.closeResultSet(currencies);
	        	this.closePreparedStatement(psCurrencies);            	
	        }
		}
		catch(Exception e) {
			throw new ServiceException(e);
		}
	}		
	
	//-----------------------------------------------------------------------
	public void mapProduct(
		ResultSet rs
	) throws ServiceException {
		PreparedStatement ps = null;
		try {
			int id_product = rs.getInt("id_product");
			int id_supplier = rs.getInt("id_supplier");
			int id_manufacturer = rs.getInt("id_manufacturer");
			int id_tax_rules_group = rs.getInt("id_tax_rules_group");
			int id_category_default = rs.getInt("id_category_default");
			int on_sale = rs.getInt("on_sale");
			int online_only = rs.getInt("online_only");
			String ean13 = rs.getString("ean13");
			String upc = rs.getString("upc");
			BigDecimal ecotax = rs.getBigDecimal("ecotax");
			int quantity = rs.getInt("quantity");
			int minimal_quantity = rs.getInt("minimal_quantity");
			BigDecimal price = rs.getBigDecimal("price");
			BigDecimal wholesale_price = rs.getBigDecimal("wholesale_price");
			String unity = rs.getString("unity");
			BigDecimal additional_shipping_cost = rs.getBigDecimal("additional_shipping_cost");
			String reference = rs.getString("reference");
			String supplier_reference = rs.getString("supplier_reference");
			String location = rs.getString("location");
			BigDecimal width = rs.getBigDecimal("width");
			BigDecimal height = rs.getBigDecimal("height");
			BigDecimal depth = rs.getBigDecimal("depth");
			BigDecimal weight = rs.getBigDecimal("weight");
			int out_of_stock = rs.getInt("out_of_stock");
			int quantity_discount = rs.getInt("quantity_discount");
			int customizable = rs.getInt("customizable");
			int uploadable_files = rs.getInt("uploadable_files");
			int text_fields = rs.getInt("text_fields");
			int active = rs.getInt("active");
			int available_for_order = rs.getInt("available_for_order");
			String condition = rs.getString("condition");
			int show_price = rs.getInt("show_price");
			int indexed = rs.getInt("indexed");
			int cache_is_pack = rs.getInt("cache_is_pack");
			int cache_has_attachments = rs.getInt("cache_has_attachments");
			int cache_default_attribute = rs.getInt("cache_default_attribute");
			Timestamp date_add = rs.getTimestamp("date_add");
			Timestamp date_upd = rs.getTimestamp("date_upd");

			// Dimension
			String dimension = height + " x " + width + " x " + depth;
			
            // Product name and descriptions     
            String productName = null;
            List<ProductDescriptionT> productDescriptionsT = new ArrayList<ProductDescriptionT>();            
            PreparedStatement psProductLangs = this.getPreparedStatement(
            	this.conn,
            	"SELECT * FROM " + this.tablePrefix + "product_lang WHERE id_product = ?"
            );
            psProductLangs.setInt(1, id_product);
            ResultSet productLangs = psProductLangs.executeQuery();
            while(productLangs.next()) {
            	int d_id_product = productLangs.getInt("id_product");
            	int d_id_lang = productLangs.getInt("id_lang");
            	String d_description = productLangs.getString("description");
            	String d_description_short = productLangs.getString("description_short");
            	String d_link_rewrite = productLangs.getString("link_rewrite");
            	String d_meta_description = productLangs.getString("meta_description");
            	String d_meta_keywords = productLangs.getString("meta_keywords");
            	String d_meta_title = productLangs.getString("meta_title");
            	String d_name = productLangs.getString("name");
            	String d_available_now = productLangs.getString("available_now");
            	String d_available_later = productLangs.getString("available_later");
            	if(d_id_lang == 1) {
            		productName = productLangs.getString("name");	            			            		
            	}            
            	productDescriptionsT.add(
                    Datatypes.create(
                        ProductDescriptionT.class,
                        Datatypes.member(
                            ProductDescriptionT.Member.language,
                            this.getLanguageCode(d_id_lang)
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.description,
                            d_description_short
                        ),
                        Datatypes.member(
                            ProductDescriptionT.Member.detailedDescription,
                            d_description
                        )
                    )            		
            	);            	
            }
            this.closeResultSet(productLangs);
            this.closePreparedStatement(psProductLangs);            			
			// Product combinations
			ps = this.getPreparedStatement(
				this.conn,
				"SELECT * FROM " + this.tablePrefix + "product_attribute WHERE id_product = ?"
			);
			ps.setInt(1, id_product);
			ResultSet productAttributes = ps.executeQuery();			
			// No product combinations
			boolean hasAttributes = false;
			while(productAttributes.next()) {
				hasAttributes = true;
				int a_id_product_attribute = productAttributes.getInt("id_product_attribute");
				int a_id_product = productAttributes.getInt("id_product");
				String a_reference = productAttributes.getString("reference");
				String a_supplier_reference = productAttributes.getString("supplier_reference");
				String a_location = productAttributes.getString("location");
				String a_ean13 = productAttributes.getString("ean13");
				String a_upc = productAttributes.getString("upc");
				BigDecimal a_wholesale_price = productAttributes.getBigDecimal("wholesale_price");
				BigDecimal a_price = productAttributes.getBigDecimal("price");
				BigDecimal a_ecotax = productAttributes.getBigDecimal("ecotax");
				int a_quantity = productAttributes.getInt("quantity");
				int a_weight = productAttributes.getInt("weight");
				BigDecimal unit_price_impact = productAttributes.getBigDecimal("unit_price_impact"); 
				int a_default_on = productAttributes.getInt("default_on");			  
	            // Complete product name. Get all attributes and append to name
        		PreparedStatement psAttributes = this.getPreparedStatement(
        			this.conn,
        			"SELECT" +
					"  attribute.id_attribute," +
					"  attribute_group_lang.name AS group_name," +
					"  attribute_lang.name AS attribute_name " +
					"FROM" +
					"  " + this.tablePrefix + "product_attribute_combination product_attribute_combination " +
					"INNER JOIN" +
					"  " + this.tablePrefix + "attribute attribute " +
					"ON" +
					"  product_attribute_combination.id_attribute = attribute.id_attribute " +
					"INNER JOIN" +
					"  " + this.tablePrefix + "attribute_lang attribute_lang " +
					"ON" +
					"  attribute.id_attribute = attribute_lang.id_attribute AND" +
					"  attribute_lang.id_lang = 1 " +
					"INNER JOIN" +
					"  " + this.tablePrefix + "attribute_group_lang attribute_group_lang " +
					"ON" +
					"  attribute.id_attribute_group = attribute_group_lang.id_attribute_group AND" +
					"  attribute_group_lang.id_lang = 1 " +
					"WHERE" +
					"  product_attribute_combination.id_product_attribute = ? " +
					"ORDER BY " +
					"  group_name"
				);
        		psAttributes.setInt(1, a_id_product_attribute);
        		ResultSet attributes = psAttributes.executeQuery();
				String productNumber = this.getProductNumber(id_product, a_id_product_attribute);
        		String fullProductName = productName == null ? productNumber : productName;
        		String sep = " -";
        		while(attributes.next()) {
        			String groupName = attributes.getString("group_name");
        			String attributeName = attributes.getString("attribute_name");
        			fullProductName += sep + " ";
        			fullProductName += groupName + " : " + attributeName;
        			sep = ",";
        		}
        		this.closeResultSet(attributes);
        		this.closePreparedStatement(psAttributes);
        		List<String> alternateProductNumbers = new ArrayList<String>();
        		if(a_ean13 != null && !a_ean13.isEmpty()) {
        			alternateProductNumbers.add(a_ean13);
        		}
        		if(a_upc != null && !a_upc.isEmpty()) {
        			alternateProductNumbers.add(a_upc);
        		}
        		this.createOrUpdateProduct(
        			productNumber, 
        			fullProductName, 
        			productDescriptionsT, 
        			dimension, 
        			weight.add(new BigDecimal(a_weight)), 
        			alternateProductNumbers,
        			price.add(a_price)
        		);
            }
			if(!hasAttributes) {
				String productNumber = this.getProductNumber(id_product, 0);
        		List<String> alternateProductNumbers = new ArrayList<String>();
        		if(ean13 != null && !ean13.isEmpty()) {
        			alternateProductNumbers.add(ean13);
        		}
        		if(upc != null && !upc.isEmpty()) {
        			alternateProductNumbers.add(upc);
        		}
				this.createOrUpdateProduct(
					productNumber, 
					productName, 
					productDescriptionsT, 
					dimension, 
					weight,
        			alternateProductNumbers,
					price
				);
			}
		}
		catch(Exception e) {
			throw new ServiceException(e);
		}
		finally {
			this.closePreparedStatement(ps);
		}
	}
	
	//-----------------------------------------------------------------------
	protected String getCustomerNumber(
		int id_customer
	) {
		String userName = this.getUserName(id_customer);
		
		// Get credentials
		GetCredentialsParams getCredentialsParams = Datatypes.create(
			GetCredentialsParams.class,
            Datatypes.member(
            	GetCredentialsParams.Member.userName,
            	userName
            )
        );
		GetCredentialsResult getCredentialsResult = 
			this.shopService.getCredentials(
				getCredentialsParams
			);			
		String customerNumber = getCredentialsResult.getCustomerNumber() == null ?
			null :
				getCredentialsResult.getCustomerNumber();
		return customerNumber;		
	}
	
	//-----------------------------------------------------------------------
	public CustomerT mapCustomer(
		ResultSet rs
	) throws ServiceException {
		PreparedStatement ps = null;
		try {
			int id_customer = rs.getInt("id_customer");
			int id_gender = rs.getInt("id_gender");
			int id_default_group = rs.getInt("id_default_group");
			String firstname = rs.getString("firstname");
			String lastname = rs.getString("lastname");
			String email = rs.getString("email");
			String passwd = rs.getString("passwd");
			Timestamp last_passwd_gen = rs.getTimestamp("last_passwd_gen");
			java.sql.Date birthday = rs.getDate("birthday");
			int newsletter = rs.getInt("newsletter");
			String ip_registration_newsletter = rs.getString("ip_registration_newsletter");
			Timestamp newsletter_date_add = rs.getTimestamp("newsletter_date_add");
			int optin = rs.getInt("optin");
			String secure_key = rs.getString("secure_key");
			String note = rs.getString("note");
			int active = rs.getInt("active");
			int is_guest = rs.getInt("is_guest");
			int deleted = rs.getInt("deleted");
			Timestamp date_add = rs.getTimestamp("date_add");
			Timestamp date_upd = rs.getTimestamp("date_upd");	
			
			String userName = this.getUserName(id_customer);
			String customerNumber = this.getCustomerNumber(id_customer);
			
			if(customerNumber == null) {
				// Create customer
		        CreateCustomerAsContactParams createCustomerParams = Datatypes.create(
		            CreateCustomerAsContactParams.class,
		            Datatypes.member(
		            	CreateCustomerAsContactParams.Member.firstName,
		                firstname
		            ),
		            Datatypes.member(
		            	CreateCustomerAsContactParams.Member.lastName,
		                lastname
		            ),
		            Datatypes.member(
		            	CreateCustomerAsContactParams.Member.userName,
		                userName
		            ),
		            Datatypes.member(
		            	CreateCustomerAsContactParams.Member.emailAddressHome,
		                email
		            )
		        );		
				CreateCustomerAsContactResult createCustomerResult = 
					this.shopService.createCustomerAsContact(
						createCustomerParams
					);	
				if(createCustomerResult.getStatus().getReturnCode() == BasicException.Code.NONE) {
					customerNumber = createCustomerResult.getCustomer().getCustomerNumber();				
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
			                        CustomerContractT.Member.salesTaxType,
			                        "Sales Tax 0%"
			                    ),
			                    Datatypes.member(
			                        CustomerContractT.Member.contractCurrency,
			                        0
			                    ),
			                    Datatypes.member(
			                        CustomerContractT.Member.noBilling,
			                        Boolean.FALSE
			                    )
			                )
			            )
			        );
		        	this.shopService.createCustomerContract(
		        		createCustomerContractParams
		        	);
				} else {
					throw new ServiceException(
						BasicException.Code.DEFAULT_DOMAIN,
						BasicException.Code.ASSERTION_FAILURE,						
						"Unable to create customer",
						new BasicException.Parameter("status.returnCode", createCustomerResult.getStatus().getReturnCode()),
						new BasicException.Parameter("status.returnParams", createCustomerResult.getStatus().getReturnParams()),
						new BasicException.Parameter("customer.customerNumber", customerNumber),
						new BasicException.Parameter("customer.firstname", firstname),
						new BasicException.Parameter("customer.lastname", lastname),
						new BasicException.Parameter("customer.userName", userName),
						new BasicException.Parameter("customer.email", email)						
					);						
				}
			}
			// Set credential
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
	                        passwd
	                    )
	                )
	            )
	        );
        	this.shopService.setCredentials(
        		setCredentialsParams
        	);
			// Map addresses
			ps = this.getPreparedStatement(
				this.conn, 
				"SELECT * FROM " + this.tablePrefix + "address WHERE id_customer = ? AND deleted = 0 AND active = 1"
			); 
			ps.setInt(1, id_customer);
			ResultSet addresses = ps.executeQuery();
			int a_id_address = -1;
			int a_id_country = -1;
			int a_aid_state = -1;
			String a_alias = null;
			String a_company = null;
			String a_lastname = null;
			String a_firstname = null;
			String a_address1 = null;
			String a_address2 = null;
			String a_postcode = null;
			String a_city = null;
			String a_other = null;
			String a_phone = null;
			String a_phone_mobile = null;
			String a_vat_number = null; 
			if(addresses.next()) {
				a_id_address = addresses.getInt("id_address");
				a_id_country = addresses.getInt("id_country");
				a_aid_state = addresses.getInt("id_state");
				a_alias = addresses.getString("alias");
				a_company = addresses.getString("company");
				a_lastname = addresses.getString("lastname");
				a_firstname = addresses.getString("firstname");
				a_address1 = addresses.getString("address1");
				a_address2 = addresses.getString("address2");
				a_postcode = addresses.getString("postcode");
				a_city = addresses.getString("city");
				a_other = addresses.getString("other");
				a_phone = addresses.getString("phone");
				a_phone_mobile = addresses.getString("phone_mobile");
				a_vat_number = addresses.getString("vat_number"); 				
			}
			this.closeResultSet(addresses);
			this.closePreparedStatement(ps);
			
			int postalCountryCode = 0;
			if(a_id_country >= 0) {
				ps = this.getPreparedStatement(
					this.conn, 
					"SELECT * FROM " + this.tablePrefix + "country WHERE id_country = ?"
				);
				ps.setInt(1, a_id_country);
				ResultSet countries = ps.executeQuery();
				if(countries.next()) {
					String isoCode = countries.getString("iso_code");
					postalCountryCode = Addresses.getInstance().mapToPostalCountryCode(
						"[" + isoCode + "]", 
						null // codeSegment
					);
				}
				this.closeResultSet(countries);
				this.closePreparedStatement(ps);
			}			
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
	                Boolean.FALSE
	            ),
	            Datatypes.member(
	                UpdateCustomerParams.Member.updateGenericData,
	                Boolean.FALSE
	            ),            
	            Datatypes.member(
	                UpdateCustomerParams.Member.updateBookmarks,
	                Boolean.FALSE
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
	                    	CustomerT.Member.contact,
	                    	Datatypes.create(
	                    		ContactT.class,
			                    Datatypes.member(
			                    	ContactT.Member.salutationCode,
			                        id_gender // 1=Mr., 2=Mrs.
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.firstName,
			                        firstname
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.lastName,
			                        lastname
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.birthDate,
			                        birthday == null ? null : new Date(birthday.getTime())
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.birthDateIsValidated,
			                        1
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.gender,
			                        id_gender // 1=male, 2=female
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.doNotEmail,
			                    	newsletter == 0
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.familyStatus,
			                    	0
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.postalAddressHome,
			                    	a_id_address == -1 ? null :
				                        Datatypes.create(
				                            PostalAddressT.class,
				                            Datatypes.member(
				                                PostalAddressT.Member.postalAddressLine0,
				                                a_company == null || a_company.isEmpty() ? a_firstname + " " + a_lastname : a_company
				                            ),
				                            Datatypes.member(
				                                PostalAddressT.Member.postalAddressLine1,
				                                a_company == null || a_company.isEmpty() ? "" : a_firstname + " " + a_lastname
				                            ),
				                            Datatypes.member(
				                                PostalAddressT.Member.postalStreet0,
				                                a_address1
				                            ),
				                            Datatypes.member(
				                                PostalAddressT.Member.postalStreet1,
				                                a_address2
				                            ),
				                            Datatypes.member(
				                                PostalAddressT.Member.postalCity,
				                                a_city
				                            ),
				                            Datatypes.member(
				                                PostalAddressT.Member.postalCode,
				                                a_postcode
				                            ),
				                            Datatypes.member(
				                                PostalAddressT.Member.postalCountry,
				                                postalCountryCode
				                            )
				                        )
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.emailAddressHome,
			                        Datatypes.create(
			                            EmailAddressT.class,
			                            Datatypes.member(
			                                EmailAddressT.Member.emailAddress,
			                                email
			                            ),
			                            Datatypes.member(
			                                EmailAddressT.Member.emailValid,
			                                Boolean.TRUE
			                            )
			                        )
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.phoneNumberHome,
			                    	a_phone == null || a_phone.isEmpty() ? null :
				                        Datatypes.create(
				                            PhoneNumberT.class,
				                            Datatypes.member(
				                                PhoneNumberT.Member.phoneNumber,
				                                a_phone
				                            ),
				                            Datatypes.member(
				                                PhoneNumberT.Member.phoneNumberVerified,
				                                Boolean.TRUE
				                            )
				                        )
			                    ),
			                    Datatypes.member(
			                    	ContactT.Member.phoneNumberMobile,
			                    	a_phone_mobile == null || a_phone_mobile.isEmpty() ? null :
				                        Datatypes.create(
				                            PhoneNumberT.class,
				                            Datatypes.member(
				                                PhoneNumberT.Member.phoneNumber,
				                                a_phone_mobile
				                            ),
				                            Datatypes.member(
				                                PhoneNumberT.Member.phoneNumberVerified,
				                                Boolean.TRUE
				                            )
				                        )
			                    )                   
			                )
			            )
	                )
	            )
	        );	
	        UpdateCustomerResult updateCustomerResult = 
	        	this.shopService.updateCustomer(
	        		updateCustomerParams
	        	);
			return updateCustomerResult.getCustomer();
		} catch(SQLException e) {
			throw new ServiceException(e);
		}
		finally {
			this.closePreparedStatement(ps);
		}
	}

	//-----------------------------------------------------------------------
    /*
	    1 - awaiting cheque payment
	    2 - payment accepted
	    3 - preparation in progress
	    4 - shipped
	    5 - delivered
	    6 - cancelled
	    7 - refund
	    8 - payment error
	    9 - on back order
	    10 - awaiting bank wire payment
	    11 - awaiting paypal payment
    */
	public void mapOrderHistory(
		ResultSet rs
	) throws ServiceException {
		PreparedStatement ps = null;
		try {
			int id_order_history = rs.getInt("id_order_history");
			int id_employee = rs.getInt("id_employee");
			int id_order = rs.getInt("id_order");
			int id_order_state = rs.getInt("id_order_state");
			Timestamp date_add = rs.getTimestamp("date_add");
			
			String salesOrderNumber = null;
			ps = this.getPreparedStatement(
				this.conn,
				"SELECT * FROM " + this.tablePrefix + "orders WHERE id_order = ?"
			);
			ps.setInt(1, id_order);
			ResultSet orders = ps.executeQuery();
			if(orders.next()) {
				int id_customer = orders.getInt("id_customer");
				String customerNumber = this.getCustomerNumber(id_customer);
				salesOrderNumber = this.getSalesOrderNumber(customerNumber, id_order);
			}
			this.closePreparedStatement(ps);
			this.closeResultSet(orders);			
			// Sales order status
			// - DRAFT
			// - OPEN
			// - HOLD_PENDING
			// - HOLD_IN_PROGRESS
			// - WON
			// - LOST
			// - CANCELLED
			SalesOrderState salesOrderState = null;
			switch(id_order_state) {
				case 1:
				case 10:
				case 11:
					salesOrderState = SalesOrderState.OPEN;
					break;
				case 2:
				case 5:
					salesOrderState = SalesOrderState.WON;
					break;
				case 6:
					salesOrderState = SalesOrderState.CANCELLED;
					break;					
			}
			if(salesOrderState != null && salesOrderNumber != null) {
		        SetSalesOrderStatusParams setSalesOrderStatusParams = Datatypes.create(
		            SetSalesOrderStatusParams.class,
		            Datatypes.member(
		                SetSalesOrderStatusParams.Member.salesOrderNumber,
		                salesOrderNumber
		            ),
		            Datatypes.member(
		                SetSalesOrderStatusParams.Member.salesOrderStatus,
		                Datatypes.create(
		                    ContractStatusT.class,
		                    Datatypes.member(
		                        ContractStatusT.Member.status,
		                        salesOrderState.getValue()
		                    ),
		                    Datatypes.member(
		                         ContractStatusT.Member.description,
		                        "Set to " + salesOrderState + " by " + id_employee + "@" + this.shopName
		                    )
		                )
		            )
		        );
				this.shopService.setSalesOrderStatus(
					setSalesOrderStatusParams
				);
			}			
			// Invoice status
		    // - DRAFT
		    // - OPEN
		    // - HOLD
		    // - PAID 
		    // - WRITTEN_OFF
			InvoiceState invoiceState = null;
			switch(id_order_state) {
				case 1:
				case 10:
				case 11:
					invoiceState = InvoiceState.DRAFT;
					break;
				case 2:
				case 5:
					invoiceState = InvoiceState.PAID;
					break;
				case 3:
					invoiceState = InvoiceState.OPEN;
					break;
				case 6:
				case 8:
				case 9:
					invoiceState = InvoiceState.WRITTEN_OFF;
					break;
			}
			if(invoiceState != null && salesOrderNumber != null) {
				SalesOrder salesOrder = this.shopService.findSalesOrder(salesOrderNumber);
				if(salesOrder != null) {
					List<Invoice> invoices = salesOrder.getInvoice();
					for(Invoice invoice: invoices) {
				        SetInvoiceStatusParams setInvoiceStatusParams = Datatypes.create(
				            SetInvoiceStatusParams.class,
				            Datatypes.member(
				                SetInvoiceStatusParams.Member.invoiceNumber,
				                invoice.getContractNumber()
				            ),
				            Datatypes.member(
				                SetInvoiceStatusParams.Member.invoiceStatus,
				                Datatypes.create(
				                    ContractStatusT.class,
				                    Datatypes.member(
				                        ContractStatusT.Member.status,
				                        invoiceState.getValue()
				                    ),
				                    Datatypes.member(
				                         ContractStatusT.Member.description,
				                        "Set to " + invoiceState + " by " + id_employee + "@" + this.shopName
				                    )
				                )
				            )
				        );
						this.shopService.setInvoiceStatus(
							setInvoiceStatusParams
						);
					}
				}
			}
		}
		catch(Exception e) {
			throw new ServiceException(e);
		}
		finally {
			this.closePreparedStatement(ps);
		}
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String PRODUCT_NAME_SHIPPING = "Shipping";
	public static final String UOM_NAME_PIECE = "Piece";
	public static final String SALES_TAX_0 = "VAT 0%";
	
	private final static NumberFormat PRODUCT_NUMBER_FORMAT = new DecimalFormat("000000000");
	private final static NumberFormat PRODUCT_ATTRIBUTE_FORMAT = new DecimalFormat("00000");
	private final static NumberFormat USER_NUMBER_FORMAT = new DecimalFormat("000000000");
	private final static NumberFormat ORDER_NUMBER_FORMAT = new DecimalFormat("000000000");
	
	private final PersistenceManager pm;
	private final String shopName;
	private final Connection conn;
	private final String tablePrefix;
	private final ShopServiceImpl shopService;
	
}
