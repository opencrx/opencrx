/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Forecasts
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.Depots.BookingStatus;
import org.opencrx.kernel.backend.Depots.BookingType;
import org.opencrx.kernel.contract1.cci2.InvoicePositionQuery;
import org.opencrx.kernel.contract1.cci2.OpportunityPositionQuery;
import org.opencrx.kernel.contract1.cci2.QuotePositionQuery;
import org.opencrx.kernel.contract1.cci2.SalesOrderPositionQuery;
import org.opencrx.kernel.contract1.jmi1.AbstractContract;
import org.opencrx.kernel.contract1.jmi1.InvoicePosition;
import org.opencrx.kernel.contract1.jmi1.OpportunityPosition;
import org.opencrx.kernel.contract1.jmi1.QuotePosition;
import org.opencrx.kernel.contract1.jmi1.SalesContract;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.opencrx.kernel.contract1.jmi1.SalesOrderPosition;
import org.opencrx.kernel.depot1.cci2.SimpleBookingQuery;
import org.opencrx.kernel.depot1.jmi1.DepotPosition;
import org.opencrx.kernel.depot1.jmi1.SimpleBooking;
import org.opencrx.kernel.forecast1.jmi1.AbstractBudget;
import org.opencrx.kernel.forecast1.jmi1.QuantityBasedSalesVolumeBudgetPosition;
import org.opencrx.kernel.forecast1.jmi1.SalesVolumeBudget;
import org.opencrx.kernel.forecast1.jmi1.SalesVolumeBudgetContributionSource;
import org.opencrx.kernel.forecast1.jmi1.SalesVolumeBudgetPosition;
import org.opencrx.kernel.forecast1.jmi1.ValueBasedSalesVolumeBudgetPosition;
import org.opencrx.kernel.product1.cci2.ProductQuery;
import org.opencrx.kernel.product1.jmi1.Product;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;

public class Forecasts extends AbstractImpl {

    //-------------------------------------------------------------------------
	public static void register(
	) {
		registerImpl(new Forecasts());
	}
	
    //-------------------------------------------------------------------------
	public static Forecasts getInstance(
	) throws ServiceException {
		return getInstance(Forecasts.class);
	}

	//-------------------------------------------------------------------------
	protected Forecasts(
	) {
		
	}
	    
    //-----------------------------------------------------------------------
    /**
     * @return Returns the forecast segment.
     */
    public org.opencrx.kernel.forecast1.jmi1.Segment getForecastSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.forecast1.jmi1.Segment) pm.getObjectById(
            new Path("xri://@openmdx*org.opencrx.kernel.forecast1").getDescendant("provider", providerName, "segment", segmentName)
        );
    }

    //-----------------------------------------------------------------------
	/**
	 * Invoked by jdoPreStore() can be overriden by custom-specific extension.
	 */
	public void updateBudget(
		AbstractBudget budget
	) throws ServiceException  {		
	}

    //-----------------------------------------------------------------------
	public void recalcBudget(
		SalesVolumeBudget budget
	) throws ServiceException  {
		PersistenceManager pm = JDOHelper.getPersistenceManager(budget);
		if(budget.getDepot() == null) return;
		String providerName = budget.refGetPath().get(2);
		String segmentName = budget.refGetPath().get(4);
		org.opencrx.kernel.contract1.jmi1.Segment contractSegment = 
			(org.opencrx.kernel.contract1.jmi1.Segment)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName)
			);
		// Get contribution sources
		Date lastCalculatedAt = budget.getLastCalculatedAt();
		List<SalesVolumeBudgetContributionSource> contributionSources = new ArrayList<SalesVolumeBudgetContributionSource>();
		if(budget.getContractType() == Contracts.CONTRACT_TYPE_OPPORTUNITY) {
			OpportunityPositionQuery query = (OpportunityPositionQuery)PersistenceHelper.newQuery(
        		pm.getExtent(OpportunityPosition.class),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName, "opportunity", ":*", "position", ":*")  
        	);
			query.thereExistsBudget().equalTo(budget);
			if(lastCalculatedAt != null) {
				query.modifiedAt().greaterThan(lastCalculatedAt);
			}
			List<OpportunityPosition> opportunityPositions = contractSegment.getExtent(query);
			contributionSources.addAll(opportunityPositions);
		} else if(budget.getContractType() == Contracts.CONTRACT_TYPE_QUOTE) {
			QuotePositionQuery query = (QuotePositionQuery)PersistenceHelper.newQuery(
        		pm.getExtent(QuotePosition.class),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName, "quote", ":*", "position", ":*")  
        	);
			query.thereExistsBudget().equalTo(budget);
			if(lastCalculatedAt != null) {
				query.modifiedAt().greaterThan(lastCalculatedAt);
			}
			List<QuotePosition> quotePositions = contractSegment.getExtent(query);
			contributionSources.addAll(quotePositions);			
		} else if(budget.getContractType() == Contracts.CONTRACT_TYPE_SALES_ORDER) {
			SalesOrderPositionQuery query = (SalesOrderPositionQuery)PersistenceHelper.newQuery(
        		pm.getExtent(SalesOrderPosition.class),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName, "salesOrder", ":*", "position", ":*")  
        	);
			query.thereExistsBudget().equalTo(budget);
			if(lastCalculatedAt != null) {
				query.modifiedAt().greaterThan(lastCalculatedAt);
			}
			List<SalesOrderPosition> salesOrderPositions = contractSegment.getExtent(query);
			contributionSources.addAll(salesOrderPositions);			
		} else if(budget.getContractType() == Contracts.CONTRACT_TYPE_INVOICE) {
			InvoicePositionQuery query = (InvoicePositionQuery)PersistenceHelper.newQuery(
        		pm.getExtent(InvoicePosition.class),
                new Path("xri://@openmdx*org.opencrx.kernel.contract1").getDescendant("provider", providerName, "segment", segmentName, "invoice", ":*", "position", ":*")  
        	);		
			query.thereExistsBudget().equalTo(budget);
			if(lastCalculatedAt != null) {
				query.modifiedAt().greaterThan(lastCalculatedAt);
			}
			List<InvoicePosition> invoicePositions = contractSegment.getExtent(query);
			contributionSources.addAll(invoicePositions);			
		}
		// Update budget positions
		org.opencrx.kernel.depot1.jmi1.Segment depotSegment = 
			(org.opencrx.kernel.depot1.jmi1.Segment)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.depot1").getDescendant("provider", providerName, "segment", segmentName)
			);
		for(SalesVolumeBudgetContributionSource contributionSource: contributionSources) {
			Date valueDate = null;
			Product product = null;
			String bookingText = null;
			String originId = null;
			List<String> originContextParams = null;
			if(contributionSource instanceof OpportunityPosition) {
				OpportunityPosition contractPosition = (OpportunityPosition)contributionSource;
				AbstractContract contract = (AbstractContract)pm.getObjectById(contractPosition.refGetPath().getParent().getParent());				
				valueDate = contractPosition.getModifiedAt();
				lastCalculatedAt = contractPosition.getModifiedAt();
				product = contractPosition.getProduct();
				originId = contract.getContractNumber() + "/" + contractPosition.getLineItemNumber();
				originContextParams = Arrays.asList(contract.getContractNumber(), Long.toString(contractPosition.getLineItemNumber()));
				bookingText = contract.getContractNumber() + " / " + contractPosition.getLineItemNumber() + " / " + contractPosition.getName();
			} else if(contributionSource instanceof QuotePosition) {
				QuotePosition contractPosition = (QuotePosition)contributionSource;
				AbstractContract contract = (AbstractContract)pm.getObjectById(contractPosition.refGetPath().getParent().getParent());				
				valueDate = contractPosition.getModifiedAt();
				lastCalculatedAt = contractPosition.getModifiedAt();
				product = contractPosition.getProduct();
				originId = contract.getContractNumber() + "/" + contractPosition.getLineItemNumber();
				originContextParams = Arrays.asList(contract.getContractNumber(), Long.toString(contractPosition.getLineItemNumber()));
				bookingText = contract.getContractNumber() + " / " + contractPosition.getLineItemNumber() + " / " + contractPosition.getName();
			} else if(contributionSource instanceof SalesOrderPosition) {
				SalesOrderPosition contractPosition = (SalesOrderPosition)contributionSource;
				AbstractContract contract = (AbstractContract)pm.getObjectById(contractPosition.refGetPath().getParent().getParent());				
				valueDate = contractPosition.getModifiedAt();
				lastCalculatedAt = contractPosition.getModifiedAt();
				product = contractPosition.getProduct();
				originId = contract.getContractNumber() + "/" + contractPosition.getLineItemNumber();
				originContextParams = Arrays.asList(contract.getContractNumber(), Long.toString(contractPosition.getLineItemNumber()));
				bookingText = contract.getContractNumber() + " / " + contractPosition.getLineItemNumber() + " / " + contractPosition.getName();
			} else if(contributionSource instanceof InvoicePosition) {
				InvoicePosition contractPosition = (InvoicePosition)contributionSource;
				AbstractContract contract = (AbstractContract)pm.getObjectById(contractPosition.refGetPath().getParent().getParent());				
				valueDate = contractPosition.getModifiedAt();
				lastCalculatedAt = contractPosition.getModifiedAt();
				product = contractPosition.getProduct();
				originId = contract.getContractNumber() + "/" + contractPosition.getLineItemNumber();
				originContextParams = Arrays.asList(contract.getContractNumber(), Long.toString(contractPosition.getLineItemNumber()));
				bookingText = contract.getContractNumber() + " / " + contractPosition.getLineItemNumber() + " / " + contractPosition.getName();
			}
			Collection<SalesVolumeBudgetPosition> budgetPositions = budget.getPosition();
			for(SalesVolumeBudgetPosition budgetPosition: budgetPositions) {
				boolean matches = false;
				BigDecimal quantity = null;
				if(budgetPosition instanceof QuantityBasedSalesVolumeBudgetPosition) {
					QuantityBasedSalesVolumeBudgetPosition quantityBasedBudgetPosition = (QuantityBasedSalesVolumeBudgetPosition)budgetPosition;
					if(contributionSource instanceof SalesContractPosition) {
						SalesContractPosition contractPosition = (SalesContractPosition)contributionSource;
						quantity = contractPosition.getQuantity();
						matches = quantityBasedBudgetPosition.getUom() == null || quantityBasedBudgetPosition.getUom().equals(contractPosition.getUom());
					}
				}
				else if(budgetPosition instanceof ValueBasedSalesVolumeBudgetPosition) {
					ValueBasedSalesVolumeBudgetPosition valueBasedBudgetPosition = (ValueBasedSalesVolumeBudgetPosition)budgetPosition;
					if(contributionSource instanceof SalesContractPosition) {
						SalesContractPosition contractPosition = (SalesContractPosition)contributionSource;
						SalesContract contract = (SalesContract)pm.getObjectById(contractPosition.refGetPath().getParent().getParent());				
						matches =  valueBasedBudgetPosition.getCurrency() == contract.getContractCurrency();
						switch(valueBasedBudgetPosition.getValueType()) {
							case VALUE_TYPE_NA:
							case VALUE_TYPE_BASE_AMOUNT:
								quantity = contractPosition.getBaseAmount();
								break;
							case VALUE_TYPE_AMOUNT:
								quantity = contractPosition.getAmount();
								break;
							case VALUE_TYPE_DISCOUNT_AMOUNT:
								quantity = contractPosition.getDiscountAmount();
								break;
							case VALUE_TYPE_PRICE_PER_UNIT:
								quantity = contractPosition.getPricePerUnit();
								break;
						}
					}
				}
				// Product must match product filter
				if(matches && product != null) {					
					ProductQuery productQuery = (ProductQuery)pm.newQuery(Product.class);
					productQuery.productNumber().equalTo(product.getProductNumber());
					matches = !budgetPosition.getFilteredProduct(productQuery).isEmpty();
				}
				// Create contribution booking
				if(matches && quantity != null) {
					DepotPosition depotPosition = budgetPosition.getDepotPosition();
					if(budgetPosition.getDepotPosition() == null) {
						depotPosition = Depots.getInstance().openDepotPosition(
							budget.getDepot(), 
							budgetPosition.getName(), 
							null, // description 
							budget.getStartingFrom(), 
							null, // depotPositionQualifier 
							null, // product 
							false // isLocked
						);
						budgetPosition.setDepotPosition(depotPosition);
					}
					SimpleBooking booking = null;
					SimpleBookingQuery bookingQuery = (SimpleBookingQuery)pm.newQuery(SimpleBooking.class);
					bookingQuery.thereExistsOriginContext().equalTo(contributionSource);
					List<SimpleBooking> bookings = depotPosition.getSimpleBooking(bookingQuery);
					if(bookings.isEmpty()) {
						booking = pm.newInstance(SimpleBooking.class);
						booking.setName(bookingText);
						if(originId != null) {
							booking.setOriginId(originId);
						}
						if(originContextParams != null) {
							booking.getOriginContextParams().addAll(originContextParams);
						}
						booking.setPosition(depotPosition);
						if(contributionSource instanceof BasicObject) {
							booking.getOriginContext().add((BasicObject)contributionSource);
						}
						booking.setBookingDate(new Date());
						booking.setBookingType(BookingType.STANDARD.getValue());
						booking.setBookingStatus(BookingStatus.PENDING.getValue());
						depotSegment.addSimpleBooking(
							this.getUidAsString(),
							booking
						);
					} else {
						booking = bookings.iterator().next();
					}
					booking.setValueDate(valueDate);
					if(budgetPosition.getContributionFactor() != null) {
						quantity = quantity.multiply(budgetPosition.getContributionFactor());
						if(budgetPosition.getContributionRoundingFactor() != null) {
							long roundedQuantity = quantity.multiply(budgetPosition.getContributionRoundingFactor()).longValue();
							quantity = new BigDecimal(roundedQuantity).divide(budgetPosition.getContributionRoundingFactor());
						}
					}
					booking.setQuantity(quantity);					
				}
			}
		}
		budget.setLastCalculatedAt(
			lastCalculatedAt == null ? new Date() : lastCalculatedAt
		);
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
	}

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.backend.AbstractImpl#preStore(org.opencrx.kernel.generic.jmi1.CrxObject)
	 */
	@Override
	public void preStore(
		RefObject_1_0 object
	) throws ServiceException {
		super.preStore(object);
		if(object instanceof AbstractBudget) {
			this.updateBudget((AbstractBudget)object);
		}
	}

	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	// Value types
    public static final short VALUE_TYPE_NA = 0;
    public static final short VALUE_TYPE_BASE_AMOUNT = 1;
    public static final short VALUE_TYPE_AMOUNT = 2;
    public static final short VALUE_TYPE_DISCOUNT_AMOUNT = 3;
    public static final short VALUE_TYPE_PRICE_PER_UNIT = 4;
    
}

//--- End of File -----------------------------------------------------------
