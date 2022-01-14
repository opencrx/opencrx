/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SalesContractPositionImpl
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.kernel.contract1.aop2;

import java.math.BigDecimal;

import javax.jdo.JDOUserException;
import javax.jdo.listener.DeleteCallback;
import javax.jdo.listener.StoreCallback;

import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.contract1.jmi1.PreviewRepriceParams;
import org.opencrx.kernel.contract1.jmi1.PreviewRepriceResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;

/**
 * SalesContractPositionImpl
 *
 * @param <S>
 * @param <N>
 * @param <C>
 */
public class SalesContractPositionImpl
	<S extends org.opencrx.kernel.contract1.jmi1.SalesContractPosition,N extends org.opencrx.kernel.contract1.cci2.SalesContractPosition,C extends SalesContractPositionImpl.DerivedAttributes>
	extends AbstractObject<S,N,C>
	implements StoreCallback, DeleteCallback {

	public static class DerivedAttributes {
		
		public DerivedAttributes(
			BigDecimal[] quantities,
			BigDecimal[] amounts,
			String[] uomDescriptions,
			String[] priceUomDescriptions,
			String[] productDescriptions,
			String[] salesTaxTypeDescriptions
		) {
			this.quantityShipped = quantities[0];
			this.quantityBackOrdered = quantities[1];
			this.baseAmount = amounts[0];
			this.discountAmount = amounts[1];
			this.amount = amounts[2];
			this.taxAmount = amounts[3];
			this.uomDescription = uomDescriptions[0];
			this.uomDetailedDescription = uomDescriptions[1];
			this.priceUomDescription = priceUomDescriptions[0];
			this.priceUomDetailedDescription = priceUomDescriptions[1];
			this.productDescription = productDescriptions[0];
			this.productDetailedDescription = productDescriptions[1];
			this.salesTaxTypeDescription = salesTaxTypeDescriptions[0];
			this.salesTaxTypeDetailedDescription = salesTaxTypeDescriptions[1];
		}
		
		public BigDecimal quantityShipped;
		public BigDecimal quantityBackOrdered;
		public BigDecimal baseAmount;
		public BigDecimal discountAmount;
		public BigDecimal amount;
		public BigDecimal taxAmount;
		public String uomDescription;
		public String uomDetailedDescription;
		public String priceUomDescription;
		public String priceUomDetailedDescription;
		public String productDescription;
		public String productDetailedDescription;
		public String salesTaxTypeDescription;
		public String salesTaxTypeDetailedDescription;
		
	}
	
    public SalesContractPositionImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    public java.math.BigDecimal getQuantityShipped(
    ) {
    	return super.thisContext().quantityShipped;
    }
    
    public java.math.BigDecimal getQuantityBackOrdered(
    ) {
    	return super.thisContext().quantityBackOrdered;
    }
    
    public java.math.BigDecimal getBaseAmount(
    ) {
    	return super.thisContext().baseAmount;
    }
    
    public java.math.BigDecimal getDiscountAmount(
    ) {
    	return super.thisContext().discountAmount;
    }
    
    public java.math.BigDecimal getAmount(
    ) {
    	return super.thisContext().amount;
    }
    
    public java.math.BigDecimal getTaxAmount(
    ) {
    	return super.thisContext().taxAmount;
    }
    
    public String getUomDescription(
    ) {
    	return super.thisContext().uomDescription;
    }
    
    public String getUomDetailedDescription(
    ) {
    	return super.thisContext().uomDetailedDescription;
    }
    
    public String getPriceUomDescription(
    ) {
    	return super.thisContext().priceUomDescription;
    }
    
    public String getPriceUomDetailedDescription(
    ) {
    	return super.thisContext().priceUomDetailedDescription;
    }
    
    public String getProductDescription(
    ) {
    	return super.thisContext().productDescription;
    }
    
    public String getProductDetailedDescription(
    ) {
    	return super.thisContext().productDetailedDescription;
    }
    
    public String getSalesTaxTypeDescription(
    ) {
    	return super.thisContext().salesTaxTypeDescription;
    }
    
    public String getSalesTaxTypeDetailedDescription(
    ) {
    	return super.thisContext().salesTaxTypeDetailedDescription;
    }
    
    /**
     * Reprice sales contract position.
     * 
     * @return
     */
    public org.openmdx.base.jmi1.Void reprice(
    ) {
        try {
            Contracts.getInstance().repriceSalesContractPosition(
                this.sameObject()
            );
            return super.newVoid();
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }        
    }

    /**
     * Preview reprice.
     * 
     * @param in
     * @return
     */
    public PreviewRepriceResult previewReprice(
        PreviewRepriceParams in
    ) {
        try {
            return Contracts.getInstance().previewRepriceSalesContractPosition(
                this.sameObject(),
                in.getOverrideCalculationRule(),
                in.getOverridePriceLevel(),
                in.getOverrideProduct(),
                in.getOverridePriceUom(),
                in.isOverrideManualPricing(),
                in.getOverridePricePerUnit(),
                in.getOverrideQuantity(),
                in.getOverrideUomScaleFactor(),
                in.getOverrideSalesTaxRate(),
                in.getOverrideDiscount(),
                in.isOverrideDiscountIsPercentage(),
                in.getOverrideDiscountCalculationType(),
                in.getOverrideContractPositionState(),
                in.getOverridePricingDate()
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
    
    /**
     * Set price per unit.
     * 
     * @param pricePerUnit
     */
    public void setPricePerUnit(
        java.math.BigDecimal pricePerUnit
    ) {
        try {
            Contracts.getInstance().updatePricingState(
                this.sameObject(),
                Contracts.PRICING_STATE_DIRTY
            );
            this.nextObject().setPricePerUnit(pricePerUnit);
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            	
    }

    /**
     * Set pricing date.
     * 
     * @param pricingDate
     */
    public void setPricingDate(
        java.util.Date pricingDate
    ) {
        try {
            Contracts.getInstance().updatePricingState(
                this.sameObject(),
                Contracts.PRICING_STATE_DIRTY
            );
            this.nextObject().setPricingDate(pricingDate);
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }        	    	
    }
    
	/* (non-Javadoc)
	 * @see org.openmdx.base.aop2.AbstractObject#jdoPreStore()
	 */
	@Override
    public void jdoPreStore(
    ) {
    	try {    
    		Contracts.getInstance().preStore(
    			this.sameObject()
    		);
    		super.jdoPreStore();
    	} catch(ServiceException e) {
    		throw new JDOUserException(
    			"jdoPreStore failed",
    			e,
    			this.sameObject()
    		);
    	}
    }
    
    /* (non-Javadoc)
     * @see org.openmdx.base.aop2.AbstractObject#jdoPreDelete()
     */
    @Override
    public void jdoPreDelete(
    ) {
    	try {
    		Contracts.getInstance().preDelete(
    			this.sameObject(), 
    			true
    		);
    		super.jdoPreDelete();
    	} catch(ServiceException e) {
    		throw new JDOUserException(
    			"jdoPreDelete failed",
    			e,
    			this.sameObject()
    		);
    	}
    }

	/* (non-Javadoc)
	 * @see org.openmdx.base.aop2.AbstractObject#newContext()
	 */
	@SuppressWarnings("unchecked")
    @Override
    protected C newContext(
    ) {
		try {
			return (C)new DerivedAttributes(
				Contracts.getInstance().calculateQuantities(
					this.sameObject()
				),
				Contracts.getInstance().calculateAmounts(
					this.sameObject()
				),
				Contracts.getInstance().calculateUomDescriptions(
					this.sameObject()
				),
				Contracts.getInstance().calculatePriceUomDescriptions(
					this.sameObject()
				),
				Contracts.getInstance().calculateProductDescriptions(
					this.sameObject()
				),
				Contracts.getInstance().calculateSalesTaxTypeDescriptions(
					this.sameObject()
				)
		    );
		} catch(Exception e) {
			new ServiceException(e).log();
			throw new JDOUserException(
				"newContext failed",
				e,
				this.sameObject()
			);
		}
    }

}
