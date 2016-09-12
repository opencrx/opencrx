/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SalesContractImpl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2016, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.contract1.aop2;

import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.contract1.jmi1.SalesContractPosition;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class SalesContractImpl
	<S extends org.opencrx.kernel.contract1.jmi1.SalesContract,N extends org.opencrx.kernel.contract1.cci2.SalesContract,C extends Void>
	extends AbstractContractImpl<S,N,C> {

    //-----------------------------------------------------------------------
    public SalesContractImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void reprice(
    ) {
        try {
            Contracts.getInstance().repriceSalesContract(
                this.sameObject()
            );
            return super.newVoid();
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.contract1.jmi1.CreatePositionResult createPosition(
        org.opencrx.kernel.contract1.jmi1.CreatePositionParams params
    ) {
    	try {
    		SalesContractPosition position = Contracts.getInstance().createSalesContractPosition(
	            this.sameObject(),
	            params.isIgnoreProductConfiguration(),
	            params.getName(),
	            params.getQuantity(),
	            params.getPricingDate(),
	            params.getProduct(),
	            params.getUom(),
	            params.getPriceUom(),
	            params.getPricingRule()            
	        );
            return Structures.create(
            	org.opencrx.kernel.contract1.jmi1.CreatePositionResult.class, 
            	Datatypes.member(org.opencrx.kernel.contract1.jmi1.CreatePositionResult.Member.position, position)
            );    		
    	} catch(Exception e) {
    		throw new JmiServiceException(e);
    	}
    }

    //-----------------------------------------------------------------------
    public void setPricingDate(
        java.util.Date pricingDate
    ) {
        try {
            Contracts.getInstance().updatePricingState(
                this.sameObject(),
                Contracts.PRICING_STATE_DIRTY
            );
            this.nextObject().setPricingDate(pricingDate);
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }        	    	
    }
    
    //-----------------------------------------------------------------------
    public void setActiveOn(
        java.util.Date activeOn
    ) {
        try {
            Contracts.getInstance().updatePricingState(
                this.sameObject(),
                Contracts.PRICING_STATE_DIRTY
            );
            this.nextObject().setActiveOn(activeOn);
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }        	    	
    }
	
	/* (non-Javadoc)
	 * @see org.opencrx.kernel.contract1.aop2.AbstractContractImpl#jdoPreStore()
	 */
	@Override
    public void jdoPreStore(
    ) {
   		super.jdoPreStore();
    }

	/* (non-Javadoc)
	 * @see org.opencrx.kernel.contract1.aop2.AbstractContractImpl#jdoPreDelete()
	 */
	@Override
    public void jdoPreDelete(
    ) {
		super.jdoPreDelete();
    }
    
}
