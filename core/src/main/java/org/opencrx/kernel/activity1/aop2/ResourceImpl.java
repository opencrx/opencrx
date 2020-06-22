/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CompoundBookingImpl
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
package org.opencrx.kernel.activity1.aop2;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.opencrx.kernel.activity1.jmi1.ActivityWorkRecord;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.uom1.jmi1.Uom;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class ResourceImpl
	<S extends org.opencrx.kernel.activity1.jmi1.Resource,N extends org.opencrx.kernel.activity1.cci2.Resource,C extends Void>
	extends AbstractObject<S,N,C> {

    //-----------------------------------------------------------------------
    public ResourceImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult addWorkRecord(
        org.opencrx.kernel.activity1.jmi1.ResourceAddWorkRecordParams params
    ) {
        try {
        	Uom uomHour = null;
        	try {
        		uomHour = (Uom)this.sameManager().getObjectById(
        			new Path("xri:@openmdx:org.opencrx.kernel.uom1/provider/" + this.sameObject().refGetPath().get(2) + "/segment/Root/uom/hour")
        		);
        	}
        	catch(Exception e) {}        
        	List<PrincipalGroup> owningGroups = params.getOwningGroup();        	
            ActivityWorkRecord workRecord = Activities.getInstance().addWorkAndExpenseRecord(
                params.getActivity(),
                this.sameObject(), 
                params.getName(),
                params.getDescription(),
                params.getStartAt(),
                params.getEndAt(),
                new BigDecimal(params.getDurationHours() == null ? 0 : params.getDurationHours()).add(
                	new BigDecimal(params.getDurationMinutes() == null ? 0.0 : params.getDurationMinutes().doubleValue() / 60.0)
                ),
                uomHour,
                params.getRecordType(),
                (short)0,
                params.getDepotSelector(),
                params.getRate(),
                params.getRateCurrency(),
                params.isBillable(),
                Boolean.FALSE, // isReimbursable
                owningGroups
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.Member.workRecord, workRecord)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                   
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult addExpenseRecord(
        org.opencrx.kernel.activity1.jmi1.ResourceAddExpenseRecordParams params
    ) {
        try {
        	List<PrincipalGroup> owningGroups = params.getOwningGroup();
            ActivityWorkRecord workRecord = Activities.getInstance().addWorkAndExpenseRecord(
                params.getActivity(),
                this.sameObject(), 
                params.getName(),
                params.getDescription(),
                params.getStartAt(),
                params.getEndAt(),
                params.getQuantity(),
                params.getQuantityUom(),
                params.getRecordType(),
                params.getPaymentType(),
                params.getDepotSelector(),
                params.getRate(),
                params.getRateCurrency(),
                params.isBillable(),
                params.isReimbursable(),
                owningGroups
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.AddWorkAndExpenseRecordResult.Member.workRecord, workRecord)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                    
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult calcTotalQuantity(
        org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityParams params    	
    ) {
        try {
            List<BigDecimal> totalQuantities = new ArrayList<BigDecimal>();
            List<Uom> quantityUoms = new ArrayList<Uom>();  	        	
            Activities.getInstance().calcTotalQuantity(
                this.sameObject(),
                params.getRecordType(),
                params.getStartAt(),
                params.getEndAt(),
                totalQuantities,
                quantityUoms
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult.Member.quantityUom, quantityUoms),
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.CalcTotalQuantityResult.Member.totalQuantity, totalQuantities)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }

}
