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
package org.opencrx.kernel.depot1.aop2;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.jdo.JDOUserException;
import javax.jdo.listener.DeleteCallback;

import org.opencrx.kernel.backend.Depots;
import org.opencrx.kernel.depot1.jmi1.BookingOrigin;
import org.opencrx.kernel.depot1.jmi1.BookingText;
import org.opencrx.kernel.depot1.jmi1.CompoundBooking;
import org.opencrx.kernel.depot1.jmi1.DepotPosition;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class CompoundBookingImpl
	<S extends org.opencrx.kernel.depot1.jmi1.CompoundBooking,N extends org.opencrx.kernel.depot1.cci2.CompoundBooking,C extends Void>
	extends AbstractObject<S,N,C>
	implements DeleteCallback {

    //-----------------------------------------------------------------------
    public CompoundBookingImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult cancelCb(
    ) {
        try {
            List<String> errors = new ArrayList<String>();
            CompoundBooking compoundBooking = Depots.getInstance().cancelCompoundBooking(
                this.sameObject(),
                errors
            );
            if(compoundBooking == null) {
                return Structures.create(
                	org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.class, 
                	Datatypes.member(org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.Member.cancelBooking, null),
                	Datatypes.member(org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.Member.status, (short)1),
                	Datatypes.member(org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.Member.statusMessage, errors.toString())                	
                );            	
            } else {
                return Structures.create(
                	org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.class, 
                	Datatypes.member(org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.Member.cancelBooking, compoundBooking),
                	Datatypes.member(org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.Member.status, (short)0),
                	Datatypes.member(org.opencrx.kernel.depot1.jmi1.CancelCompoundBookingResult.Member.statusMessage, null)                	
                );            	
            }
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }

    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void acceptCb(
    ) {
        try {
            Depots.getInstance().acceptCompoundBooking(
                this.sameObject()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
        
    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void finalizeCb(
    ) {
        try {
            Depots.getInstance().finalizeCompoundBooking(
                this.sameObject()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
        
    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void lockCb(
        org.opencrx.kernel.depot1.jmi1.LockCompoundBookingParams params
    ) {
        try {
            Depots.getInstance().lockCompoundBooking(
                this.sameObject(),
                params.getLockingReason()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
    
    /**
     * Append bookings to compound booking.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.depot1.jmi1.AppendBookingsResult appendBookings(
        org.opencrx.kernel.depot1.jmi1.AppendBookingsParams params
    ) {
        try {
            List<String> errors = new ArrayList<String>();
            Boolean[] arrIsCreditBookings = params.getIsCreditBooking().toArray(new Boolean[params.getIsCreditBooking().size()]);
            BigDecimal[] arrQuantities = params.getQuantity().toArray(new BigDecimal[params.getQuantity().size()]);
            BookingText[] arrBookingTexts = params.getBookingText().toArray(new BookingText[params.getBookingText().size()]);
            DepotPosition[] arrDepotPositions = params.getDepotPosition().toArray(new DepotPosition[params.getDepotPosition().size()]);
            BookingOrigin[] arrBookingOrigins = params.getBookingOrigin().toArray(new BookingOrigin[params.getBookingOrigin().size()]);
            String[] arrBookingTextSuffixes = params.getBookingTextSuffix().toArray(new String[params.getBookingTextSuffix().size()]);
            Depots.getInstance().validateBookings(
            	params.getValueDate(), 
            	params.getBookingType(), 
            	params.isNoBalanceValidation(), 
            	arrIsCreditBookings, 
            	arrQuantities, 
            	arrBookingTexts, 
            	arrDepotPositions, 
            	arrBookingOrigins, 
            	errors
            );
            if(errors.isEmpty()) {
	            Depots.getInstance().appendBookings(
	            	this.sameObject(), 
	            	params.getValueDate(), 
	            	params.getBookingType(), 
	            	params.isNoBalanceValidation(),
	            	arrIsCreditBookings, 
	            	arrQuantities, 
	            	arrBookingTexts, 
	            	arrDepotPositions, 
	            	arrBookingOrigins,
	            	arrBookingTextSuffixes,
	            	errors
	            );
            }
            return Structures.create(
            	org.opencrx.kernel.depot1.jmi1.AppendBookingsResult.class, 
            	Datatypes.member(org.opencrx.kernel.depot1.jmi1.AppendBookingsResult.Member.status, errors.isEmpty() ? (short)0 : (short)1),
            	Datatypes.member(org.opencrx.kernel.depot1.jmi1.AppendBookingsResult.Member.statusMessage, errors.toString().replace("[", "{").replace("]", "}"))
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }        
    }

    /**
     * Append bookings to compound booking.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.depot1.jmi1.AppendBookingsResult appendBookings2(
        org.opencrx.kernel.depot1.jmi1.AppendBookings2Params params
    ) {
        try {
            List<String> errors = new ArrayList<String>();
            List<Boolean> isCreditBookings = new ArrayList<Boolean>();
            List<BigDecimal> quantities = new ArrayList<BigDecimal>();
            List<BookingText> bookingTexts = new ArrayList<BookingText>();
            List<DepotPosition> depotPositions = new ArrayList<DepotPosition>();
            List<BookingOrigin> bookingOrigins = new ArrayList<BookingOrigin>();
            List<String> bookingTextSuffixes = new ArrayList<String>();
            if(params.getDepotPosition0() != null) {
            	isCreditBookings.add(params.isCreditBooking0());
            	quantities.add(params.getQuantity0());
            	bookingTexts.add(params.getBookingText0());
            	depotPositions.add(params.getDepotPosition0());
            	bookingOrigins.add(params.getBookingOrigin0());
            	bookingTextSuffixes.add(params.getBookingTextSuffix0());
            }
            if(params.getDepotPosition1() != null) {
            	isCreditBookings.add(params.isCreditBooking1());
            	quantities.add(params.getQuantity1());
            	bookingTexts.add(params.getBookingText1());
            	depotPositions.add(params.getDepotPosition1());
            	bookingOrigins.add(params.getBookingOrigin1());
                bookingTextSuffixes.add(params.getBookingTextSuffix1());
            }
            if(params.getDepotPosition2() != null) {
            	isCreditBookings.add(params.isCreditBooking2());
            	quantities.add(params.getQuantity2());
            	bookingTexts.add(params.getBookingText2());
            	depotPositions.add(params.getDepotPosition2());
            	bookingOrigins.add(params.getBookingOrigin2());
                bookingTextSuffixes.add(params.getBookingTextSuffix2());
            }
            if(params.getDepotPosition3() != null) {
            	isCreditBookings.add(params.isCreditBooking3());
            	quantities.add(params.getQuantity3());
            	bookingTexts.add(params.getBookingText3());
            	depotPositions.add(params.getDepotPosition3());
            	bookingOrigins.add(params.getBookingOrigin3());
                bookingTextSuffixes.add(params.getBookingTextSuffix3());
            }
            if(params.getDepotPosition4() != null) {
            	isCreditBookings.add(params.isCreditBooking4());
            	quantities.add(params.getQuantity4());
            	bookingTexts.add(params.getBookingText4());
            	depotPositions.add(params.getDepotPosition4());
            	bookingOrigins.add(params.getBookingOrigin4());
                bookingTextSuffixes.add(params.getBookingTextSuffix4());
            }
            if(params.getDepotPosition5() != null) {
            	isCreditBookings.add(params.isCreditBooking5());
            	quantities.add(params.getQuantity5());
            	bookingTexts.add(params.getBookingText5());
            	depotPositions.add(params.getDepotPosition5());
            	bookingOrigins.add(params.getBookingOrigin5());
                bookingTextSuffixes.add(params.getBookingTextSuffix5());
            }
            Boolean[] arrIsCreditBookings = isCreditBookings.toArray(new Boolean[isCreditBookings.size()]);
            BigDecimal[] arrQuantities = quantities.toArray(new BigDecimal[quantities.size()]);
            BookingText[] arrBookingTexts = bookingTexts.toArray(new BookingText[bookingTexts.size()]);
            DepotPosition[] arrDepotPositions = depotPositions.toArray(new DepotPosition[depotPositions.size()]);
            BookingOrigin[] arrBookingOrigins = bookingOrigins.toArray(new BookingOrigin[bookingOrigins.size()]);
            String[] arrBookingTextSuffixes = bookingTextSuffixes.toArray(new String[bookingTextSuffixes.size()]);
            Depots.getInstance().validateBookings(
            	params.getValueDate(), 
            	params.getBookingType(), 
            	params.isNoBalanceValidation(),
            	arrIsCreditBookings, 
            	arrQuantities, 
            	arrBookingTexts, 
            	arrDepotPositions, 
            	arrBookingOrigins, 
            	errors
            );
            if(errors.isEmpty()) {
	            Depots.getInstance().appendBookings(
	            	this.sameObject(), 
	            	params.getValueDate(), 
	            	params.getBookingType(), 
	            	params.isNoBalanceValidation(),
	            	arrIsCreditBookings, 
	            	arrQuantities, 
	            	arrBookingTexts, 
	            	arrDepotPositions, 
	            	arrBookingOrigins,
	            	arrBookingTextSuffixes,
	            	errors
	            );
            }
            return Structures.create(
            	org.opencrx.kernel.depot1.jmi1.AppendBookingsResult.class, 
            	Datatypes.member(org.opencrx.kernel.depot1.jmi1.AppendBookingsResult.Member.status, errors.isEmpty() ? (short)0 : (short)1),
            	Datatypes.member(org.opencrx.kernel.depot1.jmi1.AppendBookingsResult.Member.statusMessage, errors.toString().replace("[", "{").replace("]", "}"))                	
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }   
    }

    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void unlockCb(
    ) {
        try {
            Depots.getInstance().unlockCompoundBooking(
                this.sameObject()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }

    /* (non-Javadoc)
     * @see org.openmdx.base.aop2.AbstractObject#jdoPreDelete()
     */
    @Override
    public void jdoPreDelete(
    ) {
    	try {
    		Depots.getInstance().preDelete(
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
        
}
