/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Depots
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
package org.opencrx.kernel.backend;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.Query;

import org.opencrx.kernel.depot1.cci2.BookingPeriodQuery;
import org.opencrx.kernel.depot1.cci2.BookingTextQuery;
import org.opencrx.kernel.depot1.cci2.CompoundBookingQuery;
import org.opencrx.kernel.depot1.cci2.CreditBookingQuery;
import org.opencrx.kernel.depot1.cci2.DepotPositionQuery;
import org.opencrx.kernel.depot1.cci2.DepotQuery;
import org.opencrx.kernel.depot1.cci2.DepotReportItemPositionQuery;
import org.opencrx.kernel.depot1.cci2.DepotReportQuery;
import org.opencrx.kernel.depot1.cci2.ProductDepotPositionQuery;
import org.opencrx.kernel.depot1.cci2.SimpleBookingQuery;
import org.opencrx.kernel.depot1.cci2.SingleBookingQuery;
import org.opencrx.kernel.depot1.jmi1.BookingOrigin;
import org.opencrx.kernel.depot1.jmi1.BookingPeriod;
import org.opencrx.kernel.depot1.jmi1.BookingText;
import org.opencrx.kernel.depot1.jmi1.CompoundBooking;
import org.opencrx.kernel.depot1.jmi1.CreditBooking;
import org.opencrx.kernel.depot1.jmi1.DebitBooking;
import org.opencrx.kernel.depot1.jmi1.Depot;
import org.opencrx.kernel.depot1.jmi1.DepotEntity;
import org.opencrx.kernel.depot1.jmi1.DepotGroup;
import org.opencrx.kernel.depot1.jmi1.DepotHolder;
import org.opencrx.kernel.depot1.jmi1.DepotPosition;
import org.opencrx.kernel.depot1.jmi1.DepotReport;
import org.opencrx.kernel.depot1.jmi1.DepotReportItemPosition;
import org.opencrx.kernel.depot1.jmi1.DepotType;
import org.opencrx.kernel.depot1.jmi1.PhoneNumber;
import org.opencrx.kernel.depot1.jmi1.ProductDepotPosition;
import org.opencrx.kernel.depot1.jmi1.SimpleBooking;
import org.opencrx.kernel.depot1.jmi1.SingleBooking;
import org.opencrx.kernel.generic.OpenCrxException;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.product1.jmi1.Product;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.persistence.cci.UserObjects;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.format.DateTimeFormat;

public class Depots extends AbstractImpl {

	/**
	 * Register backend.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new Depots());
	}
	
	/**
	 * Get instance of registered backend.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static Depots getInstance(
	) throws ServiceException {
		return getInstance(Depots.class);
	}

	/**
	 * Constructor.
	 * 
	 */
	protected Depots(
	) {
		
	}
	
    /**
     * Returns the depot segment.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.opencrx.kernel.depot1.jmi1.Segment getDepotSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.depot1.jmi1.Segment) pm.getObjectById(
            new Path("xri://@openmdx*org.opencrx.kernel.depot1").getDescendant("provider", providerName, "segment", segmentName)
        );
    }

    /**
     * Test that the given depot position accepts bookings of the given type:
     * <ul>
     *   <li>Check for opening and closing date at level depot and depot position
     *   <li>Check for locked flag at level depot and depot position
     *   <li>Check value date for non-final and open booking period
     * </ul>
     * 
     * @param valueDate
     * @param bookingType
     * @param depotPosition
     * @throws ServiceException
     */
    public void assertOpenPosition(
        Date valueDate,
        short bookingType,
        DepotPosition depotPosition
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depotPosition);
        DepotEntity depotEntity = (DepotEntity)pm.getObjectById(depotPosition.refGetPath().getPrefix(7));
        Collection<BookingPeriod> bookingPeriods = depotEntity.getBookingPeriod();
        Depot depot = (Depot)pm.getObjectById(depotPosition.refGetPath().getParent().getParent());  
        String depotNumber = depot.getDepotNumber();
        String positionName = depotPosition.getName();        
        // Check for depot position isLocked
        if(depotPosition.isLocked()) { 
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_POSITION_IS_LOCKED,
                "Depot position is locked",
                new BasicException.Parameter("param0", depotNumber),
                new BasicException.Parameter("param1", positionName)
            );
        }
        // Check for closing date of depot position
        if(
            (depotPosition.getClosingDate() != null) && 
            (valueDate.compareTo(depotPosition.getClosingDate()) >= 0) 
        ) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_POSITION_IS_CLOSED,
                "Depot position is closed",
                new BasicException.Parameter("param0", depotNumber),
                new BasicException.Parameter("param1", positionName),
                new BasicException.Parameter("param2", depotPosition.getClosingDate())
            );
        }
        // Check for opening date of depot position
        if(
            (depotPosition.getOpeningDate() != null) && 
            (valueDate.compareTo(depotPosition.getOpeningDate()) < 0) 
        ) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_POSITION_IS_NOT_OPEN,
                "Depot position is not open",
                new BasicException.Parameter("param0", depotNumber),
                new BasicException.Parameter("param1", positionName),
                new BasicException.Parameter("param2", depotPosition.getOpeningDate())
            );
        }        
        // Check for depot isLocked
        if(depot.isLocked()) { 
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_DEPOT_IS_LOCKED,
                "Depot is locked",
                new BasicException.Parameter("param0", depotNumber)
            );
        }
        // Check for closing date of depot
        if(
            (depot.getClosingDate() != null) && 
            (valueDate.compareTo(depot.getClosingDate()) >= 0) 
        ) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_DEPOT_IS_CLOSED_CAN_NOT_BOOK,
                "Depot is closed",
                new BasicException.Parameter("param0", depotNumber),
                new BasicException.Parameter("param1", depot.getClosingDate())
            );
        }
        // Check for opening date of depot
        if(
            (depot.getOpeningDate() != null) && 
            (valueDate.compareTo(depot.getOpeningDate()) < 0) 
        ) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_DEPOT_IS_NOT_OPEN,
                "Depot is not open",
                new BasicException.Parameter("param0", depotNumber),
                new BasicException.Parameter("param1", depot.getOpeningDate())                
            );
        }
        // Find booking period matching value date
        BookingPeriod bookingPeriod = null;
        for(BookingPeriod period: bookingPeriods) {
            Date periodStartsAt = period.getPeriodStartsAt();
            Date periodEndsAtExclusive = period.getPeriodEndsAtExclusive();            
            if(
                ((periodStartsAt == null) || (valueDate.compareTo(periodStartsAt) >= 0)) &&
                ((periodEndsAtExclusive == null) || (valueDate.compareTo(periodEndsAtExclusive) < 0))
            ) {
                bookingPeriod = period;
                break;
            }
        }
        if(bookingPeriod == null) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_BOOKING_PERIOD_NOT_FOUND,
                "No booking period found for value date",
                new BasicException.Parameter("param0", valueDate)
            );
        }
        String bookingPeriodName = bookingPeriod.getName();        
        // Check for non-final booking period
        if(
            (bookingPeriod.isFinal() != null) &&  
            bookingPeriod.isFinal().booleanValue()
        ) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_BOOKING_PERIOD_IS_FINAL,
                "Booking period is final",
                new BasicException.Parameter("param0", bookingPeriodName)
            );
        }               
        // Check for non-closed booking period
        if(
            (bookingPeriod.isClosed() != null) && 
            bookingPeriod.isClosed().booleanValue() &&
            (bookingType <= bookingPeriod.getClosingBookingTypeThreshold()) 
        ) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_BOOKING_PERIOD_IS_CLOSED,
                "Booking period is closed",
                new BasicException.Parameter("param0", bookingPeriodName),
                new BasicException.Parameter("param1", bookingPeriod.getClosingBookingTypeThreshold())
            );
        }
    }
    
    /**
     * Assert that balance of all credit bookings of given compound booking are equal to balance.
     * 
     * @param cb
     * @param balance
     * @throws ServiceException
     */
    public void assertCreditBalance(
        CompoundBooking cb,
        BigDecimal balance
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(cb);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, cb.refGetPath().getSegment(2).toClassicRepresentation(), cb.refGetPath().getSegment(4).toClassicRepresentation());
    	CreditBookingQuery creditBookingQuery = (CreditBookingQuery)pm.newQuery(CreditBooking.class);
    	creditBookingQuery.thereExistsCb().equalTo(cb);
    	List<CreditBooking> creditBookings = depotSegment.getBooking(creditBookingQuery);
        BigDecimal compoundBalance = BigDecimal.ZERO;
    	for(CreditBooking booking: creditBookings) {
            compoundBalance = compoundBalance.add(
                booking.getQuantityCredit()
            );
        }
        if(compoundBalance.compareTo(balance) != 0) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_REVERSAL_BALANCE_MISMATCH,
                "Balance mismatch",
                new BasicException.Parameter("param0", compoundBalance),
                new BasicException.Parameter("param1", balance)
            );            
        }
    }
    
    /**
     * Find booking text by name.
     * 
     * @param depotEntity
     * @param bookingTextName
     * @return
     */
    public BookingText findBookingText(
    	DepotEntity depotEntity,
    	String bookingTextName
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depotEntity);
    	BookingTextQuery bookingTextQuery = (BookingTextQuery)pm.newQuery(BookingText.class);
    	bookingTextQuery.name().equalTo(bookingTextName);
    	List<BookingText> texts = depotEntity.getBookingText(bookingTextQuery);
    	return texts.isEmpty() ? null : texts.iterator().next();
    }

    /**
     * Derive name of compound booking from given credit / debit position and booking text.
     * Override for custom naming.
     * 
     * @param positionCredit
     * @param positionDebit
     * @param bookingText
     * @return
     * @throws ServiceException
     */
    public String getCompoundBookingName(
        DepotPosition positionCredit,
        DepotPosition positionDebit,
        BookingText bookingText
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(positionCredit);
        String positionNameCredit = positionCredit.getName();                    
        Depot depotCredit = (Depot)pm.getObjectById(
            positionCredit.refGetPath().getParent().getParent()
        );
        String depotNumberCredit = depotCredit.getDepotNumber();
        Depot depotDebit = (Depot)pm.getObjectById(
        	positionDebit.refGetPath().getParent().getParent()
        );
        String depotNumberDebit = depotDebit.getDepotNumber();
        String compoundBookingName = null;
        if(!bookingText.isCreditFirst()) {
        	compoundBookingName = depotNumberCredit + " " + bookingText.getCbNameInfix1() + " " + positionNameCredit + " " + bookingText.getCbNameInfix2() + " " + depotNumberDebit;  
        } else {
        	compoundBookingName = depotNumberDebit + " " + bookingText.getCbNameInfix1() + " " + positionNameCredit + " " + bookingText.getCbNameInfix2() + " " + depotNumberCredit; 
        }
        return compoundBookingName;
    }
    
    /**
     * Create a credit / debit booking as compound booking for the given quantity.
     * 
     * @param depotEntity
     * @param valueDate
     * @param bookingType
     * @param quantity
     * @param bookingTextName
     * @param bookingText
     * @param positionCredit
     * @param positionDebit
     * @param originIdentity
     * @param errors
     * @return
     * @throws ServiceException
     */
    public CompoundBooking createCreditDebitBooking(
        DepotEntity depotEntity,
        Date valueDate,
        short bookingType,
        BigDecimal quantity,
        String bookingTextName,
        BookingText bookingText,
        DepotPosition positionCredit,
        DepotPosition positionDebit,
        BookingOrigin originIdentity,
        String bookingTextSuffix,
        List<String> errors
    ) throws ServiceException {
    	if(bookingText == null) {
    		if(bookingTextName != null) {
    			bookingText = this.findBookingText(depotEntity, bookingTextName);
    		}
    	}
        if(bookingText == null) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_MISSING_BOOKING_TEXT,
                "Missing booking text",
                new BasicException.Parameter("param0", bookingTextName)                   
            );
        }
        if(
            (positionCredit.getName() == null) ||
            (positionDebit.getName() == null) ||
            !positionCredit.getName().equals(positionDebit.getName())
        ) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.DEPOT_POSITION_NAME_MISMATCH,
               "position names debit/credit do not match",
               new BasicException.Parameter("param0", positionCredit.getName()),
               new BasicException.Parameter("param1", positionDebit.getName())
           );                            
        }
        CompoundBooking compoundBooking = this.createCompoundBooking(
            depotEntity,
            this.getCompoundBookingName(positionCredit, positionDebit, bookingText),
            bookingType
        );
        this.appendBookings(
        	compoundBooking, 
        	valueDate, 
        	bookingType, 
        	false, // noBalanceValidation
        	new Boolean[]{Boolean.TRUE, Boolean.FALSE}, 
        	new BigDecimal[]{quantity, quantity}, 
        	new BookingText[]{bookingText, bookingText}, 
        	new DepotPosition[]{positionCredit, positionDebit}, 
        	new BookingOrigin[]{originIdentity, originIdentity},
        	new String[]{bookingTextSuffix, bookingTextSuffix},
        	errors
        );
        return compoundBooking;
    }

    /**
     * Validate bookings and report errors.
     * 
     * @param valueDate
     * @param bookingType
     * @param noBalanceValidation
     * @param isCreditBookings
     * @param quantities
     * @param bookingTexts
     * @param depotPositions
     * @param origins
     * @param errors
     * @throws ServiceException
     */
    public void validateBookings(
        Date valueDate,
        short bookingType,
        Boolean noBalanceValidation,
        Boolean[] isCreditBookings,
        BigDecimal[] quantities,
        BookingText[] bookingTexts,
        DepotPosition[] depotPositions,
        BookingOrigin[] origins,
        List<String> errors
    ) throws ServiceException {
        if(depotPositions == null || depotPositions.length == 0) {
        	errors.add("Missing depot positions");
        	return;
        }
        int numberOfPositions = depotPositions.length;
        if(
        	isCreditBookings == null || isCreditBookings.length != numberOfPositions ||
        	quantities == null || quantities.length != numberOfPositions ||
        	bookingTexts == null || bookingTexts.length != numberOfPositions ||
        	origins == null || origins.length != numberOfPositions
        ) {
        	errors.add("Number of quantities, booking texts, depot positions and origins must match");
        	return;
        }
        int index = 0;
        for(DepotPosition depotPosition: depotPositions) {
        	if(depotPosition == null) {
        		errors.add("depot position at index " + index + " is empty");
        	}
        	index++;
        }
        index = 0;
        for(BookingText bookingText: bookingTexts) {
        	if(bookingText == null) {
        		errors.add("booking text at index " + index + " is empty");
        	}
        }
        index = 0;
        for(BigDecimal quantity: quantities) {
        	if(quantity == null) {
        		errors.add("quantity at index " + index + " is empty");
        	}
        	index++;
        }
        if(!errors.isEmpty()) {
        	return;
        }
        BigDecimal balanceCredit = BigDecimal.ZERO;
        BigDecimal balanceDebit = BigDecimal.ZERO;
        index = 0;
        for(BigDecimal quantity: quantities) {
        	if(Boolean.TRUE.equals(isCreditBookings[index])) {
        		balanceCredit = balanceCredit.add(quantity);
        	} else {
        		balanceDebit = balanceDebit.add(quantity);
        	}
        	index++;
        }
        if(
        	!Boolean.TRUE.equals(noBalanceValidation) && 
        	balanceCredit.compareTo(balanceDebit) != 0
        ) {
        	errors.add("balance credit / debit mismatch {balanceCredit: " + balanceCredit + "; balanceDebit: " + balanceDebit + "}");
        }
    }

    /**
     * Append bookings to given compound booking.
     * 
     * @param compoundBooking
     * @param valueDate
     * @param bookingType
     * @param isCreditBookings
     * @param quantities
     * @param bookingTexts
     * @param depotPositions
     * @param origins
     * @param bookingTextSuffixes
     * @param errors
     * @throws ServiceException
     */
    public void appendBookings(
    	CompoundBooking compoundBooking,
        Date valueDate,
        short bookingType,
        Boolean noBalanceValidation,
        Boolean[] isCreditBookings,
        BigDecimal[] quantities,
        BookingText[] bookingTexts,
        DepotPosition[] depotPositions,
        BookingOrigin[] origins,
        String[] bookingTextSuffixes,
        List<String> errors
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(compoundBooking);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, compoundBooking.refGetPath().getSegment(2).toClassicRepresentation(), compoundBooking.refGetPath().getSegment(4).toClassicRepresentation());
        if(compoundBooking.isLocked()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_POSITION_IS_LOCKED,
                "Booking is locked. Append is not allowed."
            );            
        }
        if(compoundBooking.getBookingStatus() != BookingStatus.PENDING.getValue()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status must be pending. Append is not allowed."
            );                                                
        }
        this.validateBookings(
        	valueDate, 
        	bookingType, 
        	noBalanceValidation, 
        	isCreditBookings, 
        	quantities, 
        	bookingTexts, 
        	depotPositions, 
        	origins, 
        	errors
        );
        if(!errors.isEmpty()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_INVALID_COMPOUND_BOOKING,
                "Invalid compound booking."
            );        	
        }
        // Set default valueDate to current date
        if(valueDate == null) {
            valueDate = new Date();
        }
        // Assert depot positions
        for(DepotPosition position: depotPositions) {
        	this.assertOpenPosition(
                valueDate,
                bookingType,
                position
            );
        }
        List<CreditBooking> creditBookings = new ArrayList<CreditBooking>();
        List<DebitBooking> debitBookings = new ArrayList<DebitBooking>();
        Date bookingDate = new Date();
        // Create bookings
        for(int i = 0; i < depotPositions.length; i++) {
        	DepotPosition depotPosition = depotPositions[i];
            String positionName = depotPosition.getName();                        
            Depot depot = (Depot)pm.getObjectById(
                depotPosition.refGetPath().getParent().getParent()
            );
            String depotNumber = depot.getDepotNumber();
            SingleBooking booking = null;
            String bookingTextSuffix = bookingTextSuffixes == null 
            	? null 
            	: i < bookingTextSuffixes.length 
            		? bookingTextSuffixes[i]
            		: null;
            if(Boolean.TRUE.equals(isCreditBookings[i])) {
            	CreditBooking creditBooking = pm.newInstance(CreditBooking.class);
            	creditBooking.setQuantityCredit(quantities[i]);
            	creditBooking.setName(
                    (depotNumber + " " + bookingTexts[i].getCreditBookingNameInfix() + " " + positionName + (bookingTextSuffix == null ? "" : bookingTextSuffix)).trim()
                );
                creditBookings.add(creditBooking);
                booking = creditBooking;
            } else {
            	DebitBooking debitBooking = pm.newInstance(DebitBooking.class);
            	debitBooking.setQuantityDebit(quantities[i]);
            	debitBooking.setName(
                    (depotNumber + " " + bookingTexts[i].getDebitBookingNameInfix() + " " + positionName + (bookingTextSuffix == null ? "" : bookingTextSuffix)).trim()
                );
            	debitBookings.add(debitBooking);
            	booking = debitBooking;
            }
            booking.setValueDate(valueDate);
            booking.setBookingType(new Short(bookingType));
            booking.setBookingStatus(BookingStatus.PENDING.getValue());
            booking.setBookingDate(bookingDate);
            booking.setPosition(depotPosition);
            booking.setCb(compoundBooking);
            if(origins[i] != null) {
                booking.setOrigin(origins[i]);
            }
            depotSegment.addBooking(
            	this.getUidAsString(),
            	booking
            );
        }    	
    }

    /**
     * Create a compound booking for the given quantities and debit / credit positions.
     * 
     * @param depotEntity
     * @param name
     * @param valueDate
     * @param bookingType
     * @param errors
     * @return
     * @throws ServiceException
     */
    public CompoundBooking createCompoundBooking(
        DepotEntity depotEntity,
        String name,
        short bookingType
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depotEntity);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, depotEntity.refGetPath().getSegment(2).toClassicRepresentation(), depotEntity.refGetPath().getSegment(4).toClassicRepresentation());
        Date bookingDate = new Date();
        CompoundBooking compoundBooking = pm.newInstance(CompoundBooking.class);
        compoundBooking.setName(name);
        compoundBooking.setBookingType(new Short(bookingType));
        compoundBooking.setBookingStatus(BookingStatus.PENDING.getValue());
        compoundBooking.setBookingDate(bookingDate);
        depotSegment.addCb(
        	this.getUidAsString(),
        	compoundBooking
        );
        return compoundBooking;
    }

    /**
     * Lookup depot position. Auto-create position if it does not exist and depot allows
     * auto-creation of positions.
     * 
     * @param depotEntity
     * @param depotNumber
     * @param depot
     * @param positionName
     * @param product
     * @param openingDate
     * @return
     * @throws ServiceException
     */
    public DepotPosition getAndCreateDepotPosition(
        DepotEntity depotEntity,
        String depotNumber,
        Depot depot,
        String positionName,
        Product product,
        Date openingDate
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depotEntity);
    	if(depot == null) {
	        if(depotNumber == null) {
	           throw new ServiceException(
	               OpenCrxException.DOMAIN,
	               OpenCrxException.DEPOT_MISSING_DEPOT_NUMBER,
	               "Missing depot number"
	           );                                        
	        }
	        DepotQuery depotQuery = (DepotQuery)pm.newQuery(Depot.class);
	        depotQuery.depotNumber().equalTo(depotNumber);
	        List<Depot> depots = depotEntity.getDepot(depotQuery);
	        if(!depots.isEmpty()) {
	            depot = depots.iterator().next();
	        } else {
	           throw new ServiceException(
	               OpenCrxException.DOMAIN,
	               OpenCrxException.DEPOT_DEPOT_NOT_FOUND,
	               "Depot not found",
	               new BasicException.Parameter("param0", depotNumber)
	           );                                        
	        }
    	}
    	DepotPositionQuery depotPositionQuery = (DepotPositionQuery)pm.newQuery(DepotPosition.class);
    	depotPositionQuery.name().equalTo(positionName);
    	List<DepotPosition> depotPositions = depot.getPosition(depotPositionQuery);
	    if(!depotPositions.isEmpty()) {
	        return depotPositions.iterator().next();
	    } else {
		    // auto-create position
	        if(depot.isAllowPositionAutoCreate()) {
	        	return this.openDepotPosition(
	        		depot, 
	        		positionName, 
	        		null, 
	        		openingDate, 
	        		null, 
	        		product, 
	        		Boolean.FALSE 
	        	);
	        } else {
	            return null;
	        }
	    }
    }

    /**
     * Create credit / debit bookings for the given depot positions.
     * 
     * @param depotEntity
     * @param valueDate
     * @param bookingType
     * @param quantity
     * @param bookingTextName
     * @param bookingText
     * @param positionCredit
     * @param positionDebit
     * @param origin
     * @param reversalOf
     * @param errors
     * @return
     * @throws ServiceException
     */
    public CompoundBooking createBookingByPosition(
        DepotEntity depotEntity,
        Date valueDate,
        short bookingType,
        BigDecimal quantity,
        String bookingTextName,
        BookingText bookingText,
        DepotPosition positionCredit,
        DepotPosition positionDebit,
        BookingOrigin origin,
        String bookingTextSuffix,
        List<String> errors
    ) throws ServiceException {
        return this.createCreditDebitBooking(                
            depotEntity,
            valueDate,
            bookingType,
            quantity,
            bookingTextName,
            bookingText,
            positionCredit,
            positionDebit,
            origin,
            bookingTextSuffix,
            errors
        );
    }

    /**
     * Create credit / debit booking for the depot positions defined by the given product.
     * 
     * @param depotEntity
     * @param valueDate
     * @param bookingType
     * @param quantity
     * @param bookingTextName
     * @param bookingText
     * @param product
     * @param depotNumberCredit
     * @param depotCredit
     * @param depotNumberDebit
     * @param depotDebit
     * @param origin
     * @param reversalOf
     * @param errors
     * @return
     * @throws ServiceException
     */
    public CompoundBooking createBookingByProduct(
        DepotEntity depotEntity,
        Date valueDate,
        short bookingType,
        BigDecimal quantity,
        String bookingTextName,
        BookingText bookingText,
        Product product,
        String depotNumberCredit,
        Depot depotCredit,
        String depotNumberDebit,
        Depot depotDebit,
        BookingOrigin origin,
        String bookingTextSuffix,
        List<String> errors
    ) throws ServiceException {
        if(product == null) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.DEPOT_MISSING_PRODUCT,
               "Missing product"
           );                                    
        }
        String positionName = product.getProductNumber() != null 
        	? product.getProductNumber() 
        	: product.getName();            
        DepotPosition positionCredit = this.getAndCreateDepotPosition(
            depotEntity,
            depotNumberCredit,
            depotCredit,
            positionName,
            product,
            valueDate
        );
        if(positionCredit == null) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.DEPOT_INVALID_POSITION_CREDIT,
               "Can not get/create credit depot position"
           );                                                
        }
        DepotPosition positionDebit = this.getAndCreateDepotPosition(
            depotEntity,
            depotNumberDebit,
            depotDebit,
            positionName,
            product,
            valueDate
        );
        if(positionDebit == null) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.DEPOT_INVALID_POSITION_DEBIT,
               "Can not get/create debit depot position"
           );                                                
        }
        return this.createCreditDebitBooking(
            depotEntity,
            valueDate,
            bookingType,
            quantity,            
            bookingTextName,
            bookingText,
            positionCredit,
            positionDebit,
            origin,
            bookingTextSuffix,
            errors
        );
    }
    
    /**
     * Create credit / debit booking for the depot positions defined by the position name.
     * 
     * @param depotEntity
     * @param valueDate
     * @param bookingType
     * @param quantity
     * @param bookingTextName
     * @param bookingText
     * @param positionName
     * @param depotNumberCredit
     * @param depotCredit
     * @param depotNumberDebit
     * @param depotDebit
     * @param origin
     * @param reversalOf
     * @param errors
     * @return
     * @throws ServiceException
     */
    public CompoundBooking createBookingByPositionName(
        DepotEntity depotEntity,
        Date valueDate,
        short bookingType,
        BigDecimal quantity,
        String bookingTextName,
        BookingText bookingText,
        String positionName,
        String depotNumberCredit,
        Depot depotCredit,
        String depotNumberDebit,
        Depot depotDebit,
        BookingOrigin origin,
        String bookingTextSuffix,
        List<String> errors
    ) throws ServiceException {
        DepotPosition positionCredit = this.getAndCreateDepotPosition(
            depotEntity,
            depotNumberCredit,
            depotCredit,
            positionName,
            null,
            valueDate
        );
        if(positionCredit == null) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.DEPOT_INVALID_POSITION_CREDIT,
               "Can not get/create credit depot position"
           );                                                
        }
        DepotPosition positionDebit = this.getAndCreateDepotPosition(
            depotEntity,
            depotNumberDebit,
            depotDebit,
            positionName,
            null,
            valueDate
        );
        if(positionDebit == null) {
           throw new ServiceException(
               OpenCrxException.DOMAIN,
               OpenCrxException.DEPOT_INVALID_POSITION_DEBIT,
               "Can not get/create debit depot position"
           );                                                
        }
        return this.createCreditDebitBooking(
            depotEntity,
            valueDate,
            bookingType,
            quantity,            
            bookingTextName,
            bookingText,
            positionCredit,
            positionDebit,
            origin,
            bookingTextSuffix,
            errors
        );
    }

    /**
     * Get position name for depot report item position. By default
     * this is the depot position name or the product number for 
     * ProductDepotPositions. Override for custom-specific naming.
     * 
     * @param depotPosition
     * @return
     * @throws ServiceException
     */
    public String getDepotReportItemPositionName(
    	DepotPosition depotPosition
    ) throws ServiceException {
    	return depotPosition.getName();
    }

    /**
     * DepotReportItem
     *
     */
    private static class DepotReportItem {
        /**
		 * @return the balanceBop
		 */
		public BigDecimal getBalanceBop() {
			return balanceBop;
		}
		/**
		 * @param balanceBop the balanceBop to set
		 */
		public void setBalanceBop(BigDecimal balanceBop) {
			this.balanceBop = balanceBop;
		}
		/**
		 * @return the balanceDebitBop
		 */
		public BigDecimal getBalanceDebitBop() {
			return balanceDebitBop;
		}
		/**
		 * @param balanceDebitBop the balanceDebitBop to set
		 */
		public void setBalanceDebitBop(BigDecimal balanceDebitBop) {
			this.balanceDebitBop = balanceDebitBop;
		}
		/**
		 * @return the balanceCreditBop
		 */
		public BigDecimal getBalanceCreditBop() {
			return balanceCreditBop;
		}
		/**
		 * @param balanceCreditBop the balanceCreditBop to set
		 */
		public void setBalanceCreditBop(BigDecimal balanceCreditBop) {
			this.balanceCreditBop = balanceCreditBop;
		}
		/**
		 * @return the balanceCredit
		 */
		public BigDecimal getBalanceCredit() {
			return balanceCredit;
		}
		/**
		 * @param balanceCredit the balanceCredit to set
		 */
		public void setBalanceCredit(BigDecimal balanceCredit) {
			this.balanceCredit = balanceCredit;
		}
		/**
		 * @return the balanceDebit
		 */
		public BigDecimal getBalanceDebit() {
			return balanceDebit;
		}
		/**
		 * @param balanceDebit the balanceDebit to set
		 */
		public void setBalanceDebit(BigDecimal balanceDebit) {
			this.balanceDebit = balanceDebit;
		}
		/**
		 * @return the balanceSimple
		 */
		public BigDecimal getBalanceSimple() {
			return balanceSimple;
		}
		/**
		 * @param balanceSimple the balanceSimple to set
		 */
		public void setBalanceSimple(BigDecimal balanceSimple) {
			this.balanceSimple = balanceSimple;
		}
		/**
		 * @return the balanceSimpleBop
		 */
		public BigDecimal getBalanceSimpleBop() {
			return balanceSimpleBop;
		}
		/**
		 * @param balanceSimpleBop the balanceSimpleBop to set
		 */
		public void setBalanceSimpleBop(BigDecimal balanceSimpleBop) {
			this.balanceSimpleBop = balanceSimpleBop;
		}
		private BigDecimal balanceBop = BigDecimal.ZERO;
        private BigDecimal balanceDebitBop = BigDecimal.ZERO;
        private BigDecimal balanceCreditBop = BigDecimal.ZERO;
        private BigDecimal balanceCredit = BigDecimal.ZERO;
        private BigDecimal balanceDebit = BigDecimal.ZERO;
        private BigDecimal balanceSimple = BigDecimal.ZERO;
        private BigDecimal balanceSimpleBop = BigDecimal.ZERO;    	
    }

    /**
     * Re-calculate the given depot report. Re-calculate draft reports only.
     * 
     * @param depot
     * @param report
     * @param reportPreviousPeriod
     * @throws ServiceException
     */
    public void refreshReport(
        Depot depot,
        DepotReport report,
        DepotReport reportPreviousPeriod
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depot);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, depot.refGetPath().getSegment(2).toClassicRepresentation(), depot.refGetPath().getSegment(4).toClassicRepresentation());    	
        // Refresh only only draft reports
        if(report.isDraft()) {
        	final int FETCH_SIZE = 500;
        	Map<DepotPosition,DepotReportItemPosition> depotReportItemPositions = new HashMap<DepotPosition,DepotReportItemPosition>();
        	DepotReportItemPositionQuery depotReportItemPositionQuery = (DepotReportItemPositionQuery)pm.newQuery(DepotReportItemPosition.class);
        	((Query)depotReportItemPositionQuery).getFetchPlan().setFetchSize(FETCH_SIZE);  	
        	for(DepotReportItemPosition depotReportItemPosition: report.<DepotReportItemPosition>getItemPosition(depotReportItemPositionQuery)) {
        		depotReportItemPositions.put(
        			depotReportItemPosition.getPosition(), 
        			depotReportItemPosition
        		);
        	}
            BookingPeriod bookingPeriod = report.getBookingPeriod();
            Date periodStartsAt = bookingPeriod.getPeriodStartsAt();
            // Set beginning of report balances to end of period balances of previous report 
            Map<DepotPosition,DepotReportItem> reportItems = new HashMap<DepotPosition,DepotReportItem>();
            DepotPositionQuery depotPositionQuery = (DepotPositionQuery)pm.newQuery(DepotPosition.class);
            ((Query)depotPositionQuery).getFetchPlan().setFetchSize(FETCH_SIZE);   
            for(DepotPosition position: depot.<DepotPosition>getPosition(depotPositionQuery)) {
        		DepotReportItem depotReportItem = new DepotReportItem();
                reportItems.put(position, depotReportItem);
	            if(reportPreviousPeriod != null) {
	            	DepotReportItemPositionQuery itemPositionQuery = (DepotReportItemPositionQuery)pm.newQuery(DepotReportItemPosition.class);
	            	itemPositionQuery.thereExistsPosition().equalTo(position);
	            	List<DepotReportItemPosition> itemPositions = reportPreviousPeriod.getItemPosition(itemPositionQuery);
	            	if(!itemPositions.isEmpty()) {
	            		DepotReportItemPosition itemPosition = itemPositions.iterator().next();
	            		depotReportItem.setBalanceBop(itemPosition.getBalance());                        
	            		depotReportItem.setBalanceCreditBop(itemPosition.getBalanceCredit());
	            		depotReportItem.setBalanceDebitBop(itemPosition.getBalanceDebit());
	            		depotReportItem.setBalanceCredit(itemPosition.getBalanceCredit());
	            		depotReportItem.setBalanceDebit(itemPosition.getBalanceDebit());
	            		depotReportItem.setBalanceSimpleBop(itemPosition.getBalanceSimple());                                                
	            		depotReportItem.setBalanceSimple(itemPosition.getBalanceSimple());
	                }
	            }
            }
            // Sum up single bookings for all positions of depot within booking period
            {    	
            	SingleBookingQuery singleBookingQuery = (SingleBookingQuery)pm.newQuery(SingleBooking.class);
            	singleBookingQuery.thereExistsPosition().elementOf(
            		PersistenceHelper.getCandidates(
            			pm.getExtent(DepotPosition.class),
            			depot.refGetPath().getDescendant(new String[]{"position", "%"})
            		)
		    	);
                singleBookingQuery.bookingStatus().greaterThanOrEqualTo(report.getBookingStatusThreshold());
                if(bookingPeriod.getPeriodStartsAt() != null) {
                	singleBookingQuery.valueDate().greaterThanOrEqualTo(bookingPeriod.getPeriodStartsAt());
                }
                if(bookingPeriod.getPeriodEndsAtExclusive() != null) {
                	singleBookingQuery.valueDate().lessThan(bookingPeriod.getPeriodEndsAtExclusive());
                }
                ((Query)singleBookingQuery).getFetchPlan().setFetchSize(FETCH_SIZE);
                List<SingleBooking> singleBookings = depotSegment.getBooking(singleBookingQuery);
                for(SingleBooking singleBooking: singleBookings) {
                	try {
	                	DepotPosition position = singleBooking.getPosition();
	                	DepotReportItem depotReportItem = reportItems.get(position);
	                	if(depotReportItem == null) {
	                		reportItems.put(position, depotReportItem = new DepotReportItem());
	                	}
	                    if(singleBooking instanceof CreditBooking) {
	                        // Credit booking
	                        BigDecimal quantityCredit = ((CreditBooking)singleBooking).getQuantityCredit();
	                        depotReportItem.setBalanceCredit(
	                        	depotReportItem.getBalanceCredit().add(quantityCredit)
	                        );
	                    } else if(singleBooking instanceof DebitBooking) {
	                        // Debit booking
	                        BigDecimal quantityDebit = ((DebitBooking)singleBooking).getQuantityDebit();
	                        depotReportItem.setBalanceDebit(
	                        	depotReportItem.getBalanceDebit().add(quantityDebit)
	                        );
	                    }
                	} catch(Exception e) {
                		new ServiceException(e).log();
                	}
                }
            }
            // Sum up simple bookings for all positions of depot within booking period
            {
            	SimpleBookingQuery simpleBookingQuery = (SimpleBookingQuery)pm.newQuery(SimpleBooking.class);
            	simpleBookingQuery.thereExistsPosition().elementOf(
            		PersistenceHelper.getCandidates(
            			pm.getExtent(DepotPosition.class),
            			depot.refGetPath().getDescendant(new String[]{"position", "%"})
            		)
		    	);
                simpleBookingQuery.bookingStatus().greaterThanOrEqualTo(report.getBookingStatusThreshold());
                if(bookingPeriod.getPeriodStartsAt() != null) {
                	simpleBookingQuery.valueDate().greaterThanOrEqualTo(bookingPeriod.getPeriodStartsAt());
                }
                if(bookingPeriod.getPeriodEndsAtExclusive() != null) {
                	simpleBookingQuery.valueDate().lessThan(bookingPeriod.getPeriodEndsAtExclusive());
                }
                ((Query)simpleBookingQuery).getFetchPlan().setFetchSize(FETCH_SIZE);              
                List<SimpleBooking> simpleBookings = depotSegment.getSimpleBooking(simpleBookingQuery);
                // Sum up simple bookings
                for(SimpleBooking simpleBooking: simpleBookings) {
                	try {
	                	DepotPosition position = simpleBooking.getPosition(); 
	                	DepotReportItem depotReportItem = reportItems.get(position);
	                	if(depotReportItem == null) {
	                		reportItems.put(position, depotReportItem = new DepotReportItem());
	                	}                	
	                    BigDecimal quantity = simpleBooking.getQuantity();
	                    depotReportItem.setBalanceSimple(
	                    	depotReportItem.getBalanceSimple().add(quantity)
	                    );
                	} catch(Exception e) {
                		new ServiceException(e).log();
                	}
                }
            }
            // Create/update item positions
            for(Map.Entry<DepotPosition,DepotReportItem> entry: reportItems.entrySet()) {
            	DepotPosition position = entry.getKey();
            	DepotReportItem depotReportItem = entry.getValue();
                DepotReportItemPosition depotReportItemPosition = null;
                if(!depotReportItemPositions.containsKey(position)) {
                    depotReportItemPosition = pm.newInstance(DepotReportItemPosition.class);
                    report.addItemPosition(
                    	this.getUidAsString(),
                    	depotReportItemPosition
                    );                	
                } else {
                	depotReportItemPosition = depotReportItemPositions.get(position);
                }
                // Inherit access levels from parent, i.e. report
                // Default would otherwise be 3/2/2 (browse/update/delete)
                // Only touch if values have changed
                if(depotReportItemPosition.getAccessLevelBrowse() != report.getAccessLevelBrowse()) {
                	depotReportItemPosition.setAccessLevelBrowse(report.getAccessLevelBrowse());
                }
                if(depotReportItemPosition.getAccessLevelDelete() != report.getAccessLevelDelete()) {
                	depotReportItemPosition.setAccessLevelDelete(report.getAccessLevelDelete());
                }
                if(depotReportItemPosition.getAccessLevelUpdate() != report.getAccessLevelUpdate()) {
                	depotReportItemPosition.setAccessLevelUpdate(report.getAccessLevelUpdate());
                }
                String positionName = this.getDepotReportItemPositionName(position);
                if(!Utils.areEqual(depotReportItemPosition.getPositionName(), positionName)) {
                	depotReportItemPosition.setPositionName(positionName);
                }
                if(!Utils.areEqual(depotReportItemPosition.getValueDate(), periodStartsAt)) {
                	depotReportItemPosition.setValueDate(periodStartsAt);
                }
                if(!Utils.areEqual(depotReportItemPosition.getPosition(), position)) {
                	depotReportItemPosition.setPosition(position);
                }
                if(!Utils.areEqual(depotReportItemPosition.getBalanceBop(), depotReportItem.getBalanceBop())) {
                	depotReportItemPosition.setBalanceBop(depotReportItem.getBalanceBop());
                }
                if(!Utils.areEqual(depotReportItemPosition.getBalanceCreditBop(), depotReportItem.getBalanceCreditBop())) {
                	depotReportItemPosition.setBalanceCreditBop(depotReportItem.getBalanceCreditBop());
                }
                if(!Utils.areEqual(depotReportItemPosition.getBalanceDebitBop(), depotReportItem.getBalanceDebitBop())) {
                	depotReportItemPosition.setBalanceDebitBop(depotReportItem.getBalanceDebitBop());
                }
                if(!Utils.areEqual(depotReportItemPosition.getBalanceSimpleBop(), depotReportItem.getBalanceSimpleBop())) {
                	depotReportItemPosition.setBalanceSimpleBop(depotReportItem.getBalanceSimpleBop());
                }
                BigDecimal balance = depotReportItem.getBalanceCredit().subtract(depotReportItem.getBalanceDebit());
                if(!Utils.areEqual(depotReportItemPosition.getBalance(), balance)) {
                	depotReportItemPosition.setBalance(balance);
                }
                if(!Utils.areEqual(depotReportItemPosition.getBalanceCredit(), depotReportItem.getBalanceCredit())) {
                	depotReportItemPosition.setBalanceCredit(depotReportItem.getBalanceCredit());
                }
                if(!Utils.areEqual(depotReportItemPosition.getBalanceDebit(), depotReportItem.getBalanceDebit())) {
                	depotReportItemPosition.setBalanceDebit(depotReportItem.getBalanceDebit());
                }
                if(!Utils.areEqual(depotReportItemPosition.getBalanceSimple(), depotReportItem.getBalanceSimple())) {
                	depotReportItemPosition.setBalanceSimple(depotReportItem.getBalanceSimple());
                }
            }
            // Remove redundant item positions
            depotReportItemPositions.keySet().removeAll(reportItems.keySet());
            for(DepotReportItemPosition depotReportItemPosition: depotReportItemPositions.values()) {
            	depotReportItemPosition.refDelete();
            }
        }
    }

    /**
     * Assert that a refreshed depot report exists for the given depot and
     * all booking periods defined for the depot entity. On the depot update
     * the reference to the latest report.
     * 
     * @param depot
     * @param bookingStatusThreshold
     * @throws ServiceException
     */
    public void assertReports(
        Depot depot,
        short bookingStatusThreshold
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depot);    	
        DepotEntity depotEntity = (DepotEntity)pm.getObjectById(
        	depot.refGetPath().getPrefix(7)
        );
        BookingPeriodQuery bookingPeriodQuery = (BookingPeriodQuery)pm.newQuery(BookingPeriod.class);
        bookingPeriodQuery.orderByPeriodStartsAt().ascending();
        List<BookingPeriod> bookingPeriods = depotEntity.getBookingPeriod(bookingPeriodQuery);
        DepotReport reportPreviousPeriod = null;
        DepotReport latestReport = null;        
        // Assert report for each booking period
        for(BookingPeriod bookingPeriod: bookingPeriods) {
        	DepotReportQuery depotReportQuery = (DepotReportQuery)pm.newQuery(DepotReport.class);
        	depotReportQuery.thereExistsBookingPeriod().equalTo(bookingPeriod);
        	List<DepotReport> reports = depot.getReport(depotReportQuery);
            DepotReport report = null;
            if(!reports.isEmpty()) {
                report = reports.iterator().next();
            } else {
                DepotReport newReport = pm.newInstance(DepotReport.class);
                // Improve security by inheriting access levels from parent, i.e. depot
                // Default would otherwise be 3/2/2 (browse/update/delete)
                newReport.setAccessLevelBrowse(depot.getAccessLevelBrowse());
                newReport.setAccessLevelDelete(depot.getAccessLevelDelete());
                newReport.setAccessLevelUpdate(depot.getAccessLevelUpdate());
                newReport.setName(bookingPeriod.getName());
                newReport.setDescription(bookingPeriod.getDescription());
                newReport.setDraft(Boolean.TRUE);
                newReport.setBookingStatusThreshold(new Short(bookingStatusThreshold));
                newReport.setBookingPeriod(bookingPeriod);
                depot.addReport(
                	this.getUidAsString(),
                	newReport
                );
                report = newReport;
            }
            // Latest report
            Date currentDate = new Date();
            if(
               (currentDate.compareTo(bookingPeriod.getPeriodStartsAt()) >= 0) &&
               ((bookingPeriod.getPeriodEndsAtExclusive() == null) || (currentDate.compareTo(bookingPeriod.getPeriodEndsAtExclusive()) < 0))
            ) {
                latestReport = report;
            }
            // Refresh
            this.refreshReport(
                depot,
                report,
                reportPreviousPeriod
            );
            reportPreviousPeriod = report;
        }
        if(latestReport != null) {
            depot.setLatestReport(latestReport);
        }
    }

    /**
     * Cancel compound booking.
     * 
     * @param cb
     * @param errors
     * @return
     * @throws ServiceException
     */
    public CompoundBooking cancelCompoundBooking(
        CompoundBooking cb,
        List<String> errors
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(cb);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, cb.refGetPath().getSegment(2).toClassicRepresentation(), cb.refGetPath().getSegment(4).toClassicRepresentation());    	    	
        boolean isFinal = cb.getBookingStatus() == BookingStatus.FINAL.getValue();
        if(!isFinal) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PROCESSED,
                "Booking status must be final. Cancel is not allowed."
            );                                                
        }
        short bookingType = cb.getBookingType() > 0 
        	? cb.getBookingType() 
        	: BookingType.STANDARD.getValue();
        // Can not cancel reversal bookings
        if(bookingType == BookingType.REVERSAL.getValue()) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_CAN_NOT_CANCEL_REVERSAL_BOOKING,
                "Can not cancel reversal booking"
            );                        
        }
        // Check whether compound booking already has a reversal booking
        CompoundBookingQuery cbQuery = (CompoundBookingQuery)pm.newQuery(CompoundBooking.class);
        cbQuery.thereExistsReversalOf().equalTo(cb);
        List<CompoundBooking> compoundBookings = depotSegment.getCb(cbQuery);
        if(!compoundBookings.isEmpty()) {
        	CompoundBooking reversal = compoundBookings.iterator().next();
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_ALREADY_HAS_REVERSAL_BOOKING,
                "Compound booking already cancelled",
                new BasicException.Parameter("param0", reversal.getName() + " / " + reversal.getBookingDate())
            );                                    
        }
        // Create cancel compound booking
        CompoundBooking cancelCb = pm.newInstance(CompoundBooking.class);
        if(cb.getName() != null) {
            cancelCb.setName(cb.getName());
        }
        if(cb.getDescription() != null) {
            cancelCb.setDescription(cb.getDescription());
        }
        cancelCb.setBookingDate(new Date());
        cancelCb.setBookingType(BookingType.REVERSAL.getValue());
        cancelCb.setBookingStatus(BookingStatus.PENDING.getValue());
        cancelCb.setReversalOf(cb);
        depotSegment.addCb(
        	this.getUidAsString(),
        	cancelCb
        );
        // Create cancel bookings
        SingleBookingQuery singleBookingQuery = (SingleBookingQuery)pm.newQuery(SingleBooking.class);
        singleBookingQuery.thereExistsCb().equalTo(cb);
        List<SingleBooking> bookings = depotSegment.getBooking(singleBookingQuery);
        for(SingleBooking booking: bookings) {
        	SingleBooking cancelBooking = null;
        	if(booking instanceof CreditBooking) {
        		cancelBooking = pm.newInstance(DebitBooking.class);
        		((DebitBooking)cancelBooking).setQuantityDebit(((CreditBooking)booking).getQuantityCredit());
        	} else if(booking instanceof DebitBooking){
        		cancelBooking = pm.newInstance(CreditBooking.class);
        		((CreditBooking)cancelBooking).setQuantityCredit(((DebitBooking)booking).getQuantityDebit());
        	}
        	cancelBooking.setName(booking.getName());
        	cancelBooking.setDescription(booking.getDescription());
        	cancelBooking.setValueDate(booking.getValueDate());
        	cancelBooking.setBookingDate(new Date());
        	cancelBooking.setPosition(booking.getPosition());
        	this.assertOpenPosition(
                cancelBooking.getValueDate(),
                BookingType.REVERSAL.getValue(),
                cancelBooking.getPosition()
            );
            cancelBooking.setBookingType(BookingType.REVERSAL.getValue());
            cancelBooking.setBookingStatus(BookingStatus.PENDING.getValue());
            cancelBooking.setCb(cancelCb);
            depotSegment.addBooking(
            	this.getUidAsString(),
            	cancelBooking
            );
        }
        return cancelCb;
    }

    /**
     * Accept compound booking.
     * 
     * @param compoundBooking
     * @throws ServiceException
     */
    public void acceptCompoundBooking(
        CompoundBooking compoundBooking
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(compoundBooking);    	
        boolean isPending = compoundBooking.getBookingStatus() == BookingStatus.PENDING.getValue();
        if(!isPending) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status must be pending. Accept is not allowed."
            );                                                
        }
        List<String> principals = UserObjects.getPrincipalChain(pm);
        String acceptedBy = principals.isEmpty() ? "NA" : principals.get(0) + " @ " + DateTimeFormat.BASIC_UTC_FORMAT.format(new Date());
        compoundBooking.getAcceptedBy().add(acceptedBy);
    }
    
    /**
     * Finalize compound booking.
     * 
     * @param cb
     * @throws ServiceException
     */
    public void finalizeCompoundBooking(
        CompoundBooking cb
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(cb);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, cb.refGetPath().getSegment(2).toClassicRepresentation(), cb.refGetPath().getSegment(4).toClassicRepresentation());    	    	    	
        boolean isPending = cb.getBookingStatus() == BookingStatus.PENDING.getValue();
        if(!isPending) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status must be pending. Finalize is not allowed."
            );                                                
        }
        // Process bookings
        SingleBookingQuery singleBookingQuery = (SingleBookingQuery)pm.newQuery(SingleBooking.class);
        singleBookingQuery.thereExistsCb().equalTo(cb);
        List<SingleBooking> bookings = depotSegment.getBooking(singleBookingQuery);
        for(SingleBooking booking: bookings) {
        	this.assertOpenPosition(
                booking.getValueDate(),
                booking.getBookingType(),
                booking.getPosition()
            );
            booking.setBookingStatus(BookingStatus.FINAL.getValue());
        }
        cb.setBookingStatus(BookingStatus.FINAL.getValue());
    }

    /**
     * Remove compound booking.
     * 
     * @param compoundBooking
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeCompoundBooking(
        CompoundBooking cb,
        boolean preDelete
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(cb);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, cb.refGetPath().getSegment(2).toClassicRepresentation(), cb.refGetPath().getSegment(4).toClassicRepresentation());    	    	    	    	
        // isPending
        boolean isPending = cb.getBookingStatus() == BookingStatus.PENDING.getValue();
        if(!isPending) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status is not pending. Delete is not allowed."
            );                                                
        }
        // isLocked
        boolean isLocked = cb.isLocked();
        if(isLocked) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_IS_LOCKED_CAN_NOT_DELETE,
                "Compound booking is locked. Delete is not allowed."
            );                                                
        }
        // Delete bookings
        SingleBookingQuery bookingQuery = (SingleBookingQuery)pm.newQuery(SingleBooking.class);
        bookingQuery.thereExistsCb().equalTo(cb);
        List<SingleBooking> bookings = depotSegment.getBooking(bookingQuery);
        for(SingleBooking booking: bookings) {
        	booking.refDelete();
        }
        if(!preDelete) {
        	cb.refDelete();
        }
    }

    /**
     * Remove simple booking.
     * 
     * @param simpleBooking
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeSimpleBooking(
        SimpleBooking simpleBooking,
        boolean preDelete
    ) throws ServiceException {
        boolean isPending = simpleBooking.getBookingStatus() == BookingStatus.PENDING.getValue();
        if(!isPending) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status is not pending. Delete is not allowed."
            );                                                
        }
        if(!preDelete) {
        	simpleBooking.refDelete();
        }
    }

    /**
     * Remove single booking.
     * 
     * @param singleBooking
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeSingleBooking(
        SingleBooking singleBooking,
        boolean preDelete
    ) throws ServiceException {
        boolean isPending = singleBooking.getBookingStatus() == BookingStatus.PENDING.getValue();
        if(!isPending) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status is not pending. Delete is not allowed."
            );                                                
        }
        if(!preDelete) {
        	singleBooking.refDelete();
        }
    }

    /**
     * Test for bookings having a depot position which is composite of
     * the given booking target. The booking target can be a depot entity, 
     * a depot holder, a depot or a depot position.
     * 
     * @param bookingTarget
     * @return
     * @throws ServiceException
     */
    public boolean hasBookings(
        CrxObject bookingTarget
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(bookingTarget);
        org.opencrx.kernel.depot1.jmi1.Segment depotSegment = this.getDepotSegment(pm, bookingTarget.refGetPath().getSegment(2).toClassicRepresentation(), bookingTarget.refGetPath().getSegment(4).toClassicRepresentation());    	    	    	    	    	
        SingleBookingQuery bookingQuery = (SingleBookingQuery)pm.newQuery(SingleBooking.class);
        bookingQuery.thereExistsPosition().elementOf(
        	PersistenceHelper.getCandidates(
        		pm.getExtent(DepotPosition.class, true),
        		bookingTarget.refMofId() + (bookingTarget.refGetPath().size() < 13 ? "/($...)" : "")
        	)
        );
        List<SingleBooking> bookings = depotSegment.getBooking(bookingQuery);
        return !bookings.isEmpty();
    }

    /**
     * Remove depot entity. Test for existing bookings.
     * 
     * @param depotEntity
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeDepotEntity(
        DepotEntity depotEntity,
        boolean preDelete
    ) throws ServiceException {
        if(this.hasBookings(depotEntity)) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_ENTITY_HAS_BOOKINGS,
                "Depot entity has bookings."
            );                                                                        
        }
        if(!preDelete) {
        	depotEntity.refDelete();
        }
    }
    
    /**
     * Remove depot holder. Test for existing bookings.
     * 
     * @param depotHolder
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeDepotHolder(
        DepotHolder depotHolder,
        boolean preDelete
    ) throws ServiceException {
        if(this.hasBookings(depotHolder)) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_CONTRACT_HAS_BOOKINGS,
                "Depot holder has bookings."
            );                                                                        
        }
        if(!preDelete) {
        	depotHolder.refDelete();
        }
    }
    
    /**
     * Remove depot. Test for existing bookings.
     * 
     * @param depot
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeDepot(
        Depot depot,
        boolean preDelete
    ) throws ServiceException {
        if(this.hasBookings(depot)) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_DEPOT_HAS_BOOKINGS,
                "Depot has bookings."
            );                                                                        
        }
        if(!preDelete) {
        	depot.refDelete();
        }
    }
    
    /**
     * Remove depot position. Test for existing bookings.
     * 
     * @param depotPosition
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeDepotPosition(
        DepotPosition depotPosition,
        boolean preDelete
    ) throws ServiceException {
        if(this.hasBookings(depotPosition)) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.DEPOT_POSITION_HAS_BOOKINGS,
                "Depot position has bookings."
            );                                                       
        }
        if(!preDelete) {
        	depotPosition.refDelete();
        }
    }
    
    /**
     * Remove depot group.
     * 
     * @param depotGroup
     * @param preDelete
     * @throws ServiceException
     */
    protected void removeDepotGroup(
        DepotGroup depotGroup,
        boolean preDelete
    ) throws ServiceException {
        if(!preDelete) {
        	depotGroup.refDelete();
        }
    }

    /**
     * Open depot with the given depot number. The method does not test for duplicate depot numbers.
     * 
     * @param depotHolder
     * @param name
     * @param description
     * @param depotNumber
     * @param openingDate
     * @param depotType
     * @param depotGroup
     * @param errors
     * @return
     * @throws ServiceException
     */
    public Depot openDepot(
        DepotHolder depotHolder,
        String name,
        String description,
        String depotNumber,
        Date openingDate,
        DepotType depotType,
        DepotGroup depotGroup,
        List<String> errors
    ) throws ServiceException {
        if(depotNumber == null) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_DEPOT_NUMBER_REQUIRED,
                "Depot number is required."
            );                                                            
        }
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depotHolder);        
        Depot depot = pm.newInstance(Depot.class);
        if(name != null) {
            depot.setName(name);
        }
        if(description != null) {
            depot.setDescription(description);
        }
        depot.setDepotNumber(depotNumber);        
        depot.setOpeningDate(
            openingDate == null ? 
            	new Date() : 
            	openingDate
        );
        if(depotType != null) {
            depot.getDepotType().add(depotType);
        }
        if(depotGroup != null) {
            depot.setDepotGroup(depotGroup);
        }
        depot.setDefault(Boolean.FALSE);
        depot.setLocked(Boolean.FALSE);
        depot.setAllowPositionAutoCreate(Boolean.FALSE);
        depotHolder.addDepot(
        	this.getUidAsString(),
        	depot
        );
        return depot;
    }
    
    /**
     * Closes the given depot, i.e. sets the closing date to the current date.
     * 
     * @param depot
     * @param closingDate
     * @param errors
     * @throws ServiceException
     */
    public void closeDepot(
        Depot depot,
        Date closingDate,
        List<String> errors
    ) throws ServiceException {
        if(depot.getClosingDate() != null) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_DEPOT_IS_CLOSED_CAN_NOT_CLOSE,
                "Depot is closed. Can not close."
            );                                                                        
        }
        depot.setLocked(Boolean.TRUE);
        depot.setClosingDate(
            closingDate == null ? 
            	new Date() : 
            	closingDate
        );        
    }
    
    /**
     * Open a new depot position. Return existing if it already exists.
     * 
     * @param depot
     * @param positionName
     * @param positionDescription
     * @param openingDate
     * @param depotPositionQualifier
     * @param product
     * @param isLocked
     * @return
     * @throws ServiceException
     */
    public DepotPosition openDepotPosition(
        Depot depot,
        String positionName,
        String positionDescription,
        Date openingDate,
        String depotPositionQualifier,
        Product product,
        Boolean isLocked
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(depot);    	
    	DepotPosition depotPosition = null;
        if(product != null) {
            // Check whether position with given productRole already
            // exists. If yes, do not create new position.
            List<ProductDepotPosition> positions = null;
            if(depotPositionQualifier == null) {
            	ProductDepotPositionQuery depotPositionQuery = (ProductDepotPositionQuery)pm.newQuery(ProductDepotPosition.class);
            	depotPositionQuery.thereExistsProduct().equalTo(product);
            	positions = depot.getPosition(depotPositionQuery);
            } else {
                // qualifier and productRole must match
            	ProductDepotPositionQuery depotPositionQuery = (ProductDepotPositionQuery)pm.newQuery(ProductDepotPosition.class);
            	depotPositionQuery.thereExistsProduct().equalTo(product);
            	depotPositionQuery.thereExistsQualifier().equalTo(depotPositionQualifier);
            	positions = depot.getPosition(depotPositionQuery);
            }
            if(!positions.isEmpty()) {
                return positions.iterator().next();
            }
            depotPosition = pm.newInstance(ProductDepotPosition.class);
            ((ProductDepotPosition)depotPosition).setProduct(product);
        } else {
        	depotPosition = pm.newInstance(DepotPosition.class);
        }        
        // In case a depot position qualifier is specified, set name to productNumber + " #" + depotPositionQualifier.
        String name = positionName != null 
        	? positionName 
        	: product != null 
        		? product.getProductNumber() != null 
        			? product.getProductNumber()
        			: product.getName() 
        		: "N/A";
        if(depotPositionQualifier != null) {
            depotPosition.setQualifier(depotPositionQualifier);
            name += " #" + depotPositionQualifier;
        }
        depotPosition.setName(name);
        // description
        depotPosition.setDescription(
            positionDescription != null 
            	? positionDescription 
            	: product != null 
            		? product.getDescription() 
            		: ""
        );
        if(openingDate != null) {
            depotPosition.setOpeningDate(openingDate);
        }
        depotPosition.setLocked(isLocked);
        depot.addPosition(
        	this.getUidAsString(),
        	depotPosition
        );
        return depotPosition;
    }
    
    /**
     * Close depot position, i.e. set closing date.
     * 
     * @param depotPosition
     * @param closingDate
     * @param errors
     * @throws ServiceException
     */
    public void closeDepotPosition(       
        DepotPosition depotPosition,
        Date closingDate,
        List<String> errors
    ) throws ServiceException {
        if(depotPosition.getClosingDate() != null) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_DEPOT_POSITION_IS_CLOSED_CAN_NOT_CLOSE,
                "Depot position is closed. Can not close."
            );                                                                        
        }
        depotPosition.setLocked(Boolean.TRUE);
        depotPosition.setClosingDate(
            closingDate == null 
            	? new Date() 
            	: closingDate
        );        
    }
    
    /**
     * Set the lock flag for the given compound booking to true.
     * 
     * @param cb
     * @param lockingReason
     * @throws ServiceException
     */
    public void lockCompoundBooking(
        CompoundBooking cb,
        short lockingReason
    ) throws ServiceException {
        boolean isPending = cb.getBookingStatus() == BookingStatus.PENDING.getValue();
        if(!isPending) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status is not pending. Locking is not allowed."
            );                                                
        }
        cb.setLocked(Boolean.TRUE);
        cb.setLockingReason(new Short(lockingReason));
        cb.setLockModifiedAt(new Date());
    }
    
    /**
     * Set the lock flag of the given compound booking to false.
     * 
     * @param cb
     * @throws ServiceException
     */
    public void unlockCompoundBooking(
        CompoundBooking cb
    ) throws ServiceException {
        boolean isPending = cb.getBookingStatus() == BookingStatus.PENDING.getValue();
        if(!isPending) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.BOOKING_STATUS_MUST_BE_PENDING,
                "Booking status is not pending. Unlocking is not allowed."
            );                                                
        }
        cb.setLocked(Boolean.FALSE);
        cb.setLockingReason(new Short((short)0));
        cb.setLockModifiedAt(new Date());
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
		if(object instanceof CompoundBooking) {
			this.removeCompoundBooking((CompoundBooking)object, preDelete);
		} else if(object instanceof DepotEntity) {
			this.removeDepotEntity((DepotEntity)object, preDelete);
		} else if(object instanceof DepotGroup) {
			this.removeDepotGroup((DepotGroup)object, preDelete);
		} else if(object instanceof DepotHolder) {
			this.removeDepotHolder((DepotHolder)object, preDelete);
		} else if(object instanceof Depot) {
			this.removeDepot((Depot)object, preDelete);
		} else if(object instanceof DepotPosition) {
			this.removeDepotPosition((DepotPosition)object, preDelete);
		} else if(object instanceof SimpleBooking) {
			this.removeSimpleBooking((SimpleBooking)object, preDelete);
		} else if(object instanceof SingleBooking) {
			this.removeSingleBooking((SingleBooking)object, preDelete);
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
		if(object instanceof PhoneNumber) {
			Addresses.getInstance().updatePhoneNumber((PhoneNumber)object);
		}
	}

	/**
	 * BookingType
	 *
	 */
	public enum BookingType {
		STANDARD((short)10),
		CLOSING((short)20),
		REVERSAL((short)30);
		
		private BookingType(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
		private final short value;
	}

	/**
	 * BookingStatus
	 *
	 */
	public enum BookingStatus {
		PENDING((short)1),
		FINAL((short)2);
		
		private BookingStatus(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
		private final short value;
	}

	/**
	 * DepotUsage
	 *
	 */
	public enum DepotUsage {
		GOODS_ISSUE((short)1),
		GOODS_RETURN((short)2),
		GOODS_DELIVERY((short)3),
		WORK_EFFORT((short)10);
		
		private DepotUsage(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
		private final short value;
	}

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------    
    
}
