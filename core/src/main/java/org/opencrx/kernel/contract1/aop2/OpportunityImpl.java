/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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

import java.util.List;

import javax.jdo.listener.DeleteCallback;
import javax.jdo.listener.StoreCallback;

import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.contract1.cci2.QuoteQuery;
import org.opencrx.kernel.contract1.jmi1.Quote;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class OpportunityImpl
	<S extends org.opencrx.kernel.contract1.jmi1.Opportunity,N extends org.opencrx.kernel.contract1.cci2.Opportunity,C extends Void>
	extends SalesContractImpl<S,N,C>
	implements StoreCallback, DeleteCallback {

    //-----------------------------------------------------------------------
    public OpportunityImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public List<org.opencrx.kernel.contract1.jmi1.Quote> getQuote(
    ) {
		org.opencrx.kernel.contract1.jmi1.Segment contractSegment =
			(org.opencrx.kernel.contract1.jmi1.Segment)this.sameManager().getObjectById(
				this.sameObject().refGetPath().getPrefix(5)
			);
		QuoteQuery query = (QuoteQuery)this.sameManager().newQuery(Quote.class);
		query.thereExistsOrigin().equalTo(this.sameObject());
		List<Quote> quotes = contractSegment.getQuote(query);
		return quotes;
    }
        
    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void markAsClosed(
        org.opencrx.kernel.contract1.jmi1.OpportunityMarkAsClosedParams params
    ) {
        try {        
            Contracts.getInstance().markAsClosed(
                this.sameObject(),
                params.getOpportunityState()
            );
            return super.newVoid();
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                    
    }
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.contract1.jmi1.OpportunityCreateQuoteResult createQuote(
    ) {
        try {
            Quote quote = Contracts.getInstance().createQuote(
                this.sameObject()
            );
            return Structures.create(
            	org.opencrx.kernel.contract1.jmi1.OpportunityCreateQuoteResult.class, 
            	Datatypes.member(org.opencrx.kernel.contract1.jmi1.OpportunityCreateQuoteResult.Member.quote, quote)
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }

    //-----------------------------------------------------------------------
	@Override
    public void jdoPreStore(
    ) {
	    super.jdoPreStore();
    }

    //-----------------------------------------------------------------------
	@Override
    public void jdoPreDelete(
    ) {
	    super.jdoPreDelete();
    }
		
}
