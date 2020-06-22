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
import org.opencrx.kernel.contract1.cci2.InvoiceQuery;
import org.opencrx.kernel.contract1.jmi1.Contract1Package;
import org.opencrx.kernel.contract1.jmi1.Invoice;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.exception.ServiceException;

public class SalesOrderImpl
	<S extends org.opencrx.kernel.contract1.jmi1.SalesOrder,N extends org.opencrx.kernel.contract1.cci2.SalesOrder,C extends Void>
	extends SalesContractImpl<S,N,C>
	implements StoreCallback, DeleteCallback {

    //-----------------------------------------------------------------------
    public SalesOrderImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.openmdx.base.jmi1.Void markAsClosed(
        org.opencrx.kernel.contract1.jmi1.SalesOrderMarkAsClosedParams params
    ) {
        try {        
            Contracts.getInstance().markAsClosed(
                this.sameObject(),
                params.getSalesOrderState()
            );
            return super.newVoid();
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                    
    }
    
    //-----------------------------------------------------------------------
    public List<org.opencrx.kernel.contract1.jmi1.Invoice> getInvoice(
    ) {
		org.opencrx.kernel.contract1.jmi1.Segment contractSegment =
			(org.opencrx.kernel.contract1.jmi1.Segment)this.sameManager().getObjectById(
				this.sameObject().refGetPath().getPrefix(5)
			);
		InvoiceQuery query = (InvoiceQuery)this.sameManager().newQuery(Invoice.class);
		query.thereExistsOrigin().equalTo(this.sameObject());
		List<Invoice> invoices = contractSegment.getInvoice(query);
		return invoices;
    }
        
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.contract1.jmi1.SalesOrderCreateInvoiceResult createInvoice(
    ) {
        try {
            Invoice invoice = Contracts.getInstance().createInvoice(
                this.sameObject()
            );
            return this.<Contract1Package>samePackage().createSalesOrderCreateInvoiceResult(
                invoice
            );        
        }
        catch(ServiceException e) {
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
