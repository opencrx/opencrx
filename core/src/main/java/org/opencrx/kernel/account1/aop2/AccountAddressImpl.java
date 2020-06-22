/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AccountAddressImpl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.account1.aop2;

import javax.jdo.JDOUserException;
import javax.jdo.listener.DeleteCallback;
import javax.jdo.listener.StoreCallback;

import org.opencrx.kernel.backend.Accounts;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * AccountAddressImpl
 *
 * @param <S>
 * @param <N>
 * @param <C>
 */
public class AccountAddressImpl
	<S extends org.opencrx.kernel.account1.jmi1.AccountAddress,N extends org.opencrx.kernel.account1.cci2.AccountAddress,C extends Void>
	extends AbstractObject<S,N,C>
	implements StoreCallback, DeleteCallback {

    /**
     * Constructor.
     * 
     * @param same
     * @param next
     */
    public AccountAddressImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }
 
    /**
     * Move address to account.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.account1.jmi1.MoveAddressToAccountResult moveAddressToAccount(
        org.opencrx.kernel.account1.jmi1.MoveAddressToAccountParams params
    ) {
        try {
            int count = Accounts.getInstance().moveAddressToAccount(
                this.sameObject(),
                params.getTargetAccount(),
                params.getUpdateRelationshipsSince(),
                params.getUpdateRelationshipsBefore()
            );
            return Structures.create(
            	org.opencrx.kernel.account1.jmi1.MoveAddressToAccountResult.class, 
            	Datatypes.member(org.opencrx.kernel.account1.jmi1.MoveAddressToAccountResult.Member.count, count)
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
    
    /**
     * Move address.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.account1.jmi1.MoveAddressResult moveAddress(
        org.opencrx.kernel.account1.jmi1.MoveAddressParams params
    ) {
        try {
            int count = Accounts.getInstance().moveAddress(
                this.sameObject(),
                params.getTargetAddress(),
                params.getUpdateRelationshipsSince(),
                params.getUpdateRelationshipsBefore()
            );
            return Structures.create(
            	org.opencrx.kernel.account1.jmi1.MoveAddressResult.class, 
            	Datatypes.member(org.opencrx.kernel.account1.jmi1.MoveAddressResult.Member.count, count)
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }

    /**
     * Check whether address qualifies for auto-update.
     * 
     * @return
     */
    public org.opencrx.kernel.account1.jmi1.CheckForAutoUpdateResult checkForAutoUpdate(
    ) {
    	try {
	    	return Accounts.getInstance().checkForAutoUpdate(
	    		this.sameObject()
	    	);
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
			Accounts.getInstance().preStore(
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
    		Accounts.getInstance().preDelete(
    			this.sameObject(), 
    			true // preDelete
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
