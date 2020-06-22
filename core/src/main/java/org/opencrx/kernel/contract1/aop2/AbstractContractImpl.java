/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AbstractContractImpl
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
package org.opencrx.kernel.contract1.aop2;

import javax.jdo.JDOUserException;

import org.opencrx.kernel.backend.Contracts;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;

/**
 * AbstractContractImpl
 *
 * @param <S>
 * @param <N>
 * @param <C>
 */
public class AbstractContractImpl
	<S extends org.opencrx.kernel.contract1.jmi1.AbstractContract,N extends org.opencrx.kernel.contract1.cci2.AbstractContract,C extends Void>
	extends AbstractObject<S,N,C> {

    /**
     * Constructor.
     * 
     * @param same
     * @param next
     */
    public AbstractContractImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Set contract state.
     * 
     * @param contractState
     */
    public void setContractState(
        short contractState
    ) {
    	try {
    		Contracts.getInstance().updateContractState(
    			this.sameObject(),
    			contractState
    		);    		
    		this.nextObject().setContractState(contractState);
    	}
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }

    /**
     * Re-apply activity creator.
     * 
     * @param params
     * @return
     */
    public org.openmdx.base.jmi1.Void reapplyContractCreator(
        org.opencrx.kernel.contract1.jmi1.ReapplyContractCreatorParams params
    ) {
        try {
            Contracts.getInstance().reapplyContractCreator(
                this.sameObject(),
                params.getContractCreator()
            );
            return this.newVoid();
        }
        catch(ServiceException e) {
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

}
