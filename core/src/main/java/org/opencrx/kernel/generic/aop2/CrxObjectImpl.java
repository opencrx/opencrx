/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CrxObjectImpl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.generic.aop2;

import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.generic.jmi1.EnableDisableCrxObjectParams;
import org.opencrx.kernel.generic.jmi1.EnableDisableCrxObjectResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;

/**
 * CrxObjectImpl
 *
 * @param <S>
 * @param <N>
 * @param <C>
 */
public class CrxObjectImpl
	<S extends org.opencrx.kernel.generic.jmi1.CrxObject,N extends org.opencrx.kernel.generic.cci2.CrxObject,C extends Void>
	extends AbstractObject<S,N,C> {

    /**
     * Constructor.
     * 
     * @param same
     * @param next
     */
    public CrxObjectImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Disable CrxObject.
     * 
     * @param in
     * @return
     */
    public EnableDisableCrxObjectResult disableCrxObject(
    	EnableDisableCrxObjectParams in
    ) {
    	try {
	    	return Base.getInstance().disableCrxObject(
	    		this.sameObject(), 
	    		in.getMode(), 
	    		in.getReason()
	    	);
    	} catch(ServiceException e) {
    		throw new JmiServiceException(e);
    	}
    }

    /**
     * Enable CrxObject.
     * 
     * @param in
     * @return
     */
    public EnableDisableCrxObjectResult enableCrxObject(
    	EnableDisableCrxObjectParams in
    ) {
    	try {
    		return Base.getInstance().enableCrxObject(
	    		this.sameObject(), 
	    		in.getMode(), 
	    		in.getReason()
    		);
    	} catch(ServiceException e) {
    		throw new JmiServiceException(e);
    	}
    }

}
