/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: InventoryLevelImpl
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
package org.opencrx.kernel.depot1.aop2;

import javax.jdo.JDOUserException;
import javax.jdo.listener.DeleteCallback;
import javax.jdo.listener.StoreCallback;

import org.opencrx.kernel.backend.Depots;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;

/**
 * InventoryLevelImpl
 * 
 */
public class InventoryLevelImpl
	<S extends org.opencrx.kernel.depot1.jmi1.InventoryLevel,N extends org.opencrx.kernel.depot1.cci2.InventoryLevel,C extends Void>
	extends AbstractObject<S,N,C>
	implements DeleteCallback, StoreCallback {

    /**
     * Constructor.
     * 
     * @param same
     * @param next
     */
    public InventoryLevelImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Accept inventory level.
     * 
     * @return
     */
    public org.openmdx.base.jmi1.Void acceptInventoryLevel(
    ) {
        try {
            Depots.getInstance().acceptInventoryLevel(
                this.sameObject()
            );
            return super.newVoid();            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
        
    /**
     * Finalize inventory level.
     * 
     * @return
     */
    public org.openmdx.base.jmi1.Void finalizeInventoryLevel(
    ) {
        try {
            Depots.getInstance().finalizeInventoryLevel(
                this.sameObject()
            );
            return super.newVoid();            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
        
    /**
     * Lock inventory level.
     * 
     * @param params
     * @return
     */
    public org.openmdx.base.jmi1.Void lockInventoryLevel(
        org.opencrx.kernel.depot1.jmi1.LockInventoryLevelParams params
    ) {
        try {
            Depots.getInstance().lockInventoryLevel(
                this.sameObject(),
                params.getLockingReason()
            );
            return super.newVoid();            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
    
    /**
     * Unlock inventory level.
     * 
     * @return
     */
    public org.openmdx.base.jmi1.Void unlockInventoryLevel(
    ) {
        try {
            Depots.getInstance().unlockInventoryLevel(
                this.sameObject()
            );
            return super.newVoid();            
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
    		Depots.getInstance().preStore(
    			(CrxObject)this.sameObject() 
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
