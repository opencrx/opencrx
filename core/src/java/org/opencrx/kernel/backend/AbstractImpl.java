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
package org.opencrx.kernel.backend;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;

public abstract class AbstractImpl {

	private static ConcurrentMap<String,Object> currentImpls = new ConcurrentHashMap<String,Object>();
	private static Set<String> registeredImpls = new HashSet<String>();
	
	/**
	 * Get backend impl of given type.
	 * 
	 * @param backendClass
	 * @return
	 * @throws ServiceException
	 */
	@SuppressWarnings("unchecked")
    protected static <B extends AbstractImpl> B getInstance(
		Class<B> backendClass
	) throws ServiceException {
		B impl = (B)currentImpls.get(backendClass.getSimpleName());
		if(impl == null) {
            throw new ServiceException(
                BasicException.Code.DEFAULT_DOMAIN,
                BasicException.Code.NOT_FOUND,
                "Requested backend class is not registered",
                new BasicException.Parameter("backend.id", backendClass.getSimpleName())
            );            
		}
		return impl;
	}

	/**
	 * Register backend impl.
	 * 
	 * @param impl
	 */
	protected static void registerImpl(
		AbstractImpl impl
	) {
		if(!registeredImpls.contains(impl.getClass().getName())) {
			String key = impl.getClass().getSimpleName();
			try {
				SysLog.info("Registering impl", impl.getClass().getName());
				currentImpls.put(
					key,
					impl
				);
				registeredImpls.add(
					impl.getClass().getName()
				);
			} catch(Exception e) {
				ServiceException e0 = new ServiceException(
					e,
	                BasicException.Code.DEFAULT_DOMAIN,
	                BasicException.Code.ACTIVATION_FAILURE,
	                "Backend class can not be registered",
	                new BasicException.Parameter("backend.id", key)
				);
				e0.log();
			}
		} else {
			SysLog.detail("Impl already registered. Ignoring.", impl.getClass().getName());			
		}
	}
	
    /**
     * Helper to get UID.
     * 
     * @return
     */
    public String getUidAsString(
    ) {
        return UUIDConversion.toUID(UUIDs.newUUID());        
    }

    /**
     * Pre-delete callback. This method is called before can object is deleted. Override
     * this method for custom-specific behaviour.
     * 
     * @param object
     * @param preDelete
     */
    public void preDelete(
        RefObject_1_0 object,
        boolean preDelete
    ) throws ServiceException {
    	// 
    }

    /**
     * Pre-store callback. This method is called before an object is made persistent. Override
     * this method for custom-specific behaviour.
     * 
     * @param object
     */
    public void preStore(
    	RefObject_1_0 object
    ) throws ServiceException {    	
    }

	//-------------------------------------------------------------------------
    // Members
	//-------------------------------------------------------------------------
    
}
