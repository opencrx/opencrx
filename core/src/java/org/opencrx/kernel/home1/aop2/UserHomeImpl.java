/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: UserHomeImpl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2015, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.home1.aop2;

import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.home1.jmi1.ObjectFinder;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * UserHomeImpl
 *
 * @param <S>
 * @param <N>
 * @param <C>
 */
public class UserHomeImpl
	<S extends org.opencrx.kernel.home1.jmi1.UserHome,N extends org.opencrx.kernel.home1.cci2.UserHome,C extends Void>
	extends AbstractObject<S,N,C> {

    /**
     * Constructor.
     * 
     * @param same
     * @param next
     */
    public UserHomeImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Refresh items.
     * 
     * @return
     */
    public org.openmdx.base.jmi1.Void refreshItems(
    ) {
        try {
            UserHomes.getInstance().refreshItems(
                this.sameObject()
            );
            return super.newVoid();            
        }
        catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }
    
    /**
     * Request password reset.
     * 
     * @return
     */
    public org.openmdx.base.jmi1.Void requestPasswordReset(
    ) {
        try {
            UserHomes.getInstance().requestPasswordReset(
                this.sameObject()
            );
            return super.newVoid();
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }    	
    }
    
    /**
     * Basic full-text search.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.home1.jmi1.SearchResult searchBasic(
        org.opencrx.kernel.home1.jmi1.SearchBasicParams params
    ) {
        try {
            ObjectFinder objectFinder = UserHomes.getInstance().searchBasic(
                this.sameObject(),
                params.getSearchExpression()
            );
            return Structures.create(
            	org.opencrx.kernel.home1.jmi1.SearchResult.class, 
            	Datatypes.member(org.opencrx.kernel.home1.jmi1.SearchResult.Member.objectFinder, objectFinder)                	
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }              
    }
    
    /**
     * Advanced full-text search.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.home1.jmi1.SearchResult searchAdvanced(
        org.opencrx.kernel.home1.jmi1.SearchAdvancedParams params
    ) {
        try {
            ObjectFinder objectFinder = UserHomes.getInstance().searchAdvanced(
                this.sameObject(),
                params.getAllWords(),
                params.getAtLeastOneOfTheWords(),
                params.getWithoutWords()
            );
            return Structures.create(
            	org.opencrx.kernel.home1.jmi1.SearchResult.class, 
            	Datatypes.member(org.opencrx.kernel.home1.jmi1.SearchResult.Member.objectFinder, objectFinder)                	
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }              
    }

    /**
     * Change password.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.home1.jmi1.ChangePasswordResult changePassword(
        org.opencrx.kernel.home1.jmi1.ChangePasswordParams params
    ) {
    	try {
	        short status = UserHomes.getInstance().changePassword(
	            this.sameObject(),
	            params.getOldPassword(),
	            params.getNewPassword(),
	            params.getNewPasswordVerification()
	        );
            return Structures.create(
            	org.opencrx.kernel.home1.jmi1.ChangePasswordResult.class, 
            	Datatypes.member(org.opencrx.kernel.home1.jmi1.ChangePasswordResult.Member.status, status)                	
            );	        
    	} catch(Exception e) {
    		throw new JmiServiceException(e);
    	}
    }
            
}
