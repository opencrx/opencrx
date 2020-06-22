/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SegmentImpl
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
package org.opencrx.kernel.home1.aop2;

import java.util.ArrayList;
import java.util.List;

import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.backend.SecureObject;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * SegmentImpl
 *
 * @param <S>
 * @param <N>
 * @param <C>
 */
public class SegmentImpl
	<S extends org.opencrx.kernel.home1.jmi1.Segment,N extends org.opencrx.kernel.home1.cci2.Segment,C extends Void>
	extends AbstractObject<S,N,C> {

    /**
     * Constructor.
     * 
     * @param same
     * @param next
     */
    public SegmentImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Create user.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.home1.jmi1.CreateUserResult createUser(
        org.opencrx.kernel.home1.jmi1.CreateUserParams params
    ) {
    	try {
	        String principalName = params.getPrincipalName();
	        if(principalName != null) {
	        	principalName = principalName.trim();
	        }
	        Contact contact = params.getContact();
	        org.opencrx.security.realm1.jmi1.PrincipalGroup primaryUserGroup = params.getPrimaryUserGroup();
	        String initialPassword = params.getInitialPassword();
	        String initialPasswordVerification = params.getInitialPasswordVerification();
	        org.openmdx.security.realm1.jmi1.Realm realm = (org.openmdx.security.realm1.jmi1.Realm)this.sameManager().getObjectById(
	        	SecureObject.getRealmIdentity(
	        		this.sameObject().refGetPath().get(2), 
	        		this.sameObject().refGetPath().get(4)
	        	)
	        );
	        List<String> errors = new ArrayList<String>();
	        UserHome userHome = UserHomes.getInstance().createUserHome(
	            realm,
	            contact,
	            primaryUserGroup,
	            principalName,
	            null,
	            false,
	            initialPassword,
	            initialPasswordVerification,
	            null, // eMailAddress
	            null, // timezone
	            errors
	        );
	        if((userHome == null) || !errors.isEmpty()) {
	        	String statusMessage = "";
	        	for(String error: errors) {
	        		statusMessage += statusMessage.isEmpty() ? "" : "\n";
	        		statusMessage += error;
	        	}
	            return Structures.create(
	            	org.opencrx.kernel.home1.jmi1.CreateUserResult.class, 
	            	Datatypes.member(org.opencrx.kernel.home1.jmi1.CreateUserResult.Member.createdUserHome, null),
	            	Datatypes.member(org.opencrx.kernel.home1.jmi1.CreateUserResult.Member.status, (short)1),               	
	            	Datatypes.member(org.opencrx.kernel.home1.jmi1.CreateUserResult.Member.statusMessage, statusMessage)                	
	            );	        	
	        } else {
	            return Structures.create(
	            	org.opencrx.kernel.home1.jmi1.CreateUserResult.class, 
	            	Datatypes.member(org.opencrx.kernel.home1.jmi1.CreateUserResult.Member.createdUserHome, userHome),
	            	Datatypes.member(org.opencrx.kernel.home1.jmi1.CreateUserResult.Member.status, (short)0),               	
	            	Datatypes.member(org.opencrx.kernel.home1.jmi1.CreateUserResult.Member.statusMessage, null)                	
	            );	        	
	        }
    	} catch(Exception e) {
    		throw new JmiServiceException(e);
    	}
    }
    
    /**
     * Import users.
     * 
     * @param params
     * @return
     */
    public org.opencrx.kernel.home1.jmi1.ImportUsersResult importUsers(
        org.opencrx.kernel.home1.jmi1.ImportUsersParams params
    ) {
        try {
            String statusMessage = UserHomes.getInstance().importUsers(
                this.sameObject(),
                params.getItem()
            );          
            return Structures.create(
            	org.opencrx.kernel.home1.jmi1.ImportUsersResult.class, 
            	Datatypes.member(org.opencrx.kernel.home1.jmi1.ImportUsersResult.Member.statusMessage, statusMessage)                	
            );            
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                
    }
    
}
