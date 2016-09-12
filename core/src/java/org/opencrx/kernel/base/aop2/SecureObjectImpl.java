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
package org.opencrx.kernel.base.aop2;

import java.util.ArrayList;
import java.util.List;

import javax.jdo.JDOUserException;
import javax.jdo.listener.StoreCallback;

import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.SecureObject;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class SecureObjectImpl
	<S extends org.opencrx.kernel.base.jmi1.SecureObject,N extends org.opencrx.kernel.base.cci2.SecureObject,C extends Void>
	extends AbstractObject<S,N,C>
	implements StoreCallback {
	
    //-----------------------------------------------------------------------
    public SecureObjectImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.base.jmi1.ModifySecureObjectResult setOwningUser(
        org.opencrx.kernel.base.jmi1.SetOwningUserParams params
    ) {
        try {
            List<String> report = new ArrayList<String>();
            SecureObject.getInstance().setOwningUser(
            	this.sameObject(),
            	params.getUser(),
            	params.getMode(),
            	report
            );
            return Structures.create(
            	org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.class, 
            	Datatypes.member(org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.Member.statusMessage, Base.getInstance().analyseReport(report))
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }         
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.base.jmi1.ModifySecureObjectResult addOwningGroup(
        org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams params
    ) {
        try {
            List<String> report = new ArrayList<String>();
            SecureObject.getInstance().addOwningGroup(
            	this.sameObject(),
            	params.getGroup(),
            	params.getMode(),
            	report
            );
            return Structures.create(
            	org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.class, 
            	Datatypes.member(org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.Member.statusMessage, Base.getInstance().analyseReport(report))
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.base.jmi1.ModifySecureObjectResult removeOwningGroup(
        org.opencrx.kernel.base.jmi1.ModifyOwningGroupParams params
    ) {
        try {
            List<String> report = new ArrayList<String>();
            SecureObject.getInstance().removeOwningGroup(
            	this.sameObject(),
            	params.getGroup(),
            	params.getMode(),
            	report
            );
            return Structures.create(
            	org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.class, 
            	Datatypes.member(org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.Member.statusMessage, Base.getInstance().analyseReport(report))
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.base.jmi1.ModifySecureObjectResult removeAllOwningGroup(
        org.opencrx.kernel.base.jmi1.RemoveAllOwningGroupParams params
    ) {
        try {
            List<String> report = new ArrayList<String>();
            SecureObject.getInstance().removeAllOwningGroup(
            	this.sameObject(),
            	params.getMode(),
            	report
            );
            return Structures.create(
            	org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.class, 
            	Datatypes.member(org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.Member.statusMessage, Base.getInstance().analyseReport(report))
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }   
    
    //-----------------------------------------------------------------------
    public org.opencrx.kernel.base.jmi1.ModifySecureObjectResult setAccessLevel(
        org.opencrx.kernel.base.jmi1.SetAccessLevelParams params
    ) {
        try {
            List<String> report = new ArrayList<String>();
            SecureObject.getInstance().setAccessLevel(
            	this.sameObject(),
                params.getAccessLevelBrowse(),
                params.getAccessLevelUpdate(),
                params.getAccessLevelDelete(),
                params.getMode(),
                report
            );
            return Structures.create(
            	org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.class, 
            	Datatypes.member(org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.Member.statusMessage, Base.getInstance().analyseReport(report))
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }            
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.base.jmi1.ModifySecureObjectResult replaceOwningGroup(
        org.opencrx.kernel.base.jmi1.ModifyOwningGroupsParams params
    ) {
        try {
            List<String> report = new ArrayList<String>();
            List<org.opencrx.security.realm1.jmi1.PrincipalGroup> groups = params.getGroup();            
            SecureObject.getInstance().replaceOwningGroups(
            	this.sameObject(),
            	groups,
            	params.getMode(),
            	report
            );
            return Structures.create(
            	org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.class, 
            	Datatypes.member(org.opencrx.kernel.base.jmi1.ModifySecureObjectResult.Member.statusMessage, Base.getInstance().analyseReport(report))
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                   
    }

    //-----------------------------------------------------------------------
	@Override
    public void jdoPreStore(
    ) {
    	try {
    		SecureObject.getInstance().updateSecureObject(
    			this.sameObject() 
    		);
    		super.jdoPreStore();
    	}
    	catch(ServiceException e) {
    		throw new JDOUserException(
    			"jdoPreStore failed",
    			e,
    			this.sameObject()
    		);
    	}
    }
	
}
