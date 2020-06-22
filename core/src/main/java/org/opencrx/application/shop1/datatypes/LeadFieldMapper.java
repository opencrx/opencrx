/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: LeadFieldMapper
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2009, CRIXP Corp., Switzerland
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
package org.opencrx.application.shop1.datatypes;

import org.opencrx.kernel.contract1.jmi1.Lead;


public class LeadFieldMapper {

    public String getContractNumber(
        Lead customerContract
    ) {
        return customerContract.getContractNumber();
    }
    
    public Boolean acceptedLegal(
        Lead customerContract
    ) {
        return customerContract.isUserBoolean0();
    }
    
    public Boolean acceptedMarketing(
        Lead customerContract
    ) {
        return customerContract.isUserBoolean1();        
    }
    
    public Boolean acceptedPrivateDataForwarding(
        Lead customerContract
    ) {
        return customerContract.isUserBoolean2();        
    }
    
    public Boolean noBilling(
        Lead customerContract
    ) {
        return customerContract.isUserBoolean3();        
    }
                    
    public String getReferrer(
        Lead customerContract
    ) {
        return customerContract.getUserString2();        
    }

    public Integer getContactSource(
        Lead customerContract
    ) {
        return DatatypeMappers.toInteger(customerContract.getUserCode0());        
    }
        
    public String getSalesTaxType(
        Lead customerContract
    ) {
        return customerContract.getUserString1();        
    }

    public Integer getContractCurrency(
        Lead customerContract
    ) {
        return DatatypeMappers.toInteger(customerContract.getContractCurrency());        
    }
            
    public void setContractNumber(
        Lead customerContract,
        String newValue
    ) {
        customerContract.setContractNumber(newValue);
        customerContract.setName(newValue);
    }
        
    public void setAcceptedLegal(
        Lead customerContract,
        Boolean newValue
    ) {
        customerContract.setUserBoolean0(newValue);
    }
    
    public void setAcceptedMarketing(
        Lead customerContract,
        Boolean newValue
    ) {
        customerContract.setUserBoolean1(newValue);        
    }
    
    public void setAcceptedPrivateDataForwarding(
        Lead customerContract,
        Boolean newValue
    ) {
        customerContract.setUserBoolean2(newValue);        
    }
    
    public void setReferrer(
        Lead customerContract,
        String newValue
    ) {
        customerContract.setUserString2(newValue);        
    }

    public void setContactSource(
        Lead customerContract,
        Integer newValue
    ) {
        customerContract.setUserCode0(DatatypeMappers.toShort(newValue));        
    }
    
    public void setSalesTaxType(
        Lead customerContract,
        String newValue
    ) {
        customerContract.setUserString1(newValue);        
    }
    
    public void setContractCurrency(
        Lead customerContract,
        Integer newValue
    ) {
        customerContract.setContractCurrency(DatatypeMappers.toShort(newValue));        
    }

    public void setNoBilling(
        Lead customerContract,
        Boolean newValue
    ) {
        customerContract.setUserBoolean3(newValue);        
    }
            
}
