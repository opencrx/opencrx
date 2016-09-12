/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: AbstractContractFieldMapper
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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.opencrx.kernel.contract1.jmi1.AbstractContract;

public class AbstractContractFieldMapper {

    public List<String> getStateHistory(
        AbstractContract contract
    ) {
        return contract.getUserString4();
    }
    
    public String getBillingPartner(
        AbstractContract contract
    ) {
        return contract.getUserString0();
    }

    public void setBillingPartner(
        AbstractContract contract,
        String newValue
    ) {
        contract.setUserString0(newValue);
    }

    public List<String> getBillingPartnerRegistrationId(
        AbstractContract contract
    ) {
        String registrationIds = contract.getUserString3();
        if(registrationIds == null) {
            return Collections.emptyList();
        }
        else {
            return Arrays.asList(registrationIds.split(","));
        }
    }

    public void setBillingPartnerRegistrationId(
        AbstractContract contract,
        List<String> newValue
    ) {
        StringBuilder registrationIds = new StringBuilder();
        String separator = "";
        for(String registrationId: newValue) {
            registrationIds.append(separator).append(registrationId);
            separator = ",";
        }
        contract.setUserString3(registrationIds.toString());
    }
    
}
