/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: DeliveryInformationFieldMapper
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

import java.util.Date;

import org.opencrx.kernel.contract1.jmi1.DeliveryInformation;

public class DeliveryInformationFieldMapper {

    public void setDeliveryStatus(
    	DeliveryInformation deliveryInformation,
        Integer newValue
    ) {
        deliveryInformation.setUserCode0(
        	newValue == null ?
        		(short)0 :
        			newValue.shortValue()
        );
    }
    
    public Integer getDeliveryStatus(
    	DeliveryInformation deliveryInformation
    ) {
    	return deliveryInformation.getUserCode0() == null ? 
    		null :
    			new Integer(deliveryInformation.getUserCode0());
    }
    
    public void setDeliveryStatusDescription(
    	DeliveryInformation deliveryInformation,
    	String newValue
    ) {
    	deliveryInformation.setUserString0(newValue);
    }
    
    public String getDeliveryStatusDescription(
    	DeliveryInformation deliveryInformation
    ) {
    	return deliveryInformation.getUserString0(); 
    }
    
    public void setProductAssembledAt(
    	DeliveryInformation deliveryInformation,
    	Date newValue
    ) {
    	deliveryInformation.setUserDateTime0(newValue);
    }
    
    public Date getProductAssembledAt(
    	DeliveryInformation deliveryInformation
    ) {
    	return deliveryInformation.getUserDateTime0();
    }
    	
}
