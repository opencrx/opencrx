/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ActivityCreatorImpl
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
package org.opencrx.kernel.activity1.aop2;

import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.ICalendar.ICalClass;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class ActivityCreatorImpl 
	<S extends org.opencrx.kernel.activity1.jmi1.ActivityCreator,N extends org.opencrx.kernel.activity1.cci2.ActivityCreator,C extends Void>
	extends AbstractObject<S,N,C> {

    //-----------------------------------------------------------------------
    public ActivityCreatorImpl(
        S same,
        N next
    ) {
        super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.activity1.jmi1.NewActivityResult newActivity(
        org.opencrx.kernel.activity1.jmi1.NewActivityParams params
    ) {
        try {        
            Activity activity = Activities.getInstance().newActivity(
                this.sameObject(),
                params.getName(),
                params.getDescription(),
                params.getDetailedDescription(),
                params.getScheduledStart(),
                params.getScheduledEnd(),
                params.getDueBy(),
                params.getPriority(),
                params.getIcalType(),
                ICalClass.NA,
                params.getReportingContact(),
                params.getCreationContext()
            );
            return Structures.create(
            	org.opencrx.kernel.activity1.jmi1.NewActivityResult.class, 
            	Datatypes.member(org.opencrx.kernel.activity1.jmi1.NewActivityResult.Member.activity, activity)
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }       
    }

}
