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
package org.opencrx.kernel.portal;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jmi.reflect.RefObject;

import org.opencrx.kernel.activity1.cci2.ActivityQuery;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.contract1.jmi1.AbstractContract;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DefaultDataBinding;

/**
 * ContractHasAssignedActivitiesDataBinding
 *
 */
public class ContractHasAssignedActivitiesDataBinding extends DefaultDataBinding {

	/**
	 * Constructor.
	 */
	public ContractHasAssignedActivitiesDataBinding(
	) {		
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.DefaultDataBinding#getValue(javax.jmi.reflect.RefObject, java.lang.String, org.openmdx.portal.servlet.ApplicationContext)
	 */
    @Override
    public Object getValue(
    	RefObject object, 
    	String qualifiedFeatureName, 
    	ApplicationContext app
    ) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(object);
    	if(object instanceof AbstractContract) {
    		AbstractContract contract = (AbstractContract)object;
    		org.opencrx.kernel.activity1.jmi1.Segment activitySegment = 
    			(org.opencrx.kernel.activity1.jmi1.Segment)pm.getObjectById(
    				new Path("xri://@openmdx*org.opencrx.kernel.activity1").getDescendant("provider", contract.refGetPath().get(2), "segment", contract.refGetPath().get(4))
    			);
    		ActivityQuery activityQuery = (ActivityQuery)pm.newQuery(Activity.class);
    		activityQuery.thereExistsContract().equalTo(contract);
    		// Return candidates and query. The query is performed on the
    		// candidate objects by the caller.
    		return new Object[]{
    			activitySegment.getActivity(),
    			activityQuery
    		};
    	} else {
    		return super.getValue(
    			object, 
    			qualifiedFeatureName, 
    			app
    		);
    	}
    }

}
