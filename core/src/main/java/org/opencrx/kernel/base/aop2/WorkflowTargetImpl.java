/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: WorkflowTargetImpl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams;
import org.opencrx.kernel.base.jmi1.ExecuteWorkflowResult;
import org.opencrx.kernel.base.jmi1.WorkflowTarget;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.workflow1.jmi1.WfProcess;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * WorkflowTargetImpl
 *
 * @param <S>
 * @param <N>
 * @param <C>
 */
public class WorkflowTargetImpl
	<S extends WorkflowTarget,N extends org.opencrx.kernel.base.cci2.WorkflowTarget,C extends Void>
	extends AbstractObject<S,N,C> {

    /**
     * Constructor.
     * 
     * @param same
     * @param next
     */
    public WorkflowTargetImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Implementation of executeWorkflow operation.
     * 
     * @param params
     * @return
     */
    public ExecuteWorkflowResult executeWorkflow(
        ExecuteWorkflowParams params
    ) {
        try {
        	// booleanParams
        	Map<String,Boolean> booleanParams = new HashMap<String,Boolean>();
        	for(int i = 0; i < params.getBooleanParamName().size(); i++) {
        		if(params.getBooleanParamName().get(i) != null) {
	        		booleanParams.put(
	        			params.getBooleanParamName().get(i), 
	        			params.getBooleanParamValue().get(i)
	        		);
        		}
        	}
        	// stringParams
        	Map<String,String> stringParams = new HashMap<String,String>();
        	for(int i = 0; i < params.getStringParamName().size(); i++) {
        		if(params.getStringParamName().get(i) != null) {
	        		stringParams.put(
	        			params.getStringParamName().get(i), 
	        			params.getStringParamValue().get(i)
	        		);
        		}
        	}
        	if(params.getTriggeredByEventId() != null) {
        		stringParams.put(Workflows.PARAM_NAME_TRIGGERED_BY_EVENT_ID, params.getTriggeredByEventId());
        	}
        	// integerParams
        	Map<String,Integer> integerParams = new HashMap<String,Integer>();
        	for(int i = 0; i < params.getIntegerParamName().size(); i++) {
        		if(params.getIntegerParamName().get(i) != null) {
	        		integerParams.put(
	        			params.getIntegerParamName().get(i), 
	        			params.getIntegerParamValue().get(i)
	        		);
        		}
        	}
        	if(params.getTriggeredByEventType() != null) {
        		integerParams.put(Workflows.PARAM_NAME_TRIGGERED_BY_EVENT_TYPE, params.getTriggeredByEventType());
        	}
        	// decimalParams
        	Map<String,BigDecimal> decimalParams = new HashMap<String,BigDecimal>();
        	for(int i = 0; i < params.getDecimalParamName().size(); i++) {
        		if(params.getDecimalParamName().get(i) != null) {
	        		decimalParams.put(
	        			params.getDecimalParamName().get(i), 
	        			params.getDecimalParamValue().get(i)
	        		);
        		}
        	}
        	// dateTimeParams
        	Map<String,Date> dateTimeParams = new HashMap<String,Date>();
        	for(int i = 0; i < params.getDateTimeParamName().size(); i++) {
        		if(params.getDateTimeParamName().get(i) != null) {
	        		dateTimeParams.put(
	        			params.getDateTimeParamName().get(i), 
	        			params.getDateTimeParamValue().get(i)
	        		);
        		}
        	}
        	// uriParams
        	Map<String,Path> uriParams = new HashMap<String,Path>();
        	for(int i = 0; i < params.getUriParamName().size(); i++) {
        		if(params.getUriParamName().get(i) != null) {
	        		uriParams.put(
	        			params.getUriParamName().get(i), 
	        			((ContextCapable)params.getUriParamValue().get(i)).refGetPath()
	        		);
        		}
        	}
        	if(params.getTriggeredBy() != null) {
        		uriParams.put(Workflows.PARAM_NAME_TRIGGERED_BY, params.getTriggeredBy().refGetPath());
        	}
            WfProcessInstance wfInstance = Workflows.getInstance().executeWorkflow(
            	params.getName(),
            	this.sameObject(),
                (WfProcess)params.getWorkflow(),
                params.getTargetObject(),
                booleanParams,
                stringParams,
                integerParams,
                decimalParams,
                dateTimeParams,
                uriParams,
                params.getParentProcessInstance()
            );
            return Structures.create(
            	ExecuteWorkflowResult.class, 
            	Datatypes.member(ExecuteWorkflowResult.Member.workflowInstance, wfInstance)
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }

}
