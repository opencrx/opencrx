/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: WorkflowHelper
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.utils;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.base.cci2.StringProperty;
import org.opencrx.kernel.base.jmi1.BooleanProperty;
import org.opencrx.kernel.base.jmi1.DateProperty;
import org.opencrx.kernel.base.jmi1.DateTimeProperty;
import org.opencrx.kernel.base.jmi1.DecimalProperty;
import org.opencrx.kernel.base.jmi1.IntegerProperty;
import org.opencrx.kernel.base.jmi1.Property;
import org.opencrx.kernel.base.jmi1.PropertySet;
import org.opencrx.kernel.base.jmi1.ReferenceProperty;
import org.opencrx.kernel.base.jmi1.UriProperty;
import org.opencrx.kernel.home1.jmi1.WfActionLogEntry;
import org.opencrx.kernel.home1.jmi1.WfBooleanParameter;
import org.opencrx.kernel.home1.jmi1.WfDateParameter;
import org.opencrx.kernel.home1.jmi1.WfDateTimeParameter;
import org.opencrx.kernel.home1.jmi1.WfDecimalParameter;
import org.opencrx.kernel.home1.jmi1.WfIntegerParameter;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.home1.jmi1.WfProcessInstanceParameter;
import org.opencrx.kernel.home1.jmi1.WfReferenceParameter;
import org.opencrx.kernel.home1.jmi1.WfStringParameter;
import org.opencrx.kernel.home1.jmi1.WfUriParameter;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;

/**
 * WorkflowHelper
 *
 */
public abstract class WorkflowHelper {

    /**
     * Create log entry for specified workflow instance.
     * 
     * @param wfProcessInstance
     * @param name
     * @param description
     * @throws ServiceException
     */
    public static void createLogEntry(        
        WfProcessInstance wfProcessInstance,
        String name,
        String description
    ) throws ServiceException  {
    	createLogEntry(
    		wfProcessInstance,
    		name,
    		description,
    		null // correlation
    	);
    }

    /**
     * Create log entry for specified workflow instance.
     * 
     * @param wfProcessInstance
     * @param name
     * @param description
     * @param correlation
     * @throws ServiceException
     */
    public static void createLogEntry(        
        WfProcessInstance wfProcessInstance,
        String name,
        String description,
        BasicObject correlation
    ) throws ServiceException  {
        if(wfProcessInstance == null) return;
        PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
        WfActionLogEntry logEntry = pm.newInstance(WfActionLogEntry.class);        
        try {
            boolean isTxLocal = !pm.currentTransaction().isActive();
            if(isTxLocal) {
            	pm.currentTransaction().begin();
            }
            wfProcessInstance.addActionLog(
                Workflows.getInstance().getUidAsString(),
                logEntry
            );
            logEntry.setName(name == null ? "N/A" : name);
            logEntry.setDescription(description);
            logEntry.setCorrelation(correlation);
            if(isTxLocal) {
            	pm.currentTransaction().commit();
            }
        } catch(Exception e) {
            new ServiceException(e).log();
            try {
                pm.currentTransaction().rollback();                
            } catch(Exception e0) {}
        }
    }

	/**
	 * Collect workflow parameters.
	 * 
	 * @param wfProcessInstance
	 * @return
	 */
	public static Map<String,Object> getWorkflowParameters(
		WfProcessInstance wfProcessInstance
	) {
        Map<String,Object> params = new HashMap<String,Object>();
        // Collect from workflow
        {
            Collection<Property> properties = wfProcessInstance.getProcess().getProperty();
            // Add parameters of executeWorkflow operation to params
            for(Property property: properties) {
                Object val = null;
                if(property instanceof BooleanProperty) {
                    val = ((BooleanProperty)property).isBooleanValue();
                } else if(property instanceof IntegerProperty) {
                    val = ((IntegerProperty)property).getIntegerValue();
                } else if(property instanceof DecimalProperty) {
                    val = ((DecimalProperty)property).getDecimalValue();
                } else if(property instanceof UriProperty) {
                    val = ((UriProperty)property).getUriValue();                
                } else if(property instanceof StringProperty) {
                    val = ((StringProperty)property).getStringValue();
                } else if(property instanceof ReferenceProperty) {
                    val = ((ReferenceProperty)property).getReferenceValue();
                } else if(property instanceof DateTimeProperty) {
                    val = ((DateTimeProperty)property).getDateTimeValue();
                } else if(property instanceof DateProperty) {
                    val = ((DateProperty)property).getDateValue();
                }
                params.put(
                    property.getName(),
                    val
                );
            }
        }
        // Collect from instance (override workflow properties)
        {
            Collection<WfProcessInstanceParameter> parameters = wfProcessInstance.getParameter();
            for(WfProcessInstanceParameter parameter: parameters) {
                Object val = null;
                if(parameter instanceof WfBooleanParameter) {
                    val = ((WfBooleanParameter)parameter).isBooleanValue();
                } else if(parameter instanceof WfIntegerParameter) {
                    val = ((WfIntegerParameter)parameter).getIntegerValue();
                } else if(parameter instanceof WfDecimalParameter) {
                    val = ((WfDecimalParameter)parameter).getDecimalValue();
                } else if(parameter instanceof WfUriParameter) {
                    val = ((WfUriParameter)parameter).getUriValue();                
                } else if(parameter instanceof WfStringParameter) {
                    val = ((WfStringParameter)parameter).getStringValue();
                } else if(parameter instanceof WfReferenceParameter) {
                    val = ((WfReferenceParameter)parameter).getReferenceValue();
                } else if(parameter instanceof WfDateTimeParameter) {
                    val = ((WfDateTimeParameter)parameter).getDateTimeValue();
                } else if(parameter instanceof WfDateParameter) {
                    val = ((WfDateParameter)parameter).getDateValue();
                }                
                params.put(
                    parameter.getName(),
                    val
                );
            }       	
        }
        // Collect from trigger (override workflow and workflow instance properties)
        if(params.get("triggeredBy") != null) {
        	PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
        	ContextCapable triggeredBy = null;
        	try {
        		triggeredBy = (ContextCapable)pm.getObjectById(
        			new Path((String)params.get("triggeredBy"))
        		);
        	} catch(Exception e) {}
        	if(triggeredBy instanceof WfProcessInstance) {
	            for(WfProcessInstanceParameter parameter: ((WfProcessInstance)triggeredBy).<WfProcessInstanceParameter>getParameter()) {
	                Object val = null;
	                if(parameter instanceof WfBooleanParameter) {
	                    val = ((WfBooleanParameter)parameter).isBooleanValue();
	                } else if(parameter instanceof WfIntegerParameter) {
	                    val = ((WfIntegerParameter)parameter).getIntegerValue();
	                } else if(parameter instanceof WfDecimalParameter) {
	                    val = ((WfDecimalParameter)parameter).getDecimalValue();
	                } else if(parameter instanceof WfUriParameter) {
	                    val = ((WfUriParameter)parameter).getUriValue();                
	                } else if(parameter instanceof WfStringParameter) {
	                    val = ((WfStringParameter)parameter).getStringValue();
	                } else if(parameter instanceof WfReferenceParameter) {
	                    val = ((WfReferenceParameter)parameter).getReferenceValue();
	                } else if(parameter instanceof WfDateTimeParameter) {
	                    val = ((WfDateTimeParameter)parameter).getDateTimeValue();
	                } else if(parameter instanceof WfDateParameter) {
	                    val = ((WfDateParameter)parameter).getDateValue();
	                }	                
	                params.put(
	                    parameter.getName(),
	                    val
	                );
	            }
        	} else if(triggeredBy instanceof PropertySet) {
	            for(Property property: ((PropertySet)triggeredBy).<Property>getProperty()) {
	                Object val = null;
	                if(property instanceof BooleanProperty) {
	                    val = ((BooleanProperty)property).isBooleanValue();
	                } else if(property instanceof IntegerProperty) {
	                    val = ((IntegerProperty)property).getIntegerValue();
	                } else if(property instanceof DecimalProperty) {
	                    val = ((DecimalProperty)property).getDecimalValue();
	                } else if(property instanceof UriProperty) {
	                    val = ((UriProperty)property).getUriValue();                
	                } else if(property instanceof StringProperty) {
	                    val = ((StringProperty)property).getStringValue();
	                } else if(property instanceof ReferenceProperty) {
	                    val = ((ReferenceProperty)property).getReferenceValue();
	                } else if(property instanceof DateTimeProperty) {
	                    val = ((DateTimeProperty)property).getDateTimeValue();
	                } else if(property instanceof DateProperty) {
	                    val = ((DateProperty)property).getDateValue();
	                }	                
	                params.put(
	                    property.getName(),
	                    val
	                );
	            }
        	}
        }
		return params;
	}
    
	/**
	 * Add reference parameter to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addParameter(
		WfProcessInstance wfProcessInstance,
		String name,
		BasicObject value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
			WfReferenceParameter parameter = pm.newInstance(WfReferenceParameter.class);
			parameter.setName(name);
			parameter.setReferenceValue(value);
			wfProcessInstance.addParameter(
				Utils.getUidAsString(),
				parameter
			);
		}
	}

	/**
	 * Add boolean parameter to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addParameter(
		WfProcessInstance wfProcessInstance,
		String name,
		Boolean value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
			WfBooleanParameter parameter = pm.newInstance(WfBooleanParameter.class);
			parameter.setName(name);
			parameter.setBooleanValue(value);
			wfProcessInstance.addParameter(
				Utils.getUidAsString(),
				parameter
			);
		}
	}
	
	/**
	 * Add string parameter to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addParameter(
		WfProcessInstance wfProcessInstance,
		String name,
		String value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
			WfStringParameter parameter = pm.newInstance(WfStringParameter.class);
			parameter.setName(name);
			parameter.setStringValue(value);
			wfProcessInstance.addParameter(
				Utils.getUidAsString(),
				parameter
			);
		}
	}

	/**
	 * Add string parameter to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addParameter(
		WfProcessInstance wfProcessInstance,
		String name,
		Date value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
			WfDateTimeParameter parameter = pm.newInstance(WfDateTimeParameter.class);
			parameter.setName(name);
			parameter.setDateTimeValue(value);
			wfProcessInstance.addParameter(
				Utils.getUidAsString(),
				parameter
			);
		}
	}

	/**
	 * Add integer parameter to propertySet.
	 * 
	 * @param propertySet
	 * @param value
	 * @throws ServiceException
	 */
	public static void addParameter(
		WfProcessInstance wfProcessInstance,
		String name,
		Number value
	) throws ServiceException {
		if(value != null) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
			WfIntegerParameter parameter = pm.newInstance(WfIntegerParameter.class);
			parameter.setName(name);
			parameter.setIntegerValue(value.intValue());
			wfProcessInstance.addParameter(
				Utils.getUidAsString(),
				parameter
			);
		}
	}
	
}
