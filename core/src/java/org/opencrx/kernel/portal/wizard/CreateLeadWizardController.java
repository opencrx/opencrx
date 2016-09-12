/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateLeadWizardController
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
package org.opencrx.kernel.portal.wizard;

import java.io.Writer;
import java.util.Date;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.Contracts;
import org.openmdx.base.accessor.cci.SystemAttributes;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;

/**
 * CreateLeadWizardController
 *
 */
public class CreateLeadWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateLeadWizardController(
	) {
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#initFormFields(java.util.Map)
	 */
    @Override
    protected void initFormFields(
    	Map<String, Object> formFields
    ) throws ServiceException {
    	formFields.put(
    	    SystemAttributes.OBJECT_CLASS,
    	    "org:opencrx:kernel:contract1:Lead"
    	);
    }

    /**
     * Handle OK action.
     * 
     * @param formFields
     * @throws ServiceException
     */
    public void doOK(
    	@RequestParameter(name = "isInitialized") Boolean isInitialized,
    	@FormParameter(forms = "CreateLeadForm") Map<String,Object> formFields
    ) throws ServiceException {
    	PersistenceManager pm = this.getPm();
    	ApplicationContext app = this.getApp();
    	this.doRefresh(
    		isInitialized,
    		formFields
    	);
	    org.opencrx.kernel.account1.jmi1.Account customer = formFields.get("org:opencrx:kernel:contract1:SalesContract:customer") != null 
	    	? (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(
	    		formFields.get("org:opencrx:kernel:contract1:SalesContract:customer")
	    	  ) 
	    	: null;
	    String name = (String)formFields.get("org:opencrx:kernel:contract1:AbstractContract:name");
	    String contractNumber = (String)formFields.get("org:opencrx:kernel:contract1:AbstractContract:contractNumber");
	    Short leadSource = (Short)formFields.get("org:opencrx:kernel:contract1:Lead:leadSource");
	    Short leadState = (Short)formFields.get("org:opencrx:kernel:contract1:AbstractContract:contractState");
	    Short leadRating = (Short)formFields.get("org:opencrx:kernel:contract1:Lead:leadRating");
	    Short closeProbability = (Short)formFields.get("org:opencrx:kernel:contract1:Lead:closeProbability");
	    Short priority = (Short)formFields.get("org:opencrx:kernel:contract1:AbstractContract:priority");
	    Short contractCurrency = (Short)formFields.get("org:opencrx:kernel:contract1:SalesContract:contractCurrency");
	    java.math.BigDecimal estimatedValue = (java.math.BigDecimal)formFields.get("org:opencrx:kernel:contract1:Lead:estimatedValue");
	    Date estimatedCloseDate = (Date)formFields.get("org:opencrx:kernel:contract1:Lead:estimatedCloseDate");
	    java.math.BigDecimal estimatedSalesCommission = (java.math.BigDecimal)formFields.get("org:opencrx:kernel:contract1:Lead:estimatedSalesCommission");
	    String description = (String)formFields.get("org:opencrx:kernel:contract1:AbstractContract:description");
	    String nextStep = (String)formFields.get("org:opencrx:kernel:contract1:Lead:nextStep");
	    if(
	        (customer != null) &&
	        (name != null)
	    ) {
	    	org.opencrx.kernel.contract1.jmi1.Segment contractSegment = Contracts.getInstance().getContractSegment(pm, this.getProviderName(), this.getSegmentName());
	        org.opencrx.kernel.contract1.jmi1.Lead lead = pm.newInstance(org.opencrx.kernel.contract1.jmi1.Lead.class);
	        lead.setName(name);
	        lead.setContractNumber(contractNumber);
	        lead.setCustomer(customer);
	        lead.setLeadSource(leadSource);
	        lead.setContractState(leadState);
	        lead.setLeadRating(leadRating);
	        lead.setCloseProbability(closeProbability);
	        lead.setPriority(priority);
	        lead.setContractCurrency(contractCurrency);
	        lead.setEstimatedValue(estimatedValue);
	        lead.setEstimatedCloseDate(estimatedCloseDate);
	        lead.setEstimatedSalesCommission(estimatedSalesCommission);
	        lead.setDescription(description);
	        lead.setNextStep(nextStep);
	      	pm.currentTransaction().begin();
	      	contractSegment.addLead(
	            Base.getInstance().getUidAsString(),
	            lead
	        );
	        pm.currentTransaction().commit();
	        this.setExitAction(
	        	new ObjectReference(lead, app).getSelectObjectAction()
	        );
	    }    	
    }
    
    /**
     * Handle Refresh action.
     * 
     * @param formFields
     * @throws ServiceException
     */
    public void doRefresh(
    	@RequestParameter(name = "isInitialized") Boolean isInitialized,    	
    	@FormParameter(forms = "CreateLeadForm") Map<String,Object> formFields    	
    ) throws ServiceException {
    	PersistenceManager pm = this.getPm();
    	RefObject_1_0 obj = this.getObject();
    	this.formFields = formFields;
    	if(!Boolean.TRUE.equals(isInitialized)) {
        	if(obj instanceof org.opencrx.kernel.account1.jmi1.Account) {
        	    org.opencrx.kernel.account1.jmi1.Account account = (org.opencrx.kernel.account1.jmi1.Account)obj;
        	    formFields.put(
        	        "org:opencrx:kernel:contract1:SalesContract:customer",
        	        account.refGetPath()
        	    );
        	}    		
    	}
		Account customer = formFields.get("org:opencrx:kernel:contract1:SalesContract:customer") != null 
			? (Account)pm.getObjectById(
				this.formFields.get("org:opencrx:kernel:contract1:SalesContract:customer")
			  )
			: null;
    	if(customer != null) {
    		String contractNumber = (String)this.formFields.get("org:opencrx:kernel:contract1:AbstractContract:contractNumber");
    		if(contractNumber == null || contractNumber.isEmpty()) {
			    this.formFields.put(
			       	"org:opencrx:kernel:contract1:AbstractContract:contractNumber",
			        (customer.getAliasName() != null ? customer.getAliasName() : customer.getFullName()) + "-" + (System.currentTimeMillis() / 1000)
			    );
    		}
    	}
    }
    
    /**
     * Handle Cancel action.
     * 
     */
    public void doCancel(
    ) {
		this.setExitAction(
			new ObjectReference(getObject(), getApp()).getSelectObjectAction()
		);    	
    }
    
	/**
	 * @return the form values
	 */
	public Map<String,Object> getFormFields(
	) {
		return this.formFields;
	}

	/**
	 * Get viewPort.
	 * 
	 * @param out
	 * @return
	 */
	public ViewPort getViewPort(
		Writer out
	) {
		if(this.viewPort == null) {
			TransientObjectView view = new TransientObjectView(
				this.getFormFields(),
				this.getApp(),
				this.getObject(),
				this.getPm()
			);
			this.viewPort = ViewPortFactory.openPage(
				view,
				this.getRequest(),
				out
			);
		}
		return this.viewPort;
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#close()
	 */
    @Override
    public void close(
    ) throws ServiceException {
	    super.close();
		if(this.viewPort != null) {
			this.viewPort.close(false);		
		}	    
    }

	//-------------------------------------------------------------------
	// Members
	//-------------------------------------------------------------------	
	private Map<String,Object> formFields;
	private ViewPort viewPort;

}
