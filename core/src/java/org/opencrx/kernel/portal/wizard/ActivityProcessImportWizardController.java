/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ActivityProcessImportWizardController
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

import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.jmi1.ActivityProcess;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;

/**
 * ActivityProcessImportWizardController
 *
 */
public class ActivityProcessImportWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public ActivityProcessImportWizardController(
	) {
	}

	/**
	 * Refresh action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doRefresh(
		@FormParameter(forms = "ActivityProcessImportForm") Map<String,Object> formFields
	) throws ServiceException {
		this.formFields = formFields;
	}
	
	/**
	 * OK action.
	 * 
	 * @throws ServiceException
	 */
	public void doOK(
		@FormParameter(forms = "ActivityProcessImportForm") Map<String,Object> formFields
	) throws ServiceException, IOException {
		ApplicationContext app = this.getApp();
		PersistenceManager pm = this.getPm();
		this.doRefresh(formFields);
	    Document document = formFields.get("org:opencrx:kernel:generic:DocumentAttachment:document") != null
	    	? (Document)pm.getObjectById(
	    		formFields.get("org:opencrx:kernel:generic:DocumentAttachment:document")
	    	  )
	    	: null;
	    if(document != null) {
	    	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, this.getProviderName(), this.getSegmentName());
	    	String contentType = document.getContentType();
	    	if(contentType != null && contentType.startsWith("text/")) {
	    		DocumentRevision headRevision = document.getHeadRevision();
	    		if(headRevision instanceof org.opencrx.kernel.document1.jmi1.MediaContent) {
	    			org.opencrx.kernel.document1.jmi1.MediaContent mediaContent = (org.opencrx.kernel.document1.jmi1.MediaContent)headRevision;
	    			InputStream content = mediaContent.getContent().getContent();
    				pm.currentTransaction().begin();
    				ActivityProcess activityProcess = Activities.getInstance().importActivityProcessFromScXml(
   						activitySegment,
   						content,
   						this.report
   					);
    				if(this.report.isEmpty()) {
    					pm.currentTransaction().commit();
		    	    	if(activityProcess != null) {
		    	    		this.setExitAction(
		    	    			new ObjectReference(activityProcess, app).getSelectObjectAction()
		    	    		);
		    	    	}    					
    				} else {
    					try {
    						pm.currentTransaction().rollback();
    					} catch(Exception e1) {}
    				}
	    		}
	    	}
	    }		
	}

	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(
	) {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

	/**
	 * @return the formFields
	 */
	public Map<String, Object> getFormFields(
	) {
		return this.formFields;
	}

	/**
	 * @return the report
	 */
	public List<String> getReport(
	) {
		return this.report;
	}

	/**
	 * Get view port.
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

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private Map<String,Object> formFields;
	private List<String> report = new ArrayList<String>();
	private ViewPort viewPort;

}
