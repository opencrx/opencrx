/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DuplicateDocumentWizardController
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
package org.opencrx.kernel.portal.wizard;

import java.io.Writer;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;

/**
 * DuplicateDocumentWizardController
 *
 */
public class DuplicateDocumentWizardController extends org.openmdx.portal.servlet.AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public DuplicateDocumentWizardController(
   	) {
   		super();
   	}

	/**
	 * Refresh action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doRefresh(
		@FormParameter(forms = "DuplicateDocumentForm") Map<String,Object> formFields
	) throws ServiceException {
		RefObject_1_0 obj = this.getObject();
		this.formFields = formFields;
	    String name = (String)formFields.get("org:opencrx:kernel:document1:Document:name");
	    name = name == null ? null : name.trim();
	    if(name == null && obj instanceof Document) {
	    	Document document = (Document)obj;
	    	name = "Copy of " + document.getName();
	    	this.formFields.put("org:opencrx:kernel:document1:Document:name", name);
	    }
	    this.name = name;
	}

	/**
	 * OK action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
		@FormParameter(forms = "DuplicateDocumentForm") Map<String,Object> formFields
	) throws ServiceException {
		this.doRefresh(formFields);
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		RefObject_1_0 obj = this.getObject();
		String name = this.getName();
	    if(name != null && !name.isEmpty() && obj instanceof Document) {
	    	Document document = (Document)obj;
	    	org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, this.getProviderName(), this.getSegmentName());
	    	pm.currentTransaction().begin();
	    	Document copiedDocument = (Document)PersistenceHelper.clone(obj);
	    	copiedDocument.setName(name);
	    	documentSegment.addDocument(
	    		Utils.getUidAsString(),
	    		copiedDocument
	    	);
	    	if(document.getHeadRevision() != null) {
	    		DocumentRevision copiedRevision = (DocumentRevision)PersistenceHelper.clone(document.getHeadRevision());
	    		copiedRevision.setName(name);
	    		copiedRevision.setVersion("1");
	    		if(copiedRevision instanceof MediaContent) {
	    			((MediaContent)copiedRevision).setContentName(name);
	    		}
	    		copiedDocument.addRevision(
	    			Utils.getUidAsString(),
	    			copiedRevision
	    		);
	    		copiedDocument.setHeadRevision(copiedRevision);
	    	}
	    	pm.currentTransaction().commit();
	    	this.setExitAction(
		    	new ObjectReference(copiedDocument, app).getSelectObjectAction()
		    );
			return;
	    }
	}

   	/**
   	 * Cancel action.
   	 * 
   	 * @throws ServiceException
   	 */
   	public void doCancel(
	) throws ServiceException {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

   	/**
   	 * Get form values.
   	 * 
   	 * @return
   	 */
   	public Map<String,Object> getFormFields(
   	) {
   		return this.formFields;
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
	
	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	private Map<String,Object> formFields;
	private String name;
	private ViewPort viewPort;
}
