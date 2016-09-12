/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: EditDocumentRevisionWizardController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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

import java.io.ByteArrayOutputStream;
import java.io.Writer;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * EditDocumentRevisionWizardController
 *
 */
public class EditDocumentRevisionWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public EditDocumentRevisionWizardController(
	) {
		super();
	}
	
	/**
	 * Refresh action.
	 * 
	 * @param isInitialized
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doRefresh(
		@RequestParameter(name = "isInitialized") Boolean isInitialized,		
		@FormParameter(forms = "editDocumentRevisionForm") Map<String,Object> formFields		
	) throws ServiceException {
		this.formFields = formFields;
		if(!Boolean.TRUE.equals(isInitialized)) {
			if(this.getObject() instanceof Document) {
				Document document = (Document)this.getObject();
				String text = "";
				if(document.getHeadRevision() instanceof MediaContent) {
					MediaContent mediaContent = (MediaContent)document.getHeadRevision();
				    if(mediaContent.getContentMimeType().startsWith("text/")) {
				    	try {
					    	ByteArrayOutputStream content = new ByteArrayOutputStream();
					    	BinaryLargeObjects.streamCopy(mediaContent.getContent().getContent(), 0L, content);
					    	content.close();
					    	text = new String(content.toByteArray(), "UTF-8");
				    	} catch(Exception e) {
				    		throw new ServiceException(e);
				    	}
				    }
	          	}
	          	this.formFields.put("org:opencrx:kernel:base:Note:text", text);
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
			new ObjectReference(getObject(), getApp()).getSelectObjectAction()
		);		
	}

	/**
	 * OK action.
	 * 
	 * @param isInitialized
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
		@RequestParameter(name = "isInitialized") Boolean isInitialized,		
		@FormParameter(forms = "editDocumentRevisionForm") Map<String,Object> formFields		
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.doRefresh(
			isInitialized, 
			formFields
		);
		if(this.getObject() instanceof Document) {
			Document document = (Document)this.getObject();
			MediaContent newHeadRevision = pm.newInstance(MediaContent.class);
			if(document.getHeadRevision() instanceof MediaContent) {
				MediaContent headRevision = (MediaContent)document.getHeadRevision();
				String newName = headRevision.getName() == null
					? (document.getName() == null ? "no_name" : document.getName())
					: headRevision.getName();
				newHeadRevision.setName(newName);
				newHeadRevision.setContentName(headRevision.getContentName());
				if(headRevision.getContentMimeType().startsWith("text/")) {
					newHeadRevision.setContentMimeType(headRevision.getContentMimeType());
				}
				int newVersion = 0;
				try {
					newVersion = Integer.valueOf(headRevision.getVersion());
					newVersion++;
				} catch(Exception e) {}
				newHeadRevision.setVersion(Integer.toString(newVersion));
			} else {
				String newName = document.getName() == null
					? "no_name" 
					: document.getName();
				newHeadRevision.setName(newName);
				newHeadRevision.setContentName(document.getName());
				newHeadRevision.setContentMimeType("text/plain");
			}
			newHeadRevision.setAuthor(app.getLoginPrincipal());
			String text = (String)this.formFields.get("org:opencrx:kernel:base:Note:text");
			byte[] textAsBytes = new byte[]{};
			if(text != null) {
				try {
					textAsBytes = text.getBytes("UTF-8");
				} catch(Exception e) {
					textAsBytes = text.getBytes();
				}
			}
			newHeadRevision.setContent(
				BinaryLargeObjects.valueOf(textAsBytes)
			);
			pm.currentTransaction().begin();
			document.addRevision(
				org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
				newHeadRevision
			);
			document.setHeadRevision(newHeadRevision);
			pm.currentTransaction().commit();
		}
		this.forward(
			"Cancel", 
			this.getRequest().getParameterMap()
		);
	}

	/**
	 * @return the formFields
	 */
	public Map<String, Object> getFormFields() {
		return formFields;
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
