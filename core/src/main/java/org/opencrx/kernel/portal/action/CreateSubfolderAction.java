/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateSubfolderAction
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011-2014, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.action;

import java.io.IOException;
import java.util.Map;

import javax.jdo.PersistenceManager;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ViewsCache;
import org.openmdx.portal.servlet.action.ActionPerformResult;
import org.openmdx.portal.servlet.action.BoundAction;
import org.openmdx.portal.servlet.component.ObjectView;
import org.openmdx.portal.servlet.component.ShowObjectView;

/**
 * CreateSubfolderAction
 *
 */
public class CreateSubfolderAction extends BoundAction {

	/**
	 * Constructor.
	 * 
	 * @param alertState
	 */
	public CreateSubfolderAction(
	) {
	}
	
	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.action.BoundAction#perform(org.openmdx.portal.servlet.view.ObjectView, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse, java.lang.String, javax.servlet.http.HttpSession, java.util.Map, org.openmdx.portal.servlet.ViewsCache, org.openmdx.portal.servlet.ViewsCache)
	 */
	@Override
    public ActionPerformResult perform(
        ObjectView view,
        HttpServletRequest request,
        HttpServletResponse response,        
        String parameter,
        HttpSession session,
        Map<String,String[]> requestParameters,
        ViewsCache editViewsCache,
        ViewsCache showViewsCache      
    ) throws IOException, ServletException {
        ApplicationContext app = view.getApplicationContext();
        if(view instanceof ShowObjectView) {
            ShowObjectView currentView = (ShowObjectView)view;    	
        	PersistenceManager pm = app.getNewPmData();
        	try {
        		if(currentView.getObject() instanceof DocumentFolder) {
        			DocumentFolder parentFolder = (DocumentFolder)currentView.getObject();
        			org.opencrx.kernel.document1.jmi1.Segment documentSegment = Documents.getInstance().getDocumentSegment(pm, parentFolder.refGetPath().get(2), parentFolder.refGetPath().get(4));
        			pm.currentTransaction().begin();
        			DocumentFolder folder = pm.newInstance(DocumentFolder.class);
        			folder.setName("New folder");
        			folder.setParent((DocumentFolder)pm.getObjectById(parentFolder.refGetPath()));
        			for(PrincipalGroup owningGroup: parentFolder.<PrincipalGroup>getOwningGroup()) {
        				folder.getOwningGroup().add(
        					(PrincipalGroup)pm.getObjectById(owningGroup.refGetPath())
        				);
        			}
        			documentSegment.addFolder(
        				Utils.getUidAsString(),
        				folder
        			);
        			pm.currentTransaction().commit();
        		}
        	} catch(Exception e) {
        		try {
        			pm.currentTransaction().rollback();
        		} catch(Exception ignore) {}
        	}
	        pm.close();
        }
        return new ActionPerformResult(view);
    }

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final int EVENT_ID = 109;		
}
