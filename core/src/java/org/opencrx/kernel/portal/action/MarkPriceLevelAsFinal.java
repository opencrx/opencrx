/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MarkPriceLevelAsFinal
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.jdo.PersistenceManager;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.opencrx.kernel.product1.jmi1.AbstractPriceLevel;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ViewsCache;
import org.openmdx.portal.servlet.action.ActionPerformResult;
import org.openmdx.portal.servlet.action.BoundAction;
import org.openmdx.portal.servlet.attribute.ObjectReferenceValue;
import org.openmdx.portal.servlet.component.Grid;
import org.openmdx.portal.servlet.component.ObjectView;
import org.openmdx.portal.servlet.component.ReferencePane;
import org.openmdx.portal.servlet.component.ShowObjectView;
import org.openmdx.portal.servlet.component.UiGrid;

/**
 * MarkPriceLevelAsFinal
 *
 */
public class MarkPriceLevelAsFinal extends BoundAction {

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
	            int paneIndex = -1;
	            try { 
	            	paneIndex = Integer.parseInt(requestParameters.get(Action.PARAMETER_PANE)[0]);
	            } catch(Exception e) {}
	            int referenceIndex = -1;
	            try {
	            	referenceIndex = Integer.parseInt(requestParameters.get(Action.PARAMETER_REFERENCE)[0]);
	            } catch(Exception e) {}
	            List<ReferencePane> referencePanes = currentView.getChildren(ReferencePane.class);
	            if(paneIndex < referencePanes.size()) {
	                currentView.selectReferencePane(paneIndex);
	                referencePanes.get(paneIndex).selectReference(referenceIndex);
	                Grid grid = referencePanes.get(paneIndex).getGrid();
	                if(grid instanceof UiGrid) {
	                	UiGrid uiGrid = (UiGrid)grid;
	                	List<Path> selectedObjectIdentities = new ArrayList<Path>();
	                    StringTokenizer tokenizer = new StringTokenizer(parameter, " ");
	                    while(tokenizer.hasMoreTokens()) {
	                    	try {
		                        selectedObjectIdentities.add(
		                        	new Path(Action.getParameter(tokenizer.nextToken(), Action.PARAMETER_OBJECTXRI))
		                        );
	                    	} catch(Exception e) {}
	                    }
	                    int maxItems = 500; // default maxItems
	                    try {
	                    	maxItems = Integer.parseInt(requestParameters.get(Action.PARAMETER_SIZE)[0]);
	                    } catch(Exception e) {}
	                    Collection<Object> objects = new ArrayList<Object>();
	                    // Set alert states
	            		if(maxItems < Integer.MAX_VALUE) {
            				// Selected objects
            				if(selectedObjectIdentities != null && !selectedObjectIdentities.isEmpty()) {
            					for(Path identity: selectedObjectIdentities) {
            						objects.add(
            							pm.getObjectById(identity)
            						);
            					}
            				} else {
            					// Current page
            					List<UiGrid.GridRow> rows = uiGrid.getRows(pm);
            					for(UiGrid.GridRow row: rows) {
            						List<Object> cells = row.getCells();
            						if(cells != null && !cells.isEmpty()) {
            							objects.add(
            								((ObjectReferenceValue)cells.get(0)).getObject()
            							);
            						}
            					}
            				}
            			}
	            		try {
		            		pm.currentTransaction().begin();
		                    for(Object object: objects) {
		                    	if(object instanceof AbstractPriceLevel) {		                    		
	                    			((AbstractPriceLevel)object).setFinal(true);
		                    	}
		                    }
            				pm.currentTransaction().commit();
	            		} catch(Exception ignore) {}
	            			try {
	            				pm.currentTransaction().rollback();
	            			} catch(Exception e) {
	            		}
	                }
	            }
	    	} catch (Exception e) {
	            ServiceException e0 = new ServiceException(e);
	            SysLog.warning(e0.getMessage(), e0.getCause());
	            try {
	                pm.currentTransaction().rollback();
	            } catch(Exception e1) {}
	        }	        
	        pm.close();
        }
        return new ActionPerformResult(view);
    }

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final int EVENT_ID = 108;		

}
