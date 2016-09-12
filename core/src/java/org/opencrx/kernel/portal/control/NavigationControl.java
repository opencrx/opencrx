/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: NavigationControl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2016, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.control;

import java.util.Collection;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jmi.reflect.RefObject;

import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.action.SelectObjectAction;

public class NavigationControl extends org.openmdx.portal.servlet.control.NavigationControl {

	/**
	 * Constructor.
	 * 
	 * @param id
	 * @param locale
	 * @param localeAsIndex
	 */
	public NavigationControl(
		String id, 
		String locale, 
		int localeAsIndex
	) {
	    super(id, locale, localeAsIndex);
    }

	/**
	 * Paint alert box.
	 * 
	 * @param p
	 * @param forEditing
	 * @throws ServiceException
	 */
	public static void paintAlertBox(
		ViewPort p,
		boolean forEditing
	) throws ServiceException {
		ApplicationContext app = p.getApplicationContext();
		RefObject[] rootObjects = app.getRootObject();
		for(RefObject rootObject: rootObjects) {
		    if(rootObject instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
		    	PersistenceManager pm = JDOHelper.getPersistenceManager(rootObject);
		        org.opencrx.kernel.home1.cci2.AlertQuery alertQuery = (org.opencrx.kernel.home1.cci2.AlertQuery)pm.newQuery(org.opencrx.kernel.home1.jmi1.Alert.class);
		        alertQuery.alertState().lessThanOrEqualTo(new Short((short)1));
		        Collection<org.opencrx.kernel.home1.jmi1.Alert> alerts = ((org.opencrx.kernel.home1.jmi1.UserHome)rootObject).getAlert(alertQuery);
		        int alertsSize = 0;
		        boolean hasMoreAlerts = false;
		        for(org.opencrx.kernel.home1.jmi1.Alert alert: alerts) {
		            alertsSize++;
		            if(alertsSize >= 9) {
		                hasMoreAlerts = true;
		                break;
		            }
		        }
		        if(alertsSize > 0) {
		            Action selectAlertsAction = new Action(
		                SelectObjectAction.EVENT_ID,
			            new Action.Parameter[]{
       		                new Action.Parameter(Action.PARAMETER_PANE, "0"),
       		                new Action.Parameter(Action.PARAMETER_REFERENCE, "0"),
       		                new Action.Parameter(Action.PARAMETER_OBJECTXRI, rootObject.refMofId())
		                },
		                "Alerts",
		                true
		            );
		            p.write("<div id=\"alertBox\" onclick=\"javascript:window.location.href=", p.getEvalHRef(selectAlertsAction), ";\"><div>", Integer.toString(alertsSize), (hasMoreAlerts ? "+" : ""), "</div></div>");
		        }
		        break;
			  }
		}		
	}
	
	/**
	 * Paint share button.
	 * 
	 * @param p
	 * @param forEditing
	 * @throws ServiceException
	 */
	public static void paintShareButton(
		ViewPort p,
		boolean forEditing
	) throws ServiceException {
		p.write("<div id=\"shareButtonBox\">");
		p.write("  <share-button style=\"background:none;\"></share-button>");
		p.write("</div>");
		p.write("<style type=\"text/css\">@import url(\"./js/share-button.css\");</style>");
		p.write("<script src='./js/share-button.js'></script>");
		p.write("<script>");
		p.write("  var shareButton = new ShareButton({");
		p.write("    ui: {");
		p.write("      buttonText: ''");
		p.write("    },");
		p.write("    networks: {");
		p.write("      email: {");
		p.write("        description: window.location.href");
		p.write("      }");
		p.write("    }");
		p.write("  });");
		p.write("  shareButton.toggleListen();");
		p.write("</script>");
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
    private static final long serialVersionUID = 7946560008514514040L;
	
}
