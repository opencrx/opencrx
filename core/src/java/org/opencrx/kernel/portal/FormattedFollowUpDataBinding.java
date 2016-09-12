/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: NoteDataBinding
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
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

import java.util.Date;

import javax.jmi.reflect.RefObject;

import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityFollowUp;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DefaultDataBinding;
import org.openmdx.portal.servlet.attribute.DateValue;

public class FormattedFollowUpDataBinding extends DefaultDataBinding {

    //-----------------------------------------------------------------------
    public FormattedFollowUpDataBinding(
    ) {
    }
    
    //-----------------------------------------------------------------------
    
    private String getLabel(
    	org.openmdx.ui1.jmi1.ElementDefinition field,
    	ApplicationContext app
    ) {
    	return app.getCurrentLocaleAsIndex() < field.getLabel().size() ?
    		field.getLabel().get(app.getCurrentLocaleAsIndex()) :
    		field.getLabel().get(0);
    }
    
    //-----------------------------------------------------------------------
    @Override
    public Object getValue(
        RefObject object, 
        String qualifiedFeatureName,
        ApplicationContext app
    ) {
        if(object instanceof ActivityFollowUp) {
        	org.openmdx.ui1.jmi1.ElementDefinition fCreatedAt = null;
        	try {
        		fCreatedAt = app.getUiElementDefinition("org:openmdx:base:Creatable:createdAt");
        	} catch(Exception e) {}
        	ActivityFollowUp followUp = (ActivityFollowUp)object;
        	Contact reportedBy = followUp.getAssignedTo();
        	Date modifiedAt = followUp.getModifiedAt();
        	Date createdAt = followUp.getCreatedAt();
        	StringBuilder formattedText = new StringBuilder();
        	// Transition
        	ActivityProcessTransition transition = null;
        	try {
        		transition = followUp.getTransition();
        	} catch(Exception e) {}
        	if(transition != null) {
	        	formattedText
	        		.append("")
	        		.append("<b>")
	        		.append(transition.getName())
	        		.append("</b><br />");
        	}
        	// Activity number
        	Activity activity = null;
        	try {
        		activity = followUp.getActivity();
        	} catch(Exception e) {}
        	if(activity != null) {
        		formattedText
	        		.append("")
	        		.append("activity:" + activity.getActivityNumber())
		    		.append("<br />");
        		// Process state
        		ActivityProcessState processState = null;
        		try {
        			processState = activity.getProcessState();
        		} catch(Exception e) {}
        		if(processState != null) {
	        		formattedText
		        		.append("")
		        		.append(activity.getProcessState().getName())
			    		.append("<br />");
        		}
        	}
        	// reportedBy
        	formattedText
	    		.append("")
	    		.append("")
	    		.append(reportedBy.getFullName())
	    		.append("<br />");
        	// modifiedAt
        	formattedText
	    		.append("")
	    		.append("")
	    		.append(DateValue.getLocalizedDateTimeFormatter(null, false, app).format(modifiedAt));
        	if(createdAt.compareTo(modifiedAt) != 0) {
        		formattedText
		    		.append("<br />(")
		    		.append(fCreatedAt == null ? "createdAt" : this.getLabel(fCreatedAt, app))
		    		.append(": ")
		    		.append(DateValue.getLocalizedDateTimeFormatter(null, false, app).format(createdAt))
		    		.append(")");
        	}
		    formattedText.append("<br />");
        	return formattedText.toString();
        }
        else {
            return null;
        }
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    
}
