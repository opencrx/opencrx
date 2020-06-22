/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AssignedActivityGroupsDataBinding
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2008, CRIXP Corp., Switzerland
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.jmi.reflect.RefObject;

import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DataBinding;

public class DocumentFolderAssignmentsDataBinding extends DataBinding {

	/**
	 * @param parameterString
	 */
	public DocumentFolderAssignmentsDataBinding(
		String parameterString
	) {
		if(parameterString != null && parameterString.startsWith("name=")) {
			this.folderNameFilter = parameterString.substring(5).split(",");
		}
		else {
			this.folderNameFilter = null;
		}
	}
	
    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DataBinding#getValue(javax.jmi.reflect.RefObject, java.lang.String, org.openmdx.portal.servlet.ApplicationContext)
     */
    public Object getValue(
        RefObject object, 
        String qualifiedFeatureName,
        ApplicationContext app
    ) {
        if(object instanceof org.opencrx.kernel.generic.jmi1.CrxObject) {
        	org.opencrx.kernel.generic.jmi1.CrxObject crxObject = (org.opencrx.kernel.generic.jmi1.CrxObject)object;
            List<org.opencrx.kernel.document1.jmi1.DocumentFolder> folders = new ArrayList<org.opencrx.kernel.document1.jmi1.DocumentFolder>();
            Collection<org.opencrx.kernel.generic.jmi1.DocumentFolderAssignment> assignments = crxObject.getDocumentFolderAssignment();
            for(org.opencrx.kernel.generic.jmi1.DocumentFolderAssignment assignment: assignments) {
                if(assignment.getDocumentFolder() != null) {
                	org.opencrx.kernel.document1.jmi1.DocumentFolder folder = assignment.getDocumentFolder();
                	boolean matches = this.folderNameFilter == null;
                	if(!matches) {
                		for(String folderName: this.folderNameFilter) {
                			if(folder.getName().startsWith(folderName)) {
                				matches = true;
                				break;
                			}
                		}
                	}
                	if(matches) {
                		folders.add(folder);
                	}
                }
            }
            return folders;
        }
        return null;
    }

    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DataBinding#setValue(javax.jmi.reflect.RefObject, java.lang.String, java.lang.Object, org.openmdx.portal.servlet.ApplicationContext)
     */
    public void setValue(
        RefObject object, 
        String qualifiedFeatureName, 
        Object newValue,
        ApplicationContext app
    ) {
    }
        
    private final String[] folderNameFilter;
    
}
