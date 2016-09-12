/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.base.aop2;

import java.util.ArrayList;
import java.util.List;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.backend.XmlImporter;
import org.opencrx.kernel.backend.VCard;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class ImporterImpl
	<S extends org.opencrx.kernel.base.jmi1.Importer,N extends org.opencrx.kernel.base.cci2.Importer,C extends Void>
	extends AbstractObject<S,N,C> {

    //-----------------------------------------------------------------------
    public ImporterImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    //-----------------------------------------------------------------------
    public org.opencrx.kernel.base.jmi1.ImportResult importItem(
        org.opencrx.kernel.base.jmi1.ImportParams params
    ) {
        try {
            short locale = params.getLocale();
            byte[] item = params.getItem();
            String itemName = params.getItemName();
            String itemMimeType = params.getItemMimeType();
            List<String> report = new ArrayList<String>();
            List<String> errors = new ArrayList<String>();
            BasicObject importedObject = null; 
            if(VCard.MIME_TYPE.equals(itemMimeType) || itemName.endsWith(VCard.FILE_EXTENSION)) {
            	if(this.sameObject() instanceof Account) {
	                importedObject = VCard.getInstance().importItem(
	                	item, 
	                	(Account)this.sameObject(), 
	                	locale, 
	                	errors, 
	                	report
	                );
            	}
            } else if(ICalendar.MIME_TYPE.equals(itemMimeType) || itemName.endsWith(ICalendar.FILE_EXTENSION)) {
            	if(this.sameObject() instanceof Activity) {
	            	importedObject = ICalendar.getInstance().importItem(
	            		item, 
	            		(Activity)this.sameObject(), 
	            		locale, 
	            		errors, 
	            		report
	            	);
            	}
            } else if(XmlImporter.MIME_TYPE.equals(itemMimeType) || itemName.endsWith(XmlImporter.FILE_EXTENSION)) {
                importedObject = (BasicObject)XmlImporter.getInstance().importItem(
                	item,
                	locale,
                	(BasicObject)this.sameObject(),
                	errors,
                	report
                );
            } else {
            	return Structures.create(
            		org.opencrx.kernel.base.jmi1.ImportResult.class, 
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.importedObject, null),
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.status, Base.IMPORT_EXPORT_FORMAT_NOT_SUPPORTED),
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.statusMessage, null)            		
            	);
            }
            if(importedObject != null) {
            	return Structures.create(
            		org.opencrx.kernel.base.jmi1.ImportResult.class, 
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.importedObject, importedObject),
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.status, Base.IMPORT_EXPORT_OK),
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.statusMessage, Base.getInstance().analyseReport(report))            		
            	);
            } else {
            	return Structures.create(
            		org.opencrx.kernel.base.jmi1.ImportResult.class, 
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.importedObject, null),
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.status, Base.IMPORT_EXPORT_ITEM_NOT_VALID),
            		Datatypes.member(org.opencrx.kernel.base.jmi1.ImportResult.Member.statusMessage, Base.getInstance().analyseReport(errors))            		
            	);
            }
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }                
    }
        
}
