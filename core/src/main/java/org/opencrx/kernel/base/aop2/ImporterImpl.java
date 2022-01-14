/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
import org.opencrx.kernel.backend.VCard;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.backend.XmlImporter;
import org.opencrx.kernel.base.jmi1.ImportItemResult;
import org.opencrx.kernel.workflow1.jmi1.RunImportResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.text.conversion.Base64;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class ImporterImpl
	<S extends org.opencrx.kernel.base.jmi1.Importer,N extends org.opencrx.kernel.base.cci2.Importer,C extends Void>
	extends AbstractObject<S,N,C> {

    public ImporterImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Import item.
     * 
     * @param params
     * @return
     */
    public ImportItemResult importItem(
        org.opencrx.kernel.base.jmi1.ImportItemParams params
    ) {
        try {
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
	                	(short)0, 
	                	errors, 
	                	report
	                );
            	}
            } else if(ICalendar.MIME_TYPE.equals(itemMimeType) || itemName.endsWith(ICalendar.FILE_EXTENSION)) {
            	if(this.sameObject() instanceof Activity) {
	            	importedObject = ICalendar.getInstance().importItem(
	            		item, 
	            		(Activity)this.sameObject(), 
	            		(short)0, 
	            		errors,
	            		report
	            	);
            	}
            } else if(XmlImporter.MIME_TYPE.equals(itemMimeType) || itemName.endsWith(XmlImporter.FILE_EXTENSION)) {
            	if(this.sameObject() instanceof org.openmdx.base.jmi1.Segment) {
	                importedObject = (BasicObject)XmlImporter.getInstance().importItem(
	                	item,
	                	(short)0,
	                	(org.openmdx.base.jmi1.Segment)this.sameObject(),
	                	errors,
	                	report
	                );
            	}
            } else {
            	List<String> runImportParams = new ArrayList<String>();
            	runImportParams.add(this.sameObject().refGetPath().toXRI());
            	runImportParams.add(Base64.encode(item));
            	runImportParams.add(itemName);
            	runImportParams.add(itemMimeType);
            	RunImportResult result = Workflows.getInstance().runImport(
            		params.getImporterTask(),
            		runImportParams
            	);
                return Structures.create(
                	ImportItemResult.class,
            		Datatypes.member(ImportItemResult.Member.importedObject, result.getImportedObject()),
                	Datatypes.member(ImportItemResult.Member.status, result.getStatus()),
                	Datatypes.member(ImportItemResult.Member.statusMessage, result.getStatusMessage()),
                	Datatypes.member(ImportItemResult.Member.item, result.getFile()),
                	Datatypes.member(ImportItemResult.Member.itemName, result.getFileName()),
                	Datatypes.member(ImportItemResult.Member.itemMimeType, result.getFileMimeType())
                );
            }
            if(importedObject != null) {
            	return Structures.create(
            		ImportItemResult.class,
            		Datatypes.member(ImportItemResult.Member.importedObject, importedObject),
            		Datatypes.member(ImportItemResult.Member.status, Base.IMPORT_EXPORT_OK),
            		Datatypes.member(ImportItemResult.Member.statusMessage, Base.getInstance().analyseReport(report))
            	);
            } else {
            	return Structures.create(
            		ImportItemResult.class,
            		Datatypes.member(ImportItemResult.Member.importedObject, null),
            		Datatypes.member(ImportItemResult.Member.status, Base.IMPORT_EXPORT_ITEM_NOT_VALID),
            		Datatypes.member(ImportItemResult.Member.statusMessage, Base.getInstance().analyseReport(errors))
            	);
            }
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }
        
}
