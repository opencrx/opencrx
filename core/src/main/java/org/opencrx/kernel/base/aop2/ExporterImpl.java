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

import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.base.jmi1.ExportItemResult;
import org.opencrx.kernel.workflow1.jmi1.RunExportResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;
import org.openmdx.base.exception.ServiceException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

public class ExporterImpl
	<S extends org.opencrx.kernel.base.jmi1.Exporter,N extends org.opencrx.kernel.base.cci2.Exporter,C extends Void>
	extends AbstractObject<S,N,C> {

    public ExporterImpl(
        S same,
        N next
    ) {
    	super(same, next);
    }

    /**
     * Export item.
     * 
     * @param params
     * @return
     */
    public ExportItemResult exportItem(
        org.opencrx.kernel.base.jmi1.ExportItemParams params
    ) {
        try {
        	List<String> runExportParams = new ArrayList<String>();
        	runExportParams.add(this.sameObject().refGetPath().toXRI());
            RunExportResult result = Workflows.getInstance().runExport(
            	params.getExporterTask(),
            	runExportParams
            );
            return Structures.create(
            	ExportItemResult.class, 
            	Datatypes.member(ExportItemResult.Member.item, result.getFile()),
            	Datatypes.member(ExportItemResult.Member.itemMimeType, result.getFileMimeType()),
            	Datatypes.member(ExportItemResult.Member.itemName, result.getFileName()),
            	Datatypes.member(ExportItemResult.Member.status, result.getStatus()),
            	Datatypes.member(ExportItemResult.Member.statusMessage, result.getStatusMessage())            	
            );
        } catch(ServiceException e) {
            throw new JmiServiceException(e);
        }
    }

}
