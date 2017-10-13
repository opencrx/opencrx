/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ImporterTaskImpl
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011-2016, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.workflow1.aop2;

import java.util.ArrayList;
import java.util.List;

import javax.jdo.listener.DeleteCallback;
import javax.jdo.listener.StoreCallback;

import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.workflow1.jmi1.ImporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunImportExportParams;
import org.opencrx.kernel.workflow1.jmi1.RunImportResult;
import org.openmdx.base.accessor.jmi.cci.JmiServiceException;
import org.openmdx.base.aop2.AbstractObject;

/**
 * ImporterTaskImpl
 * 
 */
public class ImporterTaskImpl
	<S extends org.opencrx.kernel.workflow1.jmi1.ImporterTask,N extends org.opencrx.kernel.workflow1.cci2.ImporterTask,C extends Void>
	extends AbstractObject<S,N,C>
	implements StoreCallback, DeleteCallback {

	public ImporterTaskImpl(S same, N next) {
		super(same, next);
	}

	/**
	 * Run export.
	 * 
	 * @return
	 */
	public RunImportResult runImport(
		RunImportExportParams params			
	) {
        try {
        	ImporterTask importerTask = this.sameObject();
        	List<String> importParams = new ArrayList<String>();
        	importParams.add(params.getParam0());
        	importParams.add(params.getParam1());
        	importParams.add(params.getParam2());
        	importParams.add(params.getParam3());
        	importParams.add(params.getParam4());
        	importParams.add(params.getParam5());
        	importParams.add(params.getParam6());
        	importParams.add(params.getParam7());
        	importParams.add(params.getParam8());
        	importParams.add(params.getParam9());        	
        	return Workflows.getInstance().runImport(
        		importerTask,
        		importParams
        	);
        } catch(Exception e) {
            throw new JmiServiceException(e);
        }
	}

	/* (non-Javadoc)
	 * @see org.openmdx.base.aop2.AbstractObject#jdoPreStore()
	 */
	@Override
    public void jdoPreStore(
    ) {
		super.jdoPreStore();
    }
    
    /* (non-Javadoc)
     * @see org.openmdx.base.aop2.AbstractObject#jdoPreDelete()
     */
    @Override
    public void jdoPreDelete(
    ) {
    	super.jdoPreDelete();
    }
    
	//------------------------------------------------------------------------
	// Members
	//------------------------------------------------------------------------
}
