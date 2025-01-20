/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: RunExportAction
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
package org.opencrx.application.bpi.adapter;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

import javax.jdo.PersistenceManager;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.opencrx.kernel.workflow1.jmi1.ExporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunExportResult;
import org.opencrx.kernel.workflow1.jmi1.RunImportExportParams;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * Run Export Action.
 *
 */
public class RunExportAction extends BpiAction {

	/* (non-Javadoc)
	 * @see org.opencrx.application.bpi.adapter.BpiAdapterServlet.Action#handle(org.openmdx.base.naming.Path, javax.jdo.PersistenceManager, jakarta.servlet.http.HttpServletRequest, jakarta.servlet.http.HttpServletResponse)
	 */
	@Override
    public void perform(
    	Path path, PersistenceManager pm,
    	BpiPlugIn plugIn,    	
    	HttpServletRequest req, 
    	HttpServletResponse resp
    ) throws IOException, ServiceException {
    	List<ExporterTask> exporterTasks = plugIn.findExporterTasks(path, pm);
    	if(exporterTasks == null || exporterTasks.isEmpty()) {
    		resp.setStatus(HttpServletResponse.SC_NOT_FOUND); 
    	} else {
    		try {
	    		ExporterTask exporterTask = exporterTasks.iterator().next();
	    		RunImportExportParams runExportParams = Structures.create(
	    			RunImportExportParams.class,
    				Datatypes.member(RunImportExportParams.Member.param0, req.getParameter("param0")),
    				Datatypes.member(RunImportExportParams.Member.param1, req.getParameter("param1")),			
    				Datatypes.member(RunImportExportParams.Member.param2, req.getParameter("param2")),			
    				Datatypes.member(RunImportExportParams.Member.param3, req.getParameter("param3")),			
    				Datatypes.member(RunImportExportParams.Member.param4, req.getParameter("param4")),	
    				Datatypes.member(RunImportExportParams.Member.param5, req.getParameter("param5")),	
    				Datatypes.member(RunImportExportParams.Member.param6, req.getParameter("param6")),
    				Datatypes.member(RunImportExportParams.Member.param7, req.getParameter("param7")),
    				Datatypes.member(RunImportExportParams.Member.param8, req.getParameter("param8")),
    				Datatypes.member(RunImportExportParams.Member.param9, req.getParameter("param9"))
    			);
	    		RunExportResult result = exporterTask.runExport(runExportParams);
	    		resp.setContentType(result.getFileMimeType());
	    		resp.setHeader("Content-disposition", "attachment;filename=" + path.getLastSegment().toString());
	    		byte[] source = result.getFile();
	    		OutputStream target = resp.getOutputStream();
	    		BinaryLargeObjects.streamCopy(BinaryLargeObjects.valueOf(result.getFile()).getContent(), 0L, target);
	    		resp.setContentLength(source.length);
    		} catch(Exception e) {
        		resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);	    			
    			new ServiceException(e).log();
    			try {
    				pm.currentTransaction().rollback();
    			} catch(Exception ignore) {}
    		}
    	}	    
    }

}
