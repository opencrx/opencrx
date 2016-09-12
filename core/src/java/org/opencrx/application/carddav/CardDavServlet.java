/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CalDavServlet
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
package org.opencrx.application.carddav;

import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;

import org.opencrx.application.uses.net.sf.webdav.AbstractWebDavServlet;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.exceptions.WebdavException;
import org.opencrx.application.uses.net.sf.webdav.methods.WebDavMethod;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;

public class CardDavServlet extends AbstractWebDavServlet {

	//-----------------------------------------------------------------------
    @Override
    public void init(
        ServletConfig config            
    ) throws ServletException {
        super.init(config);        
    }

	//-----------------------------------------------------------------------
    private PersistenceManagerFactory getPersistenceManagerFactory(
    ) throws ServiceException, NamingException {
        if(this.persistenceManagerFactory == null) {                    
            Utils.getModel();
            this.persistenceManagerFactory = Utils.getPersistenceManagerFactory();
        }     
        return this.persistenceManagerFactory;
    }
    
	//-----------------------------------------------------------------------
	@Override
    protected WebDavStore constructStore(
    ) {
		try {
			return new CardDavStore(this.getPersistenceManagerFactory());
		} catch(Exception e) {
			throw new WebdavException("Unable to construct store", e);
		}
    }

	//-----------------------------------------------------------------------
	@Override
    protected WebDavMethod newDoPropfind(
    	WebDavStore store 
    ) {
		return new DoPropfind(store);
    }

	//-----------------------------------------------------------------------
	@Override
    protected WebDavMethod newDoOptions(
    	WebDavStore store 
    ) {
	    return new DoOptions(store);
    }

	//-----------------------------------------------------------------------
	@Override
    protected WebDavMethod newDoReport(
    	WebDavStore store 
    ) {
	    return new DoReport(
	    	store 
	    );
    }

	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------    
    private static final long serialVersionUID = 5694521485737189681L;

	protected PersistenceManagerFactory persistenceManagerFactory = null;
    
}
