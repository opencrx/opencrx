/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: GetAccountsAction
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

import java.util.List;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.AccountAddressQuery;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.persistence.cci.Queries;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Query_2Facade;

/**
 * GetContactsAction
 *
 */
public abstract class GetAccountsAction extends BpiAction {

	/**
	 * Extract address sub-queries from given query.
	 * 
	 * @param pm
	 * @param query
	 * @param addressQueries
	 * @return
	 * @throws ServiceException
	 */
	protected String getAddressQueries(
		PersistenceManager pm,
		String query,
		List<AccountAddressQuery> addressQueries
	) throws ServiceException {
        if(query != null) {
        	while(true) {
    	        Query_2Facade addressQueryFacade = Facades.newQuery(null);
    	        int pos1;
    	        int pos2;
    	        int pos3;
	        	if((pos1 = query.indexOf("thereExistsPostalAddress().")) >= 0) {
	        		pos2 = query.indexOf(".", pos1);
	        		pos3 = query.indexOf(";", pos1);
	        		addressQueryFacade.setQueryType("org:opencrx:kernel:account1:PostalAddress");
	        		addressQueryFacade.setQuery(query.substring(pos2 + 1, pos3));
	        	} else if((pos1 = query.indexOf("thereExistsEMailAddress().")) >= 0) {
	        		pos2 = query.indexOf(".", pos1);
	        		pos3 = query.indexOf(";", pos1);		        		
	        		addressQueryFacade.setQueryType("org:opencrx:kernel:account1:EMailAddress");
	        		addressQueryFacade.setQuery(query.substring(pos2 + 1, pos3));
	        	} else if((pos1 = query.indexOf("thereExistsPhoneNumber().")) >= 0) {
	        		pos2 = query.indexOf(".", pos1);		        		
	        		pos3 = query.indexOf(";", pos1);
	        		addressQueryFacade.setQueryType("org:opencrx:kernel:account1:PhoneNumber");
	        		addressQueryFacade.setQuery(query.substring(pos2 + 1, pos3));
	        	} else {
	        		break;
	        	}
		        addressQueries.add(
		        	(AccountAddressQuery)pm.newQuery(
			        	Queries.QUERY_LANGUAGE, 
			        	addressQueryFacade.getDelegate()
			        )
		        );
		        query = query.substring(0, pos1) + query.substring(pos3 + 1);
        	}
        }
        return query;
	}
    
}
