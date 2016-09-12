/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: GetAddressGroupMembersAction
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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
package org.opencrx.application.bpi.adapter;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.jdo.PersistenceManager;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.bpi.datatype.BpiAddress;
import org.opencrx.kernel.account1.cci2.AccountAddressQuery;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.activity1.cci2.AddressGroupMemberQuery;
import org.opencrx.kernel.activity1.jmi1.AddressGroup;
import org.opencrx.kernel.activity1.jmi1.AddressGroupMember;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.persistence.cci.Queries;
import org.openmdx.base.rest.spi.Facades;
import org.openmdx.base.rest.spi.Query_2Facade;

/**
 * GetAddressGroupMembersAction
 *
 */
public class GetAddressGroupMembersAction extends BpiAction {

	/* (non-Javadoc)
	 * @see org.opencrx.application.bpi.adapter.BpiAdapterServlet.Action#handle(org.openmdx.base.naming.Path, javax.jdo.PersistenceManager, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
	 */
	@Override
    public void perform(
    	Path path, 
    	PersistenceManager pm,
    	BpiPlugIn plugIn,    	
    	HttpServletRequest req, 
    	HttpServletResponse resp
    ) throws IOException, ServiceException {
    	List<AddressGroup> addressGroups = plugIn.findAddressGroups(path.getPrefix(7), pm);
    	if(addressGroups == null || addressGroups.isEmpty()) {
    		resp.setStatus(HttpServletResponse.SC_NOT_FOUND); 
    	} else {
			try {
				AddressGroup addressGroup = addressGroups.iterator().next();
		        Query_2Facade queryFacade = Facades.newQuery(
		        	addressGroup.refGetPath().getChild("member")
		        );
		        String queryType = req.getParameter("queryType");
		        queryFacade.setQueryType(
		        	queryType == null ? "org:opencrx:kernel:account1:AccountAddress" : queryType
		        );
		        String query = req.getParameter("query");
		        queryFacade.setQuery(query);
		        String position = req.getParameter("position");
		        queryFacade.setPosition(
		            position == null ? Integer.valueOf(DEFAULT_POSITION) : Integer.valueOf(position)
		        );
		        String size = req.getParameter("size");
		        queryFacade.setSize(
		            Integer.valueOf(size == null ? DEFAULT_SIZE : Integer.parseInt(size))
		        );
		        AccountAddressQuery accountAddressQuery = (AccountAddressQuery)pm.newQuery(
		        	Queries.QUERY_LANGUAGE, 
		        	queryFacade.getDelegate()
		        );
	    		resp.setCharacterEncoding("UTF-8");
	    		resp.setContentType("application/json");
	    		AddressGroupMemberQuery addressGroupMemberQuery = (AddressGroupMemberQuery)pm.newQuery(AddressGroupMember.class);
	    		addressGroupMemberQuery.forAllDisabled().isFalse();
	    		addressGroupMemberQuery.thereExistsAddress().elementOf(
	    			PersistenceHelper.asSubquery(accountAddressQuery)
	    		);
	    		List<BpiAddress> bpiAddresses = new ArrayList<BpiAddress>();
	    		int count = 0;
	    		for(Iterator<AddressGroupMember> i = addressGroup.<AddressGroupMember>getMember(addressGroupMemberQuery).listIterator(queryFacade.getPosition().intValue()); i.hasNext(); ) {
	    			AddressGroupMember member = i.next();
	    			if(member.getAddress() != null) {
	    				AccountAddress address = member.getAddress();
	    				if(address instanceof EMailAddress) {
	    					bpiAddresses.add(
	    						plugIn.toBpiEMailAddress(
	    							(EMailAddress)address, 
	    							this.getFetchGroup(req)
	    						)
	    					);
	    				} else if(address instanceof PostalAddress) {
	    					bpiAddresses.add(
	    						plugIn.toBpiPostalAddress(
	    							(PostalAddress)address,
	    							this.getFetchGroup(req)
	    						)
	    					);    					
	    				} else if(address instanceof PhoneNumber) {
	    					bpiAddresses.add(
	    						plugIn.toBpiPhoneNumber(
	    							(PhoneNumber)address,
	    							this.getFetchGroup(req)
	    						)
	    					);    					
	    				}
	    			}
	    			count++;
	    			if(count > queryFacade.getSize()) {
	    				break;
	    			}
	    		}
	    		PrintWriter pw = resp.getWriter();
	    		plugIn.printObject(pw, bpiAddresses);
	    		resp.setStatus(HttpServletResponse.SC_OK);
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
