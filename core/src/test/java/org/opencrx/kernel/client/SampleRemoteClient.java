/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SimpleRemoteClient
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2021, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.client;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;

import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;

/**
 * Simple remote openCRX client program. This client remotely
 * connects to an openCRX server instance.
 * 
 */
public class SampleRemoteClient {

	public static void main(
		String[] args
	) throws NamingException, ServiceException {
		String connectionUrl = "http://127.0.0.1:8080/opencrx-rest-CRX/";
		String userName = "admin-Standard";
		String password = "admin-Standard";
		PersistenceManagerFactory pmf = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactoryProxy(
			connectionUrl,
			userName,
			password,
			"application/vnd.openmdx.wbxml" // or 'text/xml' for plain xml protocol
		);
		PersistenceManager pm = pmf.getPersistenceManager(userName, null);
		org.opencrx.kernel.account1.jmi1.Segment accountSegment = 
			(org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/CRX/segment/Standard")
			);
		org.opencrx.kernel.account1.cci2.ContactQuery contactQuery = 
			(org.opencrx.kernel.account1.cci2.ContactQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Contact.class);
		contactQuery.orderByFullName().ascending();
		contactQuery.thereExistsFullName().like("G.*");
		int count = 0;
		for (org.opencrx.kernel.account1.jmi1.Contact contact : accountSegment.<org.opencrx.kernel.account1.jmi1.Contact> getAccount(contactQuery)) {
			System.out.println(contact.refGetPath().toXRI() + ": " + contact.getFullName());
			count++;
			if (count > 100) {
				break;
			}
		}
		pm.close();
	}

}
