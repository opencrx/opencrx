/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ImportAddressesWizardController
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
package org.opencrx.kernel.portal.wizard;

import java.util.HashSet;
import java.util.Set;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.AddressFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.AddressGroup;
import org.opencrx.kernel.activity1.jmi1.AddressGroupMember;
import org.opencrx.kernel.backend.Base;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * ImportAddressesWizardController
 *
 */
public class ImportAddressesWizardController extends AbstractWizardController {

	/**
	 * ImportAddressesWizardController
	 */
	public ImportAddressesWizardController(
	) {
		super();
	}
	
	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(
	) {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

	/**
	 * Refresh action.
	 * 
	 * @param addressFilterXri
	 * @param countLimit
	 */
	public void doRefresh(
		@RequestParameter(name = "addressFilterXri") String addressFilterXri,
		@RequestParameter(name = "countLimit") Integer countLimit		
	) {
	}

	/**
	 * OK action.
	 * 
	 * @param addressFilterXri
	 * @param countLimit
	 * @throws ServiceException
	 */
	public void doOK(
		@RequestParameter(name = "addressFilterXri") String addressFilterXri,
		@RequestParameter(name = "countLimit") Integer countLimit		
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			addressFilterXri,
			countLimit
		);
		if(this.getObject() instanceof AddressGroup && addressFilterXri != null) {
			AddressGroup addressGroup = (AddressGroup)this.getObject();
			Set<Path> existingAddresses = new HashSet<Path>();
			for(AddressGroupMember member: addressGroup.<AddressGroupMember>getMember()) {
				existingAddresses.add(member.getAddress().refGetPath());
			}
			AddressFilterGlobal addressFilter = (AddressFilterGlobal)pm.getObjectById(new Path(addressFilterXri));
			if(countLimit != null && countLimit > 0) {
				pm.currentTransaction().begin();
				int count = 0;
				for(AccountAddress address: addressFilter.<AccountAddress>getFilteredAddress()) {
					// do not import duplicates				
					if(!existingAddresses.contains(address.refGetPath())) {
						AddressGroupMember member = pm.newInstance(AddressGroupMember.class);
						member.setAddress(address);
						addressGroup.addMember(
							Base.getInstance().getUidAsString(),
							member
					    );
						count++;
						if(count >= countLimit) {
							break;
						}
						if(count % 100 == 0) {
							pm.currentTransaction().commit();
							pm.currentTransaction().begin();
						}
					}
				}
				pm.currentTransaction().commit();
			}
			this.setExitAction(
				new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()				
			);
		}
	}

}
