/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AddressGroupRemoveDuplicateMembersController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.activity1.jmi1.AddressGroup;
import org.opencrx.kernel.activity1.jmi1.AddressGroupMember;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * AddressGroupRemoveDuplicateMembersController
 *
 */
public class AddressGroupRemoveDuplicateMembersController extends AbstractWizardController {

	public AddressGroupRemoveDuplicateMembersController(
	) {
		super();
	}
	
	/**
	 * OK action.
	 * 
	 * @throws ServiceException
	 */
	public void doOK(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		RefObject_1_0 obj = this.getObject();
		if(obj instanceof AddressGroup) {
			AddressGroup addressGroup = (AddressGroup)obj;
			Set<Path> addressXris = new HashSet<Path>();
			Set<String> emailAddresses = new HashSet<String>();
			Set<String> phoneNumbers = new HashSet<String>();
			Set<AddressGroupMember> membersToDelete = new HashSet<AddressGroupMember>();
			for(AddressGroupMember addressGroupMember: addressGroup.<AddressGroupMember>getMember()) {
				AccountAddress address = addressGroupMember.getAddress();
				if(addressXris.contains(address.refGetPath())) {
					membersToDelete.add(addressGroupMember);
				} else {
					addressXris.add(address.refGetPath());
				}
				if(address instanceof EMailAddress) {
					EMailAddress eMailAddress = (EMailAddress)address;
					if(eMailAddress.getEmailAddress() != null) {
						// current e-mail address is duplicate
						if(emailAddresses.contains(eMailAddress.getEmailAddress())) {
							membersToDelete.add(addressGroupMember);
						} else {
							emailAddresses.add(eMailAddress.getEmailAddress());							
						}
					}
				}
				if(address instanceof PhoneNumber) {
					PhoneNumber phoneNumber = (PhoneNumber)address;
					if(phoneNumber.getPhoneNumberFull() != null) {
						if(phoneNumbers.contains(phoneNumber.getPhoneNumberFull())) {
							membersToDelete.add(addressGroupMember);
						} else {
							phoneNumbers.add(phoneNumber.getPhoneNumberFull());
						}
					}
				}
			}
			for(AddressGroupMember addressGroupMember: membersToDelete) {
				try {
					pm.currentTransaction().begin();
					addressGroupMember.setDisabled(true);
					pm.currentTransaction().commit();
				} catch (Exception e) {
					new ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch (Exception ignore) {}
				}
			}
		}
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

}
