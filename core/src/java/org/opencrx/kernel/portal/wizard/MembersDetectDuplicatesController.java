/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MembersDetectDuplicatesController
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

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Member;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * MembersDetectDuplicatesController
 *
 */
public class MembersDetectDuplicatesController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public MembersDetectDuplicatesController(
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
	 */
	public void doRefresh(
	) {
		this.membersToDelete = new HashSet<Member>();		
	}
	
	/**
	 * OK action.
	 * 
	 */
	public void doOK(
	) throws ServiceException {
		this.doRefresh();
		Set<Path> accountXris = new HashSet<Path>();
		if (this.getObject() instanceof Account) {
			int sizeOfSet = 0;
			for(Member member: ((Account)this.getObject()).<Member>getMember()) {
				try {
					if(member.getAccount() != null) {
						accountXris.add(member.getAccount().refGetPath());
					}
				} catch (Exception ignore) {}
				if (accountXris.size() <= sizeOfSet) {
					// current account is duplicate
					this.membersToDelete.add(member);
				}
				sizeOfSet = accountXris.size();
			}
		}
	}

	/**
	 * @return the membersToDelete
	 */
	public Set<Member> getMembersToDelete() {
		return membersToDelete;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String MEMBER_CLASS = "org:opencrx:kernel:account1:Member";
	public static final String ACCOUNT_CLASS = "org:opencrx:kernel:account1:Account";
	public static final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
	public static final String GROUP_CLASS = "org:opencrx:kernel:account1:Group";
	public static final String INDENT = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
	
	private Set<Member> membersToDelete;	
}
