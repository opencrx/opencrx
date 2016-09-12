/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MergeMembersController
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

import java.util.ArrayList;
import java.util.List;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.backend.Base;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * MergeMembersController
 *
 */
public class MergeMembersController extends AbstractWizardController {

	/**
	 * FormFields
	 * 
	 */
	public static class FormFields {
		/**
		 * @return the accountTargetXriTitle
		 */
		public String getAccountTargetXriTitle() {
			return accountTargetXriTitle;
		}
		/**
		 * @param accountTargetXriTitle the accountTargetXriTitle to set
		 */
		public void setAccountTargetXriTitle(String accountTargetXriTitle) {
			this.accountTargetXriTitle = accountTargetXriTitle;
		}
		/**
		 * @return the accountTargetXri
		 */
		public String getAccountTargetXri() {
			return accountTargetXri;
		}
		/**
		 * @param accountTargetXri the accountTargetXri to set
		 */
		public void setAccountTargetXri(String accountTargetXri) {
			this.accountTargetXri = accountTargetXri;
		}
		/**
		 * @return the includeDisabledMembers
		 */
		public Boolean getIncludeDisabledMembers() {
			return includeDisabledMembers;
		}
		/**
		 * @param includeDisabledMembers the includeDisabledMembers to set
		 */
		public void setIncludeDisabledMembers(Boolean includeDisabledMembers) {
			this.includeDisabledMembers = includeDisabledMembers;
		}
		/**
		 * @return the includeDisabledAccounts
		 */
		public Boolean getIncludeDisabledAccounts() {
			return includeDisabledAccounts;
		}
		/**
		 * @param includeDisabledAccounts the includeDisabledAccounts to set
		 */
		public void setIncludeDisabledAccounts(Boolean includeDisabledAccounts) {
			this.includeDisabledAccounts = includeDisabledAccounts;
		}
		String accountTargetXriTitle;
		String accountTargetXri;
		Boolean includeDisabledMembers;
		Boolean includeDisabledAccounts;		
	}
	
	/**
	 * MergedMember
	 *
	 */
	public static class MergedMember {	
		/**
		 * @return the info
		 */
		public String getInfo() {
			return info;
		}
		/**
		 * @param info the info to set
		 */
		public void setInfo(String info) {
			this.info = info;
		}
		/**
		 * @return the memberHref
		 */
		public String getMemberHref() {
			return memberHref;
		}
		/**
		 * @param memberHref the memberHref to set
		 */
		public void setMemberHref(String memberHref) {
			this.memberHref = memberHref;
		}
		/**
		 * @return the newMemberCreated
		 */
		public boolean isNewMemberCreated() {
			return newMemberCreated;
		}
		/**
		 * @param newMemberCreated the newMemberCreated to set
		 */
		public void setNewMemberCreated(boolean newMemberCreated) {
			this.newMemberCreated = newMemberCreated;
		}
		/**
		 * @return the member
		 */
		public Member getMember() {
			return member;
		}
		/**
		 * @param member the member to set
		 */
		public void setMember(Member member) {
			this.member = member;
		}
		/**
		 * @return the account
		 */
		public Account getAccount() {
			return account;
		}
		/**
		 * @param account the account to set
		 */
		public void setAccount(Account account) {
			this.account = account;
		}

		String info;
		String memberHref;
		Member member;
		Account account;
		boolean newMemberCreated;
	}

	/**
	 * Constructor.
	 * 
	 */
	public MergeMembersController(
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
	 * @param formFields
	 */
	public void doRefresh(
		@RequestParameter(type = "Bean") FormFields formFields
	) {
		PersistenceManager pm = this.getPm();
		this.formFields = formFields;
		this.accountTarget = null;
		this.accountTargetXri = null;
		this.accountTargetXriTitle = formFields.getAccountTargetXriTitle();
		if(formFields.getAccountTargetXri() != null && !formFields.getAccountTargetXri().isEmpty()) {
			try {
				this.accountTargetXri = new Path(formFields.getAccountTargetXri());
				this.accountTarget = (Account)pm.getObjectById(this.accountTargetXri);
			} catch (Exception ignore) {}
		}
		if(this.accountTargetXriTitle == null || this.accountTargetXriTitle.trim().isEmpty()) {
			this.accountTargetXri = null;
			this.accountTarget = null;
		}
	}

	/**
	 * OK action.
	 * 
	 * @param formFields
	 */
	public void doOK(
		@RequestParameter(type = "Bean") FormFields formFields
	) {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.doRefresh(formFields);
		this.mergedMembers = new ArrayList<MergedMember>();
		if(this.accountTarget != null) {
			Account accountSource = (Account)this.getObject();
			for(Member member: accountSource.<Member>getMember()) {
				MergedMember mergedMember = new MergedMember();
				mergedMember.setInfo("");
				mergedMember.setMemberHref("");
				boolean createNewMember = false;
				try {
					mergedMember.setInfo("---");
					Account memberAccount = member.getAccount();
					if(memberAccount != null) {
						mergedMember.setInfo((new ObjectReference(memberAccount, app)).getTitle());
						Action action = new ObjectReference(
							member,
							app
						).getSelectObjectAction();
						mergedMember.setMemberHref(action.getEncodedHRef());
						if (
							(Boolean.TRUE.equals(formFields.getIncludeDisabledMembers()) || (member != null && !Boolean.TRUE.equals(member.isDisabled()))) &&
							(Boolean.TRUE.equals(formFields.getIncludeDisabledAccounts()) || (memberAccount != null && !Boolean.TRUE.equals(memberAccount.isDisabled())))
						) {
							createNewMember = true;
						}
					}
					if(createNewMember && (memberAccount != null) && !this.accountTarget.equals(memberAccount)) {
						mergedMember.setInfo(mergedMember.getInfo() + " (added)");
						pm.currentTransaction().begin();
						Member newMember = pm.newInstance(Member.class);
						newMember.getForUseBy().addAll(member.getForUseBy());
						newMember.setName(member.getName());
						newMember.setDescription(member.getDescription());
						newMember.setMemberRole(member.getMemberRole());
						newMember.setValidFrom(new java.util.Date());
						newMember.setAccount(memberAccount);
						newMember.setQuality((short)5); // normal
						this.accountTarget.addMember(
							Base.getInstance().getUidAsString(),
							newMember
						);
						pm.currentTransaction().commit();
						mergedMember.setNewMemberCreated(true);
					} else {
						mergedMember.setNewMemberCreated(false);
						mergedMember.setInfo(mergedMember.getInfo() + " (NOT added)");												
					}
					mergedMember.setMember(member);
					mergedMember.setAccount(memberAccount);
					this.mergedMembers.add(mergedMember);
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
				}
			}
		}
	}

	/**
	 * @return the formFields
	 */
	public FormFields getFormFields() {
		return formFields;
	}

	/**
	 * @return the accountTargetXri
	 */
	public Path getAccountTargetXri() {
		return accountTargetXri;
	}

	/**
	 * @return the accountTarget
	 */
	public Account getAccountTarget() {
		return accountTarget;
	}

	/**
	 * @return the accountTargetXriTitle
	 */
	public String getAccountTargetXriTitle() {
		return accountTargetXriTitle;
	}

	/**
	 * @return the mergedMembers
	 */
	public List<MergedMember> getMergedMembers() {
		return mergedMembers;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String MEMBER_CLASS = "org:opencrx:kernel:account1:Member";
	public static final String ACCOUNT_CLASS = "org:opencrx:kernel:account1:Account";
	public static final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
	public static final String GROUP_CLASS = "org:opencrx:kernel:account1:Group";
	public static final String ACCOUNT_REFERENCE = "org:opencrx:kernel:account1:Segment:account";

	private FormFields formFields;
	private Path accountTargetXri;
	private String accountTargetXriTitle;
	private Account accountTarget;
	private List<MergedMember> mergedMembers;

}
