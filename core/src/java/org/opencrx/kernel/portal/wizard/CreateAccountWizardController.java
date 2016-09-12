/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateAccountWizardController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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

import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.portal.EmailAddressDataBinding;
import org.opencrx.kernel.portal.PhoneNumberDataBinding;
import org.opencrx.kernel.portal.PostalAddressDataBinding;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DataBinding;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;

/**
 * CreateAccountWizardController
 *
 */
public abstract class CreateAccountWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateAccountWizardController(
   	) {
   		super();
   	}

	/**
	 * Map form fields to account. Override this method in custom projects.
	 * 
	 * @param contact
	 * @param formFields
	 */
	abstract protected void mapToAccount(
		Account account,
		Map<String,Object> formFields
	);

	/**
	 * Map form fields to contact addresses. Override this method in custom projects.
	 * 
	 * @param contact
	 * @param formFields
	 */
	protected void mapToAddresses(
		Account account,
		Map<String,Object> formFields
	) {
		PersistenceManager pm = this.getPm();
	    // Phone Business
	    DataBinding phoneBusinessDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)500;automaticParsing=(boolean)true");
	    phoneBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull")
	    );
	    // Phone Mobile
	    DataBinding phoneMobileDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)200;automaticParsing=(boolean)true");
	    phoneMobileDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull")
	    );
	    // Mail Business
	    String emailBusiness = (String)formFields.get("org:opencrx:kernel:account1:Account:address*Business!emailAddress");
	    if (emailBusiness != null) {emailBusiness = emailBusiness.trim();}
	    DataBinding mailBusinessDataBinding = new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)500;[emailType=(short)1]");
	    mailBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!emailAddress",
	        emailBusiness
	    );
	    // Postal Business
	    DataBinding postalBusinessDataBinding = new PostalAddressDataBinding("[isMain=(boolean)true];usage=(short)500?zeroAsNull=true");
	    postalBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!postalAddressLine",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalAddressLine")
	    );
	    postalBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!postalStreet",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalStreet")
	    );
	    postalBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!postalCity",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCity")
	    );
	    postalBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!postalState",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalState")
	    );
	    postalBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!postalCode",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCode")
	    );
	    postalBusinessDataBinding.setValue(
	    	account,
	        "org:opencrx:kernel:account1:Account:address*Business!postalCountry",
	        formFields.get("org:opencrx:kernel:account1:Account:address*Business!postalCountry")
	    );
	    try {
		    postalBusinessDataBinding.setValue(
		    	account,
		        "org:opencrx:kernel:account1:Account:address*Business!authority",
		        formFields.get("org:opencrx:kernel:account1:Account:address*Business!authority") == null 
		        	? null 
		        	: pm.getObjectById(formFields.get("org:opencrx:kernel:account1:Account:address*Business!authority"))
		    );
	    } catch(Exception ignore) {}
	}

	/**
	 * Map form fields to member.
	 * 
	 * @param member
	 * @param formFields
	 */
	protected void mapToMember(
		Member member,
		Map<String,Object> formFields
	) {
        Object memberRole = formFields.get("org:opencrx:kernel:account1:Member:memberRole");
        if(memberRole instanceof Short) {
        	member.setMemberRole(Collections.singletonList((Short)memberRole));
        } else {
        	@SuppressWarnings("unchecked")
            List<Short> memberRoles = (List<Short>)memberRole;
        	member.setMemberRole(memberRoles);
        }
	}

	/**
	 * Map form fields to membership.
	 * 
	 * @param member
	 * @param formFields
	 */
	protected void mapToMembership(
		Member member,
		Map<String,Object> formFields
	) {
        Object memberRole = formFields.get("org:opencrx:kernel:account1:AccountMembership:memberRole");	
        if(memberRole instanceof Short) {
        	member.setMemberRole(Collections.singletonList((Short)memberRole));
        } else {
        	@SuppressWarnings("unchecked")
            List<Short> memberRoles = (List<Short>)memberRole;
        	member.setMemberRole(memberRoles);
        }
	}

	/**
	 * Init alias name for contact. In case aliasName is not set in formFields 
	 * this implementation sets the next free number. Override this method in custom projects.
	 *  
	 * @param formFields
	 * @return
	 */
	protected void initAliasName(
		org.opencrx.kernel.account1.jmi1.Segment accountSegment,
		Map<String,Object> formFields		
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
	    String aliasName = (String)formFields.get("org:opencrx:kernel:account1:Account:aliasName");
	    if(aliasName == null) {
	        AccountQuery accountQuery = (AccountQuery)pm.newQuery(Account.class);
	        accountQuery.thereExistsAliasName().greaterThanOrEqualTo("A00000000");
	        accountQuery.thereExistsAliasName().lessThanOrEqualTo("A99999999");
	        accountQuery.orderByAliasName().descending();
	        List<Account> accounts = accountSegment.getAccount(accountQuery);
	        if(accounts.isEmpty()) {
	            aliasName = "A00000000";
	        } else {
	            String lastAliasName = ((Account)accounts.iterator().next()).getAliasName();
	            String nextAccountNumber = "00000000" + (Integer.valueOf(lastAliasName.substring(1)).intValue() + 1);
	            aliasName = "A" + nextAccountNumber.substring(nextAccountNumber.length() - 8);
	        }
	        formFields.put(
                "org:opencrx:kernel:account1:Account:aliasName",
                aliasName
	        );
	    }
	}

	/**
	 * Update account.
	 * 
	 * @param account
	 * @param formFields
	 * @param isAddMembershipMode
	 * @param membershipXri
	 * @throws ServiceException
	 */
	protected void updateAccount(
		Account account,
		Map<String,Object> formFields,
		Boolean isAddMemberMode,
		String memberXri,		
		Boolean isAddMembershipMode,
		String membershipXri
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		try {
		    org.opencrx.kernel.account1.jmi1.Segment accountSegment = (org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
		    	new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", this.getProviderName(), "segment", this.getSegmentName())
		    );
		    this.initAliasName(
		    	accountSegment,
		    	formFields
		    );
		    pm.currentTransaction().begin();
		    this.mapToAccount(
		    	account, 
		    	formFields
		    );
		    if(!javax.jdo.JDOHelper.isPersistent(account)) {
			    accountSegment.addAccount(
			        Base.getInstance().getUidAsString(),
			        account
			    );
		    }
		    pm.currentTransaction().commit();
		} catch (Exception e) {
			new ServiceException(e).log();
			this.errorMsg = "ERROR - cannot create/save contact";
			Throwable err = e;
			while (err.getCause() != null) {
				err = err.getCause();
			}
			this.errorTitle += "<pre>" + err + "</pre>";
			try {
				pm.currentTransaction().rollback();
			} catch (Exception ignore) {}
		}
	    // Update Addresses
	    try {
		    pm.currentTransaction().begin();
		    this.mapToAddresses(
		    	account, 
		    	formFields
		    );
		    pm.currentTransaction().commit();
		} catch (Exception e) {
			new ServiceException(e).log();
			this.errorMsg = "ERROR - cannot create/save contact";
			Throwable err = e;
			while (err.getCause() != null) {
				err = err.getCause();
			}
			this.errorTitle += "<pre>" + err + "</pre>";
			try {
				pm.currentTransaction().rollback();
			} catch (Exception ignore) {}
		}
		// Create/update data for member
	    if(
	    	Boolean.TRUE.equals(isAddMemberMode) || 
	    	(memberXri != null && !memberXri.isEmpty())
	    ) {
			try {
				Member member = null;
				Account accountTo = null;
				try {
					accountTo = this.formFields.get("org:opencrx:kernel:account1:AccountAssignment:account") != null ?
						(Account)pm.getObjectById(
							this.formFields.get("org:opencrx:kernel:account1:AccountAssignment:account")
						) : null;
				} catch (Exception e) {}
				if (accountTo != null) {
					if(memberXri != null && !memberXri.isEmpty()) {
						// update existing member
						member = (Member)pm.getObjectById(new Path(memberXri));
					} else {
						// create new member
						try {
							pm.currentTransaction().begin();
							member = this.newMemberInstance();
							member.setValidFrom(new java.util.Date());
							member.setQuality((short)5);
							member.setName((String)this.formFields.get("org:opencrx:kernel:account1:Member:name"));
							if (member.getName() == null || member.getName().length() == 0) {
								member.setName(accountTo.getFullName());
							}
							member.setAccount(accountTo);
							account.addMember(
								Base.getInstance().getUidAsString(),
								member
							);
							pm.currentTransaction().commit();
						} catch (Exception e) {
							member = null;
							new ServiceException(e).log();
							this.errorMsg = "ERROR - cannot create/save member";
							Throwable err = e;
							while (err.getCause() != null) {
								err = err.getCause();
							}
							this.errorTitle += "<pre>" + err + "</pre>";
							try {
								pm.currentTransaction().rollback();
							} catch (Exception er) {}
						}
					}
					try {
						pm.currentTransaction().begin();
						member.setAccount(accountTo);
						member.setName((String)this.formFields.get("org:opencrx:kernel:account1:Member:name"));
						if (member.getName() == null || member.getName().isEmpty()) {
							member.setName(accountTo.getFullName());
						}
						member.setDescription((String)this.formFields.get("org:opencrx:kernel:account1:Member:description"));
						pm.currentTransaction().commit();
					} catch (Exception e) {
						member = null;
						new ServiceException(e).log();
						try {
							pm.currentTransaction().rollback();
						} catch (Exception er) {}
					}
					if(member != null && this.formFields.get("org:opencrx:kernel:account1:Member:memberRole") != null) {
						try {
                            pm.currentTransaction().begin();
							this.mapToMember(
								member, 
								formFields
							);
							pm.currentTransaction().commit();
						} catch (Exception e) {
							member = null;
							new ServiceException(e).log();
							this.errorMsg = "ERROR - cannot create/save member";
							Throwable err = e;
							while (err.getCause() != null) {
								err = err.getCause();
							}
							this.errorTitle += "<pre>" + err + "</pre>";
							try {
								pm.currentTransaction().rollback();
							} catch (Exception er) {}
						}
					}
				}
			} catch (Exception e) {
				new ServiceException(e).log();
			}
	    }	   	
		// Create/update data for membership
	    if(
	    	Boolean.TRUE.equals(isAddMembershipMode) || 
	    	(membershipXri != null && !membershipXri.isEmpty())
	    ) {
			try {
				Account parentAccount = null;
				Member membership = null;
				if(membershipXri != null && !membershipXri.isEmpty()) {
					membership = (Member)pm.getObjectById(new Path(membershipXri));
					parentAccount = (Account)pm.getObjectById(membership.refGetPath().getParent().getParent());
				} else {
					// create new membership
					// get parent account
					try {
						parentAccount = formFields.get("org:opencrx:kernel:account1:AccountAssignment:account") != null 
							? (Account)pm.getObjectById(
								formFields.get("org:opencrx:kernel:account1:AccountAssignment:account")
							  ) 
							: null;
					} catch (Exception e) {}
					if (parentAccount != null) {
						// create new member
						try {
							pm.currentTransaction().begin();
							membership = this.newMembershipInstance();
							membership.setValidFrom(new java.util.Date());
							membership.setQuality((short)5);
							membership.setName(account.getFullName());
							membership.setAccount(account);
							parentAccount.addMember(
								Base.getInstance().getUidAsString(),
								membership
							);
							pm.currentTransaction().commit();
						} catch (Exception e) {
							membership = null;
							new ServiceException(e).log();
							this.errorMsg = "ERROR - cannot create/save member";
							Throwable err = e;
							while (err.getCause() != null) {
								err = err.getCause();
							}
							this.errorTitle += "<pre>" + err + "</pre>";
							try {
								pm.currentTransaction().rollback();
							} catch (Exception er) {}
						}
					}
				}
				if(membership != null && formFields.get("org:opencrx:kernel:account1:AccountMembership:memberRole") != null) {
					// update member roles
					try {
						pm.currentTransaction().begin();
						this.mapToMembership(
							membership, 
							formFields
						);
						pm.currentTransaction().commit();
					} catch (Exception e) {
						membership = null;
						new ServiceException(e).log();
						this.errorMsg = "ERROR - cannot create/save member";
						Throwable err = e;
						while (err.getCause() != null) {
							err = err.getCause();
						}
						this.errorTitle += "<pre>" + err + "</pre>";
						try {
							pm.currentTransaction().rollback();
						} catch (Exception er) {}
					}
				}
			} catch (Exception e) {
				new ServiceException(e).log();
			}
	    }
	    // Touch account - this updates all derived information
		try {
			pm.currentTransaction().begin();
			Utils.touchObject(account);
			pm.currentTransaction().commit();
		} catch (Exception e) {
			new ServiceException(e).log();
		}
	}

	abstract protected Account newAccountInstance(
	) throws ServiceException;

	/**
	 * Get new Member instance. Override this method to return a 
	 * custom-specific Member class
	 * 
	 * @return
	 */
	protected Member newMemberInstance(
	) {
		return this.getPm().newInstance(Member.class);		
	}

	/**
	 * Get new Membership instance. Override this method to return
	 * a custom-specific Member class.
	 * 
	 * @return
	 */
	protected Member newMembershipInstance(
	) {
		return this.getPm().newInstance(Member.class);		
	}

	/**
	 * This method is called by doOK to check whether updateAccount() is allowed to 
	 * be invoked. By default this method returns false in case doRefresh() has reported errors.
	 * Override this method for custom-specific behavior.
	 *
	 * @return
	 */
	protected boolean isValidForUpdate(
	) {
		return this.errorMsg == null || this.errorMsg.isEmpty();
	}

	/**
	 * Refresh action.
	 * 
	 * @param accountXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doRefresh(
   	   	@RequestParameter(name = "accountXri") String accountXri,
   		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
   	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.formFields = formFields;
		RefObject_1_0 obj = this.getObject();
		if(obj instanceof Account) {
			this.account = (Account)obj;
		} else if(accountXri != null && !accountXri.isEmpty()) {
			try {
				this.account = (Account)pm.getObjectById(new Path(accountXri));
			} catch(Exception ignore) {}
		}
		if(!app.getErrorMessages().isEmpty()) {
			this.errorMsg = "";
			int i = 0;
            Set<String> errorMessages = new HashSet<String>(app.getErrorMessages());
			for(Object message: errorMessages) {
				if(i > 0) {
					this.errorMsg += "<br />";
				}
				this.errorMsg += message.toString();
				i++;
			}
			app.getErrorMessages().clear();
		}
	}

	/**
	 * Cancel action.
	 * 
	 * @param accountXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doCancel(
   	   	@RequestParameter(name = "accountXri") String accountXri,
   		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields   				
	) throws ServiceException {
		this.doRefresh(
			accountXri, 
			formFields
		);
		this.setExitAction(
			new ObjectReference(
				this.getAccount() == null 
					? this.getObject() 
					: this.getAccount(), 
				this.getApp()
			).getSelectObjectAction()
		);
	}

	/**
	 * Search action.
	 * 
	 * @param accountXri
	 * @param formFields
	 */
	abstract public void doSearch(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
   		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields   				
	) throws ServiceException;

	/**
	 * DisableMembership action.
	 * 
	 * @param memberXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doDisableMembership(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
		@RequestParameter(name = "Para0") String memberXri,
		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			accountXri,
			formFields
		);
		try {
			pm.currentTransaction().begin();
			Member member = (Member)pm.getObjectById(new Path(memberXri));
			member.setDisabled(true);
			pm.currentTransaction().commit();
		} catch (Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception ignore) {}
			throw new ServiceException(e);
		}   			
	}

	/**
	 * AddMembership action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doAddMembership(
   	   	@RequestParameter(name = "accountXri") String accountXri,	
   	   	@RequestParameter(name = "Para0") String targetAccountXri,	
		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		this.doRefresh(
			accountXri,
			formFields
		);
		this.isAddMembershipMode = true;		
		formFields.put(
			"org:opencrx:kernel:account1:AccountAssignment:account", 
			targetAccountXri == null || targetAccountXri.isEmpty() ? null : new Path(targetAccountXri)
		);
		formFields.put("org:opencrx:kernel:account1:AccountMembership:memberRole", null);
	}

	/**
	 * EditMembership action.
	 * 
	 * @param membershipXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doEditMembership(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
   		@RequestParameter(name = "membershipXri") String membershipXri,   				
		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();   			
		this.doRefresh(
			accountXri, 
			formFields
		);
		this.membershipXri = membershipXri;
		if(membershipXri != null && !membershipXri.isEmpty()) {
			Member membership = null;
			try {
				membership = (Member)pm.getObjectById(new Path(membershipXri));
				this.formFields.put(
					"org:opencrx:kernel:account1:AccountAssignment:account", 
					membership.refGetPath().getParent().getParent()
				);
			} catch (Exception ignore) {}
			if(membership != null) {
				this.formFields.put(
					"org:opencrx:kernel:account1:AccountMembership:memberRole", 
					new ArrayList<Short>(membership.getMemberRole())
				);
			}
		}
	}

	/**
	 * DisableMember action.
	 * 
	 * @param accountXri
	 * @param memberXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doDisableMember(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
		@RequestParameter(name = "Para0") String memberXri,
		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			accountXri, 
			formFields
		);
		try {
			pm.currentTransaction().begin();
			Member member = (Member)pm.getObjectById(new Path(memberXri));
			member.setDisabled(true);
			pm.currentTransaction().commit();
		} catch (Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception ignore) {}
			throw new ServiceException(e);
		}
	}

	/**
	 * AddMember action.
	 * 
	 * @param accountXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doAddMember(
   	   	@RequestParameter(name = "accountXri") String accountXri,		
		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		this.doRefresh(
			accountXri, 
			formFields
		);
		this.isAddMemberMode = true;
		this.memberXri = null;
		formFields.put("org:opencrx:kernel:account1:AccountAssignment:account", null);
		formFields.put("org:opencrx:kernel:account1:Member:memberRole", null);
		formFields.put("org:opencrx:kernel:account1:Member:name", null);
		formFields.put("org:opencrx:kernel:account1:Member:description", null);
		formFields.put("org:opencrx:kernel:account1:Member:memberRole", Arrays.asList(new Short[]{(short)11}));
	}

	/**
	 * EditMember action.
	 * 
	 * @param accountXri
	 * @param memberXri
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doEditMember(
   	   	@RequestParameter(name = "accountXri") String accountXri,
   		@RequestParameter(name = "memberXri") String memberXri,
		@FormParameter(forms = {"ContactForm", "LegalEntityForm", "ContactMembershipForm", "LegalEntityMembershipForm", "MemberForm"}) Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();   			
		this.doRefresh(
			accountXri, 
			formFields
		);
		this.memberXri = memberXri;
		if(memberXri != null && !memberXri.isEmpty()) {
			Member member = (Member)pm.getObjectById(new Path(memberXri));
			this.formFields.put("org:opencrx:kernel:account1:AccountAssignment:account", member.getAccount().refGetPath());
			this.formFields.put("org:opencrx:kernel:account1:Member:memberRole", new ArrayList<Short>(member.getMemberRole()));
			this.formFields.put("org:opencrx:kernel:account1:Member:name", member.getName());
			this.formFields.put("org:opencrx:kernel:account1:Member:description", member.getDescription());
		}
	}

	/**
	 * Get form values.
	 * 
	 * @return
	 */
	public Map<String,Object> getFormFields(
	) {
		return this.formFields;
	}
	
	/**
	 * Get error message.
	 * 
	 * @return
	 */
	public String getErrorMsg(
	) {
		return this.errorMsg;
	}
	
	/**
	 * Get error title.
	 * 
	 * @return
	 */
	public String getErrorTitle(
	) {
		return this.errorTitle;
	}
	
	/**
	 * Get XRI of selected account membership.
	 * 
	 * @return
	 */
	public String getMembershipXri(
	) {
		return this.membershipXri;
	}
	
	/**
	 * Get accounts selected by doSearch action.
	 * 
	 * @return
	 */
	public List<Account> getMatchingAccounts(
	) {
		return this.matchingAccounts;
	}

	/**
	 * Return true if in add membership mode.
	 * 
	 * @return
	 */
	public Boolean getIsAddMembershipMode(
	) {
		return this.isAddMembershipMode;
	}
	
	/**
	 * @return the isMemberMode
	 */
	public Boolean getIsAddMemberMode() {
		return this.isAddMemberMode;
	}

	/**
	 * @return the memberXri
	 */
	public String getMemberXri() {
		return this.memberXri;
	}

	/**
	 * Get current contact.
	 * 
	 * @return
	 */
	public Account getAccount(
	) {
		return this.account;
	}

	/**
	 * Get view port.
	 * 
	 * @param out
	 * @return
	 */
	public ViewPort getViewPort(
		Writer out
	) {
		if(this.viewPort == null) {
			TransientObjectView view = new TransientObjectView(
				this.getFormFields(),
				this.getApp(),
				this.getObject(),
				this.getPm()
			);
			this.viewPort = ViewPortFactory.openPage(
				view,
				this.getRequest(),
				out
			);			
		}
		return this.viewPort;
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#close()
	 */
    @Override
    public void close(
    ) throws ServiceException {
	    super.close();
	    if(this.viewPort != null) {
	    	this.viewPort.close(false);
	    }
    }

	/**
	 * Flag to turn on/off create contract buttons.
	 * 
	 * @return
	 */
	public boolean isCreateContractButtonsEnabled(
	) {
		return true;
	}
	
	/**
	 * Flag to turn on / off create activity button.
	 * 
	 * @return
	 */
	public boolean isCreateActivityButtonEnabled(
	) {
		return true;
	}
	
	//-------------------------------------------------------------------
	// Members
	//-------------------------------------------------------------------
    protected String errorMsg = "";
    protected String errorTitle = "";
    protected Map<String,Object> formFields;
    protected Account account;
    protected Boolean isAddMembershipMode;
    protected String membershipXri;
    protected Boolean isAddMemberMode;
    protected String memberXri;
    protected List<Account> matchingAccounts;
    protected ViewPort viewPort;

}
