/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ImportAccountsFromXlsController
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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.TreeMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.opencrx.kernel.account1.cci2.AbstractGroupQuery;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.Group;
import org.opencrx.kernel.account1.jmi1.LegalEntity;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.UnspecifiedAccount;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.SecureObject;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.generic.cci2.LocalizedFieldQuery;
import org.opencrx.kernel.generic.cci2.NoteQuery;
import org.opencrx.kernel.generic.jmi1.CrxObject;
import org.opencrx.kernel.generic.jmi1.LocalizedField;
import org.opencrx.kernel.generic.jmi1.LocalizedFieldContainer;
import org.opencrx.kernel.generic.jmi1.Note;
import org.opencrx.kernel.portal.BooleanPropertyDataBinding;
import org.opencrx.kernel.portal.DecimalPropertyDataBinding;
import org.opencrx.kernel.portal.EmailAddressDataBinding;
import org.opencrx.kernel.portal.IntegerPropertyDataBinding;
import org.opencrx.kernel.portal.PhoneNumberDataBinding;
import org.opencrx.kernel.portal.PostalAddressDataBinding;
import org.opencrx.kernel.portal.StringPropertyDataBinding;
import org.opencrx.kernel.portal.wizard.ImportAccountsFromXlsController.MapFieldContext.DataBindingType;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.cci.ModelElement_1_0;
import org.openmdx.base.mof.cci.ModelHelper;
import org.openmdx.base.mof.cci.Model_1_0;
import org.openmdx.base.mof.cci.PrimitiveTypes;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.Codes;
import org.openmdx.portal.servlet.DataBinding;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.action.SelectObjectAction;
import org.openmdx.portal.servlet.databinding.CompositeObjectDataBinding;
import org.openmdx.security.realm1.jmi1.Realm;
import org.w3c.format.DateTimeFormat;

/**
 * ImportAccountsFromXlsController
 *
 */
public class ImportAccountsFromXlsController extends AbstractWizardController {

	/**
	 * AccountType
	 *
	 */
	public enum AccountType {
		NA,
		Contact,
		LegalEntity,
		Group,
		UnspecifiedAccount
	}
	
	/**
	 * ProgressMeter
	 *
	 */
	public static class ProgressMeter {
		
		/**
		 * @return the total
		 */
		public int getTotal() {
			return total;
		}
		/**
		 * @param total the total to set
		 */
		public void setTotal(int total) {
			this.total = total;
		}
		/**
		 * @return the current
		 */
		public int getCurrent() {
			return current;
		}
		/**
		 * @param current the current to set
		 */
		public void setCurrent(int current) {
			this.current = current;
		}
		
		private int total = 0;
		private int current = 0;
		
	}
	
	/**
	 * AccountRecordDefinition
	 *
	 */
	public static class AccountRecordDefinition {
	
		/**
		 * @return the attributeMap
		 */
		public Map<Integer,String> getColumns() {
			return columns;
		}

		/**
		 * @return the idxExtString0
		 */
		public int getIdxExtString0() {
			return idxExtString0;
		}

		/**
		 * @return the idxFirstName
		 */
		public int getIdxFirstName() {
			return idxFirstName;
		}

		/**
		 * @return the idxLastName
		 */
		public int getIdxLastName() {
			return idxLastName;
		}

		/**
		 * @return the idxAliasName
		 */
		public int getIdxAliasName() {
			return idxAliasName;
		}

		/**
		 * @return the idxEMailAddress
		 */
		public int getIdxEMailAddress() {
			return idxEMailAddress;
		}

		/**
		 * @return the idxCompany
		 */
		public int getIdxCompany() {
			return idxCompany;
		}

		/**
		 * @return the idxXri
		 */
		public int getIdxXri() {
			return idxXri;
		}

		/**
		 * @return the idxDtype
		 */
		public int getIdxDtype() {
			return idxDtype;
		}

		/**
		 * @param idxExtString0 the idxExtString0 to set
		 */
		public void setIdxExtString0(int idxExtString0) {
			this.idxExtString0 = idxExtString0;
		}

		/**
		 * @param idxFirstName the idxFirstName to set
		 */
		public void setIdxFirstName(int idxFirstName) {
			this.idxFirstName = idxFirstName;
		}

		/**
		 * @param idxLastName the idxLastName to set
		 */
		public void setIdxLastName(int idxLastName) {
			this.idxLastName = idxLastName;
		}

		/**
		 * @param idxAliasName the idxAliasName to set
		 */
		public void setIdxAliasName(int idxAliasName) {
			this.idxAliasName = idxAliasName;
		}

		/**
		 * @param idxEMailAddress the idxEMailAddress to set
		 */
		public void setIdxEMailAddress(int idxEMailAddress) {
			this.idxEMailAddress = idxEMailAddress;
		}

		/**
		 * @param idxCompany the idxCompany to set
		 */
		public void setIdxCompany(int idxCompany) {
			this.idxCompany = idxCompany;
		}

		/**
		 * @param idxXri the idxXri to set
		 */
		public void setIdxXri(int idxXri) {
			this.idxXri = idxXri;
		}

		/**
		 * @param idxDtype the idxDtype to set
		 */
		public void setIdxDtype(int idxDtype) {
			this.idxDtype = idxDtype;
		}

		private final Map<Integer,String> columns = new TreeMap<Integer,String>(); // ordered by index
		private int idxExtString0 = -1; 
		private int idxFirstName = -1;
		private int idxLastName = -1;
		private int idxAliasName = -1;
		private int idxEMailAddress = -1;
		private int idxCompany = -1;
		private int idxXri = -1;
		private int idxDtype = -1;

	}

	/**
	 * AccountRecord
	 *
	 */
	public static class AccountRecord {
	
		public enum ImportStatusElement {
			ATTR_CLASS,
			CONTENT
		}
		
		/**
		 * Constructor.
		 * 
		 * @param recordDefinition
		 */
		public AccountRecord(
			AccountRecordDefinition recordDefinition
		) {
			this.recordDefinition = recordDefinition;
		}

		/**
		 * @return the typeExplicitlySet
		 */
		public boolean isTypeExplicitlySet() {
			return typeExplicitlySet;
		}

		/**
		 * @param typeExplicitlySet the typeExplicitlySet to set
		 */
		public void setTypeExplicitlySet(boolean typeExplicitlySet) {
			this.typeExplicitlySet = typeExplicitlySet;
		}

		/**
		 * @return the accountType
		 */
		public AccountType getAccountType() {
			return accountType;
		}

		/**
		 * @param accountType the accountType to set
		 */
		public void setAccountType(AccountType accountType) {
			this.accountType = accountType;
		}

		/**
		 * @return the firstName
		 */
		public String getFirstName() {
			return firstName;
		}

		/**
		 * @param firstName the firstName to set
		 */
		public void setFirstName(String firstName) {
			this.firstName = firstName;
		}

		/**
		 * @return the lastName
		 */
		public String getLastName() {
			return lastName;
		}

		/**
		 * @param lastName the lastName to set
		 */
		public void setLastName(String lastName) {
			this.lastName = lastName;
		}

		/**
		 * @return the aliasName
		 */
		public String getAliasName() {
			return aliasName;
		}

		/**
		 * @param aliasName the aliasName to set
		 */
		public void setAliasName(String aliasName) {
			this.aliasName = aliasName;
		}

		/**
		 * @return the emailAddress
		 */
		public String getEmailAddress() {
			return emailAddress;
		}

		/**
		 * @param emailAddress the emailAddress to set
		 */
		public void setEmailAddress(String emailAddress) {
			this.emailAddress = emailAddress;
		}

		/**
		 * @return the company
		 */
		public String getCompany() {
			return company;
		}

		/**
		 * @param company the company to set
		 */
		public void setCompany(String company) {
			this.company = company;
		}

		/**
		 * @return the xriExplicitlySet
		 */
		public boolean isXriExplicitlySet() {
			return xriExplicitlySet;
		}

		/**
		 * @param xriExplicitlySet the xriExplicitlySet to set
		 */
		public void setXriExplicitlySet(boolean xriExplicitlySet) {
			this.xriExplicitlySet = xriExplicitlySet;
		}

		/**
		 * @return the xri
		 */
		public String getXri() {
			return xri;
		}

		/**
		 * @param xri the xri to set
		 */
		public void setXri(String xri) {
			this.xri = xri;
		}

		/**
		 * @return the extString0
		 */
		public String getExtString0() {
			return extString0;
		}

		/**
		 * @param extString0 the extString0 to set
		 */
		public void setExtString0(String extString0) {
			this.extString0 = extString0;
		}

		/**
		 * Set value for given field.
		 *  
		 * @param fieldName
		 * @param fieldValue
		 */
		public void setFieldValue(
			Integer fieldIndex,
			Object fieldValue
		) {
			this.fieldMap.put(fieldIndex, fieldValue);
		}

		/**
		 * Return the field index for the given column.
		 * 
		 * @param columnName
		 * @param startPos
		 * @return
		 */
		public int getFieldIndex(
			String columnName,
			int startPos
		) {
			for(Map.Entry<Integer,String> column: this.recordDefinition.getColumns().entrySet()) {
				if(column.getKey() >= startPos && column.getValue().equalsIgnoreCase(columnName)) {
					return column.getKey();
				}
			}
			return -1;
		}

		/**
		 * Get cell value.
		 * 
		 * @param fieldIndex
		 * @return
		 */
		public Object getFieldValue(
			int fieldIndex
		) {
			return this.fieldMap.get(fieldIndex);
		}

		/**
		 * Append s to import status of given field and element.
		 * 
		 * @param fieldIndex
		 * @param element
		 * @param s
		 */
		public void appendImportStatus(
			int fieldIndex,
			ImportStatusElement element,
			String s
		) {
			Map<ImportStatusElement,StringBuffer> importReportField = this.importReportFields.get(fieldIndex);
			if(importReportField == null) {
				this.importReportFields.put(
					fieldIndex,
					importReportField = new HashMap<ImportStatusElement,StringBuffer>()
				);
			}
			StringBuffer importReport = importReportField.get(element);
			if(importReport == null) {
				importReportField.put(
					element,
					importReport = new StringBuffer()
				);
			}
			importReport.append(s);
		}

		/**
		 * Print import status for all fields of this record to importReport.
		 * 
		 * @param importReport
		 */
		public void printImportStatus(
			Writer importReport
		) throws IOException {
			for(Map.Entry<Integer,String> column: this.recordDefinition.getColumns().entrySet()) {
				Map<ImportStatusElement,StringBuffer> importReportField = this.importReportFields.get(column.getKey());
				if(importReportField != null) {
					importReport.append("<td ");
					if(importReportField.get(ImportStatusElement.ATTR_CLASS) != null) {
						importReport.append("class=\"" + importReportField.get(ImportStatusElement.ATTR_CLASS) + "\"");						
					}
					importReport.append(">");
					if(importReportField.get(ImportStatusElement.CONTENT) != null) {
						importReport.append(importReportField.get(ImportStatusElement.CONTENT));
					}
					importReport.append("</td>");
				} else {
					importReport.append("<td class=\"empty\">&nbsp;</td>");					
				}
			}
		}

		/**
		 * @return the recordDefinition
		 */
		public AccountRecordDefinition getRecordDefinition(
		) {
			return this.recordDefinition;
		}

		//-------------------------------------------------------------------
		// Members
		//-------------------------------------------------------------------
		private final Map<Integer,Object> fieldMap = new HashMap<Integer,Object>();
		private final AccountRecordDefinition recordDefinition;
		private final Map<Integer,Map<ImportStatusElement,StringBuffer>> importReportFields = new HashMap<Integer,Map<ImportStatusElement,StringBuffer>>();
		private boolean typeExplicitlySet;
		private AccountType accountType;
		private String firstName;
		private String lastName;
		private String aliasName;
		private String emailAddress;
		private String extString0;
		private String company;
		private boolean xriExplicitlySet;
		private String xri;
	}

	/**
	 * Constructor
	 */
	public ImportAccountsFromXlsController(
		boolean requiresAdminRole
	) {
		super();
		this.requiresAdminRole = requiresAdminRole;
	}

	/**
	 * Find contact.
	 * 
	 * @param firstName
	 * @param lastName
	 * @param aliasName
	 * @param emailAddress
	 * @param extString0
	 * @param accountSegment
	 * @return
	 */
	public List<Contact> findContact(
        String firstName,
        String lastName,
        String aliasName,
        String emailAddress,
        String extString0,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment
    ) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
        List<Contact> matchingContacts = null;
        try {
            boolean hasQueryProperty = false;
            ContactQuery contactFilter = (ContactQuery)pm.newQuery(Contact.class);
            if(extString0 != null) {
                hasQueryProperty = true;
                contactFilter.thereExistsExtString0().equalTo(extString0); // exact match required
            } else {
                contactFilter.forAllDisabled().isFalse(); // ignore disabled contacts
                if(firstName != null) {
                    hasQueryProperty = true;
                    contactFilter.thereExistsFirstName().equalTo(firstName); // exact match
                }
                if(lastName != null) {
                    hasQueryProperty = true;
                    contactFilter.thereExistsLastName().equalTo(lastName); // exact match
                }
                if(aliasName != null) {
                    hasQueryProperty = true;
                    contactFilter.thereExistsAliasName().equalTo(aliasName); // exact match
                }
                if(emailAddress != null) {
                    hasQueryProperty = true;
			    	EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
			    	query.thereExistsEmailAddress().equalTo(emailAddress);
			    	query.forAllDisabled().isFalse(); // ignore disabled e-mail addresses
					contactFilter.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));						    	
                }
            }
            if (hasQueryProperty) {
                Collection<Contact> contacts = accountSegment.<Contact>getAccount(contactFilter);
                if (!contacts.isEmpty()) {
                    for(Iterator<Contact> c = contacts.iterator(); c.hasNext(); ) {
                        if (matchingContacts == null) {
                        	matchingContacts = new ArrayList<Contact>();
                        }
                        matchingContacts.add(c.next());
                    }
                }
            }
        } catch (Exception e) {
            new ServiceException(e).log();
        }
        return matchingContacts;
    }
	
    /**
     * Find group.
     * 
     * @param name
     * @param aliasName
     * @param emailAddress
     * @param extString0
     * @param allowDtypeGroup
     * @param allowDtypeLegalEntity
     * @param allowDtypeUnspecifiedAccount
     * @param accountSegment
     * @return
     */
    public List<AbstractGroup> findAbstractGroup(
        String name,
        String aliasName,
        String emailAddress,
        String extString0,
        boolean allowDtypeGroup,
        boolean allowDtypeLegalEntity,
        boolean allowDtypeUnspecifiedAccount,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
        List<AbstractGroup> matchingAbstractGroups = null;
        try {
            boolean hasQueryProperty = false;
            AbstractGroupQuery abstractGroupFilter = (AbstractGroupQuery)pm.newQuery(AbstractGroup.class);
            if(extString0 != null) {
                hasQueryProperty = true;
                abstractGroupFilter.thereExistsExtString0().equalTo(extString0); // exact match required
            } else {
                abstractGroupFilter.forAllDisabled().isFalse();
                if(name != null) {
                    hasQueryProperty = true;
                    abstractGroupFilter.thereExistsFullName().like("(?i).*" + name + ".*");
                }
                if(aliasName != null) {
                    hasQueryProperty = true;
                    abstractGroupFilter.thereExistsAliasName().equalTo(aliasName); // exact match
                }
                if(emailAddress != null) {
                    hasQueryProperty = true;
			    	EMailAddressQuery query = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
			    	query.thereExistsEmailAddress().equalTo(emailAddress);
			    	query.forAllDisabled().isFalse(); // ignore disabled e-mail addresses
			    	abstractGroupFilter.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(query));						    	
                }
            }
            if (hasQueryProperty) {
                Collection<AbstractGroup> abstractGroups = accountSegment.getAccount(abstractGroupFilter);
                if (!abstractGroups.isEmpty()) {
                    for(Iterator<AbstractGroup> c = abstractGroups.iterator(); c.hasNext(); ) {
                        AbstractGroup abstractGroup = (AbstractGroup)c.next();
                        if (
                            (allowDtypeGroup && abstractGroup instanceof Group) ||
                            (allowDtypeLegalEntity && abstractGroup instanceof LegalEntity) ||
                            (allowDtypeUnspecifiedAccount && abstractGroup instanceof UnspecifiedAccount)
                        ) {
                            if (matchingAbstractGroups == null) {
                            	matchingAbstractGroups = new ArrayList<AbstractGroup>();
                            }
                            matchingAbstractGroups.add(abstractGroup);
                        }
                    }
                }
            }
        } catch (Exception e) {
            new ServiceException(e).log();
        }
        return matchingAbstractGroups;
    }

    /**
     * Find unique account.
     * 
     * @param valueToMatch
     * @param accountSegment
     * @return
     */
    public Account findUniqueTargetAccount(
        String valueToMatch,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
        Account targetAccount = null;
        if(valueToMatch != null) {
            boolean directMatch = valueToMatch.startsWith("@#");
            if (!directMatch) {
                // try to locate account based on fullName
                try {
                    AccountQuery accountQuery = (AccountQuery)pm.newQuery(Account.class);
                    accountQuery.forAllDisabled().isFalse(); // exclude disabled accounts in search
                    accountQuery.thereExistsFullName().like("(?i).*" + valueToMatch + ".*");
                    Iterator<Account> accounts = accountSegment.getAccount(accountQuery).iterator();
                    if(accounts.hasNext()) {
                        targetAccount = accounts.next();
                        if(accounts.hasNext()) {
                        	SysLog.warning("Non-unique target account. Ignoring", valueToMatch);
                            // match must be unique
                            targetAccount = null;
                        }
                    } else {
                    	SysLog.warning("Target account not found", valueToMatch);
                    }
                } catch (Exception e) {
                    new ServiceException(e).log();
                }
            } else {
                valueToMatch = valueToMatch.substring(2);
            }
            if(directMatch || targetAccount == null) {
                // try to locate account based on exact match with extString0
                try {
                    AccountQuery accountFilter = (AccountQuery)pm.newQuery(Account.class);
                    accountFilter.thereExistsExtString0().equalTo(valueToMatch); // exact match required
                    Iterator<Account> accounts = accountSegment.getAccount(accountFilter).iterator();
                    if (accounts.hasNext()) {
                        targetAccount = accounts.next();
                        if (accounts.hasNext()) {
                        	SysLog.warning("Non-unique target account. Ignoring", valueToMatch);                        	
                            // match must be unique
                            targetAccount = null;
                        }
                    } else {
                    	SysLog.warning("Target account not found", valueToMatch);                    	
                    }
                } catch (Exception e) {
                    new ServiceException(e).log();
                }
            }
        }
        return targetAccount;
    }

    /**
     * Set user fields for CrxObject.
     * 
     * @param crxObject
     * @param userStrings
     * @param userCodes
     * @param userNumbers
     */
    protected void setUserFields(
    	CrxObject crxObject,
        List<String> userStrings,
        List<Short> userCodes,
        List<BigDecimal> userNumbers
    ) {
        if(userStrings != null) {
        	if(userStrings.get(0) != null) {
        		crxObject.setUserString0(userStrings.get(0));
        	}
        	if(userStrings.get(1) != null) {
        		crxObject.setUserString1(userStrings.get(1));
        	}
        	if(userStrings.get(2) != null) {
        		crxObject.setUserString2(userStrings.get(2));
        	}
        	if(userStrings.get(3) != null) {
        		crxObject.setUserString2(userStrings.get(3));
        	}
        }
        if(userCodes != null) {
        	if(userCodes.get(0) != null) {
        		crxObject.setUserCode0(userCodes.get(0));
        	}
        	if(userCodes.get(1) != null) {
        		crxObject.setUserCode1(userCodes.get(1));
        	}
        	if(userCodes.get(2) != null) {
        		crxObject.setUserCode2(userCodes.get(2));
        	}
        	if(userCodes.get(3) != null) {
        		crxObject.setUserCode2(userCodes.get(3));
        	}
        }
        if(userNumbers != null) {
        	if(userNumbers.get(0) != null) {
        		crxObject.setUserNumber0(userNumbers.get(0));
        	}
        	if(userNumbers.get(1) != null) {
        		crxObject.setUserNumber1(userNumbers.get(1));
        	}
        	if(userNumbers.get(2) != null) {
        		crxObject.setUserNumber2(userNumbers.get(2));
        	}
        	if(userNumbers.get(3) != null) {
        		crxObject.setUserNumber2(userNumbers.get(3));
        	}
        }
    }

    /**
     * Create or update member.
     * 
     * @param parentAccount
     * @param memberAccount
     * @param keyMemberRole
     * @param feature
     * @param codes
     * @param accountSegment
     * @return
     */
    public Member createOrUpdateMember(
        Account parentAccount,
        Account memberAccount,
        String keyMemberRole, /* a semicolon-separated list */
        List<String> userStrings,
        List<Short> userCodes,
        List<BigDecimal> userNumbers,
        String feature,
        Codes codes,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
        Member member = null;
        List<Short> memberRoles = new ArrayList<Short>();
        if (keyMemberRole != null) {
            StringTokenizer tokenizer = new StringTokenizer(keyMemberRole, ";", false);
            while(tokenizer.hasMoreTokens()) {
                String memberRoleStr = (String)tokenizer.nextToken();
                short memberRole = codes.findCodeFromValue(
                    memberRoleStr,
                    feature
                );
                if (memberRole == 0) {
                    try {
                        memberRole = Short.parseShort(memberRoleStr);
                    } catch (Exception e) {}
                }
                if (memberRole != 0) {
                    memberRoles.add(memberRole);
                }
            }
        }
        // try to locate member with same parent and account (or create new member)
        MemberQuery memberFilter = (MemberQuery)pm.newQuery(Member.class);
        memberFilter.forAllDisabled().isFalse();
        memberFilter.thereExistsAccount().equalTo(memberAccount);
        try {
            List<Member> members = parentAccount.getMember(memberFilter);
            if (!members.isEmpty()) {
                member = members.iterator().next();
            } else {
                member = pm.newInstance(Member.class);
                member.setValidFrom(new java.util.Date());
                parentAccount.addMember(
                    Base.getInstance().getUidAsString(),
                    member
                );
            }
        } catch (Exception e) {
            new ServiceException(e).log();
        }
        if (member != null) {
    		if (!memberRoles.isEmpty()) {
    			// do NOT reset existing member roles if there are no new ones provided
        		member.setMemberRole(memberRoles);
    		}
            member.setAccount(memberAccount);
            member.setName(memberAccount.getFullName());
            member.setQuality((short)5); // normal
            if (
                (memberAccount.isDisabled() != null && memberAccount.isDisabled().booleanValue()) &&
                (member.isDisabled() == null || (!member.isDisabled().booleanValue()))
            ) {
                // disable member if memberAccount is disabled
                member.setDisabled(new Boolean(true));
                member.setDisabledReason("referenced Account is disabled");
            }
            this.setUserFields(
            	member, 
            	userStrings, 
            	userCodes, 
            	userNumbers
            );
        }
        return member;
    }

    /**
     * Create or update localized field.
     * 
     * @param container
     * @param name
     * @param description
     * @param locale
     * @param usage
     * @param localizedValue
     * @return
     */
    public LocalizedField createOrUpdateLocalizedField(
    	LocalizedFieldContainer container,
        String name,
        String description,
        short locale,
        short usage,
        String localizedValue
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(container);
    	LocalizedField localizedField = null;
        if(container != null && name != null  && !name.isEmpty() && localizedValue != null  && !localizedValue.isEmpty()) {
            LocalizedFieldQuery localizedFieldFilter = (LocalizedFieldQuery)pm.newQuery(LocalizedField.class);
            localizedFieldFilter.name().equalTo(name);
            localizedFieldFilter.locale().equalTo(locale);
            try {
                Iterator<LocalizedField> l = container.getLocalizedField(localizedFieldFilter).iterator();
                if (l.hasNext()) {
                    localizedField = l.next();
                } else {
                    localizedField = pm.newInstance(LocalizedField.class);
                    container.addLocalizedField(
                        Base.getInstance().getUidAsString(),
                        localizedField
                    );
                }
            } catch (Exception e) {
                new ServiceException(e).log();
            }
            if (localizedField != null) {
            	localizedField.setName(name);
            	localizedField.setDescription(description);
            	localizedField.setLocale(locale);
            	localizedField.setUsage(usage);
            	localizedField.setLocalizedValue(localizedValue);
            }
        }
        return localizedField;
    }

    /**
     * Create or update note.
     *  
     * @param account
     * @param noteTitle
     * @param noteText
     * @return
     */
    public Note createOrUpdateNote(
        Account account,
        String noteTitle,
        String noteText
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
        Note note = null;
        if (account != null && noteTitle != null  && !noteTitle.isEmpty()) {
            NoteQuery noteFilter = (NoteQuery)pm.newQuery(Note.class);
            noteFilter.thereExistsTitle().equalTo(noteTitle);
            try {
            	List<Note> notes = account.getNote(noteFilter);
                if(!notes.isEmpty()) {
                    note = notes.iterator().next();
                } else {
                    note = pm.newInstance(Note.class);
                    account.addNote(
                        Base.getInstance().getUidAsString(),
                        note
                    );
                }
            } catch (Exception e) {
                new ServiceException(e).log();
            }
            if(note != null) {
                note.setTitle(noteTitle);
                note.setText(noteText);
            }
        }
        return note;
    }

    /**
     * Get href of select object action.
     * 
     * @param object
     * @return
     */
    public String getSelectObjectHref(
        RefObject_1_0 object
    ) {
        if (object != null) {
            Action parentAction = new Action(
                SelectObjectAction.EVENT_ID,
                new Action.Parameter[]{
                    new Action.Parameter(Action.PARAMETER_OBJECTXRI, object.refMofId())
                },
                "",
                true // enabled
            );
            return parentAction.getEncodedHRef();
        } else {
        	return "";
        }
    }

    /**
     * Read import definition (first row in excel sheet).
     * 
     * @param row
     * @param nRow
     * @param maxCell
     * @param recordDefinition
     */
    public int readImportDefinition(
    	Row row,
    	int nRow,
    	AccountRecordDefinition recordDefinition,
    	Writer importReport
    ) throws IOException {
    	int maxCell = 0;
		importReport.append("<tr class=\"gridTableHeaderFull\">");
		importReport.append("  <td>#</td>");
		try {
			Iterator<Cell> cells = row.cellIterator();
			int idxCell = 0;
			while (cells.hasNext()) {
				Cell cell = cells.next();
				idxCell = cell.getColumnIndex();
				if (idxCell > maxCell) {
					maxCell = idxCell;
				}
				try {
					if (
						(cell.getCellType() == Cell.CELL_TYPE_STRING) &&
						(cell.getStringCellValue() != null)
					) {
						boolean isSearchAttribute = false;
						String cellValue = (cell.getStringCellValue().trim());
						recordDefinition.getColumns().put(
							idxCell, 
							cellValue
						);
						// get idx of select attributes
						if (this.ATTR_EXTSTRING0.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxExtString0(idxCell);
							isSearchAttribute = true;
						} else if (this.ATTR_FIRSTNAME.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxFirstName(idxCell);
							isSearchAttribute = true;
						} else if (this.ATTR_LASTNAME.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxLastName(idxCell);
							isSearchAttribute = true;
						} else if (this.ATTR_ALIASNAME.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxAliasName(idxCell);
							isSearchAttribute = true;
						} else if (this.ATTR_EMAILADDRESS.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxEMailAddress(idxCell);
							isSearchAttribute = true;
						} else if (this.ATTR_COMPANY.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxCompany(idxCell);
							isSearchAttribute = true;
						} else if (this.ATTR_DTYPE.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxDtype(idxCell);
						} else if (this.ATTR_XRI.compareToIgnoreCase(cellValue) == 0) {
							recordDefinition.setIdxXri(idxCell);
							isSearchAttribute = true;
						}
						importReport.append("<td " + (isSearchAttribute ? "class='searchAttr' title='attribute used for matching'" : "") + "Col-" + DECIMAL_FORMAT_4.format(idxCell) + EOL_HTML + cellValue + "</td>");
					} else {
						String cellType = null;
						switch(cell.getCellType()) {
							case Cell.CELL_TYPE_BLANK: 
								cellType = "CELL_TYPE_BLANK";
								break;
							case Cell.CELL_TYPE_BOOLEAN:
								cellType = "CELL_TYPE_BOOLEAN";
								break;
							case Cell.CELL_TYPE_ERROR:
								cellType = "CELL_TYPE_ERROR";
								break;
							case Cell.CELL_TYPE_FORMULA:
								cellType = "CELL_TYPE_FORMULA";
								break;
							case Cell.CELL_TYPE_NUMERIC:
								cellType = "CELL_TYPE_NUMERIC";
								break;
							case Cell.CELL_TYPE_STRING:
								cellType = "CELL_TYPE_STRING";
						}
						importReport.append("<td class=\"err\">c" + DECIMAL_FORMAT_4.format(idxCell) + " [not a string cell]<br />Type is " + cellType + "</td>");
					}
				} catch (Exception ec) {
					new ServiceException(ec).log();
					importReport.append("<td class=\"err\">c" + DECIMAL_FORMAT_4.format(idxCell) + " [unknown error]<br />" + ec.getMessage() + "</td>");
				}
			}
		} catch (Exception e) {
			new ServiceException(e).log();
			importReport.append("<td class=\"err\">ERROR in Attribute Row!</td>");
		}
		importReport.append("</tr>");
		return maxCell;
    }

    /**
     * Read account record.
     * 
     * @param row
     * @param nRow
     * @param maxCell
     * @param recordDefinition
     * @param record
     * @return
     * @throws ServiceException
     */
    public void readAccountRecord(
    	Row row,
    	int nRow,
    	int maxCell,
    	AccountRecord record,
    	Writer importReport,
    	List<String> errors
    ) throws ServiceException, IOException {
		importReport.append("<tr class=\"gridTableRowFull\">");
		importReport.append("<td>" + DECIMAL_FORMAT_4.format(nRow) + "</td>");
		AccountRecordDefinition recordDefinition = record.getRecordDefinition();
		Iterator<Cell> cells = row.cellIterator();
		int idxCell = 0;
		while(cells.hasNext()) {
			Cell cell = (Cell)cells.next();
			idxCell = cell.getColumnIndex();
			try {
				if (cell.getCellType() == Cell.CELL_TYPE_STRING) {
					String cellValue = cell.getStringCellValue().trim();
					record.setFieldValue(
						idxCell, 
						cellValue
					);
					if (idxCell == recordDefinition.getIdxDtype()) {
						if (AccountType.Group.name().compareToIgnoreCase(cellValue) == 0) {
							record.setTypeExplicitlySet(true);
							record.setAccountType(AccountType.Group);
							record.appendImportStatus(
								idxCell, 
								AccountRecord.ImportStatusElement.ATTR_CLASS, 
								" ok"
							);
						} else if(AccountType.LegalEntity.name().compareToIgnoreCase(cellValue) == 0) {
							record.setTypeExplicitlySet(true);
							record.setAccountType(AccountType.LegalEntity);
							record.appendImportStatus(
								idxCell, 
								AccountRecord.ImportStatusElement.ATTR_CLASS, 
								" ok"
							);
						} else if(AccountType.UnspecifiedAccount.name().compareToIgnoreCase(cellValue) == 0) {
							record.setTypeExplicitlySet(true);
							record.setAccountType(AccountType.UnspecifiedAccount);
							record.appendImportStatus(
								idxCell, 
								AccountRecord.ImportStatusElement.ATTR_CLASS, 
								" ok"
							);
						} else if(AccountType.Contact.name().compareToIgnoreCase(cellValue) == 0) {
							record.setTypeExplicitlySet(true);
							record.setAccountType(AccountType.Contact);
							record.appendImportStatus(
								idxCell, 
								AccountRecord.ImportStatusElement.ATTR_CLASS, 
								" ok"
							);
						}
					} else if(idxCell == recordDefinition.getIdxExtString0()) {
						record.setExtString0(cellValue);
					} else if(idxCell == recordDefinition.getIdxFirstName()) {
						record.setFirstName(cellValue);
					} else if(idxCell == recordDefinition.getIdxLastName()) {
						record.setLastName(cellValue);
					} else if(idxCell == recordDefinition.getIdxAliasName()) {
						record.setAliasName(cellValue);
					} else if(idxCell == recordDefinition.getIdxEMailAddress()) {
						record.setEmailAddress(cellValue);
					} else if(idxCell == recordDefinition.getIdxCompany()) {
						record.setCompany(cellValue);
					} else if(idxCell == recordDefinition.getIdxXri()) {
						record.setXriExplicitlySet(true);
						record.setXri(cellValue);
					}
					record.appendImportStatus(
						idxCell, 
						AccountRecord.ImportStatusElement.CONTENT, 
						(cellValue != null ? (cellValue.replace("\r\n", EOL_HTML)).replace("\n", EOL_HTML) : "")
					);
				} else if (cell.getCellType() == Cell.CELL_TYPE_NUMERIC) {
					BigDecimal cellValue = new BigDecimal(cell.getNumericCellValue());
					if (idxCell == recordDefinition.getIdxExtString0()) {
						record.setExtString0(cellValue.toString());
						record.setFieldValue(
							idxCell, 
							record.getExtString0()
						);
					} else {
						record.setFieldValue(
							idxCell, 
							cellValue
						);
					}
					record.appendImportStatus(
						idxCell, 
						AccountRecord.ImportStatusElement.CONTENT, 
						cellValue.toString()
					);
				} else if(cell.getCellType() == Cell.CELL_TYPE_BOOLEAN) {
					boolean cellValue = cell.getBooleanCellValue();
					record.setFieldValue(
						idxCell, 
						cellValue
					);
					record.appendImportStatus(
						idxCell, 
						AccountRecord.ImportStatusElement.CONTENT, 
						(cellValue ? "TRUE" : "FALSE")
					);
				} else if (cell.getCellType() == Cell.CELL_TYPE_BLANK) {
					record.appendImportStatus(
						idxCell, 
						AccountRecord.ImportStatusElement.ATTR_CLASS, 
						" empty"
					);
					record.appendImportStatus(
						idxCell, 
						AccountRecord.ImportStatusElement.CONTENT, 
						"&nbsp;"
					);
				} else {
					record.appendImportStatus(
						idxCell, 
						AccountRecord.ImportStatusElement.ATTR_CLASS, 
						" err"
					);
					record.appendImportStatus(
						idxCell, 
						AccountRecord.ImportStatusElement.CONTENT, 
						"r" + DECIMAL_FORMAT_4.format(nRow) + ":c" + DECIMAL_FORMAT_4.format(idxCell) + ": cell type '" + cell.getCellType() + "' not supported<br>"
					);
				}
			} catch (Exception e) {
				ServiceException e0 = new ServiceException(e);
				e0.log();
				errors.add(e0.getMessage());
				record.appendImportStatus(
					idxCell, 
					AccountRecord.ImportStatusElement.ATTR_CLASS, 
					" err"
				);
				record.appendImportStatus(
					idxCell, 
					AccountRecord.ImportStatusElement.CONTENT, 
					"r" + DECIMAL_FORMAT_4.format(nRow) + ":c" + DECIMAL_FORMAT_4.format(idxCell) + ": Unknown ERROR. See log.<br>"
				);
			}
		}
    }

    /**
     * Parse date.
     * 
     * @param s
     * @return
     */
    public Date parseDate(
    	String s
    ) {
		if (!s.startsWith("00") && !s.startsWith("0.") && !s.startsWith("0-")) {
			try {
				return DateTimeFormat.BASIC_UTC_FORMAT.parse(s);
			} catch (Exception ignore) {}
			try {				
				DateFormat format = new SimpleDateFormat("yyyyMMdd");
				format.setTimeZone(TimeZone.getTimeZone("UTC"));
				return format.parse(s);
			} catch (Exception ignore) {}
			try {
				DateFormat format = new SimpleDateFormat("MM/dd/yyyy");
				format.setTimeZone(TimeZone.getTimeZone("UTC"));
				return format.parse(s);
			} catch (Exception ignore) {}
			try {
				DateFormat format = new SimpleDateFormat("MM/dd/yy");
				format.setTimeZone(TimeZone.getTimeZone("UTC"));
				return format.parse(s);
			} catch (Exception ignore) {}
			try {
				DateFormat format = new SimpleDateFormat("dd-MM-yyyy");
				format.setTimeZone(TimeZone.getTimeZone("UTC"));
				return format.parse(s);
			} catch (Exception ignore) {}
			try {
				DateFormat format = new SimpleDateFormat("dd-MM-yy");
				format.setTimeZone(TimeZone.getTimeZone("UTC"));				
				return format.parse(s);
			} catch (Exception ignore) {}
		}
		return null;
    }

    /**
     * Normalize multi-line string.
     * 
     * @param s
     * @return
     */
    public String normalizeMultiLineString(
    	String s
    ) {
		String preparedString = s.toString().replace("\\n\\r", "\r\n");
		preparedString = preparedString.replace("\\r\\n", "\r\n");
		preparedString = preparedString.replace("\r\n\r\n", "\r\n");
		return preparedString;
    }

    /**
     * Get data binding for PostalAddress.
     * 
     * @param parameters
     * @return
     */
    public DataBinding getPostalAddressDataBinding(    	
    	short usage,
    	String parameters,
    	DataBinding dataBinding
    ) {
    	return parameters == null || parameters.isEmpty()
    		? dataBinding == null
    			? new PostalAddressDataBinding("[isMain=(boolean)true];usage=(short)" + usage + "?zeroAsNull=true")
    			: dataBinding
    		: new PostalAddressDataBinding(parameters + ";usage=(short)" + usage + "?zeroAsNull=true");
    }

    /**
     * Get data binding for PhoneNumber.
     * 
     * @param parameters
     * @return
     */
    public DataBinding getPhoneNumberDataBinding(
    	short usage,
    	String parameters,
    	DataBinding dataBinding
    ) {
    	return parameters == null || parameters.isEmpty() 
    		? dataBinding == null
    			? new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)" + usage + ";automaticParsing=(boolean)true")
    			: dataBinding
    		: new PhoneNumberDataBinding(parameters + ";usage=(short)" + usage + ";automaticParsing=(boolean)true");
    }

    /**
     * Get data binding for EmailAddress.
     * 
     * @param usage
     * @param parameters
     * @return
     */
    public DataBinding getEmailAddressDataBinding(
    	short usage,
    	short emailType,
    	String parameters,
    	DataBinding dataBinding
    ) {
    	return parameters == null || parameters.isEmpty() 
    		? dataBinding == null
    			? new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)" + usage + ";[emailType=(short)" + emailType + "]")
    			: dataBinding
    		: new EmailAddressDataBinding(parameters + ";usage=(short)" + usage + ";[emailType=(short)" + emailType + "]");
    }

    /**
     * Get data binding for CompositeObject.
     * 
     * @param usage
     * @param parameters
     * @return
     */
    public DataBinding getCompositeObjectDataBinding(
    	String type,
    	short usage,
    	String parameters,
    	DataBinding dataBinding
    ) {
    	return parameters == null || parameters.isEmpty() 
    		? dataBinding == null
    			? new CompositeObjectDataBinding("type=" + type + ";disabled=(boolean)false;[isMain=(boolean)true];usage=(short)" + usage)
    			: dataBinding
    		: new CompositeObjectDataBinding("type=" + type + ";disabled=(boolean)false;" + parameters + ";usage=(short)" + usage);
    }

	/**
     * MapFieldContext
     *
     */
    public static class MapFieldContext {
    	
    	public enum DataBindingType {
    		POSTAL_HOME,
    		POSTAL_BUSINESS,
        	PHONE_HOME,
        	PHONE_OTHER,
        	PHONE_BUSINESS,
        	PHONE_BUSINESS2,
        	FAX_BUSINESS,
        	PHONE_MOBILE,
        	MAIL_BUSINESS,
        	MAIL_HOME,
        	MAIL_OTHER,
        	MAIL_X500,
        	WEB_BUSINESS    		
    	};
    	
    	/**
		 * @return the isOk
		 */
		public Boolean getIsOk() {
			return isOk;
		}
		/**
		 * @param isOk the isOk to set
		 */
		public void setIsOk(Boolean isOk) {
			this.isOk = isOk;
		}
		
		public void setDataBinding(
			DataBindingType dataBindingType,
			DataBinding dataBinding
		) {
			this.dataBindings.put(dataBindingType, dataBinding);
		}
		
		public DataBinding getDataBinding(
			DataBindingType dataBindingType
		) {
			return this.dataBindings.get(dataBindingType);
		}
		
		private Boolean isOk;
    	private Map<DataBindingType,DataBinding> dataBindings = new HashMap<DataBindingType,DataBinding>();
    }

    /**
     * Map field to account.
     * 
     * @param nRow
     * @param account
     * @param accountRecord
     * @param fieldName
     * @param fieldParameters
     * @param fieldLocale
     * @param fieldValue
     * @param jsBuffer
     * @param FIXED_MAPPED_ATTRIBUTES
     * @param accountSegment
     * @param codes
     * @param app
     * @param context
     * @throws ServiceException
     */
    public void mapField(
    	int nRow,
    	Account account,
    	AccountRecord accountRecord,
    	int fieldIndex,
    	String fieldName,
    	String fieldParameters,
    	Short fieldLocale,
    	Object fieldValue,
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment,
    	Codes codes,
    	ApplicationContext app,
    	MapFieldContext context
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	Boolean isOk = context.getIsOk();
    	DataBinding postalAddressHomeDataBinding = context.getDataBinding(DataBindingType.POSTAL_HOME);
    	DataBinding postalAddressBusinessDataBinding = context.getDataBinding(DataBindingType.POSTAL_BUSINESS);
    	DataBinding phoneHomeDataBinding = context.getDataBinding(DataBindingType.PHONE_HOME);
    	DataBinding phoneOtherDataBinding = context.getDataBinding(DataBindingType.PHONE_OTHER);
    	DataBinding phoneBusinessDataBinding = context.getDataBinding(DataBindingType.PHONE_BUSINESS);
    	DataBinding phoneBusiness2DataBinding = context.getDataBinding(DataBindingType.PHONE_BUSINESS2);
    	DataBinding faxBusinessDataBinding = context.getDataBinding(DataBindingType.FAX_BUSINESS);
    	DataBinding phoneMobileDataBinding = context.getDataBinding(DataBindingType.PHONE_MOBILE);
    	DataBinding mailBusinessDataBinding = context.getDataBinding(DataBindingType.MAIL_BUSINESS);
    	DataBinding mailHomeDataBinding = context.getDataBinding(DataBindingType.MAIL_HOME);
    	DataBinding mailOtherDataBinding = context.getDataBinding(DataBindingType.MAIL_OTHER);
    	DataBinding mailX500DataBinding = context.getDataBinding(DataBindingType.MAIL_X500);
    	DataBinding webPageBusinessDataBinding = context.getDataBinding(DataBindingType.WEB_BUSINESS);
    	if(account instanceof Contact) {
    		Contact contact = (Contact)account;
			if(fieldName.equalsIgnoreCase(this.ATTR_TITLE)) {
				if(fieldValue != null) {
					short salutationCode = codes.findCodeFromValue(
						fieldValue.toString(),
						FEATURE_SALUTATION_CODE
					);
					contact.setSalutationCode(salutationCode);
					if (salutationCode != 0) {
						isOk = true;
					} else {
						isOk = false;
					}
					String salutation = fieldValue.toString();
					if (salutation != null && !salutation.isEmpty()) {
						contact.setSalutation(salutation);
					}
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_ACADEMICTITLE)) {
				if(fieldValue != null) {
					short academicTitle = codes.findCodeFromValue(
						fieldValue.toString(),
						FEATURE_ACADEMICTITLE
					);
					account.setUserCode1(academicTitle);
					if (academicTitle != 0) {
						isOk = true;
					} else {
						isOk = false;
					}
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_SALUTATION)) {
				if(fieldValue != null) {
					String salutation = fieldValue.toString();
					if (salutation != null && !salutation.isEmpty()) {
						contact.setSalutation(salutation);
						isOk = true;
					} else {
						isOk = false;
					}
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_FIRSTNAME)) {
				if(fieldValue != null) {
					contact.setFirstName(fieldValue.toString()); 
					isOk = true;
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_MIDDLENAME)) {
				if(fieldValue != null) {
					contact.setMiddleName(fieldValue.toString());
					isOk = true;
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_LASTNAME)) {
				if(fieldValue != null) {
					contact.setLastName(fieldValue.toString()); 
					isOk = true;
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_SUFFIX)) {
				if(fieldValue != null) {
					contact.setSuffix(fieldValue.toString()); 
					isOk = true;
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_NICKNAME)) {
				if(fieldValue != null) {
					contact.setNickName(fieldValue.toString()); 
					isOk = true;
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_COMPANY)) {
				if(fieldValue != null) {
					String memberRole = null;
					int fieldIndexCompanyRole = accountRecord.getFieldIndex(this.ATTR_COMPANYROLE, fieldIndex);
					if (fieldIndexCompanyRole >= 0) {
						try {
							memberRole = (String)accountRecord.getFieldValue(fieldIndexCompanyRole);
						} catch (Exception e) {
							try {
								memberRole = ((BigDecimal)accountRecord.getFieldValue(fieldIndexCompanyRole)).toString();
							} catch (Exception ignore) {}
						}
					}
					String valueToMatch = null;
					try {
						valueToMatch = fieldValue.toString();
					} catch (Exception ignore) {}
					Account company = this.findUniqueTargetAccount(valueToMatch, accountSegment);
					Member member = null;
					if(company != null) {
						member = this.createOrUpdateMember(
							company,
							contact,
							memberRole,
							null, // userStrings
							null, // userCodes
							null, // userNumbers
							FEATURE_MEMBERROLE,
							codes,
							accountSegment
						);
					}
					if(member != null) {
						int fieldIndexJobTitle = accountRecord.getFieldIndex(this.ATTR_JOBTITLE, fieldIndex);
						if(fieldIndexJobTitle >= 0) {
							member.setDescription((String)accountRecord.getFieldValue(fieldIndexJobTitle));
						}
						isOk = true;
						if (memberRole != null) {
							accountRecord.appendImportStatus(
								fieldIndexCompanyRole,
								AccountRecord.ImportStatusElement.ATTR_CLASS,
								" ok"
							);
						}
						accountRecord.appendImportStatus(
							fieldIndex,
							AccountRecord.ImportStatusElement.CONTENT,
							"<br /><a href=\"" 
							+ this.getSelectObjectHref(company) + "\" target=\"_blank\">(<b>" + new ObjectReference(company, app).getTitle() 
							+ "</b>)</a>"
						);
						contact.setOrganization(company.getFullName()); 
						isOk = true;
					} else {
						contact.setOrganization(fieldValue.toString()); 
						isOk = true;
					}
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_DEPARTMENT) && contact != null) {
				if(fieldValue != null) {
					contact.setDepartment(fieldValue.toString()); 
					isOk = true;
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_JOBTITLE) && contact != null) {
				if(fieldValue != null) {
					contact.setJobTitle(fieldValue.toString()); 
					isOk = true;
				}
			} else if(fieldName.equalsIgnoreCase(this.ATTR_BIRTHDAY) && contact != null) {
				if(fieldValue != null) {
					Date birthdate = this.parseDate(fieldValue.toString());
					if(birthdate != null) {
						contact.setBirthdate(birthdate); 
						isOk = true;
					}
				}
			}
    	} else if(account instanceof AbstractGroup) {    		
    		AbstractGroup abstractGroup = (AbstractGroup)account;
			if(fieldName.equalsIgnoreCase(this.ATTR_COMPANY)) {
				if(fieldLocale == null) {
					if(fieldValue != null) {
						abstractGroup.setName(fieldValue.toString()); 
						isOk = true;
					};
				} else {
					if(fieldValue != null) {
						this.createOrUpdateLocalizedField(
							abstractGroup,
							"name",
							null,
							fieldLocale,
							(short)0,
							fieldValue.toString()
						);		
						isOk = true;
					}
				}
			}
		}
    	// Account
    	if(fieldName.startsWith("StringProperty:")) {    		
    		StringPropertyDataBinding dataBinding = new StringPropertyDataBinding();
    		if(fieldValue != null) {
	    		dataBinding.setValue(
	    			account, 
	    			fieldName.substring("StringProperty".length()), 
	    			fieldValue.toString()
	    		);
				isOk = true;
    		}
    	} else if(fieldName.startsWith("BooleanProperty:")) {
    		BooleanPropertyDataBinding dataBinding = new BooleanPropertyDataBinding();
    		if(fieldValue != null) {
	    		dataBinding.setValue(
	    			account, 
	    			fieldName.substring("BooleanProperty".length()), 
	    			fieldValue instanceof Boolean ? (Boolean)fieldValue : Boolean.valueOf(fieldValue.toString())
	    		);
				isOk = true;
    		}
    	} else if(fieldName.startsWith("IntegerProperty:")) {
    		IntegerPropertyDataBinding dataBinding = new IntegerPropertyDataBinding();
    		if(fieldValue != null) {
	    		dataBinding.setValue(
	    			account, 
	    			fieldName.substring("IntegerProperty".length()), 
	    			fieldValue instanceof Number ? ((Number)fieldValue).intValue() : Integer.parseInt(fieldValue.toString())
	    		);
				isOk = true;
    		}
		} else if(fieldName.startsWith("DecimalProperty:")) {
			DecimalPropertyDataBinding dataBinding = new DecimalPropertyDataBinding();
			if(fieldValue != null) {
				dataBinding.setValue(
					account, 
					fieldName.substring("DecimalProperty".length()), 
					fieldValue instanceof BigDecimal ? (BigDecimal)fieldValue : new BigDecimal(fieldValue.toString())
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_ALIASNAME)) {
			if(fieldLocale == null) {
				if(fieldValue != null) {
					account.setAliasName(fieldValue.toString()); 
					isOk = true;
				}
			} else {
				if(fieldValue != null) {
					this.createOrUpdateLocalizedField(
						account,
						"aliasName",
						null,
						fieldLocale,
						(short)0,
						fieldValue.toString()
					);		
					isOk = true;
				}
			}
    	} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEPHONE)) {
    		if(fieldValue != null) {
				phoneHomeDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_HOME, fieldParameters, phoneHomeDataBinding);
				phoneHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!phoneNumberFull",
					fieldValue.toString()
				);
				isOk = true;
    		}
    	} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEPHONE_AUTHORITY)) {
    		if(fieldValue instanceof String) {
				phoneHomeDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_HOME, fieldParameters, phoneHomeDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				phoneHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!authority",
					authority
				);
				isOk = true;
    		}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEPHONE2)) {
			if(fieldValue != null) {
				phoneOtherDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_OTHER, fieldParameters, phoneOtherDataBinding);
				phoneOtherDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Other!phoneNumberFull",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEPHONE2_AUTHORITY)) {
			if(fieldValue instanceof String) {
				phoneOtherDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_OTHER, fieldParameters, phoneOtherDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				phoneOtherDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Other!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEFAX)) {
			if(fieldValue != null) {
				DataBinding faxHomeDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_HOME_FAX, fieldParameters, null);
				faxHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address*Fax!phoneNumberFull",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEFAX_AUTHORITY)) {
			if(fieldValue instanceof String) {
				DataBinding faxHomeDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_HOME_FAX, fieldParameters, null);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				faxHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address*Fax!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEPOSTAL_AUTHORITY)) {
			if(fieldValue instanceof String) {
				postalAddressHomeDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_HOME, fieldParameters, postalAddressHomeDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				postalAddressHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEADDRESSLINE)) {
			postalAddressHomeDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_HOME, fieldParameters, postalAddressHomeDataBinding);
			List<String> postalAddressLines = new ArrayList<String>();
			if(fieldValue != null) {
				String preparedString = this.normalizeMultiLineString(fieldValue.toString());
				StringTokenizer tokenizer = new StringTokenizer(preparedString, "\r\n", false);
				while(tokenizer.hasMoreTokens()) {
					postalAddressLines.add(tokenizer.nextToken());
				}
				postalAddressHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!postalAddressLine",
					postalAddressLines
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMESTREET)) {
			postalAddressHomeDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_HOME, fieldParameters, postalAddressHomeDataBinding);
			List<String> postalStreetLines = new ArrayList<String>();
			if(fieldValue != null) {
				String preparedString = this.normalizeMultiLineString(fieldValue.toString());
				StringTokenizer tokenizer = new StringTokenizer(preparedString, "\r\n", false);
				while(tokenizer.hasMoreTokens()) {
					postalStreetLines.add(tokenizer.nextToken());
				}
				postalAddressHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!postalStreet",
					postalStreetLines
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMECITY)) {
			postalAddressHomeDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_HOME, fieldParameters, postalAddressHomeDataBinding);
			if(fieldValue != null) {
				postalAddressHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!postalCity",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMEPOSTALCODE)) {
			postalAddressHomeDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_HOME, fieldParameters, postalAddressHomeDataBinding);
			if(fieldValue != null) {
				String postalCode = null;
				try {
					postalCode = fieldValue.toString();
				} catch (Exception e) {}
				postalAddressHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!postalCode",
					postalCode
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMESTATE)) {
			postalAddressHomeDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_HOME, fieldParameters, postalAddressHomeDataBinding);
			if(fieldValue != null) {
				postalAddressHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!postalState",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_HOMECOUNTRY) || fieldName.equalsIgnoreCase(this.ATTR_HOMECOUNTRYREGION)) {
			postalAddressHomeDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_HOME, fieldParameters, postalAddressHomeDataBinding);
			if(fieldValue != null) {
				short postalCountry = codes.findCodeFromValue(
					fieldValue.toString(),
					"country"
				);
				postalAddressHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!postalCountry",
					postalCountry
				);
				if(postalCountry != 0) {
					isOk = true;
				} else {
					isOk = false;
				}
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_XRI)) {
			isOk = true;
		} else if(fieldName.equalsIgnoreCase(this.ATTR_EXTSTRING0)) {
			// verify, but do NOT set extString0 (may only be set during creation of new contact!!!)
			isOk = fieldValue != null && (account.getExtString0() != null) && (fieldValue.toString().compareTo(account.getExtString0()) == 0);
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSTYPE)) {
			List<Short> businessTypes = new ArrayList<Short>();
			if(fieldValue != null) {
				String preparedString = this.normalizeMultiLineString(fieldValue.toString());
				StringTokenizer tokenizer = new StringTokenizer(preparedString, "\r\n", false);
				while(tokenizer.hasMoreTokens()) {
					businessTypes.add(Short.parseShort(tokenizer.nextToken()));
				}
				account.setBusinessType(businessTypes);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_NOTES)) {
			if(fieldValue != null) {
				account.setDescription(fieldValue.toString());
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSPHONE)) {
			if(fieldValue != null) {
				phoneBusinessDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, phoneBusinessDataBinding);
				phoneBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSPHONE_AUTHORITY)) {
			if(fieldValue instanceof String) {
				phoneBusinessDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, phoneBusinessDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				phoneBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSPHONE2)) {
			if(fieldValue != null) {
				phoneBusiness2DataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_OTHER, fieldParameters, phoneBusiness2DataBinding);
				phoneBusiness2DataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Other!phoneNumberFull",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSPHONE2_AUTHORITY)) {
			if(fieldValue instanceof String) {
				phoneBusiness2DataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_OTHER, fieldParameters, phoneBusiness2DataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				phoneBusiness2DataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Other!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSFAX)) {
			if(fieldValue != null) {
				faxBusinessDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_BUSINESS_FAX, fieldParameters, faxBusinessDataBinding);
				faxBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*BusinessFax!phoneNumberFull",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSFAX_AUTHORITY)) {
			if(fieldValue instanceof String) {
				faxBusinessDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_BUSINESS_FAX, fieldParameters, faxBusinessDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				faxBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*BusinessFax!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_MOBILEPHONE)) {
			if(fieldValue != null) {
				phoneMobileDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_MOBILE, fieldParameters, phoneMobileDataBinding);
				phoneMobileDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_MOBILEPHONE_AUTHORITY)) {
			if(fieldValue != null) {
				phoneMobileDataBinding = this.getPhoneNumberDataBinding(Addresses.USAGE_MOBILE, fieldParameters, phoneMobileDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				phoneMobileDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Mobile!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_EMAILADDRESS)) {
			if(fieldValue != null) {
				mailBusinessDataBinding = this.getEmailAddressDataBinding(Addresses.USAGE_BUSINESS, (short)1, fieldParameters, mailBusinessDataBinding);
				mailBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!emailAddress",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_EMAILADDRESS_AUTHORITY)) {
			if(fieldValue instanceof String) {
				mailBusinessDataBinding = this.getEmailAddressDataBinding(Addresses.USAGE_BUSINESS, (short)1, fieldParameters, mailBusinessDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				mailBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_EMAIL2ADDRESS)) {
			if(fieldValue != null) {
				mailHomeDataBinding = this.getEmailAddressDataBinding(Addresses.USAGE_HOME, (short)1, fieldParameters, mailHomeDataBinding);
				mailHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!emailAddress",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_EMAIL2ADDRESS_AUTHORITY)) {
			if(fieldValue instanceof String) {
				mailHomeDataBinding = this.getEmailAddressDataBinding(Addresses.USAGE_HOME, (short)1, fieldParameters, mailHomeDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				mailHomeDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Contact:address!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_EMAIL3ADDRESS)) {
			if(fieldValue != null) {
				mailOtherDataBinding = this.getEmailAddressDataBinding(Addresses.USAGE_OTHER, (short)1, fieldParameters, mailOtherDataBinding);
				mailOtherDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Other!emailAddress",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_EMAIL3ADDRESS_AUTHORITY)) {
			if(fieldValue instanceof String) {
				mailOtherDataBinding = this.getEmailAddressDataBinding(Addresses.USAGE_OTHER, (short)1, fieldParameters, mailOtherDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				mailOtherDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Other!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_X500ADDRESS)) {
			if(fieldValue != null) {
				mailX500DataBinding = this.getEmailAddressDataBinding((short)0, (short)2, fieldParameters, mailX500DataBinding);
				mailX500DataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*X500!emailAddress",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_X500ADDRESS_AUTHORITY)) {
			if(fieldValue instanceof String) {
				mailX500DataBinding = this.getEmailAddressDataBinding((short)0, (short)2, fieldParameters, mailX500DataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				mailX500DataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*X500!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_WEBPAGE)) {
			if(fieldValue != null) {
				webPageBusinessDataBinding = this.getCompositeObjectDataBinding("org:opencrx:kernel:account1:WebAddress", Addresses.USAGE_BUSINESS, fieldParameters, webPageBusinessDataBinding);
				webPageBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:LegalEntity:address!webUrl",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_WEBPAGE_AUTHORITY)) {
			if(fieldValue instanceof String) {
				webPageBusinessDataBinding = this.getCompositeObjectDataBinding("org:opencrx:kernel:account1:WebAddress", Addresses.USAGE_BUSINESS, fieldParameters, webPageBusinessDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				webPageBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:LegalEntity:address!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSPOSTAL_AUTHORITY)) {
			if(fieldValue instanceof String) {
				postalAddressBusinessDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, postalAddressBusinessDataBinding);
				Account authority = this.findUniqueTargetAccount((String)fieldValue, accountSegment);
				postalAddressBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!authority",
					authority
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSADDRESSLINE)) {
			postalAddressBusinessDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, postalAddressBusinessDataBinding);
			List<String> postalAddressLines = new ArrayList<String>();
			if(fieldValue != null) {
				String preparedString = this.normalizeMultiLineString(fieldValue.toString());
				StringTokenizer tokenizer = new StringTokenizer(preparedString, "\r\n", false);
				while(tokenizer.hasMoreTokens()) {
					postalAddressLines.add(tokenizer.nextToken());
				}
				postalAddressBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!postalAddressLine",
					postalAddressLines
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSSTREET)) {
			postalAddressBusinessDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, postalAddressBusinessDataBinding);
			List<String> postalStreetLines = new ArrayList<String>();
			if(fieldValue != null) {
				String preparedString = this.normalizeMultiLineString(fieldValue.toString());
				StringTokenizer tokenizer = new StringTokenizer(preparedString, "\r\n", false);
				while(tokenizer.hasMoreTokens()) {
					postalStreetLines.add(tokenizer.nextToken());
				}
				postalAddressBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!postalStreet",
					postalStreetLines
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSCITY)) {
			postalAddressBusinessDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, postalAddressBusinessDataBinding);
			if(fieldValue != null) {
				postalAddressBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!postalCity",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSPOSTALCODE)) {
			postalAddressBusinessDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, postalAddressBusinessDataBinding);
			if(fieldValue != null) {
				String postalCode = null;
				try {
					postalCode = fieldValue.toString();
				} catch (Exception ignore) {}
				postalAddressBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!postalCode",
					postalCode
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSSTATE)) {
			postalAddressBusinessDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, postalAddressBusinessDataBinding);
			if(fieldValue != null) {
				postalAddressBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!postalState",
					fieldValue.toString()
				);
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_BUSINESSCOUNTRY) || fieldName.equalsIgnoreCase(this.ATTR_BUSINESSCOUNTRYREGION)) {
			postalAddressBusinessDataBinding = this.getPostalAddressDataBinding(Addresses.USAGE_BUSINESS, fieldParameters, postalAddressBusinessDataBinding);
			if(fieldValue != null) {
				short postalCountry = codes.findCodeFromValue(
					fieldValue.toString(),
					"country"
				);
				postalAddressBusinessDataBinding.setValue(
					account,
					"org:opencrx:kernel:account1:Account:address*Business!postalCountry",
					postalCountry
				);
				if (postalCountry != 0) {
					isOk = true;
				} else {
					isOk = false;
				}
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_ASSISTANTSNAME)) {
			if(fieldValue != null) {
				String memberRole = null;
				int fieldIndexAssistantsNameRole = accountRecord.getFieldIndex(this.ATTR_ASSISTANTSNAMEROLE, fieldIndex);
				if (fieldIndexAssistantsNameRole >= 0) {
					try {
						memberRole = (String)accountRecord.getFieldValue(fieldIndexAssistantsNameRole);
					} catch (Exception e) {
						try {
							memberRole = ((BigDecimal)accountRecord.getFieldValue(fieldIndexAssistantsNameRole)).toString();
						} catch (Exception ignore) {}
					}
				}
				String valueToMatch = null;
				try {
					valueToMatch = fieldValue.toString();
				} catch (Exception ignore) {}
				Account assistant = this.findUniqueTargetAccount(valueToMatch, accountSegment);
				Member member = null;
				if(assistant != null) {
					member = this.createOrUpdateMember(
						assistant,
						account,
						memberRole,
						null, // userStrings
						null, // userCodes
						null, // userNumbers
						FEATURE_MEMBERROLE,
						codes,
						accountSegment
					);
				}
				if(member != null) {
					isOk = true;
					if(memberRole != null) {
						accountRecord.appendImportStatus(
							fieldIndexAssistantsNameRole, 
							AccountRecord.ImportStatusElement.ATTR_CLASS, 
							" ok"
						);
					}
					accountRecord.appendImportStatus(
						fieldIndex, 
						AccountRecord.ImportStatusElement.CONTENT, 
						"<br /><a href=\""
						+ this.getSelectObjectHref(assistant) + "\" target=\"_blank\">(<b>" + new ObjectReference(assistant, app).getTitle() 
						+ "</b>)</a>"
					);
				}
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_MANAGERSNAME)) {
			if(fieldValue != null) {
				String memberRole = null;
				int fieldIndexManagersRole = accountRecord.getFieldIndex(this.ATTR_MANAGERSROLE, fieldIndex);
				if (fieldIndexManagersRole >= 0) {
					try {
						memberRole = (String)accountRecord.getFieldValue(fieldIndexManagersRole);
					} catch (Exception e) {
						try {
							memberRole = ((BigDecimal)accountRecord.getFieldValue(fieldIndexManagersRole)).toString();
						} catch (Exception ignore) {}
					}
				}
				String valueToMatch = null;
				try {
					valueToMatch = fieldValue.toString();
				} catch (Exception ignore) {}
				Account manager = this.findUniqueTargetAccount(valueToMatch, accountSegment);
				Member member = null;
				if(manager != null) {
					member = this.createOrUpdateMember(
						account,
						manager,
						memberRole,
						null, // userStrings
						null, // userCodes
						null, // userNumbers
						FEATURE_MEMBERROLE,
						codes,
						accountSegment
					);
				}
				if(member != null) {
					isOk = true;
					if (memberRole != null) {
						accountRecord.appendImportStatus(
							fieldIndexManagersRole, 
							AccountRecord.ImportStatusElement.ATTR_CLASS, 
							" ok"
						);
					}
					accountRecord.appendImportStatus(
						fieldIndex, 
						AccountRecord.ImportStatusElement.CONTENT, 
						"<br /><a href=\""
						+ this.getSelectObjectHref(account) + "\" target=\"_blank\">(<b>" + new ObjectReference(account, app).getTitle() 
						+ "</b>)</a>"
					);
				}
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_CATEGORIES)) {
			if(fieldValue != null) {
				String preparedString = this.normalizeMultiLineString(fieldValue.toString());
				// multiple values are delimited by newlines or semicolons
				StringTokenizer tokenizer = new StringTokenizer(preparedString, "\r\n;", false);
				while(tokenizer.hasMoreTokens()) {
					String category = (String)tokenizer.nextToken();
					if (!account.getCategory().contains(category)) {
						account.getCategory().add(category);
					}
				}
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_GROUPS)) {
			if(fieldValue != null) {
				Realm realm = SecureObject.getInstance().getRealm(pm, this.getProviderName(), this.getSegmentName());
				String preparedString = this.normalizeMultiLineString(fieldValue.toString());
				// multiple values are delimited by newlines or semicolons
				StringTokenizer tokenizer = new StringTokenizer(preparedString, "\r\n;", false);
				while(tokenizer.hasMoreTokens()) {
					String group = (String)tokenizer.nextToken();
					org.openmdx.security.realm1.jmi1.Principal principal = SecureObject.getInstance().findPrincipal(group, realm);
					if(
						principal instanceof PrincipalGroup && 
						!account.getOwningGroup().contains(principal)
					) {
						account.getOwningGroup().add((PrincipalGroup)principal);
					}
				}
				isOk = true;
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_MEMBEROF)) {
			if(fieldValue != null) {
				// Roles
				String memberRoles = null;
				int fieldIndexMemberRole = accountRecord.getFieldIndex(this.ATTR_MEMBERROLE, fieldIndex);
				if (fieldIndexMemberRole >= 0) {
					try {
						memberRoles = (String)accountRecord.getFieldValue(fieldIndexMemberRole);
					} catch (Exception e) {
						try {
							memberRoles = ((BigDecimal)accountRecord.getFieldValue(fieldIndexMemberRole)).toString();
						} catch (Exception ignore) {}
					}
				}
				// userStrings
				List<String> userStrings = new ArrayList<String>();
				for(String userField: USER_STRING_FIELDS) {
					int fieldIndexUser = accountRecord.getFieldIndex(this.ATTR_MEMBER_PREFIX + userField, fieldIndex);
					if(fieldIndexUser >= 0) {
						userStrings.add((String)accountRecord.getFieldValue(fieldIndexUser));
					} else {
						userStrings.add(null);
					}
				}
				// userCodes
				List<Short> userCodes = new ArrayList<Short>();
				for(String userField: USER_CODE_FIELDS) {
					int fieldIndexUser = accountRecord.getFieldIndex(this.ATTR_MEMBER_PREFIX + userField, fieldIndex);
					if(fieldIndexUser >= 0) {
						userCodes.add(((Number)accountRecord.getFieldValue(fieldIndexUser)).shortValue());
					} else {
						userCodes.add(null);
					}
				}
				// userNumbers
				List<BigDecimal> userNumbers = new ArrayList<BigDecimal>();
				for(String userField: USER_NUMBER_FIELDS) {
					int fieldIndexUser = accountRecord.getFieldIndex(this.ATTR_MEMBER_PREFIX + userField, fieldIndex);
					if(fieldIndexUser >= 0) {
						userNumbers.add((BigDecimal)accountRecord.getFieldValue(fieldIndexUser));
					} else {
						userNumbers.add(null);
					}
				}
				String valueToMatch = null;
				try {
					valueToMatch = fieldValue.toString();
				} catch (Exception ignore) {}
				Account parentAccount = this.findUniqueTargetAccount(valueToMatch, accountSegment);
				Member member = null;
				if(parentAccount != null) {
					member = this.createOrUpdateMember(
						parentAccount,
						account,
						memberRoles,
						userStrings,
						userCodes,
						userNumbers,
						FEATURE_MEMBERROLE,
						codes,
						accountSegment
					);
				}
				if(member != null) {
					isOk = true;
					if (memberRoles != null) {
						member.setDescription(memberRoles.toString());
						accountRecord.appendImportStatus(
							fieldIndexMemberRole, 
							AccountRecord.ImportStatusElement.ATTR_CLASS, 
							" ok"
						);
					}
					accountRecord.appendImportStatus(
						fieldIndex, 
						AccountRecord.ImportStatusElement.CONTENT, 
						"<br /><a href=\""
						+ this.getSelectObjectHref(parentAccount) + "\" target=\"_blank\">(<b>" + new ObjectReference(parentAccount, app).getTitle() 
						+ "</b>)</a>"
					);
				}
			}
		} else if(fieldName.equalsIgnoreCase(this.ATTR_NOTETITLE)) {
			if(fieldValue != null) {
				String noteTitle = fieldValue.toString();
				String noteText = null;
				Date noteCreatedAt = null;
				int fieldIndexNoteText = accountRecord.getFieldIndex(this.ATTR_NOTETEXT, fieldIndex);
				if(fieldIndexNoteText >= 0) {
					noteText = (String)accountRecord.getFieldValue(fieldIndexNoteText);
					if(noteText != null) {
						noteText = noteText.replace("\\n\\r", "\n");
					}
				}
				int fieldIndexNoteCreatedAt = accountRecord.getFieldIndex(this.ATTR_NOTECREATEDAT, fieldIndex - 1);
				if(fieldIndexNoteCreatedAt >= 0) {
					noteCreatedAt = this.parseDate(accountRecord.getFieldValue(fieldIndexNoteCreatedAt).toString());
				}
				Note note = this.createOrUpdateNote(
					account,
					noteTitle + (noteCreatedAt == null ? "" : " @" + noteCreatedAt.toString()),
					noteText
				);
				isOk = note != null;
				if(isOk && noteText != null) {
					accountRecord.appendImportStatus(
						fieldIndexNoteText, 
						AccountRecord.ImportStatusElement.ATTR_CLASS, 
						" ok"
					);					
				}
				if(isOk && noteCreatedAt != null) {
					accountRecord.appendImportStatus(
						fieldIndexNoteCreatedAt, 
						AccountRecord.ImportStatusElement.ATTR_CLASS, 
						" ok"
					);
				}
			}
		} else {
			if(fieldValue != null) {
				try {
					Model_1_0 model = Model_1Factory.getModel();
					@SuppressWarnings("unchecked")
	                Map<String,ModelElement_1_0> tmp = model.getElement(account.refClass().refMofId()).objGetMap("allFeature");
					Map<String,ModelElement_1_0> features = tmp;
					ModelElement_1_0 featureDef = (features == null ? null : (ModelElement_1_0)features.get(fieldName));
					if(featureDef != null) {
						if(
							PrimitiveTypes.STRING.equals(model.getElementType(featureDef).getQualifiedName()) &&
							ModelHelper.getMultiplicity(featureDef).isSingleValued()
						) {
							account.refSetValue(fieldName, fieldValue.toString());
							isOk = true;
						}
						if(
							PrimitiveTypes.SHORT.equals(model.getElementType(featureDef).getQualifiedName()) &&
							ModelHelper.getMultiplicity(featureDef).isSingleValued()
						) {
							account.refSetValue(fieldName, Short.parseShort(fieldValue.toString()));
							isOk = true;
						}
						if(
							PrimitiveTypes.SHORT.equals(model.getElementType(featureDef).getQualifiedName()) &&
							ModelHelper.getMultiplicity(featureDef).isMultiValued()
						) {
							// optional, multi-valued Short, individual values separated by semicolon, e.g. 5;21;113;218
							List<Short> codeValues = new ArrayList<Short>();
							if (fieldValue != null) {
								StringTokenizer tokenizer = new StringTokenizer(fieldValue.toString(), ";", false);
								while(tokenizer.hasMoreTokens()) {
									try {
										Short codeValue = Short.parseShort((String)tokenizer.nextToken());
										if (codeValue != null) {
											codeValues.add(codeValue);
										}
									} catch (Exception ignore) {}
								}
							}
							account.refSetValue(fieldName, codeValues);
							isOk = true;
						}
						if(
							PrimitiveTypes.BOOLEAN.equals(model.getElementType(featureDef).getQualifiedName()) &&
							ModelHelper.getMultiplicity(featureDef).isSingleValued()
						) {
							// optional, single-valued Boolean
							account.refSetValue(fieldName, Boolean.valueOf(fieldValue.toString()));
							isOk = true;
						}
						if(
							PrimitiveTypes.DECIMAL.equals(model.getElementType(featureDef).getQualifiedName()) &&
							ModelHelper.getMultiplicity(featureDef).isSingleValued()
						) {
							account.refSetValue(fieldName, new BigDecimal(fieldValue.toString()));
							isOk = true;
						}
					}
				} catch (Exception e) {
					new ServiceException(e).log();
					isOk = false;
				}
			}
		}
		if(Boolean.FALSE.equals(isOk)) {
			accountRecord.appendImportStatus(
				fieldIndex,
				AccountRecord.ImportStatusElement.ATTR_CLASS,
				" nok"
			);
		} else if (Boolean.TRUE.equals(isOk)) {
			accountRecord.appendImportStatus(
				fieldIndex,
				AccountRecord.ImportStatusElement.ATTR_CLASS,
				" ok"
			);
		}
		context.setIsOk(isOk);
    	context.setDataBinding(DataBindingType.POSTAL_HOME, postalAddressHomeDataBinding);
    	context.setDataBinding(DataBindingType.POSTAL_BUSINESS, postalAddressBusinessDataBinding);
    	context.setDataBinding(DataBindingType.PHONE_HOME, phoneHomeDataBinding);
    	context.setDataBinding(DataBindingType.PHONE_OTHER, phoneOtherDataBinding);
    	context.setDataBinding(DataBindingType.PHONE_BUSINESS, phoneBusinessDataBinding);
    	context.setDataBinding(DataBindingType.PHONE_BUSINESS2, phoneBusiness2DataBinding);
    	context.setDataBinding(DataBindingType.FAX_BUSINESS, faxBusinessDataBinding);
    	context.setDataBinding(DataBindingType.PHONE_MOBILE, phoneMobileDataBinding);
    	context.setDataBinding(DataBindingType.MAIL_BUSINESS, mailBusinessDataBinding);
    	context.setDataBinding(DataBindingType.MAIL_HOME, mailHomeDataBinding);
    	context.setDataBinding(DataBindingType.MAIL_OTHER, mailOtherDataBinding);
    	context.setDataBinding(DataBindingType.MAIL_X500, mailX500DataBinding);
    	context.setDataBinding(DataBindingType.WEB_BUSINESS, webPageBusinessDataBinding);
    }

    /**
     * Map fields of account record to account.
     * 
     * @param accountRecord
     * @param contact
     * @param nRow
     * @param codes
     * @param jsBuffer
     * @param app
     * @throws ServiceException
     */
    public void updateAccount(
    	AccountRecord accountRecord,
    	Account account,
    	Integer nRow,
    	Codes codes,
    	ApplicationContext app,
    	List<String> errors
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
    	String providerName = account.refGetPath().getSegment(2).toClassicRepresentation();
    	String segmentName = account.refGetPath().getSegment(4).toClassicRepresentation();
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, providerName, segmentName);
    	MapFieldContext mapFieldContext = new MapFieldContext();
		for(Map.Entry<Integer,String> column: accountRecord.getRecordDefinition().getColumns().entrySet()) {
			int fieldIndex = column.getKey();
			String fieldName = column.getValue();
			String fieldParameters = null;
			Short fieldLocale = null;
			Object fieldValue = accountRecord.getFieldValue(fieldIndex);
			if(fieldName.indexOf("?") > 0) {
				int index = fieldName.indexOf("?");
				fieldParameters = fieldName.substring(index + 1);
				fieldName = fieldName.substring(0, index);
			}
			if(fieldName.indexOf(LOCALE_SEPARATOR) > 0) {
				int index = fieldName.indexOf(LOCALE_SEPARATOR);
				fieldLocale = codes.findCodeFromValue(
					fieldName.substring(index + 1), 
					"locale"
				);
				fieldName = fieldName.substring(0, index);				
			}
			try {
				mapFieldContext.setIsOk(null);
				this.mapField(
					nRow, 
					account, 
					accountRecord,
					fieldIndex,
					fieldName, 
					fieldParameters, 
					fieldLocale, 
					fieldValue, 
					accountSegment, 
					codes, 
					app,
					mapFieldContext
				);
			} catch (Exception e) {
				ServiceException e0 = new ServiceException(e);
				e0.log();
				errors.add(e0.getMessage());
				mapFieldContext.setIsOk(false);
			}
		}
    }

	/**
	 * Refresh action.
	 * 
	 * @param locale
	 * @param description
	 */
	public void doRefresh(
		@RequestParameter(name = "locale") Short locale,
		@RequestParameter(name = "description") String description
	) {
		ApplicationContext app = this.getApp();
		this.locale = locale == null ? 0 : locale;
		if(this.locale == 1) { /* de_CH */
			this.ATTR_EXTSTRING0 = "extString0"; // index attribute
			this.ATTR_FIRSTNAME = "Vorname"; // index attribute
			this.ATTR_LASTNAME = "Nachname"; // index attribute
			this.ATTR_ALIASNAME = "AliasName"; // index attribute
			this.ATTR_COMPANY = "Firma"; // index attribute
			this.ATTR_MEMBEROF = "MemberOf";
			this.ATTR_MEMBERROLE = "MemberRole";
			this.ATTR_NOTETITLE = "NoteTitle";
			this.ATTR_NOTETEXT = "NoteText";
			this.ATTR_NOTECREATEDAT = "NoteCreatedAt";
			this.ATTR_MIDDLENAME = "WeitereVornamen";
			this.ATTR_SUFFIX = "Suffix";
			this.ATTR_NICKNAME = "Spitzname";
			this.ATTR_JOBTITLE = "Position";
			this.ATTR_DEPARTMENT = "Abteilung";
			this.ATTR_BIRTHDAY = "Geburtstag";
			this.ATTR_HOMEPHONE = "Telefonprivat";
			this.ATTR_HOMEPHONE2 = "Telefonprivat2";
			this.ATTR_HOMEFAX = "Faxprivat";
			this.ATTR_HOMESTREET = "Straßeprivat";
			this.ATTR_HOMECITY = "Ortprivat";
			this.ATTR_HOMEPOSTALCODE = "Postleitzahlprivat";
			this.ATTR_HOMESTATE = "BundeslandKantonprivat";
			this.ATTR_HOMECOUNTRY = "Landprivat";
			this.ATTR_HOMECOUNTRYREGION = "LandRegionprivat";
			this.ATTR_NOTES = "Notizen";
			this.ATTR_BUSINESSPHONE = "Telefongeschäftlich";
			this.ATTR_BUSINESSPHONE2 = "Telefongeschäftlich2";
			this.ATTR_BUSINESSFAX = "Faxgeschäftlich";
			this.ATTR_MOBILEPHONE = "Mobiltelefon";
			this.ATTR_EMAILADDRESS = "EMailAdresse";   // index attribute
			this.ATTR_EMAIL2ADDRESS = "EMail2Adresse";
			this.ATTR_EMAIL3ADDRESS = "EMail3Adresse";
			this.ATTR_X500ADDRESS = "X500Adresse";
			this.ATTR_WEBPAGE = "Webseite";
			this.ATTR_BUSINESSSTREET = "Straßegeschäftlich";
			this.ATTR_BUSINESSCITY = "Ortgeschäftlich";
			this.ATTR_BUSINESSPOSTALCODE = "Postleitzahlgeschäftlich";
			this.ATTR_BUSINESSSTATE = "BundeslandKantongeschäftlich";
			this.ATTR_BUSINESSCOUNTRY = "Landgeschäftlich";
			this.ATTR_BUSINESSCOUNTRYREGION = "LandRegiongeschäftlich";
			this.ATTR_ASSISTANTSNAME = "NameAssistent";
			this.ATTR_MANAGERSNAME = "NamedesrVorgesetzten";
			this.ATTR_CATEGORIES = "Kategorien";
			this.ATTR_GROUPS = "Gruppen";
		}
		if(this.requiresAdminRole) {
			String currentUserRole = app.getCurrentUserRole();
			String adminRole = SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + this.getSegmentName() + "@" + this.getSegmentName();
			this.hasPermission = currentUserRole.equals(adminRole);
		} else {
		   this.hasPermission = true;
		}
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
	 * UpdateProgressMeter action.
	 */
	public void doUpdateProgressMeter(
	) {
	}

	/**
	 * OK action.
	 * 
	 * @param locale
	 * @param description
	 */
	public void doOK(
		@RequestParameter(name = "locale") Short locale,
		@RequestParameter(name = "description") String description		
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		this.doRefresh(
			locale,
			description
		);
		if(!this.hasPermission) {
			this.errorMessage = "no permission to run this wizard";
		} else {
			String location = app.getTempFileName(UPLOAD_FILE_FIELD_NAME, "");
			try {
				if(
					new File(location + ".INFO").exists() &&
					new File(location).exists() &&
					(new File(location).length() > 0)
				) {
					String contentMimeType = null;
					String contentName = null;
					try {
						// mimeType and name
						BufferedReader r = new BufferedReader(
							new FileReader(location + ".INFO")
						);
						contentMimeType = r.readLine();
						contentName = r.readLine();
						r.close();
						new File(location + ".INFO").delete();
					} catch(Exception ignore) {}
					if(
						(contentName != null) &&
						!contentName.isEmpty() &&
						(contentMimeType != null) &&
						!contentMimeType.isEmpty()
					) {
						// Save location for performImport
						this.importFileLocation = location;
					}
				}
			} catch(Exception ignore) {}
		}
	}

	/**
	 * @return the errorMessage
	 */
	public String getErrorMessage() {
		return errorMessage;
	}

	/**
	 * Perform the import and generate report.
	 * 
	 * @return the this.importReport
	 */
	public void performImport(
		Writer importReport,
		ProgressMeter progressMeter
	) throws ServiceException, IOException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		Codes codes = this.getCodes();
		if(this.importFileLocation != null) {
			try {
				boolean isImportMembershipMode = this.getObject() instanceof Group; 
				Group parentGroup = null;
				Member groupMember = null;
				int parentGroupMemberSize = 0;
				if(isImportMembershipMode) {
					parentGroup = (Group)this.getObject();
					parentGroupMemberSize = parentGroup.getMember().size();
				}
				// Get account segment
				org.opencrx.kernel.account1.jmi1.Segment accountSegment =
				(org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
					new Path("xri://@openmdx*org.opencrx.kernel.account1").getDescendant("provider", this.getProviderName(), "segment", this.getSegmentName())
				);
				// Read workbook
				Workbook wb = null;
				try {
					wb = WorkbookFactory.create(new FileInputStream(this.importFileLocation));
				} catch (Exception e) {
					this.errorMessage = e.getMessage();
				}				
				if(wb != null) {
					for (int i = 0; i < 1; i++) {
						Sheet sheet = wb.getSheetAt(i);
						int nLinesRead = 0;
						int nContactsUpdated = 0;
						int nContactsCreated = 0;
						int nGroupsUpdated = 0;
						int nGroupsCreated = 0;
						int nLegalEntitiesUpdated = 0;
						int nLegalEntitiesCreated = 0;
						int nUnspecifiedAccountsUpdated = 0;
						int nUnspecifiedAccountsCreated = 0;
						progressMeter.setTotal(sheet.getLastRowNum());
						Iterator<Row> rows = sheet.rowIterator();
						int nRow = 0;
						int maxCell = 0;
						Row row = null;
						AccountRecordDefinition recordDefinition = new AccountRecordDefinition();
						// Read first row with attribute names
						if(rows.hasNext()) {
							nRow += 1;
							row = rows.next();
							maxCell = this.readImportDefinition(
								row, 
								nRow, 
								recordDefinition,
								importReport
							);
						}
						// Read data
						while (rows.hasNext()) {
							nRow += 1;
							progressMeter.setCurrent(nRow);
							nLinesRead += 1;
							Account account = null;
							String multiMatchList = "";
							boolean isCreation = false;
							boolean isUpdate = false;
							String appendErrorRow = null;
							AccountRecord accountRecord = new AccountRecord(recordDefinition);
							accountRecord.setTypeExplicitlySet(false);
							accountRecord.setXriExplicitlySet(false);
							accountRecord.setAccountType(AccountType.Contact);
							row = (Row) rows.next();
							List<String> errors = new ArrayList<String>();
							this.readAccountRecord(
								row, 
								nRow, 
								maxCell, 
								accountRecord,
								importReport,
								errors
							);
							String accountHref = "";
							// Import Members
							if (isImportMembershipMode) {
								groupMember = null;
								List<Account> matchingAccounts = null;
								// try to locate contacts with firstName and lastName only
								List<Contact> matchingContacts = this.findContact(
									accountRecord.getFirstName(),
									accountRecord.getLastName(),
									null,
									accountRecord.getEmailAddress(),
									null,
									accountSegment
								);
								if (matchingContacts != null) {
									matchingAccounts = new ArrayList<Account>();
									for (Iterator<Contact> c = matchingContacts.iterator(); c.hasNext();) {
										try {
											matchingAccounts.add((Account)c.next());
										} catch (Exception e) {}
									}
								}
								// try to locate legal entities with name only
								List<AbstractGroup> matchingAbstractGroups = this.findAbstractGroup(
									accountRecord.getCompany(),
									null,
									null,
									null,
									false, /* Group allowed: no */
									true,  /* LegalEntity allowed: yes */
									false, /* UnspecifiedAccount allowed: no */
									accountSegment
								);
								if (matchingAbstractGroups != null) {
									if (matchingAccounts == null) {
										matchingAccounts = new ArrayList<Account>();
									}
									for (Iterator<AbstractGroup> l = matchingAbstractGroups.iterator(); l.hasNext();) {
										try {
											matchingAccounts.add(l.next());
										} catch (Exception e) {}
									}
								}
								if (matchingAccounts != null) {
									for (Iterator<Account> a = matchingAccounts.iterator(); a.hasNext();) {
										try {
											pm.currentTransaction().begin();
											Account acct = (Account)a.next();
											// create or update membership
											groupMember = this.createOrUpdateMember(
												parentGroup,
												acct,
												null, /* no particular membership role */
												null, // userStrings
												null, // userCodes
												null, // userNumbers
												FEATURE_MEMBERROLE,
												codes,
												accountSegment
											);
											pm.currentTransaction().commit();
										} catch (Exception e) {
											ServiceException e0 = new ServiceException(e);
											e0.log();
											errors.add(e0.getMessage());
											try {
												pm.currentTransaction().rollback();
											} catch(Exception e1) {}
										}
									}
								}
							}
							// Import Accounts
							else {
								boolean updateExisting = false;
								List<Contact> matchingContacts = null;
								List<AbstractGroup> matchingAbstractGroups = null;
								if (accountRecord.isXriExplicitlySet()) {
									// try to find existing account with provided xri
									try {
										account = (Account)pm.getObjectById(new Path(accountRecord.getXri()));
									} catch (Exception e) {
										new ServiceException(e).log();
									}
									if (account != null) {
										accountRecord.setTypeExplicitlySet(true);
										accountRecord.setAccountType(AccountType.NA);
										if (account instanceof Contact) {
											accountRecord.setAccountType(AccountType.Contact);
											matchingContacts = new ArrayList<Contact>();
											matchingContacts.add((Contact)account);
										} else if (account instanceof Group) {
											accountRecord.setAccountType(AccountType.Group);
											matchingAbstractGroups = new ArrayList<AbstractGroup>();
											matchingAbstractGroups.add((AbstractGroup)account);
										} else if (account instanceof LegalEntity) {
											accountRecord.setAccountType(AccountType.LegalEntity);
											matchingAbstractGroups = new ArrayList<AbstractGroup>();
											matchingAbstractGroups.add((AbstractGroup)account);
										} else if (account instanceof UnspecifiedAccount) {
											accountRecord.setAccountType(AccountType.UnspecifiedAccount);
											matchingAbstractGroups = new ArrayList<AbstractGroup>();
											matchingAbstractGroups.add((AbstractGroup)account);
										}
									}
								}
								if(!accountRecord.isTypeExplicitlySet()) {
									// try to find existing account to determine dtype
									matchingContacts = this.findContact(
										accountRecord.getFirstName(),
										accountRecord.getLastName(),
										accountRecord.getAliasName(),
										accountRecord.getEmailAddress(),
										accountRecord.getExtString0(),
										accountSegment
									);
									if(matchingContacts == null && accountRecord.getExtString0() == null) {
										// try again without aliasName
										matchingContacts = this.findContact(
											accountRecord.getFirstName(),
											accountRecord.getLastName(),
											null,
											accountRecord.getEmailAddress(),
											accountRecord.getExtString0(),
											accountSegment
										);
									}
									if(matchingContacts == null && accountRecord.getExtString0() == null) {
										// try again without aliasName and without emailAddress
										matchingContacts = this.findContact(
											accountRecord.getFirstName(),
											accountRecord.getLastName(),
											null,
											null,
											accountRecord.getExtString0(),
											accountSegment
										);
									}
									if(matchingContacts != null) {
										accountRecord.setTypeExplicitlySet(true);
									} else {
										matchingAbstractGroups = this.findAbstractGroup(
											accountRecord.getCompany(),
											accountRecord.getAliasName(),
											accountRecord.getEmailAddress(),
											accountRecord.getExtString0(),
											true,
											true,
											true,
											accountSegment
										);
										if(matchingAbstractGroups == null && accountRecord.getExtString0() == null) {
											// try again without emailaddress
											matchingAbstractGroups = this.findAbstractGroup(
												accountRecord.getCompany(),
												accountRecord.getAliasName(),
												null,
												accountRecord.getExtString0(),
												true,
												true,
												true,
												accountSegment
											);
										}
										if(matchingAbstractGroups != null) {
											AbstractGroup matchingAbstractGroup = (AbstractGroup)(matchingAbstractGroups.iterator().next());
											if (matchingAbstractGroup instanceof Group) {
												accountRecord.setTypeExplicitlySet(true);
												accountRecord.setAccountType(AccountType.Group);
											} else if (matchingAbstractGroup instanceof LegalEntity) {
												accountRecord.setTypeExplicitlySet(true);
												accountRecord.setAccountType(AccountType.LegalEntity);
											} else if (matchingAbstractGroup instanceof UnspecifiedAccount) {
												accountRecord.setTypeExplicitlySet(true);
												accountRecord.setAccountType(AccountType.UnspecifiedAccount);
											}
										}
									}
								}
								if(accountRecord.getAccountType() == AccountType.Contact) {
									if (matchingContacts == null) {
										matchingContacts = this.findContact(
											accountRecord.getFirstName(),
											accountRecord.getLastName(),
											accountRecord.getAliasName(),
											accountRecord.getEmailAddress(),
											accountRecord.getExtString0(),
											accountSegment
										);
									}
									if(matchingContacts == null && accountRecord.getExtString0() == null) {
										// try again without aliasName
										matchingContacts = this.findContact(
											accountRecord.getFirstName(),
											accountRecord.getLastName(),
											null,
											accountRecord.getEmailAddress(),
											accountRecord.getExtString0(),
											accountSegment
										);
									}
									if(matchingContacts == null && accountRecord.getExtString0() == null) {
										// try again without aliasName and without emailaddress
										matchingContacts = this.findContact(
											accountRecord.getFirstName(),
											accountRecord.getLastName(),
											null,
											null,
											accountRecord.getExtString0(),
											accountSegment
										);
									}
									if(matchingContacts != null) {
										// at least 1 match with existing contacts
										updateExisting = true;
										for(Iterator<Contact> c = matchingContacts.iterator(); c.hasNext(); ) {
											Contact matchingContact = (Contact)c.next();
											if (c.hasNext()) {
												// more than 1 match
												updateExisting = false;;
												accountHref = this.getSelectObjectHref(matchingContact);
												multiMatchList += "<br><a href='" + accountHref + " target='_blank'><b>" + (new ObjectReference(matchingContact, app)).getTitle() + "</b> [" + matchingContact.refMofId() + "]</a>";
											} else if (updateExisting) {
												nContactsUpdated += 1;
												isUpdate = true;
												account = matchingContact;
											}
										}
									} else {
										// no match with existing contacts
										if (
											// minimum requirements to create contact
											((accountRecord.getFirstName() != null) || (accountRecord.getLastName() != null))
										) {
											try {
												pm.currentTransaction().begin();
												Contact contact = pm.newInstance(Contact.class);
												contact.setFirstName(accountRecord.getFirstName());
												contact.setLastName(accountRecord.getLastName());
												contact.setExtString0(accountRecord.getExtString0());
												accountSegment.addAccount(
													Base.getInstance().getUidAsString(),
													contact
												);													
												pm.currentTransaction().commit();
												account = contact;
											} catch (Exception e) {
												new ServiceException(e).log();
												try {
													pm.currentTransaction().rollback();
												} catch(Exception e1) {}
											}
										}
										if (account != null) {
											nContactsCreated += 1;
											isCreation = true;
										} else {
											// creation failed
											appendErrorRow = "<tr class='gridTableRowFull'><td class='err' colspan='" + (maxCell+2) + "'>CREATION FAILED [<b>" + accountRecord.getAccountType() + "</b>]</td></tr>";
										}
									}
									if(account != null) {
										// update new or existing contact
										accountHref = this.getSelectObjectHref(account);
										try {
											Contact contact = (Contact)account;
											pm.currentTransaction().begin();
											this.updateAccount(
												accountRecord,
												contact,
												nRow,
												codes,
												app,
												errors
											);
											pm.currentTransaction().commit();
										} catch (Exception e) {
											ServiceException e0 = new ServiceException(e);
											e0.log();
											errors.add(e0.getMessage());
											try {
												pm.currentTransaction().rollback();
											} catch(Exception e1) {}
										}
									}
								} else if (
									accountRecord.getAccountType() == AccountType.Group ||
									accountRecord.getAccountType() == AccountType.LegalEntity ||
									accountRecord.getAccountType() == AccountType.UnspecifiedAccount
								) {
									if (matchingAbstractGroups == null) {
										matchingAbstractGroups = this.findAbstractGroup(
											accountRecord.getCompany(),
											accountRecord.getAliasName(),
											accountRecord.getEmailAddress(),
											accountRecord.getExtString0(),
											accountRecord.getAccountType() == AccountType.Group,
											accountRecord.getAccountType() == AccountType.LegalEntity,
											accountRecord.getAccountType() == AccountType.UnspecifiedAccount,
											accountSegment
										);
										if(matchingAbstractGroups == null && accountRecord.getExtString0() == null) {
											// try again without emailaddress
											matchingAbstractGroups = this.findAbstractGroup(
												accountRecord.getCompany(),
												accountRecord.getAliasName(),
												null,
												accountRecord.getExtString0(),
												accountRecord.getAccountType() == AccountType.Group,
												accountRecord.getAccountType() == AccountType.LegalEntity,
												accountRecord.getAccountType() == AccountType.UnspecifiedAccount,
												accountSegment
											);
										}
									}
									if(matchingAbstractGroups != null) {
										// at least 1 match with existing AbstractGroups
										updateExisting = true;
										for(Iterator<AbstractGroup> c = matchingAbstractGroups.iterator(); c.hasNext(); ) {
											AbstractGroup matchingAbstractGroup = (AbstractGroup)c.next();
											if (c.hasNext()) {
												// more than 1 match
												updateExisting = false;;
												accountHref = this.getSelectObjectHref(matchingAbstractGroup);
												multiMatchList += "<br><a href='" + accountHref + " target='_blank'><b>" + (new ObjectReference(matchingAbstractGroup, app)).getTitle() + "</b> [" + matchingAbstractGroup.refMofId() + "]</a>";
											} else if (updateExisting) {
												isUpdate = true;
												if (accountRecord.getAccountType() == AccountType.Group) {
													nGroupsUpdated += 1;
													account = (Group)matchingAbstractGroup;
												} else if (accountRecord.getAccountType() == AccountType.LegalEntity) {
													nLegalEntitiesUpdated += 1;
													account = (LegalEntity)matchingAbstractGroup;
												} else if (accountRecord.getAccountType() == AccountType.UnspecifiedAccount) {
													nUnspecifiedAccountsUpdated += 1;
													account = (UnspecifiedAccount)matchingAbstractGroup;
												}
											}
										}
									} else {
										// no match with existing AbstractGroups
										if (
											// minimum requirements to create AbstractGroup
											(accountRecord.getCompany() != null)
										) {
											try {
												pm.currentTransaction().begin();
												if (accountRecord.getAccountType() == AccountType.Group) {
													Group group = pm.newInstance(Group.class);
													group.setName(accountRecord.getCompany());
													group.setExtString0(accountRecord.getExtString0());
													accountSegment.addAccount(
														Base.getInstance().getUidAsString(),
														group
													);
													account = group;
												} else if (accountRecord.getAccountType() == AccountType.LegalEntity) {
													LegalEntity legalEntity = pm.newInstance(LegalEntity.class);
													legalEntity.setName(accountRecord.getCompany());
													legalEntity.setExtString0(accountRecord.getExtString0());
													accountSegment.addAccount(
														Base.getInstance().getUidAsString(),
														legalEntity
													);
													account = legalEntity;
												} else if (accountRecord.getAccountType() == AccountType.UnspecifiedAccount) {
													UnspecifiedAccount unspecifiedAccount = pm.newInstance(UnspecifiedAccount.class);
													unspecifiedAccount.setName(accountRecord.getCompany());
													unspecifiedAccount.setExtString0(accountRecord.getExtString0());
													accountSegment.addAccount(
														Base.getInstance().getUidAsString(),
														unspecifiedAccount
													);
													account = unspecifiedAccount;
												}
												pm.currentTransaction().commit();
											} catch (Exception e) {
												new ServiceException(e).log();
												try {
													pm.currentTransaction().rollback();
												} catch(Exception e1) {}
											}
										}
										if(account instanceof Group) {
											nGroupsCreated += 1;
											isCreation = true;
										} else if (account instanceof LegalEntity) {
											nLegalEntitiesCreated += 1;
											isCreation = true;
										} else if (account instanceof UnspecifiedAccount) {
											nUnspecifiedAccountsCreated += 1;
											isCreation = true;
										} else {
											// creation failed
											appendErrorRow = "<tr class='gridTableRowFull'><td class='err' colspan='" + (maxCell+2) + "'>CREATION FAILED [<b>" + accountRecord.getAccountType() + "</b>]</td></tr>";
										}
									}
									if(account != null) {
										accountHref = this.getSelectObjectHref(account);
										try {
											pm.currentTransaction().begin();
											this.updateAccount(
												accountRecord,
												account,
												nRow,
												codes,
												app,
												errors
											);
											pm.currentTransaction().commit();
										} catch (Exception e) {
											ServiceException e0 = new ServiceException(e).log();
											errors.add(e0.getMessage());
											try {
												pm.currentTransaction().rollback();
											} catch(Exception e1) {}
										}
									}
								}
							}
							accountRecord.printImportStatus(importReport);
							importReport.append("</tr>");
							if(appendErrorRow != null) {
								importReport.append(appendErrorRow);
							}
							if(isImportMembershipMode) {
								importReport.append("<tr class='gridTableRowFull'>");
								importReport.append("<td class=\"" + (!errors.isEmpty() ? "err" : "match") + "\" colspan=\"" + (maxCell+2) + "\">");
								importReport.append("MEMBER " + (!errors.isEmpty() ? "FAILED [Reason: " + errors.toString() + "]" : "OK") + ":");
								if(groupMember != null) {
									String memberHref = this.getSelectObjectHref(groupMember);
									importReport.append("<a href=\"" + memberHref + "\" target=\"_blank\"><b>" + (new ObjectReference(groupMember, app)).getTitle() + "</b> [" + groupMember.refMofId() + "]</a>");
								} else {
									importReport.append(this.ATTR_FIRSTNAME + "=" + accountRecord.getFirstName() + "/" + this.ATTR_LASTNAME + "=" + accountRecord.getLastName() + "/" + this.ATTR_ALIASNAME + "=" + accountRecord.getAliasName() + "/" + this.ATTR_EMAILADDRESS + "=" + accountRecord.getEmailAddress() + "/" + this.ATTR_COMPANY + "=" + accountRecord.getCompany());														
								}
								importReport.append("</td>");
								importReport.append("</tr>");
							} else {
								if(isCreation) {
									importReport.append("<tr class='gridTableRowFull'>");
									importReport.append("<td class=\"" + (!errors.isEmpty() ? "err" : "match") + "\" colspan=\"" + (maxCell+2) + "\">");
									importReport.append("CREATE " + (!errors.isEmpty() ? "FAILED [Reason: " + errors.toString() + "]" : "OK") + "[<b>" + accountRecord.getAccountType() + "</b>]: <a href=\"" + accountHref + "\" target=\"_blank\"><b>" + (new ObjectReference(account, app)).getTitle() + "</b> [" + account.refMofId() + "]</a>");
									importReport.append("</td>");
									importReport.append("</tr>");
								}
								if(isUpdate) {
									if(!multiMatchList.isEmpty()) {
										importReport.append("<tr class='gridTableRowFull'>");
										importReport.append("<td class=\"err\" colspan=\"" + (maxCell+2) + "\">");
										importReport.append("NO UPDATE [<b>" + accountRecord.getAccountType() + "</b>] - Multiple Matches:" + multiMatchList);
										importReport.append("</td>");
										importReport.append("</tr>");
									} else {
										importReport.append("<tr class='gridTableRowFull'>");
										importReport.append("<td class=\"" + (!errors.isEmpty() ? "err" : "match") + "\" colspan=\"" + (maxCell+2) + "\">");
										importReport.append("UPDATE " + (!errors.isEmpty() ? "FAILED [Reason is:" + errors.toString() + "]" : "OK") + " [<b>" + accountRecord.getAccountType() + "</b>]: <a href=\"" + accountHref + "\" target=\"_blank\"><b>" + (new ObjectReference(account, app)).getTitle() + "</b> [" + account.refMofId() + "]</a>");
										importReport.append("</td>");
										importReport.append("</tr>");
									}
								}
							}
						} /* while */
						// Spacer
						importReport.append("<tr class='gridTableRowFull' style=\"background-color:white;\">");
						importReport.append("  <td colspan='" + (maxCell+2) + "'>&nbsp;</td>");								
						importReport.append("</tr>");
						// Summary
						importReport.append("<tr class='sheetInfo gridTableRowFull'>");
						importReport.append("<td colspan=\"" + (maxCell+2) + "\">");
						importReport.append("Sheet: <b>" + sheet.getSheetName() + "</b> |");
						importReport.append("data lines <b>read: " + nLinesRead + "</b><br>");
						importReport.append("</td>");
						importReport.append("</tr>");
						importReport.append("<tr class='sheetInfo gridTableRowFull'>");
						importReport.append("   <td>&nbsp;</td>");
						importReport.append("   <td><b>Created</b></td>");
						importReport.append("<td colspan=\"" + maxCell + "\"><b>Updated</b></td>");
						importReport.append("</tr>");
						importReport.append("<tr class='sheetInfo gridTableRowFull'>");
						importReport.append("<td><b>" + AccountType.Contact.name() + "</b></td>");
						importReport.append("   <td>" + nContactsCreated + "</td>");
						importReport.append("   <td colspan=\"" + maxCell + "\">" + nContactsUpdated + "</td>");
						importReport.append("</tr>");
						importReport.append("<tr class='sheetInfo gridTableRowFull'>");
						importReport.append("   <td><b>" + AccountType.Group.name() + "</b></td>");
						importReport.append("   <td>" + nGroupsCreated + "</td>");
						importReport.append("   <td colspan=\"" + maxCell + "\">" + nGroupsUpdated + "</td>");
						importReport.append("</tr>");
						importReport.append("<tr class='sheetInfo gridTableRowFull'>");
						importReport.append("   <td><b>" + AccountType.LegalEntity.name() + "</b></td>");
						importReport.append("   <td>" + nLegalEntitiesCreated + "</td>");
						importReport.append("   <td colspan=\"" + maxCell + "\">" + nLegalEntitiesUpdated + "</td>");
						importReport.append("</tr>");
						importReport.append("<tr class='sheetInfo gridTableRowFull'>");
						importReport.append("   <td><b>" + AccountType.UnspecifiedAccount.name() + "</b></td>");
						importReport.append("   <td>" + nUnspecifiedAccountsCreated + "</td>");
						importReport.append("   <td colspan=\"" + maxCell + "\">" + nUnspecifiedAccountsUpdated + "</td>");
						importReport.append("</tr>");
						if(isImportMembershipMode) {
							importReport.append("<tr class='sheetInfo gridTableRowFull'>");
							importReport.append("  <td colspan=\"" + (maxCell+2) + "\">" + parentGroup.getFullName() + " has now " + parentGroup.getMember().size() + " Members (before import: " + parentGroupMemberSize + "  Members)</td>");
							importReport.append("</tr>");
						} else {
							if (nLinesRead != nContactsCreated + nContactsUpdated +
								nGroupsCreated + nGroupsUpdated +
								nLegalEntitiesCreated + nLegalEntitiesUpdated +
								nUnspecifiedAccountsCreated + nUnspecifiedAccountsUpdated
							) {
								importReport.append("<tr class='sheetInfo gridTableRowFull'>");
								importReport.append("  <td class=\"err\" colspan=\"" + (maxCell+2) + "\">WARNING: some data lines were not processed due to data errors (e.g. multiple matches, missing first/last name, etc.)</td>");
								importReport.append("</tr>");
							}
						}
					}
				}
			} finally {
				new File(this.importFileLocation).delete();
			}
		}
	}

	/**
	 * @return the locale
	 */
	public short getLocale() {
		return locale;
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String FEATURE_POSTALCOUNTRY_CODE = "org:opencrx:kernel:address1:PostalAddressable:postalCountry";
	public static final String FEATURE_SALUTATION_CODE = "org:opencrx:kernel:account1:Contact:salutationCode";
	public static final String FEATURE_ACADEMICTITLE = "org:opencrx:kernel:account1:Contact:userCode1";
	public static final String FEATURE_MEMBERROLE = "org:opencrx:kernel:account1:Member:memberRole";
	
	public static final String EXTSTRING0PREFIX = "@#";
	public static final String LOCALE_SEPARATOR = ".";
	public static final String EOL_HTML = "<br>";

    public static final String UPLOAD_FILE_FIELD_NAME = "uploadFile";

	protected static NumberFormat DECIMAL_FORMAT_4 = new DecimalFormat("0000");
    
	private String ATTR_EXTSTRING0 = "extString0"; // index attribute
	private String ATTR_FIRSTNAME = "FirstName";   // index attribute
	private String ATTR_LASTNAME = "LastName";     // index attribute
	private String ATTR_ALIASNAME = "AliasName";   // index attribute
	private String ATTR_COMPANY = "Company";       // index attribute
	private String ATTR_DTYPE = "Dtype";           // index attribute
	private String ATTR_XRI = "xri";               // index attribute
	
	private String ATTR_MEMBEROF = "MemberOf";
	private String ATTR_MEMBERROLE = "MemberRole";
	private String ATTR_MEMBER_PREFIX = "Member:";
	private String ATTR_NOTETITLE = "NoteTitle";
	private String ATTR_NOTETEXT = "NoteText";
	private String ATTR_NOTECREATEDAT = "NoteCreatedAt";
	private String ATTR_TITLE = "title"; // coded --> salutationCode
	private String ATTR_SALUTATION = "salutation"; // string --> salutation
	private String ATTR_ACADEMICTITLE = "academicTitle";
	private String ATTR_MIDDLENAME = "middleName";
	private String ATTR_SUFFIX = "suffix";
	private String ATTR_NICKNAME = "nickName";
	private String ATTR_COMPANYROLE = "companyRole";
	private String ATTR_JOBTITLE = "jobTitle";
	private String ATTR_DEPARTMENT = "DEPARTMENT";
	private String ATTR_BIRTHDAY = "BIRTHDAY";
	private String ATTR_HOMEPHONE = "HOMEPHONE";
	private String ATTR_HOMEPHONE_AUTHORITY = "HOMEPHONE_AUTHORITY";
	private String ATTR_BUSINESSPHONE = "BUSINESSPHONE";
	private String ATTR_BUSINESSPHONE_AUTHORITY = "BUSINESSPHONE_AUTHORITY";
	private String ATTR_HOMEPHONE2 = "HOMEPHONE2";
	private String ATTR_HOMEPHONE2_AUTHORITY = "HOMEPHONE2_AUTHORITY";
	private String ATTR_BUSINESSPHONE2 = "BUSINESSPHONE2";
	private String ATTR_BUSINESSPHONE2_AUTHORITY = "BUSINESSPHONE2_AUTHORITY";
	private String ATTR_HOMEFAX = "HOMEFAX";
	private String ATTR_HOMEFAX_AUTHORITY = "HOMEFAX_AUTHORITY";
	private String ATTR_BUSINESSFAX = "BUSINESSFAX";
	private String ATTR_BUSINESSFAX_AUTHORITY = "BUSINESSFAX_AUTHORITY";
	private String ATTR_MOBILEPHONE = "MOBILEPHONE";
	private String ATTR_MOBILEPHONE_AUTHORITY = "MOBILEPHONE_AUTHORITY";
	private String ATTR_EMAILADDRESS = "EMAILADDRESS";   // index attribute
	private String ATTR_EMAILADDRESS_AUTHORITY = "EMAILADDRESS_AUTHORITY";
	private String ATTR_EMAIL2ADDRESS = "EMAIL2ADDRESS";
	private String ATTR_EMAIL2ADDRESS_AUTHORITY = "EMAIL2ADDRESS_AUTHORITY";
	private String ATTR_EMAIL3ADDRESS = "EMAIL3ADDRESS";
	private String ATTR_EMAIL3ADDRESS_AUTHORITY = "EMAIL3ADDRESS_AUTHORITY";
	private String ATTR_X500ADDRESS = "X500ADDRESS";
	private String ATTR_X500ADDRESS_AUTHORITY = "X500ADDRESS_AUTHORITY";
	private String ATTR_WEBPAGE = "WEBPAGE";
	private String ATTR_WEBPAGE_AUTHORITY = "WEBPAGE_AUTHORITY";
	
	private String ATTR_HOMEPOSTAL_AUTHORITY = "HOMEPOSTAL_AUTHORITY";
	private String ATTR_HOMEADDRESSLINE = "HOMEADDRESSLINE";
	private String ATTR_HOMESTREET = "HOMESTREET";
	private String ATTR_HOMECITY = "HOMECITY";
	private String ATTR_HOMEPOSTALCODE = "HOMEPOSTALCODE";
	private String ATTR_HOMESTATE = "HOMESTATE";
	private String ATTR_HOMECOUNTRY = "HOMECOUNTRY";
	private String ATTR_HOMECOUNTRYREGION = "HOMECOUNTRYREGION";
	
	private String ATTR_BUSINESSPOSTAL_AUTHORITY = "BUSINESSPOSTAL_AUTHORITY";
	private String ATTR_BUSINESSADDRESSLINE = "BUSINESSADDRESSLINE";
	private String ATTR_BUSINESSSTREET = "BUSINESSSTREET";
	private String ATTR_BUSINESSCITY = "BUSINESSCITY";
	private String ATTR_BUSINESSPOSTALCODE = "BUSINESSPOSTALCODE";
	private String ATTR_BUSINESSSTATE = "BUSINESSSTATE";
	private String ATTR_BUSINESSCOUNTRY = "BUSINESSCOUNTRY";
	private String ATTR_BUSINESSCOUNTRYREGION = "BUSINESSCOUNTRYREGION";

	private String ATTR_NOTES = "NOTES";
	private String ATTR_ASSISTANTSNAME = "ASSISTANTSNAME";
	private String ATTR_ASSISTANTSNAMEROLE = "ASSISTANTSNAMEROLE";
	private String ATTR_MANAGERSNAME = "MANAGERSNAME";
	private String ATTR_MANAGERSROLE = "MANAGERSROLE";
	private String ATTR_CATEGORIES = "CATEGORIES";
	private String ATTR_GROUPS = "GROUPS";
	private String ATTR_BUSINESSTYPE = "BUSINESSTYPE";

	private static List<String> USER_STRING_FIELDS = Arrays.asList(
		"userString0", "userString1", "userString2", "userString3"
	);
	private static List<String> USER_CODE_FIELDS = Arrays.asList(
		"userCode0", "userCode1", "userCode2", "userCode3"
	);
	private static List<String> USER_NUMBER_FIELDS = Arrays.asList(
		"userNumber0", "userNumber1", "userNumber2", "userNumber3"
	);
	
	private final boolean requiresAdminRole;
	private boolean hasPermission;
	private short locale;
	private String errorMessage;
	private String importFileLocation;
	
}
