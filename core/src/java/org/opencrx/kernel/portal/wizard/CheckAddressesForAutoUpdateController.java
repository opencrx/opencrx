/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CheckAddressesForAutoUpdateController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2014, CRIXP Corp., Switzerland
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

import java.text.DecimalFormat;
import java.text.NumberFormat;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.AccountAddressQuery;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.AddressFilterGlobal;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * CheckAddressesForAutoUpdateController
 *
 */
public class CheckAddressesForAutoUpdateController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CheckAddressesForAutoUpdateController(
	) {
		super();
	}
	
	/**
	 * Refresh action.
	 * 
	 * @throws ServiceException
	 */
	public void doRefresh(
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		PersistenceManager pm = this.getPm();
		RefObject_1_0 obj = this.getObject();
		NumberFormat formatter = new DecimalFormat("0");			
		int warningCounter = 0;
		int counter = 0;
		this.report = 
			"<table><tr><td>\n" + 
			"  The following addresses have authority set, but 'Check for auto update' failed\n" +
			"  <p>\n" +
			"  <table id=\"resultTable\" class=\"gridTableFull\">\n" + 
			"    <tr class=\"gridTableHeaderFull\"><!-- 2 columns -->\n" + 
			"	   <td align=\"left\">&nbsp;Address</td>\n" + 
			"	   <td align=\"left\">&nbsp;Authority</td>\n" + 
			"	   <td align=\"left\">&nbsp;No Candidate Addresses</td>\n" + 
			"	   <td align=\"left\">&nbsp;Fields With Differences</td>\n" + 
			"	  </tr>";
		if(obj instanceof AddressFilterGlobal) {
			AddressFilterGlobal addressFilter = (AddressFilterGlobal)obj;
			int batchCount = 0;
			org.opencrx.kernel.account1.cci2.AccountAddressQuery addressQuery = (AccountAddressQuery)pm.newQuery(AccountAddress.class);
			addressQuery.authority().isNonNull();
			for(AccountAddress addr: addressFilter.<AccountAddress>getFilteredAddress(addressQuery)) {
				if (
					addr instanceof org.opencrx.kernel.account1.jmi1.PostalAddress ||
					addr instanceof org.opencrx.kernel.account1.jmi1.EMailAddress
				) {
					if(batchCount >= 200) {
						batchCount = 0;
						System.out.println("addresses checked: " + formatter.format(counter));
					}
					counter += 1;
					org.opencrx.kernel.account1.jmi1.CheckForAutoUpdateResult checkResult = null;
					if (addr instanceof org.opencrx.kernel.account1.jmi1.PostalAddress) {
						org.opencrx.kernel.account1.jmi1.PostalAddress postalAddr = (org.opencrx.kernel.account1.jmi1.PostalAddress)addr;
						checkResult = postalAddr.checkForAutoUpdate();
					} else if (addr instanceof org.opencrx.kernel.account1.jmi1.EMailAddress) {
						org.opencrx.kernel.account1.jmi1.EMailAddress emailAddr = (org.opencrx.kernel.account1.jmi1.EMailAddress)addr;
						checkResult = emailAddr.checkForAutoUpdate();
					}
					if(checkResult != null) {
						String fieldsWithDifferences = "";
						boolean hasNoCandidateAddresses = true;
						if (checkResult.getCandidateAddress1() != null) {
							hasNoCandidateAddresses = false; 
							if(
								(checkResult.isCandidateQualifiesForAutoUpdate1() != null && 
								!checkResult.isCandidateQualifiesForAutoUpdate1().booleanValue())
							) {
								fieldsWithDifferences += checkResult.getCandidateNonMatchingFields1() + " ";
							}
						}
						if (checkResult.getCandidateAddress2() != null) {
							hasNoCandidateAddresses = false; 
							if (
								(checkResult.isCandidateQualifiesForAutoUpdate2() != null && 
								!checkResult.isCandidateQualifiesForAutoUpdate2().booleanValue())
							) {
								fieldsWithDifferences += checkResult.getCandidateNonMatchingFields2() + " ";
							}
						}
						if (checkResult.getCandidateAddress3() != null) {
							hasNoCandidateAddresses = false; 
							if (
								(checkResult.isCandidateQualifiesForAutoUpdate3() != null && 
								!checkResult.isCandidateQualifiesForAutoUpdate3().booleanValue())
							) {
								fieldsWithDifferences += checkResult.getCandidateNonMatchingFields3() + " ";
							}
						}
						if (checkResult.getCandidateAddress4() != null) {
							hasNoCandidateAddresses = false; 
							if (
								(checkResult.isCandidateQualifiesForAutoUpdate4() != null && 
								!checkResult.isCandidateQualifiesForAutoUpdate4().booleanValue())
							) {
								fieldsWithDifferences += checkResult.getCandidateNonMatchingFields4() + " ";
							}
						}
						if (checkResult.getCandidateAddress5() != null) {
							hasNoCandidateAddresses = false; 
							if (
								(checkResult.isCandidateQualifiesForAutoUpdate5() != null && 
								!checkResult.isCandidateQualifiesForAutoUpdate5().booleanValue())
							) {
								fieldsWithDifferences += checkResult.getCandidateNonMatchingFields5() + " ";
							}
						}
						if (hasNoCandidateAddresses || fieldsWithDifferences.length() > 0) {
							warningCounter++;
							Action selectAddressAction = null;
							if(addr != null) {
								ObjectReference addressRef  = new ObjectReference(addr, app);
								selectAddressAction = addressRef.getSelectObjectAction();
							}
							Action selectAuthorityAction = null;
							if(addr.getAuthority() != null) {
								ObjectReference authorityRef = new ObjectReference(addr.getAuthority(), app);
								selectAuthorityAction = authorityRef.getSelectObjectAction();
							}
							this.report += 
								"<tr class=\"gridTableRow\">\n" + 
								"  <td align=\"left\">" + (selectAddressAction == null ? "" : "<a href=\"" + selectAddressAction.getEncodedHRef() + "\" target=\"_blank\"><b><img class=\"popUpButton\" border=\"1\" alt=\"lookup\" title=\"" + selectAddressAction.getTitle() + "\" src=\"./images/" + selectAddressAction.getIconKey() + "\" />&nbsp;" + selectAddressAction.getTitle() + "</b></a>") + "</td>\n" +
								"  <td align=\"left\">" + (selectAuthorityAction == null ? "" : "<a href=\"" + selectAuthorityAction.getEncodedHRef() + "\" target=\"_blank\">" + selectAuthorityAction.getTitle() + "</a>") + "</td>\n" + 
								"  <td align=\"center\">" + (hasNoCandidateAddresses ? "<img src='./images/Alert.gif' alt='no candidate addresses' title='no candidate addresses found' />" : "") + "</td>\n" + 
								"  <td align=\"left\">" + fieldsWithDifferences + "</td>\n" + 
								"</tr>";
						}
					}
				}
			}
		}
		this.report += 
			"  </table>\n" + 
			"</td></tr></table>"; 
		this.report += 
			"<div>" + 
			"  Checked " + formatter.format(counter) + " addresses, " + formatter.format(warningCounter) + " with warnings" + 
			"</div>"; 
	}

	/**
	 * @return the report
	 */
	public String getReport() {
		return report;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private String report;

}
