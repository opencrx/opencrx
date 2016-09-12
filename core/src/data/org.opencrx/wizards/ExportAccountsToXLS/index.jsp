<%@	page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8" %><%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Name:        ISSA_xls_Contact_list.jsp
 * Description: create sheet with list of organizations
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2013-2013, CRIXP Corp., Switzerland
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
%><%@ page session="true" import="
java.util.*,
java.util.regex.*,
java.io.*,
java.text.*,
java.math.*,
java.net.URL,
java.net.URLEncoder,
java.net.MalformedURLException,
java.io.UnsupportedEncodingException,
org.opencrx.kernel.backend.*,
org.opencrx.kernel.portal.*,
org.openmdx.kernel.id.*,
org.openmdx.portal.servlet.databinding.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.kernel.log.*,
org.apache.poi.hssf.usermodel.*,
org.apache.poi.hssf.util.*,
org.apache.poi.poifs.filesystem.POIFSFileSystem
" %>
<%!
	private static HSSFSheet addSheet(
    HSSFWorkbook wb,
    String sheetName,
    boolean isLandscape,
    HSSFCellStyle headerStyle
	) {
    HSSFSheet sheet = wb.createSheet(sheetName);
    sheet.setMargin(HSSFSheet.TopMargin,    0.5);
    sheet.setMargin(HSSFSheet.RightMargin,  0.3);
    sheet.setMargin(HSSFSheet.BottomMargin, 0.6);
    sheet.setMargin(HSSFSheet.LeftMargin,   0.5);
    sheet.setAutobreaks(true);

    HSSFPrintSetup ps = sheet.getPrintSetup();
    /*
    ps.setFitHeight((short)100);
    ps.setFitWidth((short)1);
    */
    ps.setPaperSize(HSSFPrintSetup.A4_PAPERSIZE);
    ps.setLandscape(isLandscape);
    ps.setFooterMargin(0.3);

    HSSFFooter footer = sheet.getFooter();
    footer.setRight(HSSFFooter.page() + " / " + HSSFFooter.numPages());

		String[] labels = new String[] {
		   "FullName",
		   "Title",
		   "FirstName",
		   "MiddleName",
		   "LastName",
		   "AliasName",
		   "NickName",
		   "Notes",
		   "HomeAddressLine",
		   "HomeStreet",
		   "HomeCity",
		   "HomePostalCode",
		   "HomeCountryRegion",
		   "HomeState",
		   "HomeFax",
		   "HomePhone",
		   "HomePhone2",
		   "MobilePhone",
		   "Birthday",
		   "EmailAddress",
		   "Email2Address",
		   "Email3Address",
		   "WebPage",
		   "Company",
		   "JobTitle",
		   "BusinessAddressLine",
		   "BusinessStreet",
		   "BusinessCity",
		   "BusinessPostalCode",
		   "BusinessCountryRegion",
		   "BusinessState",
		   "BusinessFax",
		   "BusinessPhone",
		   "BusinessPhone2",
		   "MemberOf",
		   "MemberRole",
		   /*
		   "AssistantsName",
		   "AssistantsNameRole",
		   "ManagersName",
		   "ManagersRole",
		   */
		   "Categories",
		   "extString0",
		   "extString1",
		   "extString2",
		   "extString3",
		   "extDate0",
		   "Dtype",
		   "XRI"
		};

    HSSFRow row = null;
    HSSFCell cell = null;
    short nRow = 0;
    short nCell = 0;
    row = sheet.createRow(nRow++);
    for (int i=0; i<labels.length; i++) {
        cell = row.createCell(nCell++);
        cell.setCellStyle(headerStyle);
        cell.setCellValue(labels[i]);
    }
    sheet.setColumnWidth((short)7, (short)8000);
    sheet.setColumnWidth((short)8, (short)8000);
    sheet.setColumnWidth((short)24, (short)8000);
    sheet.setColumnWidth((short)25, (short)8000);
    return sheet;
  }

	private static short addAccount(
    HSSFSheet sheet,
    org.opencrx.kernel.account1.jmi1.Account account,
    short rowNumber,
    HSSFCellStyle wrappedStyle,
    HSSFCellStyle topAlignedStyle,
    SimpleDateFormat exceldate,
    ApplicationContext app,
    org.openmdx.portal.servlet.Codes codes
	) {
	  javax.jdo.PersistenceManager pm = javax.jdo.JDOHelper.getPersistenceManager(account);
	  org.opencrx.kernel.account1.jmi1.Contact contact = null;
	  org.opencrx.kernel.account1.jmi1.LegalEntity legalEntity = null;
	  org.opencrx.kernel.account1.jmi1.UnspecifiedAccount unspecifiedAccount = null;
	  org.opencrx.kernel.account1.jmi1.Group group = null;

    if (account instanceof org.opencrx.kernel.account1.jmi1.Contact) {
        contact = (org.opencrx.kernel.account1.jmi1.Contact)account;
    } else if (account instanceof org.opencrx.kernel.account1.jmi1.LegalEntity) {
        legalEntity = (org.opencrx.kernel.account1.jmi1.LegalEntity)account;
    } else if (account instanceof org.opencrx.kernel.account1.jmi1.UnspecifiedAccount) {
        unspecifiedAccount = (org.opencrx.kernel.account1.jmi1.UnspecifiedAccount)account;
    } else if (account instanceof org.opencrx.kernel.account1.jmi1.Group) {
        group = (org.opencrx.kernel.account1.jmi1.Group)account;
    }

    DataBinding postalHomeDataBinding = new PostalAddressDataBinding("[isMain=(boolean)true];usage=(short)400?zeroAsNull=true");
    DataBinding faxHomeDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)430;automaticParsing=(boolean)true");
    DataBinding phoneHomeDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)400;automaticParsing=(boolean)true");
    DataBinding phoneOtherDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)1800;automaticParsing=(boolean)true");
    DataBinding phoneMobileDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)200;automaticParsing=(boolean)true");
    DataBinding mailBusinessDataBinding = new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)500;[emailType=(short)1]");
    DataBinding mailHomeDataBinding = new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)400;[emailType=(short)1]");
    DataBinding mailOtherDataBinding = new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)1800;[emailType=(short)1]");
    org.openmdx.portal.servlet.databinding.CompositeObjectDataBinding webPageBusinessDataBinding =
        new org.openmdx.portal.servlet.databinding.CompositeObjectDataBinding("type=org:opencrx:kernel:account1:WebAddress;disabled=(boolean)false;[isMain=(boolean)true];usage=(short)500");
    DataBinding postalBusinessDataBinding = new PostalAddressDataBinding("[isMain=(boolean)true];usage=(short)500?zeroAsNull=true");
    DataBinding faxBusinessDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)530;automaticParsing=(boolean)true");
    DataBinding phoneBusinessDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)500;automaticParsing=(boolean)true");

    HSSFRow row = null;
    HSSFCell cell = null;
    short nRow = rowNumber;
    short nCell = 0;
    row = sheet.createRow(nRow++);

    List memberOfList = new ArrayList();
    List memberRoleList = new ArrayList();
    List jobTitleList = new ArrayList();

    try {
      org.opencrx.kernel.account1.cci2.AccountMembershipQuery accountMembershipFilter = (org.opencrx.kernel.account1.cci2.AccountMembershipQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountMembership.class);
      accountMembershipFilter.distance().equalTo(
        new Short((short)-1) // only direct/immediate memberships are of interest
      );
      accountMembershipFilter.forAllDisabled().isFalse();
    	for(
    		Iterator am = account.getAccountMembership(accountMembershipFilter).iterator();
    		am.hasNext();
    	) {
    	  org.opencrx.kernel.account1.jmi1.AccountMembership accountMembership =
    	    (org.opencrx.kernel.account1.jmi1.AccountMembership)am.next();
    	  try {
    	      org.opencrx.kernel.account1.jmi1.Member member = accountMembership.getMember();
    	      org.opencrx.kernel.account1.jmi1.Account accountFrom = accountMembership.getAccountFrom();
    	      memberOfList.add(accountFrom.getFullName());
    	      String rolesText = "";
            for(Iterator roles = accountMembership.getMemberRole().iterator(); roles.hasNext(); ) {
                if (rolesText.length() > 0) {
                    rolesText += ";";
                }
                rolesText += codes.getLongTextByCode("memberRole", app.getCurrentLocaleAsIndex(), true).get(new Short(((Short)roles.next()).shortValue()));
    	      }
    	      memberRoleList.add(rolesText);
    	      String jobTitleM = member.getUserString0();
    	      if (jobTitleM == null) {jobTitleM = "";}
    	      jobTitleList.add(jobTitleM);
        }
        catch(Exception em) {
          new ServiceException(em).log();
        }
    	}
    }
    catch(Exception e) {
      new ServiceException(e).log();
    }

    //FullName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account.getFullName() != null) {cell.setCellValue(account.getFullName());}

    //Title
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact != null) {
        if (contact.getSalutationCode() != 0) {
        	cell.setCellValue((String)(codes.getShortText("salutationCode", app.getCurrentLocaleAsIndex(), true, true).get(contact.getSalutationCode())));
        } else if (contact.getSalutation() != null) {
        	cell.setCellValue(contact.getSalutation());
        }
    }

    //FirstName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact != null) {
        if (contact.getFirstName() != null) {cell.setCellValue(contact.getFirstName());}
    }

    //MiddleName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact != null) {
        if (contact.getMiddleName() != null) {cell.setCellValue(contact.getMiddleName());}
    }

    //LastName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact != null) {
        if (contact.getLastName() != null) {cell.setCellValue(contact.getLastName());}
    }

    //AliasName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account.getAliasName() != null) {cell.setCellValue(account.getAliasName());}

    //NickName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact != null) {
        if (contact.getNickName() != null) {cell.setCellValue(contact.getNickName());}
    }

    //Notes
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account.getDescription() != null) {cell.setCellValue(account.getDescription());}

    if (contact != null) {
        //HomeAddressLine
        cell = row.createCell(nCell++);
        cell.setCellStyle(wrappedStyle);
        List<String> addressLines = (java.util.List)postalHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address!postalAddressLine");
        if (addressLines != null) {
            String s = "";
            boolean isFirstLine = true;
            for(Iterator m = ((java.util.List)addressLines).iterator(); m.hasNext(); ) {
                String line = (m.next().toString()).trim();
                if (line != null && line.length() > 0) {
                    s += (isFirstLine ? "" : "\r\n") + line;
                    isFirstLine = false;
                }
            }
            cell.setCellValue(s);
        }

        //HomeStreet
        cell = row.createCell(nCell++);
        cell.setCellStyle(wrappedStyle);
        addressLines = (java.util.List)postalHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address!postalStreet");
        if (addressLines != null) {
            String s = "";
            boolean isFirstLine = true;
            for(Iterator m = ((java.util.List)addressLines).iterator(); m.hasNext(); ) {
                String line = (m.next().toString()).trim();
                if (line != null && line.length() > 0) {
                    s += (isFirstLine ? "" : "\r\n") + line;
                    isFirstLine = false;
                }
            }
            cell.setCellValue(s);
        }

        //HomeCity
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        String value = (String)postalHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address!postalCity");
        if (value != null) {
            cell.setCellValue(value);
        }

        //HomePostalCode
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        value = (String)postalHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address!postalCode");
        if (value != null) {
            cell.setCellValue(value);
        }

        //HomeCountryRegion
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        Short svalue = (Short)postalHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address!postalCountry");
        if (svalue != null && ((short)svalue) != 0) {
            value = (codes.getLongTextByCode("country", app.getCurrentLocaleAsIndex(), true).get((short)svalue));
            if (value != null && (value.split("\\[").length>0)) {
                cell.setCellValue((value.split("\\[")[0]).trim());
            }
        }

        //HomeState
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        value = (String)postalHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address!postalState");
        if (value != null) {
            cell.setCellValue(value);
        }

        //HomeFax
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        value = (String)faxHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address*Fax!phoneNumberFull");
        if (value != null) {
            cell.setCellValue(value);
        }

        //HomePhone
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        value = (String)phoneHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Contact:address!phoneNumberFull");
        if (value != null) {
            cell.setCellValue(value);
        }

        //HomePhone2
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        value = (String)phoneOtherDataBinding.getValue(contact, "org:opencrx:kernel:account1:Account:address*Other!phoneNumberFull");
        if (value != null) {
            cell.setCellValue(value);
        }
    } else {
        for (int i = 0; i <= 8; i++) {
            cell = row.createCell(nCell++);
        }
    }

    //MobilePhone
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    String value = (String)phoneMobileDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull");
    if (value != null) {
        cell.setCellValue(value);
    }

    //Birthday
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact != null) {
        if (contact.getBirthdate() != null) {cell.setCellValue(exceldate.format(contact.getBirthdate()));}
    }

    //EmailAddress
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)mailBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!emailAddress");
    if (value != null) {
        cell.setCellValue(value);
    }

    //Email2Address
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact != null) {
        value = (String)mailHomeDataBinding.getValue(contact, "org:opencrx:kernel:account1:Account:address*Business!emailAddress");
        if (value != null) {
            cell.setCellValue(value);
        }
    }

    //Email3Address
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)mailOtherDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Other!emailAddress");
    if (value != null) {
        cell.setCellValue(value);
    }

    //WebPage
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)webPageBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:LegalEntity:address!webUrl");
    if (value != null) {
        cell.setCellValue(value);
    }

    //Company
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (contact == null && account != null) {
        if (((org.opencrx.kernel.account1.jmi1.AbstractGroup)account).getName() != null) {cell.setCellValue(((org.opencrx.kernel.account1.jmi1.AbstractGroup)account).getName());}
    }

    //JobTitle
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    String jobTitle = null;
    if (contact != null) {
        if (contact.getJobTitle() != null) {
        		jobTitle = contact.getJobTitle();
	    	}
        if (jobTitleList.size() > 0 && ((String)jobTitleList.get(0)).length() > 0) {
        		if (jobTitle == null) {
        				jobTitle = (String)jobTitleList.get(0);
        		} else {
        				jobTitle += " / " + jobTitleList.get(0); 
        		}
        }
    }
		cell.setCellValue(jobTitle);

    //BusinessAddressLine
    cell = row.createCell(nCell++);
    cell.setCellStyle(wrappedStyle);
    List<String> addressLines = (java.util.List)postalBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!postalAddressLine");
    if (addressLines != null) {
        String s = "";
        boolean isFirstLine = true;
        for(Iterator m = ((java.util.List)addressLines).iterator(); m.hasNext(); ) {
            String line = (m.next().toString()).trim();
            if (line != null && line.length() > 0) {
                s += (isFirstLine ? "" : "\r\n") + line;
                isFirstLine = false;
            }
        }
        cell.setCellValue(s);
    }

    //BusinessStreet
    cell = row.createCell(nCell++);
    cell.setCellStyle(wrappedStyle);
    addressLines = (java.util.List)postalBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!postalStreet");
    if (addressLines != null) {
        String s = "";
        boolean isFirstLine = true;
        for(Iterator m = ((java.util.List)addressLines).iterator(); m.hasNext(); ) {
            String line = (m.next().toString()).trim();
            if (line != null && line.length() > 0) {
                s += (isFirstLine ? "" : "\r\n") + line;
                isFirstLine = false;
            }
        }
        cell.setCellValue(s);
    }

    //BusinessCity
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)postalBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!postalCity");
    if (value != null) {
        cell.setCellValue(value);
    }

    //BusinessPostalCode
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)postalBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!postalCode");
    if (value != null) {
        cell.setCellValue(value);
    }

    //BusinessCountryRegion
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    Short svalue = (Short)postalBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!postalCountry");
    if (svalue != null && ((short)svalue) != 0) {
        value = (codes.getLongTextByCode("country", app.getCurrentLocaleAsIndex(), true).get((short)svalue));
        if (value != null && (value.split("\\[").length>0)) {
            cell.setCellValue((value.split("\\[")[0]).trim());
        }
    }

    //BusinessState
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)postalBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!postalState");
    if (value != null) {
        cell.setCellValue(value);
    }

    //BusinessFax
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)faxBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*BusinessFax!phoneNumberFull");
    if (value != null) {
        cell.setCellValue(value);
    }

    //BusinessPhone
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)phoneBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull");
    if (value != null) {
        cell.setCellValue(value);
    }

    //BusinessPhone2
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    value = (String)phoneOtherDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Other!phoneNumberFull");
    if (value != null) {
        cell.setCellValue(value);
    }

    //MemberOf
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (memberOfList.size() > 0) {
        cell.setCellValue((String)memberOfList.get(0));
    }

    //MemberRole
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (memberRoleList.size() > 0) {
        cell.setCellValue((String)memberRoleList.get(0));
    }

    /*
    //AssistantsName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);

    //AssistantsNameRole
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);

    //ManagersName
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);

    //ManagersRole
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    */

    //Categories
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    List<String> categories = (java.util.List)account.getCategory();
    if (categories != null) {
        String s = "";
        boolean isFirstLine = true;
        for(Iterator m = ((java.util.List)categories).iterator(); m.hasNext(); ) {
            s += (isFirstLine ? "" : ";") + (m.next()).toString();
            isFirstLine = false;
        }
        cell.setCellValue(s);
    }

    //extString0
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account.getExtString0() != null) {cell.setCellValue(account.getExtString0());}

    //extString1
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account.getExtString1() != null) {cell.setCellValue(account.getExtString1());}

    //extString2
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account.getExtString2() != null) {cell.setCellValue(account.getExtString2());}

    //extString3
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account.getExtString3() != null) {cell.setCellValue(account.getExtString3());}

    //extDate0
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    
    if (account.getExtDate0() != null) {
    	String ddmmyyyy = Integer.toString(account.getExtDate0().getDay());
    	ddmmyyyy += "." + Integer.toString(account.getExtDate0().getMonth());
    	ddmmyyyy += "." + Integer.toString(account.getExtDate0().getYear());
    	cell.setCellValue(ddmmyyyy);
    }

    //Dtype
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    if (account instanceof org.opencrx.kernel.account1.jmi1.Contact) {
        cell.setCellValue("Contact");
    } else if (account instanceof org.opencrx.kernel.account1.jmi1.LegalEntity) {
        cell.setCellValue("LegalEntity");
    } else if (account instanceof org.opencrx.kernel.account1.jmi1.UnspecifiedAccount) {
        cell.setCellValue("UnspecifiedAccount");
    } else if (account instanceof org.opencrx.kernel.account1.jmi1.Group) {
        cell.setCellValue("Group");
    } else {
        cell.setCellValue("unknown");
    }

    //XRI
    cell = row.createCell(nCell++);
    cell.setCellStyle(topAlignedStyle);
    cell.setCellValue(account.refMofId());


    for(int i = 1; i < memberOfList.size(); i++) {
        // create additional rows to list all memberOf accounts
        nCell = 0;
        row = sheet.createRow(nRow++);

        //FullName
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        if (account.getFullName() != null) {cell.setCellValue(account.getFullName());}

        //Title
        cell = row.createCell(nCell++);

        //FirstName
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        if (contact != null) {
            if (contact.getFirstName() != null) {cell.setCellValue(contact.getFirstName());}
        }

        //MiddleName
        cell = row.createCell(nCell++);

        //LastName
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        if (contact != null) {
            if (contact.getLastName() != null) {cell.setCellValue(contact.getLastName());}
        }

        //AliasName
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        if (account.getAliasName() != null) {cell.setCellValue(account.getAliasName());}

        for (int j = 0; j < 17; j++) {
            cell = row.createCell(nCell++);
        }

        //Company
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        if (contact == null && account != null) {
            if (((org.opencrx.kernel.account1.jmi1.AbstractGroup)account).getName() != null) {cell.setCellValue(((org.opencrx.kernel.account1.jmi1.AbstractGroup)account).getName());}
        }

        //userString0 (jobTitle)
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        cell.setCellValue((String)jobTitleList.get(i));
        
        for (int j = 0; j < 9; j++) {
            cell = row.createCell(nCell++);
        }

        //MemberOf
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        cell.setCellValue((String)memberOfList.get(i));

        //MemberRole
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        cell.setCellValue((String)memberRoleList.get(i));

        //Categories
        cell = row.createCell(nCell++);

        //extString0, extString1, extString2, extString3
        cell = row.createCell(nCell++);
        cell = row.createCell(nCell++);
        cell = row.createCell(nCell++);
        cell = row.createCell(nCell++);

        //extDate0
        cell = row.createCell(nCell++);
        
        //Dtype
        cell = row.createCell(nCell++);

        //XRI
        cell = row.createCell(nCell++);
        cell.setCellStyle(topAlignedStyle);
        cell.setCellValue(account.refMofId());

    }

    //return sheet;
    return nRow;
  }

%>
<%
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =	request.getParameter(Action.PARAMETER_REQUEST_ID);
	String requestIdParam = Action.PARAMETER_REQUEST_ID + "=" + requestId;
	String objectXri = request.getParameter("xri");
	if(app == null || objectXri == null || viewsCache.getView(requestId) == null) {
			response.sendRedirect(
				request.getContextPath() + "/" + WebKeys.SERVLET_NAME
			);
			return;
	}
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	Texts_1_0 texts = app.getTexts();
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
  <title>Export Accounts to XLS</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <link rel='shortcut icon' href='../../images/favicon.ico' />
</head>

<body class="ytheme-gray">
<%
	String formAction = "openCRX_Accounts.jsp";
	String filename = "openCRX_Accounts.xls";
	String sheetName = "Accounts";
	short largeFontSize = 12;
	short titleFontSize = 17;
	try {
			Codes codes = app.getCodes();
			UserDefinedView userView = new UserDefinedView(
   				pm.getObjectById(new Path(objectXri)),
   				app,
   				viewsCache.getView(requestId)
   			);

	    // Timezone is reusable
	    final TimeZone timeZone = TimeZone.getTimeZone(app.getCurrentTimeZone());
	    // DateFormat is not multi-thread-safe!
	    DateFormat dateTimeFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm");
	    dateTimeFormat.setLenient(false); // if the timestamp string is always complete
	    dateTimeFormat.setTimeZone(timeZone);

	    SimpleDateFormat exceldate = new SimpleDateFormat("dd-MM-yyyy");
			exceldate.setTimeZone(timeZone);

	    // Get account1 package
			org.opencrx.kernel.account1.jmi1.Account1Package accountPkg = org.opencrx.kernel.utils.Utils.getAccountPackage(pm);
			
	    RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
	
	    Path objPath = new Path(obj.refMofId());
	    String providerName = objPath.get(2);
	    String segmentName = objPath.get(4);
	    //String providerName = "ISSA";
	    //String segmentName = "Standard";

	    boolean exportSingleAccount = false;
    	Iterator i = null;
	    if (obj instanceof org.opencrx.kernel.account1.jmi1.Contact || obj instanceof org.opencrx.kernel.account1.jmi1.LegalEntity) {
	        exportSingleAccount = true;
	    } else {
	    	if (obj instanceof org.opencrx.kernel.account1.jmi1.Group) {
	      	i = ((org.opencrx.kernel.account1.jmi1.Group)obj).getMember().iterator();
	    	} else if (obj instanceof org.opencrx.kernel.account1.jmi1.AccountFilterGlobal) {
	       	i = ((org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)obj).getFilteredAccount().iterator();
	    	}
	    }
	
	    // Generate report
	    String location = UUIDs.getGenerator().next().toString();
	    File f = new File(
	        app.getTempFileName(location, "")
	    );
	    FileOutputStream os = new FileOutputStream(f);

	    HSSFWorkbook wb = new HSSFWorkbook();

			// Header Style (black background, orange/bold font)
			HSSFFont headerfont = wb.createFont();
			headerfont.setFontHeightInPoints((short)10);
			headerfont.setFontName("Tahoma");
			headerfont.setBoldweight(HSSFFont.BOLDWEIGHT_BOLD);
			headerfont.setColor(HSSFColor.ORANGE.index);

			HSSFCellStyle headerStyle = wb.createCellStyle();
			headerStyle.setFillForegroundColor(HSSFColor.BLACK.index);
			headerStyle.setFillPattern(HSSFCellStyle.SOLID_FOREGROUND);
			headerStyle.setFont(headerfont);
			
			// Wrapped Style
			HSSFCellStyle wrappedStyle = wb.createCellStyle();
			wrappedStyle.setVerticalAlignment(HSSFCellStyle.VERTICAL_TOP);
			wrappedStyle.setWrapText(true);
			
			// TopAligned Style
			HSSFCellStyle topAlignedStyle = wb.createCellStyle();
			topAlignedStyle.setVerticalAlignment(HSSFCellStyle.VERTICAL_TOP);
			
			HSSFSheet sheetAccounts = addSheet(wb, "Accounts", true, headerStyle);
      short spreadSheetRow = 1;
	    
	    while (
 	      ((i != null) && (i.hasNext())) ||
 	      exportSingleAccount
 	    ) {
 	    	try {
 		      org.opencrx.kernel.account1.jmi1.Account account = null;
 					if (exportSingleAccount) {
 							account = (org.opencrx.kernel.account1.jmi1.Account)obj;
 							exportSingleAccount = false;
 				  } else {
 					  	try {
 					  			org.opencrx.kernel.generic.jmi1.CrxObject crxObject =	(org.opencrx.kernel.generic.jmi1.CrxObject)i.next();
 					  			if (crxObject instanceof org.opencrx.kernel.account1.jmi1.Member) {
 					  					org.opencrx.kernel.account1.jmi1.Member member = (org.opencrx.kernel.account1.jmi1.Member)crxObject;
 					  					if (member.getAccount() != null) {
 					  						account = (org.opencrx.kernel.account1.jmi1.Account)member.getAccount();
 					  					}
 					  			} else if (crxObject instanceof org.opencrx.kernel.account1.jmi1.Account) {
 					  					account = (org.opencrx.kernel.account1.jmi1.Account)crxObject;
 									}
 					  	} catch (Exception e) {
 					  			new ServiceException(e).log();
 					  	}
 		      }
 					if (account == null) {continue;}
 					spreadSheetRow = addAccount(sheetAccounts, account, spreadSheetRow, wrappedStyle, topAlignedStyle, exceldate, app, codes);
 					
 	    	} catch (Exception e) {
 	    		new ServiceException(e).log();
 	    	}
	    }
		  
			wb.write(os);
	
			os.flush();
			os.close();
	
			Action downloadAction =
					new Action(
							Action.EVENT_DOWNLOAD_FROM_LOCATION,
							new Action.Parameter[]{
									new Action.Parameter(Action.PARAMETER_LOCATION, location),
									new Action.Parameter(Action.PARAMETER_NAME, filename),
									new Action.Parameter(Action.PARAMETER_MIME_TYPE, "application/vnd.ms-excel")
							},
							app.getTexts().getClickToDownloadText() + " " + filename,
							true
			);
			response.sendRedirect(
				request.getContextPath() + "/" + downloadAction.getEncodedHRef(requestId)
			);
%>
	Download report from <a href="../<%= downloadAction.getEncodedHRef(requestId) %>">here</a>
</body>
</html>
<%
	}
	catch (Exception e) {
		ServiceException e0 = new ServiceException(e);
		SysLog.warning("Can not create spreadsheet", "Wizard " + formAction);
		SysLog.warning(e0.getMessage(), e0.getCause());
		// Go back to previous view
		Action nextAction = new ObjectReference(
			(RefObject_1_0)pm.getObjectById(new Path(objectXri)),
			app
		).getSelectObjectAction();
		response.sendRedirect(
			request.getContextPath() + "/" + nextAction.getEncodedHRef()
		);
	}
	finally {
		if(pm != null) {
			pm.close();
		}
	}
%>
