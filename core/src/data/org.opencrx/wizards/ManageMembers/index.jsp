<%@  page contentType= "text/html;charset=utf-8" language="java" pageEncoding= "UTF-8" %><%
/**
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description:	Manage members
 * Owner:		the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * * Redistribution and use in source and binary forms, with or without
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
%>
<%@ page session="true" import="
java.util.*,
java.util.logging.Level,
java.io.*,
java.text.*,
java.math.*,
java.net.*,
java.sql.*,
javax.naming.Context,
javax.naming.InitialContext,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.opencrx.kernel.portal.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.base.exception.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.base.text.conversion.*,
org.openmdx.kernel.log.*,
org.apache.poi.ss.usermodel.*,
org.apache.poi.hssf.usermodel.*,
org.apache.poi.hssf.util.*
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
    if (contact != null) {
        if (contact.getJobTitle() != null) {cell.setCellValue(contact.getJobTitle());}
    }

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

    List memberOfList = new ArrayList();
    List memberRoleList = new ArrayList();

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
        }
        catch(Exception em) {
        	SysLog.log(Level.WARNING, "ManageMembers {0} - addAccount no permission", accountMembership.refGetPath());
        }
    	}
    }
    catch(Exception e) {
      new ServiceException(e).log();
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

        for (int j = 0; j < 10; j++) {
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
  final String WIZARD_NAME = "ManageMembers";
  final String FORMACTION   = WIZARD_NAME + ".jsp";
  request.setCharacterEncoding("UTF-8");
  ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
  ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
  String requestId =  request.getParameter(Action.PARAMETER_REQUEST_ID);
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
  org.openmdx.portal.servlet.Codes codes = app.getCodes();
%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<html>

<head>
  <title>Manage Members</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
  <link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
  <link rel="stylesheet" href="../../_style/colors.css">
  <link rel="stylesheet" href="../../_style/n2default.css">
  <link rel="stylesheet" href="../../_style/ssf.css">
  <!--[if lt IE 7]><script type="text/javascript" src="../../js/iehover-fix.js"></script><![endif]-->
  <script language="javascript" type="text/javascript" src="../../js/portal-all.js"></script>
  <link rel="shortcut icon" href="../../images/favicon.ico" />
</head>

<style type="text/css" media="all">
  .gridTableRowFull TD {white-space:nowrap;}
  .gridTableHeaderFull TD {white-space:nowrap;vertical-align:bottom;}
</style>

<%
final String location = UUIDConversion.toUID(UUIDs.newUUID());
String mode = (request.getParameter("mode") == null ? "0" : request.getParameter("mode")); // default is [Manage Members]
%>

<body>
<div id="container">
	<div id="wrap">

    <form name="ManageMembers" style="padding:0;border-top:3px solid #E4E4E4;margin:0;" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
      <input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" />
      <input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
      <input type="hidden" name="previousSheet" id="previousSheet" value="<%= location %>" />
      <input type="hidden" name="mode" id="mode" value="<%= mode %>" />
      <input type="hidden" name="paging" id="paging" value="" />
      <input type="checkbox" style="display:none;" name="isFirstCall" checked />
      <input type="checkbox" style="display:none;" name="isSelectionChange" id="isSelectionChange" />

			<div id="header" style="height:90px;">
		    <div id="logoTable">
		      <table id="headerlayout">
		        <tr id="headRow">
		          <td id="head" colspan="2">
		            <table id="info">
		              <tr>
		                <td id="headerCellLeft"><img id="logoLeft" src="../../images/logoLeft.gif" alt="openCRX" title="" /></td>
		                <td id="headerCellSpacerLeft"></td>
		                <td id="headerCellMiddle">&nbsp;</td>
		                <td id="headerCellRight"><img id="logoRight" src="../../images/logoRight.gif" alt="" title="" /></td>
		              </tr>
		            </table>
		          </td>
		        </tr>
		      </table>
		    </div>
<%
		    String accountTitle = "";
		    try {
			    // get reference of calling object
			    RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));

			    Path objectPath = new Path(objectXri);
			    String providerName = objectPath.get(2);
			    String segmentName = objectPath.get(4);

			    UserDefinedView userView = new UserDefinedView(
			      obj,
			      app,
			      viewsCache.getView(requestId)
			    );

			    // Get account segment
			    org.opencrx.kernel.account1.jmi1.Segment accountSegment =
			      (org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
			        new Path("xri:@openmdx:org.opencrx.kernel.account1/provider/" + providerName + "/segment/" + segmentName)
			       );

			    org.opencrx.kernel.account1.jmi1.Account accountSource = null;
			    if (obj instanceof org.opencrx.kernel.account1.jmi1.Account) {
			        accountSource = (org.opencrx.kernel.account1.jmi1.Account)obj;
			        accountTitle = (new ObjectReference(accountSource, app)).getTitle();
			    }
%>

			    <div id="etitle" style="height:20px;padding-left:12px;">
			       Manage Members of "<%= accountTitle %>"
			    </div>

					<div id="topnavi">
						<ul id="<%=CssClass.ssf_navigation %>" class="<%=CssClass.ssf_navigation %>" onmouseover="sfinit(this);">
							<li class="<%= mode.compareTo("0")==0 ? "selected" : "" %>"><a href="#" onclick="javascript:try{$('mode').value='0';}catch(e){};setTimeout('disableSubmit()', 10);$('Reload.Button').click();";><span>Manage Members</span></a></li>
							<li class="<%= mode.compareTo("1")==0 ? "selected" : "" %>"><a href="#" onclick="javascript:try{$('mode').value='1';}catch(e){};setTimeout('disableSubmit()', 10);$('Reload.Button').click();";><span>Add Members</span></a></li>
						</ul>
<%
						NumberFormat formatter = new DecimalFormat("0");

						// Format dates/times
						TimeZone timezone = TimeZone.getTimeZone(app.getCurrentTimeZone());
						SimpleDateFormat timeFormat = new SimpleDateFormat("dd-MMM-yyyy HH:mm", app.getCurrentLocale());
						timeFormat.setTimeZone(timezone);
						SimpleDateFormat timestamp = new SimpleDateFormat("yyyyMMdd_HHmmss", app.getCurrentLocale());
						timestamp.setTimeZone(timezone);
						SimpleDateFormat exceldate = new SimpleDateFormat("dd-MM-yyyy");
						exceldate.setTimeZone(timezone);


						final String MEMBER_CLASS = "org:opencrx:kernel:account1:Member";
						final String MEMBERSHIP_CLASS = "org:opencrx:kernel:account1:AccountMembership";
						final String ACCOUNTSEGMENT_CLASS = "org:opencrx:kernel:account1:Segment";
						final String ACCOUNT_CLASS = "org:opencrx:kernel:account1:Account";
						final String ACCOUNTFILTERGLOBAL_CLASS = "org:opencrx:kernel:account1:AccountFilterGlobal";
						final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
						final String LEGALENTITY_CLASS = "org:opencrx:kernel:account1:LegalEntity";
						final String GROUP_CLASS = "org:opencrx:kernel:account1:Group";
						final String UNSPECIFIEDACCOUNT_CLASS = "org:opencrx:kernel:account1:UnspecifiedAccount";
						final String EMAILADDRESS_CLASS = "org:opencrx:kernel:account1:EMailAddress";
						final String POSTALADDRESS_CLASS = "org:opencrx:kernel:account1:PostalAddress";

						final String ACCOUNT_FILTER_XRI_PREFIX = "ACCOUNT_FILTER_XRI_";

						final int DEFAULT_PAGE_SIZE = 20;

						final String colorDuplicate = "#FFA477";
						final String colorMember = "#D2FFD2";
						final String colorMemberDisabled = "#F2F2F2";

						final String CAUTION = "<img border='0' alt='' height='16px' src='../../images/caution.gif' />";
						final String SPREADSHEET = "<img border='0' alt='EXCEL' title='EXCEL' align='top' height='24px' src='../../images/spreadsheet.png' />";
						final String sheetName = "Accounts_(openCRX)_" + timestamp.format(new java.util.Date());
						File f = null;
						FileOutputStream os = null;
						HSSFWorkbook wb = null;
						Action downloadAction =	null;
						HSSFSheet sheetAccounts = null;
						HSSFFont headerfont = null;
						HSSFCellStyle headerStyle = null;
						HSSFCellStyle wrappedStyle = null;
						HSSFCellStyle topAlignedStyle = null;

						HSSFRow row = null;
						HSSFCell cell = null;
						short nRow = 0;
						short nCell = 0;

						String errorMsg = "";

						final String wildcard = ".*";
						String searchString = (request.getParameter("searchString") == null ? "" : request.getParameter("searchString"));
						String previousSearchString = (request.getParameter("previousSearchString") == null ? "" : request.getParameter("previousSearchString"));

						org.opencrx.kernel.account1.cci2.AccountQuery accountFilter = (org.opencrx.kernel.account1.cci2.AccountQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Account.class);
						accountFilter.forAllDisabled().isFalse();
						accountFilter.orderByFullName().ascending();

						org.opencrx.kernel.account1.cci2.AccountQuery searchAccountFullNameFilter = (org.opencrx.kernel.account1.cci2.AccountQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Account.class);
						searchAccountFullNameFilter.forAllDisabled().isFalse();
						searchAccountFullNameFilter.thereExistsFullName().like("(?i)" + wildcard + searchString + wildcard);
						searchAccountFullNameFilter.orderByFullName().ascending();

						org.opencrx.kernel.account1.cci2.ContactQuery contactFilter = (org.opencrx.kernel.account1.cci2.ContactQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Contact.class);
						contactFilter.forAllDisabled().isFalse();
						contactFilter.orderByFullName().ascending();

						org.opencrx.kernel.account1.cci2.LegalEntityQuery legalEntityFilter = (org.opencrx.kernel.account1.cci2.LegalEntityQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.LegalEntity.class);
						legalEntityFilter.forAllDisabled().isFalse();
						legalEntityFilter.orderByFullName().ascending();

						org.opencrx.kernel.account1.cci2.GroupQuery groupFilter = (org.opencrx.kernel.account1.cci2.GroupQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Group.class);
						groupFilter.forAllDisabled().isFalse();
						groupFilter.orderByFullName().ascending();

						org.opencrx.kernel.account1.cci2.UnspecifiedAccountQuery unspecifiedAccountFilter = (org.opencrx.kernel.account1.cci2.UnspecifiedAccountQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.UnspecifiedAccount.class);
						unspecifiedAccountFilter.forAllDisabled().isFalse();
						unspecifiedAccountFilter.orderByFullName().ascending();

						org.opencrx.kernel.account1.cci2.MemberQuery memberFilter = (org.opencrx.kernel.account1.cci2.MemberQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Member.class);
						memberFilter.orderByCreatedAt().ascending();

						org.opencrx.kernel.account1.cci2.EMailAddressQuery searchEMailAddressFilter = (org.opencrx.kernel.account1.cci2.EMailAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.EMailAddress.class);
						searchEMailAddressFilter.forAllDisabled().isFalse();
						searchEMailAddressFilter.thereExistsEmailAddress().like("(?i)" + wildcard + searchString + wildcard);
						searchEMailAddressFilter.orderByEmailAddress().ascending();

						org.opencrx.kernel.account1.cci2.PostalAddressQuery searchPostalAddressCityFilter = (org.opencrx.kernel.account1.cci2.PostalAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.PostalAddress.class);
						searchPostalAddressCityFilter.forAllDisabled().isFalse();
						searchPostalAddressCityFilter.thereExistsPostalCity().like("(?i)" + wildcard + searchString + wildcard);
						searchPostalAddressCityFilter.orderByPostalCity().ascending();

						org.opencrx.kernel.account1.cci2.PostalAddressQuery searchPostalAddressCodeFilter = (org.opencrx.kernel.account1.cci2.PostalAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.PostalAddress.class);
						searchPostalAddressCodeFilter.forAllDisabled().isFalse();
						searchPostalAddressCodeFilter.thereExistsPostalCode().like("(?i)" + wildcard + searchString + wildcard);
						searchPostalAddressCodeFilter.orderByPostalCode().ascending();

						int tabIndex = 1;
						int pageSize = DEFAULT_PAGE_SIZE;
						int displayStart = 0;
						long highAccount = 0;
						boolean isFirstCall = request.getParameter("isFirstCall") == null; // used to properly initialize various options
						boolean highAccountIsKnown = ((request.getParameter("highAccountIsKnown") != null) && (request.getParameter("highAccountIsKnown").length() > 0));
						boolean isSelectionChange = isFirstCall || request.getParameter("isSelectionChange") != null || previousSearchString.compareTo(searchString) != 0;
						String accountFilterXri = null;
						org.opencrx.kernel.account1.jmi1.AccountFilterGlobal selectedAccountFilterGlobal = null;
						int accountSelectorType = 1;
						  /*    0: select all accounts (segment)
						        1: select members only
						        2: select based on AccountFilter
						      100: Search based on "full name"
						      110: Search based on "e-mail address"
						      111: Search based on "postal address city"
						      112: Search based on "postal address zip"
						  */
						if (request.getParameter("accountSelectorType") != null && !request.getParameter("accountSelectorType").startsWith(ACCOUNT_FILTER_XRI_PREFIX)) {
						    try {
						        accountSelectorType = Integer.parseInt(request.getParameter("accountSelectorType"));
						    } catch (Exception e) {}
						} else if (request.getParameter("accountSelectorType") != null && request.getParameter("accountSelectorType").startsWith(ACCOUNT_FILTER_XRI_PREFIX)) {
						    accountFilterXri = request.getParameter("accountSelectorType").substring(ACCOUNT_FILTER_XRI_PREFIX.length());
						    try {
						        selectedAccountFilterGlobal = (org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)pm.getObjectById(new Path(accountFilterXri));
						        accountSelectorType = 2;
						    } catch (Exception e) {}
						}
						if (mode.compareTo("0") == 0) {
						    accountSelectorType = 1; // model [Manage Members] requires selection of membersOnly
						} else {
						    if (accountSelectorType == 1) {
						        accountSelectorType = 100; // set to search based on Full Name
						    }
						}
						boolean duplicatesOnly = ((request.getParameter("duplicatesOnly") != null) && (request.getParameter("duplicatesOnly").length() > 0));
						if (duplicatesOnly) {accountSelectorType = 1;}
						boolean detectDuplicates = ((request.getParameter("detectDuplicates") != null) && (request.getParameter("detectDuplicates").length() > 0));
						boolean disabledAccountsOnly = ((request.getParameter("disabledAccountsOnly") != null) && (request.getParameter("disabledAccountsOnly").length() > 0));
						if (disabledAccountsOnly) {accountSelectorType = 1;}
						boolean detectDisabledAccounts = ((request.getParameter("detectDisabledAccounts") != null) && (request.getParameter("detectDisabledAccounts").length() > 0));
						boolean membersOnly = accountSelectorType == 1;
						boolean selectAccount = false;
						boolean selectContact = false;
						boolean selectLegalEntity = false;
						boolean selectGroup = false;
						boolean selectUnspecifiedAccount = false;
						if (membersOnly) {
						    selectAccount = true;
						} else {
						    if (request.getParameter("accountSelector") != null) {
						        selectContact =            request.getParameter("accountSelector").compareTo("selectContact") == 0;
						        selectLegalEntity =        request.getParameter("accountSelector").compareTo("selectLegalEntity") == 0;
						        selectGroup =              request.getParameter("accountSelector").compareTo("selectGroup") == 0;
						        selectUnspecifiedAccount = request.getParameter("accountSelector").compareTo("selectUnspecifiedAccount") == 0;
						    }
						}
						try {
						  pageSize = request.getParameter("pageSize") != null ? Integer.parseInt(request.getParameter("pageSize")) : DEFAULT_PAGE_SIZE;
						} catch (Exception e) {}
						try {
						  highAccount = request.getParameter("highAccount") != null ? Long.parseLong(request.getParameter("highAccount")) : 0;
						} catch (Exception e) {}
						try {
						  if (request.getParameter("paging") != null && request.getParameter("paging").startsWith("--")) {
						    displayStart = ((int)((highAccount - (long)(10*pageSize)) / (long)pageSize)) - 1;
						    if (displayStart < 0) {
						      displayStart = 0;
						    }
						  } else if (request.getParameter("paging") != null && request.getParameter("paging").startsWith("++")) {
						    displayStart = ((int)((highAccount + (long)(10*pageSize)) / (long)pageSize)) - 1;
						    if (displayStart < 0) {
						      displayStart = 0;
						    }
						  } else if (request.getParameter("displayStart") != null && request.getParameter("displayStart").startsWith("+")) {
						    displayStart = ((int)((highAccount + Long.parseLong(request.getParameter("displayStart").substring(1))) / (long)pageSize)) - 1;
						    if (displayStart < 0) {
						      displayStart = 0;
						    }
						  } else {
						    displayStart = request.getParameter("displayStart") != null ? Integer.parseInt(request.getParameter("displayStart")) : 0;
						  }
						} catch (Exception e) {}
						if (isSelectionChange) {
						   highAccount = 0;
						   displayStart = 0;
						   highAccountIsKnown = false;
						}

						if (request.getParameter("Reload.Button") != null) {
						    //System.out.println("reload.button");
						    //app.resetPmData(); // evict pm data, i.e. clear cache
						}

						Iterator accounts = null;
						long counter = 0;
						boolean iteratorNotSet = true;
						int itSetCounter = 0;
						final int MAXITSETCOUNTER = 2;

						while (iteratorNotSet && itSetCounter < MAXITSETCOUNTER) {
						    itSetCounter++;
						    try {
						        if (selectContact) {
						            if (accountSelectorType == 0) {
						                accounts = accountSegment.getAccount(contactFilter).listIterator((int)displayStart*pageSize);
						            } else {
						                accounts = selectedAccountFilterGlobal.getFilteredAccount(contactFilter).listIterator((int)displayStart*pageSize);
						            }
						        } else if (selectLegalEntity) {
						            if (accountSelectorType == 0) {
						                accounts = accountSegment.getAccount(legalEntityFilter).listIterator((int)displayStart*pageSize);
						            } else {
						                accounts = selectedAccountFilterGlobal.getFilteredAccount(legalEntityFilter).listIterator((int)displayStart*pageSize);
						            }
						        } else if (selectGroup) {
						            if (accountSelectorType == 0) {
						                accounts = accountSegment.getAccount(groupFilter).listIterator((int)displayStart*pageSize);
						            } else {
						                accounts = selectedAccountFilterGlobal.getFilteredAccount(groupFilter).listIterator((int)displayStart*pageSize);
						            }
						        } else if (selectUnspecifiedAccount) {
						            if (accountSelectorType == 0) {
						                accounts = accountSegment.getAccount(unspecifiedAccountFilter).listIterator((int)displayStart*pageSize);
						            } else {
						                accounts = selectedAccountFilterGlobal.getFilteredAccount(unspecifiedAccountFilter).listIterator((int)displayStart*pageSize);
						            }
						        } else {
						            selectAccount = true;
						            if (accountSelectorType == 0) {
						                accounts = accountSegment.getAccount(accountFilter).listIterator((int)displayStart*pageSize);
						            } else if (accountSelectorType == 1) {
						                // membersOnly
						                accounts = (accountSource.getMember(memberFilter)).listIterator((int)displayStart*pageSize);
						            } else if (accountSelectorType == 2) {
						                accounts = selectedAccountFilterGlobal.getFilteredAccount(accountFilter).listIterator((int)displayStart*pageSize);
						            } else if (accountSelectorType == 100) {
						        	      // full name
						        	      if (searchString.length() > 0) {
						                    accounts = accountSegment.getAccount(searchAccountFullNameFilter).listIterator((int)displayStart*pageSize);
						                }
						        	  } else if (accountSelectorType == 110) {
						        	  	  // e-mail address
						        	      if (searchString.length() > 0) {
						                    accounts = accountSegment.getAddress(searchEMailAddressFilter).listIterator((int)displayStart*pageSize);
						                }
						        	  } else if (accountSelectorType == 111) {
						        	  	  // postal address city
						        	      if (searchString.length() > 0) {
						                    accounts = accountSegment.getAddress(searchPostalAddressCityFilter).listIterator((int)displayStart*pageSize);
						                }
						        	  } else if (accountSelectorType == 112) {
						        	  	  // postal address zip
						        	      if (searchString.length() > 0) {
						                    accounts = accountSegment.getAddress(searchPostalAddressCodeFilter).listIterator((int)displayStart*pageSize);
						                }
						        	  }
						        }
						        counter = displayStart*pageSize;
						        if (accounts != null && !accounts.hasNext()) {
						            displayStart = (int)((highAccount / (long)pageSize));
						            counter = displayStart*pageSize;
						        } else {
						          iteratorNotSet = false;
						        }
						    } catch (Exception e) {
						        new ServiceException(e).log();
						        displayStart = 0;
						        counter = 0;
						    }
						}

						if (request.getParameter("previousSheet") != null) {
								// delete previous temp file if it exists
								try {
										File previousFile = new File(
											app.getTempFileName(request.getParameter("previousSheet"), "")
										);
										if (previousFile.exists()) {
												previousFile.delete();
												//System.out.println("deleted previous temp file " + request.getParameter("previousSheet"));
										}
								} catch (Exception e){
										new ServiceException(e).log();
								}
						}
						if (request.getParameter("ACTION.create") != null) {
						    //System.out.println("CREATE: " + request.getParameter("ACTION.create"));
						    if (accountSource != null) {
						        try {
						            pm.currentTransaction().begin();
						            org.opencrx.kernel.account1.jmi1.Account accountTarget = (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(new Path((request.getParameter("ACTION.create"))));
						            org.opencrx.kernel.account1.jmi1.Member newMember = pm.newInstance(org.opencrx.kernel.account1.jmi1.Member.class);
						            //newMember.setMemberRole(new short[]{MEMBERROLESALESREGION});
						            newMember.setValidFrom(new java.util.Date());
						            newMember.setAccount(accountTarget);
						            newMember.setQuality((short)5);
						            newMember.setName(accountTarget.getFullName());
						            accountSource.addMember(
						              false,
						              org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
						              newMember
						            );
						            pm.currentTransaction().commit();
						        } catch (Exception e) {
						            errorMsg = "Cannot enable " + app.getLabel(MEMBER_CLASS);
						            new ServiceException(e).log();
						            try {
						                pm.currentTransaction().rollback();
						            } catch (Exception er) {}
						        }
						    } else {
						        errorMsg = "Cannot create " + app.getLabel(MEMBER_CLASS) + " [parent missing]";
						    }
						} else if (request.getParameter("ACTION.enable") != null) {
						    //System.out.println("ENABLE: " + request.getParameter("ACTION.enable"));
						    try {
						        pm.currentTransaction().begin();
						        org.opencrx.kernel.account1.jmi1.Member member = (org.opencrx.kernel.account1.jmi1.Member)pm.getObjectById(new Path((request.getParameter("ACTION.enable"))));
						        member.setDisabled(new Boolean(false));
						        if (member.getValidFrom() == null) {
						            member.setValidFrom(new java.util.Date());
						        }
						        member.setValidTo(null);
						        if ((member.getName() == null || member.getName().length() == 0) && member.getAccount() != null) {
						            member.setName(member.getAccount().getFullName());
						        }
						        pm.currentTransaction().commit();
						    } catch (Exception e) {
						        errorMsg = "Cannot enable " + app.getLabel(MEMBER_CLASS);
						        new ServiceException(e).log();
						        try {
						            pm.currentTransaction().rollback();
						        } catch (Exception er) {}
						    }
						} else if (request.getParameter("ACTION.disable") != null) {
						    //System.out.println("DISABLE: " + request.getParameter("ACTION.disable"));
						    try {
						        pm.currentTransaction().begin();
						        org.opencrx.kernel.account1.jmi1.Member member = (org.opencrx.kernel.account1.jmi1.Member)pm.getObjectById(new Path((request.getParameter("ACTION.disable"))));
						        member.setDisabled(new Boolean(true));
						        member.setValidTo(new java.util.Date());
						        if ((member.getName() == null || member.getName().length() == 0) && member.getAccount() != null) {
						            member.setName(member.getAccount().getFullName());
						        }
						        pm.currentTransaction().commit();
						    } catch (Exception e) {
						        errorMsg = "Cannot disable " + app.getLabel(MEMBER_CLASS);
						        new ServiceException(e).log();
						        try {
						            pm.currentTransaction().rollback();
						        } catch (Exception er) {}
						    }
						} else if (request.getParameter("ACTION.delete") != null) {
						    //System.out.println("DELETE: " + request.getParameter("ACTION.delete"));
						    try {
						        pm.currentTransaction().begin();
						        org.opencrx.kernel.account1.jmi1.Member member = (org.opencrx.kernel.account1.jmi1.Member)pm.getObjectById(new Path((request.getParameter("ACTION.delete"))));
						        ((RefObject_1_0)member).refDelete();
						        pm.currentTransaction().commit();
						    } catch (Exception e) {
						        errorMsg = "Cannot delete " + app.getLabel(MEMBER_CLASS);
						        new ServiceException(e).log();
						        try {
						            pm.currentTransaction().rollback();
						        } catch (Exception er) {}
						    }
						} else if (request.getParameter("ACTION.exportXLS") != null) {
						    //System.out.println("Export_XLS: " + request.getParameter("ACTION.exportXLS"));
						    try {
						      	f = new File(
						      		app.getTempFileName(location, "")
						      	);
						      	os = new FileOutputStream(f);
						      	wb = new HSSFWorkbook();

						        // Header Style (black background, orange/bold font)
						        headerfont = wb.createFont();
						        headerfont.setFontHeightInPoints((short)10);
						        headerfont.setFontName("Tahoma");
						        headerfont.setBold(true);
						        headerfont.setColor(IndexedColors.ORANGE.getIndex());
						        // Fonts are set into a style so create a new one to use.
						        headerStyle = wb.createCellStyle();
						        headerStyle.setFillForegroundColor(IndexedColors.BLACK.getIndex());
						        headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
						        headerStyle.setFont(headerfont);

						        // Wrapped Style
						        wrappedStyle = wb.createCellStyle();
						        wrappedStyle.setVerticalAlignment(VerticalAlignment.TOP);
						        wrappedStyle.setWrapText(true);

						        // TopAligned Style
						        topAlignedStyle = wb.createCellStyle();
						        topAlignedStyle.setVerticalAlignment(VerticalAlignment.TOP);

						        downloadAction =	new Action(
						            Action.EVENT_DOWNLOAD_FROM_LOCATION,
						            new Action.Parameter[]{
						                new Action.Parameter(Action.PARAMETER_LOCATION, location),
						                new Action.Parameter(Action.PARAMETER_NAME, sheetName + ".xls"),
						                new Action.Parameter(Action.PARAMETER_MIME_TYPE, "application/vnd.ms-excel")
						            },
						            app.getTexts().getClickToDownloadText() + " " + sheetName,
						            true
						        );
										sheetAccounts = addSheet(wb, "Accounts", true, headerStyle);
						    } catch (Exception e) {
						        new ServiceException(e).log();
						    }
						}
%>
				    <table class="fieldGroup" style="width:100%;margin-top:0;padding-top:0;border-top:0;border-collapse:collapse;">
				      <tr>
				        <td id="submitButtons" style="font-weight:bold;background-color:#E4E4E4;padding-bottom:3px;">
				          <div style="padding:8px 3px;">
				            <%= app.getTexts().getSelectAllText() %> <select <%= mode.compareTo("0")==0 ? "disabled" : "" %> id="accountSelectorType" name="accountSelectorType" onchange="javascript:$('waitMsg').style.visibility='visible';$('submitButtons').style.visibility='hidden';$('isSelectionChange').checked=true;$('Reload.Button').click();" >
<%
							if (mode.compareTo("0") == 0) {
%>
								<option <%= accountSelectorType == 1 ? "selected" : "" %> value="1"><%= app.getLabel(MEMBER_CLASS)  %>&nbsp;</option>
<%
							} else {
%>
								<option <%= accountSelectorType == 100 ? "selected" : "" %> value="100">? <%= app.getTexts().getSearchText() %> <%= userView.getFieldLabel("org:opencrx:kernel:account1:LegalEntity", "name", app.getCurrentLocaleAsIndex()) %>&nbsp;</option>
								<option <%= accountSelectorType == 110 ? "selected" : "" %> value="110">? <%= app.getTexts().getSearchText() %> <%= app.getLabel(EMAILADDRESS_CLASS) %>&nbsp;</option>
								<option <%= accountSelectorType == 111 ? "selected" : "" %> value="111">? <%= app.getTexts().getSearchText() %> <%= app.getLabel(POSTALADDRESS_CLASS) %> <%= userView.getFieldLabel("org:opencrx:kernel:address1:PostalAddressable", "postalCity", app.getCurrentLocaleAsIndex()) %>&nbsp;</option>
								<option <%= accountSelectorType == 112 ? "selected" : "" %> value="112">? <%= app.getTexts().getSearchText() %> <%= app.getLabel(POSTALADDRESS_CLASS) %> <%= userView.getFieldLabel("org:opencrx:kernel:address1:PostalAddressable", "postalCode", app.getCurrentLocaleAsIndex()) %>&nbsp;</option>
								<option <%= accountSelectorType ==   0 ? "selected" : "" %> value="0"  >* <%= app.getLabel(ACCOUNTSEGMENT_CLASS) %>&nbsp;</option>
<%
								org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery accountFilterGlobalQuery = (org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.AccountFilterGlobal.class);
								accountFilterGlobalQuery.forAllDisabled().isFalse();
								accountFilterGlobalQuery.orderByName().ascending();
								for(Iterator i = accountSegment.getAccountFilter(accountFilterGlobalQuery).iterator(); i.hasNext(); ) {
									org.opencrx.kernel.account1.jmi1.AccountFilterGlobal accountFilterGlobal = (org.opencrx.kernel.account1.jmi1.AccountFilterGlobal)i.next();
%>
									<option <%= (accountSelectorType == 2) && (accountFilterXri != null && accountFilterXri.compareTo(accountFilterGlobal.refMofId()) == 0) ? "selected" : "" %> value="<%= ACCOUNT_FILTER_XRI_PREFIX %><%= accountFilterGlobal.refMofId() %>"><%= app.getLabel(ACCOUNTFILTERGLOBAL_CLASS) %>: <%= accountFilterGlobal.getName() != null ? accountFilterGlobal.getName() : "?" %> &nbsp;</option>
<%
								}
							}
%>
		                	</select>&nbsp;
<%
			                if (accountSelectorType >= 100) {
%>
				              <input type="text" name="searchString" id="searchString" tabindex="<%= tabIndex++ %>" value="<%= searchString %>" />
				              <inpput type="hidden" name="previousSearchString" id="previousSearchString" tabindex="<%= tabIndex++ %>" value="<%= searchString %>" />
							  <input type="submit" name="go" id="go" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" title="<%= app.getTexts().getSearchText() %>" tabindex="<%= tabIndex++ %>" value=">>" onclick="setTimeout('disableSubmit()', 10);$('Reload.Button').click();" />
<%
				            } else {
								if (!membersOnly) {
%>
									&nbsp;&nbsp;<input type="radio" name="accountSelector" <%= selectAccount            ? "checked" : "" %> value="selectAccount"            onchange="javascript:setTimeout('disableSubmit()', 10);$('isSelectionChange').checked=true;$('Reload.Button').click();" /> *
									&nbsp;&nbsp;<input type="radio" name="accountSelector" <%= selectContact            ? "checked" : "" %> value="selectContact"            onchange="javascript:setTimeout('disableSubmit()', 10);$('isSelectionChange').checked=true;$('Reload.Button').click();" /> <%= app.getLabel(CONTACT_CLASS) %>
									&nbsp;&nbsp;<input type="radio" name="accountSelector" <%= selectLegalEntity        ? "checked" : "" %> value="selectLegalEntity"        onchange="javascript:setTimeout('disableSubmit()', 10);$('isSelectionChange').checked=true;$('Reload.Button').click();" /> <%= app.getLabel(LEGALENTITY_CLASS) %>
									&nbsp;&nbsp;<input type="radio" name="accountSelector" <%= selectGroup              ? "checked" : "" %> value="selectGroup"              onchange="javascript:setTimeout('disableSubmit()', 10);$('isSelectionChange').checked=true;$('Reload.Button').click();" /> <%= app.getLabel(GROUP_CLASS) %>
									&nbsp;&nbsp;<input type="radio" name="accountSelector" <%= selectUnspecifiedAccount ? "checked" : "" %> value="selectUnspecifiedAccount" onchange="javascript:setTimeout('disableSubmit()', 10);$('isSelectionChange').checked=true;$('Reload.Button').click();" /> <%= app.getLabel(UNSPECIFIEDACCOUNT_CLASS) %>
<%
								}
							}
%>
				          </div>
							<table style="display:inline;">
								<tr>
									<td>
										<input type="Submit" id="Reload.Button" name="Reload.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getReloadText() %>" onmouseup="javascript:setTimeout('disableSubmit()', 10);" />
										<input type="Button" name="Print.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="Print" onClick="javascript:window.print();return false;" />
										<input type="Submit" name="ACTION.exportXLS" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="Export" onmouseup="javascript:setTimeout('disableSubmit()', 10);" />
<%
										if (downloadAction != null) {
%>
											<span>
												<a href="<%= request.getContextPath() %>/<%= downloadAction.getEncodedHRef(requestId) %>"><%= SPREADSHEET %></a>&nbsp;
											</span>
<%
										}
%>
										<input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getCloseText() %>" onClick="javascript:window.close();" />
									</td>
									<td>
										<input type="checkbox" name="detectDuplicates" id="detectDuplicates" <%= detectDuplicates ? "checked" : "" %> /> Detect Duplicates
									</td>
									<td>
										<input type="checkbox" name="duplicatesOnly" id="duplicatesOnly" <%= duplicatesOnly ? "checked" : "" %> /> Duplicates Only
									</td>
								</tr>
								<tr>
									<td>
							          <a href="#" onclick="javascript:try{$('paging').value='--';}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/previous_fast.gif" style="padding-top:5px;"></a>
							          <a href="#" onclick="javascript:try{($('displayStart').value)--;}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/previous.gif" style="padding-top:5px;"></a>
							          <span id="displayStartSelector">...</span>
							          <a href="#" onclick="javascript:try{($('displayStart').value)++;}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/next.gif" style="padding-top:5px;"></a>
							          <a href="#" onclick="javascript:try{$('paging').value='++';}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/next_fast.gif" style="padding-top:5px;"></a>
							          &nbsp;&nbsp;&nbsp;
							          <select id="pageSize" name="pageSize" style="text-align:right;" onchange="javascript:$('waitMsg').style.visibility='visible';$('submitButtons').style.visibility='hidden';$('isSelectionChange').checked=true;$('Reload.Button').click();" >
							            <option <%= pageSize ==  10 ? "selected" : "" %> value="10">10&nbsp;</option>
							            <option <%= pageSize ==  20 ? "selected" : "" %> value="20">20&nbsp;</option>
							            <option <%= pageSize ==  50 ? "selected" : "" %> value="50">50&nbsp;</option>
							            <option <%= pageSize == 100 ? "selected" : "" %> value="100">100&nbsp;</option>
							            <option <%= pageSize == 500 ? "selected" : "" %> value="500">500&nbsp;</option>
							          </select>
									</td>
									<td>
										<input type="checkbox" name="detectDisabledAccounts" id="detectDisabledAccounts" <%= detectDisabledAccounts ? "checked" : "" %> /> Detect active Members with disabled Accounts
									</td>
									<td>
										<input type="checkbox" name="disabledAccountsOnly" id="disabledAccountsOnly" <%= disabledAccountsOnly ? "checked" : "" %> /> Only active Members with disabled Accounts
									</td>
								</tr>
							</table>
				        </td>
				        <td id="waitMsg" style="display:none;">
				          <div style="padding-left:5px; padding: 11px 0px 50px 0px;">
				            <img src="../../images/wait.gif" alt="" />
				          </div>
				        </td>
				      </tr>
				    </table>
<%
						if (errorMsg.length() > 0) {
%>
							<div style="background-color:red;color:white;border:1px solid black;padding:10px;font-weight:bold;margin-top:10px;">
								<%= errorMsg %>
							</div>
<%
						}
%>
				</div> <!-- topnavi -->
      </div> <!-- header -->

	    <div id="content-wrap">
	    	<div id="content" style="padding:13.5em 0.5em 0px 0.5em;">

        <table style="background:white;"><tr><td>
        <table id="resultTable" class="gridTableFull">
          <tr class="gridTableHeaderFull"><!-- 10 columns -->
            <td align="right"><a href="#" onclick="javascript:try{$('paging').value='--';}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/previous_fast.gif"></a>
              <a href="#" onclick="javascript:try{($('displayStart').value)--;}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/previous.gif"></a>
              #
              <a href="#" onclick="javascript:try{($('displayStart').value)++;}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/next.gif"></a>
              <a href="#" onclick="javascript:try{$('paging').value='++';}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/next_fast.gif"></a></td>
            <td align="left">&nbsp;<b><%= app.getLabel(ACCOUNT_CLASS) %></b></td>
            <td align="left">&nbsp;<b><%= app.getLabel(EMAILADDRESS_CLASS) %></b></td>
            <td align="left">&nbsp;<b><%= app.getLabel(POSTALADDRESS_CLASS) %></b></td>
            <td align="left" nowrap>
               <!-- <img src="../../images/NumberReplacement.gif" alt="" align="top" /> -->
               <INPUT type="submit" name="addvisible"     id="addvisible"     title="add/enable visible" tabindex="<%= tabIndex++ %>" value="+"       onclick="javascript:$('executemulti').style.visibility='visible';$('executemulti').name=this.name;$('disablevisible').style.display='none';$('deletevisible').style.display='none';return false;" onmouseup="this.style.border='3px solid red';" style="font-size:10px;font-weight:bold;" />
               <INPUT type="submit" name="disablevisible" id="disablevisible" title="disable visible"    tabindex="<%= tabIndex++ %>" value="&ndash;" onclick="javascript:$('executemulti').style.visibility='visible';$('executemulti').name=this.name;$('addvisible').style.display='none';$('deletevisible').style.display='none';return false;"     onmouseup="this.style.border='3px solid red';" style="font-size:10px;font-weight:bold;" />
               <INPUT type="submit" name="deletevisible"  id="deletevisible"  title="delete visible"     tabindex="<%= tabIndex++ %>" value="X"       onclick="javascript:$('executemulti').style.visibility='visible';$('executemulti').name=this.name;$('addvisible').style.display='none';$('disablevisible').style.display='none';return false;"    onmouseup="this.style.border='3px solid red';" style="font-size:10px;font-weight:bold;" />
               <INPUT type="submit" name="executemulti"   id="executemulti"   title="<%= app.getTexts().getOkTitle() %>" style="visibility:hidden;" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getOkTitle() %>" onmouseup="javascript:setTimeout('disableSubmit()', 10);$('addvisible').style.display='none';$('disablevisible').style.display='none';$('deletevisible').style.display='none';this.style.display='none';this.name='ACTION.'+this.name;" style="font-size:10px;font-weight:bold;" /><br>
               <b><%= app.getLabel(MEMBERSHIP_CLASS) %></b>
            </td>
            <td align="center">&nbsp;<b><%= userView.getFieldLabel(MEMBER_CLASS, "memberRole", app.getCurrentLocaleAsIndex()) %></b></td>
            <td align="center">&nbsp;<b><%= userView.getFieldLabel(MEMBER_CLASS, "validFrom", app.getCurrentLocaleAsIndex()) %></b></td>
            <td align="center">&nbsp;<b><%= userView.getFieldLabel(MEMBER_CLASS, "validTo", app.getCurrentLocaleAsIndex()) %></b></td>
            <td align="center"><b><%= userView.getFieldLabel(MEMBER_CLASS, "name", app.getCurrentLocaleAsIndex()) != null ? userView.getFieldLabel(MEMBER_CLASS, "name", app.getCurrentLocaleAsIndex()) : "" %></b></td>
            <td align="center"><b><%= userView.getFieldLabel(MEMBER_CLASS, "description", app.getCurrentLocaleAsIndex()) %></b></td>
          </tr>
<%
					Set<String> processedMembers = new TreeSet<String>();
          if (accounts != null) {
              short spreadSheetRow = 1;
              for(
                Iterator i = accounts;
                i.hasNext() && (counter <= (displayStart+1)*pageSize);
              ) {
		              org.opencrx.kernel.account1.jmi1.PostalAddress infoAddr = null;
		              org.opencrx.kernel.account1.jmi1.Account account = null;
		              org.opencrx.kernel.generic.jmi1.CrxObject crxObject = (org.opencrx.kernel.generic.jmi1.CrxObject)i.next();
		              try {
				              if (crxObject instanceof org.opencrx.kernel.account1.jmi1.Account) {
				                  account = (org.opencrx.kernel.account1.jmi1.Account)crxObject;
				              } else if (crxObject instanceof org.opencrx.kernel.account1.jmi1.EMailAddress) {
				              	  // get parent account
				                  account = (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(new Path(crxObject.refMofId()).getParent().getParent());
				              } else if (crxObject instanceof org.opencrx.kernel.account1.jmi1.PostalAddress) {
				              	  // get parent account
				                  account = (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(new Path(crxObject.refMofId()).getParent().getParent());
				                  infoAddr = (org.opencrx.kernel.account1.jmi1.PostalAddress)crxObject;
				              } else {
				              		try {
				                  	account = ((org.opencrx.kernel.account1.jmi1.Member)crxObject).getAccount();
				                  } catch (Exception e) {
				                  	SysLog.log(Level.WARNING, "ManageMembers {0} - getAccount no permission", crxObject.refGetPath());
				                  }
				              }
				          } catch (Exception e) {
				              new ServiceException(e).log();
				          }

                  if (account == null) {
                      continue;
                  }

                  String accountHref = "";
                  Action action = new ObjectReference(
                      account,
                      app
                  ).getSelectObjectAction();
                  accountHref = "../../" + action.getEncodedHRef();

                  int memberCounter = 0;
                  String memberHref = "";
                  org.opencrx.kernel.account1.jmi1.Member member = null;
                  org.opencrx.kernel.account1.cci2.MemberQuery isMemberFilter = (org.opencrx.kernel.account1.cci2.MemberQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Member.class);
                  isMemberFilter.thereExistsAccount().equalTo(account);
                  boolean hasProcessedMember = false;
                  for(Iterator m = accountSource.getMember(isMemberFilter).iterator(); m.hasNext(); ) {
                      try {
                          memberCounter++;
                          org.opencrx.kernel.account1.jmi1.Member currentMember = (org.opencrx.kernel.account1.jmi1.Member)m.next();
                          if (!hasProcessedMember && !processedMembers.contains(currentMember.getIdentity()) && (member == null || currentMember.isDisabled() == null || !currentMember.isDisabled().booleanValue())) {
                              processedMembers.add(currentMember.getIdentity());
                              hasProcessedMember = true;
                              member = currentMember;
                              action = new ObjectReference(
                                  currentMember,
                                  app
                              ).getSelectObjectAction();
                              memberHref = "../../" + action.getEncodedHRef();
                          }
                      } catch (Exception e) {}
                  }
                  if (duplicatesOnly && memberCounter <= 1) {
                      continue;
                  }

                  if (!i.hasNext()) {
                      highAccountIsKnown = true;
                      highAccount = counter+1;
                  }
                  counter++;
                  if (counter < displayStart*pageSize || counter > (displayStart+1)*pageSize) {
                    continue;
                  }

                  if (downloadAction != null && sheetAccounts != null) {
                      spreadSheetRow = addAccount(sheetAccounts, account, spreadSheetRow, wrappedStyle, topAlignedStyle, exceldate, app, codes);
                      //spreadSheetRow++;
                  }

                  String image = "Contact.gif";
                  String label = app.getLabel("org:opencrx:kernel:account1:Contact");
                  if (account instanceof org.opencrx.kernel.account1.jmi1.UnspecifiedAccount) {
                    image = "UnspecifiedAccount.gif";
                    label = app.getLabel("org:opencrx:kernel:account1:UnspecifiedAccount");
                  }
                  if (account instanceof org.opencrx.kernel.account1.jmi1.LegalEntity) {
                    image = "LegalEntity.gif";
                    label = app.getLabel("org:opencrx:kernel:account1:LegalEntity");
                  }
                  if (account instanceof org.opencrx.kernel.account1.jmi1.Group) {
                    image = "Group.gif";
                    label = app.getLabel("org:opencrx:kernel:account1:Group");
                  }

                  // get all (not disabled) EMailAddresses of this account
                  SortedSet<String> eMailAddresses = new TreeSet<String>();
                  org.opencrx.kernel.account1.cci2.EMailAddressQuery eMailAddressFilter = (org.opencrx.kernel.account1.cci2.EMailAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.EMailAddress.class);
                  eMailAddressFilter.forAllDisabled().isFalse();
                  for (
                    Iterator a = account.getAddress(eMailAddressFilter).iterator();
                    a.hasNext();
                  ) {
                      org.opencrx.kernel.account1.jmi1.EMailAddress addr = (org.opencrx.kernel.account1.jmi1.EMailAddress)a.next();
                      if (addr.getEmailAddress() != null && addr.getEmailAddress().length() > 0) {
                          // add this address to list
                          eMailAddresses.add(addr.getEmailAddress());
                      }
                  }

                  // get Postaladdresses of this account (business if available, otherwise private, otherwise any postal address
                  org.opencrx.kernel.account1.jmi1.PostalAddress businessAddr = null;
                  org.opencrx.kernel.account1.jmi1.PostalAddress homeAddr = null;
                  org.opencrx.kernel.account1.jmi1.PostalAddress otherAddr = null;
                  if (infoAddr == null) {
		                  org.opencrx.kernel.account1.cci2.PostalAddressQuery addressFilter = (org.opencrx.kernel.account1.cci2.PostalAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.PostalAddress.class);
		                  addressFilter.forAllDisabled().isFalse();
		                  for (
		                    Iterator a = account.getAddress(addressFilter).iterator();
		                    a.hasNext();
		                  ) {
		                    org.opencrx.kernel.account1.jmi1.PostalAddress addr = (org.opencrx.kernel.account1.jmi1.PostalAddress)a.next();
		                    for(
		                        Iterator k = addr.getUsage().iterator();
		                        k.hasNext();
		                    ) {
		                        switch (((Number)k.next()).intValue()) {
		                            case  400:  homeAddr = addr; break;
		                            case  500:  businessAddr = addr; break;
		                            default  :  otherAddr = addr; break;
		                        }
		                    }
		                  }
		                  if (businessAddr != null) {
		                    infoAddr = businessAddr;
		                  } else if (homeAddr != null) {
		                    infoAddr = homeAddr;
		                  } else if (otherAddr != null) {
		                    infoAddr = otherAddr;
		                  }
		              }
                  String addressInfo = "";
                  if (infoAddr != null) {
                    if (infoAddr.getPostalCode() != null) {addressInfo += infoAddr.getPostalCode() + " ";}
                    if (infoAddr.getPostalCity() != null) {addressInfo += infoAddr.getPostalCity();}
                  }

                  if (member == null && request.getParameter("ACTION.addvisible") != null) {
                      // create new member
                      if (accountSource != null) {
                          try {
                              pm.currentTransaction().begin();
                              org.opencrx.kernel.account1.jmi1.Account accountTarget = account;
                              org.opencrx.kernel.account1.jmi1.Member newMember = pm.newInstance(org.opencrx.kernel.account1.jmi1.Member.class);
                              //newMember.setMemberRole(new short[]{0});
                              newMember.setValidFrom(new java.util.Date());
                              newMember.setAccount(accountTarget);
                              newMember.setQuality((short)5);
                              newMember.setName(accountTarget.getFullName());
                              accountSource.addMember(
                                false,
                                org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
                                newMember
                              );
                              pm.currentTransaction().commit();
                              member = newMember;
                          } catch (Exception e) {
                              errorMsg = "Cannot create " + app.getLabel(MEMBER_CLASS);
                              new ServiceException(e).log();
                              try {
                                  pm.currentTransaction().rollback();
                              } catch (Exception er) {}
                          }
                      } else {
                          errorMsg = "Cannot create " + app.getLabel(MEMBER_CLASS) + " [parent missing]";
                      }
                  }
                  if (member != null && member.isDisabled() != null && member.isDisabled().booleanValue() && request.getParameter("ACTION.addvisible") != null) {
                      // enable existing member
                      try {
                          pm.currentTransaction().begin();
                          member.setDisabled(new Boolean(false));
                          if (member.getValidFrom() == null) {
                              member.setValidFrom(new java.util.Date());
                          }
                          member.setValidTo(null);
                          if ((member.getName() == null || member.getName().length() == 0) && member.getAccount() != null) {
                              member.setName(member.getAccount().getFullName());
                          }
                          pm.currentTransaction().commit();
                      } catch (Exception e) {
                          errorMsg = "Cannot enable " + app.getLabel(MEMBER_CLASS);
                          new ServiceException(e).log();
                          try {
                              pm.currentTransaction().rollback();
                          } catch (Exception er) {}
                      }
                  }
                  if (member != null && request.getParameter("ACTION.disablevisible") != null) {
                      try {
                          pm.currentTransaction().begin();
                          member.setDisabled(new Boolean(true));
                          member.setValidTo(new java.util.Date());
                          if ((member.getName() == null || member.getName().length() == 0) && member.getAccount() != null) {
                              member.setName(member.getAccount().getFullName());
                          }
                          pm.currentTransaction().commit();
                      } catch (Exception e) {
                          errorMsg = "Cannot disable " + app.getLabel(MEMBER_CLASS);
                          new ServiceException(e).log();
                          try {
                              pm.currentTransaction().rollback();
                          } catch (Exception er) {}
                      }
                  }
                  if (member != null && (request.getParameter("ACTION.deletevisible") != null || request.getParameter("ACTION.deletevisible") != null)) {
                      try {
                          pm.currentTransaction().begin();
                          ((RefObject_1_0)member).refDelete();
                          pm.currentTransaction().commit();
                          member = null;
                      } catch (Exception e) {
                          errorMsg = "Cannot delete " + app.getLabel(MEMBER_CLASS);
                          new ServiceException(e).log();
                          try {
                              pm.currentTransaction().rollback();
                          } catch (Exception er) {}
                      }
                  }

                  boolean isMember = member != null;
                  boolean isDisabled = member != null && member.isDisabled() != null && member.isDisabled().booleanValue();

				  if (disabledAccountsOnly && !((isMember && !isDisabled && account.isDisabled() != null && account.isDisabled().booleanValue()))) {
                      continue;
                  }

				  String memberRoles = "";
                  if (member != null) {
                      for(
                          Iterator r = member.getMemberRole().iterator();
                          r.hasNext();
                      ) {
                          memberRoles += (codes.getLongTextByCode("memberRole", app.getCurrentLocaleAsIndex(), true).get(new Short((Short)r.next()))) + "<br />";
                      }
                      if ((member.getName() == null || member.getName().length() == 0) && member.getAccount() != null) {
                          // set member.name (a mandatory attribute)
                          try {
                              pm.currentTransaction().begin();
                              member.setName(member.getAccount().getFullName());
                              pm.currentTransaction().commit();
                          } catch (Exception e) {
                              new ServiceException(e).log();
                              try {
                                  pm.currentTransaction().rollback();
                              } catch (Exception er) {}
                          }
                      }

                  }
                  boolean markAsMemberWithDisabledAccount = isMember && !isDisabled && detectDisabledAccounts && account.isDisabled() != null && account.isDisabled().booleanValue();
%>
                  <tr class="gridTableRowFull" style="<%= isMember ? "background-color:" + (isDisabled ? colorMemberDisabled + ";font-style:italic" : (markAsMemberWithDisabledAccount ? "color:white;background-color:" + colorDuplicate + ";" : colorMember + ";font-weight:bold")) : "" %>;"><!-- 8 columns -->
                    <td align="right"><%= isMember ? "<a href='" + memberHref + "' target='_blank'>" : "" %><%= formatter.format(counter) %><%= isMember ? "</a>" : "" %></td>
                    <td align="left" title="<%= label %>">
<%
						if (markAsMemberWithDisabledAccount) {
%>
						  <img title="account is disabled" src="../../images/filter_disabled.gif" />
<%							
						}
%>
                    	<a href="<%= accountHref %>" target="_blank"><img src="../../images/<%= image %>" border="0" align="top" alt="o" />&nbsp;<%= (new ObjectReference(account, app)).getTitle() %></a>
                    </td>
                    <td align="left" title="<%= label %>">
                      <a href="<%= accountHref %>" target="_blank">
<%
                      for (
                        Iterator e = eMailAddresses.iterator();
                        e.hasNext();
                      ) {
                          String email = (String)e.next();
%>
                          <%= email %><br>
<%
                      }
%>
                      </a>
                    </td>
                    <td align="left" title="<%= label %>"><a href="<%= accountHref %>" target="_blank"><%= addressInfo %></a></td>
<%
                    if (isMember && !isDisabled) {
                      // is enabled member
%>
                      <td align="left" title="<%= app.getLabel(MEMBER_CLASS) %>">
												<button type="submit" name="disable" tabindex="<%= tabIndex++ %>" value="&mdash;" onmouseup="javascript:setTimeout('disableSubmit()', 10);this.name='ACTION.'+this.name;this.value='<%= member.refMofId() %>';" style="border:0; background:transparent;font-size:10px;font-weight:bold;" ><img src="../../images/checked.gif" /></button>&nbsp;&nbsp;
												<button type="submit" name="delete" tabindex="<%= tabIndex++ %>" value="X" title="<%= app.getTexts().getDeleteTitle() %>" onmouseup="javascript:setTimeout('disableSubmit()', 10);this.name='ACTION.'+this.name;this.value='<%= member.refMofId() %>';" style="border:0; background:transparent;font-size:10px;font-weight:bold;" ><img src="../../images/delete.gif" /></button>
                      </td>
                      <td align="left" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= memberRoles %></td>
                      <td align="center" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getValidFrom() != null ? timeFormat.format(member.getValidFrom()) : "--" %></td>
                      <td align="center" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getValidTo()   != null ? timeFormat.format(member.getValidTo())   : "--" %></td>
                      <td align="left" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getName() != null ? member.getName() : "" %></td>
                      <td align="left" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getDescription() != null ? member.getDescription() : "" %></td>
<%
                    } else if (isMember && isDisabled) {
                      // is disabled member
%>
                      <td align="left" title="<%= app.getLabel(MEMBER_CLASS) %> (<%= userView.getFieldLabel(MEMBER_CLASS, "disabled", app.getCurrentLocaleAsIndex()) %>)">
												<button type="submit" name="enable" tabindex="<%= tabIndex++ %>" value="*" onmouseup="javascript:setTimeout('disableSubmit()', 10);this.name='ACTION.'+this.name;this.value='<%= member.refMofId() %>';" style="border:0; background:transparent;font-size:10px;font-weight:bold;" ><img src="../../images/ifneedbe.gif" /></button> (<%= userView.getFieldLabel(MEMBER_CLASS, "disabled", app.getCurrentLocaleAsIndex()) %>)&nbsp;&nbsp;
												<button type="submit" name="delete" tabindex="<%= tabIndex++ %>" value="X" title="<%= app.getTexts().getDeleteTitle() %>" onmouseup="javascript:setTimeout('disableSubmit()', 10);this.name='ACTION.'+this.name;this.value='<%= member.refMofId() %>';" style="border:0; background:transparent;font-size:10px;font-weight:bold;" ><img src="../../images/delete.gif" /></button>
                      </td>
                      <td align="left" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= memberRoles %></td>
                      <td align="center" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getValidFrom() != null ? timeFormat.format(member.getValidFrom()) : "--" %></td>
                      <td align="center" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getValidTo()   != null ? timeFormat.format(member.getValidTo())   : "--" %></td>
                      <td align="left" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getName() != null ? member.getName() : "" %></td>
                      <td align="left" <%= isMember ? "onclick='javascript:window.open(\"" + memberHref + "\");'" : "" %>><%= member.getDescription() != null ? member.getDescription() : "" %></td>
<%
                    } else {
                      // not a member
%>
                      <td align="left"><button type="submit" name="create" tabindex="<%= tabIndex++ %>" value="+" onmouseup="javascript:setTimeout('disableSubmit()', 10);this.name='ACTION.'+this.name;this.value='<%= account.refMofId() %>';" style="border:0; background:transparent;font-size:10px;font-weight:bold;" ><img src="../../images/notchecked.gif" /></button></td>
                      <td align="left"></td>
                      <td colspan="4">&nbsp;</td>
<%
                    }
%>
                  </tr>
<%
                  if (detectDuplicates && memberCounter > 1) {
                      //===================================================================
                      // deal with duplicates (members that reference to the same account)
                      //===================================================================
                      for(Iterator m = accountSource.getMember(isMemberFilter).iterator(); m.hasNext(); ) {
                          String currentMemberHref = "";
                          org.opencrx.kernel.account1.jmi1.Member currentMember = null;
                          try {
                              currentMember = (org.opencrx.kernel.account1.jmi1.Member)m.next();
                              action = new ObjectReference(
                                  currentMember,
                                  app
                              ).getSelectObjectAction();
                              currentMemberHref = "../../" + action.getEncodedHRef();
                          } catch (Exception e) {new ServiceException(e).log();}
                          boolean isEnabled = currentMember != null && (currentMember.isDisabled() == null || !currentMember.isDisabled().booleanValue());
                          if (currentMember.refMofId().compareTo(member.refMofId()) != 0) {
                            String currentMemberRoles = "";
                            for(
                                Iterator r = currentMember.getMemberRole().iterator();
                                r.hasNext();
                            ) {
                                currentMemberRoles += (codes.getLongTextByCode("memberRole", app.getCurrentLocaleAsIndex(), true).get(new Short((Short)r.next()))) + "<br />";
                            }
%>
                            <tr class="gridTableRowFull" style="<%= isEnabled ? "color:white;background-color:" + colorDuplicate + ";" : "font-style:italic;" %>"><!-- 6 columns -->
                              <td align="right" title="<%= app.getLabel(MEMBER_CLASS) %> referencing same <%= app.getLabel(ACCOUNT_CLASS) %>" onclick='javascript:window.open(\"" + currentMemberHref + "\");'><a href="<%= currentMemberHref %>" target="_blank"><img src="../../images/downRed3T.gif" border="0" align="top" alt="o" /></a></td>
                              <td align="left" title="<%= app.getLabel(MEMBER_CLASS) %> referencing same <%= app.getLabel(ACCOUNT_CLASS) %>"><a href="<%= currentMemberHref %>" target="_blank"><%= CAUTION %><img src="../../images/<%= image %>" border="0" align="top" alt="o" />&nbsp;<%= (new ObjectReference(account, app)).getTitle() %></a></td>
                              <td align="left" title="<%= app.getLabel(MEMBER_CLASS) %> referencing same <%= app.getLabel(ACCOUNT_CLASS) %>">&nbsp;</td>
                              <td align="left" title="<%= app.getLabel(MEMBER_CLASS) %> referencing same <%= app.getLabel(ACCOUNT_CLASS) %>">&nbsp;</td>
                              <td align="left" title="<%= app.getLabel(MEMBER_CLASS) %> referencing same <%= app.getLabel(ACCOUNT_CLASS) %>">
<%
                              if (isEnabled) {
%>
																	<button type="submit" name="disable" tabindex="<%= tabIndex++ %>" value="&mdash;" onmouseup="javascript:setTimeout('disableSubmit()', 10);this.name='ACTION.'+this.name;this.value='<%= currentMember.refMofId() %>';" style="border:0; background:transparent;font-size:10px;font-weight:bold;" ><img src="../../images/checked.gif" /></button>
<%
                              } else {
%>
																	<button type="submit" name="enable" tabindex="<%= tabIndex++ %>" value="*" onmouseup="javascript:setTimeout('disableSubmit()', 10);this.name='ACTION.'+this.name;this.value='<%= currentMember.refMofId() %>';" style="border:0; background:transparent;font-size:10px;font-weight:bold;" ><img src="../../images/ifneedbe.gif" /></button>
                                  (<%= userView.getFieldLabel(MEMBER_CLASS, "disabled", app.getCurrentLocaleAsIndex()) %>)
<%
                              }
%>
                              </td>
                              <td align="left" onclick='javascript:window.open(\"" + currentMemberHref + "\");'><%= currentMemberRoles %></td>
                              <td align="center" onclick='javascript:window.open(\"" + currentMemberHref + "\");'><%= currentMember.getValidFrom() != null ? timeFormat.format(currentMember.getValidFrom()) : "--" %></td>
                              <td align="center" onclick='javascript:window.open(\"" + currentMemberHref + "\");'><%= currentMember.getValidTo()   != null ? timeFormat.format(currentMember.getValidTo())   : "--" %></td>
                              <td align="left" onclick='javascript:window.open(\"" + currentMemberHref + "\");'><%= currentMember.getName() != null ? currentMember.getName() : "" %></td>
                              <td align="left" onclick='javascript:window.open(\"" + currentMemberHref + "\");'><%= currentMember.getDescription() != null ? currentMember.getDescription() : "" %></td>
                            </tr>
<%
                          }
                      }
                  }

                  //===================================================================
                  // try to detect duplicates based on e-mail addresses
                  //===================================================================
                  //org.opencrx.kernel.account1.cci2.EMailAddressQuery eMailAddressFilter = accountPkg.createEMailAddressQuery();
                  //eMailAddressFilter.forAllDisabled().isFalse();
                  if (detectDuplicates) {
                      eMailAddressFilter.thereExistsEmailAddress().elementOf(new ArrayList(eMailAddresses));
                      for (
                        Iterator a = accountSegment.getAddress(eMailAddressFilter).iterator();
                        a.hasNext();
                      ) {
                          org.opencrx.kernel.account1.jmi1.EMailAddress addr = (org.opencrx.kernel.account1.jmi1.EMailAddress)a.next();
                          if (addr.getEmailAddress() != null && addr.getEmailAddress().length() > 0) {
                              // get parent account
                              org.opencrx.kernel.account1.jmi1.Account parentAccount = (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(new Path(addr.refMofId()).getParent().getParent());
                              if (parentAccount.refMofId().compareTo(account.refMofId()) == 0) {
                                  continue; // found e-mail address of current account
                              }

                              String parentAccountHref = "";
                              action = new ObjectReference(
                                  parentAccount,
                                  app
                              ).getSelectObjectAction();
                              parentAccountHref = "../../" + action.getEncodedHRef();

                              image = "Contact.gif";
                              label = app.getLabel("org:opencrx:kernel:account1:Contact");
                              if (parentAccount instanceof org.opencrx.kernel.account1.jmi1.UnspecifiedAccount) {
                                image = "UnspecifiedAccount.gif";
                                label = app.getLabel("org:opencrx:kernel:account1:UnspecifiedAccount");
                              }
                              if (parentAccount instanceof org.opencrx.kernel.account1.jmi1.LegalEntity) {
                                image = "LegalEntity.gif";
                                label = app.getLabel("org:opencrx:kernel:account1:LegalEntity");
                              }
                              if (parentAccount instanceof org.opencrx.kernel.account1.jmi1.Group) {
                                image = "Group.gif";
                                label = app.getLabel("org:opencrx:kernel:account1:Group");
                              }
%>
                              <tr class="gridTableRowFull" style="color:white;background-color:<%= colorDuplicate %>;"><!-- 10 columns -->
                                <td align="right" title="<%= app.getLabel(ACCOUNT_CLASS) %> with duplicate <%= app.getLabel(EMAILADDRESS_CLASS) %>"><img src="../../images/downRed3T.gif" border="0" align="top" alt="o" /></td>
                                <td align="left" title="<%= app.getLabel(ACCOUNT_CLASS) %> with duplicate <%= app.getLabel(EMAILADDRESS_CLASS) %>"><a href="<%= parentAccountHref %>" target="_blank"><img src="../../images/<%= image %>" border="0" align="top" alt="o" />&nbsp;<%= (new ObjectReference(parentAccount, app)).getTitle() %></a></td>
                                <td align="left" title="<%= app.getLabel(ACCOUNT_CLASS) %> with duplicate <%= app.getLabel(EMAILADDRESS_CLASS) %>"><%= CAUTION %>
<%
                                    SortedSet<String> currentEMailAddresses = new TreeSet<String>();
                                    org.opencrx.kernel.account1.cci2.EMailAddressQuery ceMailAddressFilter = (org.opencrx.kernel.account1.cci2.EMailAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.EMailAddress.class);
                                    ceMailAddressFilter.forAllDisabled().isFalse();
                                    for (
                                      Iterator ca = parentAccount.getAddress(eMailAddressFilter).iterator();
                                      ca.hasNext();
                                    ) {
                                        org.opencrx.kernel.account1.jmi1.EMailAddress caddr = (org.opencrx.kernel.account1.jmi1.EMailAddress)ca.next();
                                        if (caddr.getEmailAddress() != null && caddr.getEmailAddress().length() > 0) {
                                            // add this address to list
                                            currentEMailAddresses.add(caddr.getEmailAddress());
                                        }
                                    }
%>
                                    <a href="<%= parentAccountHref %>" target="_blank">
<%
                                        for (
                                          Iterator e = currentEMailAddresses.iterator();
                                          e.hasNext();
                                        ) {
                                            String email = (String)e.next();
%>
                                            <%= email %><br>
<%
                                        }
%>
                                    </a>
                                </td>
                                <td align="left" title="<%= app.getLabel(ACCOUNT_CLASS) %> with duplicate <%= app.getLabel(EMAILADDRESS_CLASS) %>">&nbsp;</td>
                                <td align="left" title="<%= app.getLabel(ACCOUNT_CLASS) %> with duplicate <%= app.getLabel(EMAILADDRESS_CLASS) %>">&nbsp;</td>
                                <td align="left" onclick='javascript:window.open(\"" + parentAccountHref + "\");'>&nbsp;</td>
                                <td align="left" onclick='javascript:window.open(\"" + parentAccountHref + "\");'>&nbsp;</td>
                                <td align="left" onclick='javascript:window.open(\"" + parentAccountHref + "\");'>&nbsp;</td>
                                <td align="left" onclick='javascript:window.open(\"" + parentAccountHref + "\");'>&nbsp;</td>
                                <td align="left" onclick='javascript:window.open(\"" + parentAccountHref + "\");'>&nbsp;</td>
                              </tr>
<%
                          }
                      }
                  }

              }
          }
%>
          <tr class="gridTableHeaderFull"><!-- 7 columns -->
            <td align="right"><a href="#" onclick="javascript:try{$('paging').value='--';}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/previous_fast.gif"></a>
              <a href="#" onclick="javascript:try{($('displayStart').value)--}catch(e){};$('Reload.Button').click();"><img border="0" align="top" alt="&lt;" src="../../images/previous.gif"></a>
              #
              <a href="#" onclick="javascript:try{($('displayStart').value)++}catch(e){};$('Reload.Button').click();"><img border="0" align="top" alt="&lt;" src="../../images/next.gif"></a>
              <a href="#" onclick="javascript:try{$('paging').value='++';}catch(e){};$('Reload.Button').click();" onmouseup="javascript:setTimeout('disableSubmit()', 10);" ><img border="0" align="top" alt="&lt;" src="../../images/next_fast.gif"></a></td>
            <td align="left">&nbsp;<b><%= app.getLabel(ACCOUNT_CLASS) %></b></td>
            <td align="left">&nbsp;<b><%= app.getLabel(EMAILADDRESS_CLASS) %></b></td>
            <td align="left">&nbsp;<b><%= app.getLabel(POSTALADDRESS_CLASS) %></b></td>
            <td align="left" nowrap><!-- <img src="../../images/NumberReplacement.gif" alt="" align="top" /> --><b><%= app.getLabel(MEMBERSHIP_CLASS) %></b></td>
            <td align="center">&nbsp;<b><%= userView.getFieldLabel(MEMBER_CLASS, "memberRole", app.getCurrentLocaleAsIndex()) %></b></td>
            <td align="center">&nbsp;<b><%= userView.getFieldLabel(MEMBER_CLASS, "validFrom", app.getCurrentLocaleAsIndex()) %></b></td>
            <td align="center">&nbsp;<b><%= userView.getFieldLabel(MEMBER_CLASS, "validTo", app.getCurrentLocaleAsIndex()) %></b></td>
            <td align="center"><b><%= userView.getFieldLabel(MEMBER_CLASS, "name", app.getCurrentLocaleAsIndex()) != null ? userView.getFieldLabel(MEMBER_CLASS, "name", app.getCurrentLocaleAsIndex()) : "" %></b></td>
            <td align="center"><b><%= userView.getFieldLabel(MEMBER_CLASS, "description", app.getCurrentLocaleAsIndex()) %></b></td>
          </tr>
        </table>
        </td></tr></table>
<%
        if (!highAccountIsKnown && (counter-1 > highAccount)) {
          highAccount = counter-1;
        }
%>
        <input type="hidden" name="highAccountIsKnown" id="highAccountIsKnown" value="<%= highAccountIsKnown ? "highAccountIsKnown" : ""  %>" />
        <input type="hidden" id="highAccount" name="highAccount" value="<%= highAccount %>" />
<%
      String displayStartSelector = "<select onchange='javascript:$(\\\"waitMsg\\\").style.visibility=\\\"visible\\\";$(\\\"submitButtons\\\").style.visibility=\\\"hidden\\\";$(\\\"Reload.Button\\\").click();' id='displayStart' name='displayStart' tabindex='" + tabIndex++ + "' style='text-align:right;'>";
      int i = 0;
      while (i*pageSize < highAccount) {
        displayStartSelector += "<option " + (i == displayStart ? "selected" : "") + " value='" + formatter.format(i) + "'>" + formatter.format(pageSize*i + 1) + ".." + formatter.format(highAccount < pageSize*(i+1) ? highAccount : (pageSize*(i+1))) + "&nbsp;</option>";
        i++;
      }
      if (!highAccountIsKnown) {
        displayStartSelector += "<option value='" + formatter.format(i) + "'>" + formatter.format(pageSize*i + 1) + ".." + formatter.format(pageSize*(i+1)) + "&nbsp;</option>";
        displayStartSelector += "<option value='+100'>+100&nbsp;</option>";
        displayStartSelector += "<option value='+500'>+500&nbsp;</option>";
        displayStartSelector += "<option value='+1000'>+1000&nbsp;</option>";
        displayStartSelector += "<option value='+5000'>+5000&nbsp;</option>";
        displayStartSelector += "<option value='+10000'>+10000&nbsp;</option>";
        displayStartSelector += "<option value='+50000'>+50000&nbsp;</option>";
        displayStartSelector += "<option value='+100000'>+100000&nbsp;</option>";
        displayStartSelector += "<option value='+500000'>+500000&nbsp;</option>";
      }
      displayStartSelector += "</select>";
%>
      <script language='javascript' type='text/javascript'>
        try {
          $('displayStartSelector').innerHTML = "<%= displayStartSelector %>";
          $('highAccount').value = "<%= formatter.format(highAccount) %>";
          $('submitButtons').style.visibility='visible';
        } catch(e){};

        function disableSubmit() {
          $('waitMsg').style.display='block';
          $('submitButtons').style.display='none';
        }
      </script>
      <br />
      <input type="Button" name="Print.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="Print" onClick="javascript:window.print();return false;" />
      <input type="Submit" name="Cancel.Button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getCloseText() %>" onClick="javascript:window.close();" />
      <br />&nbsp;
<%
      if (downloadAction != null) {
          wb.write(os);
          os.flush();
          os.close();
      }

    }
    catch (Exception e) {
			new ServiceException(e).log();
      out.println("<p><b>!! Failed !!<br><br>The following exception(s) occured:</b><br><br><pre>");
      PrintWriter pw = new PrintWriter(out);
      ServiceException e0 = new ServiceException(e);
      pw.println(e0.getMessage());
      pw.println(e0.getCause());
      out.println("</pre>");
    } finally {
    	if(pm != null) {
    		pm.close();
    	}
    }
%>
      </div> <!-- content -->
    </div> <!-- content-wrap -->
    </form>
  </div> <!-- wrap -->
</div> <!-- container -->
</body>
</html>
