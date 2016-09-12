/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: VCard
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2015, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.backend;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;
import java.util.UUID;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.account1.jmi1.WebAddress;
import org.opencrx.kernel.base.jmi1.ImportParams;
import org.opencrx.kernel.document1.jmi1.Media;
import org.opencrx.kernel.generic.jmi1.Note;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Codes;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * VCard
 *
 */
public class VCard extends AbstractImpl {

	/**
	 * Register VCard backend.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new VCard());
	}
	
	/**
	 * Get registered VCard backend.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public static VCard getInstance(
	) throws ServiceException {
		return getInstance(VCard.class);
	}

	/**
	 * Constructor.
	 * 
	 */
	protected VCard(
	) {
		
	}
	
	/**
	 * VCardField
	 *
	 */
	public static class VCardField {
		
		public VCardField(
			String name,
			String value,
			Map<String,List<String>> parameters
		) {
			this.name = name;
			this.value = value;
			this.parameters = parameters;
		}

		public static VCardField getField(
			String name,
			String parameterName,
			List<String> parameterValues,
			Map<String,VCardField> vcard
		) {
			for(VCardField field: vcard.values()) {
				if(
					name.equals(field.getName()) && 
					(parameterName == null || (field.getParameters().get(parameterName) != null &&
					field.getParameters().get(parameterName).containsAll(parameterValues)))
				) {
					return field;
				}
			}
			return null;
		}
		
		public static String getFieldValue(
			String name,
			String parameterName,
			List<String> parameterValues,
			Map<String,VCardField> vcard			
		) {
			VCardField field = getField(
				name,
				parameterName,
				parameterValues,
				vcard
			);
			return field == null ? null : field.getValue();
		}
		
		public static String getFieldValue(
			String name,
			Map<String,VCardField> vcard			
		) {
			return getFieldValue(
				name,
				null,
				null,
				vcard
			);
		}
		
		public String getName(
		) {
			return this.name;
		}
		
		public String getValue(
		) {
			return this.value;
		}
		
		public void setValue(
			String value
		) {
			this.value = value;
		}
		
		public Map<String,List<String>> getParameters(
		) {
			return this.parameters;
		}

		@Override
        public boolean equals(
        	Object obj
        ) {
			if(obj instanceof VCardField) {
				VCardField that = (VCardField)obj;
				return 
					this.name.equals(that.name) && 
					this.parameters.equals(that.parameters) &&
					this.value.equals(that.value);
			} else {
				return super.equals(obj);
			}
        }
		
		@Override
        public String toString(
        ) {
			return this.name + this.parameters.toString() + ":" + this.value;
        }

		private final String name;
		private String value;
		private final Map<String,List<String>> parameters;
	}

	/**
	 * Escape new lines.
	 * 
	 * @param from
	 * @return
	 */
	protected String escapeNewlines(
        String from
    ) {
        String to = "";
        for(int i = 0; i < from.length(); i++) {
            if(from.charAt(i) == '\n') {
                to += "\\n";
            }
            else {
                to += from.charAt(i);
            }
        }
        return to;
    }
    
    /**
     * Get UTC formatted dateTime.
     * 
     * @param dateTime
     * @param dateTimeFormatter
     * @return
     * @throws ParseException
     */
    protected String getUtcDateTime(
        String dateTime,
        SimpleDateFormat dateTimeFormatter
    ) throws ParseException {
        Date date = null;
        if(dateTime.endsWith("Z")) {
            if(dateTime.length() == 16) {
                date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime.substring(0, 15) + ".000Z");
            }
            else {
                date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime);
            }
        }
        else if(dateTime.length() == 8) {
            date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime + "T000000.000Z");
        }
        else {
            date = dateTimeFormatter.parse(dateTime);
        }
        return DateTimeFormat.BASIC_UTC_FORMAT.format(date);
    }
    
    /**
     * Get UTC formatted date.
     * 
     * @param dateTime
     * @param dateTimeFormatter
     * @return
     * @throws ParseException
     */
    protected Date getUtcDate(
        String dateTime,
        SimpleDateFormat dateTimeFormatter
    ) throws ParseException {
        Date date = null;
        if(dateTime.endsWith("Z")) {
            if(dateTime.length() == 16) {
                date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime.substring(0, 15) + ".000Z");
            }
           else {
                date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime);
            }
        }
        else if(dateTime.length() == 8) {
            date = DateTimeFormat.BASIC_UTC_FORMAT.parse(dateTime + "T000000.000Z");
        }
        else {
            date = dateTimeFormatter.parse(dateTime);
        }
        return date;
    }
    
    /**
     * Encode vcard string.
     * 
     * @param s
     * @return
     */
    protected String encodeString(
        String s
    ) {
        if(s == null) return s;
        s = s.replace(";", "\\;");
        return s;
    }
    
    /**
     * Map salutation code to text.
     * 
     * @param salutationCode
     * @return
     * @throws ServiceException
     */
    private String mapToSalutationText(
        short salutationCode
    ) throws ServiceException {
    	return salutations.get(Integer.valueOf(salutationCode));
    }
    
    /**
     * Map salutation text to code.
     * 
     * @param salutation
     * @param codeSegment
     * @return
     * @throws ServiceException
     */
    private short mapToSalutationCode(
        String salutation,
        org.opencrx.kernel.code1.jmi1.Segment codeSegment        
    ) throws ServiceException {
    	if(codeSegment != null) {
    		try {
		    	Codes codes = new Codes(codeSegment);
		    	return codes.findCodeFromValue(
		    		salutation, 
		    		"org:opencrx:kernel:account1:Contact:salutationCode" 
		    	);
    		} catch(Exception e) {}
    	}    	
    	for(Entry<Integer,String> entry: salutations.entrySet()) {
    		if(entry.getValue().equals(salutation)) {
    			return entry.getKey().shortValue();
    		}
    	}
    	return 0;
    }
    
    /**
     * Update sourceVcard with account values and return merged vcard.
     * 
     * @param account
     * @param sourceVcard
     * @param statusMessage
     * @return
     * @throws ServiceException
     */
    public String mergeVcard(
        Account account,
        String sourceVcard,
        List<String> statusMessage
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
        org.opencrx.kernel.code1.jmi1.Segment codeSegment = null;
        try {
        	codeSegment = (org.opencrx.kernel.code1.jmi1.Segment)pm.getObjectById(
        		new Path("xri://@openmdx*org.opencrx.kernel.code1").getDescendant("provider", account.refGetPath().get(2), "segment", "Root")        		
        	);
        } catch(Exception e) {}    	
    	boolean isContact = account instanceof Contact;
        // N
        String n = null;        
        if(isContact) {
        	Contact contact = (Contact)account;
            if(contact.getLastName() != null) {
                String lastName = contact.getLastName();
                String firstName = contact.getFirstName() == null ? "" : contact.getFirstName();
                String middleName = contact.getMiddleName() == null ? "" : contact.getMiddleName();
                Short salutationCode = contact.getSalutationCode();
                String salutation = contact.getSalutation() == null ?
                    (salutationCode == null) || (salutationCode.shortValue() == 0) ?
                        null :
                        this.mapToSalutationText(salutationCode) :
                    contact.getSalutation();
                String suffix = contact.getSuffix() == null ? "" : contact.getSuffix();
                n = lastName + ";" + firstName + ";"+ middleName + ";" + (salutation == null ? "" : salutation) + ";" + suffix;
            }
        } else if(account instanceof AbstractGroup) {
        	AbstractGroup group = (AbstractGroup)account;
            if(group.getName() != null) {
                String name = group.getName();
                n = name;
            }
        }
        // FN
        String fn = null;
        if(isContact) {
        	Contact contact = (Contact)account;
            String firstName = contact.getFirstName() == null ? "" : contact.getFirstName();
            String lastName = contact.getLastName() == null ? "" : contact.getLastName();
            String middleName = contact.getMiddleName() == null ? "" : contact.getMiddleName();
            String suffix = contact.getSuffix() == null ? "" : contact.getSuffix();
            fn = firstName + (middleName.length() == 0 ? "" : " " + middleName) + (lastName.length() == 0 ? "" : " " + lastName) + (suffix.length() == 0 ? "" : " " + suffix);
        } else {
            String fullName = account.getFullName() == null ? "" : account.getFullName();
            fn = fullName;            
        }
        // NICKNAME
        String nickName = null;
        if(isContact) {
        	Contact contact = (Contact)account;        	
            nickName = contact.getNickName() == null ? "" : contact.getNickName();            
        }
        // REV
        String rev = DateTimeFormat.BASIC_UTC_FORMAT.format(new Date());        
        // ORG
        String org = null;
        if(isContact) {
        	Contact contact = (Contact)account;        	
            if(contact.getOrganization() != null) {
                org = contact.getOrganization();
            }
        }
        // TITLE
        String title = null;
        if(isContact) {
        	Contact contact = (Contact)account;        	
            if(contact.getJobTitle() != null) {
                title = contact.getJobTitle();
            }
        }
        // BDAY
        String bday = null;
        if(isContact) {
        	Contact contact = (Contact)account;        	
            if(contact.getBirthdate() != null) {
                bday = DateTimeFormat.BASIC_UTC_FORMAT.format(contact.getBirthdate());
            }
        }
        // NOTE
        String note = "";
        try {
        	if(!JDOHelper.isNew(account)) {
	        	Note aNote = (Note)pm.getObjectById(
	        		account.refGetPath().getDescendant("note", "VCARD")
	        	);
	        	if(aNote != null) {
	        		note = aNote.getText();
	        	}
        	}
        } catch(Exception e) {}
        // ADR, TEL
        AccountAddress[] addresses = new AccountAddress[11];
        try {
            addresses = Accounts.getInstance().getMainAddresses(
            	account
            );
        } catch(Exception e) {
            ServiceException e0 = new ServiceException(e);
            if(e0.getExceptionCode() != BasicException.Code.NOT_FOUND) {
                throw e0;
            }
        }
        // TEL;WORK;VOICE
        String telWorkVoice = addresses[Accounts.PHONE_BUSINESS] == null ? 
            "" : 
            ((PhoneNumber)addresses[Accounts.PHONE_BUSINESS]).getPhoneNumberFull();
        // TEL;HOME;VOICE
        String telHomeVoice = addresses[Accounts.PHONE_HOME] == null ? 
            "" : 
            ((PhoneNumber)addresses[Accounts.PHONE_HOME]).getPhoneNumberFull();
        // TEL;CELL;VOICE
        String telCellVoice = addresses[Accounts.MOBILE] == null ? 
            "" : 
            ((PhoneNumber)addresses[Accounts.MOBILE]).getPhoneNumberFull();
        // TEL;FAX
        String telWorkFax = addresses[Accounts.FAX_BUSINESS] == null ? 
            "" : 
            ((PhoneNumber)addresses[Accounts.FAX_BUSINESS]).getPhoneNumberFull();
        // TEL;HOME;FAX
        String telHomeFax = addresses[Accounts.FAX_HOME] == null ? 
            "" : 
            ((PhoneNumber)addresses[Accounts.FAX_HOME]).getPhoneNumberFull();
        // ADR;WORK
        String adrWork = "";
        if(addresses[Accounts.POSTAL_BUSINESS] != null) {
            PostalAddress postalAddress = (PostalAddress)addresses[Accounts.POSTAL_BUSINESS];
            StringBuilder adr = new StringBuilder();
            // postalAddressLine
            List<String> addressLines = postalAddress.getPostalAddressLine();
            for(int j = 0; j < addressLines.size(); j++) {
                adr.append(adr.length() == 0 ? "" : "\\n");
                adr.append(this.encodeString(addressLines.get(j)));
            }
            // postalStreet
            List<String> postalStreet = postalAddress.getPostalStreet();
            for(int j = 0; j < postalStreet.size(); j++) {
                adr.append(adr.length() == 0 ? "" : "\\n");
                adr.append(this.encodeString(postalStreet.get(j)));
            }
            // postalCity
            adr.append(
                postalAddress.getPostalCity() == null 
                	? ";" 
                	: ";" + this.encodeString(postalAddress.getPostalCity())
            );
            // postalState
            adr.append(
                postalAddress.getPostalState() == null 
                	? ";" 
                	: ";" + this.encodeString(postalAddress.getPostalState())
            );            
            // postalCode
            adr.append(
                postalAddress.getPostalCode() == null
                	? ";" 
                	: ";" + this.encodeString(postalAddress.getPostalCode())
            );
            adr.append(";");
            String[] postalCountry = Addresses.getInstance().mapToPostalCountryText(
        		postalAddress.getPostalCountry(),
        		codeSegment
        	); 
            adr.append(
            	postalCountry == null 
            		? "" 
            		: postalCountry[0]
            );   
            adrWork = adr.toString();
        }
        // ADR;HOME
        String adrHome = "";
        if(addresses[Accounts.POSTAL_HOME] != null) {
            PostalAddress postalAddress = (PostalAddress)addresses[Accounts.POSTAL_HOME];
            StringBuilder adr = new StringBuilder();
            // postalAddressLine
            List<String> addressLines = postalAddress.getPostalAddressLine();
            for(int j = 0; j < addressLines.size(); j++) {
                adr.append(adr.length() == 0 ? "" : "\\n");
                adr.append(this.encodeString(addressLines.get(j)));
            }
            // postalStreet
            List<String> postalStreet = postalAddress.getPostalStreet();
            for(int j = 0; j < postalStreet.size(); j++) {
                adr.append(adr.length() == 0 ? "" : "\\n");
                adr.append(this.encodeString(postalStreet.get(j)));
            }
            // postalCity
            adr.append(
                postalAddress.getPostalCity() == null 
                	? ";" 
                	: ";" + this.encodeString(postalAddress.getPostalCity())
            );
            // postalState
            adr.append(
                postalAddress.getPostalState() == null 
                	? ";" 
                	: ";" + this.encodeString(postalAddress.getPostalState())
            );
            // postalCode
            adr.append(
                postalAddress.getPostalCode() == null 
                	? ";" 
                	: ";" + this.encodeString(postalAddress.getPostalCode())
            );
            // postalCountry
            adr.append(";");
            String[] postalCountry = Addresses.getInstance().mapToPostalCountryText(
        		postalAddress.getPostalCountry(),
        		codeSegment
        	);            
            adr.append(
            	postalCountry == null 
	        		? "" 
	        		: postalCountry[0]            	
            );   
            adrHome = adr.toString();
        }
        // URL;WORK
        String urlWork = addresses[Accounts.WEB_BUSINESS] == null ? 
        	"" : 
        	((WebAddress)addresses[Accounts.WEB_BUSINESS]).getWebUrl();
        // URL;HOME
        String urlHome = addresses[Accounts.WEB_HOME] == null ? 
            "" : 
            ((WebAddress)addresses[Accounts.WEB_HOME]).getWebUrl();
        // EMAIL;TYPE=PREF,INTERNET,WORK
        String emailWork = addresses[Accounts.MAIL_BUSINESS] == null ? 
            "" : 
            ((EMailAddress)addresses[Accounts.MAIL_BUSINESS]).getEmailAddress();
        // EMAIL;TYPE=INTERNET,HOME
        String emailHome = addresses[Accounts.MAIL_HOME] == null ? 
            "" : 
            ((EMailAddress)addresses[Accounts.MAIL_HOME]).getEmailAddress();        
        // return if data is missing
        if(!statusMessage.isEmpty()) {
            return null;
        }
        if((sourceVcard == null) || sourceVcard.isEmpty()) {
            // Empty template
            UUID uid = null;
            try {
                uid = UUIDConversion.fromString(account.refGetPath().getBase());
            } catch(Exception e) {
                uid = UUIDs.newUUID();
            }
            sourceVcard = 
                "BEGIN:VCARD\n" +
                "VERSION:3.0\n" +
                "UID:" + UUIDConversion.toUID(uid) + "\n" +
                "REV:" + rev.substring(0, 15) + "Z\n" +
                "N:\n" +
                "FN:\n" + 
                "NICKNAME:\n" +
                "ORG:\n" +
                "TITLE:\n" +
                "TEL;WORK;VOICE:\n" +
                "TEL;HOME;VOICE:\n" +
                "TEL;CELL;VOICE:\n" +
                (isContact ? "BDAY:\n" : "") +
                "TEL;FAX:\n" +
                "TEL;HOME;FAX:\n" +
                "ADR;WORK:;;\n" +
                "ADR;HOME:;;\n" +
                "URL;HOME:\n" +
                "URL;WORK:\n" +
                "EMAIL;TYPE=PREF,INTERNET,WORK:\n" +
                "EMAIL;TYPE=INTERNET,HOME:\n" +
                "NOTE:\n" +
                "END:VCARD";
        }
        try {
            QuotaByteArrayOutputStream targetVcardBos = new QuotaByteArrayOutputStream(VCard.class.getName());
            PrintWriter targetVcard = new PrintWriter(new OutputStreamWriter(targetVcardBos, "UTF-8"));
            String line = null;
            BufferedReader readerSourceVcard = new BufferedReader(new StringReader(sourceVcard));
            boolean isVcard = false;
            String tagStart = null;
            while((line = readerSourceVcard.readLine()) != null) {
                if(!line.startsWith(" ")) {
                    tagStart = line;
                }
                if(
                    line.toUpperCase().startsWith("BEGIN:VCARD")
                ) {
                    targetVcard.println("BEGIN:VCARD");                    
                    isVcard = true;
                } else if(
                    // Dump updated event fields only for first event
                    line.toUpperCase().startsWith("END:VCARD")
                ) {                    
                    // REV
                    if(rev != null) {
                        targetVcard.println("REV:" + rev.substring(0, 15) + "Z");
                    }
                    // N
                    if((n != null) && !n.isEmpty()) {
                        targetVcard.println("N:" + n);
                    }
                    // FN
                    if((fn != null) && !fn.isEmpty()) {
                        targetVcard.println("FN:" + fn);
                    }
                    // NICKNAME
                    if((nickName != null) && !nickName.isEmpty()) {
                        targetVcard.println("NICKNAME:" + nickName);
                    }
                    // ORG
                    if((org != null) && !org.isEmpty()) {
                        targetVcard.println("ORG:" + org);
                    }
                    // TITLE
                    if((title != null) && !title.isEmpty()) {
                        targetVcard.println("TITLE:" + title);
                    }
                    // BDAY
                    if((bday != null) && !bday.isEmpty()) {
                        targetVcard.println("BDAY:" + bday.substring(0, 15) + "Z");
                    }
                    // NOTE
                    if((note != null) && !note.isEmpty()) {
                    	targetVcard.println("NOTE:" + this.escapeNewlines(note));
                    }
                    // TEL;WORK;VOICE
                    if((telWorkVoice != null) && !telWorkVoice.isEmpty()) {
                        targetVcard.println("TEL;WORK;VOICE:" + telWorkVoice);
                    }
                    // TEL;HOME;VOICE
                    if((telHomeVoice != null) && !telHomeVoice.isEmpty()) {
                        targetVcard.println("TEL;HOME;VOICE:" + telHomeVoice);
                    }
                    // TEL;CELL;VOICE
                    if((telCellVoice != null) && !telCellVoice.isEmpty()) {
                        targetVcard.println("TEL;CELL;VOICE:" + telCellVoice);
                    }
                    // TEL;FAX
                    if((telWorkFax != null) && !telWorkFax.isEmpty()) {
                        targetVcard.println("TEL;FAX;WORK:" + telWorkFax);
                    }
                    // TEL;HOME;FAX
                    if((telHomeFax != null) && !telHomeFax.isEmpty()) {
                        targetVcard.println("TEL;HOME;FAX:" + telHomeFax);
                    }
                    // ADR;WORK
                    if((adrWork != null) && !adrWork.isEmpty()) {
                        targetVcard.println("ADR;WORK:;;" + adrWork);
                    }
                    // ADR;HOME
                    if((adrHome != null) && !adrHome.isEmpty()) {
                        targetVcard.println("ADR;HOME:;;" + adrHome);
                    }
                    // URL;HOME
                    if((urlHome != null) && !urlHome.isEmpty()) {
                        targetVcard.println("URL;HOME:" + urlHome);
                    }
                    // URL;WORK
                    if((urlWork != null) && !urlWork.isEmpty()) {
                        targetVcard.println("URL;WORK:" + urlWork);
                    }
                    // EMAIL;PREF;INTERNET
                    if((emailWork != null) && !emailWork.isEmpty()) {
                        targetVcard.println("EMAIL;TYPE=PREF,INTERNET,WORK:" + emailWork);
                    }
                    // EMAIL;INTERNET
                    if((emailHome != null) && !emailHome.isEmpty()) {
                        targetVcard.println("EMAIL;TYPE=INTERNET,HOME:" + emailHome);
                    }
                    targetVcard.println("END:VCARD");                                            
                    isVcard = false;
                } else if(isVcard ) {
                    boolean isUpdatableTag = 
                        tagStart.toUpperCase().startsWith("REV");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("N");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("FN");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("NICKNAME");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("ORG");
                    isUpdatableTag |= 
                        tagStart.toUpperCase().startsWith("TITLE");
                    isUpdatableTag |= 
                        tagStart.toUpperCase().startsWith("BDAY");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("TEL");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("ADR");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("URL");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("EMAIL");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("PHOTO");
                    isUpdatableTag |=
                        tagStart.toUpperCase().startsWith("NOTE");
                    if(!isUpdatableTag) {
                        targetVcard.println(line);
                    }
                } else {
                    targetVcard.println(line);                    
                }
            }
            targetVcard.flush();
            targetVcardBos.close();
            try {
                return targetVcardBos.toString("UTF-8");
            } catch(Exception e) {
                return null;
            }
        } catch(Exception e) {
            return null;
        }
    }
    
    /**
     * Update postal address with new address value.
     * 
     * @param address
     * @param newValue
     * @param locale
     * @param codeSegment
     * @return
     * @throws ServiceException
     */
    public boolean updatePostalAddress(
        PostalAddress address,
        String newValue,
        short locale,
        org.opencrx.kernel.code1.jmi1.Segment codeSegment
    ) throws ServiceException {
    	boolean modified = false;
        if((newValue != null) && (newValue.length() > 0)) {
            String[] tokens = new String[]{"", "", "", "", "", "", ""};
            // Unescape semicolons
            // Replace semicolons by tabs
            newValue = newValue.replace("\\;", "\u0001");
            newValue = newValue.replace(";", "\t");
            newValue = newValue.replace("\u0001", ";");
            StringTokenizer tokenizer = new StringTokenizer(newValue, "\t", true);
            int ii = 0;
            boolean hasTokens = false;
            while(tokenizer.hasMoreTokens() && (ii < tokens.length)) {
                String t = tokenizer.nextToken();
                if("\t".equals(t)) {
                    ii++;
                }
                else {
                    tokens[ii] = t;
                    hasTokens = true;
                }
            }
            if(!hasTokens) {
                return false;
            }
            // parse street (=0D=0A or \n separated tokens)
            List<String> street = new ArrayList<String>();
            String temp = tokens[2];
            int pos = 0;
            if(temp.indexOf("=0D=0A") >= 0) {
	            while((pos = temp.indexOf("=0D=0A")) >= 0) {
	                street.add(temp.substring(0, pos));
	                temp = temp.substring(pos + 6);
	            }
            } else if(temp.indexOf("\\n") >= 0) {
	            while((pos = temp.indexOf("\\n")) >= 0) {
	                street.add(temp.substring(0, pos));
	                temp = temp.substring(pos + 2);
	            }
            }
            street.add(temp);
            address.getPostalAddressLine().clear();
            int nPostalAddressLines = Math.min(2, street.size() - 1);
            for(int i = 0; i < nPostalAddressLines; i++) {
                address.getPostalAddressLine().add(street.get(i));
            }
            address.getPostalStreet().clear();
            for(int i = nPostalAddressLines; i < street.size(); i++) {
                address.getPostalStreet().add(street.get(i));
                modified = true;
            }
            // Only touch if changed
            if(!tokens[3].equals(address.getPostalCity())) {
            	address.setPostalCity(tokens[3]);
            	modified = true;
            }
            if(!tokens[4].equals(address.getPostalState())) {
            	address.setPostalState(tokens[4]);
            	modified = true;
            }
            if(!tokens[5].equals(address.getPostalCode())) {
            	address.setPostalCode(tokens[5]);
            	modified = true;
            }
            // Lookup country
            SysLog.trace("lookup country", tokens[6]);
            short postalCountry = Addresses.getInstance().mapToPostalCountryCode(
        		tokens[6],
               	codeSegment
            );
            if(postalCountry != address.getPostalCountry()) {
            	address.setPostalCountry(postalCountry);
            	modified = true;
            }
            SysLog.trace("updated address", address);
            return true;
        }
        return modified;
    }

    /**
     * Update phone number with new value.
     * 
     * @param address
     * @param newValue
     * @return
     * @throws ServiceException
     */
    public boolean updatePhoneNumber(
        PhoneNumber address,
        String newValue
    ) throws ServiceException {
        if((newValue != null) && (newValue.length() > 0)) {
        	String normalizedPhoneNumberNew = "";        	
        	for(int i = 0; i < newValue.length(); i++) {
        		char c = newValue.charAt(i);
        		if(c != ' ' && c != '(' && c != ')' && c != '-') {
        			normalizedPhoneNumberNew += c;
        		}
        	}
        	String normalizedPhoneNumber = "";
        	if(address.getPhoneNumberFull() != null) {
        		String phoneNumber = address.getPhoneNumberFull();
            	for(int i = 0; i < phoneNumber.length(); i++) {
            		char c = phoneNumber.charAt(i);
            		if(c != ' ' && c != '(' && c != ')' && c != '-') {
            			normalizedPhoneNumber += c;
            		}
            	}        		
        	}
        	if(!normalizedPhoneNumberNew.equals(normalizedPhoneNumber)) {
	            address.setPhoneNumberFull(newValue);
        	}
            address.setAutomaticParsing(Boolean.TRUE);
            Addresses.getInstance().updatePhoneNumber(address);
            SysLog.trace("updated address", address);
            return true;
        }
        return false;
    }

    /**
     * Update web address with new value.
     * 
     * @param address
     * @param newValue
     * @return
     */
    public boolean updateWebAddress(
        WebAddress address,
        String newValue
    ) {
        if((newValue != null) && (newValue.length() > 0)) {
            address.setWebUrl(newValue);
            SysLog.trace("updated address", address);
            return true;
        }
        return false;
    }

    /**
     * Update e-mail address with new value.
     * 
     * @param address
     * @param newValue
     * @return
     */
    public boolean updateEMailAddress(
        EMailAddress address,
        String newValue
    ) {
        if((newValue != null) && (newValue.length() > 0)) {
            address.setEmailAddress(newValue);
            SysLog.trace("updated address", address);
            return true;
        }
        return false;
    }

    /**
     * Get UID of given vcard.
     * 
     * @param vcard
     * @return
     */
    public String getVCardUid(
    	String vcard
    ) {
    	String uid = null;
    	if(vcard.indexOf("UID:") > 0) {
    		int start = vcard.indexOf("UID:");
    		int end = vcard.indexOf("\n", start);
    		if(end > start) {
    			uid = vcard.substring(start + 4, end).trim();
    		}
    	}    	
    	return uid;
    }

    /**
     * Parse vcard.
     * 
     * @param reader
     * @param vcardAsString
     * @return
     * @throws IOException
     */
    public Map<String,VCardField> parseVCard(
        BufferedReader reader,
        StringBuilder vcardAsString
    ) throws IOException {
        Map<String,VCardField> vcard = new HashMap<String,VCardField>();
        List<String> lines = new ArrayList<String>();
        try {
	        String s = null;
	        while((s = reader.readLine()) != null) {
	        	lines.add(s);
	        }
        } catch(Exception e) {}
    	int l = 0;
    	while(l < lines.size()) {
    		String line = lines.get(l++);
    		while(
    			l < lines.size() && 
    			(lines.get(l).startsWith(" ") || (line.endsWith("=") && ((line.length() - 1) % 62 == 0)))
    		) {
    			if(line.endsWith("=") && ((line.length() - 1) % 62 == 0)) {
    				line = line.substring(0, 62) + lines.get(l++);
    			} else {
    				line += lines.get(l++).substring(1);
    			}
    		}
            vcardAsString.append(line).append("\n");        	
            int pos;
            if((pos = line.indexOf(":")) >= 0) {
            	String qualifiedFieldName = line.substring(0, pos).toUpperCase();
                String[] fieldNameParts = qualifiedFieldName.split(";");
                Map<String,List<String>> parameters = new HashMap<String,List<String>>();
                for(int i = 1; i < fieldNameParts.length; i++) {
                	String fieldNamePart = fieldNameParts[i];
                	String parameterName = "TYPE";
                	String[] parameterValues = new String[]{};
                	if(fieldNamePart.indexOf("=") > 0) {
                		int index = fieldNamePart.indexOf("=");
                		parameterName = fieldNamePart.substring(0, index);
                		parameterValues = fieldNamePart.substring(index + 1).split(",");
                	} else {
                		parameterName = "TYPE";
                		parameterValues = fieldNamePart.split(",");
                	}
                	if(parameters.get(parameterName) != null) {
                		parameters.get(parameterName).addAll(
                			Arrays.asList(parameterValues)
                		);
                	} else {
                		parameters.put(
                			parameterName, 
                			new ArrayList<String>(Arrays.asList(parameterValues))
                		);
                	}
                }
                vcard.put(
                    qualifiedFieldName,
                    new VCardField(
                    	fieldNameParts[0],
                    	line.substring(pos + 1, line.length()),
                    	parameters
                    )
                );
            }
        }
    	if(this.getVCardUid(vcardAsString.toString()) == null) {
    		// Add generated UID
    		int pos = vcardAsString.indexOf("BEGIN:VCARD");
    		if(pos >= 0) {
    			String uid = this.getUidAsString();
        		vcardAsString.replace(pos, pos + 11, "BEGIN:VCARD\nUID:" + uid);
                vcard.put(
                    "UID",
                    new VCardField(
                    	"UID",
                    	uid,
                    	Collections.<String,List<String>>emptyMap()
                    )
                );
    		}
    	}
        return vcard;
    }

    /**
     * Import vcard.
     * 
     * @param item
     * @param account
     * @param locale
     * @param errors
     * @param report
     * @return
     * @throws ServiceException
     */
    public BasicObject importItem(
        byte[] item,
        Account account,
        short locale,
        List<String> errors,
        List<String> report
    ) throws ServiceException {
        try {
            InputStream is = new ByteArrayInputStream(item);
            BufferedReader reader = new BufferedReader(
                new InputStreamReader(is, "UTF-8")
            );
            StringBuilder vcardAsString = new StringBuilder();
            Map<String,VCardField> vcard = this.parseVCard(
            	reader,
                vcardAsString
            );
            SysLog.detail("Parsed vcard", vcard);
            return this.importItem(
            	vcardAsString.toString(),
                vcard,
                account,
                locale,
                report
            );
        } catch(IOException e) {
        	SysLog.warning("can not read item", e.getMessage());
        }
        return null;
    }

    /**
     * Map vcard to given account.
     * 
     * @param vcardAsString
     * @param vcard
     * @param account
     * @param locale
     * @param report
     * @return
     * @throws ServiceException
     */
    protected Account importItem(
    	String vcardAsString,
        Map<String,VCardField> vcard,
        Account account,
        short locale,
        List<String> report
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(account);
        SimpleDateFormat dateTimeFormatter = new SimpleDateFormat(DATETIME_FORMAT);
        org.opencrx.kernel.code1.jmi1.Segment codeSegment = null;
        try {
        	codeSegment = (org.opencrx.kernel.code1.jmi1.Segment)pm.getObjectById(
        		new Path("xri://@openmdx*org.opencrx.kernel.code1").getDescendant("provider", account.refGetPath().getSegment(2).toClassicRepresentation(), "segment", "Root")        		
        	);
        } catch(Exception e) {}
        dateTimeFormatter.setLenient(false);
        boolean isContact = account instanceof Contact;
        // name
        String name = VCardField.getFieldValue("N", vcard);
        if(isContact) {
        	Contact contact = (Contact)account;
            if((name != null) && (name.indexOf(";") >= 0)) {
                String[] nameTokens = new String[]{"", "", "", "", ""};
                StringTokenizer tokenizer = new StringTokenizer(name, ";", true);
                int ii = 0;
                while(tokenizer.hasMoreTokens() && (ii < nameTokens.length)) {
                    String t = tokenizer.nextToken();
                    if(";".equals(t)) {
                        ii++;
                    } else {
                        nameTokens[ii] = t;
                    }
                }
                // lastName
                if(!nameTokens[0].isEmpty()) {
                    contact.setLastName(nameTokens[0]);        
                } else if(contact.getLastName() == null) {
                    contact.setLastName("N/A");
                }
                // firstName
                if(!nameTokens[1].isEmpty()) {
                    contact.setFirstName(nameTokens[1]);
                }
                // middleName
                if(!nameTokens[2].isEmpty()) {
                    contact.setMiddleName(nameTokens[2]);
                }
                // salutation
                if(!nameTokens[3].isEmpty()) {
                    String salutation = nameTokens[3];
                    short salutationCode = this.mapToSalutationCode(
                    	salutation, 
                    	codeSegment
                    );
                    contact.setSalutationCode(salutationCode);
                    contact.setSalutation(salutation);                        
                } else {                	
                	contact.setSalutation(null);
                }
                // suffix
                if(!nameTokens[4].isEmpty()) {
                    contact.setSuffix(nameTokens[4]);
                }
            }
            // nickName
            String nickName = VCardField.getFieldValue("NICKNAME", vcard);
            if((nickName != null) && !nickName.isEmpty()) {
                contact.setNickName(nickName);
            }
            // jobTitle
            String jobTitle = VCardField.getFieldValue("TITLE", vcard);
            if((jobTitle != null) && !jobTitle.isEmpty()) {
                contact.setJobTitle(jobTitle);
            }
            // organization
            String organization = VCardField.getFieldValue("ORG", vcard);
            if((organization != null) && !organization.isEmpty()) {
                contact.setOrganization(organization.split(";")[0]);
            }
            // bday
            String bday = VCardField.getFieldValue("BDAY", vcard);
            if((bday != null) && !bday.isEmpty()) {
            	bday = bday.replace("-", "");
                try {
                	if(bday.length() == 8) {
                		bday = bday + "T000000.000Z";
                	}
                	if((bday.endsWith("T000000Z") || bday.endsWith("T000000.000Z")) && contact.getBirthdate() != null) {
                		// In case time is T000000.000Z assume time is unknown. In 
                		// this case do not override time set on contact's birthday
                		bday =
                			bday.substring(0, 9) + 
                			DateTimeFormat.BASIC_UTC_FORMAT.format(contact.getBirthdate()).substring(9);
                	}
                    contact.setBirthdate(
                    	this.getUtcDate(
                            bday, 
                            dateTimeFormatter
                        )
                    );
                } catch(Exception e) {}
            }
            report.add("Update account");
        } else if(account instanceof AbstractGroup){
        	AbstractGroup group = (AbstractGroup)account;
            if(name != null) {
                String[] nameTokens = new String[]{"", "", "", "", ""};
                StringTokenizer tokenizer = new StringTokenizer(name, ";", true);
                int ii = 0;
                while(tokenizer.hasMoreTokens() && (ii < nameTokens.length)) {
                    String t = tokenizer.nextToken();
                    if(";".equals(t)) {
                        ii++;
                    } else {
                        nameTokens[ii] = t;
                    }
                }
                // name
                if(!nameTokens[0].isEmpty()) {
                	group.setName(nameTokens[0]);        
                } else if(group.getName() == null) {
                    group.setName("N/A");
                }
            }
        }
        // photo
        String photo = VCardField.getFieldValue("PHOTO", vcard);
        if(photo != null && !photo.isEmpty()) {
        	org.opencrx.kernel.generic.jmi1.Media picture = null;
        	try {
        		picture = (org.opencrx.kernel.generic.jmi1.Media)pm.getObjectById(account.refGetPath().getDescendant("media", "VCARD"));            			
        	} catch(Exception e) {}
        	if(picture == null) {
        		picture = pm.newInstance(org.opencrx.kernel.generic.jmi1.Media.class);
            	account.addMedia(
            		"VCARD",
            		picture
            	);
        	}
        	List<String> mimeTypes = VCardField.getField("PHOTO", null, null, vcard).getParameters().get("TYPE");
        	String mimeType = mimeTypes.isEmpty() ? "image/jpeg" : mimeTypes.get(0).toLowerCase();
        	if(mimeType.indexOf("/") < 0) {
        		mimeType = "image/" + mimeType;
        	}
        	picture.setContentMimeType(mimeType);
        	picture.setContentName("VCARD~PHOTO");
        	picture.setContent(BinaryLargeObjects.valueOf(Base64.decode(photo)));
        	account.setPicture(picture);
        }
        // externalLink
        boolean hasVcardUid = false;
        List<String> externalLinks = account.getExternalLink();
        String vcardUid = vcard.get("UID") == null 
        	? account.refGetPath().getLastSegment().toClassicRepresentation() 
        	: VCardField.getFieldValue("UID", vcard);
        for(int i = 0; i < externalLinks.size(); i++) {
            if(externalLinks.get(i).startsWith(VCARD_SCHEMA)) {
                externalLinks.set(
                    i,
                    VCARD_SCHEMA + vcardUid
                );
                hasVcardUid = true;
                break;
            }
        }
        if(!hasVcardUid) {
            externalLinks.add(
                VCARD_SCHEMA + vcardUid    
            );
        }
        // note
        String s = vcard.get("NOTE") == null 
        	? null 
        	: VCardField.getFieldValue("NOTE", vcard);
        if(s != null) {
        	Note note = null;
        	try {
	            note = (Note)pm.getObjectById(
	                account.refGetPath().getDescendant(new String[]{"note", "VCARD"})
	            );
        	} catch(Exception e) {}
            if(note == null) {
                note = pm.newInstance(Note.class);
                account.addNote(
                	"VCARD",
                	note
                );
                report.add("Create note");
            } else {
                report.add("Update note");
            }
            note.setTitle("VCARD~NOTE");
            String text = "";
            int pos = 0;
            if(s.indexOf("=0D=0A") >= 0) {
	            while((pos = s.indexOf("=0D=0A")) >= 0) {
	                text += s.substring(0, pos) + "\n";
	                s = s.substring(pos + 6);
	            }
            } else if(s.indexOf("\\n") >= 0) {
	            while((pos = s.indexOf("\\n")) >= 0) {
	                text += s.substring(0, pos) + "\n";
	                s = s.substring(pos + 2);
	            }     	
            } else {
            	text = s;
            }
            note.setText(text);
        }
        // vcard
        account.setVcard(vcardAsString);
        // Addresses
        Collection<AccountAddress> addresses = account.getAddress();
        PostalAddress adrHome = null;
        PostalAddress adrWork = null;
        PhoneNumber telHomeVoice = null;
        PhoneNumber telWorkVoice = null;
        PhoneNumber telHomeFax = null;
        PhoneNumber telWorkFax = null;
        WebAddress urlHome = null;
        WebAddress urlWork = null;
        PhoneNumber telCellVoice = null;
        EMailAddress emailInternet = null;
        EMailAddress emailPrefInternet = null;
        // Get addresses
        for(AccountAddress address: addresses) {
            List<Short> usage = new ArrayList<Short>();
            for(Iterator<Short> j = address.getUsage().iterator(); j.hasNext(); ) {
                usage.add(j.next());
            }
            if(address instanceof PostalAddress) {
                if(usage.contains(Addresses.USAGE_HOME)) {
                    adrHome = (PostalAddress)address;
                } else if(usage.contains(Addresses.USAGE_BUSINESS)) {
                    adrWork = (PostalAddress)address;
                }          
            } else if(address instanceof EMailAddress) {
                if(usage.contains(Addresses.USAGE_BUSINESS)) {
                    emailPrefInternet = (EMailAddress)address;
                } else if(usage.contains(Addresses.USAGE_HOME)) {
                    emailInternet = (EMailAddress)address;
                }
            } else if(address instanceof PhoneNumber) {
                if(usage.contains(Addresses.USAGE_HOME)) {
                    telHomeVoice = (PhoneNumber)address;
                } else if(usage.contains(Addresses.USAGE_BUSINESS)) {
                	// work voice
                    telWorkVoice = (PhoneNumber)address;
                } else if(usage.contains(Addresses.USAGE_HOME_FAX)) {
                    // home fax
                    telHomeFax = (PhoneNumber)address;
                } else if(usage.contains(Addresses.USAGE_BUSINESS_FAX)) {
                    // work fax
                    telWorkFax = (PhoneNumber)address;
                } else if(usage.contains(Addresses.USAGE_MOBILE)) {
                    // cell voice
                    telCellVoice = (PhoneNumber)address;
                }          
            } else if(address instanceof WebAddress) {
                if(usage.contains(Addresses.USAGE_HOME)) {
                    urlHome = (WebAddress)address;
                } else if(usage.contains(Addresses.USAGE_BUSINESS)) {
                    // work url
                    urlWork = (WebAddress)address;
                }
            }
        }
        // update adrHome
        s = VCardField.getFieldValue("ADR", "TYPE", Arrays.asList("HOME"), vcard);
        if((s != null) && !s.isEmpty() && !s.startsWith(";;;")) {
            if(adrHome == null) {
                adrHome = pm.newInstance(PostalAddress.class);
                adrHome.getUsage().add(Addresses.USAGE_HOME);
                adrHome.setMain(Boolean.TRUE);
                this.updatePostalAddress(
                	adrHome, 
                	s, 
                	locale,
                	codeSegment
                );
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	adrHome
                );
                report.add("Create postal address");
            } else {
            	this.updatePostalAddress(
            		adrHome, 
            		s,
            		locale,
            		codeSegment
            	);
                report.add("Update postal address");
            }
        }
        // update adrWork
        s = VCardField.getFieldValue("ADR", "TYPE", Arrays.asList("WORK"), vcard);
        if((s != null) && !s.isEmpty() && !s.startsWith(";;;")) {
            if(adrWork == null) {
            	adrWork = pm.newInstance(PostalAddress.class);
                adrWork.getUsage().add(Addresses.USAGE_BUSINESS);
                adrWork.setMain(Boolean.TRUE);
                this.updatePostalAddress(
                	adrWork, 
                	s, 
                	locale,
                	codeSegment
                );
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	adrWork
                );
                report.add("Create postal address");
            } else {
            	this.updatePostalAddress(
            		adrWork, 
            		s, 
            		locale,
            		codeSegment
            	);
                report.add("Update postal address");
            }
        }
        // update telHomeVoice
        s = VCardField.getFieldValue("TEL", "TYPE", Arrays.asList("HOME", "VOICE"), vcard);
        if((s != null) && !s.isEmpty()) {
            if(telHomeVoice == null) {
                telHomeVoice = pm.newInstance(PhoneNumber.class);
                telHomeVoice.getUsage().add(Addresses.USAGE_HOME);
                telHomeVoice.setMain(Boolean.TRUE);
                this.updatePhoneNumber(telHomeVoice, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	telHomeVoice
                );
                report.add("Create phone number");
            } else {
            	this.updatePhoneNumber(telHomeVoice, s);
                report.add("Update phone number");
            }
        }
        // update telWorkVoice
        s = VCardField.getFieldValue("TEL", "TYPE", Arrays.asList("WORK", "VOICE"), vcard);
        if((s != null) && !s.isEmpty()) {        
            if(telWorkVoice == null) {
            	telWorkVoice = pm.newInstance(PhoneNumber.class);
                telWorkVoice.getUsage().add(Addresses.USAGE_BUSINESS);
                telWorkVoice.setMain(Boolean.TRUE);
                this.updatePhoneNumber(telWorkVoice, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	telWorkVoice
                );
                report.add("Create phone number");
            } else {
            	this.updatePhoneNumber(telWorkVoice, s);
                report.add("Update phone number");
            }
        }
        // update telHomeFax
        s = VCardField.getFieldValue("TEL", "TYPE", Arrays.asList("HOME", "FAX"), vcard);
        if((s != null) && !s.isEmpty()) {                
            if(telHomeFax == null) {
            	telHomeFax = pm.newInstance(PhoneNumber.class);
                telHomeFax.getUsage().add(Addresses.USAGE_HOME_FAX);
                telHomeFax.setMain(Boolean.TRUE);
                this.updatePhoneNumber(telHomeFax, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	telHomeFax
                );
                report.add("Create phone number");
            } else {
            	this.updatePhoneNumber(telHomeFax, s);
                report.add("Update phone number");
            }
        }
        // update telFax
        s = VCardField.getFieldValue("TEL", "TYPE", Arrays.asList("WORK", "FAX"), vcard);
        if((s != null) && !s.isEmpty()) {
            if(telWorkFax == null) {
            	telWorkFax = pm.newInstance(PhoneNumber.class);
                telWorkFax.getUsage().add(Addresses.USAGE_BUSINESS_FAX);
                telWorkFax.setMain(Boolean.TRUE);
                this.updatePhoneNumber(telWorkFax, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	telWorkFax
                );
                report.add("Create phone number");
            } else {
            	this.updatePhoneNumber(telWorkFax, s);
                report.add("Update phone number");
            }
        }
        // update telCellVoice
        s = VCardField.getFieldValue("TEL", "TYPE", Arrays.asList("CELL", "VOICE"), vcard);
        if((s != null) && !s.isEmpty()) {                
            if(telCellVoice == null) {
            	telCellVoice = pm.newInstance(PhoneNumber.class);
                telCellVoice.getUsage().add(Addresses.USAGE_MOBILE);
                telCellVoice.setMain(Boolean.TRUE);
                this.updatePhoneNumber(telCellVoice, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	telCellVoice
                );
                report.add("Create phone number");
            } else {
            	this.updatePhoneNumber(telCellVoice, s);
                report.add("Update phone number");
            }
        }
        // update urlHome
        s = VCardField.getFieldValue("URL", "TYPE", Arrays.asList("HOME"), vcard);
        if((s != null) && !s.isEmpty()) {                
            if(urlHome == null) {
                urlHome = pm.newInstance(WebAddress.class);
                urlHome.getUsage().add(Addresses.USAGE_HOME);
                urlHome.setMain(Boolean.TRUE);
                this.updateWebAddress(urlHome, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	urlHome
                );
                report.add("Create web address");
            } else {
            	this.updateWebAddress(urlHome, s);
                report.add("Update web address");
            }
        }
        // update urlWork
        s = VCardField.getFieldValue("URL", "TYPE", Arrays.asList("WORK"), vcard);
        if((s != null) && !s.isEmpty()) {                
            if(urlWork == null) {
            	urlWork = pm.newInstance(WebAddress.class);
                urlWork.getUsage().add(Addresses.USAGE_BUSINESS);
                urlWork.setMain(Boolean.TRUE);
                this.updateWebAddress(urlWork, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	urlWork
                );
                report.add("Create web address");
            } else {
            	this.updateWebAddress(urlWork, s);
                report.add("Update web address");
            }
        }
        // update emailPrefInternet
    	s = VCardField.getFieldValue("EMAIL", "TYPE", Arrays.asList("WORK"), vcard);
        if(s == null) {
            s = VCardField.getFieldValue("EMAIL", "TYPE", Arrays.asList("PREF", "INTERNET"), vcard);
        }
        if((s != null) && !s.isEmpty()) {                
            if(emailPrefInternet == null) {
                emailPrefInternet = pm.newInstance(EMailAddress.class);
                emailPrefInternet.getUsage().add(Addresses.USAGE_BUSINESS);
                emailPrefInternet.setMain(Boolean.TRUE);
                this.updateEMailAddress(emailPrefInternet, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	emailPrefInternet
                );
                report.add("Create email address");
            } else {
            	this.updateEMailAddress(emailPrefInternet, s);
                report.add("Update email address");
            }
        }
        // update emailInternet
    	s = VCardField.getFieldValue("EMAIL", "TYPE", Arrays.asList("HOME"), vcard);
    	if(s == null) {
    		VCardField field = VCardField.getField("EMAIL", null, null, vcard);
    		if(field != null) {
	    		if(
	    			field.getParameters().get("TYPE") == null || 
	    			field.getParameters().get("TYPE").equals(Arrays.asList("INTERNET"))
	    		) {
	    			s = field.getValue();
	    		}
    		}
    	}
        if((s != null) && !s.isEmpty()) {                
            if(emailInternet == null) {
                emailInternet = pm.newInstance(EMailAddress.class);
                emailInternet.getUsage().add(Addresses.USAGE_HOME);
                emailInternet.setMain(Boolean.TRUE);
                this.updateEMailAddress(emailInternet, s);
                account.addAddress(
                	false,
                	this.getUidAsString(),
                	emailInternet
                );
                report.add("Create email address");
            } else {
            	this.updateEMailAddress(emailInternet, s);
                report.add("Update email address");
            }
        }
        return account;
    }
        
    /**
     * Update account with given vcard.
     * 
     * @param vcardAsString
     * @param vcard
     * @param accountSegment
     * @param locale
     * @param report
     * @return
     * @throws ServiceException
     */
    public Account updateAccount(
    	String vcardAsString,
        Map<String,VCardField> vcard,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment,
        short locale,
        List<String> report
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
        String lookupEmail = VCardField.getFieldValue("EMAIL", "TYPE", Arrays.asList("PREF", "INTERNET"), vcard);
        Account contact = null;
        if((lookupEmail != null) && (lookupEmail.length() > 0)) {
        	SysLog.trace("looking up", lookupEmail);
            EMailAddressQuery addressQuery = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
            addressQuery.identity().like(
            	accountSegment.refGetPath().getDescendant(new String[]{"account", ":*", "address", ":*"}).toXRI()
            );
            addressQuery.thereExistsEmailAddress().equalTo(lookupEmail);
            List<EMailAddress> addresses = accountSegment.getAddress(addressQuery);
            if(!addresses.isEmpty()) {
            	SysLog.trace("address found");
                AccountAddress address = addresses.iterator().next();
                contact = (Account)pm.getObjectById(
                    address.refGetPath().getParent().getParent()
                );
            }
        }
        SysLog.trace("account", contact);
        boolean isNew = contact == null;
        if(isNew) {
            return null;
        }
        return this.importItem(
        	vcardAsString,
            vcard, 
            contact, 
            locale, 
            report
        );
    }

    /**
     * Find account by vcard UID.
     * 
     * @param pm
     * @param accountSegment
     * @param uid
     * @return
     */
    protected Account findAccount(
        PersistenceManager pm,
        org.opencrx.kernel.account1.jmi1.Segment accountSegment,
        String uid
    ) {
        AccountQuery query = (AccountQuery)pm.newQuery(Account.class);
        query.thereExistsExternalLink().equalTo(VCard.VCARD_SCHEMA + uid);
        List<Account> accounts = accountSegment.getAccount(query);
        if(accounts.isEmpty()) {
            query = (AccountQuery)pm.newQuery(Account.class);
            query.thereExistsExternalLink().equalTo(VCard.VCARD_SCHEMA + uid.replace('.', '+'));
            accounts = accountSegment.getAccount(query);
            if(accounts.isEmpty()) {
                return null;
            } else {
                return accounts.iterator().next();
            }
        } else {
            return accounts.iterator().next();
        }
    }

    /**
     * putVCard operation return status.
     *
     */
    public static class PutVCardResult {
    	
    	public enum Status { CREATED, UPDATED, ERROR }
    	
    	public PutVCardResult(
    		Status status,
    		String oldUID,
    		String newUID,
    		Account account
    	) {
    		this.status = status;
    		this.oldUID = oldUID;
    		this.newUID = newUID;
    		this.account = account;
    	}
    	public Status getStatus() {
        	return status;
        }
		public String getOldUID() {
        	return oldUID;
        }
		public String getNewUID() {
        	return newUID;
        }
		public Account getAccount() {
        	return account;
        }
		private final Status status;
    	private final String oldUID;
    	private final String newUID;
    	private final Account account;    	
    }
    
    /**
     * Updates existing or creates new account according to given VCARD.
     * 
     * @param reader
     * @param accountSegment
     * @return
     * @throws ServiceException
     */
    public PutVCardResult putVCard(
    	BufferedReader reader,
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment
    ) throws ServiceException {    
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
    	boolean isTxLocal = !pm.currentTransaction().isActive();
    	PutVCardResult.Status status = PutVCardResult.Status.UPDATED;
    	Account account = null;
    	String cardUID = null;
    	String newUID = null;
    	String l;
        try {
            while((l = reader.readLine()) != null) {
                if(l.toUpperCase().startsWith("BEGIN:VCARD")) {
                    String vcard = "";
                    vcard += "BEGIN:VCARD\n";
                    String cardREV = null;
                    while((l = reader.readLine()) != null) {
                    	// iOS uses for some fields "ITEM<n>." as field prefix
                    	if(l.startsWith("item") || l.startsWith("ITEM")) {
                    		int posEndItemPrefix = l.indexOf(".");
                    		if(posEndItemPrefix > 0) {
                    			l = l.substring(posEndItemPrefix + 1);
                    		}
                    	}
                        vcard += l;
                        vcard += "\n";
                        if(l.startsWith("UID:")) {
                            cardUID = l.substring(4);
                        }
                        else if(l.startsWith("REV:")) {
                        	cardREV = l.substring(4);
                        }
                        else if(l.startsWith("END:VCARD")) {
                            break;
                        }
                    }
                    SysLog.trace("VCARD", vcard);
	                newUID = cardUID;
	                if((cardUID != null) && (cardREV != null)) {
                    	SysLog.detail("Lookup account", cardUID);
                        account = this.findAccount(
                            pm,
                            accountSegment, 
                            cardUID
                        );
	                    StringBuilder dummy = new StringBuilder();
	                    Map<String,VCardField> newCard = new HashMap<String,VCardField>();
	                    try {
	                    	newCard = VCard.getInstance().parseVCard(
	                            new BufferedReader(new StringReader(vcard.toString())),
	                            dummy 
	                        );
	                    } catch(Exception e) {}
	                    newCard.remove("LAST-MODIFIED");
	                    newCard.remove("DTSTAMP");                               
	                    newCard.remove("CREATED"); 
	                    newCard.remove("REV");
	                    newCard.remove("PRODID");
	                    newCard.remove("VERSION");
	                    dummy.setLength(0);
	                    Map<String,VCardField> oldCard = null;
	                    if(account == null) {
	                    	// Ignore supplied UID in case of a new account 
	                    	// This way we prevent duplicate UIDs
	                    	int pos1 = vcard.indexOf("UID:");
	                    	if(pos1 > 0) {
	                    		int pos2 = vcard.indexOf("\n", pos1);
	                    		if(pos2 > pos1) {
	                    			newUID = Base.getInstance().getUidAsString();
	                    			vcard =
	                    				vcard.substring(0, pos1) + 
	                    				"UID:" + newUID + "\n" +
	                    				vcard.substring(pos2 + 1);		                    		
	                    		}
	                    	}
                            if(account == null) {
                            	if(!pm.currentTransaction().isActive()) {
                            		pm.currentTransaction().begin();
                            	}
                            	account = pm.newInstance(Contact.class);
                            	accountSegment.addAccount(
                            		Base.getInstance().getUidAsString(),
                            		account
                            	);
                            }
                            status = PutVCardResult.Status.CREATED;
	                    } else {
	                    	try {
	                            oldCard = this.parseVCard(
	                                new BufferedReader(new StringReader(account.getVcard())),
	                                dummy
	                            );
	                    	} catch(Exception ignore) {}
	                        oldCard.remove("LAST-MODIFIED");
	                        oldCard.remove("DTSTAMP");                                   
	                        oldCard.remove("CREATED");          
	                        oldCard.remove("REV");
	                        oldCard.remove("PRODID");
	                        oldCard.remove("VERSION");
	                        oldCard.keySet().retainAll(newCard.keySet());
	                    }
                    	if(!newCard.equals(oldCard)) {
	                        try {
                            	if(!pm.currentTransaction().isActive()) {
                            		pm.currentTransaction().begin();
                            	}
                                ImportParams importItemParams = Structures.create(
                                	ImportParams.class, 
                                	Datatypes.member(ImportParams.Member.item, vcard.toString().getBytes("UTF-8")),
                                	Datatypes.member(ImportParams.Member.itemMimeType, VCard.MIME_TYPE),                                	
                                	Datatypes.member(ImportParams.Member.itemName, "import.vcf"),                                	
                                	Datatypes.member(ImportParams.Member.locale, (short)0)                       	
                                );
                                account.importItem(importItemParams);
                                if(isTxLocal) {
	                                pm.currentTransaction().commit();
		                            pm.refresh(account);
                                }
                        	} catch(Exception e) {
	                        	new ServiceException(e).log();
	                        	status = PutVCardResult.Status.ERROR;
	                            try {
	                                pm.currentTransaction().rollback();
	                            } catch(Exception ignore) {}                                    
	                        }
                    	}
	                }
	            }
	        }
        } catch (IOException e) {
        	status = PutVCardResult.Status.ERROR;
        	new ServiceException(e).log();
        }
        return new PutVCardResult(
        	status,
        	cardUID,
        	newUID,
        	account
        );
    }

    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final Map<Integer,String> salutations = new HashMap<Integer,String>();
    
    static {
    	salutations.put(0, "NA");
    	salutations.put(1, "Mr.");
    	salutations.put(2, "Mrs.");
    	salutations.put(3, "Miss");
    	salutations.put(4, "Mr. and Mrs.");
    	salutations.put(5, "Company");
    	salutations.put(6, "Brothers");
    	salutations.put(7, "Prof.");
    	salutations.put(8, "Dr.");
    	salutations.put(9, "Family");    	
    }
    
    public static final String DATETIME_FORMAT =  "yyyyMMdd'T'HHmmss";
    public static final String MIME_TYPE = "text/x-vcard";
    public static final String FILE_EXTENSION = ".vcf";
    public static final String PROD_ID = "//OPENCRX//V3//EN";
    
    public static final int MIME_TYPE_CODE = 3;
    public static final short DEFAULT_LOCALE = 0;
    public final static String VCARD_SCHEMA = "VCARD:";    

}

