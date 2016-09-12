/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MimeUtils
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
package org.opencrx.kernel.utils;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.mail.Header;
import javax.mail.Message.RecipientType;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.Part;
import javax.mail.Session;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.InternetHeaders;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimeUtility;

import org.opencrx.application.uses.com.auxilii.msgparser.MsgParser;
import org.opencrx.application.uses.com.auxilii.msgparser.RecipientEntry;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.cci2.EMailAddressQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Addresses.EMailType;
import org.opencrx.kernel.text.RTFToText;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.StringInputStream;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.base.text.conversion.Base64;

public abstract class MimeUtils {

	public static class MimeMessageImpl extends MimeMessage {

	    //-----------------------------------------------------------------------
	    public MimeMessageImpl(
	    ) {
	        super((Session)null);
	    }
	    
	    //-----------------------------------------------------------------------
	    public MimeMessageImpl(
	       InputStream is
	    ) throws MessagingException {
	        super(null, is);
	    }
	    
	    //-----------------------------------------------------------------------
	    public MimeMessageImpl(
	        MimeMessage message
	    ) throws MessagingException {
	        super(message);
	    }
	    
	    //-----------------------------------------------------------------------
	    public long getUid(
	    ) {
	        return this.uid;
	    }

	    //-----------------------------------------------------------------------
	    public void setUid(
	        long uid
	    ) {
	        this.uid = uid;
	    }

	    //-----------------------------------------------------------------------
	    public int getMessageNumber(
	    ) {
	        return this.messageNumber;
	    }

	    //-----------------------------------------------------------------------
	    public void setMessageNumber(
	        int messageNumber
	    ) {
	        this.messageNumber = messageNumber;
	    }

	    //-----------------------------------------------------------------------
	    // Member
	    
	    protected long uid;
	    protected int messageNumber;
	    
	}
	
    //-----------------------------------------------------------------------    
    @SuppressWarnings("unchecked")
    public static String getHeadersAsRFC822(
        Part part,
        String[] fields
    ) throws MessagingException {
        if(fields == null) {
            List<String> headerNames = new ArrayList<String>();
            Enumeration<Header> allHeaders = part.getAllHeaders();
            while(allHeaders.hasMoreElements()) {
                headerNames.add(
                    allHeaders.nextElement().getName()
                );
            }
            return getHeadersAsRFC822(
                part,
                headerNames.toArray(new String[headerNames.size()])
            );
        }
        else {
            StringBuilder header = new StringBuilder();
            for(String field: fields) {
                String[] values = part.getHeader(field);
                if(values != null) {
                    for(String value: values) {
                        header.append(field).append(": ").append(value).append("\r\n");
                    }
                }
            }
            return header.toString(); 
        }
    }

    /**
     * Test whether string contains ASCII chars only.
     * 
     * @param s
     * @return
     */
    public static boolean isAllAscii(
        String s
    ) {
        int nonAscii = 0;
        int l = s.length();
        for(int i = 0; i < l; i++) {
            char c = s.charAt(i);
            boolean isNonAscii = (c >= 0177) || (c < 040 && c != '\r' && c != '\n' && c != '\t');
            if(isNonAscii) {
                nonAscii++;
            }
        }        
        return nonAscii == 0;
    }
    
    /**
     * Map X500 to SMTP address.
     * 
     * @param x500Address
     * @param accountSegment
     * @param addressMap
     * @return
     * @throws ServiceException
     */
    private static String mapX500ToSMTPAddress(
    	String x500Address,
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment,
    	Map<String,String> addressMap
    ) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
    	// Try to find X500 address
    	if(accountSegment != null) {
    		AccountQuery accountQuery = (AccountQuery)pm.newQuery(Account.class);
    		EMailAddressQuery emailAddressQuery = (EMailAddressQuery)pm.newQuery(EMailAddress.class);
    		emailAddressQuery.emailType().equalTo(EMailType.X500.getValue());
    		emailAddressQuery.thereExistsEmailAddress().like("(?i)" + x500Address);
    		accountQuery.thereExistsAddress().elementOf(PersistenceHelper.asSubquery(emailAddressQuery));
    		List<Account> accounts = accountSegment.getAccount(accountQuery);
    		if(accounts.size() == 1) {
    			AccountAddress[] addresses = Accounts.getInstance().getMainAddresses(accounts.iterator().next());
    			if(
    				addresses[Accounts.MAIL_BUSINESS] != null && 
    				((EMailAddress)addresses[Accounts.MAIL_BUSINESS]).getEmailType() == EMailType.SMTP.getValue()
    			) {
    				return ((EMailAddress)addresses[Accounts.MAIL_BUSINESS]).getEmailAddress();
    			}
    		}
    	}
    	return addressMap.containsKey(x500Address) 
    		? addressMap.get(x500Address)
    		: null;
    }

    /**
     * Return true if smtp address is valid and exists. 
     * 
     * @param accountSegment
     * @param validate
     * @param smtpAddress
     * @return
     * @throws ServiceException
     */
    private static boolean smtpAddressIsValid(
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment,
    	String smtpAddress,
    	boolean validate
    ) throws ServiceException {
    	if(smtpAddress == null) {
    		return false;
    	} else {
	    	if(validate) {
	    		if(accountSegment == null) {
	    			return false;
	    		} else {
					PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
					List<EMailAddress> emailAddresses = Accounts.getInstance().lookupEmailAddress(
						pm, 
						accountSegment.refGetPath().get(2), 
						accountSegment.refGetPath().get(4), 
						smtpAddress
					);
					return !emailAddresses.isEmpty();
	    		}
	    	} else {
	    		return true;
	    	}
    	}
    }

	/**
	 * Map MSG to MIME. X.500 addresses are mapped to SMTP addresses.
	 * 
	 * @param msgStream
	 * @param accountSegment
	 * @param addressMap
	 * @param validateAddresses if true, validates whether the mapped SMTP address exists 
	 *        in account segment.
	 * @param errors
	 * @return mapped MimeMessage or null in case of errors.
	 */
	public static MimeMessage mapMsgToMime(
		InputStream msgStream,
		org.opencrx.kernel.account1.jmi1.Segment accountSegment,
		Map<String,String> addressMap,
		boolean validateAddresses,
		List<String> errors
	) {
		MimeMessage mimeMessage = new MimeMessageImpl();
        Multipart mimeMultipart = new MimeMultipart();
		try {
			org.opencrx.application.uses.com.auxilii.msgparser.Message msg = new MsgParser().parseMsg(msgStream, true);
			// Sent Date
			{
				if(msg.getDate() != null) {
					mimeMessage.setSentDate(msg.getDate());
				}
			}
			// From
			try {
				String emailAddress = msg.getFromEmail();
				String smtpAddress = null;
				if(emailAddress.startsWith("/")) {
					smtpAddress = mapX500ToSMTPAddress(emailAddress, accountSegment, addressMap);
				} else {
					smtpAddress = emailAddress;
				}
				if(smtpAddressIsValid(accountSegment, smtpAddress, validateAddresses)) {
					mimeMessage.setFrom(
						new InternetAddress(smtpAddress, msg.getFromName())
					);
				} else {
					errors.add(emailAddress);
				}
			} catch(Exception ignore) {}
			// Subject
			{
				mimeMessage.setSubject(
					msg.getSubject(), 
					"UTF-8"
				);
			}
			// Recipients
			for(RecipientEntry recipient: msg.getRecipients()) {
				String emailAddress = recipient.getToEmail();
				String smtpAddress = null;
				if(emailAddress.startsWith("/")) {
					smtpAddress = mapX500ToSMTPAddress(emailAddress, accountSegment, addressMap);
				} else {
					smtpAddress = emailAddress;
				}
				if(smtpAddressIsValid(accountSegment, smtpAddress, validateAddresses)) {
					mimeMessage.addRecipient(
						RecipientType.TO,
						new InternetAddress(smtpAddress, recipient.getToName())
					);
				} else {
					errors.add(emailAddress);
				}
			}
			// Message-ID
			{
				if(msg.getMessageId() != null) {
					mimeMessage.addHeader("Message-ID", msg.getMessageId());
				}
			}
			// Body
			{
		        MimeBodyPart messageBodyPart = new MimeBodyPart();
		        if(msg.getBodyText() != null) {
		        	messageBodyPart.setText(msg.getBodyText());
		        } else if(msg.getBodyRTF() != null) {
		        	messageBodyPart.setText(
		        		RTFToText.toTextAsString(new StringInputStream(msg.getBodyRTF()))
		        	);
		        } else {
		        	messageBodyPart.setText("");		        	
		        }
		        mimeMultipart.addBodyPart(messageBodyPart);
			}
			// Attachments
			{
				for(org.opencrx.application.uses.com.auxilii.msgparser.attachment.Attachment attachment: msg.getAttachments()) {
					if(attachment instanceof org.opencrx.application.uses.com.auxilii.msgparser.attachment.FileAttachment) {
						org.opencrx.application.uses.com.auxilii.msgparser.attachment.FileAttachment fileAttachment = (org.opencrx.application.uses.com.auxilii.msgparser.attachment.FileAttachment)attachment;
	                    InternetHeaders headers = new InternetHeaders();
	                	headers.addHeader("Content-Type", (fileAttachment.getMimeTag() == null ? "application/octet-stream" : fileAttachment.getMimeTag()) + "; name=\"" + MimeUtility.encodeText(fileAttachment.getFilename()) + "\"");
	                    headers.addHeader("Content-Disposition", "attachment");
	                    headers.addHeader("Content-Transfer-Encoding", "base64");
	                    MimeBodyPart messageBodyPart = new MimeBodyPart(
	                        headers,
	                        Base64.encode(fileAttachment.getData(), 0, (int)fileAttachment.getSize()).getBytes("US-ASCII")
	                    );
	                    mimeMultipart.addBodyPart(messageBodyPart);
					}
				}
			}
			// Complete
	        mimeMessage.setContent(mimeMultipart);
		} catch(Exception e) {
			ServiceException e0 = new ServiceException(e);
			e0.log();
			errors.add(e0.getMessage());
		}
		return errors.isEmpty() ? mimeMessage : null;
	}

    /**
     * Parse mime content type.
     * 
     * @param contentType
     * @return
     */
    public static String[] parseContentType(
        String contentType
    ) {
        String[] result = new String[2];
        contentType = contentType.replace("\t", " ");
        contentType = contentType.replace("\r\n", "");
        Pattern pattern = Pattern.compile("([0-9a-zA-Z/\\+\\-\\.]+)(?:;(?:[ \\r\\n\\t]*)name(?:[^\\=]*)=[\\\"]?([.[^\\\"]]*)[\\\"]?)?");
        Matcher matcher = pattern.matcher(contentType);
        if(matcher.find()) {
            result[0] = matcher.group(1);
            try {
            	String name= matcher.group(2);
            	result[1] = name == null ? null : MimeUtility.decodeText(name);
            } catch(Exception e) {
            	result[1] = matcher.group(2);            	
            }
        } else {
            result[0] = contentType;
            result[1] = null;
        }
        return result;
    }

    //-----------------------------------------------------------------------
	// Members
    //-----------------------------------------------------------------------    
    public static final String[] STANDARD_HEADER_FIELDS = new String[]{
        "Return-Path", "From", "To", "Cc", "Bcc", "Subject", "Date", "Message-ID", "MIME-Version", "Content-Type", "In-Reply-To"
    };

}
