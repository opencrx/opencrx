/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MimeUtils
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
package org.opencrx.kernel.utils;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jakarta.mail.Header;
import jakarta.mail.MessagingException;
import jakarta.mail.Part;
import jakarta.mail.Session;
import jakarta.mail.internet.MimeMessage;
import jakarta.mail.internet.MimeUtility;

public abstract class MimeUtils {

	/**
	 * MimeMessageImpl
	 *
	 */
	public static class MimeMessageImpl extends MimeMessage {

	    public MimeMessageImpl(
	    ) {
	        super((Session)null);
	    }
	    
	    public MimeMessageImpl(
	       InputStream is
	    ) throws MessagingException {
	        super(null, is);
	    }
	    
	    public MimeMessageImpl(
	        MimeMessage message
	    ) throws MessagingException {
	        super(message);
	    }

	    public long getUid(
	    ) {
	        return this.uid;
	    }

	    public void setUid(
	        long uid
	    ) {
	        this.uid = uid;
	    }

	    /* (non-Javadoc)
	     * @see jakarta.mail.Message#getMessageNumber()
	     */
	    @Override
	    public int getMessageNumber(
	    ) {
	        return this.messageNumber;
	    }

	    /* (non-Javadoc)
	     * @see jakarta.mail.Message#setMessageNumber(int)
	     */
	    @Override
	    public void setMessageNumber(
	        int messageNumber
	    ) {
	        this.messageNumber = messageNumber;
	    }

	    /* (non-Javadoc)
	     * @see jakarta.mail.internet.MimeMessage#getHeader(java.lang.String, java.lang.String)
	     */
	    @Override
	    public String getHeader(
	    	String name,
	    	String delimiter
	    ) throws MessagingException {
	    	String h = this.headers.getHeader(name, delimiter);
	    	if (h != null) {
	    		return h.replace('\r', ' ');
	    	} else {
	    		return h;
	    	}
	    }

	    //-----------------------------------------------------------------------
	    // Member
	    //-----------------------------------------------------------------------	    
	    protected long uid;
	    protected int messageNumber;
	    
	}
	
    /**
     * Get headers in RFC822 format.
     * 
     * @param part
     * @param fields
     * @return
     * @throws MessagingException
     */
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
        } else {
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
