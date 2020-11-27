/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Addresses
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2011, CRIXP Corp., Switzerland
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

import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;

import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Codes;

import com.google.i18n.phonenumbers.PhoneNumberUtil;
import com.google.i18n.phonenumbers.PhoneNumberUtil.PhoneNumberFormat;

public class Addresses extends AbstractImpl {

    //-------------------------------------------------------------------------
	public static void register(
	) {
		registerImpl(new Addresses());
	}
	
    //-------------------------------------------------------------------------
	public static Addresses getInstance(
	) throws ServiceException {
		return getInstance(Addresses.class);
	}

	//-------------------------------------------------------------------------
	protected Addresses(
	) {
		
	}
	
    /**
     * Get short and long text for postal country code in locale = 0 (en_US)
     * 
     * @param countryCode
     * @param codeSegment
     * @return array with country long text at position 0 and short text at position 1. The array is of
     *   	   length 1 if no short text is available. Returns null in case countryCode can not be mapped.
     * @throws ServiceException
     */
    public String[] mapToPostalCountryText(
        short countryCode,
        org.opencrx.kernel.code1.jmi1.Segment codeSegment        
    ) throws ServiceException {
    	return this.mapToPostalCountryText(
    		countryCode, 
    		(short)0, 
    		codeSegment
    	);
    }

    /**
     * Get short and long text for postal country code.
     * 
     * @param countryCode
     * @param locale
     * @param codeSegment
     * @return array with country long text at position 0 and short text at position 1. The array is of
     *   	   length 1 if no short text is available. Returns null in case countryCode can not be mapped.
     * @throws ServiceException
     */
    public String[] mapToPostalCountryText(
        short countryCode,
        short locale,
        org.opencrx.kernel.code1.jmi1.Segment codeSegment        
    ) throws ServiceException {
    	if(codeSegment != null) {
    		try {
		    	Codes codes = new Codes(codeSegment);
		    	String longTextCountry = (String)codes.getLongText(
		    		"country", 
		    		locale, 
		    		true, // codeAsKey 
		    		true // includeAll
		    	).get(countryCode);
		    	if(locale == 0 && longTextCountry != null) {
		    		POSTAL_COUNTRIES_BY_CODE.put(
		    			countryCode,
		    			longTextCountry
		    		);
		    	}
		    	String shortTextCountry = (String)codes.getShortText(
		    		"country", 
		    		locale, 
		    		true, // codeAsKey 
		    		true // includeAll
		    	).get(countryCode);
		    	return new String[]{longTextCountry, shortTextCountry};
    		} catch(Exception e) {}    		
    	} else {
        	String longTextCountry = POSTAL_COUNTRIES_BY_CODE.get(Short.valueOf(countryCode));
        	if(longTextCountry != null) {
        		return new String[]{longTextCountry};
        	}    		
    	}
    	return null;
    }

    //-----------------------------------------------------------------------
    public short mapToPostalCountryCode(    	
        String country,
        org.opencrx.kernel.code1.jmi1.Segment codeSegment
    ) throws ServiceException {    	
    	{
	    	country = country.toUpperCase();
	    	Integer lastMatchLength = null;
	    	Short countryCode = null;
	    	for(Entry<String,Short> entry: POSTAL_COUNTRIES_BY_TEXT.entrySet()) {
	    		if(entry.getKey().toUpperCase().indexOf(country) >= 0) {
	    			if(lastMatchLength == null || entry.getKey().length() < lastMatchLength) {
	    				countryCode = entry.getValue();
	    				lastMatchLength = entry.getKey().length();
	    			}
	    		}
	    	}
	    	if(countryCode != null) {
	    		return countryCode;
	    	}
    	}
    	if(codeSegment != null) {
    		try {
		    	Codes codes = new Codes(codeSegment);
		    	short countryCode = codes.findCodeFromValue(
		    		country, 
		    		"country" 
		    	);
		    	POSTAL_COUNTRIES_BY_TEXT.put(
		    		country,
		    		countryCode
		    	);
		    	return countryCode;
    		} catch(Exception e) {
    			SysLog.warning("Unable to map country. Exception occured", e);
    		}
    	}
    	SysLog.warning("Unable to map country. Country not found", country);
    	return 0;
    }

    //-------------------------------------------------------------------------
    public Integer mapToPhoneCountryPrefix(
        int countryCode
    ) throws ServiceException {
    	return PHONE_COUNTRIES.get(countryCode);
    }

    //-----------------------------------------------------------------------
    public int mapToPhoneCountryCode(
        int prefix
    ) throws ServiceException {
    	for(Entry<Short,Integer> entry: PHONE_COUNTRIES.entrySet()) {
    		if(entry.getValue() == prefix) {
    			return entry.getKey().shortValue();
    		}
    	}
    	return 0;
    }

    /**
     * Update derived phone number fields in case automaticParsing=true.
     * 
     * @param phoneNumber
     * @throws ServiceException
     */
    public void updatePhoneNumber(
    	org.opencrx.kernel.address1.jmi1.PhoneNumberAddressable phoneNumber
    ) throws ServiceException {
        String phoneNumberFull = phoneNumber.getPhoneNumberFull();
        boolean automaticParsing = phoneNumber.isAutomaticParsing();
        if(automaticParsing && (phoneNumberFull != null)) {
        	// Normalize phone number to PhoneNumberFormat.INTERNATIONAL
        	try {
            	PhoneNumberUtil phoneUtil = PhoneNumberUtil.getInstance();        		
        		com.google.i18n.phonenumbers.Phonenumber.PhoneNumber phoneNumberProto = phoneUtil.parse(phoneNumberFull, null);
        		phoneNumberFull = phoneUtil.format(phoneNumberProto, PhoneNumberFormat.INTERNATIONAL);
        		phoneNumber.setPhoneNumberFull(phoneNumberFull);
        	} catch (Exception ignore) {}
        	String countryCode = "0";
        	String areaCode = "";
        	String localNumber = "";
        	String extension= "";
        	// Format +nn nnnnnnnnnnnnnn
        	String[] parts0 = phoneNumberFull.split(" ");
        	if(parts0.length == 2 && parts0[0].startsWith("+")) {
        		countryCode = parts0[0].substring(1).trim();
                if("1".equals(countryCode)) {
                    countryCode = "840";
                } else {
                    Integer code = null;
                    try {
                    	code = this.mapToPhoneCountryCode(Short.valueOf(countryCode));
                    } catch(Exception e) {}
                    countryCode = code == null ? 
                    	"0" : 
                    	code.toString();
                }
                if(parts0[1].length() > 9) {
	                areaCode = parts0[1].substring(0, 3);
	                localNumber = parts0[1].substring(3);
                } else if(parts0[1].length() > 2) {
	                areaCode = parts0[1].substring(0, 2);
	                localNumber = parts0[1].substring(2);                	
                } else {
	                areaCode = "";
	                localNumber = parts0[1];
                }
                localNumber = localNumber.trim();
                if(localNumber.startsWith("-")) {
                	localNumber = localNumber.substring(1);
                }
                extension = "";
        	} else if(parts0.length >= 3 && parts0[0].startsWith("+")) {
             	// Format +nn nnn nnn nn nn
        		countryCode = parts0[0].substring(1).trim();
                if("1".equals(countryCode)) {
                    countryCode = "840";
                } else {
                    Integer code = null;
                    try {
                    	code = this.mapToPhoneCountryCode(Short.valueOf(countryCode));
                    } catch(Exception e) {}
                    countryCode = code == null ? 
                    	"0" : 
                    	code.toString();
                }
                areaCode = parts0[1];
                localNumber = "";
                String sep = "";
                for(int i = 2; i < parts0.length; i++) {
                	localNumber += sep;
                	localNumber += parts0[i];
                	sep = " ";
                }
                extension = "";                
        	} else {
                // Format +nn (nnn) nnn-nnnn x nnn
	            List<String> parts1 = new ArrayList<String>();
	            StringTokenizer tokenizer = new StringTokenizer(phoneNumberFull, "/+()x");
	            while(tokenizer.hasMoreTokens()) {
	                parts1.add(tokenizer.nextToken());
	            }
	            if(parts1.size() >= 3) {
	                countryCode = parts1.get(0).toString().trim();
	                if("1".equals(countryCode)) {
	                    countryCode = "840";
	                } else {
	                    Integer code = null;
	                    try {
	                    	code = this.mapToPhoneCountryCode(Short.valueOf(countryCode));
	                    } catch(Exception e) {}
	                    countryCode = code == null ? 
	                    	"0" : 
	                    	code.toString();
	                }
	                areaCode = parts1.get(1).toString().trim();
	                localNumber = parts1.get(2).toString().trim();
	                extension = parts1.size() >= 4 ? parts1.get(3).toString().trim() : "";
	            }
        	}
        	// Only touch fields if modified
        	if(Short.valueOf(countryCode) != phoneNumber.getPhoneCountryPrefix()) {
        		phoneNumber.setPhoneCountryPrefix(Short.valueOf(countryCode));
        	}
        	if(!areaCode.equals(phoneNumber.getPhoneCityArea())) {
        		phoneNumber.setPhoneCityArea(areaCode);
        	}
        	if(!localNumber.equals(phoneNumber.getPhoneLocalNumber())) {
        		phoneNumber.setPhoneLocalNumber(localNumber);
        	}
        	if(!extension.equals(phoneNumber.getPhoneExtension())) {
        		phoneNumber.setPhoneExtension(extension);
        	}
        }
    }

    /**
     * Trim and normalize e-mail address.
     * 
     * @param emailAddress
     * @throws ServiceException
     */
    public void updateEMailAddress(
    	org.opencrx.kernel.address1.jmi1.EMailAddressable emailAddress
    ) throws ServiceException {
    	if(emailAddress.getEmailAddress() != null) {
    		if(!Utils.areEqual(emailAddress.getEmailAddress(), emailAddress.getEmailAddress().trim())) {
    			emailAddress.setEmailAddress(emailAddress.getEmailAddress().trim());
    		}
    	}
    }

    /**
     * parse uri reference.
     * 
     * @param uriAddress
     * @throws ServiceException
     */
    public void updateUriAddress(
    	org.opencrx.kernel.address1.jmi1.UriAddressable uriAddress
    ) throws ServiceException {
    	if(uriAddress.getUriReference() != null) {
    		if(Boolean.TRUE.equals(uriAddress.isAutomaticParsing())) {
    			try {
    				URI uri = new URI(uriAddress.getUriReference());
    				uriAddress.setUriAuthority(uri.getAuthority());
    				uriAddress.setUriFragment(uri.getFragment());
    				uriAddress.setUriHost(uri.getHost());
    				uriAddress.setUriPath(uri.getPath());
    				uriAddress.setUriPort(Integer.toString(uri.getPort()));
    				uriAddress.setUriQuery(uri.getQuery());
    				uriAddress.setUriScheme(uri.getScheme());
    				uriAddress.setUriSchemeSpecificPart(uri.getSchemeSpecificPart());
    				uriAddress.setUriUserInfo(uri.getRawUserInfo());
    			} catch(Exception e) {
    				ServiceException se = new ServiceException(e);
    				SysLog.log(Level.FINE, "Invalid URI {0}", uriAddress, se);
    			}
    		}
    	}
    }

	/**
	 * EMail types.
	 */
	public enum EMailType {
		
		NA((short)0),
		SMTP((short)1),
		X500((short)2);
		
		private short value;
		
		private EMailType(
			short value
		) {
			this.value = value;
		}
		
		public short getValue(
		) {
			return this.value;
		}
		
	}
    
    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final String[] ADDRESS_TYPES = 
        new String[]{
            "org:opencrx:kernel:address1:PostalAddressable",
            "org:opencrx:kernel:address1:PhoneNumberAddressable",
            "org:opencrx:kernel:address1:EMailAddressable",
            "org:opencrx:kernel:address1:WebAddressable",
            "org:opencrx:kernel:address1:RoomAddressable"
        };

    public static final Map<Short,String> POSTAL_COUNTRIES_BY_CODE = new ConcurrentHashMap<Short,String>();
    private static final Map<String,Short> POSTAL_COUNTRIES_BY_TEXT = new ConcurrentHashMap<String,Short>(); 
    public static final Map<Short,Integer> PHONE_COUNTRIES = new HashMap<Short,Integer>();
    
    static {
		POSTAL_COUNTRIES_BY_CODE.put((short)0, "--none--");
		POSTAL_COUNTRIES_BY_CODE.put((short)4, "Afghanistan");
		POSTAL_COUNTRIES_BY_CODE.put((short)8, "Albania");
		POSTAL_COUNTRIES_BY_CODE.put((short)12, "Algeria");
		POSTAL_COUNTRIES_BY_CODE.put((short)16, "American Samoa");
		POSTAL_COUNTRIES_BY_CODE.put((short)20, "Andorra");
		POSTAL_COUNTRIES_BY_CODE.put((short)24, "Angola");
		POSTAL_COUNTRIES_BY_CODE.put((short)660, "Anguilla");
		POSTAL_COUNTRIES_BY_CODE.put((short)10, "Antarctica");
		POSTAL_COUNTRIES_BY_CODE.put((short)28, "Antigua and Barbuda");
		POSTAL_COUNTRIES_BY_CODE.put((short)32, "Argentina");
		POSTAL_COUNTRIES_BY_CODE.put((short)51, "Armenia");
		POSTAL_COUNTRIES_BY_CODE.put((short)533, "Aruba");
		POSTAL_COUNTRIES_BY_CODE.put((short)36, "Australia");
		POSTAL_COUNTRIES_BY_CODE.put((short)40, "Austria");
		POSTAL_COUNTRIES_BY_CODE.put((short)31, "Azerbaijan");
		POSTAL_COUNTRIES_BY_CODE.put((short)44, "Bahamas");
		POSTAL_COUNTRIES_BY_CODE.put((short)48, "Bahrain");
		POSTAL_COUNTRIES_BY_CODE.put((short)50, "Bangladesh");
		POSTAL_COUNTRIES_BY_CODE.put((short)52, "Barbados");
		POSTAL_COUNTRIES_BY_CODE.put((short)112, "Belarus");
		POSTAL_COUNTRIES_BY_CODE.put((short)56, "Belgium");
		POSTAL_COUNTRIES_BY_CODE.put((short)84, "Belize");
		POSTAL_COUNTRIES_BY_CODE.put((short)204, "Benin");
		POSTAL_COUNTRIES_BY_CODE.put((short)60, "Bermuda");
		POSTAL_COUNTRIES_BY_CODE.put((short)64, "Bhutan");
		POSTAL_COUNTRIES_BY_CODE.put((short)68, "Bolivia");
		POSTAL_COUNTRIES_BY_CODE.put((short)70, "Bosnia and Herzegovina");
		POSTAL_COUNTRIES_BY_CODE.put((short)72, "Botswana");
		POSTAL_COUNTRIES_BY_CODE.put((short)74, "Bouvet Island");
		POSTAL_COUNTRIES_BY_CODE.put((short)76, "Brazil");
		POSTAL_COUNTRIES_BY_CODE.put((short)86, "British Indian Ocean Territory");
		POSTAL_COUNTRIES_BY_CODE.put((short)96, "Brunei Darussalam");
		POSTAL_COUNTRIES_BY_CODE.put((short)100, "Bulgaria");
		POSTAL_COUNTRIES_BY_CODE.put((short)854, "Burkina Faso");
		POSTAL_COUNTRIES_BY_CODE.put((short)108, "Burundi");
		POSTAL_COUNTRIES_BY_CODE.put((short)116, "Cambodia");
		POSTAL_COUNTRIES_BY_CODE.put((short)120, "Cameroon");
		POSTAL_COUNTRIES_BY_CODE.put((short)124, "Canada");
		POSTAL_COUNTRIES_BY_CODE.put((short)132, "Cape Verde");
		POSTAL_COUNTRIES_BY_CODE.put((short)136, "Cayman Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)140, "Central African Republic");
		POSTAL_COUNTRIES_BY_CODE.put((short)148, "Chad");
		POSTAL_COUNTRIES_BY_CODE.put((short)152, "Chile");
		POSTAL_COUNTRIES_BY_CODE.put((short)156, "China");
		POSTAL_COUNTRIES_BY_CODE.put((short)162, "Christmas Island");
		POSTAL_COUNTRIES_BY_CODE.put((short)166, "Cocos (Keeling) Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)170, "Colombia");
		POSTAL_COUNTRIES_BY_CODE.put((short)174, "Comoros");
		POSTAL_COUNTRIES_BY_CODE.put((short)178, "Congo");
		POSTAL_COUNTRIES_BY_CODE.put((short)180, "Congo, The Democratic Replublic of the");
		POSTAL_COUNTRIES_BY_CODE.put((short)184, "Cook Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)188, "Costa Rica");
		POSTAL_COUNTRIES_BY_CODE.put((short)384, "Côte d'Ivoire");
		POSTAL_COUNTRIES_BY_CODE.put((short)191, "Croatia");
		POSTAL_COUNTRIES_BY_CODE.put((short)192, "Cuba");
		POSTAL_COUNTRIES_BY_CODE.put((short)196, "Cyprus");
		POSTAL_COUNTRIES_BY_CODE.put((short)203, "Czech Republic");
		POSTAL_COUNTRIES_BY_CODE.put((short)208, "Denmark");
		POSTAL_COUNTRIES_BY_CODE.put((short)262, "Djibouti");
		POSTAL_COUNTRIES_BY_CODE.put((short)212, "Dominica");
		POSTAL_COUNTRIES_BY_CODE.put((short)214, "Dominican Republic");
		POSTAL_COUNTRIES_BY_CODE.put((short)626, "East Timor");
		POSTAL_COUNTRIES_BY_CODE.put((short)218, "Ecuador");
		POSTAL_COUNTRIES_BY_CODE.put((short)818, "Egypt");
		POSTAL_COUNTRIES_BY_CODE.put((short)222, "El Salvador");
		POSTAL_COUNTRIES_BY_CODE.put((short)226, "Equatorial Guinea");
		POSTAL_COUNTRIES_BY_CODE.put((short)232, "Eritrea");
		POSTAL_COUNTRIES_BY_CODE.put((short)233, "Estonia");
		POSTAL_COUNTRIES_BY_CODE.put((short)231, "Ethiopia");
		POSTAL_COUNTRIES_BY_CODE.put((short)238, "Falkland Islands (Islas Malvinas)");
		POSTAL_COUNTRIES_BY_CODE.put((short)234, "Faroe Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)242, "Fiji");
		POSTAL_COUNTRIES_BY_CODE.put((short)246, "Finland");
		POSTAL_COUNTRIES_BY_CODE.put((short)249, "France");
		POSTAL_COUNTRIES_BY_CODE.put((short)254, "French Guiana");
		POSTAL_COUNTRIES_BY_CODE.put((short)258, "French Polynesia");
		POSTAL_COUNTRIES_BY_CODE.put((short)260, "French Southern Territories");
		POSTAL_COUNTRIES_BY_CODE.put((short)266, "Gabon");
		POSTAL_COUNTRIES_BY_CODE.put((short)270, "Gambia");
		POSTAL_COUNTRIES_BY_CODE.put((short)268, "Georgia");
		POSTAL_COUNTRIES_BY_CODE.put((short)276, "Germany");
		POSTAL_COUNTRIES_BY_CODE.put((short)288, "Ghana");
		POSTAL_COUNTRIES_BY_CODE.put((short)292, "Gibraltar");
		POSTAL_COUNTRIES_BY_CODE.put((short)300, "Greece");
		POSTAL_COUNTRIES_BY_CODE.put((short)304, "Greenland");
		POSTAL_COUNTRIES_BY_CODE.put((short)308, "Grenada");
		POSTAL_COUNTRIES_BY_CODE.put((short)312, "Guadeloupe");
		POSTAL_COUNTRIES_BY_CODE.put((short)316, "Guam");
		POSTAL_COUNTRIES_BY_CODE.put((short)320, "Guatemala");
		POSTAL_COUNTRIES_BY_CODE.put((short)324, "Guinea");
		POSTAL_COUNTRIES_BY_CODE.put((short)624, "Guinea-Bissau");
		POSTAL_COUNTRIES_BY_CODE.put((short)328, "Guyana");
		POSTAL_COUNTRIES_BY_CODE.put((short)332, "Haiti");
		POSTAL_COUNTRIES_BY_CODE.put((short)334, "Heard Island and McDonalds Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)336, "Holy See (Vatican City State)");
		POSTAL_COUNTRIES_BY_CODE.put((short)340, "Honduras");
		POSTAL_COUNTRIES_BY_CODE.put((short)344, "Hong Kong");
		POSTAL_COUNTRIES_BY_CODE.put((short)348, "Hungary");
		POSTAL_COUNTRIES_BY_CODE.put((short)352, "Iceland");
		POSTAL_COUNTRIES_BY_CODE.put((short)356, "India");
		POSTAL_COUNTRIES_BY_CODE.put((short)360, "Indonesia");
		POSTAL_COUNTRIES_BY_CODE.put((short)364, "Iran, Islamic Republic of");
		POSTAL_COUNTRIES_BY_CODE.put((short)368, "Iraq");
		POSTAL_COUNTRIES_BY_CODE.put((short)372, "Ireland");
		POSTAL_COUNTRIES_BY_CODE.put((short)376, "Israel");
		POSTAL_COUNTRIES_BY_CODE.put((short)380, "Italy");
		POSTAL_COUNTRIES_BY_CODE.put((short)388, "Jamaica");
		POSTAL_COUNTRIES_BY_CODE.put((short)392, "Japan");
		POSTAL_COUNTRIES_BY_CODE.put((short)400, "Jordan");
		POSTAL_COUNTRIES_BY_CODE.put((short)398, "Kazakstan");
		POSTAL_COUNTRIES_BY_CODE.put((short)404, "Kenya");
		POSTAL_COUNTRIES_BY_CODE.put((short)296, "Kiribati");
		POSTAL_COUNTRIES_BY_CODE.put((short)408, "Korea, Democratic People's Republic of");
		POSTAL_COUNTRIES_BY_CODE.put((short)410, "Korea, Republic of");
		POSTAL_COUNTRIES_BY_CODE.put((short)414, "Kuwait");
		POSTAL_COUNTRIES_BY_CODE.put((short)417, "Kyrgyzstan");
		POSTAL_COUNTRIES_BY_CODE.put((short)418, "Lao People's Democratic Republic");
		POSTAL_COUNTRIES_BY_CODE.put((short)428, "Latvia");
		POSTAL_COUNTRIES_BY_CODE.put((short)422, "Lebanon");
		POSTAL_COUNTRIES_BY_CODE.put((short)426, "Lesotho");
		POSTAL_COUNTRIES_BY_CODE.put((short)430, "Liberia");
		POSTAL_COUNTRIES_BY_CODE.put((short)434, "Libyan Arab Jamahiriya");
		POSTAL_COUNTRIES_BY_CODE.put((short)438, "Liechtenstein");
		POSTAL_COUNTRIES_BY_CODE.put((short)440, "Lithuania");
		POSTAL_COUNTRIES_BY_CODE.put((short)442, "Luxembourg");
		POSTAL_COUNTRIES_BY_CODE.put((short)446, "Macau");
		POSTAL_COUNTRIES_BY_CODE.put((short)807, "Macedonia, The Former Yugoslav Republic of");
		POSTAL_COUNTRIES_BY_CODE.put((short)450, "Madagascar");
		POSTAL_COUNTRIES_BY_CODE.put((short)454, "Malawi");
		POSTAL_COUNTRIES_BY_CODE.put((short)458, "Malaysia");
		POSTAL_COUNTRIES_BY_CODE.put((short)462, "Maldives");
		POSTAL_COUNTRIES_BY_CODE.put((short)466, "Mali");
		POSTAL_COUNTRIES_BY_CODE.put((short)470, "Malta");
		POSTAL_COUNTRIES_BY_CODE.put((short)584, "Marshall Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)474, "Martinique");
		POSTAL_COUNTRIES_BY_CODE.put((short)478, "Mauritania");
		POSTAL_COUNTRIES_BY_CODE.put((short)480, "Mauritius");
		POSTAL_COUNTRIES_BY_CODE.put((short)175, "Mayotte");
		POSTAL_COUNTRIES_BY_CODE.put((short)484, "Mexico");
		POSTAL_COUNTRIES_BY_CODE.put((short)583, "Micronesia, Federated States of");
		POSTAL_COUNTRIES_BY_CODE.put((short)498, "Moldova, Republic of");
		POSTAL_COUNTRIES_BY_CODE.put((short)492, "Monaco");
		POSTAL_COUNTRIES_BY_CODE.put((short)496, "Mongolia");
		POSTAL_COUNTRIES_BY_CODE.put((short)500, "Montserrat");
		POSTAL_COUNTRIES_BY_CODE.put((short)504, "Morocco");
		POSTAL_COUNTRIES_BY_CODE.put((short)508, "Mozambique");
		POSTAL_COUNTRIES_BY_CODE.put((short)104, "Myanmar");
		POSTAL_COUNTRIES_BY_CODE.put((short)516, "Namibia");
		POSTAL_COUNTRIES_BY_CODE.put((short)520, "Nauru");
		POSTAL_COUNTRIES_BY_CODE.put((short)524, "Nepal");
		POSTAL_COUNTRIES_BY_CODE.put((short)528, "Netherlands");
		POSTAL_COUNTRIES_BY_CODE.put((short)530, "Netherlands Antilles");
		POSTAL_COUNTRIES_BY_CODE.put((short)540, "New Caledonia");
		POSTAL_COUNTRIES_BY_CODE.put((short)554, "New Zealand");
		POSTAL_COUNTRIES_BY_CODE.put((short)558, "Nicaragua");
		POSTAL_COUNTRIES_BY_CODE.put((short)562, "Niger");
		POSTAL_COUNTRIES_BY_CODE.put((short)566, "Nigeria");
		POSTAL_COUNTRIES_BY_CODE.put((short)570, "Niue");
		POSTAL_COUNTRIES_BY_CODE.put((short)574, "Norfolk Island");
		POSTAL_COUNTRIES_BY_CODE.put((short)580, "Northern Mariana Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)578, "Norway");
		POSTAL_COUNTRIES_BY_CODE.put((short)512, "Oman");
		POSTAL_COUNTRIES_BY_CODE.put((short)586, "Pakistan");
		POSTAL_COUNTRIES_BY_CODE.put((short)585, "Palau");
		POSTAL_COUNTRIES_BY_CODE.put((short)275, "Palestinian Territory, Occupied");
		POSTAL_COUNTRIES_BY_CODE.put((short)591, "Panama");
		POSTAL_COUNTRIES_BY_CODE.put((short)598, "Papua New Guinea");
		POSTAL_COUNTRIES_BY_CODE.put((short)600, "Paraguay");
		POSTAL_COUNTRIES_BY_CODE.put((short)604, "Peru");
		POSTAL_COUNTRIES_BY_CODE.put((short)608, "Philippines");
		POSTAL_COUNTRIES_BY_CODE.put((short)612, "Pitcairn");
		POSTAL_COUNTRIES_BY_CODE.put((short)616, "Poland");
		POSTAL_COUNTRIES_BY_CODE.put((short)620, "Portugal");
		POSTAL_COUNTRIES_BY_CODE.put((short)630, "Puerto Rico");
		POSTAL_COUNTRIES_BY_CODE.put((short)634, "Qatar");
		POSTAL_COUNTRIES_BY_CODE.put((short)638, "Reunion");
		POSTAL_COUNTRIES_BY_CODE.put((short)642, "Romania");
		POSTAL_COUNTRIES_BY_CODE.put((short)643, "Russian Federation");
		POSTAL_COUNTRIES_BY_CODE.put((short)646, "Rwanda");
		POSTAL_COUNTRIES_BY_CODE.put((short)654, "Saint Helena");
		POSTAL_COUNTRIES_BY_CODE.put((short)659, "Saint Kitts and Nevis");
		POSTAL_COUNTRIES_BY_CODE.put((short)662, "Saint Lucia");
		POSTAL_COUNTRIES_BY_CODE.put((short)666, "Saint Pierre and Miquelon");
		POSTAL_COUNTRIES_BY_CODE.put((short)670, "Saint Vincent and The Grenadines");
		POSTAL_COUNTRIES_BY_CODE.put((short)882, "Samoa");
		POSTAL_COUNTRIES_BY_CODE.put((short)674, "San Marino");
		POSTAL_COUNTRIES_BY_CODE.put((short)678, "Sao Tome and Principe");
		POSTAL_COUNTRIES_BY_CODE.put((short)682, "Saudia Arabia");
		POSTAL_COUNTRIES_BY_CODE.put((short)686, "Senegal");
		POSTAL_COUNTRIES_BY_CODE.put((short)688, "Serbia");
		POSTAL_COUNTRIES_BY_CODE.put((short)690, "Seychelles");
		POSTAL_COUNTRIES_BY_CODE.put((short)694, "Sierra Leone");
		POSTAL_COUNTRIES_BY_CODE.put((short)702, "Singapore");
		POSTAL_COUNTRIES_BY_CODE.put((short)703, "Slovakia");
		POSTAL_COUNTRIES_BY_CODE.put((short)705, "Slovenia");
		POSTAL_COUNTRIES_BY_CODE.put((short)90, "Solomon Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)706, "Somalia");
		POSTAL_COUNTRIES_BY_CODE.put((short)710, "South Africa");
		POSTAL_COUNTRIES_BY_CODE.put((short)239, "South Georgia and South Sandwich Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)728, "South Sudan");
		POSTAL_COUNTRIES_BY_CODE.put((short)729, "Sudan");
		POSTAL_COUNTRIES_BY_CODE.put((short)724, "Spain");
		POSTAL_COUNTRIES_BY_CODE.put((short)144, "Sri Lanka");
		POSTAL_COUNTRIES_BY_CODE.put((short)740, "Suriname");
		POSTAL_COUNTRIES_BY_CODE.put((short)744, "Svalbard and Jan Mayen");
		POSTAL_COUNTRIES_BY_CODE.put((short)748, "Swaziland");
		POSTAL_COUNTRIES_BY_CODE.put((short)752, "Sweden");
		POSTAL_COUNTRIES_BY_CODE.put((short)756, "Switzerland");
		POSTAL_COUNTRIES_BY_CODE.put((short)760, "Syrian Arab Republic");
		POSTAL_COUNTRIES_BY_CODE.put((short)158, "Taiwan, Province of China");
		POSTAL_COUNTRIES_BY_CODE.put((short)762, "Tajikistan");
		POSTAL_COUNTRIES_BY_CODE.put((short)834, "Tanzania, United Republic of");
		POSTAL_COUNTRIES_BY_CODE.put((short)764, "Thailand");
		POSTAL_COUNTRIES_BY_CODE.put((short)768, "Togo");
		POSTAL_COUNTRIES_BY_CODE.put((short)772, "Tokelau");
		POSTAL_COUNTRIES_BY_CODE.put((short)776, "Tonga");
		POSTAL_COUNTRIES_BY_CODE.put((short)780, "Trinidad and Tobago");
		POSTAL_COUNTRIES_BY_CODE.put((short)788, "Tunisia");
		POSTAL_COUNTRIES_BY_CODE.put((short)792, "Turkey");
		POSTAL_COUNTRIES_BY_CODE.put((short)795, "Turkmenistan");
		POSTAL_COUNTRIES_BY_CODE.put((short)796, "Turks and Caicos Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)798, "Tuvalu");
		POSTAL_COUNTRIES_BY_CODE.put((short)800, "Uganda");
		POSTAL_COUNTRIES_BY_CODE.put((short)804, "Ukraine");
		POSTAL_COUNTRIES_BY_CODE.put((short)784, "United Arab Emirates");
		POSTAL_COUNTRIES_BY_CODE.put((short)826, "United Kingdom");
		POSTAL_COUNTRIES_BY_CODE.put((short)840, "United States of America");
		POSTAL_COUNTRIES_BY_CODE.put((short)581, "United States Minor Outlying Islands");
		POSTAL_COUNTRIES_BY_CODE.put((short)858, "Uruguay");
		POSTAL_COUNTRIES_BY_CODE.put((short)860, "Uzbekistan");
		POSTAL_COUNTRIES_BY_CODE.put((short)548, "Vanatu");
		POSTAL_COUNTRIES_BY_CODE.put((short)862, "Venezuela");
		POSTAL_COUNTRIES_BY_CODE.put((short)704, "Viet Nam");
		POSTAL_COUNTRIES_BY_CODE.put((short)850, "Virgin Islands, U.S.");
		POSTAL_COUNTRIES_BY_CODE.put((short)92, "Virgin Islands. British");
		POSTAL_COUNTRIES_BY_CODE.put((short)876, "Wallis and Futuna");
		POSTAL_COUNTRIES_BY_CODE.put((short)732, "Western Sahara");
		POSTAL_COUNTRIES_BY_CODE.put((short)887, "Yemen");
		POSTAL_COUNTRIES_BY_CODE.put((short)891, "Yugoslavia");
		POSTAL_COUNTRIES_BY_CODE.put((short)894, "Zambia");
		POSTAL_COUNTRIES_BY_CODE.put((short)716, "Zimbabwe");
		for(Map.Entry<Short,String> entry: POSTAL_COUNTRIES_BY_CODE.entrySet()) {
			POSTAL_COUNTRIES_BY_TEXT.put(
				entry.getValue(),
				entry.getKey()
			);
		}
		
		PHONE_COUNTRIES.put((short)0, 0);
		PHONE_COUNTRIES.put((short)4, 93);
		PHONE_COUNTRIES.put((short)8, 355);
		PHONE_COUNTRIES.put((short)12, 213);
		PHONE_COUNTRIES.put((short)16, 684);
		PHONE_COUNTRIES.put((short)20, 376);
		PHONE_COUNTRIES.put((short)24, 244);
		PHONE_COUNTRIES.put((short)660, 1);
		PHONE_COUNTRIES.put((short)28, 1);
		PHONE_COUNTRIES.put((short)32, 54);
		PHONE_COUNTRIES.put((short)51, 374);
		PHONE_COUNTRIES.put((short)533, 297);
		PHONE_COUNTRIES.put((short)10000, 247);
		PHONE_COUNTRIES.put((short)36, 61);
		PHONE_COUNTRIES.put((short)10001, 672);
		PHONE_COUNTRIES.put((short)40, 43);
		PHONE_COUNTRIES.put((short)31, 994);
		PHONE_COUNTRIES.put((short)44, 1);
		PHONE_COUNTRIES.put((short)48, 973);
		PHONE_COUNTRIES.put((short)50, 880);
		PHONE_COUNTRIES.put((short)52, 1);
		PHONE_COUNTRIES.put((short)112, 375);
		PHONE_COUNTRIES.put((short)56, 32);
		PHONE_COUNTRIES.put((short)84, 501);
		PHONE_COUNTRIES.put((short)204, 229);
		PHONE_COUNTRIES.put((short)60, 1);
		PHONE_COUNTRIES.put((short)64, 975);
		PHONE_COUNTRIES.put((short)68, 591);
		PHONE_COUNTRIES.put((short)70, 387);
		PHONE_COUNTRIES.put((short)72, 267);
		PHONE_COUNTRIES.put((short)76, 55);
		PHONE_COUNTRIES.put((short)96, 673);
		PHONE_COUNTRIES.put((short)100, 359);
		PHONE_COUNTRIES.put((short)854, 226);
		PHONE_COUNTRIES.put((short)108, 257);
		PHONE_COUNTRIES.put((short)116, 855);
		PHONE_COUNTRIES.put((short)120, 237);
		PHONE_COUNTRIES.put((short)124, 1);
		PHONE_COUNTRIES.put((short)132, 238);
		PHONE_COUNTRIES.put((short)136, 1);
		PHONE_COUNTRIES.put((short)140, 236);
		PHONE_COUNTRIES.put((short)148, 235);
		PHONE_COUNTRIES.put((short)152, 56);
		PHONE_COUNTRIES.put((short)156, 86);
		PHONE_COUNTRIES.put((short)170, 57);
		PHONE_COUNTRIES.put((short)174, 269);
		PHONE_COUNTRIES.put((short)178, 242);
		PHONE_COUNTRIES.put((short)180, 243);
		PHONE_COUNTRIES.put((short)184, 682);
		PHONE_COUNTRIES.put((short)188, 506);
		PHONE_COUNTRIES.put((short)384, 225);
		PHONE_COUNTRIES.put((short)191, 385);
		PHONE_COUNTRIES.put((short)192, 53);
		PHONE_COUNTRIES.put((short)196, 357);
		PHONE_COUNTRIES.put((short)203, 420);
		PHONE_COUNTRIES.put((short)208, 45);
		PHONE_COUNTRIES.put((short)10002, 246);
		PHONE_COUNTRIES.put((short)262, 253);
		PHONE_COUNTRIES.put((short)212, 1);
		PHONE_COUNTRIES.put((short)214, 1);
		PHONE_COUNTRIES.put((short)626, 670);
		PHONE_COUNTRIES.put((short)218, 593);
		PHONE_COUNTRIES.put((short)818, 20);
		PHONE_COUNTRIES.put((short)222, 503);
		PHONE_COUNTRIES.put((short)226, 240);
		PHONE_COUNTRIES.put((short)232, 291);
		PHONE_COUNTRIES.put((short)233, 372);
		PHONE_COUNTRIES.put((short)231, 251);
		PHONE_COUNTRIES.put((short)238, 500);
		PHONE_COUNTRIES.put((short)234, 298);
		PHONE_COUNTRIES.put((short)242, 679);
		PHONE_COUNTRIES.put((short)246, 358);
		PHONE_COUNTRIES.put((short)249, 33);
		PHONE_COUNTRIES.put((short)254, 594);
		PHONE_COUNTRIES.put((short)258, 689);
		PHONE_COUNTRIES.put((short)266, 241);
		PHONE_COUNTRIES.put((short)270, 220);
		PHONE_COUNTRIES.put((short)268, 995);
		PHONE_COUNTRIES.put((short)276, 49);
		PHONE_COUNTRIES.put((short)288, 233);
		PHONE_COUNTRIES.put((short)292, 350);
		PHONE_COUNTRIES.put((short)10003, 881);
		PHONE_COUNTRIES.put((short)300, 30);
		PHONE_COUNTRIES.put((short)304, 299);
		PHONE_COUNTRIES.put((short)308, 1);
		PHONE_COUNTRIES.put((short)10004, 388);
		PHONE_COUNTRIES.put((short)312, 590);
		PHONE_COUNTRIES.put((short)316, 1);
		PHONE_COUNTRIES.put((short)320, 502);
		PHONE_COUNTRIES.put((short)324, 224);
		PHONE_COUNTRIES.put((short)624, 245);
		PHONE_COUNTRIES.put((short)328, 592);
		PHONE_COUNTRIES.put((short)332, 509);
		PHONE_COUNTRIES.put((short)336, 396);
		PHONE_COUNTRIES.put((short)340, 504);
		PHONE_COUNTRIES.put((short)344, 852);
		PHONE_COUNTRIES.put((short)348, 36);
		PHONE_COUNTRIES.put((short)352, 354);
		PHONE_COUNTRIES.put((short)356, 91);
		PHONE_COUNTRIES.put((short)360, 62);
		PHONE_COUNTRIES.put((short)10005, 871);
		PHONE_COUNTRIES.put((short)10006, 874);
		PHONE_COUNTRIES.put((short)10007, 873);
		PHONE_COUNTRIES.put((short)10008, 872);
		PHONE_COUNTRIES.put((short)10009, 870);
		PHONE_COUNTRIES.put((short)10010, 800);
		PHONE_COUNTRIES.put((short)10011, 882);
		PHONE_COUNTRIES.put((short)364, 98);
		PHONE_COUNTRIES.put((short)368, 964);
		PHONE_COUNTRIES.put((short)372, 353);
		PHONE_COUNTRIES.put((short)376, 972);
		PHONE_COUNTRIES.put((short)380, 39);
		PHONE_COUNTRIES.put((short)388, 1);
		PHONE_COUNTRIES.put((short)392, 81);
		PHONE_COUNTRIES.put((short)400, 962);
		PHONE_COUNTRIES.put((short)398, 7);
		PHONE_COUNTRIES.put((short)404, 254);
		PHONE_COUNTRIES.put((short)296, 686);
		PHONE_COUNTRIES.put((short)408, 850);
		PHONE_COUNTRIES.put((short)410, 82);
		PHONE_COUNTRIES.put((short)414, 965);
		PHONE_COUNTRIES.put((short)417, 996);
		PHONE_COUNTRIES.put((short)418, 856);
		PHONE_COUNTRIES.put((short)428, 371);
		PHONE_COUNTRIES.put((short)422, 961);
		PHONE_COUNTRIES.put((short)426, 266);
		PHONE_COUNTRIES.put((short)430, 231);
		PHONE_COUNTRIES.put((short)434, 218);
		PHONE_COUNTRIES.put((short)438, 423);
		PHONE_COUNTRIES.put((short)440, 370);
		PHONE_COUNTRIES.put((short)442, 352);
		PHONE_COUNTRIES.put((short)446, 853);
		PHONE_COUNTRIES.put((short)807, 389);
		PHONE_COUNTRIES.put((short)454, 265);
		PHONE_COUNTRIES.put((short)458, 60);
		PHONE_COUNTRIES.put((short)462, 960);
		PHONE_COUNTRIES.put((short)466, 223);
		PHONE_COUNTRIES.put((short)470, 356);
		PHONE_COUNTRIES.put((short)584, 692);
		PHONE_COUNTRIES.put((short)474, 596);
		PHONE_COUNTRIES.put((short)478, 222);
		PHONE_COUNTRIES.put((short)480, 230);
		PHONE_COUNTRIES.put((short)175, 269);
		PHONE_COUNTRIES.put((short)484, 52);
		PHONE_COUNTRIES.put((short)583, 691);
		PHONE_COUNTRIES.put((short)498, 373);
		PHONE_COUNTRIES.put((short)492, 377);
		PHONE_COUNTRIES.put((short)496, 976);
		PHONE_COUNTRIES.put((short)500, 1);
		PHONE_COUNTRIES.put((short)504, 212);
		PHONE_COUNTRIES.put((short)508, 258);
		PHONE_COUNTRIES.put((short)104, 95);
		PHONE_COUNTRIES.put((short)516, 264);
		PHONE_COUNTRIES.put((short)520, 674);
		PHONE_COUNTRIES.put((short)524, 977);
		PHONE_COUNTRIES.put((short)528, 31);
		PHONE_COUNTRIES.put((short)530, 599);
		PHONE_COUNTRIES.put((short)540, 687);
		PHONE_COUNTRIES.put((short)554, 64);
		PHONE_COUNTRIES.put((short)558, 505);
		PHONE_COUNTRIES.put((short)562, 227);
		PHONE_COUNTRIES.put((short)566, 234);
		PHONE_COUNTRIES.put((short)570, 683);
		PHONE_COUNTRIES.put((short)580, 1);
		PHONE_COUNTRIES.put((short)578, 47);
		PHONE_COUNTRIES.put((short)512, 968);
		PHONE_COUNTRIES.put((short)586, 92);
		PHONE_COUNTRIES.put((short)585, 680);
		PHONE_COUNTRIES.put((short)275, 970);
		PHONE_COUNTRIES.put((short)591, 507);
		PHONE_COUNTRIES.put((short)598, 675);
		PHONE_COUNTRIES.put((short)600, 595);
		PHONE_COUNTRIES.put((short)604, 51);
		PHONE_COUNTRIES.put((short)608, 63);
		PHONE_COUNTRIES.put((short)616, 48);
		PHONE_COUNTRIES.put((short)620, 351);
		PHONE_COUNTRIES.put((short)630, 1);
		PHONE_COUNTRIES.put((short)634, 974);
		PHONE_COUNTRIES.put((short)638, 262);
		PHONE_COUNTRIES.put((short)10012, 0);
		PHONE_COUNTRIES.put((short)10013, 875);
		PHONE_COUNTRIES.put((short)10014, 876);
		PHONE_COUNTRIES.put((short)10015, 877);
		PHONE_COUNTRIES.put((short)10016, 969);
		PHONE_COUNTRIES.put((short)10017, 878);
		PHONE_COUNTRIES.put((short)10018, 888);
		PHONE_COUNTRIES.put((short)10019, 808);
		PHONE_COUNTRIES.put((short)10020, 79);
		PHONE_COUNTRIES.put((short)10021, 979);
		PHONE_COUNTRIES.put((short)642, 40);
		PHONE_COUNTRIES.put((short)643, 7);
		PHONE_COUNTRIES.put((short)646, 250);
		PHONE_COUNTRIES.put((short)654, 290);
		PHONE_COUNTRIES.put((short)659, 1);
		PHONE_COUNTRIES.put((short)662, 1);
		PHONE_COUNTRIES.put((short)666, 508);
		PHONE_COUNTRIES.put((short)670, 1);
		PHONE_COUNTRIES.put((short)882, 685);
		PHONE_COUNTRIES.put((short)674, 378);
		PHONE_COUNTRIES.put((short)678, 239);
		PHONE_COUNTRIES.put((short)682, 966);
		PHONE_COUNTRIES.put((short)686, 221);
		PHONE_COUNTRIES.put((short)690, 248);
		PHONE_COUNTRIES.put((short)694, 232);
		PHONE_COUNTRIES.put((short)702, 65);
		PHONE_COUNTRIES.put((short)703, 421);
		PHONE_COUNTRIES.put((short)705, 386);
		PHONE_COUNTRIES.put((short)90, 677);
		PHONE_COUNTRIES.put((short)706, 252);
		PHONE_COUNTRIES.put((short)710, 27);
		PHONE_COUNTRIES.put((short)239, 0);
		PHONE_COUNTRIES.put((short)724, 34);
		PHONE_COUNTRIES.put((short)144, 94);
		PHONE_COUNTRIES.put((short)736, 249);
		PHONE_COUNTRIES.put((short)740, 597);
		PHONE_COUNTRIES.put((short)748, 268);
		PHONE_COUNTRIES.put((short)752, 46);
		PHONE_COUNTRIES.put((short)756, 41);
		PHONE_COUNTRIES.put((short)760, 963);
		PHONE_COUNTRIES.put((short)158, 886);
		PHONE_COUNTRIES.put((short)762, 992);
		PHONE_COUNTRIES.put((short)834, 255);
		PHONE_COUNTRIES.put((short)764, 66);
		PHONE_COUNTRIES.put((short)768, 228);
		PHONE_COUNTRIES.put((short)772, 690);
		PHONE_COUNTRIES.put((short)776, 676);
		PHONE_COUNTRIES.put((short)10022, 991);
		PHONE_COUNTRIES.put((short)780, 1);
		PHONE_COUNTRIES.put((short)788, 216);
		PHONE_COUNTRIES.put((short)792, 90);
		PHONE_COUNTRIES.put((short)795, 993);
		PHONE_COUNTRIES.put((short)796, 1);
		PHONE_COUNTRIES.put((short)798, 688);
		PHONE_COUNTRIES.put((short)800, 256);
		PHONE_COUNTRIES.put((short)804, 380);
		PHONE_COUNTRIES.put((short)784, 971);
		PHONE_COUNTRIES.put((short)826, 44);
		PHONE_COUNTRIES.put((short)840, 1);
		PHONE_COUNTRIES.put((short)858, 598);
		PHONE_COUNTRIES.put((short)860, 998);
		PHONE_COUNTRIES.put((short)548, 678);
		PHONE_COUNTRIES.put((short)862, 58);
		PHONE_COUNTRIES.put((short)704, 84);
		PHONE_COUNTRIES.put((short)850, 1);
		PHONE_COUNTRIES.put((short)92, 1);
		PHONE_COUNTRIES.put((short)876, 681);
		PHONE_COUNTRIES.put((short)887, 967);
		PHONE_COUNTRIES.put((short)891, 381);
		PHONE_COUNTRIES.put((short)894, 260);
		PHONE_COUNTRIES.put((short)716, 263);
			
	}
    
    @Deprecated
    public static final Short USAGE_BUSINESS_MOBILE = new Short((short)540);
    @Deprecated
    public static final Short USAGE_HOME_MOBILE = new Short((short)440);
    public static final Short USAGE_MOBILE = new Short((short)200);
    public static final Short USAGE_BUSINESS_FAX = new Short((short)530);
    public static final Short USAGE_HOME_FAX = new Short((short)430);
    @Deprecated
    public static final Short USAGE_BUSINESS_MAIN_PHONE = new Short((short)520);
    @Deprecated
    public static final Short USAGE_HOME_MAIN_PHONE = new Short((short)420);
    public static final Short USAGE_OTHER = new Short((short)1800);
    public static final Short USAGE_BUSINESS = new Short((short)500);
    public static final Short USAGE_HOME = new Short((short)400);
    public static final Short USAGE_CONTRACT_INVOICE = 10000;
    public static final Short USAGE_CONTRACT_DELIVERY = 10200;
    
    // UNASSIGNED
    public static final String UNASSIGNED_ADDRESS = "UNASSIGNED";
    
}
