/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BpiAccount
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
package org.opencrx.application.bpi.datatype;

import java.lang.reflect.Type;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;

/**
 * BpiAccount
 *
 */
public class BpiAccount extends BpiObject {

	/**
	 * BpiAccountDeserializer
	 *
	 */
	public static class BpiAccountDeserializer implements JsonDeserializer<BpiAccount> {
		
		/**
		 * Constructor.
		 * 
		 * @param gsonBuilder
		 */
		public BpiAccountDeserializer(
			GsonBuilder gsonBuilder
		) {
			this.gsonBuilder = gsonBuilder;			
		}
		
		/* (non-Javadoc)
		 * @see com.google.gson.JsonDeserializer#deserialize(com.google.gson.JsonElement, java.lang.reflect.Type, com.google.gson.JsonDeserializationContext)
		 */
		public BpiAccount deserialize(
			JsonElement json, 
			Type typeOfT, 
			JsonDeserializationContext context
		) throws JsonParseException {
			Gson gson = this.gsonBuilder.create();
			if(json instanceof JsonObject) {
				JsonObject jsonObject = (JsonObject)json;
				if(jsonObject.has("firstName")) {
					return gson.fromJson(json, BpiContact.class);
				} else {
					return gson.fromJson(json, BpiOrganization.class);				
				}
			} else {
				return null;
			}
		}
		
		private final GsonBuilder gsonBuilder;
	}

	/**
	 * @return the extString0
	 */
	public String getExtString0() {
		return extString0;
	}

	/**
	 * @param extString0 the id to set
	 */
	public void setExtString0(String extString0) {
		this.extString0 = extString0;
	}

	public String getVcard() {
		return vcard;
	}

	public void setVcard(String vcard) {
		this.vcard = vcard;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
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
	 * @return the mailBusiness
	 */
	public BpiEMailAddress getMailBusiness() {
		return mailBusiness;
	}

	/**
	 * @param mailBusiness the mailBusiness to set
	 */
	public void setMailBusiness(BpiEMailAddress mailBusiness) {
		this.mailBusiness = mailBusiness;
	}

	/**
	 * @return the mailHome
	 */
	public BpiEMailAddress getMailHome() {
		return mailHome;
	}

	/**
	 * @param mailHome the mailHome to set
	 */
	public void setMailHome(BpiEMailAddress mailHome) {
		this.mailHome = mailHome;
	}

	/**
	 * @return the phoneBusiness
	 */
	public BpiPhoneNumber getPhoneBusiness() {
		return phoneBusiness;
	}

	/**
	 * @param phoneBusiness the phoneBusiness to set
	 */
	public void setPhoneBusiness(BpiPhoneNumber phoneBusiness) {
		this.phoneBusiness = phoneBusiness;
	}
	/**
	 * @return the phoneBusiness2
	 */
	public BpiPhoneNumber getPhoneBusiness2() {
		return phoneBusiness2;
	}

	/**
	 * @param phoneBusiness2 the phoneBusiness2 to set
	 */
	public void setPhoneBusiness2(BpiPhoneNumber phoneBusiness2) {
		this.phoneBusiness2 = phoneBusiness2;
	}
	
	/**
	 * @return the phoneHome
	 */
	public BpiPhoneNumber getPhoneHome() {
		return phoneHome;
	}

	/**
	 * @param phoneHome the phoneHome to set
	 */
	public void setPhoneHome(BpiPhoneNumber phoneHome) {
		this.phoneHome = phoneHome;
	}

	/**
	 * @return the faxBusiness
	 */
	public BpiPhoneNumber getFaxBusiness() {
		return faxBusiness;
	}

	/**
	 * @param faxBusiness the faxBusiness to set
	 */
	public void setFaxBusiness(BpiPhoneNumber faxBusiness) {
		this.faxBusiness = faxBusiness;
	}

	/**
	 * @return the faxHome
	 */
	public BpiPhoneNumber getFaxHome() {
		return faxHome;
	}

	/**
	 * @param faxHome the faxHome to set
	 */
	public void setFaxHome(BpiPhoneNumber faxHome) {
		this.faxHome = faxHome;
	}

	/**
	 * @return the postalBusiness
	 */
	public BpiPostalAddress getPostalBusiness() {
		return postalBusiness;
	}

	/**
	 * @param postalBusiness the postalBusiness to set
	 */
	public void setPostalBusiness(BpiPostalAddress postalBusiness) {
		this.postalBusiness = postalBusiness;
	}

	/**
	 * @return the postalHome
	 */
	public BpiPostalAddress getPostalHome() {
		return postalHome;
	}

	/**
	 * @param postalHome the postalHome to set
	 */
	public void setPostalHome(BpiPostalAddress postalHome) {
		this.postalHome = postalHome;
	}

	/**
	 * @return the mobile
	 */
	public BpiPhoneNumber getMobile() {
		return mobile;
	}

	/**
	 * @param mobile the mobile to set
	 */
	public void setMobile(BpiPhoneNumber mobile) {
		this.mobile = mobile;
	}

	/**
	 * @return the phoneOther
	 */
	public BpiPhoneNumber getPhoneOther() {
		return phoneOther;
	}

	/**
	 * @param phoneOther the phoneOther to set
	 */
	public void setPhoneOther(BpiPhoneNumber phoneOther) {
		this.phoneOther = phoneOther;
	}

	/**
	 * @return the mailOther
	 */
	public BpiEMailAddress getMailOther() {
		return mailOther;
	}

	/**
	 * @param mailOther the mailOther to set
	 */
	public void setMailOther(BpiEMailAddress mailOther) {
		this.mailOther = mailOther;
	}
	
	/**
	 * @return the faxBusiness2
	 */
	public BpiPhoneNumber getFaxBusiness2() {
		return faxBusiness2;
	}

	/**
	 * @param faxBusiness2 the faxBusiness2 to set
	 */
	public void setFaxBusiness2(BpiPhoneNumber faxBusiness2) {
		this.faxBusiness2 = faxBusiness2;
	}

	/**
	 * @return the postalBusiness2
	 */
	public BpiPostalAddress getPostalBusiness2() {
		return postalBusiness2;
	}

	/**
	 * @param postalBusiness2 the postalBusiness2 to set
	 */
	public void setPostalBusiness2(BpiPostalAddress postalBusiness2) {
		this.postalBusiness2 = postalBusiness2;
	}
	
	/**
	 * @return the mailBusiness2
	 */
	public BpiEMailAddress getMailBusiness2() {
		return mailBusiness2;
	}

	/**
	 * @param mailBusiness2 the mailBusiness2 to set
	 */
	public void setMailBusiness2(BpiEMailAddress mailBusiness2) {
		this.mailBusiness2 = mailBusiness2;
	}

	/**
	 * @return the localizedField
	 */
	public List<BpiLocalizedField> getLocalizedField() {
		return localizedField;
	}

	/**
	 * @param localizedField the localizedField to set
	 */
	public void setLocalizedField(List<BpiLocalizedField> localizedField) {
		this.localizedField = localizedField;
	}	

	/**
	 * @return the webBusiness
	 */
	public BpiWebAddress getWebBusiness() {
		return webBusiness;
	}

	/**
	 * @param webBusiness the webBusiness to set
	 */
	public void setWebBusiness(BpiWebAddress webBusiness) {
		this.webBusiness = webBusiness;
	}

	/**
	 * @return the webHome
	 */
	public BpiWebAddress getWebHome() {
		return webHome;
	}

	/**
	 * @param webHome the webHome to set
	 */
	public void setWebHome(BpiWebAddress webHome) {
		this.webHome = webHome;
	}

	private String extString0;
	private String aliasName;
	private String fullName;
	private String vcard;
	private BpiEMailAddress mailBusiness;
	private BpiEMailAddress mailBusiness2;
	private BpiEMailAddress mailHome;
	private BpiEMailAddress mailOther;
	private BpiPhoneNumber phoneBusiness;
	private BpiPhoneNumber phoneBusiness2;
	private BpiPhoneNumber phoneHome;
	private BpiPhoneNumber phoneOther;
	private BpiPhoneNumber faxBusiness;
	private BpiPhoneNumber faxBusiness2;
	private BpiPhoneNumber faxHome;
	private BpiPostalAddress postalBusiness;
	private BpiPostalAddress postalBusiness2;
	private BpiPostalAddress postalHome;
	private BpiPhoneNumber mobile;
	private BpiWebAddress webBusiness;
	private BpiWebAddress webHome;
	private List<BpiLocalizedField> localizedField;
	
}
