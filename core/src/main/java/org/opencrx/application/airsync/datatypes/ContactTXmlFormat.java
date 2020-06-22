/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: Sync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.datatypes;

import org.opencrx.application.airsync.utils.DOMUtils;
import org.opencrx.kernel.utils.Utils;
import org.w3c.dom.Element;
import org.w3c.format.DateTimeFormat;

public class ContactTXmlFormat extends AbstractXmlFormat {

	public ContactTXmlFormat(
	) {
	}

	@Override
	public void format(
		Element eParent,
		IData data,
		double protocolVersion
	) {
		ContactT contactT = (ContactT) data;
		DateTimeFormat eutcf = DateTimeFormat.EXTENDED_UTC_FORMAT;

		this.createElement(eParent, "Contacts:", "Alias", contactT.getAlias());		
		if(contactT.getAnniversary() != null) {
			this.createElement(eParent, "Contacts:", "Anniversary", eutcf.format(contactT.getAnniversary()));
		}
		this.createElement(eParent, "Contacts:", "AssistantName", contactT.getAssistantName());
		this.createElement(eParent, "Contacts:", "AssistantTelephoneNumber", contactT.getAssistantPhoneNumber());
		this.createElement(eParent, "Contacts:", "AssistnamePhoneNumber", contactT.getAssistnamePhoneNumber());
		if(contactT.getBirthday() != null) {
			this.createElement(eParent, "Contacts:", "Birthday", eutcf.format(contactT.getBirthday()));
		}
		this.createElement(eParent, "Contacts:", "Business2PhoneNumber", contactT.getBusiness2PhoneNumber());
		this.createElement(eParent, "Contacts:", "BusinessAddressCity", contactT.getBusinessAddressCity());
		this.createElement(eParent, "Contacts:", "BusinessPhoneNumber", contactT.getBusinessPhoneNumber());
		this.createElement(eParent, "Contacts:", "WebPage", contactT.getWebPage());
		this.createElement(eParent, "Contacts:", "BusinessAddressCountry", contactT.getBusinessAddressCountry());
		this.createElement(eParent, "Contacts:", "Department", contactT.getDepartment());
		this.createElement(eParent, "Contacts:", "Email1Address", contactT.getEmail1Address());
		this.createElement(eParent, "Contacts:", "Email2Address", contactT.getEmail2Address());
		this.createElement(eParent, "Contacts:", "Email3Address", contactT.getEmail3Address());
		this.createElement(eParent, "Contacts:", "BusinessFaxNumber", contactT.getBusinessFaxNumber());
		this.createElement(eParent, "Contacts:", "FileAs", contactT.getFileAs());
		this.createElement(eParent, "Contacts:", "FirstName", contactT.getFirstName());
		this.createElement(eParent, "Contacts:", "MiddleName", contactT.getMiddleName());
		this.createElement(eParent, "Contacts:", "HomeAddressCity", contactT.getHomeAddressCity());
		this.createElement(eParent, "Contacts:", "HomeAddressCountry", contactT.getHomeAddressCountry());
		this.createElement(eParent, "Contacts:", "HomeFaxNumber", contactT.getHomeFaxNumber());
		this.createElement(eParent, "Contacts:", "HomePhoneNumber", contactT.getHomePhoneNumber());
		this.createElement(eParent, "Contacts:", "Home2PhoneNumber", contactT.getHome2PhoneNumber());
		this.createElement(eParent, "Contacts:", "HomeAddressPostalCode", contactT.getHomeAddressPostalCode());
		this.createElement(eParent, "Contacts:", "HomeAddressState", contactT.getHomeAddressState());
		this.createElement(eParent, "Contacts:", "HomeAddressStreet", contactT.getHomeAddressStreet());
		this.createElement(eParent, "Contacts:", "MobilePhoneNumber", contactT.getMobilePhoneNumber());
		this.createElement(eParent, "Contacts:", "Suffix", contactT.getSuffix());
		this.createElement(eParent, "Contacts:", "CompanyName", contactT.getCompanyName());
		this.createElement(eParent, "Contacts:", "OtherAddressCity", contactT.getOtherAddressCity());
		this.createElement(eParent, "Contacts:", "OtherAddressCountry", contactT.getOtherAddressCountry());
		this.createElement(eParent, "Contacts:", "OtherAddressPostalCode", contactT.getOtherAddressPostalCode());
		this.createElement(eParent, "Contacts:", "OtherAddressState", contactT.getOtherAddressState());
		this.createElement(eParent, "Contacts:", "OtherAddressStreet", contactT.getOtherAddressStreet());
		this.createElement(eParent, "Contacts:", "PagerNumber", contactT.getPagerNumber());
		this.createElement(eParent, "Contacts:", "Title", contactT.getTitle());
		this.createElement(eParent, "Contacts:", "BusinessAddressPostalCode", contactT.getBusinessPostalCode());
		this.createElement(eParent, "Contacts:", "LastName", contactT.getLastName());
		this.createElement(eParent, "Contacts:", "Spouse", contactT.getSpouse());
		this.createElement(eParent, "Contacts:", "BusinessAddressState", contactT.getBusinessState());
		this.createElement(eParent, "Contacts:", "BusinessAddressStreet", contactT.getBusinessStreet());
		this.createElement(eParent, "Contacts:", "JobTitle", contactT.getJobTitle());
		this.createElement(eParent, "Contacts:", "YomiFirstName", contactT.getYomiFirstName());
		this.createElement(eParent, "Contacts:", "YomiLastName", contactT.getYomiLastName());
		this.createElement(eParent, "Contacts:", "YomiCompanyName", contactT.getYomiCompanyName());
		this.createElement(eParent, "Contacts:", "OfficeLocation", contactT.getOfficeLocation());
		this.createElement(eParent, "Contacts:", "Picture", contactT.getPicture());
		if(contactT.getCategories() != null && !contactT.getCategories().isEmpty()) {
			Element eCategories = DOMUtils.createElement(eParent, "Contacts:", "Categories");
			for(String category: contactT.getCategories()) {
				this.createElement(eCategories, "Contacts:", "Category", category);
			}
		}
		if(contactT.getChildren() != null && !contactT.getChildren().isEmpty()) {
			Element eChildren = DOMUtils.createElement(eParent, "Contacts:", "Children");
			for(String child : contactT.getChildren()) {
				this.createElement(eChildren, "Contacts:", "Child", child);
			}
		}
		String body = contactT.getBody();
		if(body != null) {
			this.createElement(eParent, "Contacts:", "Body", Utils.normalizeNewLines(body).replace("\n", "\r\n"));
		}
		// Contacts2
		this.createElement(eParent, "Contacts2:", "CustomerId", contactT.getCustomerId());
		this.createElement(eParent, "Contacts2:", "IMAddress", contactT.getIMAddress());
		this.createElement(eParent, "Contacts2:", "IMAddress2", contactT.getIMAddress2());
		this.createElement(eParent, "Contacts2:", "IMAddress3", contactT.getIMAddress3());
		this.createElement(eParent, "Contacts2:", "ManagerName", contactT.getManagerName());
		this.createElement(eParent, "Contacts2:", "NickName", contactT.getNickName());
	}

	@Override
	public IData parse(
		Element syncData
	) {
		ContactT contactT = new ContactT();
		
		contactT.setAlias(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Alias")));
		contactT.setAnniversary(parseDOMDate(DOMUtils.getUniqueElement(syncData, "Contacts:", "Anniversary")));
		contactT.setAssistantName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "AssistantName")));
		contactT.setAssistantPhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "AssistantTelephoneNumber")));
		contactT.setBirthday(parseDOMDate(DOMUtils.getUniqueElement(syncData, "Contacts:", "Birthday")));
		contactT.setBusiness2PhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Business2PhoneNumber")));
		contactT.setBusinessAddressCity(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "BusinessAddressCity")));
		contactT.setBusinessPhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "BusinessPhoneNumber")));
		contactT.setWebPage(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "WebPage")));
		contactT.setBusinessAddressCountry(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "BusinessAddressCountry")));
		contactT.setDepartment(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Department")));
		contactT.setEmail1Address(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Email1Address")));
		contactT.setEmail2Address(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Email2Address")));
		contactT.setEmail3Address(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Email3Address")));
		contactT.setBusinessFaxNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "BusinessFaxNumber")));
		contactT.setFileAs(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "FileAs")));
		contactT.setFirstName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "FirstName")));
		contactT.setMiddleName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "MiddleName")));
		contactT.setHomeAddressCity(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "HomeAddressCity")));
		contactT.setHomeAddressCountry(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "HomeAddressCountry")));
		contactT.setHomeFaxNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "HomeFaxNumber")));
		contactT.setHomePhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "HomePhoneNumber")));
		contactT.setHome2PhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Home2PhoneNumber")));
		contactT.setHomeAddressPostalCode(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "HomeAddressPostalCode")));
		contactT.setHomeAddressState(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "HomeAddressState")));
		contactT.setHomeAddressStreet(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "HomeAddressStreet")));
		contactT.setMobilePhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "MobilePhoneNumber")));
		contactT.setSuffix(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Suffix")));
		contactT.setCompanyName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "CompanyName")));
		contactT.setOtherAddressCity(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "OtherAddressCity")));
		contactT.setOtherAddressCountry(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "OtherAddressCountry")));
		contactT.setCarPhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "CarTelephoneNumber")));
		contactT.setOtherAddressPostalCode(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "OtherAddressPostalCode")));
		contactT.setOtherAddressState(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "OtherAddressState")));
		contactT.setOtherAddressStreet(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "OtherAddressStreet")));
		contactT.setPagerNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "PagerNumber")));
		contactT.setTitle(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Title")));
		contactT.setBusinessPostalCode(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "BusinessAddressPostalCode")));
		contactT.setLastName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "LastName")));
		contactT.setSpouse(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Spouse")));
		contactT.setBusinessState(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "BusinessAddressState")));
		contactT.setBusinessStreet(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "BusinessAddressStreet")));
		contactT.setJobTitle(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "JobTitle")));
		contactT.setYomiFirstName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "YomiFirstName")));
		contactT.setYomiLastName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "YomiLastName")));
		contactT.setYomiCompanyName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "YomiCompanyName")));
		contactT.setOfficeLocation(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "OfficeLocation")));
		contactT.setRadioPhoneNumber(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "RadioTelephoneNumber")));
		contactT.setPicture(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Picture")));
		contactT.setCategories(parseDOMStringCollection(DOMUtils.getUniqueElement(syncData, "Contacts:", "Categories"), "Contacts:", "Category"));
		contactT.setChildren(parseDOMStringCollection(DOMUtils.getUniqueElement(syncData, "Contacts:", "Children"), "Contacts:", "Child"));
		contactT.setBody(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts:", "Body")));
		// Contacts2		
		contactT.setCustomerId(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "CustomerId")));
		contactT.setGovernmentId(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "GovernmentId")));
		contactT.setIMAddress(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "IMAddress")));
		contactT.setIMAddress2(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "IMAddress2")));
		contactT.setIMAddress3(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "IMAddress3")));
		contactT.setManagerName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "ManagerName")));
		contactT.setCompanyMainPhone(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "CompanyMainPhone")));
		contactT.setAccountName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "AccountName")));
		contactT.setNickName(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "NickName")));
		contactT.setMMS(parseDOMString(DOMUtils.getUniqueElement(syncData, "Contacts2:", "MMS")));		
		
		return contactT;
	}
	
}
