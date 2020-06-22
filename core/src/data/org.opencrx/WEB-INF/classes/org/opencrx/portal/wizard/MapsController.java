/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: MapsController
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
package org.opencrx.portal.wizard;

import java.util.ArrayList;
import java.util.List;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.PostalAddressQuery;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.address1.jmi1.PostalAddressable;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.portal.servlet.Codes;
import org.openmdx.portal.servlet.JspWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * MapsController
 *
 */
public class MapsController extends JspWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public MapsController(
	) {
		super();
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
	 * Get URL for Google maps.
	 * 
	 * @param postalAddress
	 * @return
	 */
	public String[] getGoogleMapsUrl(
		PostalAddressable postalAddress
	) {
		Codes codes = this.getCodes();
		String url = "http://maps.google.com/maps?q=";
		String htmlAddress = "";
		String allStreetLines = "";
		boolean atLeastOneIncluded = false;
		for(String postalAddressLine: postalAddress.getPostalAddressLine()) {
			htmlAddress += postalAddressLine + "<br />";
		}
		for(String streetLine: postalAddress.getPostalStreet()) {
			boolean include = false;
			for (int j=0; (j < STREET_PATTERNs.length) && !include; j++) {
				include = streetLine.indexOf(STREET_PATTERNs[j]) >= 0;
			}
			if(include) {
				atLeastOneIncluded = true;
				url += "%20" + streetLine;
			}
			allStreetLines += "%20" + streetLine;
			htmlAddress += streetLine + "<br />";
		}
		if (!atLeastOneIncluded) {
			url += "%20" + allStreetLines;
		}
		String tmp = postalAddress.getPostalCode() == null
			? ""
			: postalAddress.getPostalCode().trim();
		if (!tmp.isEmpty()) {
			url += "%20" + tmp;
			htmlAddress += tmp + " ";
		}
		tmp = postalAddress.getPostalCity() == null
			? ""
			: postalAddress.getPostalCity().trim();
		if (!tmp.isEmpty()) {
			url += "%20" + tmp;
			htmlAddress += tmp + "<br />";
		}
		String country = postalAddress.getPostalCountry() == 0
			? DEFAULT_COUNTRY
			: (codes.getLongTextByCode("country", (short)0, true).get(new Short((short)postalAddress.getPostalCountry())));
		tmp = (country.split("\\[")[0]).trim();
		if (!tmp.isEmpty()) {
			url += "%20" + tmp;
			htmlAddress += tmp;
		}
		return new String[]{
			url,
			htmlAddress
		};
	}

	/**
	 * Refresh action.
	 * 
	 */
	public void doRefresh(
	) {
		PersistenceManager pm = this.getPm();
		RefObject_1_0 obj = this.getObject();
		this.locations = new ArrayList<PostalAddressable>();
		if(obj instanceof org.opencrx.kernel.account1.jmi1.Account) {
			org.opencrx.kernel.account1.jmi1.Account account = (org.opencrx.kernel.account1.jmi1.Account)obj;
			PostalAddressQuery postalAddressQuery = (PostalAddressQuery)pm.newQuery(PostalAddress.class);
			postalAddressQuery.forAllDisabled().isFalse();
			for (PostalAddressable postalAddress: account.<PostalAddress>getAddress(postalAddressQuery)) {
				this.locations.add(postalAddress);
			}
		} else {
			this.locations.add((PostalAddressable)obj);
		}
	}

	/**
	 * @return the locations
	 */
	public List<PostalAddressable> getLocations() {
		return locations;
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private static final String[] STREET_PATTERNs = {
	    /* de */ "STR", "WEG", "HOF", "IFANG", "ACKER", "OBER", "MITTEL", "UNTER",
	    /* en */ "STR", "DR", "RD", "AV"
	};
	public static final String DEFAULT_COUNTRY = "Switzerland";

	private List<PostalAddressable> locations;
}
