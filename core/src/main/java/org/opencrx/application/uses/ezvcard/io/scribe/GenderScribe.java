package org.opencrx.application.uses.ezvcard.io.scribe;

import java.util.List;

import org.opencrx.application.uses.ezvcard.VCardDataType;
import org.opencrx.application.uses.ezvcard.VCardVersion;
import org.opencrx.application.uses.ezvcard.io.xml.XCardElement;
import org.opencrx.application.uses.ezvcard.parameter.VCardParameters;
import org.opencrx.application.uses.ezvcard.property.Gender;

/*
 Copyright (c) 2013, Michael Angstadt
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met: 

 1. Redistributions of source code must retain the above copyright notice, this
 list of conditions and the following disclaimer. 
 2. Redistributions in binary form must reproduce the above copyright notice,
 this list of conditions and the following disclaimer in the documentation
 and/or other materials provided with the distribution. 

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * Marshals {@link Gender} properties.
 * @author Michael Angstadt
 */
public class GenderScribe extends VCardPropertyScribe<Gender> {
	public GenderScribe() {
		super(Gender.class, "GENDER");
	}

	@Override
	protected VCardDataType _defaultDataType(VCardVersion version) {
		return VCardDataType.TEXT;
	}

	@Override
	protected String _writeText(Gender property, VCardVersion version) {
		String gender = property.getGender();
		String text = property.getText();

		if (text != null) {
			return structured(gender, text);
		}
		if (gender != null) {
			return structured(new Object[] { gender });
		}
		return "";
	}

	@Override
	protected Gender _parseText(String value, VCardDataType dataType, VCardVersion version, VCardParameters parameters, List<String> warnings) {
		SemiStructuredIterator it = semistructured(value, 2);

		String sex = it.next();
		if (sex != null) {
			if (sex.length() == 0) {
				sex = null;
			} else {
				sex = sex.toUpperCase();
			}
		}
		String text = it.next();

		Gender property = new Gender(sex);
		property.setText(text);
		return property;
	}

	@Override
	protected void _writeXml(Gender property, XCardElement parent) {
		parent.append("sex", property.getGender());

		String text = property.getText();
		if (text != null) {
			parent.append("identity", text);
		}
	}

	@Override
	protected Gender _parseXml(XCardElement element, VCardParameters parameters, List<String> warnings) {
		String sex = element.first("sex");
		if (sex != null) {
			Gender property = new Gender(sex);
			property.setText(element.first("identity")); //optional field
			return property;
		}

		throw missingXmlElements("sex");
	}

}
