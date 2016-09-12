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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

import org.opencrx.application.airsync.utils.DOMUtils;
import org.w3c.dom.Element;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;

public abstract class AbstractXmlFormat implements IDataFormat {

	protected Logger logger = Logger.getLogger(AbstractXmlFormat.class.getPackage().getName());

	protected DateTimeFormat getUtcFormat(
		boolean utc
	) {
		if(utc) {
			 return DateTimeFormat.EXTENDED_UTC_FORMAT;			
		} else {		
			return DateTimeFormat.getInstance("yyyyMMdd'T'HHmmss'Z'");
		}
	}
		
	public String parseDOMString(Element elt, String default_value) {
		if (elt != null) {
			return elt.getTextContent();
		}
		return default_value;
	}

	public String parseDOMString(Element elt) {
		return parseDOMString(elt, null);
	}

	public Date parseDOMDate(Element elt) {
		if (elt != null) {
			return parseDate(elt.getTextContent());
		} else {
			return null;
		}
	}

	protected void createElement(
		Element p,
		String prefix,
		String name, 
		String val
	) {
		if (val != null && val.trim().length() > 0) {
			DOMUtils.createElementAndText(p, prefix, name, val);
		}
	}
	
	public Byte parseDOMByte(Element elt, Byte default_value) {
		if (elt != null) {
			return parseByte(elt.getTextContent());
		}
		return default_value;
	}

	public Byte parseDOMByte(Element elt) {
		return parseDOMByte(elt, null);
	}

	public Integer parseDOMInt(Element elt, Integer default_value) {
		if (elt != null) {
			return parseInt(elt.getTextContent());
		}
		return default_value;
	}

	public Integer parseDOMInt(Element elt) {
		return parseDOMInt(elt, null);
	}

	public Date parseDate(
		String str
	) {
		return Datatypes.create(Date.class, str);
	}

	public List<String> parseDOMStringCollection(
		Element node,
		String elementPrefix,
		String elementName, 
		ArrayList<String> default_value
	) {
		if(node != null) {
			return DOMUtils.getTexts(						
				node,
				elementPrefix,
				elementName
			);
		}
		return default_value;
	}

	public List<String> parseDOMStringCollection(
		Element node,
		String elementPrefix,
		String elementName
	) {
		return parseDOMStringCollection(
			node, 
			elementPrefix, 
			elementName, 
			null
		);
	}

	public byte parseByte(String str) {
		return Byte.parseByte(str);
	}

	public int parseInt(String str) {
		return Integer.parseInt(str);
	}

	public boolean parseBoolean(String str) {
		return Boolean.parseBoolean(str);
	}

	public Boolean parseDOMBoolean(Element elt, Boolean default_value) {
		if (elt != null) {
			return parseBoolean(elt.getTextContent());
		}
		return default_value;
	}

	public Boolean parseDOMBoolean(Element elt) {
		return parseDOMBoolean(elt, null);
	}

	/**
	 * Return an int else -1
	 * 
	 * @param elt
	 * @return int
	 */
	public int parseDOMNoNullInt(Element elt) {
		if (elt == null)
			return -1;

		return Integer.parseInt(elt.getTextContent());
	}

	/**
	 * Return true if 1 else false
	 * 
	 * @param elt
	 * @return
	 */
	public Boolean parseDOMInt2Boolean(Element elt) {
		if (parseDOMNoNullInt(elt) == 1)
			return Boolean.TRUE;
		else
			return Boolean.FALSE;
	}
}
