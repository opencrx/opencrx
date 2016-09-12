/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: XmlUtils
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011, CRIXP Corp., Switzerland
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
import java.io.StringReader;

import javax.xml.transform.Source;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.stream.StreamSource;

import org.openmdx.base.exception.ServiceException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

public class XmlUtils {

    //-----------------------------------------------------------------------    
	public static Document convertToDoc(
		String xml
	) throws ServiceException {
		try {
			javax.xml.transform.Transformer transformer = transformerFactory.newTransformer();
			Source xmlSource = new StreamSource(new StringReader(xml));
			DOMResult outputTarget = new DOMResult();
			transformer.transform(xmlSource, outputTarget);
			return ((Document)outputTarget.getNode());
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}
	
    //-----------------------------------------------------------------------    
	public static Document convertToDoc(
		InputStream in
	) throws ServiceException {
		try {
			javax.xml.transform.Transformer transformer = transformerFactory.newTransformer();
			Source xmlSource = new StreamSource(in);
			DOMResult outputTarget = new DOMResult();
			transformer.transform(xmlSource, outputTarget);
			return ((Document)outputTarget.getNode());
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}
	
    //-----------------------------------------------------------------------    
	public static String getElementText(
		Element node
	) {
		Text txtElem = (Text) node.getFirstChild();
		if(txtElem == null) {
			return null;
		}
		return txtElem.getData();
	}
	
    //-----------------------------------------------------------------------    
	public static String getElementText(
		Element root,
		String elementName
	) {
		NodeList list = null;
		list = root.getElementsByTagName(elementName);
		if(list.getLength() == 0) {
			return null;
		}
		return getElementText((Element)list.item(0));
	}
		
    //-----------------------------------------------------------------------    
	public static String getElementTextNS(
		Element root,
		String namespaceURI,
		String elementName
	) {
		NodeList list = null;
		list = root.getElementsByTagNameNS(namespaceURI, elementName);
		if(list.getLength() == 0) {
			return null;
		}
		return getElementText((Element)list.item(0));
	}

    //-----------------------------------------------------------------------
	// Members
    //-----------------------------------------------------------------------    
	protected static final TransformerFactory transformerFactory = TransformerFactory.newInstance();

}
