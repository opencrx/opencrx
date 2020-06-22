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
package org.opencrx.application.airsync.utils;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.openmdx.application.airsync.WBXMLPlugIn;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

public final class DOMUtils {

	private static DocumentBuilderFactory documentBuilderFactory;
	private static DocumentBuilder documentBuilder;
	private static WBXMLPlugIn wbxmlPlugin = new WBXMLPlugIn();

	static {
		documentBuilderFactory = DocumentBuilderFactory.newInstance();
		documentBuilderFactory.setNamespaceAware(true);
		documentBuilderFactory.setValidating(false);
		try {
			documentBuilder = documentBuilderFactory.newDocumentBuilder();
		}
		catch (ParserConfigurationException e) {
		}
	}

	public static String getNamespaceURI(
		String prefix
	) {
		return wbxmlPlugin.getNamespaceContext().getNamespaceURI(
			prefix.endsWith(":") ? prefix.substring(0, prefix.indexOf(":")) : prefix
		);		
	}
	
	public static String getElementText(
		Element root,
		String prefix,
		String elementName
	) {
		NodeList list = null;
		if(prefix == null) { 
			list = root.getElementsByTagName(elementName);
		} else {
			list = root.getElementsByTagNameNS(
				getNamespaceURI(prefix),
				elementName
			);			
		}
		if(list.getLength() == 0) {
			return null;
		}
		return getElementText((Element) list.item(0));
	}

	public static String getElementText(
		Element node
	) {
		Text txtElem = (Text) node.getFirstChild();
		if(txtElem == null) {
			return null;
		}
		return txtElem.getData();
	}

	public static List<String> getTexts(
		Element root, 
		String prefix,
		String elementName
	) {
		NodeList list = null;
		if(prefix == null) {
			list = root.getElementsByTagName(elementName);
		} else {
			list = root.getElementsByTagNameNS(
				getNamespaceURI(prefix),
				elementName
			);			
		}
		List<String> ret = new ArrayList<String>();
		for (int i = 0; i < list.getLength(); i++) {
			Text txt = (Text) list.item(i).getFirstChild();
			if(txt != null) {
				ret.add(txt.getData());
			}
			else {
				ret.add("");
			}
		}
		return ret;
	}

	public static Element getUniqueElement(
		Element parent, 
		String prefix,
		String elementName
	) {
		NodeList list = null;
		if(prefix == null) {
			list = parent.getElementsByTagName(elementName);			
		} else {
			list = parent.getElementsByTagNameNS(
				getNamespaceURI(prefix),
				elementName
			);
		}
		for(int i = 0; i < list.getLength(); i++) {
			Node node = list.item(i);
			if(node instanceof Element && node.getParentNode().equals(parent)) {
				return (Element)node;
			}
		}
		return null;
	}

	public static Element createElementAndText(
		Element parent, 
		String prefix,
		String elementName, 
		String text
	) {
		if(text == null) return null;
		Element el = null;
		if(prefix == null) {
			el = parent.getOwnerDocument().createElement(elementName);
		} else {
			el = parent.getOwnerDocument().createElementNS(
				getNamespaceURI(prefix), 
				prefix + elementName
			);
		}
		parent.appendChild(el);
		Text txt = el.getOwnerDocument().createTextNode(text);
		el.appendChild(txt);
		return el;
	}

	public static Element createElement(
		Element parent, 
		String prefix,
		String elementName
	) {
		Element el = null;
		if(prefix == null) {
			el = parent.getOwnerDocument().createElement(elementName);
		}
		else {
			el = parent.getOwnerDocument().createElementNS(
				getNamespaceURI(prefix),
				prefix + elementName
			);			
		}
		parent.appendChild(el);
		return el;
	}

	public static Document parse(
		InputStream is
	) throws SAXException, IOException {
		Document ret = documentBuilder.parse(is);
		return ret;
	}

	public static Document createDoc(
		String namespaceURI, 
		String rootElement,
		String[] ...namespaces
	) {
		Document doc = documentBuilder.getDOMImplementation().createDocument(namespaceURI, rootElement, null);
		for(String[] namespace: namespaces) {
			doc.getDocumentElement().setAttribute(namespace[0], namespace[1]);
		}
		return doc;
	}
	
}
