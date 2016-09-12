/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: ActiveSync
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
import java.io.OutputStream;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stax.StAXResult;

import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.wbxml.WBXMLReader;
import org.openmdx.base.xml.stream.XMLOutputFactories;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

public class WbXMLTransformer {

	public static Document transformFromWBXML(
		InputStream in
	) throws ServiceException {
		try {
			XMLReader wbxmlReader = new WBXMLReader(
				new org.openmdx.application.airsync.WBXMLPlugIn()
			); 
			Transformer transformer = transformerFactory.newTransformer();
			Source xmlSource = new SAXSource(wbxmlReader, new InputSource(in));
			DOMResult outputTarget = new DOMResult();
			transformer.transform(xmlSource, outputTarget);
			return ((Document)outputTarget.getNode());
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	public static void transformToWBXML(
		Document doc,
		OutputStream out
	) throws IOException {
		try {
			XMLStreamWriter wbxmlStreamWriter = xmlOutputFactory.createXMLStreamWriter(out);
			Result outputTarget = new StAXResult(wbxmlStreamWriter);			
			transform(
				doc,
				outputTarget,
				false
			);
		}
		catch (Exception e) {
			throw new IOException(e);
		}

	}

	public static void transform(
		Document doc,
		Result outputTarget,
		boolean indent
	) throws IOException {
		try {
			Source xmlSource = new DOMSource(doc.getDocumentElement());			
			Transformer transformer = transformerFactory.newTransformer();
			if(indent) {
				transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			}
			transformer.transform(xmlSource, outputTarget);
		}
		catch (Exception e) {
			throw new IOException(e);
		}

	}

	private static XMLOutputFactory xmlOutputFactory; 
	private static TransformerFactory transformerFactory = TransformerFactory.newInstance();
	
	static {
		try {
			xmlOutputFactory = XMLOutputFactories.newInstance("application/vnd.ms-sync.wbxml");
		} catch(Exception e) {
			new ServiceException(e).log();
		}
	}
	
}
