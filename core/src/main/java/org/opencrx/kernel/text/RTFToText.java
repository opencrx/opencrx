/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: RTFToText
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.text;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.rtf.RTFEditorKit;

import org.openmdx.base.exception.ServiceException;

/**
 * RTFToText
 *
 */
public class RTFToText {

    /**
     * Parse document as RTF and return as Reader.
     * 
     * @param document
     * @return
     * @throws IOException
     * @throws BadLocationException
     */
    public static Reader toTextAsReader(
        InputStream document
    ) throws ServiceException {
    	try {
	        DefaultStyledDocument styledDoc = new DefaultStyledDocument();
	        new RTFEditorKit().read(
	            document, 
	            styledDoc, 
	            0
	        );
	       return new StringReader(
	            styledDoc.getText(
	                0, 
	                styledDoc.getLength()
	            )
	        );
    	} catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }

    /**
     * Parse document as RTF and return as String.
     * 
     * @param document
     * @return
     * @throws IOException
     * @throws BadLocationException
     */
    public static String toTextAsString(
        InputStream document
    ) throws ServiceException {
    	try {
	        DefaultStyledDocument styledDoc = new DefaultStyledDocument();
	        new RTFEditorKit().read(
	            document, 
	            styledDoc, 
	            0
	        );
	        return styledDoc.getText(
	            0, 
	            styledDoc.getLength()
	        );
    	} catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }

}
