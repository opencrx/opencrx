/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: WordToText
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2007, CRIXP Corp., Switzerland
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

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;

import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.hwpf.model.TextPiece;
import org.apache.poi.hwpf.usermodel.Paragraph;
import org.apache.poi.hwpf.usermodel.Range;
import org.openmdx.base.exception.ServiceException;

/**
 * Re-factored from 
 * http://svn.apache.org/viewvc/poi/trunk/src/scratchpad/src/org/apache/poi/hwpf/extractor/WordExtractor.java?view=log
 */
public class WordToText {

    /**
     * Get the text from the word file, as an array with one String
     *  per paragraph
     */
    public String[] getParagraphText(
        HWPFDocument doc            
    ) {
        String[] ret = new String[]{};   
        try {
            Range r = doc.getRange();
            ret = new String[r.numParagraphs()];
            for(int i = 0; i < ret.length; i++) {
                Paragraph p = r.getParagraph(i);
                ret[i] = p.text();
                if(ret[i].endsWith("\r")) {
                    ret[i] = ret[i] + "\n";
                }
            }
        } catch(Exception e) {
            // Something's up with turning the text pieces into paragraphs
            // Fall back to ripping out the text pieces
            ret = new String[1];
            ret[0] = this.getTextFromPieces(doc);
        }        
        return ret;
    }
    
    /**
     * Grab the text out of the text pieces. Might also include various
     * bits of crud, but will work in cases where the text piece -> paragraph
     * mapping is broken. Fast too.
     */
    public String getTextFromPieces(
        HWPFDocument doc            
    ) {
        StringBuffer textBuf = new StringBuffer();        
        Iterator<TextPiece> textPieces = doc.getTextTable().getTextPieces().iterator();
        while(textPieces.hasNext()) {
            TextPiece piece = textPieces.next();
            String encoding = "Cp1252";
            if (piece.isUnicode()) {
                encoding = "UTF-16LE";
            }
            try {
                String text = new String(piece.getRawBytes(), encoding);
                textBuf.append(text);
            } 
            catch(UnsupportedEncodingException e) {}
        }        
        String text = textBuf.toString();        
        // Fix line endings (Note - won't get all of them
        text = text.replaceAll("\r\r\r", "\r\n\r\n\r\n");
        text = text.replaceAll("\r\r", "\r\n\r\n");        
        if(text.endsWith("\r")) {
            text += "\n";
        }        
        return text;
    }

    /**
     * Gets the text from a Word document.
     * 
     * @param in The InputStream representing the Word file.
     */
    public Reader parse(
        InputStream in
    ) throws ServiceException {
    	try {
	        HWPFDocument doc = new HWPFDocument(
	            HWPFDocument.verifyAndBuildPOIFS(in)
	        );        
	        StringBuilder text = new StringBuilder();
	        String[] paragraphs = this.getParagraphText(doc);
	        for(String paragraph: paragraphs) {
	            text.append(paragraph);
	        }
	        return new StringReader(text.toString());
    	} catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }

}
