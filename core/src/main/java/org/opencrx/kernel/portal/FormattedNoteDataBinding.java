/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: NoteDataBinding
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2008, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal;

import javax.jmi.reflect.RefObject;

import org.opencrx.kernel.base.jmi1.Note;
import org.openmdx.base.text.conversion.HtmlEncoder;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DefaultDataBinding;

public class FormattedNoteDataBinding extends DefaultDataBinding {

    //-----------------------------------------------------------------------
    public FormattedNoteDataBinding(
    ) {
    }
    
    //-----------------------------------------------------------------------
    @Override
    public Object getValue(
        RefObject object, 
        String qualifiedFeatureName,
        ApplicationContext app
    ) {
        if(object instanceof Note) {
            Note note = (Note)object;
            String title = note.getTitle();
            String text = note.getText();
            if((title == null) || (title.length() == 0)) {
                return text;
            }
            else {
                int titleLength = title.length();
                int indexBol = 0;
                int indexEol = 0;
                boolean containsWiki = false;
                boolean containsHtml = false;
                if(text != null) {
                    while((indexEol = text.indexOf('\n', indexBol)) > 0) {
                        titleLength = Math.max(titleLength, indexEol - indexBol);
                        indexBol = indexEol + 1;                    
                    }
                    containsWiki = HtmlEncoder.containsWiki(text);
                    containsHtml = HtmlEncoder.containsHtml(text);
                }
                titleLength = Math.min(140, titleLength);                
                StringBuilder formattedText = new StringBuilder(containsWiki ? "*" : "<b>");
                formattedText.append(title.replace(" ", "&nbsp;"));
                boolean isFirst = true;
                for(int i = title.length(); i < titleLength; i++) {
                	if(isFirst) formattedText.append(" "); // separator in case title is an URL
                    formattedText.append("&nbsp;");
                    isFirst = false;
                }
                formattedText
                    .append(containsWiki ? "*\\\\ " : "</b><br />")
                    .append(text == null ? "" : containsWiki || containsHtml ? text : text.replaceAll("\n", "<br />"));
                return formattedText.toString();
            }
        }
        else {
            return null;
        }
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    
}
