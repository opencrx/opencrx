/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Bookmark
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
package org.opencrx.kernel.utils.rtf;

public class Bookmark {

    public Bookmark(
        String name, 
        int begin,
        int end,
        int contentBegin, 
        int contentEnd, 
        String rawcontent
    ) {
        this.name = name;
        this.begin = begin;
        this.end = end;
        this.contentbegin = contentBegin;
        this.contentend = contentEnd;
        this.rawcontent = rawcontent;
        if(rawcontent != null && rawcontent.indexOf("\\formfield") >= 0)
            this.field = true;
        else
            this.field = false;
    }
    
    public int getBegin(
    ) {
        return this.begin;
    }
    
    public int getEnd(
    ) {
        return this.end;
    }
    
    public int getContentBegin(
    ) {
        return contentbegin;
    }

    public int getContentEnd() {
        return contentend;
    }

    public boolean isField() {
        return field;
    }

    public boolean isFieldReadOnly() {
        boolean result = false;
        if(this.isField())
            if(rawcontent != null && rawcontent.indexOf("\\fldlock") >= 0)
                result = true;
            else
            if(rawcontent != null && rawcontent.indexOf("\\ffprot1") >= 0)
                result = true;
            else
                result = false;
        return result;
    }

    public int getFieldMaxLength() {
        int result = -1;
        if(this.isField() && rawcontent != null) {
            int pos = rawcontent.indexOf("\\ffmaxlen");
            if(pos >= 0)
                result = RtfUtil.getIntegerFromText(rawcontent, pos + 9);
        }
        return result;
    }

    public int getFieldtype() {
        int result = -1;
        if(this.field) {
            int posfd = rawcontent.indexOf("\\fftype");
            if(posfd >= 0)
                switch(rawcontent.charAt(posfd + 7)) {
                case 48: // '0'
                    result = 0;
                    break;

                case 49: // '1'
                    result = 1;
                    break;

                case 50: // '2'
                    result = 2;
                    break;

                default:
                    result = -1;
                    break;
                }
        }
        return result;
    }

    public String getName(
    ) {
        return this.name;
    }
    
    public String getRawContent() {
        return rawcontent;
    }

    public static final int TYPE_NIL = -1;
    public static final int TYPE_TEXT = 0;
    public static final int TYPE_CHECKBOX = 1;
    public static final int TYPE_LISTBOX = 2;
    private String name;
    private int begin;
    private int end;
    private int contentbegin;
    private int contentend;
    private boolean field;
    private String rawcontent;

}