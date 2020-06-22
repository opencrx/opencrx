/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: RTFTemplate
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

import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

public class RTFTemplate {
    
    //-----------------------------------------------------------------------
    public RTFTemplate(
    ) throws IOException {
    }

    //-----------------------------------------------------------------------
    public void readFrom(
        Reader in,
        boolean closeReader
    ) throws IOException {
        this.content = new StringBuffer();
        int c;
        while((c = in.read()) >= 0) { 
            this.content.append((char)c);
        }
        if(closeReader) {
            in.close();
        }
    }

    //-----------------------------------------------------------------------
    public void writeTo(
        Writer out,
        boolean closeWriter
    )  throws IOException {
        String documentAsString = this.getDocumentAsString();
        out.write(documentAsString);
        if(closeWriter) {
            out.close();
        }
    }
    
    //-----------------------------------------------------------------------
    public void writeTo(
        OutputStream out
    )  throws IOException {
        String documentAsString = this.getDocumentAsString();
        out.write(documentAsString.getBytes());
    }
    
    //-----------------------------------------------------------------------
    public boolean existBookmark(
        String name
    ) {
        int pos = this.content.toString().indexOf("\\bkmkstart " + name + "}");
        return pos >= 0;
    }

    //-----------------------------------------------------------------------
    public Bookmark searchBookmark(
        String name
    ) throws BookmarkNotFoundException {
        Bookmark result = null;
        StringBuffer rtfdoc = this.content;
        int begin = rtfdoc.indexOf("\\bkmkstart " + name + "}");
        if(begin < 0) {
            begin = rtfdoc.indexOf("\\bkmkstart " + name + '\r');
        }
        if(begin >= 0) {
            int contentBegin;
            for(contentBegin = rtfdoc.indexOf("}", begin); rtfdoc.charAt(contentBegin) == '}'; contentBegin++) {}
            int end = rtfdoc.indexOf("\\bkmkend " + name + "}", contentBegin);
            if(end < 0) {
                end = rtfdoc.indexOf("\\bkmkend " + name + '\r', contentBegin);
            }
            if(end >= 0) {
                int contentEnd;
                for(contentEnd = rtfdoc.lastIndexOf("{", end); rtfdoc.charAt(contentEnd) == '{'; contentEnd--) {}
                contentEnd++;
                result = new Bookmark(
                    name, 
                    begin, 
                    RtfUtil.getPositionOfTagEnd(rtfdoc, end),
                    contentBegin, 
                    contentEnd, 
                    rtfdoc.substring(contentBegin, contentEnd)
                );
            } 
            else {
                throw new BookmarkNotFoundException(name, "Endtag of bookmark '" + name + "' not found!");
            }
        } 
        else {
            throw new BookmarkNotFoundException(name, "Bookmark '" + name + "' not found!");
        }
        return result;
    }

    //-----------------------------------------------------------------------
    public void appendTableRow(
        String bookmarkTableEnd
    ) throws BookmarkNotFoundException {
        Bookmark bmTableEnd = this.searchBookmark(bookmarkTableEnd);
        int pos = bmTableEnd.getContentBegin();
        // Search backward starting from bookmark the row start tag \row
        int nRowCount = 2;
        int[] posRow = new int[2];
        while(pos >= 0) { 
            if(this.content.substring(pos).startsWith("\\row ")) {
                nRowCount--;
                posRow[nRowCount] = pos;
                if(nRowCount == 0) break;
            }
            pos--;
        }
        // find tag \row starting from bmRowStart
        if(pos >= 0) {
            if((posRow[0] > 0) && (posRow[1] > posRow[0])) {
                String rowContent = this.content.substring(posRow[0], posRow[1]) + " ";
                this.content.insert(pos, rowContent);
            }
        }
    }
    
    //-----------------------------------------------------------------------
    private void setBookmarkRawContent(
        String bookmark, 
        String text,
        boolean removeBookmark
    ) throws BookmarkNotFoundException {
        Bookmark bm = this.searchBookmark(bookmark);
        if(removeBookmark) {
            this.content.replace(bm.getContentEnd(), bm.getEnd(), "{\\*\\bkmkend null}");
        }
        if(bm.isField()) {
            String bkmkcontent = bm.getRawContent();
            int contentPos = bkmkcontent.indexOf("\\fldrslt");
            if(contentPos >= 0) {
                int begin = bkmkcontent.lastIndexOf("{", contentPos);
                int end = RtfUtil.getPositionOfTagEnd(bkmkcontent, begin);
                this.content.replace(bm.getContentBegin() + begin, bm.getContentBegin() + end, "{\\fldrslt " + text + "}");
            } 
            else {
                int inspos = bkmkcontent.lastIndexOf("}");
                this.content.insert(bm.getContentBegin() + inspos, "{\\fldrslt " + text + "}");
            }
        } 
        else {            
            this.content.replace(
                bm.getContentBegin(), 
                bm.getContentEnd(), 
                text
            );
        }
        if(removeBookmark) {
            this.content.replace(bm.getBegin(), bm.getContentBegin(), "\\bkmkstart null}");
        }
    }

    //-----------------------------------------------------------------------
    public void setBookmarkContent(
        String bookmark, 
        String text,
        boolean removeBookmark
    ) throws BookmarkNotFoundException {
        this.setBookmarkRawContent(
            bookmark, 
            RtfUtil.getRTFString(text),
            removeBookmark
        );
    }

    //-----------------------------------------------------------------------
    public void setBookmarkContent(
        String bookmark, 
        String text
    ) throws BookmarkNotFoundException {
        this.setBookmarkRawContent(
            bookmark, 
            RtfUtil.getRTFString(text),
            false
        );
    }

    //-----------------------------------------------------------------------
    public void setBookmarkContent(
        String bookmark, 
        Text textparts,
        boolean removeBookmark
    ) throws BookmarkNotFoundException {
        this.setBookmarkRawContent(
            bookmark, 
            textparts.getRtfContent(),
            removeBookmark
        );
    }
    
    //-----------------------------------------------------------------------
    public void setBookmarkContent(
        String bookmark, 
        Text textparts
    ) throws BookmarkNotFoundException {
        this.setBookmarkRawContent(
            bookmark, 
            textparts.getRtfContent(),
            false
        );
    }
    
    //-----------------------------------------------------------------------
    public void setBookmarkCheckbox(
        String bookmark, 
        boolean value
    ) throws BookmarkNotFoundException, BookmarkIsNotCheckboxException {
        Bookmark bmi = this.searchBookmark(bookmark);
        String cbresult = value ? "1" : "25";
        if(bmi.isField()) {
            if(bmi.getFieldtype() == 1) {
                int posres = bmi.getRawContent().indexOf("\\ffres");
                if(posres >= 0) {
                    int len = 0;
                    int countstart;
                    for(
                        countstart = bmi.getContentBegin() + posres + 6; 
                        Character.isDigit(this.content.charAt(countstart + len)); 
                        len++
                    ) {}
                    this.content.replace(countstart, countstart + len, cbresult);
                } 
                else {
                    int postype = bmi.getRawContent().indexOf("\\fftype");
                    if(postype >= 0) {
                        this.content.insert(bmi.getContentBegin() + postype + 8, "\\ffres " + cbresult);
                    }
                    else {
                        throw new BookmarkNotFoundException(bookmark, "Typedefinition not found!");
                    }
                }
            } 
            else {
                throw new BookmarkIsNotCheckboxException(bookmark);
            }
        } 
        else {
            throw new BookmarkIsNotCheckboxException(bookmark);
        }
    }

    //-----------------------------------------------------------------------
    protected String getDocumentAsString(
    ) {
        return content.toString();
    }

    //-----------------------------------------------------------------------
    public List<String> getBookmarkNames(
    ) {
        List<String> allbm = new ArrayList<String>();
        for(
            int pos = 0; 
            (pos = this.content.indexOf("\\bkmkstart ", pos)) >= 0; 
            pos++
        ) {
            int end = this.content.indexOf("}", pos);
            allbm.add(this.content.substring(pos + 11, end));
        }
        return allbm;
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private StringBuffer content;
    
}
