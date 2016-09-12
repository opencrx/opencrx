/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: IndexerServlet
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

import java.text.SimpleDateFormat;
import java.util.Date;

public class RtfUtil {

    public RtfUtil() {
    }

    public static String convertDateToRTFString(Date datum) {
        SimpleDateFormat formater = new SimpleDateFormat("'\\yr'yyyy'\\mo'MM'\\dy'dd'\\hr'HH'\\min'mm'\\sec'ss");
        return formater.format(datum);
    }

    public static String getRTFString(String text) {
        StringBuffer workstr = new StringBuffer();
        for(int i = 0; i < text.length(); i++) {
            char ch = text.charAt(i);
            switch(ch) {
            case 92: // '\\'
                workstr.append("\\\\");
                break;

            case 123: // '{'
                workstr.append("\\{");
                break;

            case 125: // '}'
                workstr.append("\\}");
                break;

            case 9: // '\t'
                workstr.append("{\\tab}");
                break;

            case 16: // '\020'
                workstr.append("{\\line}");
                break;

            default:
                if(ch < '\200') {
                    workstr.append(ch);
                } else {
                    workstr.append("\\u" + String.format("%04d", (int)ch) + "?");                    
                }
                break;
            }
        }
        return workstr.toString();
    }

    public static int getTwipFromMillimeter(double mm) {
        return (int)((mm / 25.399999999999999D) * 1440D + 0.5D);
    }

    public static double getMillimeterFromTwip(int twips) {
        return (twips * 25.399999999999999D) / 1440D;
    }

    public static int getIntegerFromText(
        String buf, 
        int startpos
    ) {
        int result = -1;
        int len;
        for(len = 0; Character.isDigit(buf.charAt(startpos + len)); len++) {}
        try {
            result = Integer.parseInt(buf.substring(startpos, startpos + len));
        }
        catch(NumberFormatException e) {
            result = -1;
        }
        return result;
    }

    public static int getPositionOfTagEnd(
        StringBuffer text, 
        int begin
    ) {
        int end = begin + 1;
        for(int count = 1; count > 0;) {
            if(text.charAt(end) == '{')
                count++;
            else
            if(text.charAt(end) == '}')
                count--;
            end++;
        }
        return end;
    }
    
    public static int getPositionOfTagEnd(
        String text, 
        int begin
    ) {
        int end = begin + 1;
        for(int count = 1; count > 0;) {
            if(text.charAt(end) == '{')
                count++;
            else
            if(text.charAt(end) == '}')
                count--;
            end++;
        }
        return end;
    }
    
}
