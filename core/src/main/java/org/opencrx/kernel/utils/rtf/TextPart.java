/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TextPart
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

import java.awt.Font;

public class TextPart
    implements Text {

    public TextPart(String text) {
        rawrtf = false;
        bold = false;
        italic = false;
        underline = false;
        capitals = false;
        outline = false;
        shadow = false;
        animtext = 0;
        fontsize = 0;
        content = text;
    }

    protected TextPart(String rawtext, boolean raw) {
        rawrtf = false;
        bold = false;
        italic = false;
        underline = false;
        capitals = false;
        outline = false;
        shadow = false;
        animtext = 0;
        fontsize = 0;
        if(raw) {
            content = rawtext;
            rawrtf = true;
        } 
        else {
            content = rawtext;
        }
    }

    public TextPart(int format, String text) {
        this(text);
        if((format & 1) == 1)
            this.setBold(true);
        if((format & 2) == 2)
            this.setItalic(true);
        if((format & 4) == 4)
            this.setUnderline(true);
        if((format & 8) == 8)
            this.setCapitals(true);
        if((format & 0x10) == 16)
            this.setOutline(true);
        if((format & 0x20) == 32)
            this.setShadow(true);
    }

    public TextPart(int format, int fontsize, Font font, String text) {
        this(format, text);
        this.fontsize = fontsize;
    }

    public void setItalic(boolean value) {
        italic = value;
    }

    public boolean isItalic() {
        return italic;
    }

    public void setBold(boolean value) {
        bold = value;
    }

    public boolean isBold() {
        return bold;
    }

    public void setUnderline(boolean value) {
        underline = value;
    }

    public boolean isUnderline() {
        return underline;
    }

    public void setCapitals(boolean value) {
        capitals = value;
    }

    public boolean isCapitals() {
        return capitals;
    }

    public void setOutline(boolean value) {
        outline = value;
    }

    public boolean isOutline() {
        return outline;
    }

    public void setShadow(boolean value) {
        shadow = value;
    }

    public boolean isShadow() {
        return shadow;
    }

    public void setAnimtext(int animtext) {
        this.animtext = animtext;
    }

    public int getFontsize() {
        return fontsize;
    }

    public void setFontsize(int size) {
        fontsize = size;
    }

    public int getAnimtext() {
        return animtext;
    }

    public void setContent(String value) {
        content = value;
    }

    public String getRtfContent() {
        String format = (bold ? "\\b" : "") + (italic ? "\\i" : "") + (underline ? "\\ul" : "") + (capitals ? "\\caps" : "") + (outline ? "\\outl" : "") + (shadow ? "\\shad" : "") + (animtext <= 0 ? "" : "\\animtext" + animtext)  + (fontsize <= 0 ? "" : "\\fs" + fontsize * 2);
        return "{" + (format.length() <= 0 ? "" : format + " ") + (rawrtf ? content : RtfUtil.getRTFString(content)) + "}";
    }

    public static final int FORMAT_NORMAL = 0;
    public static final int FORMAT_BOLD = 1;
    public static final int FORMAT_ITALIC = 2;
    public static final int FORMAT_UNDERLINE = 4;
    public static final int FORMAT_CAPITALS = 8;
    public static final int FORMAT_OUTLINE = 16;
    public static final int FORMAT_SHADOW = 32;
    public static final int ANIMTEXT_NO = 0;
    public static final int ANIMTEXT_LASVEGAS_LIGHTS = 1;
    public static final int ANIMTEXT_BLINKING_BACKGROUND = 2;
    public static final int ANIMTEXT_SPARKLE_TEXT = 3;
    public static final int ANIMTEXT_MARCHING_BLACK_ANTS = 4;
    public static final int ANIMTEXT_MARCHING_RED_ANTS = 5;
    public static final int ANIMTEXT_SHIMMER = 6;
    public static final char SIGN_TAB = 9;
    public static final TextPart TABULATOR = new TextPart("\\tab", true);
    public static final TextPart NEWLINE = new TextPart("\\line", true);
    public static final TextPart SIGN_HARDSPACE = new TextPart("\\~", true);
    public static final TextPart SIGN_NON_REQUIRED_HYPHEN = new TextPart("\\-", true);
    public static final TextPart SIGN_NO_BREAKING_HYPHEN = new TextPart("\\_", true);
    private boolean rawrtf;
    private String content;
    private boolean bold;
    private boolean italic;
    private boolean underline;
    private boolean capitals;
    private boolean outline;
    private boolean shadow;
    private int animtext;
    private int fontsize;

}
