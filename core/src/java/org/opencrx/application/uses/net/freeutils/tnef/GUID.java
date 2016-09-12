/*
 *  Copyright © 2003-2012 Amichai Rothman
 *
 *  This file is part of JTNEF - the Java TNEF package.
 *
 *  JTNEF is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  JTNEF is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with JTNEF.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  For additional info see http://www.freeutils.net/source/jtnef/
 */

package org.opencrx.application.uses.net.freeutils.tnef;

/**
 * The <code>GUID</code> class encapsulates a GUID (Globally Unique IDentifier).
 * GUID instances are immutable.
 *
 * @author Amichai Rothman
 * @since 2007-07-19
 */
public class GUID {

    String guid; // in canonized form

    /**
     * Constructs a GUID with the specified value.
     *
     * @param guid the GUID value as a hex string (with optional canonical dashes)
     * @throws IllegalArgumentException if the given string does not
     *         contain a valid GUID
     */
    public GUID(String guid) {
        this.guid = canonize(guid);
    }

    /**
     * Constructs a GUID with the specified value.
     *
     * @param guid the GUID value
     * @throws IllegalArgumentException if the given array does not
     *         contain a valid GUID
     */
    public GUID(byte[] guid) {
        StringBuffer s = new StringBuffer(32);
        for (int i = 0; i < guid.length; i++) {
            String b = Integer.toHexString(guid[i] & 0xFF);
            if (b.length() == 1)
                s.append('0');
            s.append(b);
        }
        this.guid = canonize(s.toString());
    }

    /**
     * Canonizes the given GUID string into the canonical format of the form:
     * "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", with all values presented as
     * lowercase hex digits.
     *
     * Valid GUIDs consist of 32 case insensitive hex digits (representing 16 bytes),
     * with or without the separating dashes in the appropriate positions.
     *
     * @param guid the GUID value
     * @return the canonized GUID string
     * @throws IllegalArgumentException if the given string does not
     *         contain a valid GUID
     */
    public static String canonize(String guid) {
        guid = guid.toLowerCase();
        int len = guid.length();
        if (len != 32 && len != 36)
            throw new IllegalArgumentException("invalid GUID: " + guid
                + " (string length must be 32 without dashes, or 36 with dashes)");

        char[] chars = new char[36];
        for (int src = 0, dst = 0; src < len; src++, dst++) {
            char c = guid.charAt(src);
            if (dst == 8 || dst == 13 || dst == 18 || dst == 23) {
                chars[dst++] = '-';
                if (len == 36) {
                    if (c != '-')
                        throw new IllegalArgumentException("invalid GUID: " + guid
                            + " ('-' expected at position " + src + ")");
                    c = guid.charAt(++src);
                }
            }
            if (c < '0' || c > 'f' || (c > '9' && c < 'a'))
                throw new IllegalArgumentException("invalid GUID: " + guid
                    + " (invalid hex character at position " + src + ": '" + c + "')");
            chars[dst] = c;
        }
        return new String(chars);
    }

    /**
     * Returns the GUID as a 16-byte array.
     *
     * @return the GUID as a 16-byte array
     */
    public byte[] toByteArray() {
        byte[] b = new byte[16];
        for (int src = 0, dst = 0; dst < 16; src += 2, dst++) {
            if (src == 8 || src == 13 || src == 18 || src == 23)
                src++;
            b[dst] = (byte)(Integer.parseInt(guid.substring(src, src + 2), 16) & 0xFF);
        }
        return b;
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        return guid;
    }

    /**
     * Returns whether this GUID is identical to the given GUID.
     *
     * @return <code>true</code> if this object is the same as the obj
     *         argument; <code>false</code> otherwise
     */
    public boolean equals(Object o) {
        return o instanceof GUID && guid.equals(((GUID)o).guid);
    }

    /**
     * Returns a hash code value for the object.
     *
     * @return a hash code value for this object
     */
    public int hashCode() {
        return guid.hashCode();
    }

}
