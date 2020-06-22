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

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;

/**
 * The <code>TNEFUtils</code> class provides utility methods used by the TNEF
 * processing classes.
 *
 * @author Amichai Rothman
 * @since 2003-04-25
 */
public class TNEFUtils {

    /**
     * Returns an unsigned 8-bit value from a byte array.
     *
     * @param buf a byte array from which byte value is taken
     * @param offset the offset within buf from which byte value is taken
     * @return an unsigned 8-bit value as an int
     */
    public static int getU8(byte[] buf, int offset) {
        return buf[offset] & 0xFF;
    }

    /**
     * Returns an unsigned 16-bit value from little-endian ordered bytes.
     *
     * @param b1 first byte value
     * @param b2 second byte value
     * @return an unsigned 16-bit value as an int
     */
    public static int getU16(int b1, int b2) {
        return ((b1 & 0xFF) | ((b2 & 0xFF) << 8)) & 0xFFFF;
    }

    /**
     * Returns an unsigned 16-bit value from little-endian ordered bytes.
     *
     * @param buf a byte array from which byte values are taken
     * @param offset the offset within buf from which byte values are taken
     * @return an unsigned 16-bit value as an int
     */
    public static int getU16(byte[] buf, int offset) {
        return ((buf[offset] & 0xFF) | ((buf[offset + 1] & 0xFF) << 8)) & 0xFFFF;
    }

    /**
     * Returns an unsigned 32-bit value from little-endian ordered bytes.
     *
     * @param b1 first byte value
     * @param b2 second byte value
     * @param b3 third byte value
     * @param b4 fourth byte value
     * @return an unsigned 32-bit value as a long
     */
    public static long getU32(int b1, int b2, int b3, int b4) {
        return ((b1 & 0xFF) | ((b2 & 0xFF) << 8) | ((b3 & 0xFF) << 16) | ((b4 & 0xFF) << 24)) & 0x00000000FFFFFFFFL;
    }

    /**
     * Returns an unsigned 32-bit value from little-endian ordered bytes.
     *
     * @param buf a byte array from which byte values are taken
     * @param offset the offset within buf from which byte values are taken
     * @return an unsigned 32-bit value as a long
     */
    public static long getU32(byte[] buf, int offset) {
        return ((buf[offset] & 0xFF) | ((buf[offset + 1] & 0xFF) << 8) |
               ((buf[offset + 2] & 0xFF) << 16) | ((buf[offset + 3] & 0xFF) << 24)) & 0x00000000FFFFFFFFL;
    }

    /**
     * Returns a 64-bit value from little-endian ordered bytes.
     *
     * @param buf a byte array from which byte values are taken
     * @param offset the offset within buf from which byte values are taken
     * @return a 64-bit value as a long
     */
    public static long getU64(byte[] buf, int offset) {
        return ((getU32(buf, offset + 4) & 0x00000000FFFFFFFFL) << 32) |
                (getU32(buf, offset) & 0x00000000FFFFFFFFL);
    }

    /**
     * Returns a 32-bit value containing a combined attribute type and ID.
     *
     * @param atp the attribute type
     * @param id the attribute ID
     * @return a 32-bit value containing a combined attribute type and ID
     */
    public static int attribute(int atp, int id) {
        return ((atp << 16) | id);
    }

    /**
     * Returns the ID part of a 32-bit combined attribute type and ID value.
     *
     * @param att the combined attribute type and ID value
     * @return the ID part of a 32-bit combined attribute type and ID value
     */
    public static int attID(int att) {
        return (att & 0x0000FFFF);
    }

    /**
     * Returns the type part of a 32-bit combined attribute type and ID value.
     *
     * @param att the combined attribute type and ID value
     * @return the type part of a 32-bit combined attribute type and ID value
     */
    public static int attType(int att) {
        return ((att >> 16) & 0x0000FFFF);
    }

    /**
     * Returns the checksum of a given byte array.
     *
     * @param data the byte array on which to calculate the checksum
     * @return the checksum of a given byte array
     */
    public static int calculateChecksum(byte[] data) {
        return calculateChecksum(data, 0, data.length);
    }

    /**
     * Returns the checksum of a range of bytes within a given byte array.
     *
     * @param data the byte array on which to calculate the checksum
     * @param offset the offset within the array from which to begin
     * @param length the number of bytes to calculate checksum on
     * @return the checksum of a range of bytes within a given byte array
     */
    public static int calculateChecksum(byte[] data, int offset, int length) {
        // NOTE: the AND operation expands the byte to an int containing the
        // unsigned byte value. This is necessary since a Java byte is signed.
        long checksum = 0;
        length += offset; // now marks the end index itself
        for (int i = offset; i < length; i++)
            checksum += (data[i] & 0xFF);

        return (int)(checksum % 65536);
    }

    /**
     * Returns the checksum of all the data in a given RawInputStream.
     * The stream's current position is not modified.
     *
     * @param ris the stream from which the data is read
     * @return the checksum of all the data in a given RawInputStream
     * @throws IOException if an I/O error occurs
     */
    public static int calculateChecksum(RawInputStream ris) throws IOException {
        int checksum = 0;
        int read;
        byte[] buf = new byte[4096];
        RawInputStream r = new RawInputStream(ris); // make a copy, original is unmodified
        try {
            while ((read = r.read(buf)) != -1)
                checksum = (checksum + calculateChecksum(buf, 0, read)) % 65536;
        } finally {
            r.close();
        }

        return checksum;
    }

    /**
     * Returns the name of a constant which is defined in given Class,
     * has a name beginning with given prefix, and has given value. If the
     * constant's name cannot be found, the value is returned as a hex String.
     *
     * @param cls the Class containing the constant
     * @param constPrefix the prefix of the constant name (used in grouping constants)
     * @param value the constant's value
     * @return the name of the constant
     */
    public static String getConstName(Class cls, String constPrefix, long value) {
        Field[] fields = cls.getFields();
        try {
            for (int i = 0; i < fields.length; i++) {
                if (fields[i].getName().startsWith(constPrefix) &&
                        fields[i].getLong(null) == value)
                    return fields[i].getName();
            }
        } catch (IllegalAccessException ignore) {}
        return "0x" + Long.toHexString(value);
    }

    /**
     * Removes all null characters ('\0') from the end of a given String.
     * Useful for converting a C-style null terminated string to a Java String.
     *
     * @param s a String
     * @return a String identical to the given string, with trailing null
     *         characters removed
     */
    public static String removeTerminatingNulls(String s) {
        if (s == null)
            return null;
        int len = s.length();
        while (len > 0 && s.charAt(len - 1) == '\0')
            len--;
        return len == s.length() ? s : s.substring(0, len);
    }

    /**
     * Replaces all occurrences of given substring within string with a replacement
     * string.
     *
     * @param s the string to be modified
     * @param search the substring to search for
     * @param replace the string with which to replace occurrences of the search substring
     * @return a new string consisting of the given string, with all occurrences
     *         of search string replaced by replace string. If given string or
     *         search string are empty or null, the string itself is returned.
     */
    public static String replace(String s, String search, String replace) {
        if (s == null || search == null || search.length() == 0)
            return s;
        if (replace == null)
            replace = "";
        int len = s.length();
        int slen = search.length();
        int rlen = replace.length();
        int ind = 0;
        while (ind < len && (ind = s.indexOf(search, ind)) > -1) {
            s = s.substring(0, ind) + replace + s.substring(ind + slen);
            ind += rlen;
        }
        return s;
    }

    /**
     * Creates a String from a C-style null terminated byte sequence.
     *
     * The null terminated byte sequence is interpreted as 8-bit ISO-8859-1
     * characters (a.k.a. ISO-Latin-1), which is a superset of US-ASCII.
     * This way we don't lose any 8-bit values and remain fully compatible:
     * If the source charset is unknown, a str.getByte("ISO8859_1") will
     * reconstruct the exact original byte sequence which the application
     * can then process in any charset it sees fit.
     *
     * @param bytes a byte array containing a C-style null terminated string
     * @param offset the offset within bytes where the string begins
     * @param length the length of the C-style string in bytes, which may
     *        include any number of terminating null ('\0') characters
     * @return a String containing the C-style string's characters, interpreted
     *         as ISO-8859-1 characters
     */
    public static String createString(byte[] bytes, int offset, int length) {
        try {
            return removeTerminatingNulls(new String(bytes, offset, length, "ISO8859_1"));
        } catch (UnsupportedEncodingException ignore) {}
        return "";
    }

    /**
     * Creates a String from a C-style null terminated Unicode byte sequence.
     *
     * The null terminated byte sequence is interpreted as 16-bit Unicode
     * characters (UTF-16), stored in Little Endian order.
     *
     * @param bytes a byte array containing a C-style null terminated Unicode string
     * @param offset the offset within bytes where the string begins
     * @param length the length of the C-style string in bytes, which may
     *        include any number of terminating null ('\0') characters
     * @return a String containing the C-style string's characters, interpreted
     *         as Unicode (UTF-16 Little Endian) characters
     */
    public static String createStringUnicode(byte[] bytes, int offset, int length) {
        try {
            return removeTerminatingNulls(new String(bytes, offset, length, "UTF-16LE"));
        } catch (UnsupportedEncodingException ignore) {}
        return "";
    }

    /**
     * Creates a String containing the hexadecimal representation of the given
     * bytes.
     *
     * @param bytes a byte array whose content is to be displayed
     * @return a String containing the hexadecimal representation of the given
     *         bytes
     */
    public static String toHexString(byte[] bytes) {
        return toHexString(bytes, 0, bytes != null ? bytes.length : 0, -1);
    }

    /**
     * Creates a String containing the hexadecimal representation of the given
     * bytes.
     *<p>
     * If {@code max} is non-negative and {@code bytes.length > max}, then the
     * first {@code max} bytes are returned, followed by a human-readable
     * indication that there are {@code bytes.length} total bytes of data
     * including those that are not returned.
     *
     * @param bytes a byte array whose content is to be displayed
     * @param max the maximum number of bytes to be displayed (-1 means no limit)
     * @return a String containing the hexadecimal representation of the given
     *         bytes
     */
    public static String toHexString(byte[] bytes, int max) {
        return toHexString(bytes, 0, bytes != null ? bytes.length : 0, max);
    }

    /**
     * Creates a String containing the hexadecimal representation of the given
     * bytes.
     * <p>
     * If {@code max} is non-negative and {@code len > max}, then the
     * first {@code max} bytes are returned, followed by a human-readable
     * indication that there are {@code len} total bytes of data
     * including those that are not returned.
     * <p>
     * In particular, {@code offset + len} can extend beyond the array boundaries,
     * as long as {@code offset + max} is still within them, resulting in
     * {@code max} bytes returned followed by an indication that there are
     * {@code len} total data bytes (including those that are not returned).
     *
     * @param bytes a byte array whose content is to be displayed
     * @param offset the offset within the byte array to start at
     * @param len the number of bytes
     * @param max the maximum number of bytes to be displayed (-1 means no limit)
     * @return a String containing the hexadecimal representation of the given
     *         bytes
     */
    public static String toHexString(byte[] bytes, int offset, int len, int max) {
        if (bytes == null)
            return "[null]";
        int count = max > -1 && max < len ? max : len;
        StringBuffer s = new StringBuffer();
        s.append('[');
        for (int i = 0; i < count; i++) {
            String b = Integer.toHexString(bytes[offset + i] & 0xFF).toUpperCase();
            if (b.length() == 1)
                s.append('0');
            s.append(b);
        }
        if (count < len)
            s.append("... (" + len + " bytes)");
        s.append(']');
        return s.toString();
    }

    /**
     * Checks whether the given string contains a TNEF mime type
     *
     * @param mimeType the mimeType to check
     * @return true if the given string contains a TNEF mime type,
     *         false otherwise
     */
    static public boolean isTNEFMimeType(String mimeType) {
        // note that application/ms-tnefx was also observed in the wild
        return mimeType != null
            && ((mimeType = mimeType.toLowerCase()).startsWith("application/ms-tnef")
                || mimeType.startsWith("application/vnd.ms-tnef"));
    }

    /**
     * Converts a GUID string to a byte array.
     * A GUID string can contain only hex digits, and optional dashes.
     * All dashes are ignored.
     *
     * @param guid the GUID string
     * @return the equivalent GUID byte array
     * @throws IllegalArgumentException if the given string does not
     *         represent a valid GUID
     * @deprecated use the GUID class for GUID manipulation
     */
    public static byte[] toGUID(String guid) throws IllegalArgumentException {
        return new GUID(guid).toByteArray();
    }

    /**
     * Calculates the CRC32 of the given bytes.
     * The CRC32 calculation is similar to the standard one as demonstrated
     * in RFC 1952, but with the inversion (before and after the calculation)
     * omitted.
     *
     * @param buf the byte array to calculate CRC32 on
     * @param off the offset within buf at which the CRC32 calculation will start
     * @param len the number of bytes on which to calculate the CRC32
     * @return the CRC32 value
     * @deprecated use {@link CompressedRTFInputStream#calculateCRC32} instead
     */
    public static int calculateCRC32(byte[] buf, int off, int len) {
        return CompressedRTFInputStream.calculateCRC32(buf, off, len);
    }

    /**
     * Decompresses compressed-RTF data.
     *
     * @param src the compressed-RTF data bytes
     * @return an array containing the decompressed bytes
     * @throws IllegalArgumentException if src does not contain valid
     *         compressed-RTF bytes
     * @deprecated use {@link CompressedRTFInputStream#decompressRTF} instead
     */
    public static byte[] decompressRTF(byte[] src) {
        return CompressedRTFInputStream.decompressRTF(src);
    }

    /**
     * Reads bytes from a stream. The number of bytes read is no less
     * than the given minimum and no more than the given maximum.
     *
     * @param in the input stream to read from
     * @param b the byte array to read into
     * @param off the offset within the byte array to start reading into
     * @param min the minimum number of bytes to read
     * @param max the maximum number of bytes to read
     * @return the number of bytes read
     * @throws IOException if an error occurs or there are less
     *         than the minimum number of bytes in the stream
     */
    public static int read(InputStream in, byte[] b, int off, int min, int max) throws IOException {
        int total = 0;
        while (total < min) {
            int read = in.read(b, off + total, max - total);
            if (read < 0)
                throw new IOException("Unexpected end of stream");
            total += read;
        }
        return total;
    }

}
