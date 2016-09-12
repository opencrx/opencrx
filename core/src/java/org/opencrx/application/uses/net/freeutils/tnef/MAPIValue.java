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
import java.util.Date;

/**
 * The <code>MAPIProp</code> class encapsulates the value of a MAPI property.
 *
 * @author Amichai Rothman
 * @since 2003-07-25
 */
public class MAPIValue {

    int type;
    RawInputStream rawData;

    /**
     * Constructs a MAPIValue containing a given value.
     *
     * @param type the value type
     * @param data the TNEF stream containing the value
     * @param length the length of the value data (in bytes)
     * @throws IllegalArgumentException if type is invalid
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public MAPIValue(int type, RawInputStream data, int length) throws IOException {
        if ((type & MAPIProp.MV_FLAG) != 0)
            throw new IllegalArgumentException("multivalue is not allowed in single MAPIValue");
        this.type = type;
        this.rawData = new RawInputStream(data, 0, length);
        data.skip(length);
    }

    /**
     * Gets the MAPIValue type.
     *
     * @return the MAPIValue type
     */
    public int getType() {
        return this.type;
    }

    /**
     * Gets the MAPIValue data length (in bytes).
     *
     * @return the MAPIValue data length (in bytes)
     */
    public int getLength() {
        return (int)rawData.getLength();
    }

    /**
     * Gets the MAPIValue data.
     *
     * @return the MAPIValue data
     */
    public byte[] getData() {
        byte[] b = null;
        try {
            b = this.rawData.toByteArray();
        } catch (IOException ignore) {}
        return b;
    }

    /**
     * Gets the MAPIValue raw data.
     *
     * @return the MAPIValue raw data
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public RawInputStream getRawData() throws IOException {
        return new RawInputStream(this.rawData);
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        try {
            Object o = getValue();
            if (o instanceof RawInputStream) {
                o = o.toString();
            } else if (o instanceof byte[]) {
                o = TNEFUtils.toHexString((byte[])o, 512);
            }
            return String.valueOf(o);
        } catch (IOException ioe) {
            throw new RuntimeException(ioe.toString());
        }
    }

    /**
     * Returns the value encapsulated by this MAPIValue.
     * The returned Object should be cast into an appropriate class,
     * according to the MAPIValue's type.
     *
     * @return the value encapsulated by this MAPIValue
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public Object getValue() throws IOException {
        Object o = null;
        RawInputStream ris = new RawInputStream(this.rawData); // a copy
        try {
            switch (type) {
                case MAPIProp.PT_NULL:
                    break;

                case MAPIProp.PT_SHORT:
                    // 2 bytes
                    o = new Short((short)ris.readU16());
                    break;

                case MAPIProp.PT_INT:
                    // 4 bytes
                    o = new Integer((int)ris.readU32());
                    break;

                case MAPIProp.PT_BOOLEAN:
                    // 4 bytes
                    o = Boolean.valueOf(ris.readU32() != 0);
                    break;

                case MAPIProp.PT_FLOAT:
                    // 4 bytes
                    o = new Float(Float.intBitsToFloat((int)ris.readU32()));
                    break;

                case MAPIProp.PT_ERROR:
                    // 4 bytes
                    o = new Integer((int)ris.readU32());
                    break;

                case MAPIProp.PT_APPTIME:
                case MAPIProp.PT_SYSTIME:
                    // 64-bit Windows FILETIME is 100ns since January 1, 1601 (UTC)
                    if (ris.getLength() == 8) {
                        long time = ris.readU64();
                        time = time / 10 / 1000; // to milliseconds
                        // subtract milliseconds between 1/1/1601 and 1/1/1970 (including 89 leap year days)
                        time = time - 1000L*60L*60L*24L*(365L*369L+89L);
                        o = new Date(time);
                    }
                    break;

                case MAPIProp.PT_DOUBLE:
                    // 8 bytes
                    o = new Double(Double.longBitsToDouble(ris.readU64()));
                    break;

                case MAPIProp.PT_CURRENCY:
                case MAPIProp.PT_INT8BYTE:
                    // 8 bytes
                    o = new long[] { ris.readU32(), ris.readU32() };
                    break;

                case MAPIProp.PT_CLSID:
                    // CLSID - 16 bytes
                    o = ris.toByteArray();
                    break;

                case MAPIProp.PT_STRING:
                    // variable length string
                    o = ris.readString((int)ris.getLength());
                    break;

                case MAPIProp.PT_UNICODE_STRING:
                    // variable length string
                    o = ris.readStringUnicode((int)ris.getLength());
                    break;

                case MAPIProp.PT_BINARY:
                case MAPIProp.PT_UNSPECIFIED:
                    // variable length bytes
                    o = ris;
                    ris = null; // it is returned so it shouldn't be closed
                    break;

                case MAPIProp.PT_OBJECT:
                    // get object IID
                    GUID iid = new GUID(ris.readBytes(16));
                    o = ris;
                    ris = null; // it is returned so it shouldn't be closed
                    if (iid.equals(MAPIProp.IID_IMESSAGE))
                        o = new TNEFInputStream((RawInputStream)o);
                    break;

                default:
                    throw new IOException("Unknown MAPI type: " + type);
            }
        } finally {
            if (ris != null)
                ris.close();
        }
        return o;
    }

}
