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

/**
 * The <code>MAPIPropName</code> class encapsulates the name of a named MAPI property.
 * A MAPI Property name consists of a GUID, as well as either a 32-bit identifier
 * or a String.
 *
 * @author Amichai Rothman
 * @since 2003-07-25
 */
public class MAPIPropName {

    /**
     * MAPI property name type constant.
     */
    public static final int
        MNID_ID     = 0,
        MNID_STRING = 1;

    GUID    guid;
    int     type;
    long    ID;
    String  name;
    int rawLength;

    /**
     * Constructs a MAPIPropName using the given TNEF stream.
     *
     * @param data the TNEF stream containing the property name data
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public MAPIPropName(RawInputStream data) throws IOException {
        // get TRP structure values
        long startOffest = data.getPosition();
        this.guid = new GUID(data.readBytes(16));
        this.type = (int)data.readU32();
        if (type == MNID_STRING) {
            int length = (int)data.readU32();
            this.name = data.readStringUnicode(length);
            if (length % 4 != 0) // pad to 4 byte boundary
                data.skip(4 - (length % 4));
            this.rawLength += length;
        } else if (type == MNID_ID) {
            this.ID = data.readU32();
        } else {
            throw new IOException("invalid type: " + type);
        }
        this.rawLength = (int)(data.getPosition() - startOffest);
    }

    /**
     * Constructs a MAPIPropName containing given values.
     *
     * @param guid the property GUID
     * @param ID the property ID
     */
    public MAPIPropName(GUID guid, long ID) {
        this.guid = guid;
        this.type = MNID_ID;
        this.ID = ID;
    }

    /**
     * Constructs a MAPIPropName containing given values
     *
     * @param guid the property GUID
     * @param name the property name
     */
    public MAPIPropName(GUID guid, String name) {
        this.guid = guid;
        this.type = MNID_STRING;
        this.name = name;
    }

    /**
     * Gets the MAPIPropName GUID.
     *
     * @return the MAPIPropName GUID
     */
    public GUID getGUID() {
        return this.guid;
    }

    /**
     * Gets the MAPIPropName type.
     *
     * @return the MAPIPropName type
     */
    public int getType() {
        return this.type;
    }

    /**
     * Gets the MAPIPropName ID.
     *
     * @return the MAPIPropName ID
     */
    public long getID() {
        return this.ID;
    }

    /**
     * Gets the MAPIPropName name.
     *
     * @return the MAPIPropName name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Returns a string representation of the object.
     *
     * @return a string representation of the object
     */
    public String toString() {
        StringBuffer s = new StringBuffer()
            .append("GUID=").append(guid)
            .append(" Name=").append(type == MNID_STRING ? name : "0x"+Long.toHexString(ID));
        return s.toString();
    }

    /**
     * Returns whether this MAPIPropName is identical to the given MAPIPropName.
     *
     * @return <code>true</code> if this object is the same as the obj
     *         argument; <code>false</code> otherwise
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof MAPIPropName))
            return false;
        MAPIPropName p = (MAPIPropName)o;
        return
            this.type == p.type
            && (this.type == MNID_ID ? this.ID == p.ID : this.name.equals(p.name))
            && this.guid.equals(p.guid);
    }

    /**
     * Returns a hash code value for the object.
     *
     * @return a hash code value for this object
     */
    public int hashCode() {
        int result = 17;
        result = result * 37 + type;
        result = result * 37 + guid.hashCode();
        if (type == MNID_ID)
            result = result * 37 + (int)(ID ^ (ID >>> 32));
        if (type == MNID_STRING && name != null)
            result = result * 37 + name.hashCode();
        return result;
    }

    /**
     * Gets the raw data length (in bytes) of this instance.
     *
     * @return the raw data length (in bytes) of this instance
     */
    protected int getRawLength() {
        return this.rawLength;
    }

}
