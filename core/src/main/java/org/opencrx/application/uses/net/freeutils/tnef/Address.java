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
 * The <code>Address</code> class encapsulates an address
 * used in some TNEF message attributes.
 *
 * @author Amichai Rothman
 * @since 2003-04-25
 */
public class Address {

    String displayName;
    String address;
    String type;

    /**
     * Constructs an empty Address.
     */
    public Address() {}

    /**
     * Constructs an Address using the given TNEF stream.
     *
     * @param data the TNEF stream to parse address from
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public Address(RawInputStream data) throws IOException {
        int displayNameLength = data.readU16();
        this.displayName = data.readString(displayNameLength);
        int addressLength = data.readU16();
        String typeAndAddress = data.readString(addressLength);
        int separator = typeAndAddress.indexOf(':');
        this.type = typeAndAddress.substring(0, separator);
        this.address = typeAndAddress.substring(separator + 1);
    }

    /**
     * Constructs a Address containing the specified values.
     *
     * @param displayName the display name associated with address
     * @param type the address type
     * @param address the address
     */
    public Address(String displayName, String type, String address) {
        this.displayName = displayName;
        this.type = type;
        this.address = address;
    }

    /**
     * Gets the Address display name.
     *
     * @return the Address display name
     */
    public String getDisplayName() {
        return this.displayName;
    }

    /**
     * Gets the address.
     *
     * @return the address
     */
    public String getAddress() {
        return this.address;
    }

    /**
     * Gets the Address type.
     *
     * @return the Address type
     */
    public String getType() {
        return this.type;
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        return new StringBuffer().append('"').append(getDisplayName()).append("\" [")
            .append(getType()).append(':').append(getAddress()).append(']').toString();
    }

}
