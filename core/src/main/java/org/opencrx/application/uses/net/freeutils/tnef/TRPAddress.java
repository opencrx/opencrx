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
 * The <code>TRPAddress</code> class encapsulates a TRP-formatted address
 * used in some TNEF message attributes.
 *
 * @author Amichai Rothman
 * @since 2003-04-25
 */
public class TRPAddress extends Address {

    /**
     * TRP Address ID constant.
     */
    public static final int
        trpidNull                   = 0x0000,
        trpidUnresolved             = 0x0001,
        trpidResolvedNSID           = 0x0002,
        trpidResolvedAddress        = 0x0003,
        trpidOneOff                 = 0x0004,
        trpidGroupNSID              = 0x0005,
        trpidOffline                = 0x0006,
        trpidIgnore                 = 0x0007,
        trpidClassEntry             = 0x0008,
        trpidResolvedGroupAddress   = 0x0009;

    int ID;

    /**
     * Constructs a TRPAddress using the given TNEF stream.
     *
     * @param data the TNEF stream to parse address from
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public TRPAddress(RawInputStream data) throws IOException {
        // get TRP structure values
        int trpid = data.readU16(); // from trpid* constants
        int cbgrtrp = data.readU16(); // 2 * sizeof(TRP) + display length (with padding) + address length
        int cch = data.readU16(); // display name length (with padding)
        int cbRgb = data.readU16(); // address length

        // get the actual address values
        this.ID = trpid;
        this.displayName = data.readString(cch);
        this.address = data.readString(cbRgb);
        int separator = address.indexOf(':');
        this.type = address.substring(0, separator);
        this.address = address.substring(separator + 1);
    }

    /**
     * Constructs a TRPAddress containing the specified values.
     *
     * @param ID the ID of the TRPAddress (from trpid* constants)
     * @param displayName the display name associated with address
     * @param type the address type
     * @param address the address
     */
    public TRPAddress(int ID, String displayName, String type, String address) {
        this.ID = ID;
        this.displayName = displayName;
        this.type = type;
        this.address = address;
    }

    /**
     * Gets the TRPAddress ID.
     *
     * @return the TRPAddress ID
     */
    public int getID() {
        return this.ID;
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        return new StringBuffer()
            .append(TNEFUtils.getConstName(this.getClass(), "trpid", getID()))
            .append(": ").append(super.toString()).toString();
    }

}
