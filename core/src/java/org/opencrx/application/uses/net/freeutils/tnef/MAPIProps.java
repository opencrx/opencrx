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
 * The <code>MAPIProps</code> class encapsulates a collection of MAPI properties.
 *
 * @author Amichai Rothman
 * @since 2003-08-15
 */
public class MAPIProps {

    MAPIProp[] props;
    int rawLength;

    /**
     * Creates MAPIProps using the given TNEF stream.
     *
     * @param data the TNEF stream containing property data
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public MAPIProps(RawInputStream data) throws IOException {
        long startOffest = data.getPosition();
        int count = (int)data.readU32();
        props = new MAPIProp[count];

        for (int i = 0; i < count; i++) {
            MAPIProp prop = props[i] = new MAPIProp();

            prop.type = data.readU16();
            boolean isMultiValue = (prop.type & MAPIProp.MV_FLAG) != 0;
            prop.type = prop.type & ~MAPIProp.MV_FLAG; // remove MV_FLAG
            switch (prop.type) { // all variable-length types are same as MV
                case MAPIProp.PT_STRING:
                case MAPIProp.PT_UNICODE_STRING:
                case MAPIProp.PT_OBJECT:
                case MAPIProp.PT_BINARY:
                    isMultiValue = true;
                    break;
            }

            prop.ID = data.readU16();

            // handle named properties, which include the name before the value(s)
            if (prop.ID >= 0x8000 && prop.ID <= 0xFFFE) {
                MAPIPropName name = new MAPIPropName(data);
                prop.setName(name);
            }

            // handle multivalue properties
            int valueCount = 1;
            if (isMultiValue) {
                valueCount = (int)data.readU32();
            }
            prop.values = new MAPIValue[valueCount];

            // get the value(s)
            for (int j = 0; j < prop.values.length; j++) {
                switch (prop.type) {
                    case MAPIProp.PT_NULL:
                        prop.values[j] = null;
                        break;

                    case MAPIProp.PT_INT:
                    case MAPIProp.PT_FLOAT:
                    case MAPIProp.PT_ERROR:
                    case MAPIProp.PT_BOOLEAN:   // 2 bytes + padding
                    case MAPIProp.PT_SHORT:     // 2 bytes + padding
                        // 4 bytes
                        prop.values[j] = new MAPIValue(prop.type, data, 4);
                        break;

                    case MAPIProp.PT_DOUBLE:
                    case MAPIProp.PT_APPTIME:
                    case MAPIProp.PT_CURRENCY:
                    case MAPIProp.PT_INT8BYTE:
                    case MAPIProp.PT_SYSTIME:
                        // 8 bytes
                        prop.values[j] = new MAPIValue(prop.type, data, 8);
                        break;

                    case MAPIProp.PT_CLSID:
                        // CLSID - 16 bytes
                        prop.values[j] = new MAPIValue(prop.type, data, 16);
                        break;

                    case MAPIProp.PT_STRING:
                    case MAPIProp.PT_UNICODE_STRING:
                    case MAPIProp.PT_OBJECT:
                    case MAPIProp.PT_BINARY:
                        // get value length
                        int vlen = (int)data.readU32();
                        // create value
                        prop.values[j] = new MAPIValue(prop.type, data, vlen);
                        // pad length count to 4 byte boundary
                        if (vlen % 4 != 0)
                            data.skip(4 - (vlen % 4));
                        break;
                    default:
                        throw new IOException("Unknown MAPI type: " + prop.type);
                } // switch
            } // for
        }
        this.rawLength = (int)(data.getPosition() - startOffest);
    }

    /**
     * Creates MAPIProps using the given properties.
     *
     * @param props an array of MAPI properties
     */
    public MAPIProps(MAPIProp[] props) {
        this.props = props;
    }

    /**
     * Gets all the properties.
     *
     * @return all the properties
     */
    public MAPIProp[] getProps() {
        return this.props;
    }

    /**
     * Gets a property with the given ID.
     *
     * @param ID the requested property ID
     * @return the requested property, or null if no such property exists
     */
    public MAPIProp getProp(int ID) {
        return MAPIProp.findProp(this.props, ID);
    }

    /**
     * Gets a property with the given name.
     *
     * @param name the requested property name
     * @return the requested property, or null if no such property exists
     */
    public MAPIProp getProp(MAPIPropName name) {
        return MAPIProp.findProp(this.props, name);
    }

    /**
     * Gets the first value of a specific MAPI property, if it exists.
     * This is a convenience method for single-value properties.
     *
     * @param ID the ID of the requested property
     * @return the value of the requested property, or null if it does not exist
     * @throws IOException if an I/O error occurs
     */
    public Object getPropValue(int ID) throws IOException {
        MAPIProp prop = getProp(ID);
        return prop != null ? prop.getValue() : null;
    }

    /**
     * Gets the first value of a specific MAPI property, if it exists.
     * This is a convenience method for single-value properties.
     *
     * @param name the name of the requested property
     * @return the value of the requested property, or null if it does not exist
     * @throws IOException if an I/O error occurs
     */
    public Object getPropValue(MAPIPropName name) throws IOException {
        MAPIProp prop = getProp(name);
        return prop != null ? prop.getValue() : null;
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
