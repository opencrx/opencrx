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
 * The <code>RendData</code> class encapsulates a TNEF attachment's
 * rendering data.
 *
 * @author Amichai Rothman
 * @since 2003-04-25
 */
public class RendData {

    /**
     * RendData type constant.
     */
    public static final int
        MAC_BINARY = 0x00000001,
        atypNull    = 0,
        atypFile    = 1,
        atypOle     = 2,
        atypPicture = 3,
        atypMax     = 4;

    int     type;
    long    position;
    int     width;
    int     height;
    long    flags;

    /**
     * Constructs a RendData using the given TNEF stream.
     *
     * @param data the TNEF stream to parse RendData from
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    public RendData(RawInputStream data) throws IOException {
        this.type = data.readU16();
        this.position = (int)data.readU32();
        this.width = data.readU16();
        this.height = data.readU16();
        this.flags = data.readU32();
    }

    /**
     * Constructs a RendData containing the specified values.
     *
     * @param type the type of rendering data (from atyp* constants)
     * @param position the position to render data at
     * @param width the width of object to be rendered
     * @param height the height of object to be rendered
     * @param flags the flags describing the rendering data
     */
    public RendData(int type, long position, int width, int height, long flags) {
        this.type = type;
        this.position = position;
        this.width = width;
        this.height = height;
        this.flags = flags;
    }

    /**
     * Gets the RendData type.
     *
     * @return the RendData type
     */
    public int getType() {
        return this.type;
    }

    /**
     * Gets the RendData position.
     *
     * @return the RendData position
     */
    public long getPosition() {
        return this.position;
    }

    /**
     * Gets the RendData width.
     *
     * @return the RendData width
     */
    public int getWidth() {
        return this.width;
    }

    /**
     * Gets the RendData height.
     *
     * @return the RendData height
     */
    public int getHeight() {
        return this.height;
    }

    /**
     * Gets the RendData flags.
     *
     * @return the RendData flags
     */
    public long getFlags() {
        return this.flags;
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append("RendData:")
            .append(" getType=").append(getType())
            .append(" getPosition=").append(getPosition())
            .append(" getWidth=").append(getWidth())
            .append(" getHeight=").append(getHeight())
            .append(" getFlags=").append(getFlags());
        return s.toString();
    }

}
