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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

/**
 * The <code>TNEFInputStream</code> class provides low-level access to
 * a TNEF stream.
 *
 * @author Amichai Rothman
 * @since 2003-07-25
 */
public class TNEFInputStream {

    static boolean ignoreChecksum = "true".equalsIgnoreCase(System.getProperty("jtnef.checksum.ignore"));

    InputStream in;
    int key;

    /**
     * Specifies whether TNEF attribute checksums are ignored
     * (in all TNEFInputStream instances).
     *
     * This method overrides the "jtnef.checksum.ignore" system property setting.
     *
     * @param ignore if true, checksums are ignored;
     *        if false, an exception is thrown when a checksum is invalid
     */
    static void setChecksumIgnore(boolean ignore) {
        ignoreChecksum = ignore;
    }

    /**
     * Constructs a TNEFInputStream whose content is retrieved from the
     * given InputStream.
     *
     * @param in an InputStream supplying a TNEF data stream
     * @throws IOException if the input does not start with a valid TNEF
     *         signature, or if an I/O error occurs
     */
    public TNEFInputStream(InputStream in) throws IOException {
        this.in = in;
        // read and validate TNEF stream signature
        long signature = readU32();
        if (signature != TNEFConstants.TNEF_SIGNATURE)
            throw new IOException("Invalid TNEF signature 0x"
                + Long.toHexString(signature).toUpperCase() + " (not a valid TNEF stream)");
        // read TNEF stream key
        key = readU16();
    }

    /**
     * Constructs a TNEFInputStream whose content is retrieved from the
     * given File.
     *
     * @param file a file containing a TNEF data stream
     * @throws IOException if the input does not start with a valid TNEF
     *         signature, or if an I/O error occurs
     */
    public TNEFInputStream(File file) throws IOException {
        this(new RawInputStream(file));
    }

    /**
     * Constructs a TNEFInputStream whose content is retrieved from the
     * given file.
     *
     * @param filename the fully qualified filename of a file containing
     *        a TNEF data stream
     * @throws IOException if the input does not start with a valid TNEF
     *         signature, or if an I/O error occurs
     */
    public TNEFInputStream(String filename) throws IOException {
        this(new File(filename));
    }

    /**
     * Closes the TNEFInputStream and underlying InputStream.
     *
     * @throws IOException if an I/O error occurs
     */
    public void close() throws IOException {
        if (this.in != null)
            in.close();
    }

    /**
     * Reads an 8-bit unsigned value from the stream.
     *
     * @return an 8-bit unsigned value read from the stream
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    protected int readU8() throws IOException {
        int b = in.read();
        if (b == -1)
            throw new IOException("Unexpected end of stream");
        return b;
    }

    /**
     * Reads a 16-bit unsigned value from the stream.
     *
     * @return a 16-bit unsigned value read from the stream
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    protected int readU16() throws IOException {
        return TNEFUtils.getU16(readU8(), readU8());
    }

    /**
     * Reads a 32-bit unsigned value from the stream.
     *
     * @return a 32-bit unsigned value read from the stream
     * @throws IOException if the stream end is reached,
     *         or if an I/O error occurs
     */
    protected long readU32() throws IOException {
        return TNEFUtils.getU32(readU8(), readU8(), readU8(), readU8());
    }

    /**
     * Reads a TNEF attribute from the stream.
     *
     * @return a TNEF attribute read from the stream,
     *         or null if the stream end is reached
     * @throws IOException if the stream does not contain a valid TNEF attribute,
     *         or if an I/O error occurs
     */
    public Attr readAttr() throws IOException {
        int level = in.read();
        if (level == -1)
            return null; // reached end of stream
        if (level != Attr.LVL_MESSAGE && level != Attr.LVL_ATTACHMENT)
            throw new IOException("Invalid TNEF level type: " + level);
        int typeAndName = (int)readU32();
        int length = (int)readU32();

        // read the raw attribute data
        RawInputStream rawData;
        if (in instanceof RawInputStream) { // reduce memory footprint
            RawInputStream ris = (RawInputStream)in;
            rawData = new RawInputStream(ris, 0, length);
            if (ris.skip(length) != length)
                throw new IOException("Unexpected end of stream");
        } else {
            byte[] buf = new byte[length];
            int total = 0;
            while (total < length) {
                int read = in.read(buf, total, length - total);
                if (read < 0)
                    throw new IOException("Unexpected end of stream");
                total += read;
            }
            rawData = new RawInputStream(buf, 0, length);
        }

        int checksum = readU16();
        if (!ignoreChecksum && checksum != TNEFUtils.calculateChecksum(rawData))
            throw new IOException("Invalid checksum on attribute");

        return new Attr((byte)level, TNEFUtils.attType(typeAndName),
            TNEFUtils.attID(typeAndName), rawData);
    }

    /**
     * Returns the TNEF stream key.
     *
     * @return the TNEF stream key
     */
    public int getKey() {
        return key;
    }

}
