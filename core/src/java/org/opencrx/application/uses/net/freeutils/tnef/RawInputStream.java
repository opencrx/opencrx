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

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * The <code>RawInputStream</code> class is an InputStream which can provide
 * raw byte data from several underlying sources, including a byte array,
 * a partial section of a byte array, a file, a partial section of a file,
 * another RawInputStream, or a partial section of another RawInputStream.
 *
 * @author Amichai Rothman
 * @since 2003-04-29
 */
public class RawInputStream extends InputStream {

    InputStream in;
    byte[] buf;
    File file;
    long offset; // start of stream
    long length;
    long position; // current position
    long mark; // saved position for mark/reset

    /**
     * Initializes the RawInputStream using a new InputStream.
     *
     * @param in the InputStream to retrieve data from
     * @param offset the offset within the stream to start getting data at
     * @param length the length of the partial stream to create (in bytes)
     * @throws IOException if an I/O error occurs
     */
    void init(InputStream in, long offset, long length) throws IOException {
        if (in instanceof FileInputStream && length > 16)
            in = new BufferedInputStream(in, length < 4096 ? (int)length : 4096);
        this.in = in;
        // use our skip() instead of in.skip() since it makes a better
        // attempt at doing a full skip (see InputStream.skip() contract)
        this.length = offset; // set it up for one-time internal use
        if (skip(offset) != offset)
            throw new IOException("can't skip to offset " + offset + " in stream");
        // init fields
        this.length = length;
        this.offset = offset;
        this.position = offset;
        this.mark = offset;
    }

    /**
     * Constructs a RawInputStream which provides data from a byte array.
     *
     * @param buf the byte array to retrieve data from
     * @param offset the offset within the array to start getting data at
     * @param length the length of the partial stream to create (in bytes)
     * @throws IOException if an I/O error occurs
     */
    public RawInputStream(byte[] buf, long offset, long length) throws IOException {
        init(new ByteArrayInputStream(this.buf = buf), offset, length);
    }

    /**
     * Constructs a RawInputStream which provides data from a byte array.
     *
     * @param buf the byte array to retrieve data from
     * @throws IOException if an I/O error occurs
     */
    public RawInputStream(byte[] buf) throws IOException {
        this(buf, 0, buf.length);
    }

    /**
     * Constructs a RawInputStream which provides data from a file.
     *
     * @param filename the fully qualified path of the file to retrieve data from
     * @param offset the offset within the file to start getting data at
     * @param length the length of the partial stream to create (in bytes)
     * @throws IOException if an I/O error occurs
     */
    public RawInputStream(String filename, long offset, long length) throws IOException {
        this(new File(filename), offset, length);
    }

    /**
     * Constructs a RawInputStream which provides data from a file.
     *
     * @param filename the fully qualified path of the file to retrieve data from
     * @throws IOException if an I/O error occurs
     */
    public RawInputStream(String filename) throws IOException {
        this(new File(filename));
    }

    /**
     * Constructs a RawInputStream which provides data from a file.
     *
     *  @param file the file to retrieve data from
     *  @throws IOException if an I/O error occurs
     */
    public RawInputStream(File file) throws IOException {
        this(file, 0, file.length());
    }

    /**
     * Constructs a RawInputStream which provides data from a file.
     *
     * @param file the file to retrieve data from
     * @param offset the offset within the file to start getting data at
     * @param length the length of the partial stream to create (in bytes)
     * @throws IOException if an I/O error occurs
     */
    public RawInputStream(File file, long offset, long length) throws IOException {
        init(new FileInputStream(this.file = file), offset, length);
    }

    /**
     * Constructs a RawInputStream which provides data from another RawInputStream.
     * The new stream begins at the current position of the original stream,
     * and contains all of the remaining bytes.
     *
     * @param ris the RawInputStream to retrieve data from
     * @throws IOException if an I/O error occurs
     */
    public RawInputStream(RawInputStream ris) throws IOException {
        this(ris, 0, ris.available());
    }

    /**
     * Constructs a RawInputStream which provides data from another RawInputStream.
     * The new stream begins at the current position of the original stream plus
     * the given offset, and its length is set to given length.
     *
     * @param ris the RawInputStream to retrieve data from
     * @param offset the offset from the current position at which the
     *        new stream will begin
     * @param length the length of the partial stream to create (in bytes)
     * @throws IOException if an I/O error occurs
     */
    public RawInputStream(RawInputStream ris, long offset, long length) throws IOException {
        this.file = ris.file;
        this.buf = ris.buf;
        InputStream in = this.file != null
                ? new FileInputStream(this.file)
                : (InputStream)new ByteArrayInputStream(this.buf);
        init(in, ris.position + offset, length);
    }

    /**
     * Gets the length of the raw data in bytes.
     *
     * Unlike the available() method, this method is not
     * affected by reading or skipping, and always returns
     * the total number of bytes that this stream contains.
     *
     * @return the length of the raw data in bytes
     */
    public long getLength() {
        return this.length;
    }

    /**
     * Gets the raw data as a byte array, starting at the current position
     * and ending either at the end of the stream, or after the given
     * maximum number of bytes, whichever comes first.
     *
     * @param max the maximum number of returned bytes;
     *         if negative, there is no limit
     * @return the raw data as a byte array
     * @throws IOException if an I/O error occurs
     */
    public byte[] toByteArray(int max) throws IOException {
        int len = available();
        if (max > -1 && max < len)
            len = max;
        byte[] buf = new byte[len];
        if (len > 0) {
            RawInputStream ris = new RawInputStream(this);
            try {
                ris.readFully(buf);
            } finally {
                ris.close();
            }
        }
        return buf;
    }

    /**
     * Gets the raw data as a byte array, starting at the current position
     * and ending at the end of the stream.
     *
     * @return the raw data as a byte array
     * @throws IOException if an I/O error occurs
     */
    public byte[] toByteArray() throws IOException {
        return toByteArray(-1);
    }

    /**
     * Reads the next byte of data from the input stream. The value byte is
     * returned as an <code>int</code> in the range <code>0</code> to
     * <code>255</code>. If no byte is available because the end of the stream
     * has been reached, the value <code>-1</code> is returned. This method
     * blocks until input data is available, the end of the stream is detected,
     * or an exception is thrown.
     *
     * @return the next byte of data, or <code>-1</code> if the end of the
     *         stream is reached
     * @throws IOException if an I/O error occurs
     */
    public int read() throws IOException {
        if (length - (position - offset) > 0) { // available() > 0
            position++;
            return in.read();
        }
        return -1;
    }

    /**
     * Skips over and discards <code>n</code> bytes of data from this input
     * stream. The <code>skip</code> method may, for a variety of reasons, end
     * up skipping over some smaller number of bytes, possibly <code>0</code>.
     * This may result from any of a number of conditions; reaching end of file
     * before <code>n</code> bytes have been skipped is only one possibility.
     * The actual number of bytes skipped is returned.  If <code>n</code> is
     * negative, no bytes are skipped.
     *
     * @param n the number of bytes to be skipped
     * @return the actual number of bytes skipped
     * @throws IOException if an I/O error occurs
     */
    public long skip(long n) throws IOException {
        if (n <= 0)
            return 0;
        if (n > available())
            n = available();
        long total = 0;
        while (total < n) {
            long skipped = in.skip(n - total);
            if (skipped == 0)
                return 0;
            position += skipped;
            total += skipped;
        }
        return total;
    }

    /**
     * Returns the number of bytes that can be read (or skipped over) from
     * this input stream without blocking by the next caller of a method for
     * this input stream.  The next caller might be the same thread or
     * another thread.
     *
     * @return the number of bytes that can be read from this input stream
     *         without blocking
     * @throws IOException  if an I/O error occurs
     */
    public int available() throws IOException {
        return (int)(length - (position - offset));
    }

    /**
     * Closes this input stream and releases any system resources associated
     * with the stream.
     *
     * @throws IOException if an I/O error occurs
     */
    public void close() throws IOException {
        in.close();
    }

    /**
     * Marks the current position in this input stream. A subsequent call to
     * the <code>reset</code> method repositions this stream at the last marked
     * position so that subsequent reads re-read the same bytes.
     *
     * <p> The <code>readlimit</code> arguments tells this input stream to
     * allow that many bytes to be read before the mark position gets
     * invalidated.
     *
     * <p> The general contract of <code>mark</code> is that, if the method
     * <code>markSupported</code> returns <code>true</code>, the stream somehow
     * remembers all the bytes read after the call to <code>mark</code> and
     * stands ready to supply those same bytes again if and whenever the method
     * <code>reset</code> is called.  However, the stream is not required to
     * remember any data at all if more than <code>readlimit</code> bytes are
     * read from the stream before <code>reset</code> is called.
     *
     * @param readlimit the maximum limit of bytes that can be read before
     *        the mark position becomes invalid
     * @see java.io.InputStream#reset()
     */
    public synchronized void mark(int readlimit) {
        in.mark(readlimit);
        mark = position;
    }

    /**
     * Repositions this stream to the position at the time the
     * <code>mark</code> method was last called on this input stream.
     *
     * <p> The general contract of <code>reset</code> is:
     *
     * <p><ul>
     *
     * <li> If the method <code>markSupported</code> returns
     * <code>true</code>, then:
     *
     *     <ul><li> If the method <code>mark</code> has not been called since
     *     the stream was created, or the number of bytes read from the stream
     *     since <code>mark</code> was last called is larger than the argument
     *     to <code>mark</code> at that last call, then an
     *     <code>IOException</code> might be thrown.
     *
     *     <li> If such an <code>IOException</code> is not thrown, then the
     *     stream is reset to a state such that all the bytes read since the
     *     most recent call to <code>mark</code> (or since the start of the
     *     file, if <code>mark</code> has not been called) will be resupplied
     *     to subsequent callers of the <code>read</code> method, followed by
     *     any bytes that otherwise would have been the next input data as of
     *     the time of the call to <code>reset</code>. </ul>
     *
     * <li> If the method <code>markSupported</code> returns
     * <code>false</code>, then:
     *
     *     <ul><li> The call to <code>reset</code> may throw an
     *     <code>IOException</code>.
     *
     *     <li> If an <code>IOException</code> is not thrown, then the stream
     *     is reset to a fixed state that depends on the particular type of the
     *     input stream and how it was created. The bytes that will be supplied
     *     to subsequent callers of the <code>read</code> method depend on the
     *     particular type of the input stream. </ul></ul>
     *
     * @throws IOException if this stream has not been marked or if the
     *         mark has been invalidated
     * @see java.io.InputStream#mark(int)
     * @see java.io.IOException
     */
    public synchronized void reset() throws IOException {
        in.reset();
        position = mark;
    }

    /**
     * Tests if this input stream supports the <code>mark</code> and
     * <code>reset</code> methods.
     *
     * @return <code>true</code> if this true type supports the mark and reset
     *         method; <code>false</code> otherwise
     * @see java.io.InputStream#mark(int)
     * @see java.io.InputStream#reset()
     */
    public boolean markSupported() {
        return in.markSupported();
    }

    /**
     * Return the current position in the InputStream, as an
     * offset from the beginning of the underlying InputStream.
     *
     * @return the current position
     */
    public long getPosition() {
        return this.position;
    }

    /**
     * Return a new InputStream representing a subset of the data
     * from this InputStream, starting at <code>start</code> (inclusive)
     * up to <code>end</code> (exclusive). <code>start</code> must be
     * non-negative. If <code>end</code> is -1, the new stream ends
     * at the same place as this stream.
     *
     * @param start the starting position, relative to current position
     * @param end the ending position + 1
     * @return the new stream, or null if an error occurs
     * @throws IllegalArgumentException if start < 0
     */
    public InputStream newStream(long start, long end) {
        if (start < 0)
            throw new IllegalArgumentException("start < 0");
        if (end == -1)
            end = this.length;
        RawInputStream newStream;
        try {
            newStream = new RawInputStream(this, start, end - start);
        } catch (IOException ioe) {
            newStream = null;
        }
        return newStream;
    }

    /**
     * Reads the specified number of bytes into the given array, at the
     * given position.
     *
     * @param b the array to read bytes into
     * @param offset the starting offset within b for the read bytes
     * @param length the number of bytes to read
     * @throws IOException if the end of stream has been reached
     *         before the requested number of bytes have been read,
     *         or if an I/O error occurs
     */
    void readFully(byte[] b, int offset, int length) throws IOException {
        int total = 0;
        while (total < length) {
            int read = read(b, offset + total, length - total);
            if (read < 0)
                throw new IOException("Unexpected end of stream");
            total += read;
        }
    }

    /**
     * Reads bytes into the given array, filling it entirely.
     * This is equivalent to calling
     * <code>readFully(b, 0, b.length)</code>.
     *
     * @param b the array to read bytes into
     * @throws IOException if the end of stream has been reached
     *         before the entire array has been filled,
     *         or if an I/O error occurs
     */
    void readFully(byte[] b) throws IOException {
        readFully(b, 0, b.length);
    }

    /**
     * Reads an unsigned 8-bit value from the stream.
     *
     * @return an unsigned 8-bit value as an int
     * @throws IOException if the end of stream has been reached,
     *         or if an I/O error occurs
     */
    public int readU8() throws IOException {
        int b = read();
        if (b < 0)
            throw new IOException("Unexpected end of stream");
        return b;
    }

    /**
     * Reads an unsigned 16-bit value (little-endian ordered) from the stream.
     *
     * @return an unsigned 16-bit value as an int
     * @throws IOException if the end of stream has been reached,
     *         or if an I/O error occurs
     */
    public int readU16() throws IOException {
        return (readU8() | (readU8() << 8)) & 0xFFFF;
    }

    /**
     * Reads an unsigned 32-bit value (little-endian ordered) from the stream.
     *
     * @return an unsigned 32-bit value as a long
     * @throws IOException if the end of stream has been reached,
     *         or if an I/O error occurs
     */
    public long readU32() throws IOException {
        return (readU8() | (readU8() << 8) | (readU8() << 16) | (readU8() << 24)) & 0x00000000FFFFFFFFL;
    }

    /**
     * Reads a 64-bit value (little-endian ordered) from the stream.
     *
     * @return a 64-bit value as a long
     * @throws IOException if the end of stream has been reached,
     *         or if an I/O error occurs
     */
    public long readU64() throws IOException {
        return (readU32() & 0x00000000FFFFFFFFL) | ((readU32() & 0x00000000FFFFFFFFL) << 32);
    }

    /**
     * Reads a given number of bytes from the stream.
     *
     * @param length the number of bytes to read
     * @return the read bytes
     * @throws IOException if the end of stream has been reached,
     *         or if an I/O error occurs
     */
    public byte[] readBytes(int length) throws IOException {
        byte[] b = new byte[length];
        readFully(b, 0, length);
        return b;
    }

    /**
     * Reads a C-style null terminated byte sequence from the stream,
     * as a standard String.
     *
     * The null terminated byte sequence is interpreted as 8-bit ISO-8859-1
     * characters (a.k.a. ISO-Latin-1), which is a superset of US-ASCII.
     * This way we don't lose any 8-bit values and remain fully compatible:
     * If the source charset is unknown, a str.getBytes("ISO8859_1") will
     * reconstruct the exact original byte sequence which the application
     * can then process in any charset it sees fit.
     *
     * @param length the length of the C-style string in bytes, which may
     *        include any number of terminating null ('\0') characters
     * @return a String containing the C-style string's characters, interpreted
     *         as ISO-8859-1 characters
     * @throws IOException if the end of stream has been reached,
     *         or if an I/O error occurs
     */
    public String readString(int length) throws IOException {
        return TNEFUtils.createString(readBytes(length), 0, length);
    }

    /**
     * Reads a C-style null terminated Unicode byte sequence from the stream,
     * as a standard String.
     *
     * The null terminated byte sequence is interpreted as 16-bit Unicode
     * characters (UTF-16), stored in Little Endian order.
     *
     * @param length the length of the C-style string in bytes, which may
     *        include any number of terminating null ('\0') characters
     * @return a String containing the C-style string's characters, interpreted
     *         as Unicode (UTF-16 Little Endian) characters
     * @throws IOException if the end of stream has been reached,
     *         or if an I/O error occurs
     */
    public String readStringUnicode(int length) throws IOException {
        return TNEFUtils.createStringUnicode(readBytes(length), 0, length);
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        try {
            return TNEFUtils.toHexString(toByteArray(512), 0, available(), 512);
        } catch (IOException ioe) {
            return "RawInputStream can't get bytes: " + ioe;
        }
    }

    /**
     * Finalizes this object and frees its resources (called by the JVM
     * garbage collector when this object is discarded).
     *
     * @throws Throwable the Exception raised by this method
     */
    protected void finalize() throws Throwable {
        this.close();
        super.finalize();
    }

}
