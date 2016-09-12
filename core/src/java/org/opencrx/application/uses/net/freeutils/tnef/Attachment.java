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

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * The <code>Attachment</code> class encapsulates a TNEF message attachment.
 *
 * @author Amichai Rothman
 * @since 2003-04-25
 */
public class Attachment {

    List attributes;
    String filename;
    RawInputStream rawData;
    MAPIProps MAPIProps;
    Message nestedMessage;

    /**
     * Constructs an empty Attachment.
     */
    public Attachment() {
        this.attributes = new ArrayList();
    }

    /**
     * Gets the Attachment attributes.
     *
     * @return the Attachment attributes
     */
    public List getAttributes() {
        return this.attributes;
    }

    /**
     * Sets the Attachment attributes.
     *
     * @param attributes the Attachment attributes
     */
    public void setAttributes(List attributes) {
        this.attributes = attributes;
    }

    /**
     * Gets a specific Attachment attribute.
     *
     * @param ID the requested attribute ID
     * @return the requested Attachment attribute, or null if no such
     *         attribute exists
     */
    public Attr getAttribute(int ID) {
        return Attr.findAttr(this.attributes, ID);
    }

    /**
     * Gets the Attachment filename.
     *
     * @return the Attachment filename, or null if none exists
     */
    public String getFilename() {
        if (filename == null) { // don't override previously set filename
            try {
                // long filename
                filename = (String)MAPIProps.getPropValue(MAPIProp.PR_ATTACH_LONG_FILENAME);
                // or short filename
                if (filename == null)
                    filename = (String)MAPIProps.getPropValue(MAPIProp.PR_ATTACH_FILENAME);
                // or transport filename
                if (filename == null) {
                    Attr attr = getAttribute(Attr.attAttachTransportFilename);
                    if (attr != null)
                        filename = (String)attr.getValue();
                }
                // or title (filename)
                if (filename == null) {
                    Attr attr = getAttribute(Attr.attAttachTitle);
                    if (attr != null)
                        filename = (String)attr.getValue();
                }
            } catch (IOException ioe) {
                filename = null;
            }
        }
        return filename;
    }

    /**
     * Sets the Attachment filename.
     *
     * @param filename the Attachment filename
     */
    public void setFilename(String filename) {
        this.filename = filename;
    }

    /**
     * Adds an Attachment attribute.
     *
     * @param attr the Attachment attribute to add
     */
    public void addAttribute(Attr attr) {
        this.attributes.add(attr);
    }

    /**
     * Gets the Attachment raw data.
     *
     * @return the Attachment raw data
     */
    public RawInputStream getRawData() {
        return this.rawData;
    }

    /**
     * Sets the Attachment raw data.
     *
     * @param rawData the Attachment raw data
     */
    public void setRawData(RawInputStream rawData) {
        this.rawData = rawData;
    }

    /**
     * Gets the Attachment nested message.
     *
     * @return the Attachment nested message
     */
    public Message getNestedMessage() {
        return this.nestedMessage;
    }

    /**
     * Sets the Attachment nested message.
     *
     * @param nestedMessage the Attachment nested message
     */
    public void setNestedMessage(Message nestedMessage) {
        this.nestedMessage = nestedMessage;
    }

    /**
     * Gets the Attachment MAPI properties.
     *
     * @return the Attachment MAPI properties
     */
    public MAPIProps getMAPIProps() {
        return this.MAPIProps;
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append("Attachment:");
        for (int i = 0; i < attributes.size(); i++)
            s.append("\n  ").append(attributes.get(i));
        if (getRawData() != null)
            s.append("\n  data=").append(getRawData());
        if (getMAPIProps() != null) {
            MAPIProp[] props = getMAPIProps().getProps();
            s.append("\n  MAPIProps=");
            for (int i = 0; i < props.length; i++)
                s.append("\n    ").append(props[i]);
        }
        if (getNestedMessage() != null)
            s.append("\n  Nested Message:").append(getNestedMessage());
        return s.toString();
    }

    /**
     * Sets this Attachment's MAPI properties. Special properties will be
     * translated into the Attachment's fields.
     *
     * @param MAPIProps a collection of properties to set
     * @throws IOException if an I/O error occurs
     */
    public void setMAPIProps(MAPIProps MAPIProps) throws IOException {
        this.MAPIProps = MAPIProps;
        // pick out interesting attributes that should go in Attachment
        if (MAPIProps != null) {
            // attachment data object
            MAPIProp prop = MAPIProps.getProp(MAPIProp.PR_ATTACH_DATA_OBJ);
            if (prop != null && prop.getLength() > 0) {
                MAPIValue value = prop.getValues()[0];
                if (value != null) {
                    RawInputStream data = value.getRawData();
                    if (prop.getType() == MAPIProp.PT_OBJECT) // not PT_BINARY
                        data.readBytes(16); // remove GUID
                    this.setRawData(data);
                    Object o = value.getValue();
                    if (o instanceof TNEFInputStream) {
                        this.setNestedMessage(new Message((TNEFInputStream)o));
                    }
                }
            }
        }
    }

    /**
     * Writes the content of this attachment to a file.
     *
     * @param filename the fully qualified filename to which the attachment
     *        content should be written
     * @throws IOException if an I/O error occurs
     */
    public void writeTo(String filename) throws IOException {
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(filename);
            writeTo(fos);
        } finally {
            if (fos != null)
                fos.close();
        }
    }

    /**
     * Writes the content of this attachment to a stream.
     *
     * @param out the OutputStream to which the attachment
     *        content should be written
     * @throws IOException if an I/O error occurs
     */
    public void writeTo(OutputStream out) throws IOException {
        if (this.rawData == null)
            return;
        RawInputStream in = new RawInputStream(this.rawData);
        try {
            byte[] buf = new byte[4096];
            int count;
            while ((count = in.read(buf)) != -1)
                out.write(buf, 0, count);
        } finally {
            in.close();
        }
    }

}
