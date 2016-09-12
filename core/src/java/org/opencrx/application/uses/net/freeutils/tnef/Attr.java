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
import java.util.Calendar;
import java.util.List;
import java.util.TimeZone;

/**
 * The <code>Attr</code> class encapsulates a TNEF attribute. A TNEF stream
 * consists of a flat list of attributes, which belong either to the message
 * level or attachment level. An attAttachRenddata attribute marks the
 * beginning of an attachment, and all subsequent attributes (until the
 * next attachment begins) belong to it.
 *
 * Different attributes have a different meaning for the underlying raw data,
 * thus the getValue() method returns different types of objects. The object
 * returned is determined by the attribute type, except for several special cases.
 *
 * @author Amichai Rothman
 * @since 2003-04-25
 */
public class Attr {

    /**
     * Attribute type constant.
     */
    public static final int
        atpTriples      = 0x0000,
        atpString       = 0x0001,
        atpText         = 0x0002,
        atpDate         = 0x0003,
        atpShort        = 0x0004,
        atpLong         = 0x0005,
        atpByte         = 0x0006,
        atpWord         = 0x0007,
        atpDword        = 0x0008,
        atpMax          = 0x0009;

    /**
     * Attribute level constant.
     */
    public static final byte
        LVL_MESSAGE     = 0x01,
        LVL_ATTACHMENT  = 0x02;

    /**
     * Attribute ID constant.
     */
    public static final int
        attNull                     = /*ATT( 0,*/             0x0000/*)*/,
        attFrom                     = /*ATT( atpTriples,*/    0x8000/*)*/, /* PR_ORIGINATOR_RETURN_ADDRESS */
        attSubject                  = /*ATT( atpString,*/     0x8004/*)*/, /* PR_SUBJECT */
        attDateSent                 = /*ATT( atpDate,*/       0x8005/*)*/, /* PR_CLIENT_SUBMIT_TIME */
        attDateRecd                 = /*ATT( atpDate,*/       0x8006/*)*/, /* PR_MESSAGE_DELIVERY_TIME */
        attMessageStatus            = /*ATT( atpByte,*/       0x8007/*)*/, /* PR_MESSAGE_FLAGS */
        attMessageClass             = /*ATT( atpWord,*/       0x8008/*)*/, /* PR_MESSAGE_CLASS */
        attMessageID                = /*ATT( atpString,*/     0x8009/*)*/, /* PR_MESSAGE_ID */
        attParentID                 = /*ATT( atpString,*/     0x800A/*)*/, /* PR_PARENT_ID */
        attConversationID           = /*ATT( atpString,*/     0x800B/*)*/, /* PR_CONVERSATION_ID */
        attBody                     = /*ATT( atpText,*/       0x800C/*)*/, /* PR_BODY */
        attPriority                 = /*ATT( atpShort,*/      0x800D/*)*/, /* PR_IMPORTANCE */
        attAttachData               = /*ATT( atpByte,*/       0x800F/*)*/, /* PR_ATTACH_DATA_xxx */
        attAttachTitle              = /*ATT( atpString,*/     0x8010/*)*/, /* PR_ATTACH_FILENAME */
        attAttachMetaFile           = /*ATT( atpByte,*/       0x8011/*)*/, /* PR_ATTACH_RENDERING */
        attAttachCreateDate         = /*ATT( atpDate,*/       0x8012/*)*/, /* PR_CREATION_TIME */
        attAttachModifyDate         = /*ATT( atpDate,*/       0x8013/*)*/, /* PR_LAST_MODIFICATION_TIME */
        attDateModified             = /*ATT( atpDate,*/       0x8020/*)*/, /* PR_LAST_MODIFICATION_TIME */
        attAttachTransportFilename  = /*ATT( atpByte,*/       0x9001/*)*/, /* PR_ATTACH_TRANSPORT_NAME */
        attAttachRenddata           = /*ATT( atpByte,*/       0x9002/*)*/,
        attMAPIProps                = /*ATT( atpByte,*/       0x9003/*)*/,
        attRecipTable               = /*ATT( atpByte,*/       0x9004/*)*/, /* PR_MESSAGE_RECIPIENTS */
        attAttachment               = /*ATT( atpByte,*/       0x9005/*)*/,
        attTnefVersion              = /*ATT( atpDword,*/      0x9006/*)*/,
        attOemCodepage              = /*ATT( atpByte,*/       0x9007/*)*/,
        attOriginalMessageClass     = /*ATT( atpWord,*/       0x0006/*)*/, /* PR_ORIG_MESSAGE_CLASS */
        attOwner                    = /*ATT( atpByte,*/       0x0000/*)*/, /* PR_RCVD_REPRESENTING_xxx  or
                                                                              PR_SENT_REPRESENTING_xxx */
        attSentFor                  = /*ATT( atpByte,*/       0x0001/*)*/, /* PR_SENT_REPRESENTING_xxx */
        attDelegate                 = /*ATT( atpByte,*/       0x0002/*)*/, /* PR_RCVD_REPRESENTING_xxx */
        attDateStart                = /*ATT( atpDate,*/       0x0006/*)*/, /* PR_DATE_START */
        attDateEnd                  = /*ATT( atpDate,*/       0x0007/*)*/, /* PR_DATE_END */
        attAidOwner                 = /*ATT( atpLong,*/       0x0008/*)*/, /* PR_OWNER_APPT_ID */
        attRequestRes               = /*ATT( atpShort,*/      0x0009/*)*/; /* PR_RESPONSE_REQUESTED */

    byte level;
    int type;
    int ID;
    int length;
    RawInputStream rawData;
    Object data;

    /**
     * Constructs an Attr containing the specified values.
     *
     * @param level the attribute level (from LVL_* constants)
     * @param type the attribute type (from atp* constants)
     * @param ID the attribute ID (from att* constants)
     * @param data the attribute data
     */
    public Attr(byte level, int type, int ID, Object data) {
        this.level = level;
        this.type = type;
        this.ID = ID;
        this.data = data;
        this.length = -1;
    }

    /**
     * Constructs an Attr containing the specified values.
     *
     * @param level the attribute level (from LVL_* constants)
     * @param type the attribute type (from atp* constants)
     * @param ID the attribute ID (from att* constants)
     * @param rawData the attribute's raw data
     */
    public Attr(byte level, int type, int ID, RawInputStream rawData) {
        this.level = level;
        this.type = type;
        this.ID = ID;
        this.rawData = rawData;
        this.length = rawData != null ? (int)rawData.getLength() : 0;
    }

    /**
     * Gets the Attr level.
     *
     * @return the Attr level
     */
    public byte getLevel() {
        return this.level;
    }

    /**
     * Gets the Attr type.
     *
     * @return the Attr type
     */
    public int getType() {
        return this.type;
    }

    /**
     * Gets the Attr ID.
     *
     * @return the Attr ID
     */
    public int getID() {
        return this.ID;
    }

    /**
     * Gets the Attr data length (in bytes).
     *
     * @return the Attr data length (in bytes), or -1 if it is unknown
     */
    public int getLength() {
        return this.length;
    }

    /**
     * Gets the Attr raw data.
     *
     * @return the Attr raw data
     */
    public RawInputStream getRawData() {
        return this.rawData;
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append("Attr:")
            .append(" level=").append(getLevel())
            .append(" type=").append(TNEFUtils.getConstName(this.getClass(), "atp", getType()))
            .append(" ID=").append(TNEFUtils.getConstName(this.getClass(), "att", getID()))
            .append(" length=").append(getLength());
            s.append(" value=");
        try {
            Object o = getValue();

            if (o instanceof MAPIProps[]) {
                MAPIProps[] props = (MAPIProps[])o;
                for (int i = 0; i < props.length; i++)
                    for (int j = 0; j < props[i].getProps().length; j++)
                        s.append("\n  #").append(i).append(": ").append(props[i].getProps()[j]);
            } else if (o instanceof MAPIProps) {
                MAPIProp[] props = ((MAPIProps)o).getProps();
                for (int i = 0; i < props.length; i++)
                    s.append("\n  ").append(props[i]);
            } else {
                s.append(o);
            }
        } catch (IOException ioe) {
            s.append("INVALID VALUE: ").append(ioe);
        }
        return s.toString();
    }

    /**
     * Returns the value of the Attr's data.
     * Different attributes have a different meaning for the underlying raw data,
     * thus the getValue() method returns different types of objects. The object
     * returned is determined by the attribute type, except for several special cases.
     * The returned Object should be cast into the appropriate class.
     *
     * @return the value of the Attr's data
     * @throws IOException if an I/O error occurs
     */
    public Object getValue() throws IOException {
        if (rawData != null)
            read(rawData);
        return data;
    }

    /**
     * Reads this attribute's data from the given RawInputStream.
     *
     * @param in the RawInputStream containing attribute data
     * @throws IOException if an I/O error occurs
     */
    protected void read(RawInputStream in) throws IOException {
        RawInputStream ris = new RawInputStream(in); // a new copy each time!
        Object o = null;

        try {
            // first handle special attributes
            switch (this.ID) {
                case attAttachRenddata:
                    o = new RendData(ris);
                    break;
                case attFrom:
                    o = new TRPAddress(ris);
                    break;
                case attOwner:
                case attSentFor:
                    o = new Address(ris);
                    break;
                case attRecipTable:
                    // recipients are represented by a counted list of
                    // MAPI property rows.
                    int count = (int)ris.readU32();
                    MAPIProps[] recipients = new MAPIProps[count];
                    for (int i = 0; i < count; i++) {
                        recipients[i] = new MAPIProps(ris);
                    }
                    o = recipients;
                    break;
                case attMAPIProps:
                    o = new MAPIProps(ris);
                    break;
            }

            // handle generic types
            if (o == null) switch (type) {
                case atpByte:
                    o = new RawInputStream(ris);
                    break;
                case atpShort:
                    o = new Short((short)ris.readU16());
                    break;
                case atpDword:
                    o = new Long(ris.readU32());
                    break;
                case atpLong:
                    o = new Long(ris.readU32());
                    break;
                case atpString:
                case atpText:
                case atpWord: // apparently used only in attMessageClass, as a string and not a word
                    o = ris.readString(length);
                    break;
                case atpDate:
                    if (ris.getLength() >= 14) {
                        // TNEF time is 7 16-bit fields
                        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                        cal.set(ris.readU16(),     // Calendar.YEAR
                                ris.readU16() - 1, // Calendar.MONTH
                                ris.readU16(),     // Calendar.DATE
                                ris.readU16(),     // Calendar.HOUR
                                ris.readU16(),     // Calendar.MINUTE
                                ris.readU16());    // Calendar.SECOND
                                                   // Calendar.DAY_OF_WEEK in bytes 12-13 is not needed.
                        cal.set(Calendar.MILLISECOND, 0);
                        o = cal.getTime();
                    }
                    break;
                case atpTriples:
                    o = new TRPAddress(ris);
                    break;
            }
        } finally {
            ris.close();
        }
        this.data = o;
    }

    /**
     * Finds an attribute with the specified ID within given attribute list.
     *
     * @param attributes the attribute list to search
     * @param ID the ID of the attribute to search for
     * @return an attribute with given ID found in the attribute list,
     *         or null if no such attribute exists
     */
    public static Attr findAttr(List attributes, int ID) {
        for (int i = 0; attributes != null && i < attributes.size(); i++) {
            Attr attr = (Attr)attributes.get(i);
            if (attr.getID() == ID)
                return attr;
        }
        return null;
    }

}
