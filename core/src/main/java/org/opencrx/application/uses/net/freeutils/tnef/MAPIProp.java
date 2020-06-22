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
import java.util.Arrays;

/**
 * The <code>MAPIProp</code> class encapsulates a MAPI property.
 *
 * @author Amichai Rothman
 * @since 2003-07-25
 */
public class MAPIProp {

    /**
     * MAPI property type constant.
     */
    public static final int
        MV_FLAG             = 0x1000, // OR with type means multiple values
        PT_UNSPECIFIED      = 0x0000, // Unspecified
        PT_NULL             = 0x0001, // null property (no value)
        PT_SHORT            = 0x0002, // short (signed 16 bits)
        PT_INT              = 0x0003, // integer (signed 32 bits)
        PT_FLOAT            = 0x0004, // float (4 bytes)
        PT_DOUBLE           = 0x0005, // double
        PT_CURRENCY         = 0x0006, // currency (64 bits)
        PT_APPTIME          = 0x0007, // application time
        PT_ERROR            = 0x000a, // error (32 bits)
        PT_BOOLEAN          = 0x000b, // boolean (16 bits, non-zero true)
        PT_OBJECT           = 0x000d, // embedded object
        PT_INT8BYTE         = 0x0014, // 8 byte signed int
        PT_STRING           = 0x001e, // string
        PT_UNICODE_STRING   = 0x001f, // unicode-string (null terminated)
        PT_SYSTIME          = 0x0040, // time (64 bits)
        PT_CLSID            = 0x0048, // OLE GUID
        PT_BINARY           = 0x0102; // binary

    /**
     * MAPI recipient type constant.
     */
    public static final int
        MAPI_ORIG       = 0,            // Recipient is message originator
        MAPI_TO         = 1,            // Recipient is a primary recipient
        MAPI_CC         = 2,            // Recipient is a copy recipient
        MAPI_BCC        = 3,            // Recipient is blind copy recipient
        MAPI_P1         = 0x10000000,   // Recipient is a P1 resend recipient
        MAPI_SUBMITTED  = 0x80000000;   // Recipient is already processed


    /**
     * MAPI IID for properties of type PT_OBJECT which are supported by TNEF.
     */
    public static final GUID
        IID_ISTORAGE    = new GUID("0b000000-0000-0000-c000-000000000046"),
        IID_IMESSAGE    = new GUID("07030200-0000-0000-c000-000000000046"),
        IID_ISTREAM     = new GUID("0c000000-0000-0000-c000-000000000046");

    int type;
    int ID;
    MAPIValue[] values;
    MAPIPropName name;

    /**
     * Constructs an empty MAPIProp.
     */
    public MAPIProp() {}

    /**
     * Constructs a MAPIProp containing the specified values.
     *
     * @param type the property type (from PT_* constants)
     * @param ID the property ID (from PR_*constants)
     * @param values the value(s) of this property
     */
    public MAPIProp(int type, int ID, MAPIValue[] values) {
        this.type = type;
        this.ID = ID;
        this.values = values;
    }

    /**
     * Gets the MAPIProp type.
     *
     * @return the MAPIProp type
     */
    public int getType() {
        return this.type;
    }

    /**
     * Gets the MAPIProp ID.
     *
     * @return the MAPIProp ID
     */
    public int getID() {
        return this.ID;
    }

    /**
     * Gets the number of MAPIProp values.
     *
     * @return the number of MAPIProp values
     */
    public int getLength() {
        return this.values == null ? 0 : this.values.length;
    }

    /**
     * Gets the MAPIProp values.
     *
     * @return the MAPIProp values
     */
    public MAPIValue[] getValues() {
        return this.values;
    }

    /**
     * Gets the first MAPIProp value.
     * This is a convenience method for single-value properties.
     *
     * @return the first MAPIProp value
     * @throws IOException if an I/O error occurs
     */
    public Object getValue() throws IOException {
        return (getLength() > 0 && this.values[0] != null) ? this.values[0].getValue() : null;
    }

    /**
     * Gets the MAPIProp name.
     *
     * @return the MAPIProp name
     */
    public MAPIPropName getName() {
        return this.name;
    }

    /**
     * Sets the MAPIProp name.
     *
     * @param name the MAPIProp name
     */
    public void setName(MAPIPropName name) {
        this.name = name;
    }

    /**
     * Returns a string representation of this object.
     *
     * @return a string representation of this object
     */
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append("MAPIProp:").append(" type=")
         .append(TNEFUtils.getConstName(this.getClass(), "PT_", getType()));
        if (name != null)
            s.append(" name=").append(getName());
        s.append(" ID=").append(TNEFUtils.getConstName(this.getClass(), "PR_", getID()));
        if (getLength() == 0)
            s.append(" value=").append((Object)null);
        else if (getLength() == 1)
            s.append(" value=").append(getValues()[0]);
        else
            s.append(" values=").append(Arrays.asList(getValues()));
        return s.toString();
    }

    /**
     * Finds a property with the specified ID within given property array.
     *
     * @param props the property array to search
     * @param ID the ID of the property to search for
     * @return a property with given ID found in the property array,
     *         or null if no such property exists
     */
    public static MAPIProp findProp(MAPIProp[] props, int ID) {
        for (int i = 0; props != null && i < props.length; i++) {
            if (props[i].getID() == ID)
                return props[i];
        }
        return null;
    }

    /**
     * Finds a property with the specified name within given property array.
     *
     * @param props the property array to search
     * @param name the property name of the property to search for
     * @return a property with given name found in the property array,
     *         or null if no such property exists
     */
    public static MAPIProp findProp(MAPIProp[] props, MAPIPropName name) {
        for (int i = 0; props != null && i < props.length; i++) {
            if (name.equals(props[i].getName()))
                return props[i];
        }
        return null;
    }

    /**
     * MAPI property ID constant.
     */
    public static final int
        PR_ACKNOWLEDGEMENT_MODE                         = 0x0001,
        PR_ALTERNATE_RECIPIENT_ALLOWED                  = 0x0002,
        PR_AUTHORIZING_USERS                            = 0x0003,
        PR_AUTO_FORWARD_COMMENT                         = 0x0004,
        PR_AUTO_FORWARDED                               = 0x0005,
        PR_CONTENT_CONFIDENTIALITY_ALGORITHM_ID         = 0x0006,
        PR_CONTENT_CORRELATOR                           = 0x0007,
        PR_CONTENT_IDENTIFIER                           = 0x0008,
        PR_CONTENT_LENGTH                               = 0x0009,
        PR_CONTENT_RETURN_REQUESTED                     = 0x000A,
        PR_CONVERSATION_KEY                             = 0x000B,
        PR_CONVERSION_EITS                              = 0x000C,
        PR_CONVERSION_WITH_LOSS_PROHIBITED              = 0x000D,
        PR_CONVERTED_EITS                               = 0x000E,
        PR_DEFERRED_DELIVERY_TIME                       = 0x000F,
        PR_DELIVER_TIME                                 = 0x0010,
        PR_DISCARD_REASON                               = 0x0011,
        PR_DISCLOSURE_OF_RECIPIENTS                     = 0x0012,
        PR_DL_EXPANSION_HISTORY                         = 0x0013,
        PR_DL_EXPANSION_PROHIBITED                      = 0x0014,
        PR_EXPIRY_TIME                                  = 0x0015,
        PR_IMPLICIT_CONVERSION_PROHIBITED               = 0x0016,
        PR_IMPORTANCE                                   = 0x0017,
        PR_IPM_ID                                       = 0x0018,
        PR_LATEST_DELIVERY_TIME                         = 0x0019,
        PR_MESSAGE_CLASS                                = 0x001A,
        PR_MESSAGE_DELIVERY_ID                          = 0x001B,
        PR_MESSAGE_SECURITY_LABEL                       = 0x001E,
        PR_OBSOLETED_IPMS                               = 0x001F,
        PR_ORIGINALLY_INTENDED_RECIPIENT_NAME           = 0x0020,
        PR_ORIGINAL_EITS                                = 0x0021,
        PR_ORIGINATOR_CERTIFICATE                       = 0x0022,
        PR_ORIGINATOR_DELIVERY_REPORT_REQUESTED         = 0x0023,
        PR_ORIGINATOR_RETURN_ADDRESS                    = 0x0024,
        PR_PARENT_KEY                                   = 0x0025,
        PR_PRIORITY                                     = 0x0026,
        PR_ORIGIN_CHECK                                 = 0x0027,
        PR_PROOF_OF_SUBMISSION_REQUESTED                = 0x0028,
        PR_READ_RECEIPT_REQUESTED                       = 0x0029,
        PR_RECEIPT_TIME                                 = 0x002A,
        PR_RECIPIENT_REASSIGNMENT_PROHIBITED            = 0x002B,
        PR_REDIRECTION_HISTORY                          = 0x002C,
        PR_RELATED_IPMS                                 = 0x002D,
        PR_ORIGINAL_SENSITIVITY                         = 0x002E,
        PR_LANGUAGES                                    = 0x002F,
        PR_REPLY_TIME                                   = 0x0030,
        PR_REPORT_TAG                                   = 0x0031,
        PR_REPORT_TIME                                  = 0x0032,
        PR_RETURNED_IPM                                 = 0x0033,
        PR_SECURITY                                     = 0x0034,
        PR_INCOMPLETE_COPY                              = 0x0035,
        PR_SENSITIVITY                                  = 0x0036,
        PR_SUBJECT                                      = 0x0037,
        PR_SUBJECT_IPM                                  = 0x0038,
        PR_CLIENT_SUBMIT_TIME                           = 0x0039,
        PR_REPORT_NAME                                  = 0x003A,
        PR_SENT_REPRESENTING_SEARCH_KEY                 = 0x003B,
        PR_X400_CONTENT_TYPE                            = 0x003C,
        PR_SUBJECT_PREFIX                               = 0x003D,
        PR_NON_RECEIPT_REASON                           = 0x003E,
        PR_RECEIVED_BY_ENTRYID                          = 0x003F,
        PR_RECEIVED_BY_NAME                             = 0x0040,
        PR_SENT_REPRESENTING_ENTRYID                    = 0x0041,
        PR_SENT_REPRESENTING_NAME                       = 0x0042,
        PR_RCVD_REPRESENTING_ENTRYID                    = 0x0043,
        PR_RCVD_REPRESENTING_NAME                       = 0x0044,
        PR_REPORT_ENTRYID                               = 0x0045,
        PR_READ_RECEIPT_ENTRYID                         = 0x0046,
        PR_MESSAGE_SUBMISSION_ID                        = 0x0047,
        PR_PROVIDER_SUBMIT_TIME                         = 0x0048,
        PR_ORIGINAL_SUBJECT                             = 0x0049,
        PR_DISC_VAL                                     = 0x004A,
        PR_ORIG_MESSAGE_CLASS                           = 0x004B,
        PR_ORIGINAL_AUTHOR_ENTRYID                      = 0x004C,
        PR_ORIGINAL_AUTHOR_NAME                         = 0x004D,
        PR_ORIGINAL_SUBMIT_TIME                         = 0x004E,
        PR_REPLY_RECIPIENT_ENTRIES                      = 0x004F,
        PR_REPLY_RECIPIENT_NAMES                        = 0x0050,
        PR_RECEIVED_BY_SEARCH_KEY                       = 0x0051,
        PR_RCVD_REPRESENTING_SEARCH_KEY                 = 0x0052,
        PR_READ_RECEIPT_SEARCH_KEY                      = 0x0053,
        PR_REPORT_SEARCH_KEY                            = 0x0054,
        PR_ORIGINAL_DELIVERY_TIME                       = 0x0055,
        PR_ORIGINAL_AUTHOR_SEARCH_KEY                   = 0x0056,
        PR_MESSAGE_TO_ME                                = 0x0057,
        PR_MESSAGE_CC_ME                                = 0x0058,
        PR_MESSAGE_RECIP_ME                             = 0x0059,
        PR_ORIGINAL_SENDER_NAME                         = 0x005A,
        PR_ORIGINAL_SENDER_ENTRYID                      = 0x005B,
        PR_ORIGINAL_SENDER_SEARCH_KEY                   = 0x005C,
        PR_ORIGINAL_SENT_REPRESENTING_NAME              = 0x005D,
        PR_ORIGINAL_SENT_REPRESENTING_ENTRYID           = 0x005E,
        PR_ORIGINAL_SENT_REPRESENTING_SEARCH_KEY        = 0x005F,
        PR_START_DATE                                   = 0x0060,
        PR_END_DATE                                     = 0x0061,
        PR_OWNER_APPT_ID                                = 0x0062,
        PR_RESPONSE_REQUESTED                           = 0x0063,
        PR_SENT_REPRESENTING_ADDRTYPE                   = 0x0064,
        PR_SENT_REPRESENTING_EMAIL_ADDRESS              = 0x0065,
        PR_ORIGINAL_SENDER_ADDRTYPE                     = 0x0066,
        PR_ORIGINAL_SENDER_EMAIL_ADDRESS                = 0x0067,
        PR_ORIGINAL_SENT_REPRESENTING_ADDRTYPE          = 0x0068,
        PR_ORIGINAL_SENT_REPRESENTING_EMAIL_ADDRESS     = 0x0069,
        PR_CONVERSATION_TOPIC                           = 0x0070,
        PR_CONVERSATION_INDEX                           = 0x0071,
        PR_ORIGINAL_DISPLAY_BCC                         = 0x0072,
        PR_ORIGINAL_DISPLAY_CC                          = 0x0073,
        PR_ORIGINAL_DISPLAY_TO                          = 0x0074,
        PR_RECEIVED_BY_ADDRTYPE                         = 0x0075,
        PR_RECEIVED_BY_EMAIL_ADDRESS                    = 0x0076,
        PR_RCVD_REPRESENTING_ADDRTYPE                   = 0x0077,
        PR_RCVD_REPRESENTING_EMAIL_ADDRESS              = 0x0078,
        PR_ORIGINAL_AUTHOR_ADDRTYPE                     = 0x0079,
        PR_ORIGINAL_AUTHOR_EMAIL_ADDRESS                = 0x007A,
        PR_ORIGINALLY_INTENDED_RECIP_ADDRTYPE           = 0x007B,
        PR_ORIGINALLY_INTENDED_RECIP_EMAIL_ADDRESS      = 0x007C,
        PR_TRANSPORT_MESSAGE_HEADERS                    = 0x007D,
        PR_DELEGATION                                   = 0x007E,
        PR_TNEF_CORRELATION_KEY                         = 0x007F,
        PR_BODY                                         = 0x1000,
        PR_REPORT_TEXT                                  = 0x1001,
        PR_ORIGINATOR_AND_DL_EXPANSION_HISTORY          = 0x1002,
        PR_REPORTING_DL_NAME                            = 0x1003,
        PR_REPORTING_MTA_CERTIFICATE                    = 0x1004,
        PR_RTF_SYNC_BODY_CRC                            = 0x1006,
        PR_RTF_SYNC_BODY_COUNT                          = 0x1007,
        PR_RTF_SYNC_BODY_TAG                            = 0x1008,
        PR_RTF_COMPRESSED                               = 0x1009,
        PR_RTF_SYNC_PREFIX_COUNT                        = 0x1010,
        PR_RTF_SYNC_TRAILING_COUNT                      = 0x1011,
        PR_ORIGINALLY_INTENDED_RECIP_ENTRYID            = 0x1012,
        PR_HTML                                         = 0x1013,
        PR_BODY_HTML                                    = 0x1013,
        PR_CONTENT_INTEGRITY_CHECK                      = 0x0C00,
        PR_EXPLICIT_CONVERSION                          = 0x0C01,
        PR_IPM_RETURN_REQUESTED                         = 0x0C02,
        PR_MESSAGE_TOKEN                                = 0x0C03,
        PR_NDR_REASON_CODE                              = 0x0C04,
        PR_NDR_DIAG_CODE                                = 0x0C05,
        PR_NON_RECEIPT_NOTIFICATION_REQUESTED           = 0x0C06,
        PR_DELIVERY_POINT                               = 0x0C07,
        PR_ORIGINATOR_NON_DELIVERY_REPORT_REQUESTED     = 0x0C08,
        PR_ORIGINATOR_REQUESTED_ALTERNATE_RECIPIENT     = 0x0C09,
        PR_PHYSICAL_DELIVERY_BUREAU_FAX_DELIVERY        = 0x0C0A,
        PR_PHYSICAL_DELIVERY_MODE                       = 0x0C0B,
        PR_PHYSICAL_DELIVERY_REPORT_REQUEST             = 0x0C0C,
        PR_PHYSICAL_FORWARDING_ADDRESS                  = 0x0C0D,
        PR_PHYSICAL_FORWARDING_ADDRESS_REQUESTED        = 0x0C0E,
        PR_PHYSICAL_FORWARDING_PROHIBITED               = 0x0C0F,
        PR_PHYSICAL_RENDITION_ATTRIBUTES                = 0x0C10,
        PR_PROOF_OF_DELIVERY                            = 0x0C11,
        PR_PROOF_OF_DELIVERY_REQUESTED                  = 0x0C12,
        PR_RECIPIENT_CERTIFICATE                        = 0x0C13,
        PR_RECIPIENT_NUMBER_FOR_ADVICE                  = 0x0C14,
        PR_RECIPIENT_TYPE                               = 0x0C15,
        PR_REGISTERED_MAIL_TYPE                         = 0x0C16,
        PR_REPLY_REQUESTED                              = 0x0C17,
        PR_REQUESTED_DELIVERY_METHOD                    = 0x0C18,
        PR_SENDER_ENTRYID                               = 0x0C19,
        PR_SENDER_NAME                                  = 0x0C1A,
        PR_SUPPLEMENTARY_INFO                           = 0x0C1B,
        PR_TYPE_OF_MTS_USER                             = 0x0C1C,
        PR_SENDER_SEARCH_KEY                            = 0x0C1D,
        PR_SENDER_ADDRTYPE                              = 0x0C1E,
        PR_SENDER_EMAIL_ADDRESS                         = 0x0C1F,
        PR_CURRENT_VERSION                              = 0x0E00,
        PR_DELETE_AFTER_SUBMIT                          = 0x0E01,
        PR_DISPLAY_BCC                                  = 0x0E02,
        PR_DISPLAY_CC                                   = 0x0E03,
        PR_DISPLAY_TO                                   = 0x0E04,
        PR_PARENT_DISPLAY                               = 0x0E05,
        PR_MESSAGE_DELIVERY_TIME                        = 0x0E06,
        PR_MESSAGE_FLAGS                                = 0x0E07,
        PR_MESSAGE_SIZE                                 = 0x0E08,
        PR_PARENT_ENTRYID                               = 0x0E09,
        PR_SENTMAIL_ENTRYID                             = 0x0E0A,
        PR_CORRELATE                                    = 0x0E0C,
        PR_CORRELATE_MTSID                              = 0x0E0D,
        PR_DISCRETE_VALUES                              = 0x0E0E,
        PR_RESPONSIBILITY                               = 0x0E0F,
        PR_SPOOLER_STATUS                               = 0x0E10,
        PR_TRANSPORT_STATUS                             = 0x0E11,
        PR_MESSAGE_RECIPIENTS                           = 0x0E12,
        PR_MESSAGE_ATTACHMENTS                          = 0x0E13,
        PR_SUBMIT_FLAGS                                 = 0x0E14,
        PR_RECIPIENT_STATUS                             = 0x0E15,
        PR_TRANSPORT_KEY                                = 0x0E16,
        PR_MSG_STATUS                                   = 0x0E17,
        PR_MESSAGE_DOWNLOAD_TIME                        = 0x0E18,
        PR_CREATION_VERSION                             = 0x0E19,
        PR_MODIFY_VERSION                               = 0x0E1A,
        PR_HASATTACH                                    = 0x0E1B,
        PR_BODY_CRC                                     = 0x0E1C,
        PR_NORMALIZED_SUBJECT                           = 0x0E1D,
        PR_RTF_IN_SYNC                                  = 0x0E1F,
        PR_ATTACH_SIZE                                  = 0x0E20,
        PR_ATTACH_NUM                                   = 0x0E21,
        PR_PREPROCESS                                   = 0x0E22,
        PR_ORIGINATING_MTA_CERTIFICATE                  = 0x0E25,
        PR_PROOF_OF_SUBMISSION                          = 0x0E26,
        PR_ENTRYID                                      = 0x0FFF,
        PR_OBJECT_TYPE                                  = 0x0FFE,
        PR_ICON                                         = 0x0FFD,
        PR_MINI_ICON                                    = 0x0FFC,
        PR_STORE_ENTRYID                                = 0x0FFB,
        PR_STORE_RECORD_KEY                             = 0x0FFA,
        PR_RECORD_KEY                                   = 0x0FF9,
        PR_MAPPING_SIGNATURE                            = 0x0FF8,
        PR_ACCESS_LEVEL                                 = 0x0FF7,
        PR_INSTANCE_KEY                                 = 0x0FF6,
        PR_ROW_TYPE                                     = 0x0FF5,
        PR_ACCESS                                       = 0x0FF4,
        PR_ROWID                                        = 0x3000,
        PR_DISPLAY_NAME                                 = 0x3001,
        PR_ADDRTYPE                                     = 0x3002,
        PR_EMAIL_ADDRESS                                = 0x3003,
        PR_COMMENT                                      = 0x3004,
        PR_DEPTH                                        = 0x3005,
        PR_PROVIDER_DISPLAY                             = 0x3006,
        PR_CREATION_TIME                                = 0x3007,
        PR_LAST_MODIFICATION_TIME                       = 0x3008,
        PR_RESOURCE_FLAGS                               = 0x3009,
        PR_PROVIDER_DLL_NAME                            = 0x300A,
        PR_SEARCH_KEY                                   = 0x300B,
        PR_PROVIDER_UID                                 = 0x300C,
        PR_PROVIDER_ORDINAL                             = 0x300D,
        PR_FORM_VERSION                                 = 0x3301,
        PR_FORM_CLSID                                   = 0x3302,
        PR_FORM_CONTACT_NAME                            = 0x3303,
        PR_FORM_CATEGORY                                = 0x3304,
        PR_FORM_CATEGORY_SUB                            = 0x3305,
        PR_FORM_HOST_MAP                                = 0x3306,
        PR_FORM_HIDDEN                                  = 0x3307,
        PR_FORM_DESIGNER_NAME                           = 0x3308,
        PR_FORM_DESIGNER_GUID                           = 0x3309,
        PR_FORM_MESSAGE_BEHAVIOR                        = 0x330A,
        PR_DEFAULT_STORE                                = 0x3400,
        PR_STORE_SUPPORT_MASK                           = 0x340D,
        PR_STORE_STATE                                  = 0x340E,
        PR_IPM_SUBTREE_SEARCH_KEY                       = 0x3410,
        PR_IPM_OUTBOX_SEARCH_KEY                        = 0x3411,
        PR_IPM_WASTEBASKET_SEARCH_KEY                   = 0x3412,
        PR_IPM_SENTMAIL_SEARCH_KEY                      = 0x3413,
        PR_MDB_PROVIDER                                 = 0x3414,
        PR_RECEIVE_FOLDER_SETTINGS                      = 0x3415,
        PR_VALID_FOLDER_MASK                            = 0x35DF,
        PR_IPM_SUBTREE_ENTRYID                          = 0x35E0,
        PR_IPM_OUTBOX_ENTRYID                           = 0x35E2,
        PR_IPM_WASTEBASKET_ENTRYID                      = 0x35E3,
        PR_IPM_SENTMAIL_ENTRYID                         = 0x35E4,
        PR_VIEWS_ENTRYID                                = 0x35E5,
        PR_COMMON_VIEWS_ENTRYID                         = 0x35E6,
        PR_FINDER_ENTRYID                               = 0x35E7,
        PR_CONTAINER_FLAGS                              = 0x3600,
        PR_FOLDER_TYPE                                  = 0x3601,
        PR_CONTENT_COUNT                                = 0x3602,
        PR_CONTENT_UNREAD                               = 0x3603,
        PR_CREATE_TEMPLATES                             = 0x3604,
        PR_DETAILS_TABLE                                = 0x3605,
        PR_SEARCH                                       = 0x3607,
        PR_SELECTABLE                                   = 0x3609,
        PR_SUBFOLDERS                                   = 0x360A,
        PR_STATUS                                       = 0x360B,
        PR_ANR                                          = 0x360C,
        PR_CONTENTS_SORT_ORDER                          = 0x360D,
        PR_CONTAINER_HIERARCHY                          = 0x360E,
        PR_CONTAINER_CONTENTS                           = 0x360F,
        PR_FOLDER_ASSOCIATED_CONTENTS                   = 0x3610,
        PR_DEF_CREATE_DL                                = 0x3611,
        PR_DEF_CREATE_MAILUSER                          = 0x3612,
        PR_CONTAINER_CLASS                              = 0x3613,
        PR_CONTAINER_MODIFY_VERSION                     = 0x3614,
        PR_AB_PROVIDER_ID                               = 0x3615,
        PR_DEFAULT_VIEW_ENTRYID                         = 0x3616,
        PR_ASSOC_CONTENT_COUNT                          = 0x3617,
        PR_ATTACHMENT_X400_PARAMETERS                   = 0x3700,
        PR_ATTACH_DATA_OBJ                              = 0x3701,
        PR_ATTACH_DATA_BIN                              = 0x3701,
        PR_ATTACH_ENCODING                              = 0x3702,
        PR_ATTACH_EXTENSION                             = 0x3703,
        PR_ATTACH_FILENAME                              = 0x3704,
        PR_ATTACH_METHOD                                = 0x3705,
        PR_ATTACH_LONG_FILENAME                         = 0x3707,
        PR_ATTACH_PATHNAME                              = 0x3708,
        PR_ATTACH_RENDERING                             = 0x3709,
        PR_ATTACH_TAG                                   = 0x370A,
        PR_RENDERING_POSITION                           = 0x370B,
        PR_ATTACH_TRANSPORT_NAME                        = 0x370C,
        PR_ATTACH_LONG_PATHNAME                         = 0x370D,
        PR_ATTACH_MIME_TAG                              = 0x370E,
        PR_ATTACH_ADDITIONAL_INFO                       = 0x370F,
        PR_DISPLAY_TYPE                                 = 0x3900,
        PR_TEMPLATEID                                   = 0x3902,
        PR_PRIMARY_CAPABILITY                           = 0x3904,
        PR_7BIT_DISPLAY_NAME                            = 0x39FF,
        PR_ACCOUNT                                      = 0x3A00,
        PR_ALTERNATE_RECIPIENT                          = 0x3A01,
        PR_CALLBACK_TELEPHONE_NUMBER                    = 0x3A02,
        PR_CONVERSION_PROHIBITED                        = 0x3A03,
        PR_DISCLOSE_RECIPIENTS                          = 0x3A04,
        PR_GENERATION                                   = 0x3A05,
        PR_GIVEN_NAME                                   = 0x3A06,
        PR_GOVERNMENT_ID_NUMBER                         = 0x3A07,
        PR_BUSINESS_TELEPHONE_NUMBER                    = 0x3A08,
        PR_HOME_TELEPHONE_NUMBER                        = 0x3A09,
        PR_INITIALS                                     = 0x3A0A,
        PR_KEYWORD                                      = 0x3A0B,
        PR_LANGUAGE                                     = 0x3A0C,
        PR_LOCATION                                     = 0x3A0D,
        PR_MAIL_PERMISSION                              = 0x3A0E,
        PR_MHS_COMMON_NAME                              = 0x3A0F,
        PR_ORGANIZATIONAL_ID_NUMBER                     = 0x3A10,
        PR_SURNAME                                      = 0x3A11,
        PR_ORIGINAL_ENTRYID                             = 0x3A12,
        PR_ORIGINAL_DISPLAY_NAME                        = 0x3A13,
        PR_ORIGINAL_SEARCH_KEY                          = 0x3A14,
        PR_POSTAL_ADDRESS                               = 0x3A15,
        PR_COMPANY_NAME                                 = 0x3A16,
        PR_TITLE                                        = 0x3A17,
        PR_DEPARTMENT_NAME                              = 0x3A18,
        PR_OFFICE_LOCATION                              = 0x3A19,
        PR_PRIMARY_TELEPHONE_NUMBER                     = 0x3A1A,
        PR_BUSINESS2_TELEPHONE_NUMBER                   = 0x3A1B,
        PR_MOBILE_TELEPHONE_NUMBER                      = 0x3A1C,
        PR_RADIO_TELEPHONE_NUMBER                       = 0x3A1D,
        PR_CAR_TELEPHONE_NUMBER                         = 0x3A1E,
        PR_OTHER_TELEPHONE_NUMBER                       = 0x3A1F,
        PR_TRANSMITABLE_DISPLAY_NAME                    = 0x3A20,
        PR_PAGER_TELEPHONE_NUMBER                       = 0x3A21,
        PR_USER_CERTIFICATE                             = 0x3A22,
        PR_PRIMARY_FAX_NUMBER                           = 0x3A23,
        PR_BUSINESS_FAX_NUMBER                          = 0x3A24,
        PR_HOME_FAX_NUMBER                              = 0x3A25,
        PR_COUNTRY                                      = 0x3A26,
        PR_LOCALITY                                     = 0x3A27,
        PR_STATE_OR_PROVINCE                            = 0x3A28,
        PR_STREET_ADDRESS                               = 0x3A29,
        PR_POSTAL_CODE                                  = 0x3A2A,
        PR_POST_OFFICE_BOX                              = 0x3A2B,
        PR_TELEX_NUMBER                                 = 0x3A2C,
        PR_ISDN_NUMBER                                  = 0x3A2D,
        PR_ASSISTANT_TELEPHONE_NUMBER                   = 0x3A2E,
        PR_HOME2_TELEPHONE_NUMBER                       = 0x3A2F,
        PR_ASSISTANT                                    = 0x3A30,
        PR_SEND_RICH_INFO                               = 0x3A40,
        PR_WEDDING_ANNIVERSARY                          = 0x3A41,
        PR_BIRTHDAY                                     = 0x3A42,
        PR_HOBBIES                                      = 0x3A43,
        PR_MIDDLE_NAME                                  = 0x3A44,
        PR_DISPLAY_NAME_PREFIX                          = 0x3A45,
        PR_PROFESSION                                   = 0x3A46,
        PR_PREFERRED_BY_NAME                            = 0x3A47,
        PR_SPOUSE_NAME                                  = 0x3A48,
        PR_COMPUTER_NETWORK_NAME                        = 0x3A49,
        PR_CUSTOMER_ID                                  = 0x3A4A,
        PR_TTYTDD_PHONE_NUMBER                          = 0x3A4B,
        PR_FTP_SITE                                     = 0x3A4C,
        PR_GENDER                                       = 0x3A4D,
        PR_MANAGER_NAME                                 = 0x3A4E,
        PR_NICKNAME                                     = 0x3A4F,
        PR_PERSONAL_HOME_PAGE                           = 0x3A50,
        PR_BUSINESS_HOME_PAGE                           = 0x3A51,
        PR_CONTACT_VERSION                              = 0x3A52,
        PR_CONTACT_ENTRYIDS                             = 0x3A53,
        PR_CONTACT_ADDRTYPES                            = 0x3A54,
        PR_CONTACT_DEFAULT_ADDRESS_INDEX                = 0x3A55,
        PR_CONTACT_EMAIL_ADDRESSES                      = 0x3A56,
        PR_COMPANY_MAIN_PHONE_NUMBER                    = 0x3A57,
        PR_CHILDRENS_NAMES                              = 0x3A58,
        PR_HOME_ADDRESS_CITY                            = 0x3A59,
        PR_HOME_ADDRESS_COUNTRY                         = 0x3A5A,
        PR_HOME_ADDRESS_POSTAL_CODE                     = 0x3A5B,
        PR_HOME_ADDRESS_STATE_OR_PROVINCE               = 0x3A5C,
        PR_HOME_ADDRESS_STREET                          = 0x3A5D,
        PR_HOME_ADDRESS_POST_OFFICE_BOX                 = 0x3A5E,
        PR_OTHER_ADDRESS_CITY                           = 0x3A5F,
        PR_OTHER_ADDRESS_COUNTRY                        = 0x3A60,
        PR_OTHER_ADDRESS_POSTAL_CODE                    = 0x3A61,
        PR_OTHER_ADDRESS_STATE_OR_PROVINCE              = 0x3A62,
        PR_OTHER_ADDRESS_STREET                         = 0x3A63,
        PR_OTHER_ADDRESS_POST_OFFICE_BOX                = 0x3A64,
        PR_STORE_PROVIDERS                              = 0x3D00,
        PR_AB_PROVIDERS                                 = 0x3D01,
        PR_TRANSPORT_PROVIDERS                          = 0x3D02,
        PR_DEFAULT_PROFILE                              = 0x3D04,
        PR_AB_SEARCH_PATH                               = 0x3D05,
        PR_AB_DEFAULT_DIR                               = 0x3D06,
        PR_AB_DEFAULT_PAB                               = 0x3D07,
        PR_FILTERING_HOOKS                              = 0x3D08,
        PR_SERVICE_NAME                                 = 0x3D09,
        PR_SERVICE_DLL_NAME                             = 0x3D0A,
        PR_SERVICE_ENTRY_NAME                           = 0x3D0B,
        PR_SERVICE_UID                                  = 0x3D0C,
        PR_SERVICE_EXTRA_UIDS                           = 0x3D0D,
        PR_SERVICES                                     = 0x3D0E,
        PR_SERVICE_SUPPORT_FILES                        = 0x3D0F,
        PR_SERVICE_DELETE_FILES                         = 0x3D10,
        PR_AB_SEARCH_PATH_UPDATE                        = 0x3D11,
        PR_PROFILE_NAME                                 = 0x3D12,
        PR_IDENTITY_DISPLAY                             = 0x3E00,
        PR_IDENTITY_ENTRYID                             = 0x3E01,
        PR_RESOURCE_METHODS                             = 0x3E02,
        PR_RESOURCE_TYPE                                = 0x3E03,
        PR_STATUS_CODE                                  = 0x3E04,
        PR_IDENTITY_SEARCH_KEY                          = 0x3E05,
        PR_OWN_STORE_ENTRYID                            = 0x3E06,
        PR_RESOURCE_PATH                                = 0x3E07,
        PR_STATUS_STRING                                = 0x3E08,
        PR_X400_DEFERRED_DELIVERY_CANCEL                = 0x3E09,
        PR_HEADER_FOLDER_ENTRYID                        = 0x3E0A,
        PR_REMOTE_PROGRESS                              = 0x3E0B,
        PR_REMOTE_PROGRESS_TEXT                         = 0x3E0C,
        PR_REMOTE_VALIDATE_OK                           = 0x3E0D,
        PR_CONTROL_FLAGS                                = 0x3F00,
        PR_CONTROL_STRUCTURE                            = 0x3F01,
        PR_CONTROL_TYPE                                 = 0x3F02,
        PR_DELTAX                                       = 0x3F03,
        PR_DELTAY                                       = 0x3F04,
        PR_XPOS                                         = 0x3F05,
        PR_YPOS                                         = 0x3F06,
        PR_CONTROL_ID                                   = 0x3F07,
        PR_INITIAL_DETAILS_PANE                         = 0x3F08,
        PROP_ID_SECURE_MIN                              = 0x67F0,
        PROP_ID_SECURE_MAX                              = 0x67FF;

    /**
     * MAPI property ID constant.
     */
    public static final int
        PR_USER_X509_CERTIFICATE                = 0x3A70,
        PR_OFFICE_TELEPHONE_NUMBER              = PR_BUSINESS_TELEPHONE_NUMBER,
        PR_OFFICE2_TELEPHONE_NUMBER             = PR_BUSINESS2_TELEPHONE_NUMBER,
        PR_CELLULAR_TELEPHONE_NUMBER            = PR_MOBILE_TELEPHONE_NUMBER,
        PR_BEEPER_TELEPHONE_NUMBER              = PR_PAGER_TELEPHONE_NUMBER,
        PR_BUSINESS_ADDRESS_COUNTRY             = PR_COUNTRY,
        PR_BUSINESS_ADDRESS_CITY                = PR_LOCALITY,
        PR_BUSINESS_ADDRESS_STATE_OR_PROVINCE   = PR_STATE_OR_PROVINCE,
        PR_BUSINESS_ADDRESS_STREET              = PR_STREET_ADDRESS,
        PR_BUSINESS_ADDRESS_POSTAL_CODE         = PR_POSTAL_CODE,
        PR_BUSINESS_ADDRESS_POST_OFFICE_BOX     = PR_POST_OFFICE_BOX;

    /**
     * Exchange property ID constant.
     */
    public static final int
        PR_MTS_ID                                       = 0x0047,
        PR_MTS_REPORT_ID                                = 0x0047,
        PR_MESSAGE_SIZE_EXTENDED                        = 0x0E08,
        PR_MEMBER_ENTRYID                               = 0x0FFF,
        PR_OWA_URL                                      = 0x10F1,
        PR_DISABLE_FULL_FIDELITY                        = 0x10F2,
        PR_P1_CONTENT                                   = 0x1100,
        PR_P1_CONTENT_TYPE                              = 0x1101,
        PR_EMS_AB_DISPLAY_NAME_PRINTABLE                = 0x39FF,
        PR_PREVIEW_UNREAD                               = 0x3FD8,
        PR_PREVIEW                                      = 0x3FD9,
        PR_ABSTRACT                                     = 0x3FDA,
        PR_DL_REPORT_FLAGS                              = 0x3FDB,
        PR_BILATERAL_INFO                               = 0x3FDC,
        PR_MSG_BODY_ID                                  = 0x3FDD,
        PR_INTERNET_CPID                                = 0x3FDE,
        PR_AUTO_RESPONSE_SUPPRESS                       = 0x3FDF,
        PR_ACL_TABLE                                    = 0x3FE0,
        PR_ACL_DATA                                     = 0x3FE0,
        PR_RULES_TABLE                                  = 0x3FE1,
        PR_RULES_DATA                                   = 0x3FE1,
        PR_FOLDER_DESIGN_FLAGS                          = 0x3FE2,
        PR_DELEGATED_BY_RULE                            = 0x3FE3,
        PR_DESIGN_IN_PROGRESS                           = 0x3FE4,
        PR_SECURE_ORIGINATION                           = 0x3FE5,
        PR_PUBLISH_IN_ADDRESS_BOOK                      = 0x3FE6,
        PR_RESOLVE_METHOD                               = 0x3FE7,
        PR_ADDRESS_BOOK_DISPLAY_NAME                    = 0x3FE8,
        PR_EFORMS_LOCALE_ID                             = 0x3FE9,
        PR_HAS_DAMS                                     = 0x3FEA,
        PR_DEFERRED_SEND_NUMBER                         = 0x3FEB,
        PR_DEFERRED_SEND_UNITS                          = 0x3FEC,
        PR_EXPIRY_NUMBER                                = 0x3FED,
        PR_EXPIRY_UNITS                                 = 0x3FEE,
        PR_DEFERRED_SEND_TIME                           = 0x3FEF,
        PR_CONFLICT_ENTRYID                             = 0x3FF0,
        PR_MESSAGE_LOCALE_ID                            = 0x3FF1,
        PR_RULE_TRIGGER_HISTORY                         = 0x3FF2,
        PR_MOVE_TO_STORE_ENTRYID                        = 0x3FF3,
        PR_MOVE_TO_FOLDER_ENTRYID                       = 0x3FF4,
        PR_STORAGE_QUOTA_LIMIT                          = 0x3FF5,
        PR_EXCESS_STORAGE_USED                          = 0x3FF6,
        PR_SVR_GENERATING_QUOTA_MSG                     = 0x3FF7,
        PR_CREATOR_NAME                                 = 0x3FF8,
        PR_CREATOR_ENTRYID                              = 0x3FF9,
        PR_LAST_MODIFIER_NAME                           = 0x3FFA,
        PR_LAST_MODIFIER_ENTRYID                        = 0x3FFB,
        PR_REPLY_RECIPIENT_SMTP_PROXIES                 = 0x3FFC,
        PR_MESSAGE_CODEPAGE                             = 0x3FFD,
        PR_EXTENDED_ACL_DATA                            = 0x3FFE;

    /**
     * MAPI property name GUID constant.
     */
    public static final GUID
        // Used in appointment item properties. Message class: IPM.Appointment
        GUID_CDOPROPSETID1 = new GUID("02200600-0000-0000-c000-000000000046"),
        // Used in task item properties. Message class: IPM.Task
        GUID_CDOPROPSETID2 = new GUID("03200600-0000-0000-c000-000000000046"),
        // Used in contact item properties. Message class: IPM.Contact
        GUID_CDOPROPSETID3 = new GUID("04200600-0000-0000-c000-000000000046"),
        // Common Outlook ID. Used with common contact, task and appointment
        // item properties (reminders)
        GUID_CDOPROPSETID4 = new GUID("08200600-0000-0000-c000-000000000046"),
        // Generic MAPI ID. Used with all type of item properties (categories)
        GUID_CDOPROPSETID5 = new GUID("29030200-0000-0000-c000-000000000046"),
        // Used in note item properties. Message class: IPM.StickyNote
        GUID_CDOPROPSETID6 = new GUID("0e200600-0000-0000-c000-000000000046"),
        // Used in journal item properties. Message class: IPM.Activity
        GUID_CDOPROPSETID7 = new GUID("0a200600-0000-0000-c000-000000000046");

}
