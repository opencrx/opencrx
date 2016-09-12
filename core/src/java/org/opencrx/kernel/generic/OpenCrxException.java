/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2005, CRIXP Corp., Switzerland
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of CRIXP Corp. nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.kernel.generic;

public class OpenCrxException {
    
    public static final String DOMAIN = "OpenCrxDomain";
    
    // openCRX kernel: Access Control
    public static final int AUTHORIZATION_FAILURE_UPDATE = 1000;
    public static final int AUTHORIZATION_FAILURE_DELETE = 1001;
    public static final int AUTHORIZATION_FAILURE_CREATE = 1002;
    public static final int AUTHORIZATION_FAILURE_MISSING_PRINCIPAL = 1003;    
    public static final int AUTHORIZATION_FAILURE_READ = 1004;    
    
    // openCRX kernel: Activities
    public static final int INCIDENT_USER_DOES_NOT_MATCH_CONTACT = 1100;
    public static final int INCIDENT_STATE_MUST_BE_NA_OR_OPEN = 1101;
    public static final int INCIDENT_STATE_MUST_OPEN = 1102;
    public static final int INCIDENT_STATE_MUST_BE_COMPLETE = 1103;    
    public static final int ACTIVITY_GROUP_HAS_ASSIGNED_ACTIVITIES = 1104;
    public static final int ACTIVITY_UNDEFINED_NEXT_STATE = 1105;
    public static final int ACTIVITY_TRANSITION_NOT_VALID_FOR_STATE = 1106;
    public static final int ACTIVITY_CAN_NOT_ADD_WORK_RECORD_MISSING_RESOURCE = 1107;
    public static final int ACTIVITY_CAN_NOT_ADD_WORK_RECORD_MISSING_ACTIVITY = 1108;
   
    // openCRX kernel: Depot
    public static final int DEPOT_MISSING_BOOKING_TEXT = 1200;    
    public static final int DEPOT_INVALID_COMPOUND_BOOKING = 1201;    
    public static final int DEPOT_MISSING_QUANTITY = 1203;    
    public static final int DEPOT_POSITION_NAME_MISMATCH = 1204;
    public static final int DEPOT_MISSING_PRODUCT = 1205;
    public static final int DEPOT_INVALID_POSITION_CREDIT = 1206;
    public static final int DEPOT_INVALID_POSITION_DEBIT = 1207;
    public static final int DEPOT_MISSING_DEPOT_NUMBER = 1208;
    public static final int DEPOT_DEPOT_NOT_FOUND = 1209;
    public static final int DEPOT_BOOKING_PERIOD_NOT_FOUND = 1210;
    public static final int DEPOT_BOOKING_PERIOD_IS_CLOSED = 1211;
    public static final int DEPOT_BOOKING_PERIOD_IS_FINAL = 1212;
    public static final int DEPOT_DEPOT_IS_LOCKED = 1213;
    public static final int DEPOT_DEPOT_IS_CLOSED_CAN_NOT_BOOK = 1214;
    public static final int DEPOT_DEPOT_IS_NOT_OPEN = 1215;
    public static final int DEPOT_POSITION_IS_LOCKED = 1216;
    public static final int DEPOT_POSITION_IS_CLOSED = 1217;
    public static final int DEPOT_POSITION_IS_NOT_OPEN = 1218;
    public static final int DEPOT_REVERSAL_BALANCE_MISMATCH = 1219;
    public static final int DEPOT_CAN_NOT_CANCEL_REVERSAL_BOOKING = 1220;
    public static final int DEPOT_ALREADY_HAS_REVERSAL_BOOKING = 1221;
    public static final int DEPOT_CAN_NOT_REMOVE_BOOKING = 1222;
    public static final int BOOKING_STATUS_MUST_BE_PENDING = 1223;
    public static final int BOOKING_STATUS_MUST_BE_PROCESSED = 1224;
    public static final int BOOKING_DEPOT_NUMBER_REQUIRED = 1225;
    public static final int BOOKING_DEPOT_IS_CLOSED_CAN_NOT_CLOSE = 1226;
    public static final int BOOKING_DEPOT_POSITION_IS_CLOSED_CAN_NOT_CLOSE = 1227;
    public static final int DEPOT_ENTITY_HAS_BOOKINGS = 1228;
    public static final int DEPOT_CONTRACT_HAS_BOOKINGS = 1229;
    public static final int DEPOT_DEPOT_HAS_BOOKINGS = 1230;
    public static final int DEPOT_POSITION_HAS_BOOKINGS = 1231;
    public static final int DEPOT_BALANCE_MISMATCH = 1258;

    // openCRX kernel: Contract
    public static final int CONTRACT_MISSING_PRODUCT_OFFERING = 1232;
    public static final int CONTRACT_MAX_POSITIONS_REACHED = 1233;
    public static final int CONTRACT_MIN_POSITIONS_REACHED = 1234;
    public static final int BOOKING_IS_LOCKED_CAN_NOT_DELETE = 1235;
    public static final int CONTRACT_MISSING_DEPOT_GOODS_ISSUE = 1236;
    public static final int CONTRACT_MISSING_DEPOT_GOODS_RETURN = 1237;
    public static final int CONTRACT_MISSING_DEPOT_GOODS_DELIVERY = 1238;
    
    // openCRX kernel: Workflow
    public static final int WORKFLOW_MISSING_WORKFLOW = 1239;
    public static final int WORKFLOW_MISSING_TARGET = 1240;
    public static final int WORKFLOW_NO_IMPLEMENTATION = 1241;
    public static final int WORKFLOW_MISSING_CONSTRUCTOR = 1242;
    public static final int WORKFLOW_CAN_NOT_INSTANTIATE = 1243;
    public static final int WORKFLOW_ILLEGAL_ACCESS = 1244;
    public static final int WORKFLOW_ILLEGAL_ARGUMENT = 1245;
    public static final int WORKFLOW_CAN_NOT_INVOKE = 1246;
    public static final int WORKFLOW_CAN_NOT_CREATE_PROCESS_INSTANCE = 1247;
    
    // openCRX kernel: Depot
    public static final int DEPOT_DEPOT_ENTITY_NOT_EQUAL = 1248;
    
    // openCRX kernel: Product
    public static final int PRODUCT_OPERATION_NOT_ALLOWED_FOR_NON_BASIC_PRICE_LEVEL = 1251;
    public static final int PRODUCT_OPERATION_NOT_ALLOWED_FOR_FINAL_PRICE_LEVEL = 1252;
    public static final int PRODUCT_PRICE_LEVEL_MUST_HAVE_CURRENCY = 1253;
    public static final int PRODUCT_OPERATION_NOT_ALLOWED_FOR_BASEDON_PRICE_LEVEL = 1254;
    public static final int PRODUCT_OPERATION_NOT_ALLOWED_FOR_PRICE_LEVEL_HAVING_PRICES = 1255;
    public static final int PRODUCT_OPERATION_NOT_ALLOWED_FOR_BASIC_PRICE_LEVEL = 1256;
    public static final int PRODUCT_GET_PRICELEVEL_SCRIPT_ERROR = 1257;

    // openCRX kernel: Common
    public static final int ASSERTION_FAILURE = 10000;
    public static final int MEDIA_ACCESS_FAILURE = 10001;
    public static final int REFERENCE_IS_READONLY = 10002;
    public static final int OBJECT_TYPE_IS_READONLY = 10003;
    public static final int DUPLICATE_OBJECT = 10004;

    // User-defined: >= 20000
}

//--- End of File -----------------------------------------------------------
