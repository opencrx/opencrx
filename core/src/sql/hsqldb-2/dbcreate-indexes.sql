/* This software is published under the BSD license                          */
/* as listed below.                                                          */
/*                                                                           */
/* Redistribution and use in source and binary forms, with or without        */
/* modification, are permitted provided that the following conditions        */
/* are met:                                                                  */
/*                                                                           */
/* * Redistributions of source code must retain the above copyright          */
/* notice, this list of conditions and the following disclaimer.             */
/*                                                                           */
/* * Redistributions in binary form must reproduce the above copyright       */
/* notice, this list of conditions and the following disclaimer in           */
/* the documentation and/or other materials provided with the                */
/* distribution.                                                             */
/*                                                                           */
/* * Neither the name of the openCRX team nor the names of the contributors  */
/* to openCRX may be used to endorse or promote products derived             */
/* from this software without specific prior written permission              */
/*                                                                           */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND                    */
/* CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,               */
/* INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF                  */
/* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE                  */
/* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS         */
/* BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,                  */
/* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED           */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,             */
/* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON         */
/* ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,           */
/* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY            */
/* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE                   */
/* POSSIBILITY OF SUCH DAMAGE.                                               */
/*                                                                           */
/* ------------------                                                        */
/*                                                                           */
/* This product includes software developed by the Apache Software           */
/* Foundation (http://www.apache.org/).                                      */
/*                                                                           */
/* This product includes software developed by contributors to               */
/* openMDX (http://www.openmdx.org/)                                         */
/*                                                                           */

CREATE INDEX I_OOCKE1_BOOKING_DTYPE ON OOCKE1_BOOKING(dtype) ;

CREATE INDEX I_OOCKE1_INVENTORYLEVEL_VALUE_DATE ON OOCKE1_INVENTORYLEVEL(value_date) ;
CREATE INDEX I_OOCKE1_INVENTORYLEVEL_POSITION ON OOCKE1_INVENTORYLEVEL(position) ;
CREATE INDEX I_OOCKE1_INVENTORYLEVEL_INVENTORY_LEVEL_STATUS ON OOCKE1_INVENTORYLEVEL(inventory_level_status) ;
CREATE INDEX I_OOCKE1_INVENTORYLEVEL_INVENTORY_LEVEL_TYPE ON OOCKE1_INVENTORYLEVEL(inventory_level_type) ;

CREATE INDEX I_OOCKE1_ACCOUNT_FULL_NAME ON OOCKE1_ACCOUNT(full_name) ;
CREATE INDEX I_OOCKE1_ACCOUNT_ALIAS_NAME ON OOCKE1_ACCOUNT(alias_name) ;
CREATE INDEX I_OOCKE1_ACCOUNT_EXTERNAL_LINK ON OOCKE1_ACCOUNT_(external_link) ;
CREATE INDEX I_OOCKE1_ACCOUNT_MOD_AT ON OOCKE1_ACCOUNT(modified_at);

CREATE INDEX I_OOCKE1_ADDRESS_PARENT ON OOCKE1_ADDRESS("P$$PARENT") ;
CREATE INDEX I_OOCKE1_ADDRESS_EMAILADDRESS ON OOCKE1_ADDRESS(email_address) ;
CREATE INDEX I_OOCKE1_ADDRESS_MOD_AT ON OOCKE1_ADDRESS(modified_at);

CREATE INDEX I_OOCKE1_ACTIVITY_ASSIGNED_TO ON OOCKE1_ACTIVITY(assigned_to) ;
CREATE INDEX I_OOCKE1_ACTIVITY_REP_CONTACT ON OOCKE1_ACTIVITY(rep_contact) ;
CREATE INDEX I_OOCKE1_ACTIVITY_REP_ACCT ON OOCKE1_ACTIVITY(rep_acct) ;
CREATE INDEX I_OOCKE1_ACTIVITY_PERC_COMPL ON OOCKE1_ACTIVITY(percent_complete) ;
CREATE INDEX I_OOCKE1_ACTIVITY_ACTIVITYNUM ON OOCKE1_ACTIVITY(activity_number) ;
CREATE INDEX I_OOCKE1_ACTIVITY_MOD_AT ON OOCKE1_ACTIVITY(modified_at) ;
CREATE INDEX I_OOCKE1_ACTIVITY_SCHEDSTART ON OOCKE1_ACTIVITY(scheduled_start) ;
CREATE INDEX I_OOCKE1_ACTIVITY_SCHEDEND ON OOCKE1_ACTIVITY(scheduled_end) ;
CREATE INDEX I_OOCKE1_ACTIVITY_CREATED_AT ON OOCKE1_ACTIVITY(created_at) ;
CREATE INDEX I_OOCKE1_ACTIVITY_DUE_BY ON OOCKE1_ACTIVITY(due_by) ;
CREATE INDEX I_OOCKE1_ACTIVITY_SENDER ON OOCKE1_ACTIVITY(sender) ;
CREATE INDEX I_OOCKE1_ACTIVITY_EXTERNAL_LNK ON OOCKE1_ACTIVITY_(external_link) ;
CREATE INDEX I_OOCKE1_ACTIVITY__CONTRACT ON OOCKE1_ACTIVITY_(contract) ;

CREATE INDEX I_OOCKE1_ACCOUNTASS_PARENT ON OOCKE1_ACCOUNTASSIGNMENT("P$$PARENT") ;
CREATE INDEX I_OOCKE1_ACCOUNTASS_ACCOUNT ON OOCKE1_ACCOUNTASSIGNMENT(account) ;

CREATE INDEX I_OOCKE1_ACTFLOLLOWUP_PARENT ON OOCKE1_ACTIVITYFOLLOWUP("P$$PARENT") ;
CREATE INDEX I_OOCKE1_ACTFOLLOWUP_MOD_AT ON OOCKE1_ACTIVITYFOLLOWUP(modified_at) ;
CREATE INDEX I_OOCKE1_ACTFOLLOWUP_CREAT_AT ON OOCKE1_ACTIVITYFOLLOWUP(created_at) ;

CREATE INDEX I_OOCKE1_ACTIVITYPARTY_PARENT ON OOCKE1_ACTIVITYPARTY("P$$PARENT") ;
CREATE INDEX I_OOCKE1_ACTIVITYPARTY_PARTY ON OOCKE1_ACTIVITYPARTY(party) ;

CREATE INDEX I_OOCKE1_ACTGROUPASS_PARENT ON OOCKE1_ACTIVITYGROUPASS("P$$PARENT") ;
CREATE INDEX I_OOCKE1_ACTGROUPASS_GROUP ON OOCKE1_ACTIVITYGROUPASS(activity_group) ;

CREATE INDEX I_OOCKE1_AUDITENTRY_CREATEDAT ON OOCKE1_AUDITENTRY(created_at) ;

CREATE INDEX I_OOCKE1_BOOKING_CB ON OOCKE1_BOOKING(cb) ;
CREATE INDEX I_OOCKE1_BOOKING_BOOKING_DATE ON OOCKE1_BOOKING(booking_date) ;
CREATE INDEX I_OOCKE1_BOOKING_BOOKING_STATUS ON OOCKE1_BOOKING(booking_status) ;
CREATE INDEX I_OOCKE1_BOOKING_BOOKING_TYPE ON OOCKE1_BOOKING(booking_type) ;
CREATE INDEX I_OOCKE1_BOOKING_ORIGIN ON OOCKE1_BOOKING(origin) ;
CREATE INDEX I_OOCKE1_BOOKING_POSITION ON OOCKE1_BOOKING(position) ;
CREATE INDEX I_OOCKE1_BOOKING_VALUE_DATE ON OOCKE1_BOOKING(value_date) ;

CREATE INDEX I_OOCKE1_BOOKINGTEMPLATE_PARENT ON OOCKE1_BOOKINGTEMPLATE("P$$PARENT") ;

CREATE INDEX I_OOCKE1_COMPOUNDBOOKING_NAME ON OOCKE1_COMPOUNDBOOKING(name) ;
CREATE INDEX I_OOCKE1_COMPOUNDBOOKING_ORIGIN ON OOCKE1_COMPOUNDBOOKING(origin) ;

CREATE INDEX I_OOCKE1_CONTRACT_SALES_REP ON OOCKE1_CONTRACT(sales_rep) ;
CREATE INDEX I_OOCKE1_CONTRACT_CUSTOMER ON OOCKE1_CONTRACT(customer) ;
CREATE INDEX I_OOCKE1_CONTRACT_ACTIVE_ON ON OOCKE1_CONTRACT(active_on) ;
CREATE INDEX I_OOCKE1_CONTRACT_EXPIRES_ON ON OOCKE1_CONTRACT(expires_on) ;
CREATE INDEX I_OOCKE1_CONTRACT_MOD_AT ON OOCKE1_CONTRACT(modified_at) ;
CREATE INDEX I_OOCKE1_CONTRACT_CREATEDAT ON OOCKE1_CONTRACT(created_at) ;

CREATE INDEX I_OOCKE1_CONTRACTPOS_PRODUCT ON OOCKE1_CONTRACTPOSITION(product) ;

CREATE INDEX I_OOCKE1_DEPOT_PARENT ON OOCKE1_DEPOT("P$$PARENT") ;
CREATE INDEX I_OOCKE1_DEPOT_NAME ON OOCKE1_DEPOT(name) ;
CREATE INDEX I_OOCKE1_DEPOT_DEPOT_GROUP ON OOCKE1_DEPOT(depot_group) ;
CREATE INDEX I_OOCKE1_DEPOT_DEPOT_NUMBER ON OOCKE1_DEPOT(depot_number) ;

CREATE INDEX I_OOCKE1_DEPOTGROUP_PARENT ON OOCKE1_DEPOTGROUP("P$$PARENT") ;
CREATE INDEX I_OOCKE1_DEPOTGROUP_NAME ON OOCKE1_DEPOTGROUP(name) ;

CREATE INDEX I_OOCKE1_DEPOTHOLDER_PARENT ON OOCKE1_DEPOTHOLDER("P$$PARENT") ;

CREATE INDEX I_OOCKE1_DEPOTREPORT_PARENT ON OOCKE1_DEPOTREPORT("P$$PARENT") ;
CREATE INDEX I_OOCKE1_DEPOTREPORT_NAME ON OOCKE1_DEPOTREPORT(name) ;
CREATE INDEX I_OOCKE1_DEPOTREPORT_BOOKING_PERIOD ON OOCKE1_DEPOTREPORT(booking_period) ;

CREATE INDEX I_OOCKE1_DEPOTPOSITION_NAME ON OOCKE1_DEPOTPOSITION(name) ;
CREATE INDEX I_OOCKE1_DEPOTPOSITION_PARENT ON OOCKE1_DEPOTPOSITION("P$$PARENT") ;
CREATE INDEX I_OOCKE1_DEPOTPOSITION_PRODUCT ON OOCKE1_DEPOTPOSITION(product) ;

CREATE INDEX I_OOCKE1_DEPOTREPORTITEM_PARENT ON OOCKE1_DEPOTREPORTITEM("P$$PARENT") ;
CREATE INDEX I_OOCKE1_DEPOTREPORTITEM_POSITION_NAME ON OOCKE1_DEPOTREPORTITEM(position_name) ;
CREATE INDEX I_OOCKE1_DEPOTREPORTITEM_POSITION ON OOCKE1_DEPOTREPORTITEM(position) ;
CREATE INDEX I_OOCKE1_DEPOTREPORTITEM_VALUE_DATE ON OOCKE1_DEPOTREPORTITEM(value_date) ;

CREATE INDEX I_OOCKE1_DOCUMENT_MOD_AT ON OOCKE1_DOCUMENT(modified_at) ;
CREATE INDEX I_OOCKE1_DOCUMENT_FOLDER ON OOCKE1_DOCUMENT_(folder);

CREATE INDEX I_OOCKE1_MEDIA_MOD_AT ON OOCKE1_MEDIA(modified_at) ;

CREATE INDEX I_OOCKE1_NOTE_PARENT ON OOCKE1_NOTE("P$$PARENT") ;

CREATE INDEX I_OOCKE1_USERHOME_CONTACT ON OOCKE1_USERHOME(contact) ;

CREATE INDEX I_OOCKE1_WFPROCINST_STARTED_ON ON OOCKE1_WFPROCESSINSTANCE(started_on) ;

CREATE INDEX I_OOCKE1_AUDITENTRY_VISITED_BY ON OOCKE1_AUDITENTRY_(visited_by) ;

CREATE INDEX I_OOCKE1_PRODUCT_PARENT ON OOCKE1_PRODUCT("P$$PARENT") ;
CREATE INDEX I_OOCKE1_PRODUCT_PRICE_UOM ON OOCKE1_PRODUCT_(price_uom) ;

CREATE INDEX I_OOCKE1_PRODBPR_PARENT ON OOCKE1_PRODUCTBASEPRICE("P$$PARENT") ;
CREATE INDEX I_OOCKE1_PRODBPR_PRICE_LEVEL ON OOCKE1_PRODUCTBASEPRICE_(price_level) ;

CREATE INDEX I_OOCKE1_RESASS_PARENT ON OOCKE1_RESOURCEASSIGNMENT("P$$PARENT") ;

CREATE INDEX I_OOCKE1_WORKRECORD_PARENT ON OOCKE1_WORKRECORD("P$$PARENT") ;
CREATE INDEX I_OOCKE1_WORKRECORD_STARTED_AT ON OOCKE1_WORKRECORD(started_at) ;
CREATE INDEX I_OOCKE1_WORKRECORD_MOD_AT ON OOCKE1_WORKRECORD(modified_at) ;

CREATE INDEX I_OOCKE1_INDEXENTRY_IOBJ_CAT ON OOCKE1_INDEXENTRY(indexed_object, created_at);

CREATE INDEX I_OOMSE2_PRINCIPAL ON OOMSE2_PRINCIPAL(name);
