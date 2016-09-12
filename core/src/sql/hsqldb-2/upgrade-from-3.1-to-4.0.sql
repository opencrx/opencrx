/* This software is published under the BSD license                          */
/* as listed below.                                                          */
/*                                                                           */
/* Copyright (c) 2004-2016, CRIXP Corp., Switzerland                         */
/* All rights reserved.                                                      */
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
/* * Neither the name of CRIXP Corp. nor the names of the contributors       */
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

ALTER TABLE OOCKE1_COMPOUNDBOOKING ADD COLUMN ORIGIN VARCHAR(256);

CREATE TABLE OOCKE1_WFPROCESSINSTANCEPARAM (OBJECT_ID VARCHAR(250) NOT NULL, ACCESS_LEVEL_BROWSE SMALLINT, ACCESS_LEVEL_DELETE SMALLINT, ACCESS_LEVEL_UPDATE SMALLINT, CREATED_AT TIMESTAMP, CREATED_BY_ INTEGER DEFAULT -1, MODIFIED_BY_ INTEGER DEFAULT -1, NAME VARCHAR(256), OWNER_ INTEGER DEFAULT -1, "P$$PARENT" VARCHAR(256), DTYPE VARCHAR(256), MODIFIED_AT TIMESTAMP, INTEGER_VALUE INTEGER, DATE_TIME_VALUE TIMESTAMP, STRING_VALUE VARCHAR(256), DATE_VALUE DATE, BOOLEAN_VALUE BOOLEAN, DECIMAL_VALUE NUMERIC(19,9), REFERENCE_VALUE VARCHAR(256), URI_VALUE VARCHAR(256), PRIMARY KEY (OBJECT_ID));
CREATE TABLE OOCKE1_WFPROCESSINSTANCEPARAM_ (OBJECT_ID VARCHAR(250) NOT NULL, IDX INTEGER NOT NULL, CREATED_BY VARCHAR(256), MODIFIED_BY VARCHAR(256), OWNER VARCHAR(256), DTYPE VARCHAR(256), PRIMARY KEY (OBJECT_ID, IDX));

ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_BOOLEAN0 BOOLEAN;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_BOOLEAN1 BOOLEAN;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_BOOLEAN2 BOOLEAN;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_BOOLEAN3 BOOLEAN;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_BOOLEAN4 BOOLEAN;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_CODE0 SMALLINT;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_CODE1 SMALLINT;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_CODE2 SMALLINT;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_CODE3 SMALLINT;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_CODE4 SMALLINT;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE0 DATE;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE1 DATE;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE2 DATE;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE3 DATE;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE4 DATE;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE_TIME0 TIMESTAMP;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE_TIME1 TIMESTAMP;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE_TIME2 TIMESTAMP;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE_TIME3 TIMESTAMP;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_DATE_TIME4 TIMESTAMP;
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_NUMBER0 NUMERIC(19,9);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_NUMBER1 NUMERIC(19,9);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_NUMBER2 NUMERIC(19,9);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_NUMBER3 NUMERIC(19,9);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_NUMBER4 NUMERIC(19,9);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_STRING0 VARCHAR(256);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_STRING1 VARCHAR(256);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_STRING2 VARCHAR(256);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_STRING3 VARCHAR(256);
ALTER TABLE OOCKE1_PRODUCT ADD COLUMN EXT_STRING4 VARCHAR(256);

