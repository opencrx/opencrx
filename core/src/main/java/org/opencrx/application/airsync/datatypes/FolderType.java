/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: Sync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.datatypes;

public enum FolderType {

	USER_FOLDER_GENERIC(1),
	DEFAULT_INBOX_FOLDER(2),
	DEFAULT_DRAFTS_FOLDERS(3),
	DEFAULT_DELETED_ITEMS_FOLDERS(4),
	DEFAULT_SENT_EMAIL_FOLDER(5),
	DEFAULT_OUTBOX_FOLDER(6),
	DEFAULT_TASKS_FOLDER(7),
	DEFAULT_CALENDAR_FOLDER(8),
	DEFAULT_CONTACTS_FOLDER(9),
	DEFAULT_NOTES_FOLDER(10),
	DEFAULT_JOURNAL_FOLDER(11),
	USER_CREATED_EMAIL_FOLDER(12),
	USER_CREATED_CALENDAR_FOLDER(13),
	USER_CREATED_CONTACTS_FOLDER(14),
	USER_CREATED_TASKS_FOLDER(15),
	USER_CREATED_JOURNAL_FOLDER(16),
	USER_CREATED_NOTES_FOLDER(17),
	UNKNOWN_FOLDER_TYPE(18);

	private final int value;
	
	private FolderType(
		int value
	) {
		this.value = value;
	}
	
	public int getValue(
	) {
		return this.value;
	}
	
	public static FolderType toFolderType(
		int value
	) {
		switch(value) {
			case 1: return USER_FOLDER_GENERIC;
			case 2: return DEFAULT_INBOX_FOLDER;
			case 3: return DEFAULT_DRAFTS_FOLDERS;
			case 4: return DEFAULT_DELETED_ITEMS_FOLDERS;
			case 5: return DEFAULT_SENT_EMAIL_FOLDER;
			case 6: return DEFAULT_OUTBOX_FOLDER;
			case 7: return DEFAULT_TASKS_FOLDER;
			case 8: return DEFAULT_CALENDAR_FOLDER;
			case 9: return DEFAULT_CONTACTS_FOLDER;
			case 10: return DEFAULT_NOTES_FOLDER;
			case 11: return DEFAULT_JOURNAL_FOLDER;
			case 12: return USER_CREATED_EMAIL_FOLDER;
			case 13: return USER_CREATED_CALENDAR_FOLDER;
			case 14: return USER_CREATED_CONTACTS_FOLDER;
			case 15: return USER_CREATED_TASKS_FOLDER;
			case 16: return USER_CREATED_JOURNAL_FOLDER;
			case 17: return USER_CREATED_NOTES_FOLDER;
			case 18: return UNKNOWN_FOLDER_TYPE;
			default: return USER_FOLDER_GENERIC;
		}
	}
}
