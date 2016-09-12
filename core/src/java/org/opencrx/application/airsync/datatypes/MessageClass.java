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

public enum MessageClass {
	Note, NoteRulesOofTemplateMicrosoft, NoteSMIME, NoteSMIMEMultipartSigned, ScheduleMeetingRequest, ScheduleMeetingCanceled, ScheduleMeetingRespPos, ScheduleMeetingRespTent, ScheduleMeetingRespNeg, Post;

	/*
	 * (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 */
	public String toString() {
		switch (this) {
		case Note:
			return "IPM.Note";
		case NoteRulesOofTemplateMicrosoft:
			return "IPM.Note.Rules.OofTemplate.Microsoft";
		case NoteSMIME:
			return "IPM.Note.SMIME";
		case NoteSMIMEMultipartSigned:
			return "IPM.Note.SMIME.MultipartSigned";
		case ScheduleMeetingRequest:
			return "IPM.Schedule.Meeting.Request";
		case ScheduleMeetingCanceled:
			return "IPM.Schedule.Meeting.Canceled";
		case ScheduleMeetingRespPos:
			return "IPM.Schedule.Meeting.Resp.Pos";
		case ScheduleMeetingRespTent:
			return "IPM.Schedule.Meeting.Resp.Tent";
		case ScheduleMeetingRespNeg:
			return "IPM.Schedule.Meeting.Resp.Neg";
		case Post:
			return "IPM.Post";
		default:
			return "IPM.Note";
		}
	}

}
