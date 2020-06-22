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

import org.opencrx.application.airsync.utils.DOMUtils;
import org.opencrx.kernel.utils.Utils;
import org.w3c.dom.Element;
import org.w3c.format.DateTimeFormat;

public class TaskTXmlFormat extends AbstractXmlFormat {

	@Override
	public void format(
		Element eData, 
		IData data,
		double protocolVersion
	) {
		DateTimeFormat eutcf = this.getUtcFormat(true);
		TaskT taskT = (TaskT) data;
		
		String body = taskT.getBody();
		if(body != null) {
			createElement(eData, "Tasks:", "Body", Utils.normalizeNewLines(body).replace("\n", "\r\n"));
		}
		createElement(eData, "Tasks:", "Subject", taskT.getSubject());
		createElement(eData, "Tasks:", "Importance", Integer.toString(taskT.getImportance().getValue()));
		if(taskT.getUtcstartdate() != null) {
			createElement(eData, "Tasks:", "UTCStartDate", eutcf.format(taskT.getUtcstartdate()));
		}
		if(taskT.getStartdate() != null) {
			createElement(eData, "Tasks:", "StartDate", eutcf.format(taskT.getStartdate()));
		}
		if(taskT.getUtcduedate() != null) {
			createElement(eData, "Tasks:", "UTCDueDate", eutcf.format(taskT.getUtcduedate()));
		}
		if(taskT.getDuedate() != null) {
			createElement(eData, "Tasks:", "DueDate", eutcf.format(taskT.getDuedate()));
		}
		if(taskT.getCategories() != null && !taskT.getCategories().isEmpty()) {
			Element eCategories = DOMUtils.createElement(eData, "Tasks:", "Categories");
			for(String category: taskT.getCategories()) {
				this.createElement(eCategories, "Tasks:", "Category", category);
			}
		}
		if(taskT.getRecurrence() != null) {
			this.formatRecurrence(eData, taskT, protocolVersion);
		}
		createElement(eData, "Tasks:", "Complete", Boolean.TRUE.equals(taskT.getComplete()) ? "1" : "0");
		if(taskT.getDatecompleted() != null) {
			createElement(eData, "Tasks:", "DateCompleted", eutcf.format(taskT.getDatecompleted()));
		}
		if(taskT.getSensitivity() != null) {
			createElement(eData, "Tasks:", "Sensitivity", Integer.toString(taskT.getSensitivity().getValue()));
		}
	}

	private void formatRecurrence(
		Element eData, 
		TaskT ev,
		double protocolVersion
	) {
		DateTimeFormat eutcf = this.getUtcFormat(true);
		RecurrenceT recurrenceT = ev.getRecurrence();
		if(recurrenceT == null || recurrenceT.getType() == null) return;
		Element eRecurrence = DOMUtils.createElement(eData, "Tasks:", "Recurrence");
		DOMUtils.createElementAndText(eRecurrence, "Tasks:", "Recurrence_Type", Integer.toString(recurrenceT.getType().getValue()));
		if(recurrenceT.getOccurrences() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Tasks:", "Recurrence_Occurrences", Integer.toString(recurrenceT.getOccurrences()));
		}
		if(recurrenceT.getInterval() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Tasks:", "Recurrence_Interval", recurrenceT.getInterval().toString());
		}
		if(recurrenceT.getWeekOfMonth() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Tasks:", "Recurrence_WeekOfMonth", Integer.toString(recurrenceT.getWeekOfMonth()));			
		}
		if(recurrenceT.getDayOfWeek() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Tasks:", "Recurrence_DayOfWeek", Integer.toString(RecurrenceDayOfWeek.asInt(recurrenceT.getDayOfWeek())));
		}
		if(recurrenceT.getMonthOfYear() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Tasks:", "Recurrence_MonthOfYear", Integer.toString(recurrenceT.getMonthOfYear()));			
		}
		if(recurrenceT.getUntil() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Tasks:", "Recurrence_Until", eutcf.format(recurrenceT.getUntil()));
		}
	}

	private void parseBody(
		Element eData,
		TaskT taskT
	) {		
		Element eBody = DOMUtils.getUniqueElement(eData, "AirSyncBase:", "Body");
		if(eBody != null) {
			taskT.setBody(parseDOMString(DOMUtils.getUniqueElement(eBody, "AirSyncBase:", "Data")));
		}
	}
	
	@Override
	public IData parse(
		Element eData
	) {
		TaskT taskT = new TaskT();

		this.parseBody(eData, taskT);
		taskT.setSubject(parseDOMString(DOMUtils.getUniqueElement(eData, "Tasks:", "Subject")));
		taskT.setImportance(Importance.toImportance(parseDOMNoNullInt(DOMUtils.getUniqueElement(eData, "Tasks:", "Importance"))));
		taskT.setUtcstartdate(parseDOMDate(DOMUtils.getUniqueElement(eData, "Tasks:", "UTCStartDate")));
		taskT.setStartdate(parseDOMDate(DOMUtils.getUniqueElement(eData, "Tasks:", "StartDate")));
		taskT.setUtcduedate(parseDOMDate(DOMUtils.getUniqueElement(eData, "Tasks:", "UTCDueDate")));
		taskT.setDuedate(parseDOMDate(DOMUtils.getUniqueElement(eData, "Tasks:", "DueDate")));
		taskT.setCategories(parseDOMStringCollection(DOMUtils.getUniqueElement(eData, "Tasks:", "Categories"), "Tasks:", "Category"));
		// Recurrence
		Element eRecurrence = DOMUtils.getUniqueElement(eData, "Tasks:", "Recurrence");
		if(eRecurrence != null) {
			RecurrenceT recurrenceT = new RecurrenceT();
			recurrenceT.setUntil(parseDOMDate(DOMUtils.getUniqueElement(eRecurrence, "Tasks:", "Recurrence_Until")));
			recurrenceT.setWeekOfMonth(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Tasks:", "Recurrence_WeekOfMonth")));
			recurrenceT.setMonthOfYear(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Tasks:", "Recurrence_MonthOfYear")));
			recurrenceT.setOccurrences(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Tasks:", "Recurrence_Occurrences")));
			recurrenceT.setInterval(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Tasks:", "Recurrence_Interval")));
			Integer dayOfWeek = parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Tasks:", "Recurrence_DayOfWeek"));
			if(dayOfWeek != null) {
				recurrenceT.setDayOfWeek(RecurrenceDayOfWeek.fromInt(dayOfWeek));
			}
			recurrenceT.setType(
				RecurrenceType.toRecurrenceType(parseDOMNoNullInt(DOMUtils.getUniqueElement(eRecurrence, "Tasks:", "Recurrence_Type")))
			);
			taskT.setRecurrence(recurrenceT);
		}
		taskT.setComplete("1".equals(parseDOMString(DOMUtils.getUniqueElement(eData, "Tasks:", "Complete"))));
		taskT.setDatecompleted(parseDOMDate(DOMUtils.getUniqueElement(eData, "Tasks:", "DateCompleted")));
		taskT.setSensitivity(
			Sensitivity.toSensitivity(parseDOMNoNullInt(DOMUtils.getUniqueElement(eData, "Tasks:", "Sensitivity")))
		);
		return taskT;
	}

}
