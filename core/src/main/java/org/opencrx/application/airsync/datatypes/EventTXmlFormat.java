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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.opencrx.application.airsync.utils.DOMUtils;
import org.opencrx.kernel.utils.Utils;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.format.DateTimeFormat;

public class EventTXmlFormat extends AbstractXmlFormat {

	public EventTXmlFormat() {
	}

	@Override
	public void format(
		Element eData, 
		IData data,
		double protocolVersion
	) {
		// According to [MS-ASDTYPE] 
		// Dates and times in calendar items MUST NOT include punctuation separators. 
		// For example: <A:StartTime>20021126T160000Z</A:StartTime>
		DateTimeFormat utcf = this.getUtcFormat(false);
		EventT eventT = (EventT) data;
		
		createElement(eData, "Calendar:", "Timezone", eventT.getTimezone());
		if(eventT.getAllDayEvent() != null) {
			createElement(eData, "Calendar:", "AllDayEvent", (eventT.getAllDayEvent() ? "1" : "0"));
		} else {
			createElement(eData, "Calendar:", "AllDayEvent", "0");
		}
		String body = eventT.getBody();
		if(body != null) {
			createElement(eData, "Calendar:", "Body", Utils.normalizeNewLines(body).replace("\n", "\r\n"));
		}
		createElement(eData, "Calendar:", "BusyStatus", Integer.toString(eventT.getBusyStatus().getValue()));
		createElement(eData, "Calendar:", "Organizer_Name", eventT.getOrganizerName());
		createElement(eData, "Calendar:", "Organizer_Email", eventT.getOrganizerEmail());
		createElement(eData, "Calendar:", "DtStamp", utcf.format(eventT.getDtStamp() != null ? eventT.getDtStamp() : new Date()));
		if(eventT.getEndTime() != null) {
			createElement(eData, "Calendar:", "EndTime", utcf.format(eventT.getEndTime()));
		}
		createElement(eData, "Calendar:", "Location", eventT.getLocation());
		if(eventT.getReminder() != null) {
			createElement(eData, "Calendar:", "Reminder_MinsBefore", Integer.toString(eventT.getReminder()));
		}
		createElement(eData, "Calendar:", "Sensitivity", "0");
		createElement(eData, "Calendar:", "Subject", eventT.getSubject());
		if(eventT.getStartTime() != null) {
			createElement(eData, "Calendar:", "StartTime", utcf.format(eventT.getStartTime()));
		}
		createElement(eData, "Calendar:", "UID", eventT.getUID());
		if(eventT.getMeetingStatus() != null) {
			createElement(eData, "Calendar:", "MeetingStatus", Integer.toString(eventT.getMeetingStatus().getValue()));
		}
		if(eventT.getAttendees() != null && !eventT.getAttendees().isEmpty()) {
			Element eAttendees = DOMUtils.createElement(eData, "Calendar:", "Attendees");
			for (AttendeeT attendeeT : eventT.getAttendees()) {
				Element eAttendee = DOMUtils.createElement(eAttendees, "Calendar:", "Attendee");
				createElement(eAttendee, "Calendar:", "Attendee_Email", attendeeT.getEmail());
				createElement(eAttendee, "Calendar:", "Attendee_Name", attendeeT.getName());
				if(attendeeT.getAttendeeStatus() != null) {
					createElement(eAttendee, "Calendar:", "Attendee_Status", Integer.toString(attendeeT.getAttendeeStatus().getValue()));
				}
				if(attendeeT.getAttendeeType() != null) {
					createElement(eAttendee, "Calendar:", "Attendee_Type", Integer.toString(attendeeT.getAttendeeType().getValue()));
				}
			}
		}
		if(eventT.getCategories() != null && !eventT.getCategories().isEmpty()) {
			Element eCategories = DOMUtils.createElement(eData, "Calendar:", "Categories");
			for(String category: eventT.getCategories()) {
				this.createElement(eCategories, "Calendar:", "Category", category);
			}
		}
		if(eventT.getRecurrence() != null) {
			this.formatRecurrence(eData, eventT, protocolVersion);
		}
		if(eventT.getExceptions() != null && !eventT.getExceptions().isEmpty()) {
			this.formatExceptions(eData, eventT.getExceptions(), protocolVersion);
		}
	}

	private void formatExceptions(
		Element eData, 
		List<EventT> exceptionsT,
		double protocolVersion
	) {
		DateTimeFormat utcf = this.getUtcFormat(false);
		Element es = DOMUtils.createElement(eData, "Calendar:", "Exceptions");
		for(EventT exceptionT: exceptionsT) {
			Element eException = DOMUtils.createElement(es, "Calendar:", "Exception");
			if(exceptionT.isDeletedException()) {
				DOMUtils.createElementAndText(eException, "Calendar:", "Deleted", "1");
				DOMUtils.createElementAndText(eException, "Calendar:", "MeetingStatus", Integer.toString(MeetingStatus.MEETING_IS_CANCELED.getValue()));
			}
			else {
				if(exceptionT.getExceptionStartTime() != null) {
					DOMUtils.createElementAndText(eException, "Calendar:", "Exception_StartTime", utcf.format(exceptionT.getExceptionStartTime()));
				}
				createElement(eException, "Calendar:", "Subject", exceptionT.getSubject());
				if(exceptionT.getStartTime() != null) {
					DOMUtils.createElementAndText(eException, "Calendar:", "StartTime", utcf.format(exceptionT.getStartTime()));
				}
				if(exceptionT.getEndTime() != null) {
					DOMUtils.createElementAndText(eException, "Calendar:", "EndTime", utcf.format(exceptionT.getEndTime()));
				}
				createElement(eException, "Calendar:", "Body", exceptionT.getBody());					
				createElement(eException, "Calendar:", "Location", exceptionT.getLocation());
				if(exceptionT.getCategories() != null && !exceptionT.getCategories().isEmpty()) {
					Element eCategories = DOMUtils.createElement(eException, "Calendar:", "Categories");
					for(String category: exceptionT.getCategories()) {
						this.createElement(eCategories, "Calendar:", "Category", category);
					}
				}
				if(exceptionT.getSensitivity() != null) {
					createElement(eException, "Calendar:", "Sensitivity", Integer.toString(exceptionT.getSensitivity().getValue()));
				}
				if(exceptionT.getBusyStatus() != null) {
					createElement(eException, "Calendar:", "BusyStatus", Integer.toString(exceptionT.getBusyStatus().getValue()));
				}
				createElement(eException, "Calendar:", "AllDayEvent", (Boolean.TRUE.equals(exceptionT.getAllDayEvent()) ? "1" : "0"));
				if(exceptionT.getReminder() != null) {
					createElement(eException, "Calendar:", "Reminder", Integer.toString(exceptionT.getReminder()));
				}
				if(exceptionT.getMeetingStatus() != null) {
					createElement(eException, "Calendar:", "MeetingStatus", Integer.toString(exceptionT.getMeetingStatus().getValue()));
				}
			}
			if(exceptionT.getDtStamp() != null) {
				DOMUtils.createElementAndText(eException, "Calendar:", "DtStamp", utcf.format(exceptionT.getDtStamp()));
			}
		}
	}

	private void formatRecurrence(
		Element eData, 
		EventT ev,
		double protocolVersion
	) {
		DateTimeFormat utcf = this.getUtcFormat(false);
		RecurrenceT recurrenceT = ev.getRecurrence();
		if(recurrenceT == null || recurrenceT.getType() == null) return;
		Element eRecurrence = DOMUtils.createElement(eData, "Calendar:", "Recurrence");
		DOMUtils.createElementAndText(eRecurrence, "Calendar:", "Recurrence_Type", Integer.toString(recurrenceT.getType().getValue()));
		if(recurrenceT.getOccurrences() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Calendar:", "Recurrence_Occurrences", Integer.toString(recurrenceT.getOccurrences()));
		}
		if(recurrenceT.getInterval() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Calendar:", "Recurrence_Interval", recurrenceT.getInterval().toString());
		}
		if(recurrenceT.getWeekOfMonth() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Calendar:", "Recurrence_WeekOfMonth", Integer.toString(recurrenceT.getWeekOfMonth()));			
		}
		if(recurrenceT.getDayOfWeek() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Calendar:", "Recurrence_DayOfWeek", Integer.toString(RecurrenceDayOfWeek.asInt(recurrenceT.getDayOfWeek())));
		}
		if(recurrenceT.getMonthOfYear() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Calendar:", "Recurrence_MonthOfYear", Integer.toString(recurrenceT.getMonthOfYear()));			
		}
		if(recurrenceT.getUntil() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Calendar:", "Recurrence_Until", utcf.format(recurrenceT.getUntil()));
		}
	}

	@Override
	public IData parse(
		Element eData
	) {
		EventT eventT = new EventT();

		eventT.setTimezone(parseDOMString(DOMUtils.getUniqueElement(eData, "Calendar:", "Timezone")));
		eventT.setOrganizerName(parseDOMString(DOMUtils.getUniqueElement(eData, "Calendar:", "Organizer_Name")));
		eventT.setOrganizerEmail(parseDOMString(DOMUtils.getUniqueElement(eData, "Calendar:", "Organizer_Email")));
		eventT.setUID(parseDOMString(DOMUtils.getUniqueElement(eData, "Calendar:", "UID")));
		this.parseEvent(eventT, eData);
		// Attendees
		Element eAttendees = DOMUtils.getUniqueElement(eData, "Calendar:", "Attendees");
		if(eAttendees != null) {
			List<AttendeeT> attendeesT = new ArrayList<AttendeeT>();
			for (int i = 0, n = eAttendees.getChildNodes().getLength(); i < n; i += 1) {
				Node node = eAttendees.getChildNodes().item(i);
				if(node instanceof Element) {
					Element eAttendee = (Element)node;
					AttendeeT attendeeT = new AttendeeT();
					attendeeT.setEmail(parseDOMString(DOMUtils.getUniqueElement(eAttendee, "Calendar:", "Attendee_Email")));
					attendeeT.setName(parseDOMString(DOMUtils.getUniqueElement(eAttendee, "Calendar:", "Attendee_Name")));
					Element attendeeStatus = DOMUtils.getUniqueElement(eData, "Calendar:", "Attendee_Status");					
					attendeeT.setAttendeeStatus(
						attendeeStatus == null ? null : AttendeeStatus.toAttendeeStatus(parseDOMNoNullInt(attendeeStatus))
					);
					attendeeT.setAttendeeType(
						AttendeeType.toAttendeeType(parseDOMNoNullInt(DOMUtils.getUniqueElement(eData, "Calendar:", "Attendee_Type")))
					);
					attendeesT.add(attendeeT);
				}
			}
			eventT.setAttendees(attendeesT);
		}
		// Exceptions
		Element eExceptions = DOMUtils.getUniqueElement(eData, "Calendar:", "Exceptions");
		if(eExceptions != null) {
			List<EventT> exceptions = new ArrayList<EventT>();
			for(int i = 0, n = eExceptions.getChildNodes().getLength(); i < n; i += 1) {
				Node node = eExceptions.getChildNodes().item(i);
				if(node instanceof Element) {
					Element eException = (Element)node;
					EventT exceptionT = new EventT();
					this.parseEvent(exceptionT, eException);
					exceptionT.setDeleted(parseDOMInt2Boolean(DOMUtils.getUniqueElement(eException, "Calendar:", "Deleted")));
					exceptionT.setExceptionStartTime(parseDOMDate(DOMUtils.getUniqueElement(eException, "Calendar:", "Exception_StartTime")));
					exceptionT.setMeetingStatus(
						MeetingStatus.toMeetingStatus(parseDOMNoNullInt(DOMUtils.getUniqueElement(eException, "Calendar:", "MeetingStatus")))
					);
					exceptionT.setSensitivity(
						Sensitivity.toSensitivity(parseDOMNoNullInt(DOMUtils.getUniqueElement(eException, "Calendar:", "Sensitivity")))
					);
					exceptionT.setBusyStatus(
						BusyStatus.toBusyStatus(parseDOMNoNullInt(DOMUtils.getUniqueElement(eException, "Calendar:", "BusyStatus")))
					);
					exceptions.add(exceptionT);
				}
			}
			eventT.setExceptions(exceptions);
		}
		// Recurrence
		Element eRecurrence = DOMUtils.getUniqueElement(eData, "Calendar:", "Recurrence");
		if(eRecurrence != null) {
			RecurrenceT recurrenceT = new RecurrenceT();
			recurrenceT.setUntil(parseDOMDate(DOMUtils.getUniqueElement(eRecurrence, "Calendar:", "Recurrence_Until")));
			recurrenceT.setWeekOfMonth(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Calendar:", "Recurrence_WeekOfMonth")));
			recurrenceT.setMonthOfYear(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Calendar:", "Recurrence_MonthOfYear")));
			recurrenceT.setOccurrences(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Calendar:", "Recurrence_Occurrences")));
			recurrenceT.setInterval(parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Calendar:", "Recurrence_Interval")));
			Integer dayOfWeek = parseDOMInt(DOMUtils.getUniqueElement(eRecurrence, "Calendar:", "Recurrence_DayOfWeek"));
			if(dayOfWeek != null) {
				recurrenceT.setDayOfWeek(RecurrenceDayOfWeek.fromInt(dayOfWeek));
			}
			recurrenceT.setType(
				RecurrenceType.toRecurrenceType(parseDOMNoNullInt(DOMUtils.getUniqueElement(eRecurrence, "Calendar:", "Recurrence_Type")))
			);
			eventT.setRecurrence(recurrenceT);
		}
		return eventT;
	}

	void parseEvent(
		EventT eventT, 
		Element eEvent
	) {
		eventT.setBody(parseDOMString(DOMUtils.getUniqueElement(eEvent, "Calendar:", "Body")));
		eventT.setAllDayEvent(parseDOMInt2Boolean(DOMUtils.getUniqueElement(eEvent, "Calendar:", "AllDayEvent")));
		eventT.setBusyStatus(
			BusyStatus.toBusyStatus(parseDOMNoNullInt(DOMUtils.getUniqueElement(eEvent, "Calendar:", "BusyStatus")))
		);
		eventT.setDtStamp(parseDOMDate(DOMUtils.getUniqueElement(eEvent, "Calendar:", "DtStamp")));
		eventT.setEndTime(parseDOMDate(DOMUtils.getUniqueElement(eEvent, "Calendar:", "EndTime")));
		eventT.setLocation(parseDOMString(DOMUtils.getUniqueElement(eEvent, "Calendar:", "Location")));
		eventT.setReminder(parseDOMInt(DOMUtils.getUniqueElement(eEvent, "Calendar:", "Reminder_MinsBefore")));
		eventT.setSensitivity(
			Sensitivity.toSensitivity(parseDOMNoNullInt(DOMUtils.getUniqueElement(eEvent, "Calendar:", "Sensitivity")))
		);
		eventT.setSubject(parseDOMString(DOMUtils.getUniqueElement(eEvent, "Calendar:", "Subject")));
		eventT.setStartTime(parseDOMDate(DOMUtils.getUniqueElement(eEvent, "Calendar:", "StartTime")));
		eventT.setMeetingStatus(
			MeetingStatus.toMeetingStatus(parseDOMNoNullInt(DOMUtils.getUniqueElement(eEvent, "Calendar:", "MeetingStatus")))
		);
		eventT.setCategories(parseDOMStringCollection(DOMUtils.getUniqueElement(eEvent, "Calendar:", "Categories"), "Calendar:", "Category"));
	}

}
