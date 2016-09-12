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
import java.util.Collections;
import java.util.List;

import org.opencrx.application.airsync.utils.DOMUtils;
import org.openmdx.base.text.conversion.Base64;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.format.DateTimeFormat;

public class EmailTXmlFormat extends AbstractXmlFormat {

	public EmailTXmlFormat(
	) {
	}

	@Override
	public void format(
		Element parent,
		IData data,
		double protocolVersion
	) {		
		EmailT mail = (EmailT) data;
		DateTimeFormat eutcf = DateTimeFormat.EXTENDED_UTC_FORMAT;
		if(!mail.getTo().isEmpty()) {
			DOMUtils.createElementAndText(parent, "Email:", "To", this.addressesToString(mail.getTo()));
		}
		if(!mail.getCc().isEmpty()) {
			DOMUtils.createElementAndText(parent, "Email:", "CC", this.addressesToString(mail.getCc()));
		}
		DOMUtils.createElementAndText(parent, "Email:", "From", this.addressesToString(mail.getFrom() == null ? null : Collections.singletonList(mail.getFrom())));
		DOMUtils.createElementAndText(parent, "Email:", "Subject", mail.getSubject());
		DOMUtils.createElementAndText(parent, "Email:", "ReplyTo", this.addressesToString(mail.getReplyTo() == null ? null : Collections.singletonList(mail.getReplyTo())));
		DOMUtils.createElementAndText(parent, "Email:", "DateReceived", eutcf.format(mail.getDateReceived()));
		if(!mail.getTo().isEmpty() && mail.getTo().get(0) != null && mail.getTo().get(0).getDisplayName() != null && !"".equals(mail.getTo().get(0).getDisplayName())) {
			DOMUtils.createElementAndText(parent, "Email:", "DisplayTo", mail.getTo().get(0).getDisplayName());
		}
		DOMUtils.createElementAndText(parent, "Email:", "ThreadTopic", mail.getSubject());
		if(mail.getImportance() != null) {
			DOMUtils.createElementAndText(parent, "Email:", "Importance", Integer.toString(mail.getImportance().getValue()));
		}
		DOMUtils.createElementAndText(parent, "Email:", "Read", mail.isRead() ? "1" : "0");
		this.formatBody(parent, mail);
		this.formatAttachments(parent, mail);
		if(mail.getMessageClass() != null) {
			DOMUtils.createElementAndText(parent, "Email:", "MessageClass", mail.getMessageClass().toString());
		}
		this.formatMeetingRequest(parent, mail);		
		DOMUtils.createElementAndText(parent, "Email:", "InternetCPID", "65001"); // UTF-8
	}

	protected void formatBody(
		Element e, 
		EmailT mail
	) {
		String data = mail.getBody().getData();
		DOMUtils.createElementAndText(e, "Email:", "Body", data);
		if(data != null) {
			DOMUtils.createElementAndText(e, "Email:", "BodySize", "" + data.getBytes().length);
		}
		if(mail.getInvitation() != null) {
			DOMUtils.createElementAndText(e, "Email:", "ContentClass", "urn:content-classes:calendarmessage");
		} else {
			DOMUtils.createElementAndText(e, "Email:", "ContentClass", "urn:content-classes:message");
		}
	}

	private void formatMeetingRequest(
		Element e, 
		EmailT emailT
	) {
		DateTimeFormat eutcf = DateTimeFormat.EXTENDED_UTC_FORMAT;
		if(emailT.getInvitation() != null) {
			Element eMeetingRequest = DOMUtils.createElement(e, "Email:", "MeetingRequest");
			EventT eventT = emailT.getInvitation();
			DOMUtils.createElementAndText(eMeetingRequest, "Email:", "AllDayEvent", eventT.getAllDayEvent() ? "1" : "0");
			DOMUtils.createElementAndText(eMeetingRequest, "Email:", "StartTime", eutcf.format(eventT.getStartTime()));
			DOMUtils.createElementAndText(eMeetingRequest, "Email:", "DtStamp", eutcf.format(eventT.getDtStamp()));
			DOMUtils.createElementAndText(eMeetingRequest, "Email:", "EndTime", eutcf.format(eventT.getEndTime()));
			DOMUtils.createElementAndText(eMeetingRequest, "Email:", "InstanceType", "0");
			if(eventT.getLocation() != null && !"".equals(eventT.getLocation())) {
				DOMUtils.createElementAndText(eMeetingRequest, "Email:", "Location", eventT.getLocation());
			}
			if(eventT.getOrganizerEmail() != null && !"".equals(eventT.getOrganizerEmail())) {
				DOMUtils.createElementAndText(eMeetingRequest, "Email:", "Organizer", eventT.getOrganizerEmail());
			}
			else if(eventT.getOrganizerName() != null && !"".equals(eventT.getOrganizerName())) {
				DOMUtils.createElementAndText(eMeetingRequest, "Email:", "Organizer", eventT.getOrganizerName());
			}
			if(eventT.getReminder() != null) {
				DOMUtils.createElementAndText(eMeetingRequest, "Email:", "Reminder", eventT.getReminder().toString());
			}
			DOMUtils.createElementAndText(eMeetingRequest, "Email:", "ResponseRequested", "1");
			if(eventT.getSensitivity() != null) {
				DOMUtils.createElementAndText(eMeetingRequest, "Email:", "Sensitivity", Integer.toString(eventT.getSensitivity().getValue()));
			}
			if(eventT.getBusyStatus() != null) {
				DOMUtils.createElementAndText(eMeetingRequest, "Email:", "IntDBusyStatus", Integer.toString(eventT.getBusyStatus().getValue()));
			}
			DOMUtils.createElementAndText(eMeetingRequest, "Email:", "GlobalObjId", new String(Base64.encode(eventT.getUID().getBytes())));
			this.formatRecurrence(eMeetingRequest, eventT);
		}
	}

	private void formatRecurrence(
		Element e, 
		EventT eventT
	) {
		DateTimeFormat eutcf = DateTimeFormat.EXTENDED_UTC_FORMAT;
		RecurrenceT recurrenceT = eventT.getRecurrence();
		if(recurrenceT == null || recurrenceT.getType() == null) return;
		Element eRecurrence = DOMUtils.createElement(e, "Email:", "Recurrence");
		DOMUtils.createElementAndText(eRecurrence, "Email:", "Recurrence_Type", Integer.toString(recurrenceT.getType().getValue()));
		if(recurrenceT.getOccurrences() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Email:", "Recurrence_Occurrences", Integer.toString(recurrenceT.getOccurrences()));
		}
		if(recurrenceT.getInterval() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Email:", "Recurrence_Interval", recurrenceT.getInterval().toString());
		}
		if(recurrenceT.getWeekOfMonth() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Email:", "Recurrence_WeekOfMonth", Integer.toString(recurrenceT.getWeekOfMonth()));			
		}
		if(recurrenceT.getDayOfWeek() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Email:", "Recurrence_DayOfWeek", Integer.toString(RecurrenceDayOfWeek.asInt(recurrenceT.getDayOfWeek())));
		}
		if(recurrenceT.getMonthOfYear() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Email:", "Recurrence_MonthOfYear", Integer.toString(recurrenceT.getMonthOfYear()));			
		}
		if(recurrenceT.getUntil() != null) {
			DOMUtils.createElementAndText(eRecurrence, "Email:", "Recurrence_Until", eutcf.format(recurrenceT.getUntil()));
		}
	}

	private void formatAttachments(
		Element e, 
		EmailT emailT
	) {
		if(emailT.getAttachements() != null && !emailT.getAttachements().isEmpty()) {
			Element eAttachments = DOMUtils.createElement(e, "AirSyncBase:", "Attachments");
			List<AttachmentT> attachments = emailT.getAttachements();
			for (AttachmentT attachmentT : attachments) {
				Element eAttachment = DOMUtils.createElement(eAttachments, "AirSyncBase:", "Attachment");
				DOMUtils.createElementAndText(eAttachment, "AirSyncBase:", "DisplayName", attachmentT.getDisplayName());
				DOMUtils.createElementAndText(eAttachment, "AirSyncBase:", "FileReference", attachmentT.getAttachmentName());
				DOMUtils.createElementAndText(eAttachment, "AirSyncBase:", "Method", Integer.toString(attachmentT.getMethod().getValue()));
				if(attachmentT.getEstimatedDataSize() != null) {
					DOMUtils.createElementAndText(eAttachment, "AirSyncBase:", "EstimatedDataSize", Integer.toString(attachmentT.getEstimatedDataSize()));
				}
			}
		}
	}

	private String addressesToString(
		List<AddressT> addressesT
	) {
		StringBuilder sb = new StringBuilder();
		if(addressesT != null) {
			String sep = "";
			for(AddressT addressT: addressesT) {
				sb.append(sep);
				sb.append(addressT.getMail());
				sep = ", ";
			}
		}
		return sb.toString();
	}

	private EmailBodyT parseBody(
		Element eData
	) {		
		EmailBodyT emailBodyT = new EmailBodyT();			
		emailBodyT.setData(parseDOMString(DOMUtils.getUniqueElement(eData, "Email:", "Body")));
		return emailBodyT;
	}
		
	private List<AddressT> parseAddresses(
		String addresses
	) {
		List<AddressT> addressesT = new ArrayList<AddressT>();
		if(addresses != null) {
			String[] splits = addresses.split(",");
			for(int i = 0; i < splits.length; i++) {
				String address = splits[i];
				int pos1 = address.indexOf("<");
				int pos2 = address.indexOf(">");
				if(pos1 >= 0 && pos2 > pos1) { 
					addressesT.add(
						new AddressT(address.substring(pos1 + 1, pos2).trim())
					);
				}
			}
		}
		return addressesT; 		
	}
		
	private List<AttachmentT> parseAttachments(
		Element eData
	) {
		List<AttachmentT> attachmentsT = new ArrayList<AttachmentT>();
		Element eAttachments = DOMUtils.getUniqueElement(eData, "Email:", "Attachments");
		if(eAttachments != null) {
			for (int i = 0, n = eAttachments.getChildNodes().getLength(); i < n; i += 1) {
				Node node = eAttachments.getChildNodes().item(i);
				if(node instanceof Element) {
					Element eAttachment = (Element)node;
					AttachmentT attachmentT = new AttachmentT();
					attachmentT.setMethod(
						MethodAttachment.toMethodAttachment(
							Integer.valueOf(parseDOMString(DOMUtils.getUniqueElement(eAttachment, "Email:", "AttMethod")))
						)
					);
					attachmentT.setEstimatedDataSize(parseDOMInt(DOMUtils.getUniqueElement(eAttachment, "Email:", "AttSize")));
					attachmentT.setDisplayName((parseDOMString(DOMUtils.getUniqueElement(eAttachment, "Email:", "DisplayName"))));
					attachmentT.setAttachmentName(parseDOMString(DOMUtils.getUniqueElement(eAttachment, "Email:", "AttName")));
					attachmentsT.add(attachmentT);
				}
			}
		}
		return attachmentsT;
	}
	
	@Override
	public IData parse(
		Element eData
	) {
		EmailT emailT = new EmailT();
		emailT.setMimeData(parseDOMString(DOMUtils.getUniqueElement(eData, "Email:", "MIMEData")));
		List<AddressT> addressesTo = this.parseAddresses(
			parseDOMString(DOMUtils.getUniqueElement(eData, "Email:", "To"))
		);
		emailT.setTo(addressesTo);
		List<AddressT> addressesCc = this.parseAddresses(
			parseDOMString(DOMUtils.getUniqueElement(eData, "Email:", "CC"))
		);
		emailT.setCc(addressesCc);
		emailT.setBcc(Collections.<AddressT>emptyList());
		List<AddressT> addressesFrom = this.parseAddresses(
			parseDOMString(DOMUtils.getUniqueElement(eData, "Email:", "From"))
		);
		if(!addressesFrom.isEmpty()) {
			emailT.setFrom(addressesFrom.get(0));
		}
		emailT.setSubject(parseDOMString(DOMUtils.getUniqueElement(eData, "Email:", "Subject")));
		emailT.setDateReceived(parseDOMDate(DOMUtils.getUniqueElement(eData, "Email:", "DateReceived")));
		String importance = parseDOMString(DOMUtils.getUniqueElement(eData, "Email:", "Importance"));
		if(importance != null) {
			emailT.setImportance(Importance.toImportance(Integer.parseInt(importance)));
		}
		emailT.setRead(parseDOMInt2Boolean(DOMUtils.getUniqueElement(eData, "Email:", "Read")));
		emailT.setAttachements(this.parseAttachments(eData));
		emailT.setBody(this.parseBody(eData));
		emailT.setCategories(parseDOMStringCollection(DOMUtils.getUniqueElement(eData, "Email:", "Categories"), "Email:", "Category"));
		return emailT;
	}
	
}
