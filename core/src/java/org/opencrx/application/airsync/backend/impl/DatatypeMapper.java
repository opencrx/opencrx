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
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.backend.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.mail.Message.RecipientType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.opencrx.application.airsync.backend.cci.SyncBackend.RequestContext;
import org.opencrx.application.airsync.datatypes.AddressT;
import org.opencrx.application.airsync.datatypes.AttachmentDataT;
import org.opencrx.application.airsync.datatypes.AttachmentT;
import org.opencrx.application.airsync.datatypes.AttendeeStatus;
import org.opencrx.application.airsync.datatypes.AttendeeT;
import org.opencrx.application.airsync.datatypes.AttendeeType;
import org.opencrx.application.airsync.datatypes.BusyStatus;
import org.opencrx.application.airsync.datatypes.ContactT;
import org.opencrx.application.airsync.datatypes.DataType;
import org.opencrx.application.airsync.datatypes.EmailBodyT;
import org.opencrx.application.airsync.datatypes.EmailT;
import org.opencrx.application.airsync.datatypes.EventT;
import org.opencrx.application.airsync.datatypes.IData;
import org.opencrx.application.airsync.datatypes.Importance;
import org.opencrx.application.airsync.datatypes.MeetingStatus;
import org.opencrx.application.airsync.datatypes.MethodAttachment;
import org.opencrx.application.airsync.datatypes.MimeType;
import org.opencrx.application.airsync.datatypes.NoteT;
import org.opencrx.application.airsync.datatypes.RecurrenceDayOfWeek;
import org.opencrx.application.airsync.datatypes.RecurrenceT;
import org.opencrx.application.airsync.datatypes.RecurrenceType;
import org.opencrx.application.airsync.datatypes.Sensitivity;
import org.opencrx.application.airsync.datatypes.SyncDataItem;
import org.opencrx.application.airsync.datatypes.TaskT;
import org.opencrx.kernel.account1.cci2.MemberQuery;
import org.opencrx.kernel.account1.jmi1.AbstractGroup;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.Member;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.account1.jmi1.WebAddress;
import org.opencrx.kernel.activity1.jmi1.Absence;
import org.opencrx.kernel.activity1.jmi1.AbstractActivityParty;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.ExternalActivity;
import org.opencrx.kernel.activity1.jmi1.Incident;
import org.opencrx.kernel.activity1.jmi1.Mailing;
import org.opencrx.kernel.activity1.jmi1.Meeting;
import org.opencrx.kernel.activity1.jmi1.PhoneCall;
import org.opencrx.kernel.activity1.jmi1.Task;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Activities.PartyStatus;
import org.opencrx.kernel.backend.Activities.PartyType;
import org.opencrx.kernel.backend.Activities.Priority;
import org.opencrx.kernel.backend.Addresses;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.backend.Notifications;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.backend.UserHomes.AlertState;
import org.opencrx.kernel.backend.VCard;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.DocumentFolder;
import org.opencrx.kernel.document1.jmi1.DocumentRevision;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.generic.jmi1.Media;
import org.opencrx.kernel.home1.cci2.EMailAccountQuery;
import org.opencrx.kernel.home1.jmi1.Alert;
import org.opencrx.kernel.home1.jmi1.EMailAccount;
import org.opencrx.kernel.home1.jmi1.SyncFeed;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.MimeUtils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.Base64;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.cci2.BinaryLargeObjects;
import org.w3c.format.DateTimeFormat;
import org.w3c.spi2.Datatypes;

public class DatatypeMapper {

	public DatatypeMapper(
	) {		
	}
	
	public SyncDataItem toDataItem(
		RefObject_1_0 object,
		boolean noData,
		UserHome user,
		RequestContext requestContext
	) throws ServiceException {
		if(
			object instanceof Meeting ||
			object instanceof Incident ||			
			object instanceof PhoneCall ||			
			object instanceof Mailing ||			
			object instanceof Absence ||
			object instanceof ExternalActivity			
		) {
			return toEventT(
				(Activity)object, 
				noData, 
				user,
				requestContext
			);
		}
		else if(object instanceof EMail) {
			return toEMailT(
				(EMail)object, 
				noData, 
				user,
				requestContext
			);
		}
		else if(object instanceof Task) {
			return toTaskT(
				(Task)object, 
				noData, 
				user,
				requestContext
			);
		}
		else if(object instanceof Account) {
			return toContactT(
				(Account)object, 
				noData, 
				user,
				requestContext
			);
		}
		else if(object instanceof Alert) {
			return toEMailT(
				(Alert)object, 
				noData, 
				user,
				requestContext
			);
		}
		else {
			return null;
		}
	}
	
	public void toObject(
		IData data,
		RefObject_1_0 object,
		UserHome user,
		RequestContext requestContext
	) throws ServiceException {
		if(data instanceof ContactT) {
			toAccount(
				(ContactT)data,
				(Account)object,
				user,
				requestContext
			);
		} else if(data instanceof TaskT) {
			toTask(
				(TaskT)data,
				(Activity)object,
				user,
				requestContext
			);
		} else if(data instanceof EventT) {
			toEvent(
				(EventT)data,
				(Activity)object,
				user,
				requestContext
			);
		} else if(data instanceof EmailT) {
			if(object instanceof Alert) {
				toAlert(
					(EmailT)data,
					(Alert)object,
					user,
					requestContext
				);
			} else if(object instanceof EMail) {
				toEMail(
					(EmailT)data,
					(EMail)object,
					user,
					requestContext
				);
			}
		} else if(data instanceof NoteT) {
			toNote(
				(NoteT)data,
				(Document)object,
				user,
				requestContext
			);
		}
	}
		
	public String normalizeMultilineString(
		String s
	) {
		if(s == null) return null;
		StringBuilder target = new StringBuilder();
		for(int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if(c == '\n' || c == '\t' || c >= ' ') {
				target.append(c);
			} else {
				// ignore other control characters
			}
		}
		return target.toString();
	}
	
	/**
	 * Try to match given item and return itemId. This method is
	 * typically overriden by a user-specific data type mapper.
	 * 
	 * @param user current user
	 * @param data
	 * @return id of matched item or null.
	 * @throws ServiceException
	 */
	public String matchItem(
		UserHome user,
		IData data		
	) throws ServiceException {
		return null;
	}
	
	private void updateContact(
		Account account,
		String vcard
	) throws ServiceException {
        List<String> errors = new ArrayList<String>();
        List<String> report = new ArrayList<String>();
        byte[] item = null;
        try {
            item = vcard.getBytes("UTF-8");
        } 
        catch(Exception e) {
            item = vcard.getBytes();    
        }
		VCard.getInstance().importItem(
			item, 
			account, 
			(short)0, 
			errors, 
			report
		);
	}
	
	private String getVCardContactName(
		ContactT contactT
	) {
		return
			(contactT.getLastName() == null ? "" : contactT.getLastName()) + ";" +
			(contactT.getFirstName() == null ? "" : contactT.getFirstName()) + ";" +
			(contactT.getMiddleName() == null ? "" : contactT.getMiddleName()) + ";" +
			(contactT.getTitle() == null ? "" : contactT.getTitle()) + ";" +
			(contactT.getSuffix() == null ? "" : contactT.getSuffix());
	}
	
	private String getVCardPostalAddress(
		String street,
		String city,
		String state,
		String postalCode,
		String country
	) {
		if(
			(street == null || street.length() == 0) &&
			(city == null || city.length() == 0) &&
			(state == null || state.length() == 0) &&
			(postalCode == null || postalCode.length() == 0) &&
			(country == null || country.length() == 0)
		) {
			return null;
		}
		else {
			return 
				";;" +
				(street == null ? "" : street.replace("  ", "\r\n").replace("\r\n", "=0D=0A")).replace("\n", "=0D=0A") + ";" +
				(city == null ? "" : city) + ";" +
				(state == null ? "" : state) + ";" +
				(postalCode == null ? "" : postalCode) + ";" +
				(country == null ? "" : country);
		}
	}
	
	private String parseEmailAddress(
		String emailAddress
	) {
		if(emailAddress.startsWith("\"")) {
			int end = emailAddress.lastIndexOf("\"");
			if(end > 0) {
				emailAddress = emailAddress.substring(end + 1);
			}
		}
		if(emailAddress.indexOf("<") >= 0) {
			int start = emailAddress.indexOf("<");
			int end = emailAddress.lastIndexOf(">");
			if(end > start) {
				emailAddress = emailAddress.substring(start + 1, end);
			}
		}
		return emailAddress.trim();
	}
	
	private DateTimeFormat getUserDateTimeFormat(
		UserHome user
	) throws ServiceException {
		String userTz = UserHomes.getInstance().getUserTimezone(
			UserHomes.getInstance().getUserSettings(user)			
		);
		return DateTimeFormat.getInstance(
			"yyyyMMdd'T'HHmmss.SSS'Z'", 
			userTz, 
			false
		);	
	}
	
	public void toAccount(
		ContactT contactT,
		Account account,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(account);
		DateTimeFormat utcf = DateTimeFormat.BASIC_UTC_FORMAT;
		StringBuilder vcard = new StringBuilder();
		vcard.append("BEGIN:VCARD\n");		
		vcard.append("VERSION:2.1\n");
		if(account.getVcard() != null) {
			String uid = VCard.getInstance().getVCardUid(account.getVcard());
			if(uid != null) {
				vcard.append("UID:" + uid + "\n");
			}
		}
		if(account instanceof Contact) {
			vcard.append("N:" + getVCardContactName(contactT) + "\n");
			if(contactT.getBirthday() != null) {
				vcard.append("BDAY:" + utcf.format(contactT.getBirthday()).substring(0,15) + "Z\n");
			}
			if(contactT.getCompanyName() != null) {
				vcard.append("ORG:" + contactT.getCompanyName() + "\n");
			}
			if(contactT.getJobTitle() != null) {
				vcard.append("TITLE:" + contactT.getJobTitle() + "\n");
			}
		}
		else {
			if(contactT.getFileAs() != null && contactT.getFileAs().length() > 0) {
				vcard.append("N:" + contactT.getFileAs() + "\n");			
				vcard.append("FN:" + contactT.getFileAs() + "\n");
			}
			else if(contactT.getLastName() != null && contactT.getLastName().length() > 0) {
				vcard.append("N:" + contactT.getLastName() + "\n");			
				vcard.append("FN:" + contactT.getLastName() + "\n");
			}
		}
		if(contactT.getBusinessPhoneNumber() != null) {
	        vcard.append("TEL;WORK;VOICE:" + contactT.getBusinessPhoneNumber() + "\n");
		}
		if(contactT.getHomePhoneNumber() != null) {
	        vcard.append("TEL;HOME;VOICE:" + contactT.getHomePhoneNumber() + "\n");			
		}
		if(contactT.getMobilePhoneNumber() != null) {
			vcard.append("TEL;CELL;VOICE:" + contactT.getMobilePhoneNumber() + "\n");
		}
		if(contactT.getEmail1Address() != null) {
			vcard.append("EMAIL;TYPE=PREF,INTERNET,WORK:" + parseEmailAddress(contactT.getEmail1Address()) + "\n");		
		}
		if(contactT.getEmail2Address() != null) {
			vcard.append("EMAIL;TYPE=INTERNET,HOME:" + parseEmailAddress(contactT.getEmail2Address()) + "\n");			
		}
		if(contactT.getBusinessFaxNumber() != null) {
			vcard.append("TEL;WORK;FAX:" + contactT.getBusinessFaxNumber() + "\n");
		}
		if(contactT.getHomeFaxNumber() != null) {
			vcard.append("TEL;HOME;FAX:" + contactT.getHomeFaxNumber() + "\n");			
		}
		if(contactT.getWebPage() != null) {
			vcard.append("URL;WORK:" + contactT.getWebPage() + "\n");
		}
		String adrWork = getVCardPostalAddress(
			contactT.getBusinessStreet(), 
			contactT.getBusinessAddressCity(),
			contactT.getBusinessState(),
			contactT.getBusinessPostalCode(),
			contactT.getBusinessAddressCountry()
		);
		if(adrWork != null) {
			vcard.append("ADR;WORK;ENCODING=QUOTED-PRINTABLE:" + adrWork + "\n");
		}
		String adrHome = getVCardPostalAddress(
			contactT.getHomeAddressStreet(), 
			contactT.getHomeAddressCity(),
			contactT.getHomeAddressState(),
			contactT.getHomeAddressPostalCode(),
			contactT.getHomeAddressCountry()
		);
		if(adrHome != null) {
			vcard.append("ADR;HOME;ENCODING=QUOTED-PRINTABLE:" + adrHome + "\n");			
		}
		vcard.append("END:VCARD");
		this.updateContact(
			account,
			vcard.toString()
		);
		if(account instanceof Contact) {
			Contact contact = (Contact)account;
			if(contactT.getPicture() != null && contactT.getPicture().length() > 0) {
				byte[] picture = null;
				try {
					picture = Base64.decode(contactT.getPicture());
				} catch(Exception e) {}
				if(picture != null) {
					org.opencrx.kernel.generic.jmi1.Media media = null;
					org.opencrx.kernel.generic.cci2.MediaQuery mediaQuery = 
						(org.opencrx.kernel.generic.cci2.MediaQuery)pm.newQuery(org.opencrx.kernel.generic.jmi1.Media.class);
					mediaQuery.thereExistsContentName().equalTo("me.jpg");
					Collection<org.opencrx.kernel.generic.jmi1.Media> medias = account.getMedia(mediaQuery);
					if(!medias.isEmpty()) {
						media = medias.iterator().next();
					} else {
						media = pm.newInstance(org.opencrx.kernel.generic.jmi1.Media.class);
						media.setContentMimeType("image/jpeg");
						media.setContentName("me.jpg");
						account.addMedia(
							Base.getInstance().getUidAsString(), 
							media
						);
					}
					media.setContent(BinaryLargeObjects.valueOf(picture));
					contact.setPicture(media);
				}
			}
			if(contactT.getNickName() != null) {
				contact.setNickName(contactT.getNickName());
			}
			if(contactT.getDepartment() != null) {
				contact.setDepartment(contactT.getDepartment());
			}
			if(contactT.getAnniversary() != null) {
				contact.setAnniversary(contactT.getAnniversary());
			}
			if(contactT.getChildren() != null) {
				contact.getChildrenNames().clear();
				contact.getChildrenNames().addAll(contactT.getChildren());
			}
		}
		if(contactT.getCategories() != null) {
			account.getCategory().clear();
			account.getCategory().addAll(contactT.getCategories());
		}
		if(contactT.getBody() != null) {
			String body = normalizeMultilineString(contactT.getBody());
	        int pos = body.startsWith(Base.COMMENT_SEPARATOR_BOT) ? 0 : body.lastIndexOf(Base.COMMENT_SEPARATOR_EOT);
            body =  pos >= 0 ? 
                body.substring(0, pos) : 
                	body;        	
			account.setDescription(body);
		}
	}

	public void toNote(
		NoteT noteT,
		Document document,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(document);
		document.setName(noteT.getSubject());
		document.setTitle(noteT.getSubject());
		MediaContent revision = pm.newInstance(MediaContent.class);
		revision.setName(noteT.getSubject());
		if(document.getHeadRevision() != null) {
			revision.setVersion(document.getHeadRevision().getVersion() + 1);
		}
		revision.setContentMimeType("text/plain");
		revision.setContentName(noteT.getSubject());
		if(noteT.getBody() != null) {
			try {
				revision.setContent(BinaryLargeObjects.valueOf(noteT.getBody().getBytes("UTF-8")));
			} catch(Exception e) {}
		}
		if(noteT.getCategories() != null) {
			String keywords = "";
			String sep = "";
			for(String category: noteT.getCategories()) {
				keywords += sep + category;
				sep = ",";
			}
			document.setKeywords(keywords);
		}						
		document.addRevision(
			Base.getInstance().getUidAsString(),
			revision
		);
		document.setHeadRevision(revision);
	}
	
	private void updateActivity(
		Activity activity,
		String ical
	) throws ServiceException {
        List<String> errors = new ArrayList<String>();
        List<String> report = new ArrayList<String>();
        byte[] item = null;
        try {
            item = ical.getBytes("UTF-8");
        } 
        catch(Exception e) {
            item = ical.getBytes();    
        }
        ICalendar.getInstance().importItem(
            item, 
            activity, 
            (short)0, // en_US
            errors, 
            report
        );		
	}
	
	private String getCalendarUID(
		Activity activity
	) {
		String uid = activity.refGetPath().getBase();
		for(String externalLink: activity.getExternalLink()) {
			if(externalLink.startsWith(ICalendar.ICAL_SCHEMA)) {
				uid = externalLink.substring(ICalendar.ICAL_SCHEMA.length());
				break;
			}
		}
		return uid;
	}
	
	public void toTask(
		TaskT taskT,
		Activity task,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		DateTimeFormat utcf = DateTimeFormat.BASIC_UTC_FORMAT;
		StringBuilder vtodo = new StringBuilder();
		vtodo.append("BEGIN:VCALENDAR\n");
		vtodo.append("PRODID:" + ICalendar.PROD_ID + "\n");
		vtodo.append("VERSION:2.0\n");
		vtodo.append("BEGIN:VTODO\n");
		vtodo.append("CLASS:CONFIDENTIAL\n");
		vtodo.append("UID:" + getCalendarUID(task) + "\n");		
		if(taskT.getSubject() != null) {
			vtodo.append("SUMMARY:" + taskT.getSubject() + "\n");
		}
		if(taskT.getStartdate() != null || taskT.getUtcstartdate() != null) {
			if(taskT.getStartdate() != null) {
				vtodo.append("DTSTART:" + utcf.format(taskT.getStartdate()).substring(0,15) + "Z\n");
			}
			else {
				vtodo.append("DTSTART:" + utcf.format(taskT.getUtcstartdate()).substring(0,15) + "Z\n");				
			}
		}
		if(Boolean.TRUE.equals(taskT.getComplete()) && taskT.getDatecompleted() != null) {
   			vtodo.append("COMPLETE:" + utcf.format(taskT.getDatecompleted()).substring(0, 15) + "Z\n");
		}
		if(taskT.getDuedate() != null || taskT.getUtcduedate() != null) {
			if(taskT.getUtcduedate() != null) {
				vtodo.append("DUE:" + utcf.format(taskT.getUtcduedate()).substring(0,15) + "Z\n");				
			}
			else {
				vtodo.append("DUE:" + utcf.format(taskT.getDuedate()).substring(0,15) + "Z\n");
			}
		}
		if(taskT.getRecurrence() != null) {
			vtodo.append("RRULE:" + getICalRRule(taskT.getRecurrence()) + "\n");
		}
		if(taskT.getImportance() != null) {
			vtodo.append("PRIORITY:" + getICalPriority(taskT.getImportance()) + "\n");
		}
		vtodo.append("END:VTODO\n");
		vtodo.append("END:VCALENDAR");		
		if(taskT.getCategories() != null) {
			task.getCategory().clear();
			task.getCategory().addAll(taskT.getCategories());
		}
		updateActivity(
			task, 
			vtodo.toString()
		);		
		if(taskT.getBody() != null) {
			String body = normalizeMultilineString(taskT.getBody());
            int pos = body.startsWith(Base.COMMENT_SEPARATOR_BOT) ? 0 : body.lastIndexOf(Base.COMMENT_SEPARATOR_EOT);
            body = pos >= 0 ? 
                body.substring(0, pos) : 
                body;        	
			task.setDescription(body);
		}
	}
	
	public void toEvent(
		EventT eventT,
		Activity event,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		DateTimeFormat utcf = DateTimeFormat.BASIC_UTC_FORMAT;		
		DateTimeFormat userf = this.getUserDateTimeFormat(user);
		StringBuilder vevent = new StringBuilder();
		vevent.append("BEGIN:VCALENDAR\n");
		vevent.append("PRODID:" + ICalendar.PROD_ID + "\n");
		vevent.append("VERSION:2.0\n");
		vevent.append("BEGIN:VEVENT\n");
		vevent.append("CLASS:CONFIDENTIAL\n");
		vevent.append("UID:" + (eventT.getUID() == null ? getCalendarUID(event) : eventT.getUID()) + "\n");
		if(eventT.getStartTime() != null) {
			vevent.append(
				eventT.getAllDayEvent() ?
	    			"DTSTART;VALUE=DATE:" + userf.format(eventT.getStartTime()).substring(0, 8) + "\n" :                            
	    				"DTSTART:" + utcf.format(eventT.getStartTime()).substring(0, 15) + "Z\n"
	    	);
		}
		if(eventT.getEndTime() != null) {
			vevent.append(
				eventT.getAllDayEvent() ?
	    			"DTEND;VALUE=DATE:" + userf.format(eventT.getEndTime()).substring(0, 8) + "\n":                            
	    				"DTEND:" + utcf.format(eventT.getEndTime()).substring(0, 15) + "Z\n"
	    	);			
		}
		if(eventT.getLocation() != null) {
			vevent.append("LOCATION:" + eventT.getLocation() + "\n");
		}
		if(eventT.getDtStamp() != null) {
			vevent.append("DTSTAMP:" + utcf.format(eventT.getDtStamp()).substring(0, 15) + "Z\n");
		}
		if(eventT.getSubject() != null) {
			vevent.append("SUMMARY:" + eventT.getSubject() + "\n");
		}
		if(eventT.getMeetingStatus() != null) {		
			vevent.append("STATUS:" + getICalStatus(eventT.getMeetingStatus()) + "\n");
		}
		if(eventT.getOrganizerEmail() != null) {
			vevent.append("ORGANIZER:MAILTO:" + parseEmailAddress(eventT.getOrganizerEmail()) + "\n");
		}
		if(eventT.getRecurrence() != null) {
			vevent.append("RRULE:" + getICalRRule(eventT.getRecurrence()) + "\n");
		}
		if(eventT.getAttendees() != null) {
			for(AttendeeT attendeeT: eventT.getAttendees()) {
                String partyType = attendeeT.getAttendeeType() == AttendeeType.OPTIONAL ? 
                	"OPT-PARTICIPANT" : 
                		"REQ-PARTICIPANT";
                PartyStatus partyStatus = null;
                if(attendeeT.getAttendeeStatus() == AttendeeStatus.ACCEPT) {
                	partyStatus = PartyStatus.ACCEPTED;
                } else if(attendeeT.getAttendeeStatus() == AttendeeStatus.DECLINE) {
                	partyStatus = PartyStatus.DECLINED;
                } else if(attendeeT.getAttendeeStatus() == AttendeeStatus.TENTATIVE) {
                	partyStatus = PartyStatus.TENTATIVE;
                } else if(attendeeT.getAttendeeStatus() == AttendeeStatus.NOT_RESPONDED) {
                	partyStatus = PartyStatus.NEEDS_ACTION;
	            } else if(attendeeT.getAttendeeStatus() == AttendeeStatus.RESPONSE_UNKNOWN) {
	            	partyStatus = PartyStatus.NEEDS_ACTION;
	            }
                if(attendeeT.getName() == null) {
                    vevent.append("ATTENDEE;CN=" + parseEmailAddress(attendeeT.getEmail()) + ";ROLE=" + partyType + ";" + (partyStatus == null ? "" : ";PARTSTAT=" + partyStatus.toString().replace("_", "-")) + ";RSVP=TRUE:MAILTO:" + parseEmailAddress(attendeeT.getEmail()) + "\n");          
                }
                else {
                    vevent.append("ATTENDEE;CN=\"" + attendeeT.getName() + " (" + parseEmailAddress(attendeeT.getEmail()) + ")\";ROLE=" + partyType + (partyStatus == null ? "" : ";PARTSTAT=" + partyStatus.toString().replace("_", "-")) + ";RSVP=TRUE:MAILTO:" + parseEmailAddress(attendeeT.getEmail()) + "\n");            
                }
			}
		}
		if(eventT.getExceptions() != null && !eventT.getExceptions().isEmpty()) {
			vevent.append("EXDATE:" + getICalExdates(eventT.getExceptions()) + "\n");
		}
		vevent.append("END:VEVENT\n");
		vevent.append("END:VCALENDAR");		
		if(eventT.getCategories() != null) {
			event.getCategory().clear();
			event.getCategory().addAll(eventT.getCategories());
		}
		updateActivity(
			event, 
			vevent.toString()
		);
		if(eventT.getBody() != null) {
			String body = normalizeMultilineString(eventT.getBody());
            int pos = body.startsWith(Base.COMMENT_SEPARATOR_BOT) ? 0 : body.lastIndexOf(Base.COMMENT_SEPARATOR_EOT);
            body =  pos >= 0 ? 
                body.substring(0, pos) : 
                body;        	
			event.setDescription(body);
		}
	}
	
	public void toEMail(
		EmailT emailT,
		EMail email,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(email);
		String providerName = email.refGetPath().get(2);
        String segmentName = email.refGetPath().get(4);
        if(emailT.getMimeData() != null) {
        	try {
	            MimeMessage mimeMessage = new MimeUtils.MimeMessageImpl(
	                new ByteArrayInputStream(emailT.getMimeData().getBytes("US-ASCII"))
	            );
	            pm.currentTransaction().begin();
	        	Activities.getInstance().importMimeMessage(
	        		email,
	        		mimeMessage, 
	        		true // isNew
	        	);
	        	pm.currentTransaction().commit();
        	}
        	catch(Exception e) {
        		new ServiceException(e).log();
        	}
        }
        else {
			// FROM
			String[] addressesFrom = new String[]{emailT.getFrom().getMail()};
	        List<org.opencrx.kernel.account1.jmi1.EMailAddress> senderAddresses = 
	            Accounts.getInstance().lookupEmailAddress(
	                pm,
	                providerName,
	                segmentName,
	                emailT.getFrom().getMail()
	            );
	        if(senderAddresses.isEmpty()) {
	        	senderAddresses = Accounts.getInstance().lookupEmailAddress(
	                pm,
	                providerName,
	                segmentName,
	                Addresses.UNASSIGNED_ADDRESS
	            );                            
	        }
	        EMailAddress from = null;
	        if(!senderAddresses.isEmpty()) {
	            from = senderAddresses.iterator().next();
	            email.setSender(from);
	        }
			// TO
			List<String> addresses = new ArrayList<String>();
			for(AddressT addressT: emailT.getTo()) {
				addresses.add(addressT.getMail());
			}
			String[] addressesTo = addresses.toArray(new String[addresses.size()]); 
			Activities.getInstance().mapAddressesToEMailRecipients(
				email, 
				addressesTo, 
				PartyType.EMAIL_TO 
			);
			// CC
			addresses = new ArrayList<String>();
			for(AddressT addressT: emailT.getCc()) {
				addresses.add(addressT.getMail());
			}
			String[] addressesCc = addresses.toArray(new String[addresses.size()]); 
			Activities.getInstance().mapAddressesToEMailRecipients(
				email, 
				addressesCc, 
				PartyType.EMAIL_CC 
			);
			// BCC
			addresses = new ArrayList<String>();
			for(AddressT addressT: emailT.getBcc()) {
				addresses.add(addressT.getMail());
			}
			String[] addressesBcc = addresses.toArray(new String[addresses.size()]);
			Activities.getInstance().mapAddressesToEMailRecipients(
				email, 
				addressesBcc, 
				PartyType.EMAIL_BCC 
			);
	        // Add originator and recipients to a note
			String recipientsAsText = Activities.getInstance().getRecipientsAsNoteText(
	            pm,
	            providerName,
	            segmentName,
	            addressesFrom,
	            addressesTo,
	            addressesCc,
	            addressesBcc
	        );
			org.opencrx.kernel.generic.jmi1.Note note = pm.newInstance(org.opencrx.kernel.generic.jmi1.Note.class);
			note.setTitle("Recipients");
			note.setText(recipientsAsText);
			email.addNote(
				Activities.getInstance().getUidAsString(),
				note
			);
			switch(emailT.getImportance()) {
				case LOW:
					email.setPriority(Priority.LOW.getValue());
					break;
				case NORMAL:
					email.setPriority(Priority.NORMAL.getValue());
					break;
				case HIGH:
					email.setPriority(Priority.HIGH.getValue());
					break;
			}
			email.setSendDate(emailT.getDateReceived());
			email.setMessageSubject(emailT.getSubject());
			email.setMessageBody(
				emailT.getBody() == null ? null : emailT.getBody().getData()
			);
        }
		if(emailT.getCategories() != null) {
			email.getCategory().clear();
			email.getCategory().addAll(emailT.getCategories());
		}
	}
	
	public void toAlert(
		EmailT emailT,
		Alert alert,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		if(emailT.isRead()) {
			alert.setAlertState(AlertState.READ.getValue());
		} else {
			alert.setAlertState(AlertState.NEW.getValue());
		}
	}
	
	public Path toObjectIdentity(
		String objectId
	) {
		if(objectId == null) return null;
		if(objectId.indexOf("?") > 0) {
			objectId = objectId.substring(0, objectId.lastIndexOf("?"));
		}
		if(objectId.startsWith(SYNC_FEED_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:home1",
					"provider",
					components[0],
					"segment",
					components[1],
					"userHome", 
				    components[2],
					"syncProfile", 
				    components[3],
					"feed", 
				    components[4]
				}
			);
		}
		else if(objectId.startsWith(ACTIVITY_TRACKER_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:activity1",
					"provider",
					components[0],
					"segment",
					components[1],
					"activityTracker", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(ACTIVITY_CATEGORY_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:activity1",
					"provider",
					components[0],
					"segment",
					components[1],
					"activityCategory", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(ACTIVITY_MILESTONE_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:activity1",
					"provider",
					components[0],
					"segment",
					components[1],
					"activityMilestone", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(ACTIVITY_FILTER_GLOBAL_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:activity1",
					"provider",
					components[0],
					"segment",
					components[1],
					"activityFilter", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(ACTIVITY_FILTER_GROUP_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:activity1",
					"provider",
					components[0],
					"segment",
					components[1],
					components[2], 
				    components[3],
					"activityFilter", 
				    components[4]
				}
			);
		}
		else if(objectId.startsWith(USER_HOME_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:home1",
					"provider",
					components[0],
					"segment",
					components[1],
					"userHome", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(ACTIVITY_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:activity1",
					"provider",
					components[0],
					"segment",
					components[1],
					"activity", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(ACCOUNT_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:account1",
					"provider",
					components[0],
					"segment",
					components[1],
					"account", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(ALERT_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:home1",
					"provider",
					components[0],
					"segment",
					components[1],
					"userHome", 
				    components[2],
				    "alert",
				    components[3]
				}
			);
		}
		else if(objectId.startsWith(MEDIA_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:activity1",
					"provider",
					components[0],
					"segment",
					components[1],
					"activity", 
				    components[2],
				    "media",
				    components[3]
				}
			);
		}
		else if(objectId.startsWith(DOCUMENT_FOLDER_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:document1",
					"provider",
					components[0],
					"segment",
					components[1],
					"folder", 
				    components[2]
				}
			);
		}
		else if(objectId.startsWith(DOCUMENT_URI_SCHEME)) { 
			String[] components = objectId.substring(objectId.indexOf("://") + 3).split("/");
			return new Path(
				new String[]{
					"org:opencrx:kernel:document1",
					"provider",
					components[0],
					"segment",
					components[1],
					"document", 
				    components[2]
				}
			);
		}
		else {
			return null;
		}
	}
	
	public String toObjectId(
		RefObject_1_0 object
	) {
		if(object == null) return null;
		Path path = object.refGetPath();
		if(object instanceof SyncFeed) {
			return SYNC_FEED_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6) + "/" + path.get(8) + "/" + path.get(10);
		}
		else if(object instanceof ActivityTracker) { 
			return ACTIVITY_TRACKER_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6); 
		}
		else if(object instanceof ActivityCategory) { 
			return ACTIVITY_CATEGORY_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6); 
		}
		else if(object instanceof ActivityMilestone) { 
			return ACTIVITY_MILESTONE_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6); 
		}
		else if(object instanceof ActivityFilterGlobal) {
			return ACTIVITY_FILTER_GLOBAL_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6);
		}
		else if(object instanceof ActivityFilterGroup) {
			return ACTIVITY_FILTER_GROUP_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(5) + "/" + path.get(6) + "/" + path.get(8);
		}
		else if(object instanceof UserHome) { 
			return USER_HOME_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6); 
		}
		else if(object instanceof Activity) {
			return ACTIVITY_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6); 
		}
		else if(object instanceof Account) { 
			return ACCOUNT_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6); 
		}
		else if(object instanceof Alert) { 
			return ALERT_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6) + "/" + path.get(8);
		}
		// Medias attached to an activity
		else if(object instanceof Media) { 
			return MEDIA_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6) + "/" + path.get(8);
		}
		else if(object instanceof DocumentFolder) {
			return DOCUMENT_FOLDER_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6);
		}
		else if(object instanceof Document) {
			return DOCUMENT_URI_SCHEME + path.get(2) + "/" + path.get(4) + "/" + path.get(6);
		}
		else {
			return null;
		}
	}
	
	/**
	 * Find member of a group account.
	 * @param parent
	 * @param memberRole
	 * @return
	 */
	public Account findAccountMember(
		Account parent,
		short memberRole
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(parent);
		MemberQuery memberQuery = (MemberQuery)pm.newQuery(Member.class);
		memberQuery.thereExistsMemberRole().equalTo(memberRole);
		memberQuery.orderByName().ascending();
		List<Member> members = parent.getMember(memberQuery);
		return !members.isEmpty() ? members.iterator().next().getAccount() : null;
	}
	
	/**
	 * Map activity attendees to AttendeeTs.
	 * @param activity
	 * @return
	 * @throws ServiceException
	 */
	private List<AttendeeT> getAttendeesT(
		Activity activity
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(activity);
        List<AttendeeT> attendeesT = new ArrayList<AttendeeT>();
        List<AbstractActivityParty> parties = Activities.getInstance().getActivityParties(activity);
        for(AbstractActivityParty party: parties) {
        	if(party.getPartyType() != Activities.PartyType.NA.getValue()) {
	        	RefObject_1_0 partyHolder = null;
	        	try {
	        		partyHolder = (RefObject_1_0)party.refGetValue("party");
	        	} catch(Exception e) {}
	            if(partyHolder != null) {
	                try {
	                    Account account = null;
	                    String emailAddress = null;
	                    // Party is Contact
	                    if(partyHolder instanceof Account) {
	                        account = (Account)partyHolder;
	                        emailAddress = Accounts.getInstance().getPrimaryBusinessEMail(
	                            account,
	                            party.getEmailHint()
	                        );
	                    }
	                    // Party is address
	                    else if(partyHolder instanceof EMailAddress) {
	                        account = (Account)pm.getObjectById(
	                            partyHolder.refGetPath().getParent().getParent()
	                        );
	                        emailAddress = ((EMailAddress)partyHolder).getEmailAddress();
	                    }
	                    AttendeeType partyType = party.getPartyType() == Activities.PartyType.OPTIONAL.getValue() ? 
	                    	AttendeeType.OPTIONAL : 
	                    		party.getPartyType() == Activities.PartyType.REQUIRED.getValue() ? 
	                    			AttendeeType.REQUIRED :
	                    				null;
	                    if(partyType != null && emailAddress != null) {
	                    	AttendeeStatus attendeeStatus = AttendeeStatus.RESPONSE_UNKNOWN;
	                    	if(party.getPartyStatus() == PartyStatus.ACCEPTED.getValue()) {
	                    		attendeeStatus = AttendeeStatus.ACCEPT;	                    		
	                    	} else if(party.getPartyStatus() == PartyStatus.DECLINED.getValue()) {
	                    		attendeeStatus = AttendeeStatus.DECLINE;
	                    	} else if(party.getPartyStatus() == PartyStatus.TENTATIVE.getValue()) {
	                    		attendeeStatus = AttendeeStatus.TENTATIVE;	                    		
	                    	} else if(party.getPartyStatus() == PartyStatus.NEEDS_ACTION.getValue()) {
	                    		attendeeStatus = AttendeeStatus.NOT_RESPONDED;	                    		
	                    	}
	                        String fullName = account == null ? null : account.getFullName();
	                        if(fullName == null) {
	                            attendeesT.add(
	                            	new AttendeeT(
	                            		emailAddress,
	                            		emailAddress,
	                            		attendeeStatus,
	                            		partyType
	                            	)
	                            );          
	                        }
	                        else {
	                            attendeesT.add(
	                            	new AttendeeT(
	                            		emailAddress,
	                            		fullName,
	                            		attendeeStatus,
	                            		partyType
	                            	)
	                            );
	                        }
	                    }
	                }
	                catch(ServiceException e) {
	                    if(e.getExceptionCode() != BasicException.Code.AUTHORIZATION_FAILURE) {
	                        throw e;
	                    }
	                }
	            }
            }
        }
        return attendeesT;
	}
	
	/**
	 * Get MeetingStatus of activity.
	 * @param activity
	 * @return
	 */
	private MeetingStatus getCalendarMeetingStatus(
		Activity activity
	) {
        Number percentComplete = activity.getPercentComplete();
        if(Boolean.TRUE.equals(activity.isDisabled())) {
        	return MeetingStatus.MEETING_IS_CANCELED;
        }
        else {
            if(percentComplete == null || percentComplete.intValue() == 0) {
                return MeetingStatus.MEETING_RECEIVED;
            }
            else {
                return MeetingStatus.IS_A_MEETING;
            }
        }
	}

	/**
	 * Map MeetingStatus to status.
	 * @param status
	 * @return
	 */
	private String getICalStatus(
		MeetingStatus status
	) {
		switch(status) {
			case IS_A_MEETING:
				return "CONFIRMED";
			case IS_NOT_A_MEETING:
				return "CANCELLED";
			case MEETING_RECEIVED:
				return "TENTATIVE";
			case MEETING_IS_CANCELED:
				return "CANCELLED";
			case MEETING_IS_CANCELED_AND_RECEIVED:
				return "CANCELLED";
			default:
				return "CONFIRMED";
		}		
	}
	
	/**
	 * Map activity to RecurrenceT.
	 * @param activity
	 * @return
	 */
	public RecurrenceT getRecurrence(
		Activity activity
	) {
		String ical = activity.getIcal();
		if(ical != null && ical.indexOf("RRULE:") > 0) {
			int start = ical.indexOf("RRULE:");
			int end = ical.indexOf("\n", start);
			if(end > start) {
				RecurrenceT recurrenceT = new RecurrenceT();
				String rrule = ical.substring(start+6, end);
				String[] params = rrule.split(";");
				for(String param: params) {
					if(param.startsWith("UNTIL=")) {
						String until = param.substring(6);
						if(!until.endsWith("Z")) {
							if(until.length() >= 15) {
								until = until.substring(0, 15) + "Z";
							} else {
								until = until.substring(0, 8) + "T000000Z";
							}
						}
						recurrenceT.setUntil(
							Datatypes.create(Date.class, until)
						);
					}
					else if(param.startsWith("FREQ=")) {
						String freq = param.substring(5);
						if("DAILY".equals(freq)) {
							recurrenceT.setType(RecurrenceType.DAILY);
						}
						else if("WEEKLY".equals(freq)) {
							recurrenceT.setType(RecurrenceType.WEEKLY);
						}
						else if("MONTHLY".equals(freq)) {
							recurrenceT.setType(RecurrenceType.MONTHLY);
						}
						else if("YEARLY".equals(freq)) {
							recurrenceT.setType(RecurrenceType.YEARLY);
						}
					}
					else if(param.startsWith("BYWEEK=")) {
						recurrenceT.setWeekOfMonth(
							Integer.valueOf(param.substring(7))
						);
					}
					else if(param.startsWith("BYMONTH=")) {
						recurrenceT.setMonthOfYear(
							Integer.valueOf(param.substring(8))
						);
					}
					else if(param.startsWith("BYDAY=")) {
						String[] days = param.substring(6).split(",");
						Set<RecurrenceDayOfWeek> daysOfWeek = new HashSet<RecurrenceDayOfWeek>();
						for(String day: days) {
							try {
								daysOfWeek.add(RecurrenceDayOfWeek.valueOf(day));
							} catch(Exception e) {
								new ServiceException(
									BasicException.Code.DEFAULT_DOMAIN,
									BasicException.Code.ASSERTION_FAILURE,
									"Invalid day of week",
									new BasicException.Parameter("day", day),
									new BasicException.Parameter("ical", ical),
									new BasicException.Parameter("activity", activity.refGetPath())
								).log();
							}
						}
						recurrenceT.setDayOfWeek(daysOfWeek);
					}
					else if(param.startsWith("COUNT=")) {
						recurrenceT.setOccurrences(
							Integer.valueOf(param.substring(6))
						);
					}
					else if(param.startsWith("INTERVAL=")) {
						recurrenceT.setInterval(
							Integer.valueOf(param.substring(9))
						);
					}
				}
				return recurrenceT;
			}
		}
		return null;
	}

	/**
	 * Parse RRULE
	 * @param recurrenceT
	 * @return
	 */
	public String getICalRRule(
		RecurrenceT recurrenceT
	) {
		DateTimeFormat utcf = DateTimeFormat.BASIC_UTC_FORMAT;
		String byDay = null;
		if(recurrenceT.getDayOfWeek() != null && !recurrenceT.getDayOfWeek().isEmpty()) {
			String sep = "";
			byDay = "";
			for(RecurrenceDayOfWeek day: recurrenceT.getDayOfWeek()) {
				byDay += sep;
				byDay += day.toString();
				sep = ",";
			}
		}
		return 
			(recurrenceT.getType() == null ? "" : "FREQ=" + recurrenceT.getType().toString()) +
			(recurrenceT.getUntil() == null ? "" : ";UNTIL=" + utcf.format(recurrenceT.getUntil()).substring(0,15) + "Z") +
			(recurrenceT.getOccurrences() == null ? "" : ";COUNT=" + recurrenceT.getOccurrences()) +
			(recurrenceT.getInterval() == null ? "" : ";INTERVAL=" + recurrenceT.getInterval()) +
			(byDay == null ? "" : ";BYDAY=" + byDay) +
			(recurrenceT.getMonthOfYear() == null ? "" : ";BYMONTH=" + recurrenceT.getMonthOfYear()) +
			(recurrenceT.getWeekOfMonth() == null ? "" : ";BYWEEK=" + recurrenceT.getWeekOfMonth());
	}
	
	/**
	 * Parse EXDATE
	 * @param activity
	 * @return
	 * @throws ServiceException
	 */
	private List<EventT> getExceptions(
		Activity activity
	) throws ServiceException {
		List<EventT> exceptionsT = new ArrayList<EventT>();
		String ical = activity.getIcal();
		int pos = 0;
		while(ical != null && ical.indexOf("EXDATE:", pos) >= 0) {
			int start = ical.indexOf("EXDATE:", pos);
			int end = ical.indexOf("\n", start);
			if(end > start) {
				String[] exdates = ical.substring(start+7, end).split(",");
				for(String exdate: exdates) {
					try {
						EventT exceptionT = new EventT();
						exceptionT.setExceptionStartTime(Datatypes.create(Date.class, exdate));
						exceptionT.setCategories(new ArrayList<String>(activity.getCategory()));
						exceptionsT.add(exceptionT);
					} catch(Exception e) {} // Ignore unparseable exceptions
				}
			}
			pos = end;
		}
		return exceptionsT;
	}

	/**
	 * Get EXDATE
	 * @param exceptionsT
	 * @return
	 */
	private String getICalExdates(
		List<EventT> exceptionsT
	) {
		DateTimeFormat utcf = DateTimeFormat.BASIC_UTC_FORMAT;
		String sep = "";
		String exdates = "";
		for(EventT exceptionT: exceptionsT) {
			exdates += sep;
			try {				
				exdates += utcf.format(exceptionT.getStartTime()).substring(0,15) + "Z";
				sep = ",";
			} catch(Exception e) {} // don't care if format fails
		}
		return exdates;
	}

	/**
	 * Map importance.
	 * @param activity
	 * @return
	 */
	private Importance getImportance(
		Activity activity
	) {
		Short priority = activity.getPriority();
		if(priority != null) {
	        if(priority == Priority.LOW.getValue()) {
	        	return Importance.LOW;
	        } else if(priority == Priority.NORMAL.getValue()) {
	        	return Importance.NORMAL;
	        } else if(priority == Priority.HIGH.getValue()) {
	        	return Importance.HIGH;
	        } else if(priority == Priority.URGENT.getValue()) {
	        	return Importance.HIGH;
	        } else if(priority == Priority.IMMEDIATE.getValue()) {
	        	return Importance.HIGH;
	        }
		}
		return Importance.NORMAL;
    }

	/**
	 * Map importance.
	 * @param alert
	 * @return
	 */
	private Importance getImportance(
		Alert alert
	) {
		Short importance = alert.getImportance();
		if(importance != null) {
	        if(importance == Priority.LOW.getValue()) {
	        	return Importance.LOW;
	        } else if(importance == Priority.NORMAL.getValue()) {
	        	return Importance.NORMAL;
	        } else if(importance == Priority.HIGH.getValue()) {
	        	return Importance.HIGH;
	        } else if(importance == Priority.URGENT.getValue()) {
	        	return Importance.HIGH;
	        } else if(importance == Priority.IMMEDIATE.getValue()) {
	        	return Importance.HIGH;
	        }
		}
		return Importance.NORMAL;
    }

	/**
	 * Map priority.
	 * @param importance
	 * @return
	 */
	private String getICalPriority(
		Importance importance
	) {
        switch(importance) {
			case HIGH:
				return "5";
			case LOW:
				return "1";
			case NORMAL:
			default:
				return "2";
		}
	}
	
	/**
	 * Map sender.
	 * @param alert
	 * @return
	 */
	private AddressT getSender(
		Alert alert
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(alert);
		UserHome userHome = (UserHome)pm.getObjectById(alert.refGetPath().getParent().getParent());
		EMailAccountQuery emailAccountQuery = (EMailAccountQuery)pm.newQuery(EMailAccount.class);
		emailAccountQuery.thereExistsIsDefault().isTrue();
		emailAccountQuery.thereExistsIsActive().isTrue();
		List<EMailAccount> eMailAccounts = userHome.getEMailAccount(emailAccountQuery);
		return eMailAccounts.isEmpty() ?
			null :
				new AddressT(eMailAccounts.iterator().next().getName());
	}
	
	/**
	 * Map sender.
	 * @param email
	 * @return
	 */
	private AddressT getSender(
		EMail email
	) {
		AccountAddress sender = null;
		try {
			sender = email.getSender();
		} catch(Exception e) {
			new ServiceException(e).log();
		}
		if(sender != null && sender instanceof EMailAddress) {
			return new AddressT(((EMailAddress)sender).getEmailAddress());
		}
		return null;
	}
	
	/**
	 * Get attachments.
	 * @param activity
	 * @return
	 * @throws ServiceException
	 */
	private List<AttachmentT> getAttachments(
		Activity activity
	) throws ServiceException {
		List<AttachmentT> attachmentsT = new ArrayList<AttachmentT>();
        String originalMessageMediaName = activity.getActivityNumber().trim() + ".eml.zip";		
        Collection<org.opencrx.kernel.generic.jmi1.Media> medias = activity.getMedia();
        for(org.opencrx.kernel.generic.jmi1.Media media: medias) {
            if(media.getContentName() != null) {
                try {
                    if(!originalMessageMediaName.equals(media.getContentName())) {
                    	AttachmentT attachmentT = new AttachmentT();
                    	attachmentT.setAttachmentName(toObjectId(media));
                    	attachmentT.setDisplayName(media.getContentName());
                    	attachmentT.setEstimatedDataSize(1000);
                    	attachmentT.setIsInline(false);
                    	attachmentT.setMethod(MethodAttachment.NormalAttachment);
                    	attachmentsT.add(attachmentT);
                    }
                }
                catch(Exception e) {
                    new ServiceException(e).log();
                }
            }
        }
        return attachmentsT;
	}
	
	/**
	 * Get email body.
	 * @param email
	 * @return
	 * @throws ServiceException
	 */
	private EmailBodyT getEMailBody(
		EMail email
	) throws ServiceException {
		EmailBodyT bodyT = new EmailBodyT();
		String data = email.getMessageBody();
		bodyT.setData(data);
		bodyT.setType(MimeType.PlainText);
		bodyT.setEstimatedDataSize(data == null ? 0 : data.length());
		bodyT.setTruncated(false);
		return bodyT;
	}
	
	/**
	 * Get email body.
	 * @param alert
	 * @return
	 * @throws ServiceException
	 */
	private EmailBodyT getEMailBody(
		Alert alert
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(alert);
		UserHome userHome = (UserHome)pm.getObjectById(alert.refGetPath().getParent().getParent());
		EmailBodyT bodyT = new EmailBodyT();
		String data = Notifications.getInstance().getNotificationText(
			pm, 
			alert, 
			null, // wfProcessInstance
			userHome, 
			new HashMap<String,Object>() 
		);
		bodyT.setData(data);
		bodyT.setType(MimeType.PlainText);
		bodyT.setEstimatedDataSize(data.length());
		bodyT.setTruncated(false);
		return bodyT;
	}
	
	/**
	 * Get email subject.
	 * @param alert
	 * @return
	 * @throws ServiceException
	 */
	private String getEMailSubject(
		Alert alert
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(alert);
		UserHome userHome = (UserHome)pm.getObjectById(alert.refGetPath().getParent().getParent());
		return Notifications.getInstance().getNotificationSubject(
			pm, 
			alert,
			null, // wfProcessInstanceIdentity
			userHome, 
			new HashMap<String,Object>(), 
			true
		);
	}
	
	/**
	 * Map activity to EventT.
	 * @param event
	 * @param noData
	 * @param user
	 * @param requestContext
	 * @return
	 * @throws ServiceException
	 */
	public SyncDataItem toEventT(
		Activity event,
		boolean noData,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		// data
		EventT eventT = new EventT();
		if(!noData) {
			PersistenceManager pm = JDOHelper.getPersistenceManager(event);
			// Organizer
			Account organizer = null;
			String organizerEmail = null;
			// Try to find party with partyType = PARTY_TYPE_ORGANIZER
			List<AbstractActivityParty> parties = Activities.getInstance().getActivityParties(event);
			for(AbstractActivityParty party: parties) {
				if(party.getPartyType() == Activities.PartyType.OPTIONAL.getValue()) {
		        	RefObject_1_0 partyHolder = null;
		        	try {
		        		partyHolder = (RefObject_1_0)party.refGetValue("party");
		        	} catch(Exception e) {}
		            if(partyHolder != null) {
	                    // Party is Contact
	                    if(partyHolder instanceof Account) {
	                        organizer = (Account)partyHolder;
	                        organizerEmail = Accounts.getInstance().getPrimaryBusinessEMail(
	                            organizer,
	                            party.getEmailHint()
	                        );
	                    }
	                    // Party is address
	                    else if(partyHolder instanceof EMailAddress) {
	                        organizer = (Account)pm.getObjectById(
	                            partyHolder.refGetPath().getParent().getParent()
	                        );
	                        organizerEmail = ((EMailAddress)partyHolder).getEmailAddress();
	                    }
		            }
				}
			}
			// If no organizer party is found fall back to assignedTo
			if(organizer == null) {
				organizer = event.getAssignedTo();
				if(organizer != null) {
					organizerEmail = Accounts.getInstance().getPrimaryBusinessEMail(organizer, null);					
				}
			}
			if(organizer != null) {
				eventT.setOrganizerName(organizer.getFullName());
			}
			eventT.setOrganizerEmail(organizerEmail);
			eventT.setBusyStatus(BusyStatus.TENTATIVE);
			eventT.setLocation(event.getLocation());
			eventT.setSubject(
				event.getName() + 
				(event.getActivityNumber() == null ? "" : Base.COMMENT_SEPARATOR_EOT + " #" + event.getActivityNumber())
			);
			eventT.setUID(getCalendarUID(event));
			String accessUrl = Base.getInstance().getAccessUrl(
				requestContext.getContext(), 
				"-airsync-", 
				event
			);
			eventT.setBody(				
				(event.getDescription() == null ? "" : event.getDescription().trim()) + 
				(accessUrl == null ? "" : Base.COMMENT_SEPARATOR_EOT + " " + accessUrl)
			);
			eventT.setDtStamp(event.getModifiedAt());
			eventT.setEndTime(event.getScheduledEnd());
			eventT.setStartTime(event.getScheduledStart());
			eventT.setAllDayEvent(event.isAllDayEvent());
			eventT.setMeetingStatus(this.getCalendarMeetingStatus(event));
			eventT.setAttendees(this.getAttendeesT(event));
			eventT.setCategories(new ArrayList<String>(event.getCategory()));
			eventT.setRecurrence(getRecurrence(event));
			eventT.setExceptions(getExceptions(event));
		}
		// data item
		SyncDataItem dataItem = new SyncDataItem();
		dataItem.setServerId(toObjectId(event));
		dataItem.setDataType(DataType.Calendar);
		dataItem.setData(eventT);
		return dataItem;
	}
	
	/**
	 * Map email to EMailT.
	 * @param email
	 * @param noData
	 * @param user
	 * @param requestContext
	 * @return
	 * @throws ServiceException
	 */
	public SyncDataItem toEMailT(
		EMail email,
		boolean noData,
		UserHome user,
		RequestContext requestContext
	) throws ServiceException {
		EmailT emailT = new EmailT();
		if(!noData) {
			List<AddressT> to = new ArrayList<AddressT>();
			List<AddressT> cc = new ArrayList<AddressT>();
			List<AddressT> bcc = new ArrayList<AddressT>();
			try {
				List<javax.mail.Address> recipients = Activities.getInstance().mapMessageRecipients(email);
				for(javax.mail.Address recipient: recipients) {
					if(recipient instanceof InternetAddress) {
						InternetAddress inetAddress = (InternetAddress)recipient;
						if(RecipientType.TO.equals(recipient.getType())) {
							to.add(new AddressT(inetAddress.getAddress()));
						}
						else if(RecipientType.CC.equals(recipient.getType())) {
							cc.add(new AddressT(inetAddress.getAddress()));
						}
						else if(RecipientType.BCC.equals(recipient.getType())) {
							bcc.add(new AddressT(inetAddress.getAddress()));
						}
					}
				}
			} catch(Exception e) {}
			// data
			emailT.setSubject(email.getMessageSubject());
			emailT.setBody(getEMailBody(email));
			emailT.setDateReceived(email.getCreatedAt());
			emailT.setAttachements(getAttachments(email));
			emailT.setImportance(getImportance(email));
			emailT.setFrom(getSender(email));
			emailT.setTo(to);
			emailT.setCc(cc);
			emailT.setBcc(bcc);
			emailT.setRead(Boolean.TRUE);
		}
		// data item
		SyncDataItem dataItem = new SyncDataItem();
		dataItem.setServerId(toObjectId(email));
		dataItem.setDataType(DataType.Email);
		dataItem.setData(emailT);
		return dataItem;
	}
	
	/**
	 * Map alert to EMailT.
	 * @param alert
	 * @param noData
	 * @param user
	 * @param requestContext
	 * @return
	 * @throws ServiceException
	 */
	public SyncDataItem toEMailT(
		Alert alert,
		boolean noData,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		EmailT emailT = new EmailT();
		if(!noData) {
			List<AddressT> to = new ArrayList<AddressT>();
			List<AddressT> cc = new ArrayList<AddressT>();
			List<AddressT> bcc = new ArrayList<AddressT>();
			// data
			emailT.setSubject(getEMailSubject(alert));
			emailT.setBody(getEMailBody(alert));
			emailT.setDateReceived(alert.getCreatedAt());
			emailT.setImportance(getImportance(alert));
			emailT.setFrom(getSender(alert));
			emailT.setFrom(getSender(alert));
			emailT.setTo(to);
			emailT.setCc(cc);
			emailT.setBcc(bcc);
			emailT.setRead(
				alert.getAlertState() != AlertState.NEW.getValue()
			);
		}
		// data item
		SyncDataItem dataItem = new SyncDataItem();
		dataItem.setServerId(toObjectId(alert));
		dataItem.setDataType(DataType.Email);
		dataItem.setData(emailT);
		return dataItem;
	}
	
	/**
	 * Map task to TaskT.
	 * @param task
	 * @param noData
	 * @param user
	 * @param requestContext
	 * @return
	 * @throws ServiceException
	 */
	public SyncDataItem toTaskT(
		Activity task,
		boolean noData,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		// data
		TaskT taskT = new TaskT();
		if(!noData) {
			DateTimeFormat userf = this.getUserDateTimeFormat(user);
			DateTimeFormat utcf = DateTimeFormat.BASIC_UTC_FORMAT;
			taskT.setCategories(new ArrayList<String>(task.getCategory()));
			taskT.setComplete(
				(task.getPercentComplete() != null && task.getPercentComplete().intValue() >= 100) &&
				(task.getActualEnd() != null)
			);
			taskT.setDatecompleted(task.getActualEnd());
			try {
				// Convert to user local date/time
				taskT.setDuedate(
					utcf.parse(userf.format(task.getDueBy()))
				);
			} catch(Exception e) {
				taskT.setDuedate(task.getDueBy());				
			}
			taskT.setUtcduedate(task.getDueBy());
			taskT.setImportance(getImportance(task));
			taskT.setRecurrence(getRecurrence(task));
			taskT.setSensitivity(Sensitivity.NORMAL);
			try {
				taskT.setStartdate(
					utcf.parse(userf.format(task.getScheduledStart()))
				);	
			} catch(Exception e) {
				taskT.setStartdate(task.getScheduledStart());
			}
			taskT.setUtcstartdate(task.getScheduledStart());
			taskT.setSubject(
				task.getName() + 
				(task.getActivityNumber() == null ? "" : Base.COMMENT_SEPARATOR_EOT + " #" + task.getActivityNumber().trim())
			);
			String accessUrl = Base.getInstance().getAccessUrl(
				requestContext.getContext(), 
				"-airsync-", 
				task
			);
			taskT.setBody(
				(task.getDescription() == null ? "" : task.getDescription().trim()) + 
				(accessUrl == null ? "" : Base.COMMENT_SEPARATOR_EOT + " " + accessUrl)
			);
		}
		// data item
		SyncDataItem dataItem = new SyncDataItem();
		dataItem.setServerId(toObjectId(task));
		dataItem.setDataType(DataType.Tasks);
		dataItem.setData(taskT);
		return dataItem;
	}
	
	/**
	 * Map account to ContactT.
	 * @param account
	 * @param noData
	 * @param user
	 * @param requestContext
	 * @return
	 * @throws ServiceException
	 */
	public SyncDataItem toContactT(
		Account account,
		boolean noData,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {
		// data
		ContactT contactT = new ContactT();
		if(!noData) {
			Account assistant = findAccountMember(account, Accounts.MEMBER_ROLE_ASSISTANT);
			AccountAddress[] contactAddresses = Accounts.getInstance().getMainAddresses(account);
			contactT.setFileAs(account.getFullName());
			if(account instanceof Contact) {
				Contact contact = (Contact)account;
				contactT.setFirstName(contact.getFirstName());
				contactT.setMiddleName(contact.getMiddleName());
				contactT.setDepartment(contact.getDepartment());
				contactT.setSuffix(contact.getSuffix());
				contactT.setNickName(contact.getNickName());
				contactT.setCompanyName(contact.getOrganization());
				contactT.setTitle(contact.getSalutation());
				contactT.setLastName(contact.getLastName());
				contactT.setJobTitle(contact.getJobTitle());
				contactT.setAnniversary(contact.getAnniversary());
				contactT.setBirthday(contact.getBirthdate());
				contactT.setChildren(new ArrayList<String>(contact.getChildrenNames()));
				if(
					contact.getPicture() != null && 
					"image/jpeg".equals(contact.getPicture().getContentMimeType())
				) {
					contactT.setPicture(Base64.encode(contact.getPictureContent()));
				}
			} else if(account instanceof AbstractGroup) {
				// Last name must have a value. Otherwise targets
				// such as Outlook / Exchange are in trouble
				contactT.setLastName(account.getFullName());
			}
			contactT.setCategories(new ArrayList<String>(account.getCategory()));
			String accessUrl = Base.getInstance().getAccessUrl(
				requestContext.getContext(), 
				"-airsync-", 
				account
			);
			contactT.setBody(
				(account.getDescription() == null ? "" : account.getDescription().trim()) + 
				(accessUrl == null ? "" : Base.COMMENT_SEPARATOR_EOT + " " + accessUrl)
			);
			if(assistant != null) {
				contactT.setAssistantName(assistant.getFullName());
				String assistantPhoneNumber = Accounts.getInstance().getPrimaryBusinessPhone(assistant);
				contactT.setAssistantPhoneNumber(assistantPhoneNumber);
				contactT.setAssistnamePhoneNumber(assistantPhoneNumber);
			}
			if(contactAddresses[Accounts.POSTAL_BUSINESS] != null) {
				PostalAddress postalBusiness = (PostalAddress)contactAddresses[Accounts.POSTAL_BUSINESS];
				contactT.setBusinessAddressCity(postalBusiness.getPostalCity());
				contactT.setBusinessPostalCode(postalBusiness.getPostalCode());
				contactT.setBusinessState(postalBusiness.getPostalState());
				String sep = "";
				String businessStreet = "";
				for(String line: postalBusiness.getPostalAddressLine()) {
					businessStreet += sep;
					businessStreet += line;
					sep = "\r\n";
				}
				for(String street: postalBusiness.getPostalStreet()) {
					businessStreet += sep;
					businessStreet += street;
					sep = "\r\n";
				}
				// Cut off trailing blanks and separators
				if(businessStreet != null) {
					businessStreet = businessStreet.trim();
					while(!sep.isEmpty() && businessStreet.endsWith(sep)) {
						businessStreet = businessStreet.substring(0, businessStreet.length() - sep.length());
					}
				}
				contactT.setBusinessStreet(businessStreet);
				contactT.setBusinessAddressCountry(Addresses.POSTAL_COUNTRIES_BY_CODE.get(postalBusiness.getPostalCountry()));
			}
			if(contactAddresses[Accounts.POSTAL_HOME] != null) {
				PostalAddress postalHome = (PostalAddress)contactAddresses[Accounts.POSTAL_HOME];
				contactT.setHomeAddressCity(postalHome.getPostalCity());
				contactT.setHomeAddressPostalCode(postalHome.getPostalCode());
				contactT.setHomeAddressState(postalHome.getPostalState());
				String sep = "";
				String homeStreet = "";
				for(String line: postalHome.getPostalAddressLine()) {
					homeStreet += sep;
					homeStreet += line;
					sep = "\r\n";
				}
				for(String street: postalHome.getPostalStreet()) {
					homeStreet += sep;
					homeStreet += street;
					sep = "\r\n";
				}
				// Cut off trailing blanks and separators
				if(homeStreet != null) {
					homeStreet = homeStreet.trim();
					while(!sep.isEmpty() && homeStreet.endsWith(sep)) {
						homeStreet = homeStreet.substring(0, homeStreet.length() - sep.length());
					}
				}
				contactT.setHomeAddressStreet(homeStreet);
				contactT.setHomeAddressCountry(Addresses.POSTAL_COUNTRIES_BY_CODE.get(postalHome.getPostalCountry()));
			}
			if(contactAddresses[Accounts.PHONE_BUSINESS] != null) {
				PhoneNumber phoneBusiness = (PhoneNumber)contactAddresses[Accounts.PHONE_BUSINESS];
				contactT.setBusinessPhoneNumber(phoneBusiness.getPhoneNumberFull());
			}
			if(contactAddresses[Accounts.PHONE_HOME] != null) {
				PhoneNumber phoneHome = (PhoneNumber)contactAddresses[Accounts.PHONE_HOME];
				contactT.setHomePhoneNumber(phoneHome.getPhoneNumberFull());
			}
			if(contactAddresses[Accounts.PHONE_OTHER] != null) {
				PhoneNumber phoneNumberOther = (PhoneNumber)contactAddresses[Accounts.PHONE_OTHER];
				contactT.setBusiness2PhoneNumber(phoneNumberOther.getPhoneNumberFull());			
			}
			if(contactAddresses[Accounts.WEB_BUSINESS] != null) {
				WebAddress webAddress = (WebAddress)contactAddresses[Accounts.WEB_BUSINESS];
				contactT.setWebPage(webAddress.getWebUrl());
			}
			if(contactAddresses[Accounts.FAX_BUSINESS] != null) {
				PhoneNumber faxBusiness = (PhoneNumber)contactAddresses[Accounts.FAX_BUSINESS];
				contactT.setBusinessFaxNumber(faxBusiness.getPhoneNumberFull());
			}
			if(contactAddresses[Accounts.FAX_HOME] != null) {
				PhoneNumber faxHome = (PhoneNumber)contactAddresses[Accounts.FAX_HOME];
				contactT.setHomeFaxNumber(faxHome.getPhoneNumberFull());
			}
			if(contactAddresses[Accounts.MOBILE] != null) {
				PhoneNumber phoneMobile = (PhoneNumber)contactAddresses[Accounts.MOBILE];
				contactT.setMobilePhoneNumber(phoneMobile.getPhoneNumberFull());
			}
			if(contactAddresses[Accounts.MAIL_BUSINESS] != null) {
				EMailAddress emailBusiness = (EMailAddress)contactAddresses[Accounts.MAIL_BUSINESS];
				contactT.setEmail1Address(emailBusiness.getEmailAddress());
			}
			if(contactAddresses[Accounts.MAIL_HOME] != null) {
				EMailAddress emailHome = (EMailAddress)contactAddresses[Accounts.MAIL_HOME];
				contactT.setEmail2Address(emailHome.getEmailAddress());
			}
		}
		// data item
		SyncDataItem dataItem = new SyncDataItem();
		dataItem.setServerId(toObjectId(account));
		dataItem.setDataType(DataType.Contacts);
		dataItem.setData(contactT);
		return dataItem;
	}

	/**
	 * Map document to NoteT.
	 * @param document
	 * @param noData
	 * @param user
	 * @param requestContext
	 * @return
	 * @throws ServiceException
	 */
	public SyncDataItem toNoteT(
		Document document,
		boolean noData,
		UserHome user,
		RequestContext requestContext		
	) throws ServiceException {		
		// data
		NoteT noteT = new NoteT();
		if(!noData) {
			DocumentRevision headRevision = document.getHeadRevision();
			if(headRevision instanceof MediaContent) {
				MediaContent content = (MediaContent)headRevision;
				noteT.setSubject(document.getName());
				ByteArrayOutputStream bytes = new ByteArrayOutputStream();
				try {
					BinaryLargeObjects.streamCopy(
						content.getContent().getContent(), 
						0L, 
						bytes
					);
					noteT.setBody(new String(bytes.toString("UTF-8")));
				} catch(Exception e) {}
				if(document.getKeywords() != null) {
					noteT.setCategories(Arrays.asList(document.getKeywords().split(",")));
				}
				noteT.setLastModifiedDate(headRevision.getCreatedAt());
			}
		}
		// data item
		SyncDataItem dataItem = new SyncDataItem();
		dataItem.setServerId(toObjectId(document));
		dataItem.setDataType(DataType.Notes);
		dataItem.setData(noteT);
		return dataItem;
	}

	/**
	 * Map media to AttachmentDataT.
	 * @param object
	 * @return
	 * @throws ServiceException
	 */
	public AttachmentDataT toAttachmentData(
		RefObject_1_0 object
	) throws ServiceException {
		if(object instanceof Media) {
			try {
				AttachmentDataT attachmentDataT = new AttachmentDataT();
				Media media = (Media)object;
				attachmentDataT.setContentType(media.getContentMimeType());
				attachmentDataT.setContent(media.getContent().getContent());
				return attachmentDataT;
			} catch(Exception e) {
				throw new ServiceException(e);
			}
		}
		else {
			return null;
		}
	}

	/**
	 * Create new account which represents a ContactT.
	 * @param pm
	 * @param contactT
	 * @return
	 */
	public Account newAccount(
		PersistenceManager pm,
		ContactT contactT
	) {
		return pm.newInstance(Contact.class);
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	public static final String SYNC_FEED_URI_SCHEME = "feed://";
	public static final String ACTIVITY_TRACKER_URI_SCHEME = "tracker://";
	public static final String ACTIVITY_CATEGORY_URI_SCHEME = "category://";
	public static final String ACTIVITY_MILESTONE_URI_SCHEME = "milestone://";
	public static final String ACTIVITY_FILTER_GLOBAL_URI_SCHEME = "activityFilterGlobal://";
	public static final String ACTIVITY_FILTER_GROUP_URI_SCHEME = "activityFilterGroup://";
	public static final String USER_HOME_URI_SCHEME = "userHome://";
	public static final String ACTIVITY_URI_SCHEME = "activity://";
	public static final String ACCOUNT_URI_SCHEME = "account://";
	public static final String ALERT_URI_SCHEME = "alert://";
	public static final String MEDIA_URI_SCHEME = "media://";
	public static final String DOCUMENT_FOLDER_URI_SCHEME = "documentFolder://";
	public static final String DOCUMENT_URI_SCHEME = "document://"; 
	
}
